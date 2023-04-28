# Code by Sean Anderson, 2022.

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

Years <- 1956:2022

#wdat <- read_csv(here::here("report/MeanWeightTable_3CD-allyrs.csv"))
wdat <- read_csv(file.path(generatedd,"commercial_mean_weight_3CD.csv"))

# RF: Add rows for missing years
# Not sure if we need to do this since we remove them again
missing_years <- Years[!Years %in% wdat$year]
missing_rows <- data.frame(year = missing_years,
                           mean_weight=NA)
wdat <- wdat %>%
  rbind(missing_rows) %>%
  arrange(year)

maxyr <- 2017

# wdat <- wdat[,1:2]
# colnames(wdat) <- c("year", "mean_weight")

wdat <- dplyr::filter(wdat, year < maxyr, !is.na(mean_weight)) # not many samples

plot(wdat$year, wdat$mean_weight);abline(v = 1996)

diff(wdat$year)
# TODO missing some years!! RF: This is 1997. No data

stan_wdat <- list(
  N = length(wdat$mean_weight),
  y = log(wdat$mean_weight),
  rho_sd = 0.5,
  start_observer_effect = min(which(wdat$year >= 1996)),
  sigma_o_prior1 = c(log(0.2), 0.3),
  sigma_o_prior2 = c(log(0.2), 0.3),
  sigma_p_prior = c(log(0.1), 0.3)
)

fit <- stan(
  "R/ar1ss.stan",
  data = stan_wdat,
  chains = 4,
  iter = 2000,
  seed = 92823929,
  pars = c("rho1", "rho2", "sigma_p", "sigma_o1", "sigma_o2",
    "y_init", "observer_effect", "y_true"),
  control = list(adapt_delta = 0.99, max_treedepth = 10)
)
fit

p <- extract(fit)
matplot(t(p$y_true), type = "l", col = "#00000010", lty = 1)
yrs <- seq_along(wdat$mean_weight)
points(yrs, log(wdat$mean_weight), col = "red")

rho1 <- p$rho1
rho2 <- p$rho2
sigma_p <- p$sigma_p
sigma_o1 <- p$sigma_o1
sigma_o2 <- p$sigma_o2
alpha <- p$alpha
obs_effect <- p$observer_effect

SAMPS <- 1000
y_true <- t(p$y_true)[, 1:SAMPS]
y_obs <- matrix(ncol = ncol(y_true), nrow = nrow(y_true))
for (i in 1:SAMPS) {
  for (.t in 1:stan_wdat$N) {
    if (.t < stan_wdat$start_observer_effect) {
      y_obs[.t, i] <- rnorm(1, y_true[.t, i], p$sigma_o1[i])
    } else {
      y_obs[.t, i] <- rnorm(1, y_true[.t, i], p$sigma_o2[i])
    }
  }
}

matplot(y_obs, type = "l", col = "#00000010", lty = 1)
yrs <- seq_along(wdat$mean_weight)
points(yrs, log(wdat$mean_weight), col = "red")

calc_post <- function(y0, rho2, sigma_p, sigma_o2, alpha, obs_effect, N) {
  y_true <- numeric(length = N)
  for (i in 1:N) {
    if (i == 1) {
      y_true[i] <- y0
    } else {
      y_true[i] <- alpha + rho2 * y_true[i - 1] + rnorm(1, 0, sigma_p)
    }
  }
  rnorm(length(y_true), y_true, sigma_o2)
}

x <- matrix(nrow = 10L, ncol = SAMPS)

for (i in 1:SAMPS) {
  x[, i] <- calc_post(
    y0 = p$y_true[i, ncol(p$y_true)],
    rho2 = p$rho2[i],
    # rho = 1,
    sigma_p = p$sigma_p[i],
    sigma_o2 = p$sigma_o2[i],
    # alpha = p$alpha[i],
    alpha = 0,
    obs_effect = p$observer_effect[i],
    N = 10
  )
}
matplot(x, type = "l", col = "#00000080", lty = 1)

all <- rbind(y_obs, x)

library(dplyr)
library(ggplot2)
pp <- reshape2::melt(all) %>%
  rename(year = Var1, iter = Var2)

wdat$numeric_year <- 1:nrow(wdat)
# ggplot(pp, aes(year, value, group = iter)) +
#   geom_line(alpha = 0.1) +
#   geom_point(wdata = wdat,
#     mapping = aes(numeric_year, mean_weight),
#     inherit.aes = FALSE, colour = "red")

pp %>%
  group_by(year) %>%
  summarise(
    lwr = quantile(value, probs = 0.025),
    lwr2 = quantile(value, probs = 0.25),
    upr2 = quantile(value, probs = 0.75),
    upr = quantile(value, probs = 0.975),
    med = quantile(value, probs = 0.5)
  ) %>%
  ggplot(aes(year, y = exp(med), ymin = exp(lwr), ymax = exp(upr))) +
  geom_line(alpha = 0.1) +
  geom_line(wdata = subset(pp, iter %in% 1:10), mapping = aes(x = year, y = exp(value), group = iter), alpha = 0.1, inherit.aes = FALSE) +
  geom_ribbon(alpha = 0.2) +
  geom_ribbon(aes(ymin = exp(lwr2), ymax = exp(upr2)), alpha = 0.2) +
  geom_point(
    wdata = wdat,
    mapping = aes(numeric_year, mean_weight),
    inherit.aes = FALSE, colour = "red"
  ) +
  geom_line(
    wdata = wdat,
    mapping = aes(numeric_year, mean_weight),
    inherit.aes = FALSE, colour = "red", lwd = 0.2, alpha = 0.9
  )

# RF: take a random sample
rand <- sample(1:SAMPS,8,replace=F)

fake <- wdata.frame(year = wdat$numeric_year, value = log(wdat$mean_weight), iter = NA, real_wdata = TRUE)
post <- subset(pp, iter %in% rand) %>% mutate(real_wdata = FALSE)

# fake$iter <- 4
# post$iter[post$iter == 4] <- 9

# filter(bind_rows(fake, post), year <= stan_wdat$N) %>%
filter(bind_rows(fake, post)) %>%
  ggplot() +
  geom_line(aes(x = year, y = exp(value), colour = real_wdata), inherit.aes = FALSE) +
  facet_wrap(~iter) +
  scale_colour_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  geom_vline(xintercept = stan_wdat$start_observer_effect, lty = 2) +
  geom_vline(xintercept = stan_wdat$N, lty = 2)

#========================================================
# RF: now get values to use in models
# Want 2018-2020 but for 3CD have to skip 2017!
p1<-2
p2<-4

obsyr <- wdat$year
obsnyr <- length(obsyr)
projyr_ind <- (obsnyr+p1):(obsnyr+p2)
projyr <- (obsyr[obsnyr]+p1):(obsyr[obsnyr]+p2)

allyr <- c(obsyr, (obsyr[obsnyr]+1):(obsyr[obsnyr]+10))

post_3yproj <- post %>%
  mutate(mean_weight=exp(value)) %>%
  select(-real_wdata, -value) %>%
  dplyr::filter(year %in% projyr_ind) %>%
  reshape2::dcast(year~iter) %>%
  mutate(year=projyr)

write.csv(post_3yproj,here::here("data/generated/imputed_mw_2018-2020_3CD.csv"))


# For the shortcut approach, get mean and CV from 1000 samples
means <- all %>%
  exp() %>%
  as.data.frame() %>%
  mutate(mean = rowMeans(select(., 1:SAMPS))) %>%
  mutate(year=allyr) %>%
  select(year,mean)

sds <- apply(exp(all),1,sd)
CVs <- sds/means$mean

test1 <- mean(exp(all[1,]))
test2 <- means$mean[1]
test1 == test2

test1 <- sd(exp(all[1,]))
test2 <- sds[1]
test1 == test2

means <- means %>%
  mutate(sd=sds, CV=CVs) %>%
  dplyr::filter(year %in% projyr)

write.csv(means,here::here("wdata/generated/shortcut_mw_2018-2020_3CD.csv"))




