# Code to evaluate the effects of converting commercial total lengths to fork
# lengths after it was discovered that there were many more total lengths
# after 2016 (almost none prior to 2016)
# Robyn Forrest. May 24 2022

library(tidyverse)
library(gfdata)
library(here)
# load necessary functions
source(here('R/get-data.R'))
french <- FALSE

# TODO: Add a version where total lengths are converted to fork length
# Need a conversion factor. Have asked Maria Cornthwaite

# Run get-iscam-inputs.R first to make sure data files are there

# 1. Get the data from different dates using cache_pbs_data()
# 2020 assessment was probably done with gfplot as it has "gear" not "gear_code" in commercial_samples

# 1. Data from 2020 SR (fork and total lengths mixed)
dat_2020 <- readRDS("C:/GitHub/pacific-cod-2020/data/pcod-cache/pacific-cod.rds")
comsamp_2020 <- dat_2020$commercial_samples

# 2. 2022 data (fork and total lengths mixed). Should be  same as 2020 for years to 2019
# commercial samples. Fork and total length mixed
dat_2022 <- readRDS("C:/GitHub/pacific-cod-2022/data/pcod-cache/pacific-cod.rds")
file.name <- here::here("data/generated/comm-samples-with-length-type.rds")
comsamp_2022_mix <- readRDS(file.name)

# 3. 2022 data (total converted to fork). Waiting for conversion factors
# UPDATE: Maria says these are likely recording errors rather than
# measurement errors, i.e., all lengths would be fork. P cod has a flattish tail anyway
# Set conversion factor to 1.
conv_factor <- 1.
comsamp_2022_fork <- comsamp_2022_mix %>%
  mutate(fork_length=length) %>%
  mutate(fork_length=ifelse(length_type=="total_length",conv_factor*fork_length,fork_length)) %>%
  select(-length) %>%
  rename(length=fork_length)

# Get commercial mean weights. Function is in R/get-data.R
include.usa = TRUE
# READ THIS NOTE
# for 2020, need to change gear_code to gear on L77-78 of get-data.R

# Fork and total lengths together
w5abcd_2020_mix <- get.mean.weight(comsamp_2020,
                          dat_2020$catch,
                          areas = c("5[AB]+", "5[CD]+"),
                          include.usa = include.usa,
                          a = .ALPHA5,
                          b = .BETA5)%>%
                          mutate(area="5ABCD") %>%
                          rename(mean_weight_2020_5ABCD=mean_weight)

w3cd_2020_mix <- get.mean.weight(comsamp_2020,
                             dat_2020$catch,
                               areas = c("3[CD]+"),
                               include.usa = include.usa,
                               a = .ALPHA3,
                               b = .BETA3)%>%
                              mutate(area="3CD") %>%
                              rename(mean_weight_2020_3CD=mean_weight)

# READ THIS TOO
# for this one and all others, need to change gear back to gear_code on L77-78 of get-data.R

# Fork and total lengths together
w5abcd_2022_mix <- get.mean.weight(comsamp_2022_mix,
                               dat_2022$catch,
                               areas = c("5[AB]+", "5[CD]+"),
                               include.usa = include.usa,
                               a = .ALPHA5,
                               b = .BETA5)%>%
                               mutate(area="5ABCD") %>%
                               rename(mean_weight_2022_5ABCD_mix=mean_weight)

w3cd_2022_mix <- get.mean.weight(comsamp_2022_mix,
                             dat_2022$catch,
                             areas = c("3[CD]+"),
                             include.usa = include.usa,
                             a = .ALPHA3,
                             b = .BETA3)%>%
                             mutate(area="3CD") %>%
                             rename(mean_weight_2022_3CD_mix=mean_weight)

# Total lengths converted to fork length
w5abcd_2022_fork <- get.mean.weight(comsamp_2022_fork,
                                   dat_2022$catch,
                                   areas = c("5[AB]+", "5[CD]+"),
                                   include.usa = include.usa,
                                   a = .ALPHA5,
                                   b = .BETA5)%>%
                                   mutate(area="5ABCD") %>%
                                   rename(mean_weight_2022_5ABCD_fork=mean_weight)

w3cd_2022_fork <- get.mean.weight(comsamp_2022_fork,
                                 dat_2022$catch,
                                 areas = c("3[CD]+"),
                                 include.usa = include.usa,
                                 a = .ALPHA3,
                                 b = .BETA3)%>%
                                mutate(area="3CD") %>%
                                rename(mean_weight_2022_3CD_fork=mean_weight)

# Note: all datasets only go to 2019 as there has been no commercial sampling
w5abcd_datcompare <- w5abcd_2020_mix %>%
  left_join(w5abcd_2022_mix) %>%
  left_join(w5abcd_2022_fork) %>%
  dplyr::select(-"area") %>%
  as.data.frame() %>%
  reshape2::melt(id.vars="year", variable.name="Year_length_type", value.name="mean_weight")

w3cd_datcompare <- w3cd_2020_mix %>%
  left_join(w3cd_2022_mix) %>%
  left_join(w3cd_2022_fork) %>%
  dplyr::select(-"area") %>%
  as.data.frame() %>%
  reshape2::melt(id.vars="year", variable.name="Year_length_type", value.name="mean_weight")

# plot
g1 <- ggplot(data=w5abcd_datcompare) +
  geom_line(aes(x=year,y=mean_weight, colour=Year_length_type, linetype=Year_length_type), lwd=0.75) +
  ylim(1,1.1*max(w5abcd_datcompare$mean_weight)) +
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=6),
        axis.title=element_text(size=14,face="bold")) +
  scale_x_continuous(breaks=seq(1956,2020,by=5)) +
  labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 5ABCD")+
  gfplot::theme_pbs()
ggsave(here::here("data/generated/length_type_summaries/Pcod-mean-weight-compare-5ABCD.png"))

g2 <- ggplot(data=w3cd_datcompare) +
  geom_line(aes(x=year,y=mean_weight, colour=Year_length_type, linetype=Year_length_type), lwd=0.75) +
  ylim(1,1.1*max(w3cd_datcompare$mean_weight)) +
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=6),
        axis.title=element_text(size=14,face="bold")) +
  scale_x_continuous(breaks=seq(1956,2020,by=5)) +
  labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 3CD")+
  gfplot::theme_pbs()
ggsave(here::here("data/generated/length_type_summaries/Pcod-mean-weight-compare-3CD.png"))

# query the number of length samples of each type
summary_length_type <- comsamp_2022_mix %>%
  group_by(year,major_stat_area_name) %>%
  filter(!is.na(length),year>1951,!major_stat_area_name%in% c("4B: STRAIT OF GEORGIA","3B: CAPE FLATTERY (47 20' to 220 T)","5E: WEST COAST Q.C. ISLANDS","UNKNOWN: NO POSITION INFORMATION")) %>%
  summarise(Num_samples = n(), Num_forklength=sum(length_type=="fork_length"),Num_totallength=sum(length_type=="total_length")) %>%
  arrange(major_stat_area_name,year)

# just years with total length
summary_length_type_total_length_years <- summary_length_type %>%
  dplyr::filter(Num_totallength>0)

#get a summary of the sample_ids with total length
summary_sampleid <- comsamp_2022_mix %>%
  filter(length_type=="total_length") %>%
  select(year,major_stat_area_name,sample_id) %>%
  unique() %>%
  arrange(major_stat_area_name,year,sample_id)

summary_sampleid_all_total <- comsamp_2022_mix %>%
  filter(length_type=="total_length") %>%
  select(year,major_stat_area_name,sample_id, specimen_id, length,length_type) %>%
  arrange(major_stat_area_name,year,sample_id, specimen_id)

write_csv(summary_length_type,here::here("data/generated/length_type_summaries/Pcod-comm-length-type-summary.csv"))
write_csv(summary_length_type_total_length_years,here::here("data/generated/length_type_summaries/Pcod-comm-total-length-summary.csv"))
write_csv(summary_sampleid,here::here("data/generated/length_type_summaries/Pcod-comm-total-length-sample-id-summary.csv"))
write_csv(summary_sampleid_all_total,here::here("data/generated/length_type_summaries/Pcod-comm-total-length-samples.csv"))


