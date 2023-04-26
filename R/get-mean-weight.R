# This script is called by all.R

#Length-weight parameters
#coastwide
.ALPHA <- 6.79e-06
.BETA <- 3.11

#3CD
.ALPHA3FEM <- 7.43e-06
.ALPHA3 <- 7.65616e-06
.BETA3FEM <- 3.09
.BETA3 <- 3.08

#5ABCD
.ALPHA5FEM <- 6.52e-06
.ALPHA5 <- 6.722839e-06
.BETA5FEM <- 3.12
.BETA5 <- 3.11

#old (2013 assessment)
.ALPHA2013 <- 7.377e-06
.BETA2013 <- 3.0963

prevMeanWeight <- read.csv(file.path(rootd.data, "MeanWeights_previous.csv"))

d <- dat$commercial_samples

################################################################
## Get mean weight
include.usa <- TRUE

## 3CD
df3CD <- get.mean.weight(d,
                         dat$catch,
                         areas = "3[CD]+",
                         include.usa = include.usa,
                         a = .ALPHA3,
                         b = .BETA3)
write_csv(df3CD,file.path(resultsd,"AnnualMeanWeight_3CD.csv"))

## 5ABCD
df5ABCD <- get.mean.weight(d,
                           dat$catch,
                           areas = c("5[AB]+", "5[CD]+"),
                           include.usa = include.usa,
                           a = .ALPHA5,
                           b = .BETA5)
write_csv(df5ABCD,file.path(resultsd,"AnnualMeanWeight_5ABCD.csv"))

#################################################################
## Plot results

## 3CD
df <- df3CD
ggplot(data=df, aes(x=year,y=mean_weight, group=1)) +
  geom_line(lwd=1, colour=2) +
  ylim(0,1.1*max(df$mean_weight)) +
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_x_continuous(breaks=seq(min(df$year),max(df$year),by=5)) +
  labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 3CD")
ggsave(file.path(resultsd,"AnnualMeanWeight_3CD.png"), width=8, height=6, units="in")

## 5ABCD
df <- df5ABCD
ggplot(data=df, aes(x=year,y=mean_weight, group=1)) +
  geom_line(lwd=1, colour=2) +
  ylim(0,1.1*max(df$mean_weight)) +
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_x_continuous(breaks=seq(min(df$year),max(df$year),by=5)) +
  labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 5ABCD")
ggsave(file.path(resultsd,"AnnualMeanWeight_5ABCD.png"), width=8, height=6, units="in")

#########################################################################################
