
# 1. Load libraries ------

library(rgdal)
library(sp)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggpubr) # for ggqqplots
library(lme4) # for RSFs
library(adehabitatHR) # for RSFs
library(adehabitatHS) # for RSFs
library(TMB)
library(glmmTMB) # RSFs according to Muff et al. (2019)
library(data.table) # required for section 10
library(gridExtra) # for grid.arrange() in section 10

theme_nuwcru <- function(){
  theme_bw() +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = -45, hjust = -0.05),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(size = 15, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 10),
          #legend.title = element_blank(),
          #legend.position = c(0.9, 0.9),
          legend.key = element_rect(colour = NA, fill = NA),
          legend.background = element_rect(color = "black",
                                           fill = "transparent",
                                           size = 4, linetype = "blank"))
}

setwd("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/DS_polarbear_spatial/")


# 2. Import final dataframes ---------

# import data
used_avail_RSF_pooled_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_pooled_FINAL_Apr2021.csv")
used_avail_RSF_winter_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_winter_FINAL_Apr2021.csv")
used_avail_RSF_breakup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_breakup_FINAL_Apr2021.csv")
used_avail_RSF_freezeup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_freezeup_FINAL_Apr2021.csv")

# Note: all top models were determined in script 08_RSFs


# 3. Analyze top pooled RSF model results --------

# pooled: Model 11 (BATH + CONC + DIST_LAND)
# run model 

model11_tmp <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
model11_tmp$parameters$theta[1] = log(1e3)
model11_tmp$mapArg = list(theta = factor(c(NA, 1:3)))
model11 <- glmmTMB:::fitTMB(model11_tmp) 
summary(model11)
fixef(model11)

### I NEED TO FIGURE OUT HOW TO UNSCALE THE COEFFICIENTS
# Try just running the model again on the original values

unscale_model11_tmp <- glmmTMB(USED_AVAIL~BATH+CONC+DIST_LAND+(1|ID)+(0+BATH|ID)+(0+CONC|ID)+(0+DIST_LAND|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
unscale_model11_tmp$parameters$theta[1] = log(1e3)
unscale_model11_tmp$mapArg = list(theta = factor(c(NA, 1:3)))
unscale_model11 <- glmmTMB:::fitTMB(unscale_model11_tmp) 
summary(unscale_model11)
fixef(unscale_model11)

# get mean values
mean(used_avail_RSF_pooled_FINAL$DIST_WATER_M)

##


# 4. Analyze top seasonal RSF model (freeze-up) results--------

# freeze-up: Model 7 (BATH + DIST_WATER) - note that we couldn't set the variance here
model7_freeze_free_var <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=TRUE, weights=W)
summary(model7_freeze_free_var)

# re-run model with unscaled values
unscale_model7_freeze_free_var <- glmmTMB(USED_AVAIL~BATH+DIST_WATER+(1|ID)+(0+BATH|ID)+(0+DIST_WATER|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=TRUE, weights=W)
summary(unscale_model7_freeze_free_var)

# get mean values
mean(used_avail_RSF_freezeup_FINAL$CONC)

# 5. Analyze top seasonal RSF model (winter) results--------

# winter: Model 8 (CONT + DIST_LAND) - same issue as above
model8_winter_free_var <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=TRUE, weights=W)
summary(model8_winter_free_var)

# re-run model with unscaled values
unscale_model8_winter_free_var <- glmmTMB(USED_AVAIL~CONC+DIST_LAND+(1|ID)+(0+CONC|ID)+(0+DIST_LAND|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=TRUE, weights=W)
summary(unscale_model8_winter_free_var)

# get mean values
mean(used_avail_RSF_winter_FINAL$DIST_WATER_M)

# 6. Analyze top seasonal RSF model (break-up) results--------

# break-up: Model 6 (BATH + DIST_LAND)
model6_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
model6_tmp_break$parameters$theta[1] = log(1e3)
model6_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))
model6_break <- glmmTMB:::fitTMB(model6_tmp_break) 
summary(model6_break)

# re-run model with unscaled values
unscale_model6_tmp_break <- glmmTMB(USED_AVAIL~BATH+DIST_LAND+(1|ID)+(0+BATH|ID)+(0+DIST_LAND|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
unscale_model6_tmp_break$parameters$theta[1] = log(1e3)
unscale_model6_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))
unscale_model6_break <- glmmTMB:::fitTMB(unscale_model6_tmp_break) 
summary(unscale_model6_break)

# get mean values
mean(used_avail_RSF_breakup_FINAL$DIST_WATER_M)



# 7. Creating manuscript figures ----------

# 1. FORMAT DATA

      # Create dataframe of mean values for each RSF and covariate
RSF_means <- data.frame(matrix(nrow=4, ncol=5))
RSF_means$RSF <- c("pooled", "freezeup", "winter", "breakup")
RSF_means$CONC_MEAN <- as.numeric(c("0.8442", "0.8007", "0.7841", "0.8843"))
RSF_means$BATH_MEAN <- as.numeric(c("-455.02", "-709.36", "-548.12", "-357.67"))
RSF_means$LAND_MEAN <- as.numeric(c("91440.88", "121600.10", "105215.70", "87224.08"))
RSF_means$WATER_MEAN <- as.numeric(c("347617.90", "366032.30", "452889.20", "299263.50"))
head(RSF_means)
RSF_means <- subset(RSF_means, select=-c(X1, X2, X3, X4, X5))
str(RSF_means)
      # combine all original dataframes 
            # first make a new column to differenciate between them all
used_avail_RSF_breakup_FINAL$RSF <- rep("breakup")
used_avail_RSF_freezeup_FINAL$RSF <- rep("freezeup")
used_avail_RSF_winter_FINAL$RSF <- rep("winter")
used_avail_RSF_pooled_FINAL$RSF <- rep("pooled")
            # then combine
RSF_results <- rbind(used_avail_RSF_breakup_FINAL, used_avail_RSF_freezeup_FINAL)
RSF_results2 <- rbind(RSF_results, used_avail_RSF_winter_FINAL)
RSF_results_FINAL <- rbind(RSF_results2, used_avail_RSF_pooled_FINAL)

      # order RSF column
RSF_results_FINAL$RSF <- factor(RSF_results_FINAL$RSF, levels=c("pooled", "freezeup", "winter", "breakup"))



###

# 2. MAKE BOXPLOTS

      # BATH
summary(RSF_results_FINAL)

bath_boxplot <- ggplot(RSF_results_FINAL) +
  geom_boxplot(aes(y=BATH, x=RSF), varwidth = T, alpha=0.2) +
  scale_y_continuous(limits=c(-3500, 500), breaks=c(-3500, -3000, -2500, -2000, -1500, -1000, -500, 0, 500), labels=c("-3500", "-3000", "-2500", "-2000", "-1500", "-1000", "-500", "0", "500")) +
  ylab("Ocean depth (m)") +
  xlab("Type of RSF") +
  geom_text(data=RSF_means, aes(x=RSF, y=BATH_MEAN, label=BATH_MEAN), size=3.5,  vjust=-9) +
  theme_nuwcru()
bath_boxplot

      # CONC
conc_boxplot <- ggplot(RSF_results_FINAL) +
  geom_boxplot(aes(y=CONC, x=RSF), varwidth = T, alpha=0.2) +
  scale_y_continuous(limits=c(0, 1.1), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels=c("0", "0.2", "0.4", "0.6", "0.8", "1.0")) +
  ylab("Sea ice concentration (%)") +
  xlab("Type of RSF") +
  geom_text(data=RSF_means, aes(x=RSF, y=CONC_MEAN, label=CONC_MEAN), size=3.5, vjust=-8.6) +
  theme_nuwcru()
conc_boxplot

      # LAND
land_boxplot <- ggplot(RSF_results_FINAL) +
  geom_boxplot(aes(y=DIST_LAND, x=RSF), varwidth = T, alpha=0.2) +
  scale_y_continuous(limits=c(0, 460000), breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 450000), labels=c("0", "50000", "100000", "150000", "200000", "250000", "300000", "350000", "400000", "450000")) +
  ylab("Distance to land (m)") +
  xlab("Type of RSF") +
  geom_text(data=RSF_means, aes(x=RSF, y=LAND_MEAN, label=LAND_MEAN), size=3.5, vjust=-33.5) +
  theme_nuwcru()
land_boxplot


      # WATER
water_boxplot <- ggplot(RSF_results_FINAL) +
  geom_boxplot(aes(y=DIST_WATER_M, x=RSF), varwidth = T, alpha=0.2) +
  scale_y_continuous(limits=c(0, 1600000), breaks=c(0, 200000, 400000, 600000, 800000, 1000000, 1200000, 1400000, 1600000), labels=c("0", "200000", "400000", "600000", "800000", "1000000", "1200000", "1400000", "1600000")) +
  ylab("Distance to water (m)") +
  xlab("Type of RSF") +
  geom_text(data=RSF_means, aes(x=RSF, y=WATER_MEAN, label=WATER_MEAN), size=3.5, vjust=-32) +
  theme_nuwcru()
water_boxplot

summary(used_avail_RSF_freezeup_FINAL$DIST_WATER_M)
head(RSF_results_FINAL)


###

# Look at coefficients for each model: https://terpconnect.umd.edu/~egurarie/research/NWT/Step08_RSF_PartIII.html
      # re-run all SCALED top models from section 3

      # pooled RSF
coefs.pooled <- summary(model11)$coef$cond %>% as.data.frame
head(coefs.pooled)
names(coefs.pooled) <- c("Estimate", "SE", "Z", "pvalue")
coefs.pooled$Variable <- c("Intercept", "BATH", "CONC", "DIST_LAND")
coefs.pooled$Low = coefs.pooled$Estimate-coefs.pooled$SE
coefs.pooled$High = coefs.pooled$Estimate+coefs.pooled$SE
coefs.pooled2 <- coefs.pooled[-c(1),]
head(coefs.pooled2)

ggplot(coefs.pooled2) +
  geom_point(aes(x=Estimate, y=Variable)) +
  geom_errorbarh(aes(xmin=Low, xmax=High, y=Variable, col=pvalue<0.05)) +
  #scale_y_discrete(breaks=c("DIST_LAND", "CONC", "BATH"), labels=c("Distance\nto land", "Sea ice\nconcentration", "Ocean depth")) +
  scale_y_discrete(breaks=c("DIST_LAND", "CONC", "BATH"), labels=c("LAND", "CONC", "WATER")) +
  geom_vline(xintercept=0, lty=3, lwd=1) +
  ylab("Habitat covariate") +
  scale_color_hue(l=40) +
  theme_nuwcru()
      # Note that in order to keep the legend title, I had to turn off that line in theme_nuwcru


      # freeze-up RSF
coefs.freeze <- summary(model7_freeze_free_var)$coef$cond %>% as.data.frame
head(coefs.freeze)
names(coefs.freeze) <- c("Estimate", "SE", "Z", "pvalue")
coefs.freeze$Variable <- c("Intercept", "BATH", "DIST_WATER")
coefs.freeze$Low = coefs.freeze$Estimate-coefs.freeze$SE
coefs.freeze$High = coefs.freeze$Estimate+coefs.freeze$SE
coefs.freeze2 <- coefs.freeze[-c(1),]
head(coefs.freeze2)

ggplot(coefs.freeze2) +
  geom_point(aes(x=Estimate, y=Variable)) +
  geom_errorbarh(aes(xmin=Low, xmax=High, y=Variable, col=pvalue<0.05)) +
  scale_y_discrete(breaks=c("BATH", "DIST_WATER"), labels=c("BATH", "WATER")) +
  geom_vline(xintercept=0, lty=3, lwd=1) +
  ylab("Habitat covariate") +
  scale_color_hue(l=40) +
  theme_nuwcru()

      # winter RSF
coefs.winter <- summary(model8_winter_free_var)$coef$cond %>% as.data.frame
head(coefs.winter)
names(coefs.winter) <- c("Estimate", "SE", "Z", "pvalue")
coefs.winter$Variable <- c("Intercept", "CONC", "DIST_LAND")
coefs.winter$Low = coefs.winter$Estimate-coefs.winter$SE
coefs.winter$High = coefs.winter$Estimate+coefs.winter$SE
coefs.winter2 <- coefs.winter[-c(1),]
head(coefs.winter2)

ggplot(coefs.winter2) +
  geom_point(aes(x=Estimate, y=Variable)) +
  geom_errorbarh(aes(xmin=Low, xmax=High, y=Variable, col=pvalue<0.05)) +
  scale_y_discrete(breaks=c("CONC", "DIST_LAND"), labels=c("CONC", "LAND")) +
  geom_vline(xintercept=0, lty=3, lwd=1) +
  ylab("Habitat covariate") +
  scale_color_hue(l=40) +
  theme_nuwcru()


      # break-up RSF
coefs.breakup <- summary(model6_break)$coef$cond %>% as.data.frame
head(coefs.breakup)
names(coefs.breakup) <- c("Estimate", "SE", "Z", "pvalue")
coefs.breakup$Variable <- c("Intercept", "BATH", "DIST_LAND")
coefs.breakup$Low = coefs.breakup$Estimate-coefs.breakup$SE
coefs.breakup$High = coefs.breakup$Estimate+coefs.breakup$SE
coefs.breakup2 <- coefs.breakup[-c(1),]
head(coefs.breakup2)

ggplot(coefs.breakup2) +
  geom_point(aes(x=Estimate, y=Variable)) +
  geom_errorbarh(aes(xmin=Low, xmax=High, y=Variable, col=pvalue<0.05)) +
  scale_y_discrete(breaks=c("BATH", "DIST_LAND"), labels=c("BATH", "LAND")) +
  geom_vline(xintercept=0, lty=3, lwd=1) +
  ylab("Habitat covariate") +
  scale_color_hue(l=40) +
  theme_nuwcru()


coefs.breakup3 <- coefs.breakup2
coefs.breakup3$RSF <- rep("breakup")

coefs.freeze3 <- coefs.freeze2
coefs.freeze3$RSF <- rep("freezeup")

coefs.winter3 <- coefs.winter2
coefs.winter3$RSF <- rep("winter")

coefs.pooled3 <- coefs.pooled2
coefs.pooled3$RSF <- rep("pooled")



all_coefs <- rbind(coefs.breakup3, coefs.freeze3)
all_coefs2 <- rbind(all_coefs, coefs.winter3)
all_coefs3 <- rbind(all_coefs2, coefs.pooled3)
head(all_coefs3)
all_coefs3$RSF <- factor(all_coefs3$RSF, levels=c("pooled", "freezeup", "winter", "breakup"))

ggplot(all_coefs3) +
  geom_point(aes(x=Estimate, y=Variable, col=RSF), position=position_dodge(width=1)) +
  geom_errorbarh(aes(xmin=Low, xmax=High, y=Variable, col=RSF), position=position_dodge(width=1)) +
  scale_y_discrete(breaks=c("DIST_WATER", "DIST_LAND", "CONC", "BATH"), labels=c("WATER", "LAND", "CONC", "BATH")) +
  scale_x_continuous(limits=c(-20, 5), breaks=c(-20, -15, -10, -5, 0, 5), labels=c("-20", "-15", "-10", "-5", "0", "5")) +
  geom_vline(xintercept=0, lty=3, lwd=1) +
  ylab("Habitat covariate") +
  #scale_color_hue(l=40) +
  theme_nuwcru()


ggplot(all_coefs3) +
  geom_hline(yintercept=0, lwd=0.5, colour="grey86") +
  geom_point(aes(x=Variable, y=Estimate, col=RSF), position=position_dodge(width=1), size=2) +
  geom_errorbar(aes(ymin=Low, ymax=High, x=Variable, col=RSF, linetype=pvalue<0.05), position=position_dodge(width=1)) +
  scale_x_discrete(breaks=c("DIST_WATER", "DIST_LAND", "CONC", "BATH"), labels=c("WATER", "LAND", "CONC", "BATH")) +
  scale_y_continuous(limits=c(-20, 5), breaks=c(-20, -15, -10, -5, 0, 5), labels=c("-20", "-15", "-10", "-5", "0", "5")) +
  xlab("Habitat covariate") +
  scale_color_hue(l=40) +
  theme_nuwcru()




