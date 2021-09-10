

# token: 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


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
library(AICcmodavg)
library(MuMIn)

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


# 3. - SKIP - Analyze top pooled RSF model results --------

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


# 4. - SKIP - Analyze top seasonal RSF model (freeze-up) results--------

# BATH + CONC (Model 5)
model5_tmp_freeze <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model5_tmp_freeze$parameters$theta[1] = log(1e3)
model5_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:2)))
model5_freeze <- glmmTMB:::fitTMB(model5_tmp_freeze) 
summary(model5_freeze)


# re-run model with unscaled values
#unscale_model7_freeze_free_var <- glmmTMB(USED_AVAIL~BATH+DIST_WATER+(1|ID)+(0+BATH|ID)+(0+DIST_WATER|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=TRUE, weights=W)
#summary(unscale_model7_freeze_free_var)

# get mean values
#mean(used_avail_RSF_freezeup_FINAL$CONC)

# get AICc with AICcmodavg
AICc(model5_freeze) # this and the line below both give 3,960.287, which is what the regular AIC value was
AICc(model5_freeze, return.K = FALSE)

# get AICc with MuMln
AICc(model5_freeze) # same as above


# 5. - SKIP - Analyze top seasonal RSF model (winter) results--------

# winter: Model 8 (CONT + DIST_LAND) - same issue as above
model10_winter_free_var <- glmmTMB(USED_AVAIL~DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=TRUE, weights=W)
summary(model10_winter_free_var)
AIC(model10_winter_free_var)

# re-run model with unscaled values
#unscale_model8_winter_free_var <- glmmTMB(USED_AVAIL~CONC+DIST_LAND+(1|ID)+(0+CONC|ID)+(0+DIST_LAND|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=TRUE, weights=W)
#summary(unscale_model8_winter_free_var)

# get mean values
#mean(used_avail_RSF_winter_FINAL$DIST_WATER_M)

# get AICc with AICcmodavg
AICc(model10_winter_free_var) # this and the line below both give 5142.009, which is what the regular AIC value was
AICc(model10_winter_free_var, return.K = FALSE)

# get AICc with MuMln
AICc(model10_winter_free_var) # same as above


# 6. - SKIP - Analyze top seasonal RSF model (break-up) results--------

# break-up: Model 6 (BATH + DIST_LAND)
model6_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
model6_tmp_break$parameters$theta[1] = log(1e3)
model6_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))
model6_break <- glmmTMB:::fitTMB(model6_tmp_break) 
summary(model6_break)
AIC(model6_break)

# re-run model with unscaled values
#unscale_model6_tmp_break <- glmmTMB(USED_AVAIL~BATH+DIST_LAND+(1|ID)+(0+BATH|ID)+(0+DIST_LAND|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
#unscale_model6_tmp_break$parameters$theta[1] = log(1e3)
#unscale_model6_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))
#unscale_model6_break <- glmmTMB:::fitTMB(unscale_model6_tmp_break) 
#summary(unscale_model6_break)

# get mean values
#mean(used_avail_RSF_breakup_FINAL$DIST_WATER_M)

# get AICc with AICcmodavg
AICc(model6_break) # this and the line below both give 7259.645, which is what the regular AIC value was
AICc(model6_break, return.K = FALSE)

# get AICc with MuMln
AICc(model6_break) # same as above


# 7. - SKIP - Creating manuscript figures ----------

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
\




# 8. Getting final #s per season - without pooled RSF (July 2021) -----

# 1. import data
used_avail_RSF_winter_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_winter_FINAL_Apr2021.csv")
used_avail_RSF_breakup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_breakup_FINAL_Apr2021.csv")
used_avail_RSF_freezeup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_freezeup_FINAL_Apr2021.csv")

# 2. filter and count
freeze_used <- used_avail_RSF_freezeup_FINAL %>% filter(USE=="used")
freeze_avail <- used_avail_RSF_freezeup_FINAL %>% filter(USE=="available")
winter_used <- used_avail_RSF_winter_FINAL %>% filter(USE=="used")
winter_avail <- used_avail_RSF_winter_FINAL %>% filter(USE=="available")
break_used <- used_avail_RSF_breakup_FINAL %>% filter(USE=="used")
break_avail <- used_avail_RSF_breakup_FINAL %>% filter(USE=="available")

freeze_mean <- freeze_used %>% group_by(ID) %>% summarize(count=n())
winter_mean <- winter_used %>% group_by(ID) %>% summarize(count=n())
break_mean <- break_used %>% group_by(ID) %>% summarize(count=n())


# 9. Plot coefficients FINAL (July 2021) ------

# see section 3: https://terpconnect.umd.edu/~egurarie/teaching/SpatialModelling_AKTWS2018/6_RSF_SSF.html
# we ended up removing some of the models, so the top models for most seasons have changed (break-up stayed the same)
# we're also not looking at the pooled model anymore

# need this package
library(sjPlot)

# 1. import data
used_avail_RSF_pooled_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_pooled_FINAL_Apr2021.csv")
used_avail_RSF_winter_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_winter_FINAL_Apr2021.csv")
used_avail_RSF_breakup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_breakup_FINAL_Apr2021.csv")
used_avail_RSF_freezeup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_freezeup_FINAL_Apr2021.csv")


# 2. re-run top models for each season then plot - NOTE THAT THE TOP MODELS ARE DIFFERENT NOW 

      # freeze-up: BATH + CONC 
model5_tmp_freeze <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model5_tmp_freeze$parameters$theta[1] = log(1e3)
model5_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:2)))
model5_freeze <- glmmTMB:::fitTMB(model5_tmp_freeze) 
summary(model5_freeze)
summary(model5_freeze)$coef

                    # unscaled version
model5_tmp_freeze_unscale <- glmmTMB(USED_AVAIL~BATH+CONC+(1|ID)+(0+BATH|ID)+(0+CONC|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model5_tmp_freeze_unscale$parameters$theta[1] = log(1e3)
model5_tmp_freeze_unscale$mapArg = list(theta = factor(c(NA, 1:2)))
model5_freeze_unscale <- glmmTMB:::fitTMB(model5_tmp_freeze_unscale) 
summary(model5_freeze_unscale)
summary(model5_freeze_unscale)$coef


      # winter: LAND + WATER
model10_winter_free_var <- glmmTMB(USED_AVAIL~DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=TRUE, weights=W)
summary(model10_winter_free_var)
summary(model10_winter_free_var)$coef

                    # unscaled version
model10_winter_free_var_unscale <- glmmTMB(USED_AVAIL~DIST_LAND+DIST_WATER+(1|ID)+(0+DIST_LAND|ID)+(0+DIST_WATER|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=TRUE, weights=W)
summary(model10_winter_free_var_unscale)$coef

      # break-up: BATH + LAND
model6_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
model6_tmp_break$parameters$theta[1] = log(1e3)
model6_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))
model6_break <- glmmTMB:::fitTMB(model6_tmp_break) 
summary(model6_break)
summary(model6_break)$coef

                    # unscaled version
model6_tmp_break_unscale <- glmmTMB(USED_AVAIL~BATH+DIST_LAND+(1|ID)+(0+BATH|ID)+(0+DIST_LAND|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
model6_tmp_break_unscale$parameters$theta[1] = log(1e3)
model6_tmp_break_unscale$mapArg = list(theta = factor(c(NA, 1:2)))
model6_break_unscale <- glmmTMB:::fitTMB(model6_tmp_break_unscale) 
summary(model6_break_unscale)
summary(model6_break_unscale)$coef



# 3. create dataframe of coefficients and SEs to create a better plot of them alltogether

coefficients <- data.frame(matrix(NA, nrow=6, ncol=5))
colnames(coefficients) <- c("RSF", "Covariate", "Coef", "SE", "P")
coefficients$RSF <- as.character(c("freeze", "freeze", "winter", "winter", "break", "break"))
coefficients$Covariate <- as.character(c("BATH", "CONC", "LAND", "WATER", "BATH", "LAND"))
coefficients$Coef <- as.numeric(c("0.66564536", "-0.63706218", "-9.426761140", "0.003477395", "0.231327879", "-5.713937127"))
coefficients$SE <- as.numeric(c("0.11849883", "0.09123488", "8.0580802", "0.1068137", "0.1115658", "4.0714307"))
coefficients$P <- as.numeric(c("1.939466e-08", "2.896414e-12", "0.24206054", "0.97402890", "0.03812889", "0.16049092"))
str(coefficients)
coefficients$Low = coefficients$Coef-coefficients$SE
coefficients$High = coefficients$Coef+coefficients$SE
head(coefficients)


# 4. plot 

ggplot(data=coefficients, aes(linetype=P<0.05)) +
  geom_point(aes(x=Covariate, y=Coef, col=RSF), position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=Low, ymax=High, x=Covariate, col=RSF), position=position_dodge(width=0.5)) +
  scale_colour_manual(values=c("deepskyblue3", "darkolivegreen4", "pink3")) +
  xlab("Habitat covariate") +
  ylab("Coefficient") +
  labs(fill="RSF") +
  scale_fill_discrete(name="RSF", labels=c("break-up", "freeze-up", "winter")) +
  theme_nuwcru()




###

# Ignore below

###

# freeze-up
coefs.tmb.freeze <- summary(model5_freeze)$coef$cond %>% as.data.frame()
head(coefs.tmb.freeze)
coefs.tmb.freeze2 <- setDT(coefs.tmb.freeze, keep.rownames=TRUE)
head(coefs.tmb.freeze2)
names(coefs.tmb.freeze2)[1] <- "Covariate"
names(coefs.tmb.freeze2)[3] <- "SE"
names(coefs.tmb.freeze2)[4] <- "Z"
names(coefs.tmb.freeze2)[5] <- "P"
coefs.tmb.freeze3 = coefs.tmb.freeze2[-1,]
coefs.tmb.freeze3$LOW = coefs.tmb.freeze3$Estimate-2*coefs.tmb.freeze3$SE
coefs.tmb.freeze3$HIGH = coefs.tmb.freeze3$Estimate+2*coefs.tmb.freeze3$SE

freeze_plot <- ggplot(data=coefs.tmb.freeze3, aes(col=P<0.05)) +
  geom_point(aes(x=Covariate, y=Estimate)) +
  geom_errorbar(aes(ymin=LOW, ymax=HIGH, x=Covariate))
freeze_plot

# winter
coefs.tmb.winter <- summary(model10_winter_free_var)$coef$cond %>% as.data.frame()
head(coefs.tmb.winter)
coefs.tmb.winter2 <- setDT(coefs.tmb.winter, keep.rownames=TRUE)
head(coefs.tmb.winter2)
names(coefs.tmb.winter2)[1] <- "Covariate"
names(coefs.tmb.winter2)[3] <- "SE"
names(coefs.tmb.winter2)[4] <- "Z"
names(coefs.tmb.winter2)[5] <- "P"
coefs.tmb.winter3 = coefs.tmb.winter2[-1,]
coefs.tmb.winter3$LOW = coefs.tmb.winter3$Estimate-2*coefs.tmb.winter3$SE
coefs.tmb.winter3$HIGH = coefs.tmb.winter3$Estimate+2*coefs.tmb.winter3$SE

winter_plot <- ggplot(data=coefs.tmb.winter3, aes(col=P<0.05)) +
  geom_point(aes(x=Covariate, y=Estimate)) +
  geom_errorbar(aes(ymin=LOW, ymax=HIGH, x=Covariate))
winter_plot


# break-up
coefs.tmb.break <- summary(model6_break)$coef$cond %>% as.data.frame()
head(coefs.tmb.break)
coefs.tmb.break2 <- setDT(coefs.tmb.break, keep.rownames=TRUE)
head(coefs.tmb.break2)
names(coefs.tmb.break2)[1] <- "Covariate"
names(coefs.tmb.break2)[3] <- "SE"
names(coefs.tmb.break2)[4] <- "Z"
names(coefs.tmb.break2)[5] <- "P"
coefs.tmb.break3 = coefs.tmb.break2[-1,]
coefs.tmb.break3$LOW = coefs.tmb.break3$Estimate-2*coefs.tmb.break3$SE
coefs.tmb.break3$HIGH = coefs.tmb.break3$Estimate+2*coefs.tmb.break3$SE

break_plot <- ggplot(data=coefs.tmb.break3, aes(col=P<0.05)) +
  geom_point(aes(x=Covariate, y=Estimate)) +
  geom_errorbar(aes(ymin=LOW, ymax=HIGH, x=Covariate))
break_plot


###

# combine plots together

library("gridExtra")

grid.arrange(freeze_plot, winter_plot, break_plot)


##


# 10. Plot relative probability FINAL (July 2021) -------

# we ended up removing some of the models, so the top models for most seasons have changed (break-up stayed the same)
# we're also not looking at the pooled model anymore

# 1. import and format data
used_avail_RSF_winter_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_winter_FINAL_Apr2021.csv")
used_avail_RSF_breakup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_breakup_FINAL_Apr2021.csv")
used_avail_RSF_freezeup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_freezeup_FINAL_Apr2021.csv")

freeze_used <- used_avail_RSF_freezeup_FINAL %>% filter(USE=="used")
winter_used <- used_avail_RSF_winter_FINAL %>% filter(USE=="used")
break_used <- used_avail_RSF_breakup_FINAL %>% filter(USE=="used")


# 2. create seasonal plots

      # freeze-up top model included: BATH & CONC

ggplot(used_avail_RSF_freezeup_FINAL) + geom_density(aes(x=BATH, fill=USE), alpha=0.2) +
  labs(x="Depth (m)", y="Relative Probability") +
  ggtitle("Bathymetry") +
  theme(legend.title=element_blank()) +
  scale_x_continuous(limits=c(-4000, 300)) +
  theme_nuwcru()

ggplot(used_avail_RSF_freezeup_FINAL) + geom_density(aes(x=CONC, fill=USE), alpha=0.2) +
  labs(x="Concentration (%)", y="Relative Probability") +
  ggtitle("Sea ice concentration") +
  theme(legend.title=element_blank()) +
  theme_nuwcru()


# winter top model included: CONC and DIST_LAND

ggplot(used_avail_RSF_winter_FINAL) + geom_density(aes(x=DIST_WATER, fill=USE), alpha=0.2) +
  labs(x="Distance (km)", y="Relative Probability") +
  ggtitle("Distance to water") +
  #scale_x_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels=c("0", "20", "40", "60", "80", "100")) +
  theme(legend.title=element_blank()) +
  theme_nuwcru()

ggplot(used_avail_RSF_winter_FINAL) + geom_density(aes(x=DIST_LAND, fill=USE), alpha=0.2) +
  labs(x="Distance (km)", y="Relative Probability") +
  ggtitle("Distance to land") +
  theme(legend.title=element_blank()) +
  scale_x_continuous(limits=c(0, 500000), breaks=c(0, 100000, 200000, 300000, 400000, 500000), labels=c("0", "100000", "200000", "300000", "400000", "500000")) +
  theme_nuwcru()


# break-up top model included: CONC and DIST_LAND
ggplot(used_avail_RSF_breakup_FINAL) + geom_density(aes(x=BATH, fill=USE), alpha=0.2) +
  labs(x="Depth (m)", y="Relative Probability") +
  ggtitle("Bathymetry") +
  scale_x_continuous(limits=c(-4000, 300)) +
  theme(legend.title=element_blank()) +
  theme_nuwcru()

ggplot(used_avail_RSF_breakup_FINAL) + geom_density(aes(x=DIST_LAND, fill=USED_AVAIL), alpha=0.2) +
  labs(x="Distance (km)", y="Relative Probability") +
  ggtitle("Distance to land") +
  theme(legend.title=element_blank()) +
  scale_x_continuous(limits=c(0, 400000), breaks=c(0, 100000, 200000, 300000, 400000), labels=c("0", "100000", "200000", "300000", "400000")) +
  theme_nuwcru()

summary(used_avail_RSF_breakup_FINAL)



# 11. Plot from Samantha Morin (Morin, et al. 2020) ----

# try with one season first
# import data then run top model
used_avail_RSF_freezeup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_freezeup_FINAL_Apr2021.csv")
# Freeze-up: BATH + CONC (Model 5)
model5_tmp_freeze <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL,
                             map=list(theta=factor(c(NA, 2))), start=list(theta=c(log(1000), 0)),
                             doFit=F, weights=W)
model5_tmp_freeze$parameters$theta[1] = log(1e3)
model5_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:2)))
model5_freeze <- glmmTMB:::fitTMB(model5_tmp_freeze) 
summary(model5_freeze)


###

## script below is now Samantha's
        # Need ggplot and mcclogit

###

library(mclogit)

# Pred functions
make_pred <- function(myvar,
                      model,
                      mydata){
  
  newdata <- data.frame(ID=as.character(rep("bear1",8500)),
                        BATH=mean(mydata$BATH, na.rm=T),
                        CONC=mean(mydata$CONC,na.rm=T))
  newdata[,myvar] <- seq(min(mydata[,myvar],na.rm=T),
                         max(mydata[,myvar],na.rm=T),
                         length.out = 8500)
  
  coefs <- summary(model)
  coefs <- data.frame(coefs$coefficients)
  coefs$Var <- row.names(coefs)
  
  # Fit
  var_prods <- c()
  for(v in coefs$Var){
    prods <- coefs$Estimate[coefs$Var == v]*newdata[,v]
    var_prods <- cbind(var_prods,prods)
  }
  var_prods <- data.frame(var_prods)
  colnames(var_prods) <- coefs$Var
  newdata$Pred <- rowSums(var_prods)
  
  # SE
  var_prods_upper <- var_prods
  var_prods_upper[,myvar] <- var_prods_upper[,myvar] + 1.96*coefs$Std..Error[coefs$Var == myvar]
  newdata$Upper <- rowSums(var_prods_upper)
  
  var_prods_lower <- var_prods
  var_prods_lower[,myvar] <- var_prods_lower[,myvar] - 1.96*coefs$Std..Error[coefs$Var == myvar]
  newdata$Lower <- rowSums(var_prods_lower)
  
  newdata$var <- myvar
  newdata$value <- newdata[,myvar]
  return(newdata[,c("var",'value',"Pred","Upper","Lower")])
}


myvars <- c("BATH", "CONC")
bobpreds <- lapply(myvars, make_pred, model=model5_freeze, mydata=used_avail_RSF_freezeup_FINAL)
bobpreds <- do.call("rbind",bobpreds)
bobpreds$Species <-"Bobcat"
lynxpreds <- lapply(myvars,make_pred,model = thirdlynx20,mydata = lynx20)
lynxpreds <- do.call("rbind",lynxpreds)
lynxpreds$Species <-"Lynx"
catpreds <- rbind(bobpreds,lynxpreds)
catpreds$Prob <- exp(catpreds$Pred)/(exp(catpreds$Pred)+1)
catpreds$Pupper <- exp(catpreds$Upper)/(exp(catpreds$Upper)+1)
catpreds$Plower <- exp(catpreds$Lower)/(exp(catpreds$Lower)+1)

x11(12,12)
ggplot(catpreds, aes(y = Prob, x = value, 
                     group= Species,
                     color = Species)) +
  geom_line() +
  geom_ribbon(aes(ymin=Plower, ymax=Pupper, fill = Species),
              colour = NA, alpha = 0.2) +
  theme_bw() +
  ylab("Probability of use") +
  xlab("") +
  facet_wrap(~var,scales = "free_x",nrow = 3)



# 12. Help from Kylee ----


# try with one season first
# import data then run top model
used_avail_RSF_freezeup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_freezeup_FINAL_Apr2021.csv")
# Freeze-up: BATH + CONC (Model 5)
model5_tmp_freeze <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), 
                             data=used_avail_RSF_freezeup_FINAL, map=list(theta=factor(c(NA, 2))), start=list(theta=c(log(1000), 0)),
                             doFit=F, weights=W)
model5_tmp_freeze$parameters$theta[1] = log(1e3)
model5_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:2)))
model5_freeze <- glmmTMB:::fitTMB(model5_tmp_freeze) 
summary(model5_freeze)
coef(model5_freeze)

# Kylee's loop

int <- 1.000e+06
beta1 <- 2.911e-10
beta2 <- 3.122e-09
cov1 <- used_avail_RSF_freezeup_FINAL$BATH_SCALED
cov2 <- used_avail_RSF_freezeup_FINAL$CONC_SCALED

predict_use <- function(int, beta1, beta2, cov1, cov2) {
  predict_lam <- c()
  predict_prob <- c()
  for(i in 1:nrow(cov)) {
    predict_lam[i] <- exp(int + beta1*cov1[i] + beta2*cov2[i])
    
    predict_prob[i] <- 1-exp(-predict_lam[i])
  }
  predict_prob <- as.data.frame(predict_prob)
  return(predict_prob)
}

predict_use(int, beta1, beta2, cov1, cov2)


saveRDS(model5_freeze, file="model5_freeze.RDS")

