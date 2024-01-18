#packages
library(tidyverse)
library(lme4)
library(tidyr)
library(tibble)
library(dplyr)
library(reshape2)
library(data.table)
library(readxl)
library(zoo)
library(ggplot2)
library(lmerTest)
library(modelr)
library(zoo)
library(irr)
install.packages("AICcmodavg")
library(AICcmodavg)
#get data
data1 <- read_excel("AllData57949.xlsx")
data2 <- read_excel("AllData57951.xlsx")
data3 <- read_excel("AllData57952.xlsx")
data4 <- read_excel("AllData57953.xlsx")
data5 <- read_excel("AllData57954.xlsx")
AllData <- rbind(data1, data2, data3, data4, data5)
view(AllData)

data1NA <- read_excel("data1.xlsx")
data2NA <- read_excel("data2.xlsx")
data3NA <- read_excel("data3.xlsx")
data4NA <- read_excel("data4.xlsx")
data5NA <- read_excel("data5.xlsx")

hist(data1NA$anxious)
hist(AllDataNA$excited)
hist(AllDataNA$calm)
hist(AllDataNA$bored)

AllDataNA <- rbind(data1NA, data2NA, data3NA, data4NA, data5NA)
#make all "NA" NA
AllData$anxious <- ifelse(AllData$anxious == "NA", NA, AllData$anxious)
AllData$bored <- ifelse(AllData$bored == "NA", NA, AllData$bored)
AllData$excited <- ifelse(AllData$excited == "NA", NA, AllData$excited)
AllData$calm <- ifelse(AllData$calm == "NA", NA, AllData$calm)
AllData$activity_household <- ifelse(AllData$activity_household == "NA", NA, AllData$activity_household)
AllData$activity_exercise <- ifelse(AllData$activity_exercise == "NA", NA, AllData$activity_exercise)
AllData$activity_social <- ifelse(AllData$activity_social == "NA", NA, AllData$activity_social)
AllData$activity_study <- ifelse(AllData$activity_study == "NA", NA, AllData$activity_study)
AllData$activity_relax <- ifelse(AllData$activity_relax == "NA", NA, AllData$activity_relax)
AllData$QuadrantX <- ifelse(AllData$QuadrantX == "NA", NA, AllData$QuadrantX)
AllData$QuadrantY <- ifelse(AllData$QuadrantY == "NA", NA, AllData$QuadrantY)
AllData$PulseRate <- ifelse(AllData$PulseRate == "NA", NA, AllData$PulseRate)
AllData$met <- ifelse(AllData$met == "NA", NA, AllData$met)

names(AllData)[names(AllData) == "QuadrantX"] <- "Valence"
names(AllData)[names(AllData) == "QuadrantY"] <- "Arousal"

AllData <- mutate(AllData, rounded_timestamps = round(TimeStamp, units = "mins")) #round timestamps to the closest minute

last_column <- ncol(AllData) #get this column to be the second

AllData <- AllData %>%
  group_by(rounded_timestamps) %>%
  summarise_all(~ ifelse(all(is.na(.)), NA, na.omit(.))) #group by timestamp

AllData <- AllData %>% select(-3) #delete original timestamp column

#create columns with means over 5 minutes around the value
AllData <- AllData %>%
  group_by(ID) %>%
  mutate(MeanPulseRate5around = rollapply(as.numeric(PulseRate), 
                                          width = 5, FUN = mean, align = "center", fill = NA)) %>% 
  ungroup()

#create column with means over 5 minutes after the value
AllData <- AllData %>%
  group_by(ID) %>% 
  mutate(MeanPulseRate5after = rollapply(as.numeric(PulseRate), 
                                   width = 5, FUN = mean, align = "right", fill = NA)) %>% 
  ungroup()

#create column with means 5 minutes before the value
AllData <- AllData %>%
  group_by(ID) %>% 
  mutate(MeanPulseRate5before = rollapply(as.numeric(PulseRate), 
                                   width = 5, FUN = mean, align = "left", fill = NA)) %>% 
  ungroup()


AllData <- AllData %>%
  group_by(ID) %>%
  mutate(MeanMet5around = rollapply(as.numeric(met), 
                                          width = 5, FUN = mean, align = "center", fill = NA)) %>% 
  ungroup()

AllData <- AllData %>%
  group_by(ID) %>%
  mutate(MeanMet5after = rollapply(as.numeric(met), 
                                    width = 5, FUN = mean, align = "right", fill = NA)) %>% 
  ungroup()

AllData <- AllData %>%
  group_by(ID) %>%
  mutate(MeanMet5before = rollapply(as.numeric(met), 
                                    width = 5, FUN = mean, align = "left", fill = NA)) %>% 
  ungroup()

##weighted mean
values <- seq(-2, 2, length.out = 5) #get values

#get weights
weights <- dnorm(values)

#get distribution
tibble(values = values, weights = weights) %>% 
  ggplot(aes(x = values, y = weights)) +
  geom_point() +
  geom_line() +
  ylim(0, .43) +
  xlim(-2.5, 2.5) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.background = element_blank())
  
#around 5 minutes
AllData <- AllData %>%
  group_by(ID) %>%
  mutate(WeightedMeanPulseRate5around = rollapply(as.numeric(PulseRate), 
                                          width = 5, 
                                          FUN = function(x) weighted.mean(x, w = weights), 
                                          align = "center", fill = NA)) %>%
  ungroup()

#after 5 minutes
AllData <- AllData %>%
  group_by(ID) %>%
  mutate(WeightedMeanPulseRate5after = rollapply(as.numeric(PulseRate), 
                                                   width = 5, 
                                                   FUN = function(x) weighted.mean(x, w = weights), 
                                                   align = "right", fill = NA)) %>%
  ungroup()

#before 5 minutes
AllData <- AllData %>%
  group_by(ID) %>%
  mutate(WeightedMeanPulseRate5before = rollapply(as.numeric(PulseRate), 
                                                   width = 5, 
                                                   FUN = function(x) weighted.mean(x, w = weights), 
                                                   align = "left", fill = NA)) %>%
  ungroup()

#met before 5 minutes
AllData <- AllData %>%
  group_by(ID) %>%
  mutate(WeightedMet5before = rollapply(as.numeric(met), 
                                                  width = 5, 
                                                  FUN = function(x) weighted.mean(x, w = weights), 
                                                  align = "left", fill = NA)) %>%
  ungroup()


AllData <- AllData %>%
  group_by(ID) %>%
  mutate(WeightedMet5around = rollapply(as.numeric(met), 
                                        width = 5, 
                                        FUN = function(x) weighted.mean(x, w = weights), 
                                        align = "center", fill = NA)) %>%
  ungroup()

AllData <- AllData %>%
  group_by(ID) %>%
  mutate(WeightedMet5after = rollapply(as.numeric(met), 
                                        width = 5, 
                                        FUN = function(x) weighted.mean(x, w = weights), 
                                        align = "right", fill = NA)) %>%
  ungroup()


#make data numeric before analysis
AllData$anxious <- as.numeric(AllData$anxious)
AllData$excited <- as.numeric(AllData$excited)
AllData$calm <- as.numeric(AllData$calm)
AllData$bored <- as.numeric(AllData$bored)
AllData$PulseRate <- as.numeric(AllData$PulseRate)
AllData$activity_household <- as.numeric(AllData$activity_household)
AllData$activity_study <- as.numeric(AllData$activity_study)
AllData$activity_exercise <- as.numeric(AllData$activity_exercise)
AllData$activity_social <- as.numeric(AllData$activity_social)
AllData$activity_relax <- as.numeric(AllData$activity_relax)
AllData$met <- as.numeric(AllData$met)
AllData$ID <- as.numeric(AllData$ID) 
AllData$Valence <- as.numeric(AllData$Valence)
AllData$Arousal <- as.numeric(AllData$Arousal)
AllData$rounded_timestamps <- as.POSIXct(AllData$rounded_timestamps, format="%Y-%m-%d %H:%M:%S")

##excited
modelexc1 <- AllData %>% lmer(WeightedMeanPulseRate5around ~ excited + met + (1|ID), data = .)
summary(modelexc1)
modelexc2 <- AllData %>% lmer(WeightedMeanPulseRate5before ~ excited  + met + (1|ID), data = .)
summary(modelexc2)
modelexc3 <- AllData %>% lmer(WeightedMeanPulseRate5after ~ excited  + met + (1|ID), data = .)
summary(modelexc3)

modelexc4 <- AllData %>% lmer(MeanPulseRate5around ~ excited + met + (1|ID), data = .)
summary(modelexc4)
modelexc5 <- AllData %>% lmer(MeanPulseRate5before ~ excited  + met + (1|ID), data = .)
summary(modelexc5)
modelexc6 <- AllData %>% lmer(MeanPulseRate5after ~ excited  + met + (1|ID), data = .)
summary(modelexc6)

modelexc7 <- AllData %>% lmer(excited ~ MeanPulseRate5around + MeanMet5around + (1|ID), data = .)
summary(modelexc7)
modelexc8 <- AllData %>% lmer(excited ~ MeanPulseRate5before + MeanMet5before + (1|ID), data = .)
summary(modelexc8)
modelexc9 <- AllData %>% lmer(excited ~ MeanPulseRate5after + MeanMet5after + (1|ID), data = .)
summary(modelexc9)

modelexc10 <- AllData %>% lmer(excited ~ WeightedMeanPulseRate5around + WeightedMet5around + (1|ID), data = .)
summary(modelexc10)
modelexc11 <- AllData %>% lmer(excited ~ WeightedMeanPulseRate5before + WeightedMet5before + (1|ID), data = .)
summary(modelexc11)
modelexc12 <- AllData %>% lmer(excited ~ WeightedMeanPulseRate5after + WeightedMet5after + (1|ID), data = .)
summary(modelexc12)

modelexc13 <- AllData %>% lmer(excited ~ MeanPulseRate5around + (1|ID), data = .)
summary(modelexc13)
modelexc14 <- AllData %>% lmer(excited ~ MeanPulseRate5before + (1|ID), data = .)
summary(modelexc14)
modelexc15 <- AllData %>% lmer(excited ~ MeanPulseRate5after + (1|ID), data = .)
summary(modelexc15)

modelexc16 <- AllData %>% lmer(excited ~ WeightedMeanPulseRate5around + (1|ID), data = .)
summary(modelexc16)
modelexc17 <- AllData %>% lmer(excited ~ WeightedMeanPulseRate5before + (1|ID), data = .)
summary(modelexc17)
modelexc18 <- AllData %>% lmer(excited ~ WeightedMeanPulseRate5after + (1|ID), data = .)
summary(modelexc18)

##calm
modelcalm1 <- AllData %>% lmer(WeightedMeanPulseRate5around ~ calm + met + (1|ID), data = .)
summary(modelcalm1)
modelcalm2 <- AllData %>% lmer(WeightedMeanPulseRate5before ~ calm + met + (1|ID), data = .)
summary(modelcalm2)
modelcalm3 <- AllData %>% lmer(WeightedMeanPulseRate5after ~ calm + met + (1|ID), data = .)
summary(modelcalm3)

modelcalm4 <- AllData %>% lmer(MeanPulseRate5around ~ calm + met + (1|ID), data = .)
summary(modelcalm4)
modelcalm5 <- AllData %>% lmer(MeanPulseRate5before ~ calm + met + (1|ID), data = .)
summary(modelcalm5)
modelcalm6 <- AllData %>% lmer(MeanPulseRate5after ~ calm + met + (1|ID), data = .)
summary(modelcalm6)

modelcalm7 <- AllData %>% lmer(calm ~ MeanPulseRate5around + MeanMet5around + (1|ID), data = .)
summary(modelcalm7)
modelcalm8 <- AllData %>% lmer(calm ~ MeanPulseRate5before + MeanMet5before + (1|ID), data = .)
summary(modelcalm8)
modelcalm9 <- AllData %>% lmer(calm ~ MeanPulseRate5after + MeanMet5after + (1|ID), data = .)
summary(modelcalm9)

modelcalm10 <- AllData %>% lmer(calm ~ WeightedMeanPulseRate5around+ WeightedMet5around + (1|ID), data = .)
summary(modelcalm10)
modelcalm11 <- AllData %>% lmer(calm ~ WeightedMeanPulseRate5before + WeightedMet5before + (1|ID), data = .)
summary(modelcalm11)
modelcalm12 <- AllData %>% lmer(calm ~ WeightedMeanPulseRate5after + WeightedMet5after + (1|ID), data = .)
summary(modelcalm12)

modelcalm13 <- AllData %>% lmer(calm ~ MeanPulseRate5around + (1|ID), data = .)
summary(modelcalm13)
modelcalm14 <- AllData %>% lmer(calm ~ MeanPulseRate5before + (1|ID), data = .)
summary(modelcalm14)
modelcalm15 <- AllData %>% lmer(calm ~ MeanPulseRate5after+ (1|ID), data = .)
summary(modelcalm15)

modelcalm16 <- AllData %>% lmer(calm ~ WeightedMeanPulseRate5around + (1|ID), data = .)
summary(modelcalm16)
modelcalm17 <- AllData %>% lmer(calm ~ WeightedMeanPulseRate5before + (1|ID), data = .)
summary(modelcalm17)
modelcalm18 <- AllData %>% lmer(calm ~ WeightedMeanPulseRate5after + (1|ID), data = .)
summary(modelcalm18)

###bored
modelbored1 <- AllData %>% lmer(WeightedMeanPulseRate5around ~ bored + met + (1|ID), data = .)
summary(modelbored1)
modelbored2 <- AllData %>% lmer(WeightedMeanPulseRate5before ~ bored + met +  (1|ID), data = .)
summary(modelbored2)
modelbored3<- AllData %>% lmer(WeightedMeanPulseRate5after ~ bored + met + (1|ID), data = .)
summary(modelbored3)

modelbored4 <- AllData %>% lmer(MeanPulseRate5around ~ bored + met + (1|ID), data = .)
summary(modelbored4)
modelbored5 <- AllData %>% lmer(MeanPulseRate5before ~ bored + met +  (1|ID), data = .)
summary(modelbored5)
modelbored6 <- AllData %>% lmer(MeanPulseRate5after ~ bored + met + (1|ID), data = .)
summary(modelbored6)

modelbored7 <- AllData %>% lmer(bored ~ MeanPulseRate5around + MeanMet5around + (1|ID), data = .)
summary(modelbored7)
modelbored8 <- AllData %>% lmer(bored ~ MeanPulseRate5before + MeanMet5before + (1|ID), data = .)
summary(modelbored8)
modelbored9 <- AllData %>% lmer(bored ~ MeanPulseRate5after + MeanMet5after + (1|ID), data = .)
summary(modelbored9)

modelbored10 <- AllData %>% lmer(bored ~ WeightedMeanPulseRate5around + WeightedMet5around + (1|ID), data = .)
summary(modelbored10)
modelbored11 <- AllData %>% lmer(bored ~ WeightedMeanPulseRate5before + WeightedMet5before + (1|ID), data = .)
summary(modelbored11)
modelbored12 <- AllData %>% lmer(bored ~ WeightedMeanPulseRate5after + WeightedMet5after + (1|ID), data = .)
summary(modelbored12)

modelbored13 <- AllData %>% lmer(bored ~ MeanPulseRate5around + (1|ID), data = .)
summary(modelbored13)
modelbored14 <- AllData %>% lmer(bored ~ MeanPulseRate5before + (1|ID), data = .)
summary(modelbored14)
modelbored15 <- AllData %>% lmer(bored ~ MeanPulseRate5after + (1|ID), data = .)
summary(modelbored15)

modelbored16 <- AllData %>% lmer(bored ~ WeightedMeanPulseRate5around + (1|ID), data = .)
summary(modelbored16)
modelbored17 <- AllData %>% lmer(bored ~ WeightedMeanPulseRate5before + (1|ID), data = .)
summary(modelbored17)
modelbored18 <- AllData %>% lmer(bored ~ WeightedMeanPulseRate5after + (1|ID), data = .)
summary(modelbored18)

##anxious
modelanxious1 <- AllData %>% lmer(WeightedMeanPulseRate5around ~ anxious + met + (1|ID), data = .)
summary(modelanxious1)
modelanxious2 <- AllData %>% lmer(WeightedMeanPulseRate5before ~ anxious + met + (1|ID), data = .)
summary(modelanxious2)
modelanxious3<- AllData %>% lmer(WeightedMeanPulseRate5after ~ anxious + met + (1|ID), data = .)
summary(modelanxious3)

modelanxious4 <- AllData %>% lmer(MeanPulseRate5around ~ anxious + met + (1|ID), data = .)
summary(modelanxious4)
modelanxious5 <- AllData %>% lmer(MeanPulseRate5before ~ anxious + met + (1|ID), data = .)
summary(modelanxious5)
modelanxious6<- AllData %>% lmer(MeanPulseRate5after ~ anxious + met + (1|ID), data = .)
summary(modelanxious6)

modelanxious7 <- AllData %>% lmer(anxious ~ MeanPulseRate5around + MeanMet5around + (1|ID), data = .)
summary(modelanxious7)
modelanxious8 <- AllData %>% lmer(anxious ~ MeanPulseRate5before + MeanMet5before + (1|ID), data = .)
summary(modelanxious8)
modelanxious9 <- AllData %>% lmer(anxious ~ MeanPulseRate5after + MeanMet5after + (1|ID), data = .)
summary(modelanxious9)

modelanxious10 <- AllData %>% lmer(anxious ~ WeightedMeanPulseRate5around + WeightedMet5around + (1|ID), data = .)
summary(modelanxious10)
modelanxious11 <- AllData %>% lmer(anxious ~ WeightedMeanPulseRate5before + WeightedMet5before + (1|ID), data = .)
summary(modelanxious11)
modelanxious12 <- AllData %>% lmer(anxious ~ WeightedMeanPulseRate5after + WeightedMet5after + (1|ID), data = .)
summary(modelanxious12)

modelanxious13 <- AllData %>% lmer(anxious ~ MeanPulseRate5around + (1|ID), data = .)
summary(modelanxious13)
modelanxious14 <- AllData %>% lmer(anxious ~ MeanPulseRate5before + (1|ID), data = .)
summary(modelanxious14)
modelanxious15 <- AllData %>% lmer(anxious ~ MeanPulseRate5after + (1|ID), data = .)
summary(modelanxious15)

modelanxious16 <- AllData %>% lmer(anxious ~ WeightedMeanPulseRate5around + (1|ID), data = .)
summary(modelanxious16)
modelanxious17 <- AllData %>% lmer(anxious ~ WeightedMeanPulseRate5before + (1|ID), data = .)
summary(modelanxious17)
modelanxious18 <- AllData %>% lmer(anxious ~ WeightedMeanPulseRate5after + (1|ID), data = .)
summary(modelanxious18)

#Valence
modelValence1 <- AllData %>% lmer(WeightedMeanPulseRate5around ~ Valence + met + (1|ID), data = .)
summary(modelValence1)
modelValence2 <- AllData %>% lmer(WeightedMeanPulseRate5before ~ Valence + met +  (1|ID), data = .)
summary(modelValence2)
modelValence3<- AllData %>% lmer(WeightedMeanPulseRate5after ~ Valence + met + (1|ID), data = .)
summary(modelValence3)

modelValence4 <- AllData %>% lmer(MeanPulseRate5around ~ Valence + met + (1|ID), data = .)
summary(modelValence4)
modelValence5 <- AllData %>% lmer(MeanPulseRate5before ~ Valence + met +  (1|ID), data = .)
summary(modelValence5)
modelValence6 <- AllData %>% lmer(MeanPulseRate5after ~ Valence + met + (1|ID), data = .)
summary(modelValence6)

modelValence7 <- AllData %>% lmer(Valence ~ MeanPulseRate5around + MeanMet5around + (1|ID), data = .)
summary(modelValence7)
modelValence8 <- AllData %>% lmer(Valence ~ MeanPulseRate5before + MeanMet5before + (1|ID), data = .)
summary(modelValence8)
modelValence9 <- AllData %>% lmer(Valence ~ MeanPulseRate5after + MeanMet5after + (1|ID), data = .)
summary(modelValence9)

modelValence10 <- AllData %>% lmer(Valence ~ WeightedMeanPulseRate5around + WeightedMet5around + (1|ID), data = .)
summary(modelValence10)
modelValence11 <- AllData %>% lmer(Valence ~ WeightedMeanPulseRate5before + WeightedMet5before + (1|ID), data = .)
summary(modelValence11)
modelValence12 <- AllData %>% lmer(Valence ~ WeightedMeanPulseRate5after + WeightedMet5after + (1|ID), data = .)
summary(modelValence12)

modelValence13 <- AllData %>% lmer(Valence ~ MeanPulseRate5around + (1|ID), data = .)
summary(modelValence13)
modelValence14 <- AllData %>% lmer(Valence ~ MeanPulseRate5before + (1|ID), data = .)
summary(modelValence14)
modelValence15 <- AllData %>% lmer(Valence ~ MeanPulseRate5after + (1|ID), data = .)
summary(modelValence15)

modelValence16 <- AllData %>% lmer(Valence ~ WeightedMeanPulseRate5around + (1|ID), data = .)
summary(modelValence16)
modelValence17 <- AllData %>% lmer(Valence ~ WeightedMeanPulseRate5before + (1|ID), data = .)
summary(modelValence17)
modelValence18 <- AllData %>% lmer(Valence ~ WeightedMeanPulseRate5after + (1|ID), data = .)
summary(modelValence18)

#Arousal
modelArousal1 <- AllData %>% lmer(WeightedMeanPulseRate5around ~ Arousal + met + (1|ID), data = .)
summary(modelArousal1)
modelArousal2 <- AllData %>% lmer(WeightedMeanPulseRate5before ~ Arousal + met +  (1|ID), data = .)
summary(modelArousal2)
modelArousal3<- AllData %>% lmer(WeightedMeanPulseRate5after ~ Arousal + met + (1|ID), data = .)
summary(modelArousal3)

modelArousal4 <- AllData %>% lmer(MeanPulseRate5around ~ Arousal + met + (1|ID), data = .)
summary(modelArousal4)
modelArousal5 <- AllData %>% lmer(MeanPulseRate5before ~ Arousal + met +  (1|ID), data = .)
summary(modelArousal5)
modelArousal6 <- AllData %>% lmer(MeanPulseRate5after ~ Arousal + met + (1|ID), data = .)
summary(modelArousal6)

modelArousal7 <- AllData %>% lmer(Arousal ~ MeanPulseRate5around + MeanMet5around + (1|ID), data = .)
summary(modelArousal7)
modelArousal8 <- AllData %>% lmer(Arousal ~ MeanPulseRate5before + MeanMet5before + (1|ID), data = .)
summary(modelArousal8)
modelArousal9 <- AllData %>% lmer(Arousal ~ MeanPulseRate5after + MeanMet5after + (1|ID), data = .)
summary(modelArousal9)

modelArousal10 <- AllData %>% lmer(Arousal ~ WeightedMeanPulseRate5around + WeightedMet5around + (1|ID), data = .)
summary(modelArousal10)
modelArousal11 <- AllData %>% lmer(Arousal ~ WeightedMeanPulseRate5before + WeightedMet5before + (1|ID), data = .)
summary(modelArousal11)
modelArousal12 <- AllData %>% lmer(Arousal ~ WeightedMeanPulseRate5after + WeightedMet5after + (1|ID), data = .)
summary(modelArousal12)

modelArousal13 <- AllData %>% lmer(Arousal ~ MeanPulseRate5around + (1|ID), data = .)
summary(modelArousal13)
modelArousal14 <- AllData %>% lmer(Arousal ~ MeanPulseRate5before + (1|ID), data = .)
summary(modelArousal14)
modelArousal15 <- AllData %>% lmer(Arousal ~ MeanPulseRate5after + (1|ID), data = .)
summary(modelArousal15)

modelArousal16 <- AllData %>% lmer(Arousal ~ WeightedMeanPulseRate5around + (1|ID), data = .)
summary(modelArousal16)
modelArousal17 <- AllData %>% lmer(Arousal ~ WeightedMeanPulseRate5before + (1|ID), data = .)
summary(modelArousal17)
modelArousal18 <- AllData %>% lmer(Arousal ~ WeightedMeanPulseRate5after + (1|ID), data = .)
summary(modelArousal18)

#Look for a random scatter of points around the horizontal line. 
# If there's a clear pattern or curvature, it may indicate a violation of the linearity assumption.
plot(residuals(modelArousal13) ~ fitted(modelValence13))
abline(h = 0, col = "red")  # Add a horizontal line at 0

qqnorm(resid(modelValence17))
qqline(resid(modelValence14))

mean_pulse <- mean(AllData$anxious, na.rm = TRUE)

# Calculate standard deviation
sd_pulse <- sd(AllData$anxious, na.rm = TRUE)

# Calculate minimum
min_pulse <- min(AllData$anxious, na.rm = TRUE)

# Calculate maximum
max_pulse <- max(AllData$anxious, na.rm = TRUE)

# Print the results
cat("Mean: ", mean_pulse, "\n")
cat("Standard Deviation: ", sd_pulse, "\n")
cat("Minimum: ", min_pulse, "\n")
cat("Maximum: ", max_pulse, "\n")

#visualize the emotions over time per participant
plot <- ggplot(dataNA3, aes(x = rounded_timestamps)) +
  geom_line(aes(y = anxious, linetype = "anxious"), size = 0.5) +
  geom_line(aes(y = bored, linetype = "bored"), size = 0.5) +
  geom_line(aes(y = excited, linetype = "excited"), size = 0.5) +
  geom_line(aes(y = calm, linetype = "calm"), size = 0.5) +
  geom_point(aes(y = anxious, shape = "anxious"), size = 2) +
  geom_point(aes(y = bored, shape = "bored"), size = 2) +
  geom_point(aes(y = excited, shape = "excited"), size = 2) +
  geom_point(aes(y = calm, shape = "calm"), size = 2) +
  labs(title = NULL,
       x = "Time",
       y = "Score") +
  scale_linetype_manual(values = c("anxious" = 1,"bored" = 2, "excited" = 3, "calm" = 4)) +
  scale_shape_manual(values = c("anxious" = 1, "bored" = 2, "excited" = 3, "calm" = 4)) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.background = element_blank()) +
  guides(
    shape = guide_legend(title = "Emotion"),
    linetype = guide_legend(title = "Emotion"))
       

print(plot)


#get icc
icc_result <- AllData %>%
  group_by(ID) %>%
  irr::icc(c("PulseRate"), model = "twoway", type = "consistency", unit = "single")
# Display the ICC result for pulse rate
print(icc_result)
AllData %>% icc(AllData$PulseRate, group = ID)

#get AIC
aic_value <- AIC(modelArousal18)

# Display the AIC value
print(aic_value)


models <- list(modelexc13, modelexc14, modelexc15, modelexc16, modelexc17, modelexc18)

model.names <- c('modelexc13', 'modelexc14', 'modelexc15', 'modelexc16', 'modelexc17', 'modelexc18')
aictab(cand.set = models, modnames = model.names)
