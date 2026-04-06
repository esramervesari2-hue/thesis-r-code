# RQ1 and RQ2

library(lme4)
library(ggplot2)

data$ID <- factor(data$ID)
data$Word <- factor(data$Word)

data$Group <- factor(data$Group, levels = c("L1_Turkish", "Heritage"))

data$Knowledge <- factor(
  data$Knowledge,
  levels = c(0, 1),
  labels = c("Unknown", "Known")
)

# Interaction model
model <- glmer(
  Devoicing ~ Group * Knowledge + (1 | ID) + (1 | Word),
  data = data,
  family = binomial,
  na.action = na.exclude,
  control = glmerControl(optimizer = "bobyqa")
)

summary(model)

# Non-interaction model
model_final <- glmer(
  Devoicing ~ Group + Knowledge + (1 | ID) + (1 | Word),
  data = data,
  family = binomial,
  na.action = na.exclude,
  control = glmerControl(optimizer = "bobyqa")
)

summary(model_final)

# Odds ratios
fixef_names <- names(fixef(model_final))
CI_fixef <- confint(model_final, parm = fixef_names, method = "Wald")
OR <- exp(fixef(model_final))

OR_table <- data.frame(
  Predictor = fixef_names,
  Odds_Ratio = round(exp(fixef(model_final)), 2),
  CI_lower = round(exp(CI_fixef[, 1]), 2),
  CI_upper = round(exp(CI_fixef[, 2]), 2)
)

OR_table


# RQ3

library(dplyr)
library(lme4)

# Heritage speakers only and removal of missing Devoicing values
data_H <- data3 %>%
  filter(Group == "Heritage") %>%
  filter(!is.na(Devoicing))

# Variable types
data_H$Participant_ID <- as.factor(data_H$Participant_ID)
data_H$Word <- as.factor(data_H$Word)
data_H$Early_German_Exposure <- as.numeric(data_H$`Early_German_Exposure (RQ3-1)`)
data_H$Turkish_Daily_Use <- as.numeric(data_H$`Turkish use in daily life (RQ3-2)`)
data_H$Writing <- as.numeric(data_H$`RQ3-writing_num`)
data_H$Schooling <- as.numeric(data_H$`RQ3-schooling_num`)

# Model RQ3_1
model_RQ3_1 <- glmer(
  Devoicing ~ Early_German_Exposure + (1 | Participant_ID) + (1 | Word),
  data = data_H,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

# Error: boundary (singular) fit
# The variance of the random intercept for Word was 0, so Word was removed.

# Final model for RQ3_1
model_RQ3_1_final <- glmer(
  Devoicing ~ Early_German_Exposure + (1 | Participant_ID),
  data = data_H,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

summary(model_RQ3_1_final)

# Model RQ3_2
model_RQ3_2 <- glmer(
  Devoicing ~ Early_German_Exposure + Turkish_Daily_Use +
    (1 | Participant_ID) + (1 | Word),
  family = binomial,
  data = data_H
)

# Error: boundary (singular) fit
# The variance of the random intercept for Word was 0, so Word was removed.

# Final simplified model for RQ3_2
model_RQ3_2_simple <- glmer(
  Devoicing ~ Early_German_Exposure + Turkish_Daily_Use +
    (1 | Participant_ID),
  family = binomial,
  data = data_H
)

AIC(model_RQ3_2, model_RQ3_2_simple)

# model_RQ3_2 = 119.7118
# model_RQ3_2_simple = 117.7118
# The simpler model was preferred because it had the lower AIC.

summary(model_RQ3_2_simple)

# Model RQ3_3: Writing
model_RQ3_3_Writing <- glmer(
  Devoicing ~ Writing + (1 | Participant_ID),
  family = binomial,
  data = data_H
)

# Model RQ3_3: Schooling
model_RQ3_3_Schooling <- glmer(
  Devoicing ~ Schooling + (1 | Participant_ID),
  family = binomial,
  data = data_H
)

summary(model_RQ3_3_Writing)
summary(model_RQ3_3_Schooling)
