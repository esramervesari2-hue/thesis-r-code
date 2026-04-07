# RQ1 and RQ2

library(lme4)

library(ggplot2)



data$ID    <- factor(data$ID) 

data$Word  <- factor(data$Word)



data$Group <- factor(data$Group, levels = c("L1_Turkish", "Heritage"))



data$Knowledge <- factor(data$Knowledge,levels = c(0, 1),labels = c("Unknown", "Known"))



#interactive model

model <- glmer( Devoicing ~ Group * Knowledge + (1 | ID) + (1 | Word),

                data = data, family = binomial, na.action = na.exclude, 

                 control = glmerControl(optimizer = "bobyqa"))

summary(model)



# non-interactive model

model_final <- glmer(Devoicing ~ Group + Knowledge + (1 | ID) + (1 | Word),

                    data = data, family = binomial, na.action = na.exclude,

                    control = glmerControl(optimizer = "bobyqa"))

summary(model_final)



# Odds ratios

fixef_names <- names(fixef(model_final))

CI_fixef <- confint(model_final, parm = fixef_names, method = "Wald")

OR <- exp(fixef(model_final))



OR_table <- data.frame( Predictor  = fixef_names,

            Odds_Ratio = round(exp(fixef(model_final)), 2),

            CI_lower   = round(exp(CI_fixef[,1]), 2),

             CI_upper   = round(exp(CI_fixef[,2]), 2))

OR_table



#descriptive statistics

library(dplyr)

data_clean <- data %>% filter(!is.na(Devoicing))

#1. Total number of tokens used in RQ1 and RQ2

n_total_tokens <- nrow(data_clean)

n_total_tokens

# 2. Number of tokens by Group

token_by_group <- data_clean %>%   group_by(Group) %>%  summarise(Token_n = n())

token_by_group

#3. Devoicing percentages for Heritage vs. L1 Turkish

devoicing_by_group <- data_clean %>%  group_by(Group) %>%

  summarise( Token_n = n(), Devoiced_n = sum(Devoicing == 1, na.rm = TRUE),

 Devoicing_percent = round(mean(Devoicing, na.rm = TRUE) * 100, 2) )

devoicing_by_group

#4. Devoicing by Lexical Knowledge

table_knowledge <- data %>%  filter(!is.na(Devoicing), !is.na(Knowledge)) %>%

  mutate(Knowledge = factor(Knowledge, levels = c("Known", "Unknown"))) %>%

  group_by(Knowledge) %>% summarise( Token = n(), Devoiced = sum(Devoicing == 1),

    `%` = round(Devoiced / Token * 100, 2), .groups = "drop"   )

table_knowledge



#plot1

# Create prediction data

newdata <- expand.grid( Group = levels(data$Group),Knowledge = levels(data$Knowledge))

# Predict with random effects fixed at 0

newdata$predicted_logit <- predict( model_final,newdata = newdata,re.form = NA)

# Convert to probability

newdata$predicted_prob <- plogis(newdata$predicted_logit)

# Plot

ggplot(newdata, aes(x = Group, y = predicted_prob, fill = Knowledge)) +

  geom_col(position = position_dodge(width = 0.7), width = 0.6) +

  scale_y_continuous(limits = c(0,1)) +

  labs( x = "Group", y = "Estimated devoicing probability", fill = "Lexical Knowledge" ) + theme_minimal(base_size = 13)

#plot2

#FIGURE: Group-based participant-level devoicing distribution

# Participant-level aggregation

df_participant <- data %>%  filter(!is.na(Devoicing)) %>%   group_by(ID, Group) %>%

  summarise(  devoicing_rate = mean(Devoicing), .groups = "drop"   )

# Plot

ggplot(df_participant, aes(x = Group, y = devoicing_rate)) +  geom_boxplot(outlier.shape = NA) +

  geom_jitter(width = 0.1, size = 2, alpha = 0.7) +  labs(  title = "Participant-level Devoicing by Group",

    x = "Group",   y = "Devoicing Rate"  ) +  theme_minimal()





# RQ3

library(dplyr)

library(lme4)



# Heritage speakers only and filtering

data_H <- data3 %>% filter(Group == "Heritage") %>%  filter(!is.na(Devoicing))



# Variable types

data_H$Participant_ID <- as.factor(data_H$Participant_ID)

data_H$Word <- as.factor(data_H$Word)

data_H$Early_German_Exposure <- as.numeric(data_H$`Early_German_Exposure (RQ3-1)`)

data_H$Turkish_Daily_Use <- as.numeric(data_H$`Turkish use in daily life (RQ3-2)`)

data_H$Writing <- as.numeric(data_H$`RQ3-writing_num`)

data_H$Schooling <- as.numeric(data_H$`RQ3-schooling_num`)



# Model RQ3_1 

model_RQ3_1 <- glmer( Devoicing ~ Early_German_Exposure +    (1 | Participant_ID) + (1 | Word),

  data = data_H,   family = binomial(link = "logit"),

  control = glmerControl(optimizer = "bobyqa"))



#Error: boundary (singular) fit: see help('isSingular')

# Word random intercept variance = 0, indicating no systematic difference across words.

#We revise the model by removing the Word random effect.



# # Model RQ3_1 Final

model_RQ3_1_final <- glmer( Devoicing ~ Early_German_Exposure +    (1 | Participant_ID),

  data = data_H, family = binomial(link = "logit"),

  control = glmerControl(optimizer = "bobyqa"))



summary(model_RQ3_1_final)



# Model RQ3_2 

model_RQ3_2= glmer(Devoicing ~ Early_German_Exposure + Turkish_Daily_Use +

        (1 | Participant_ID) +  (1 | Word),       family = binomial, data = data_H)



#Error: boundary (singular) fit: see help('isSingular')

# Word random intercept variance = 0, indicating no systematic difference across words.

#We revise the model by removing the Word random effect.



# # Model RQ3_2 Final simple

model_RQ3_2_simple = glmer( Devoicing ~ Early_German_Exposure + Turkish_Daily_Use +

    (1 | Participant_ID),   family = binomial,   data = data_H)



AIC(model_RQ3_2, model_RQ3_2_simple)

#model_RQ3_2:119.7118 and model_RQ3_2_simple=117.7118, so model_RQ3_2_simple is preferred.

summary(model_RQ3_2_simple)



# Model RQ3_3 Writing

model_RQ3_3_Writing=glmer(Devoicing ~ Writing + (1 | Participant_ID),

      family = binomial, data = data_H)



# Model RQ3_3 Schooling

model_RQ3_3_Schooling=glmer(Devoicing ~ Schooling + (1 | Participant_ID),

      family = binomial, data = data_H)



summary(model_RQ3_3_Writing)

summary(model_RQ3_3_Schooling)

#Heritage Group – Other Variables (Descriptive Table)

table_background <- data_H %>%  filter(Group == "Heritage") %>% group_by(Participant_ID) %>%   

    summarise(Early_German_Exposure = first(Early_German_Exposure),

                      Turkish_Daily_Use = first(Turkish_Daily_Use), Writing = first(Writing),

                      Turkish_Schooling = first(Schooling), .groups = "drop" ) %>%

summarise(Variable=c("Early German Exposure","Turkish Daily Use","Writing", "Turkish Schooling"),

        Mean = c(mean(Early_German_Exposure, na.rm = TRUE),

                 mean(Turkish_Daily_Use, na.rm = TRUE),

                 mean(Writing, na.rm = TRUE),

                 mean(Turkish_Schooling, na.rm = TRUE)),

        SD = c(sd(Early_German_Exposure, na.rm = TRUE),

               sd(Turkish_Daily_Use, na.rm = TRUE),

               sd(Writing, na.rm = TRUE),

               sd(Turkish_Schooling, na.rm = TRUE)),

        Min = c(min(Early_German_Exposure, na.rm = TRUE),

                min(Turkish_Daily_Use, na.rm = TRUE),

                min(Writing, na.rm = TRUE),

                min(Turkish_Schooling, na.rm = TRUE)),

        Max = c(max(Early_German_Exposure, na.rm = TRUE),

                max(Turkish_Daily_Use, na.rm = TRUE),

                max(Writing, na.rm = TRUE),

                max(Turkish_Schooling, na.rm = TRUE))   )

table_background

#FIGURE: Writing → predicted probability plot (RQ3)

library(ggplot2)

# Prediction data

newdata_writing <- data.frame(  Writing = seq(min(data_H$Writing, na.rm = TRUE),

                max(data_H$Writing, na.rm = TRUE),  length.out = 100))

# Random effect = 0 (population-level prediction)

newdata_writing$pred <- predict(model_RQ3_3_Writing, newdata = newdata_writing,

                                type = "response",    re.form = NA)

ggplot(newdata_writing, aes(x = Writing, y = pred)) +  geom_line(size = 1) +

  labs(    title = "Effect of Writing Proficiency on Devoicing Probability",

    x = "Writing Proficiency",    y = "Predicted Probability of Devoicing"  ) +   theme_minimal()
