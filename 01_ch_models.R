#####################################
#### Finite mixture mixed models ####
#####################################

library(flexmix)
library(tidyverse)

load("master_dataframes.RData")

## Order the columns of master_nzffd_final 
master_nzffd_final$absent = 1 - master_nzffd_final$present
colorder = c("present", "absent", "species_code", "card", "FRE3",
              "FRE3MeanDurBetween", "MALF", "MAHF", "MeanFlow", "C_constancy",
              "C_contingency", "C_predictability", "MeanLowFlowDur", 
              "MALF_Mean", "MAHF_Mean", "AnnCV", "nzsegment", "catchment_name",
              "organisation", "methGroup")
test_df = master_nzffd_final[colorder]

# Check variables for normal dist. If not, transform.
summary(test_df)
par(mfrow = c(2,4))

## Check histograms with no transformation
hist(test_df$FRE3, breaks = 30)
hist(test_df$FRE3MeanDurBetween, breaks = 30) ## Transform
hist(test_df$C_constancy, breaks = 30) ## Transform
hist(test_df$C_contingency, breaks = 30)
hist(test_df$MeanLowFlowDur, breaks = 30) ## Transform
hist(test_df$MALF_Mean, breaks = 30) ## Transform
hist(test_df$MAHF_Mean, breaks = 30)
hist(test_df$AnnCV, breaks = 30)

## Check histograms with no transformation
hist(test_df$FRE3, breaks = 30)
hist(log(test_df$FRE3MeanDurBetween), breaks = 30) ## Transform
hist(log(test_df$C_constancy), breaks = 30) ## Transform
hist(test_df$C_contingency, breaks = 30)
hist(log(test_df$MeanLowFlowDur), breaks = 30) ## Transform
hist(log(test_df$MALF_Mean), breaks = 30) ## Transform
hist(log(test_df$MAHF_Mean), breaks = 30)
hist(log(test_df$AnnCV), breaks = 30)

## Transform variables with log
test_df_log <- test_df
test_df_log$FRE3MeanDurBetween = log(test_df$FRE3MeanDurBetween)
test_df_log$C_constancy = log1p(test_df_log$C_constancy)
test_df_log$MeanLowFlowDur = log(test_df_log$MeanLowFlowDur)
test_df_log$MALF_Mean = log1p(test_df_log$MALF_Mean)
test_df_log$MAHF_Mean = log(test_df_log$MAHF_Mean)
test_df_log$AnnCV = log(test_df_log$AnnCV)

# Scale variables
scale_test = test_df_log
scale_test[,4:16] <- scale(scale_test[,4:16])


# Sample 10% of data to run a test model
sampled_df = scale_test %>%
  sample_frac(0.1, replace = FALSE)


#> Test model with one random effect and one predictor giving 3 reps to achieve
#> maximum likelihood and specifying 1:5 clusters as potential clusters. Species
#> is here specified as fixed effect, but potentially species id would be the 
#> latent variable determining the clusters.

model_test = stepFlexmix(
  cbind(present, absent) ~ 1 | nzsegment,
  #> I use the driver FLXMRglmfix as specified in Hamel et al, 2017
  #> for a finite mixture regresion.
  model = FLXMRglmfix(family = "binomial",
                      fixed = ~ FRE3 + C_constancy + C_contingency),
  nrep = 3,   # may want to increase this to 10
  k = 1:5,
  data = scale_test
)

# compare models
model_test
plot(model_test@k, AIC(model_test), type = "b")

# get the best model the smallest AIC
model_best <- getModel(model_test, which = "AIC")

# some model summaries and coefficients
summary(model_best)
plot(model_best)
parameters(model_best)  # guild responses
clusters(model_best)    # guild ID of each data row
summary(flexmix::refit(model_best)) # coefs with SE

# I don't know how to retrieve the random effects...

# playing around with prediction
pred <- predict(model_best)

