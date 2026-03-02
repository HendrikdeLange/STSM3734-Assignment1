library(car)
library(ggplot2)
df <- read.csv("~/STSM3734-MAIZE_SIMULATION/obs_data/Observational_Group14.csv", sep=";", dec=".")

#Covert to factors
df$x_sex <- factor(df$x_sex, levels = c(0,1), labels = c("Female","Male"))
df$x_pt  <- factor(df$x_pt,  levels = c(0,1), labels = c("NO","YES"))

#Making column names serious
colnames(df) <- c("gender", "personal_trainer", "visits",
                  "height", "caloric_balance", "time", "weight_y")




# library(GGally)
# 
# # Full scatterplot matrix — everything vs everything including weight_y
# ggpairs(df, 
#         columns = c("weight_y", "height", "caloric_balance", "visits", "time"),
#         aes(color = gender, alpha = 0.3),
#         upper = list(continuous = "cor"),    # correlation in upper triangle
#         lower = list(continuous = "points"), # scatterplots in lower triangle
#         diag  = list(continuous = "densityDiag")) # density on diagonal


#what I observed
#time and visits have a strong correlation
# likely a formula was set such that visits was sampled from (1-7) and 
# then time is some function of visits + noise

#weight for males > weight for females (Normaln Dist)
#weight for males > weight for females (Normal Dist)

#height is the biggest driver of weight with a correlation of 0.859

#women dont visit more than 4 times a week



#naive model
naive_model <- lm(weight_y ~ gender + personal_trainer + visits + height +
              caloric_balance + time,
            data = df)   

summary(naive_model) #to beat R^2 = 0.7746 


# 2. Check Variance Inflation Factors (VIF)
# If any VIF is > 5, you have significant multicollinearity.
# If VIF > 10, the model is likely unreliable.
vif(naive_model)


# Define competing models
# Model A: The full kitchen sink
model_full <- lm(weight_y ~ gender + personal_trainer + visits + height + 
                   caloric_balance + time, data = df)

# Model B: Remove Time (Assume Visits captures the "effort")
model_no_time <- lm(weight_y ~ gender + personal_trainer + visits + height + 
                      caloric_balance, data = df)

# Model C: Remove Visits (Assume Time captures the "effort")
model_no_visits <- lm(weight_y ~ gender + personal_trainer + time + height + 
                        caloric_balance, data = df)

# Compare them using AIC and BIC
# Lower values are better. They balance goodness-of-fit with simplicity.
AIC(model_full, model_no_time, model_no_visits)
BIC(model_full, model_no_time, model_no_visits)

# Once you identify the "winner" (the model with the lowest AIC/BIC),
# view the coefficients to find the "True" weights:
summary(model_no_time) # Replace with the winning model


# 1. Residuals vs. Fitted plot
# We want to see a random scatter of points. 
# If you see a "U" or "funnel" shape, your model might be missing 
# a non-linear term (like height^2).
plot(model_no_time, which = 1)

# 2. Q-Q Plot (Normal Q-Q)
# This checks if the residuals are normally distributed.
# You want the points to track closely along the diagonal line.
plot(model_no_time, which = 2)

# 3. Optional: Scale-Location plot (checks for heteroscedasticity)
plot(model_no_time, which = 3)



















#goimng to try with no gender
model_1 <- lm(weight_y ~ personal_trainer + visits + height + 
                caloric_balance + time, 
              data = df)

summary(model_1)
AIC(naive_model, model_1) #not improving


#personal trainer with visits
model_2 <- lm(weight_y ~ personal_trainer * visits + height + 
                caloric_balance + time, 
              data = df)

summary(model_2)
AIC(naive_model, model_2) #not improving

#visits * time
model_3 <- lm(weight_y ~ personal_trainer + visits * time + height + 
                caloric_balance , 
              data = df)

summary(model_3)
AIC(naive_model, model_3) #not improving




