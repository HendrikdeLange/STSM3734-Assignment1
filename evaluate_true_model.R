df <- read.csv("C:\\Users\\hendr\\OneDrive\\Documents\\STSM3734-MAIZE_SIMULATION\\maize_data_observed.csv")
colnames(df)


#going to test the model using lm
# All predictors
model <- lm(yield_tha ~ temperature_C + rainfall_mm + irrigation + 
              fertiliser_kgha + chem_weed_control_kgha + 
              soil_organic_matter + seed_brand + tractor_brand, 
            data = df)

summary(model)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model)

# Formal assumption tests
library(lmtest)
bptest(model)        # Breusch-Pagan: heteroscedasticity

library(nortest)
ad.test(residuals(model))   # Anderson-Darling: normality of residuals

# Variance Inflation Factors (multicollinearity)
library(car)
vif(model)

par(mfrow = c(2, 2))
plot(model)

library(sandwich)
library(lmtest)

coeftest(model, vcov = vcovHC(model, type = "HC3"))