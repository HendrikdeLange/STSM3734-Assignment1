library(dplyr)
df <- read.csv("C:\\Users\\hendr\\OneDrive\\Documents\\STSM3734-MAIZE_SIMULATION\\maize_data_observed.csv")
colnames(df)

numeric_vars <- c("temperature_C", "rainfall_mm", "fertiliser_kgha", 
                  "chem_weed_control_kgha", "soil_organic_matter")
df <- df %>%
  mutate(across(all_of(numeric_vars), ~ scale(.)[,1], 
                .names = "{.col}_z"))

Irrigated  <- ifelse(df$irrigation    == "Irrigated",  1, 0)
Dekalb     <- ifelse(df$seed_brand    == "Dekalb",     1, 0)
Pioneer    <- ifelse(df$seed_brand    == "Pioneer",    1, 0)
John_Deere <- ifelse(df$tractor_brand == "John Deere", 1, 0)

#going to test the model using lm
# All predictors
model <- lm(yield_tha ~
                 rainfall_mm_z +
                 temperature_C_z +
                 fertiliser_kgha_z +
                 chem_weed_control_kgha_z +
                 irrigation +
                 soil_organic_matter_z +
                 seed_brand +
                 seed_brand * temperature_C_z +
                 seed_brand * irrigation +
                 seed_brand * fertiliser_kgha_z +
                 tractor_brand,
               data = df)

summary(model)
