library(dplyr)
source("simulate_maize_data.R")

df <- generate_maize_data(400)



numeric_vars <- c("temperature_C", "rainfall_mm", "fertiliser_kgha", 
                  "chem_weed_control_kgha", "soil_organic_matter")
df_processed <- df %>%
  mutate(across(all_of(numeric_vars), ~ scale(.)[,1], 
                .names = "{.col}_z")) %>%
  
  mutate(
    # irrigation (Rainfed = reference)
    irrigation_irrigated = as.integer(irrigation == "Irrigated"),
    
    # seed_brand (Dekalb = reference)
    seed_Pioneer = as.integer(seed_brand == "Pioneer"),
    seed_Pannar  = as.integer(seed_brand == "Pannar"),
    
    # tractor_brand (John Deere = reference)
    tractor_Case       = as.integer(tractor_brand == "Case"),
    tractor_NewHolland = as.integer(tractor_brand == "New Holland")
  ) %>%
  
  select(-c(irrigation, seed_brand, tractor_brand, numeric_vars))

head(df_processed)



Irrigated <- df_processed$irrigation_irrigated
Dekalb    <- as.integer(df_processed$seed_Pioneer == 0 & df_processed$seed_Pannar == 0)  
Pioneer   <- df_processed$seed_Pioneer
John_Deere <- as.integer(df_processed$tractor_Case == 0 & df_processed$tractor_NewHolland == 0) 

df_processed$yield_tha <- (
  6.0
  
  + 0.80 * df_processed$rainfall_mm_z
  - 0.25 * df_processed$temperature_C_z
  
  + 0.40 * df_processed$fertiliser_kgha_z
  + 0.20 * df_processed$chem_weed_control_kgha_z
  + 0.50 * Irrigated
  
  + 0.20 * df_processed$soil_organic_matter_z
  
  + 0.30 * Dekalb                                                 # high yield advantage
  - 0.20 * Dekalb  * df_processed$temperature_C_z                 # Dekalb struggles in heat vs Pannar
  - 0.20 * Pioneer * df_processed$temperature_C_z                 # Pioneer struggles in heat vs Pannar
  + 0.25 * Pioneer * Irrigated                                    # Pioneer thrives when irrigated
  + 0.20 * Pioneer * df_processed$fertiliser_kgha_z               # Pioneer thrives with fertiliser
  + 0.15 * John_Deere
  
  + rnorm(400, mean = 0, sd = 0.7)
)
df_processed$yield_tha <- pmin(pmax(df_processed$yield_tha, 2), 16) #cap the yield to a realistic range
df$yield_tha <- df_processed$yield_tha

write.csv(df, file = "maize_data_observed.csv", row.names = FALSE)

model <- lm(yield_tha ~ rainfall_mm_z 
            + temperature_C_z 
            + fertiliser_kgha_z 
            + chem_weed_control_kgha_z 
            + Irrigated 
            + soil_organic_matter_z 
            + Dekalb 
            + Pioneer 
            + John_Deere
            + Dekalb:temperature_C_z       # interaction: Dekalb × heat
            + Pioneer:temperature_C_z      # interaction: Pioneer × heat
            + Pioneer:Irrigated            # interaction: Pioneer × irrigation
            + Pioneer:fertiliser_kgha_z,   # interaction: Pioneer × fertiliser
            data = df_processed)

summary(model)


library(car)

vif(model)














