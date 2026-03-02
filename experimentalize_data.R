df <- read.csv("C:\\Users\\hendr\\OneDrive\\Documents\\STSM3734-MAIZE_SIMULATION\\maize_data_observed.csv")
library(dplyr)
source("simulate_maize_data.R")

yield_obs <- df$yield_tha
set.seed(123)

# --- Step 1: Reference data for tertile means ---
df_ref <- generate_maize_data(131220)
tertile_labels <- c("Low", "Medium", "High")

rainfall_means <- df_ref %>%
  mutate(cat = ntile(rainfall_mm, 3)) %>%
  group_by(cat) %>% summarise(rainfall_mean = mean(rainfall_mm)) %>%
  mutate(rainfall_cat = tertile_labels) %>% select(-cat)

temperature_means <- df_ref %>%
  mutate(cat = ntile(temperature_C, 3)) %>%
  group_by(cat) %>% summarise(temperature_mean = mean(temperature_C)) %>%
  mutate(temperature_cat = tertile_labels) %>% select(-cat)

fertiliser_means <- df_ref %>%
  mutate(cat = ntile(fertiliser_kgha, 3)) %>%
  group_by(cat) %>% summarise(fertiliser_mean = mean(fertiliser_kgha)) %>%
  mutate(fertiliser_cat = tertile_labels) %>% select(-cat)

weed_control_means <- df_ref %>%
  mutate(cat = ntile(chem_weed_control_kgha, 3)) %>%
  group_by(cat) %>% summarise(weed_control_mean = mean(chem_weed_control_kgha)) %>%
  mutate(weed_control_cat = tertile_labels) %>% select(-cat)

soil_om_means <- df_ref %>%
  mutate(cat = ntile(soil_organic_matter, 3)) %>%
  group_by(cat) %>% summarise(soil_organic_matter_mean = mean(soil_organic_matter)) %>%
  mutate(soil_organic_matter_cat = tertile_labels) %>% select(-cat)

# --- Step 2: Build balanced design ---
cell_n <- 10

final_data <- expand.grid(
  rainfall_cat            = tertile_labels,
  temperature_cat         = tertile_labels,
  fertiliser_cat          = tertile_labels,
  weed_control_cat        = tertile_labels,
  soil_organic_matter_cat = tertile_labels,
  region                  = unique(df_ref$region),
  seed_brand              = unique(df_ref$seed_brand),
  tractor_brand           = unique(df_ref$tractor_brand),
  irrigation              = c("Irrigated", "Rainfed"),
  stringsAsFactors        = FALSE
) %>%
  slice(rep(1:n(), each = cell_n)) %>%
  left_join(rainfall_means,     by = "rainfall_cat") %>%
  left_join(temperature_means,  by = "temperature_cat") %>%
  left_join(fertiliser_means,   by = "fertiliser_cat") %>%
  left_join(weed_control_means, by = "weed_control_cat") %>%
  left_join(soil_om_means,      by = "soil_organic_matter_cat")

final_data %>%
  group_by(rainfall_cat, temperature_cat, fertiliser_cat, 
           weed_control_cat, soil_organic_matter_cat,
           region, seed_brand, tractor_brand, irrigation) %>%
  summarise(n = n()) %>%
  summary()  # min, max, mean of n should all be 10


#need to standardize the means
rainfall_means_Z <- rainfall_means %>%
  mutate(rainfall_mean_z = (rainfall_mean - mean(df_ref$rainfall_mm)) / sd(df_ref$rainfall_mm)) %>%
  select(rainfall_cat, rainfall_mean_z)

temperature_means_Z <- temperature_means %>%
  mutate(temperature_mean_z = (temperature_mean - mean(df_ref$temperature_C)) / sd(df_ref$temperature_C)) %>%
  select(temperature_cat, temperature_mean_z)

fertiliser_means_Z <- fertiliser_means %>%
  mutate(fertiliser_mean_z = (fertiliser_mean - mean(df_ref$fertiliser_kgha)) / sd(df_ref$fertiliser_kgha)) %>%
  select(fertiliser_cat, fertiliser_mean_z)

weed_control_means_Z <- weed_control_means %>%
  mutate(weed_control_mean_z = (weed_control_mean - mean(df_ref$chem_weed_control_kgha)) / sd(df_ref$chem_weed_control_kgha)) %>%
  select(weed_control_cat, weed_control_mean_z)

soil_om_means_Z <- soil_om_means %>%
  mutate(soil_organic_matter_mean_z = (soil_organic_matter_mean - mean(df_ref$soil_organic_matter)) / sd(df_ref$soil_organic_matter)) %>%
  select(soil_organic_matter_cat, soil_organic_matter_mean_z)

final_data <- final_data %>%
  left_join(rainfall_means_Z,     by = "rainfall_cat") %>%
  left_join(temperature_means_Z,  by = "temperature_cat") %>%
  left_join(fertiliser_means_Z,   by = "fertiliser_cat") %>%
  left_join(weed_control_means_Z, by = "weed_control_cat") %>%
  left_join(soil_om_means_Z,      by = "soil_organic_matter_cat")

#CORRECT WAY
# RAIN
beta_rain <- 0.80 * rainfall_means_Z$rainfall_mean_z
beta_rain_dev <- beta_rain - beta_rain[1]
intercept_new <- 6 + beta_rain[1]
beta_rain_2 <- beta_rain_dev[2]
beta_rain_3 <- beta_rain_dev[3]
rain_medium <- ifelse(final_data$rainfall_cat == "Medium", 1, 0)
rain_high   <- ifelse(final_data$rainfall_cat == "High", 1, 0)

# TEMPERATURE
beta_temperature <- -0.25 * temperature_means_Z$temperature_mean_z
beta_temperature_dev <- beta_temperature - beta_temperature[1]
intercept_new <- intercept_new + beta_temperature[1]
beta_temperature_2 <- beta_temperature_dev[2]
beta_temperature_3 <- beta_temperature_dev[3]
temperature_medium <- ifelse(final_data$temperature_cat == "Medium", 1, 0)
temperature_high   <- ifelse(final_data$temperature_cat == "High", 1, 0)

# FERTILISER
beta_fertiliser <- 0.40 * fertiliser_means_Z$fertiliser_mean_z
beta_fertiliser_dev <- beta_fertiliser - beta_fertiliser[1]
intercept_new <- intercept_new + beta_fertiliser[1]
beta_fertiliser_2 <- beta_fertiliser_dev[2]
beta_fertiliser_3 <- beta_fertiliser_dev[3]
fertiliser_medium <- ifelse(final_data$fertiliser_cat == "Medium", 1, 0)
fertiliser_high   <- ifelse(final_data$fertiliser_cat == "High", 1, 0)

# WEED CONTROL
beta_weed <- 0.20 * weed_control_means_Z$weed_control_mean_z
beta_weed_dev <- beta_weed - beta_weed[1]
intercept_new <- intercept_new + beta_weed[1]
beta_weed_2 <- beta_weed_dev[2]
beta_weed_3 <- beta_weed_dev[3]
weed_medium <- ifelse(final_data$weed_control_cat == "Medium", 1, 0)
weed_high   <- ifelse(final_data$weed_control_cat == "High", 1, 0)

# SOIL ORGANIC MATTER
beta_soil <- 0.20 * soil_om_means_Z$soil_organic_matter_mean_z
beta_soil_dev <- beta_soil - beta_soil[1]
intercept_new <- intercept_new + beta_soil[1]
beta_soil_2 <- beta_soil_dev[2]
beta_soil_3 <- beta_soil_dev[3]
soil_medium <- ifelse(final_data$soil_organic_matter_cat == "Medium", 1, 0)
soil_high   <- ifelse(final_data$soil_organic_matter_cat == "High", 1, 0)

# MORE DUMMIESSSSS
final_data$Irrigated  <- as.integer(final_data$irrigation == "Irrigated")
final_data$Dekalb     <- as.integer(final_data$seed_brand != "Pioneer" & final_data$seed_brand != "Pannar")
final_data$Pioneer    <- as.integer(final_data$seed_brand == "Pioneer")
final_data$John_Deere <- as.integer(final_data$tractor_brand != "Case" & final_data$tractor_brand != "New Holland")

#temp vs seed brand
beta_temperature_seed <- -0.20 * temperature_means_Z$temperature_mean_z
beta_temperature_seed_dev <- beta_temperature_seed - beta_temperature_seed[1]
beta_temperature_seed_2 <- beta_temperature_seed_dev[2]
beta_temperature_seed_3 <- beta_temperature_seed_dev[3]
temperature_seed_medium <- ifelse(final_data$temperature_cat == "Medium", 1, 0)
temperature_seed_high   <- ifelse(final_data$temperature_cat == "High", 1, 0)

#fertiliser vs seed brand
beta_fertiliser_seed <- 0.20 * fertiliser_means_Z$fertiliser_mean_z
beta_fertiliser_seed_dev <- beta_fertiliser_seed - beta_fertiliser_seed[1]
beta_fertiliser_seed_2 <- beta_fertiliser_seed_dev[2]
beta_fertiliser_seed_3 <- beta_fertiliser_seed_dev[3]
fertiliser_seed_medium <- ifelse(final_data$fertiliser_cat == "Medium", 1, 0)
fertiliser_seed_high   <- ifelse(final_data$fertiliser_cat == "High", 1, 0)

#creating the model
set.seed(123) 
final_data$yield_tha <- (
  intercept_new + 
    beta_rain_2 * rain_medium +
    beta_rain_3 * rain_high +
    
    beta_temperature_2 * temperature_medium +
    beta_temperature_3 * temperature_high +
    
    beta_fertiliser_2 * fertiliser_medium +
    beta_fertiliser_3 * fertiliser_high +
    
    beta_weed_2 * weed_medium +
    beta_weed_3 * weed_high +
    
    0.50 * final_data$Irrigated + 
    
    beta_soil_2 * soil_medium +
    beta_soil_3 * soil_high +
    
    0.30 * final_data$Dekalb + 
    
    beta_temperature_seed_2 * temperature_medium * final_data$Dekalb +
    beta_temperature_seed_3 * temperature_high   * final_data$Dekalb +
    
    beta_temperature_seed_2 * temperature_medium * final_data$Pioneer +
    beta_temperature_seed_3 * temperature_high   * final_data$Pioneer +
    
    0.25 * final_data$Pioneer  * final_data$Irrigated +
    
    beta_fertiliser_seed_2 * fertiliser_seed_medium * final_data$Pioneer +
    beta_fertiliser_seed_3 * fertiliser_seed_high * final_data$Pioneer +
    
    0.15 * final_data$John_Deere +
  

    rnorm(nrow(final_data), mean = 0, sd = 0.7)
)

final_data$yield_tha <- pmin(pmax(final_data$yield_tha, 2), 16)


yield_exp <- final_data$yield_tha
#evaluating the model
final_data <- final_data %>%
  mutate(
    Irrigated  = factor(Irrigated,  levels = c(0,1), labels = c("Rainfed","Irrigated")),
    Dekalb     = factor(Dekalb,     levels = c(0,1), labels = c("Other","Dekalb")),
    Pioneer    = factor(Pioneer,    levels = c(0,1), labels = c("Other","Pioneer")),
    John_Deere = factor(John_Deere, levels = c(0,1), labels = c("Other","John_Deere"))
  )
model <- lm(yield_tha ~ 
              # Main effects — one term per DGP coefficient group
              rainfall_cat             +   # beta_rain_2, beta_rain_3
              temperature_cat          +   # beta_temperature_2/3
              fertiliser_cat           +   # beta_fertiliser_2/3
              weed_control_cat         +   # beta_weed_2/3
              soil_organic_matter_cat  +   # beta_soil_2/3
              Irrigated                +   # 0.50
              Dekalb                   +   # 0.30
              Pioneer                  +   # enters via interactions; explicit here for clarity
              John_Deere               +   # 0.15
              
              # Interactions — mirror DGP exactly
              temperature_cat : Dekalb    +   # beta_temperature_seed * Dekalb
              temperature_cat : Pioneer   +   # beta_temperature_seed * Pioneer
              Pioneer         : Irrigated +   # 0.25
              fertiliser_cat  : Pioneer,      # beta_fertiliser_seed * Pioneer
            
            data = final_data)

summary(model)

#QQ plot
# qqplot(income_obs, income_exp,
#        main = "QQ Plot: Experimental vs Observational",
#        xlab = "Observational Quantiles",
#        ylab = "Experimental Quantiles",
#        pch = 16, col = "darkgray")
# 
# abline(0, 1, col = "red", lwd = 2)
#please add residual vs fitted graph


#Gonna use this to make our QQplot
qqplot(yield_obs, yield_exp,
       main = "QQ Plot: Experimental vs Observational",
       xlab = "Observational Quantiles",
       ylab = "Experimental Quantiles",
       pch = 16, col = "darkgray")

abline(0, 1, col = "red", lwd = 2)



#scatterplot correlation matrix
library(corrplot)
cor_data <- final_data %>%
  select(
    rainfall_mean_z,
    temperature_mean_z,
    fertiliser_mean_z,
    weed_control_mean_z,
    soil_organic_matter_mean_z
  )

cor_matrix <- cor(cor_data)

corrplot(cor_matrix,
         method      = "color",
         type        = "upper",
         addCoef.col = "black",
         tl.col      = "black",
         tl.srt      = 45,
         number.cex  = 0.7,
         title       = "Correlation Matrix: Experimental Data",
         mar         = c(0, 0, 1, 0))

