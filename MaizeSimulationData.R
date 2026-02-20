#Generate Observational Data

library(GGally)
library(writexl)


set.seed(123)
n <- 1000  # total observations (fields)

# -----------------------------------------------------------
# 1. REGION  
#    1 = Free State, 2 = North West, 3 = Mpumalanga
# -----------------------------------------------------------
regions <- c("Free State", "North West", "Mpumalanga")
region  <- sample(1:3, n, replace = TRUE, prob = c(0.6, 0.15, 0.25))

# -----------------------------------------------------------
# 2. AVERAGE TEMPERATURE  
# -----------------------------------------------------------
temp_params <- list(
  "1" = c(mean = 23, sd = 3),   # Free State
  "2" = c(mean = 24, sd = 2.5),   # North West
  "3" = c(mean = 23, sd = 1.5)    # Mpumalanga
)

temperature <- mapply(function(reg) {
  p   <- temp_params[[as.character(reg)]]
  val <- rnorm(1, mean = p["mean"], sd = p["sd"])
  val <- max(-5, min(42, val))
  val
}, region)

# -----------------------------------------------------------
# 3. RAINFALL  
# -----------------------------------------------------------
rainfall_params <- list(
  "1" = c(mean = 1080, shape = 25),  # Free State
  "2" = c(mean = 996,  shape = 25),  # North West
  "3" = c(mean = 1230, shape = 25)   # Mpumalanga
)

rainfall <- mapply(function(reg) {
  p    <- rainfall_params[[as.character(reg)]]
  rate <- p["shape"] / p["mean"]
  rgamma(1, shape = p["shape"], rate = rate)
}, region)


# -----------------------------------------------------------
# 5. IRRIGATION TYPE
#    1 = Rainfed, 2 = Irrigated
# -----------------------------------------------------------
irrigation <- mapply(function(reg) {
  if (reg == 3) {
    1L  # Rainfed only
  } else {
    sample(1:2, 1, prob = c(0.9, 0.1))
  }
}, region)

# -----------------------------------------------------------
# 6. FERTILISER APPLICATION RATE  
# -----------------------------------------------------------
fert_mu    <- log(180^2 / sqrt(180^2 + 80^2))
fert_sigma <- sqrt(log(1 + (80 / 180)^2))

fertiliser <- rlnorm(n, meanlog = fert_mu, sdlog = fert_sigma)
fertiliser <- pmax(0, pmin(500, fertiliser))

# -----------------------------------------------------------
# 7. Chemical Weed Control PER HECTARE 
# -----------------------------------------------------------
pest_mu    <- log(150^2 / sqrt(150^2 + 70^2))
pest_sigma <- sqrt(log(1 + (70 / 150)^2))

pesticide_kgha <- rlnorm(n, meanlog = pest_mu, sdlog = pest_sigma)
pesticide_kgha <- pmax(0, pmin(500, pesticide_kgha))

# -----------------------------------------------------------
# 8. SOIL ORGANIC MATTER  (Beta → rescaled to [1, 6]%)
#    + rainfall, - temperature
# -----------------------------------------------------------
rain_soil <- (rainfall - min(rainfall)) / (max(rainfall) - min(rainfall))
temp_soil <- (temperature - min(temperature)) / (max(temperature) - min(temperature))

soil_organic_matter <- 0.6 * rain_soil + 0.4 * (1 - temp_soil)

soil_organic_matter <- 1 + soil_organic_matter * 5
soil_organic_matter <- pmin(pmax(soil_organic_matter, 1), 6)

# -----------------------------------------------------------
# 12. SEED BRAND  
#     1 = Pannar, 2 = Dekalb, 3 = Pioneer
# -----------------------------------------------------------
seed_brand <- sample(1:3, n, replace = TRUE, prob = c(1/3, 1/3, 1/3))

# -----------------------------------------------------------
# 13. TRACTOR BRAND  (Nuisance)
#     1 = John Deere, 2 = Case, 3 = New Holland
# -----------------------------------------------------------
tractor_brand <- sample(1:3, n, replace = TRUE, prob = c(0.6, 0.2, 0.2))

# -----------------------------------------------------------
# 14. ASSEMBLE DATASET
# -----------------------------------------------------------
maize_data <- data.frame(
  region              = region,           # 1=Free State, 2=North West, 3=Mpumalanga
  temperature_C       = round(temperature,          2),
  rainfall_mm         = round(rainfall,              1),
  irrigation          = irrigation,  
  fertiliser_kgha     = round(fertiliser,            2),
  pesticide_kgha      = round(pesticide_kgha,        2),
  soil_organic_matter = round(soil_organic_matter,   3),
  seed_brand          = seed_brand,      
  tractor_brand       = tractor_brand     
)

maize_data_observed <- maize_data #for viewing
# --- Correct variable types ---
maize_data$region        <- factor(maize_data$region,        labels = c("Free State", "North West", "Mpumalanga"))
maize_data$irrigation    <- factor(maize_data$irrigation,    labels = c("Rainfed", "Irrigated"))
maize_data$seed_brand    <- factor(maize_data$seed_brand,    labels = c("Pannar", "Dekalb", "Pioneer"))
maize_data$tractor_brand <- factor(maize_data$tractor_brand, labels = c("John Deere", "Case", "New Holland"))

# --- Standardise continuous variables ---
maize_data$temperature_C_z       <- as.numeric(scale(maize_data$temperature_C))
maize_data$rainfall_mm_z         <- as.numeric(scale(maize_data$rainfall_mm))
maize_data$fertiliser_kgha_z     <- as.numeric(scale(maize_data$fertiliser_kgha))
maize_data$pesticide_kgha_z      <- as.numeric(scale(maize_data$pesticide_kgha))
maize_data$soil_organic_matter_z <- as.numeric(scale(maize_data$soil_organic_matter))

# --- Dummies (Pannar = reference, so no Pannar dummy) ---
maize_data$Dekalb    <- as.integer(maize_data$seed_brand == "Dekalb")
maize_data$Pioneer   <- as.integer(maize_data$seed_brand == "Pioneer")
maize_data$Irrigated <- as.integer(maize_data$irrigation == "Irrigated")
maize_data$John    <- as.integer(maize_data$tractor_brand == "John Deere")
maize_data$Case   <- as.integer(maize_data$tractor_brand == "New Holland")

# Local vectors for use in yield formula
Dekalb    <- maize_data$Dekalb
Pioneer   <- maize_data$Pioneer
Irrigated <- maize_data$Irrigated

# --- Simulate yield ---
# Reference groups: Pannar seed, Rainfed irrigation
maize_data$yield_tha <- (
  6.0                                                         # baseline = Pannar, Rainfed
  
  + 0.80  * maize_data$rainfall_mm_z                         # strongest driver
  - 0.25  * maize_data$temperature_C_z                       # heat penalty
  
  + 0.35  * maize_data$fertiliser_kgha_z
  + 0.15  * maize_data$pesticide_kgha_z
  + 0.25  * Irrigated                                        # vs Rainfed
  
  + 0.10  * maize_data$soil_organic_matter_z
  
  + 0.30  * Dekalb                                           # vs Pannar
  + 0.20  * Dekalb   * maize_data$temperature_C_z           # Pannar thrives in heat (negative relative to Dekalb)
  + 0.25  * Pioneer  * Irrigated                             # Pioneer boost when irrigated
  + 0.20  * Pioneer  * maize_data$fertiliser_kgha_z         # Pioneer boost with high fertiliser
  
  + rnorm(n, mean = 0, sd = 0.3)
)

maize_data$yield_tha <- pmin(pmax(maize_data$yield_tha, 3), 10)

# --- Subset columns ---
maize_data <- maize_data[, c("yield_tha",
                             "region",
                             "temperature_C_z",
                             "rainfall_mm_z",
                             "Irrigated",
                             "fertiliser_kgha_z",
                             "pesticide_kgha_z",
                             "soil_organic_matter_z",
                             "Dekalb",
                             "Pioneer",
                             "John",
                             "Case",
                             "seed_brand",
                             "irrigation")]

summary(maize_data$yield_tha)
hist(maize_data$yield_tha, breaks = 30,
     main = "Simulated Maize Yield", xlab = "t/ha", col = "steelblue")

library(ggplot2)
library(gridExtra)

###SOME GRAPHS
# 1. Yield distribution by seed brand
p1 <- ggplot(maize_data, aes(x = yield_tha, fill = seed_brand)) +
  geom_density(alpha = 0.5) +
  labs(title = "Yield Distribution by Seed Brand", x = "Yield (t/ha)", fill = "Seed Brand") +
  theme_minimal()

# 2. Yield by region (boxplot)
p2 <- ggplot(maize_data, aes(x = region, y = yield_tha, fill = region)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Yield by Region", x = "", y = "Yield (t/ha)") +
  theme_minimal() +
  theme(legend.position = "none")

# 3. Rainfall vs Yield (scatter) coloured by seed brand
p3 <- ggplot(maize_data, aes(x = rainfall_mm_z, y = yield_tha, colour = seed_brand)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Rainfall vs Yield by Seed Brand", 
       x = "Rainfall (standardised)", y = "Yield (t/ha)", colour = "Seed Brand") +
  theme_minimal()

# 4. Irrigation effect by seed brand
p4 <- ggplot(maize_data, aes(x = seed_brand, y = yield_tha, fill = irrigation)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Irrigation Effect by Seed Brand", x = "", y = "Yield (t/ha)", fill = "Irrigation") +
  theme_minimal()

# 5. Temperature vs Yield — should show Pannar doing better in heat
p5 <- ggplot(maize_data, aes(x = temperature_C_z, y = yield_tha, colour = seed_brand)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Temperature vs Yield by Seed Brand",
       x = "Temperature (standardised)", y = "Yield (t/ha)", colour = "Seed Brand") +
  theme_minimal()

# 6. Fertiliser vs Yield — Pioneer interaction should be visible
p6 <- ggplot(maize_data, aes(x = fertiliser_kgha_z, y = yield_tha, colour = seed_brand)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Fertiliser vs Yield by Seed Brand",
       x = "Fertiliser (standardised)", y = "Yield (t/ha)", colour = "Seed Brand") +
  theme_minimal()

# Display all together
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)


#I will export the necessary dataset for the next step
maize_data_observed$yield_tha <-maize_data$yield_tha
write.csv(maize_data_observed, "observed_data.csv", row.names = FALSE)