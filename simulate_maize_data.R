#Generate observed data

set.seed(123)
n <- 1080  # total observations (fields)

# -----------------------------------------------------------
# 1. REGION  
# -----------------------------------------------------------
regions <- c("Free State", "North West", "Mpumalanga")
region <- sample(regions, size = n, replace = TRUE, prob = c(0.6, 0.15, 0.25))

# -----------------------------------------------------------
# 2. AVERAGE TEMPERATURE  
# -----------------------------------------------------------
temp_params <- list(
  "Free State" = c(mean = 24, sd = 4),   
  "North West" = c(mean = 28, sd = 5),   
  "Mpumalanga" = c(mean = 22, sd = 3)    
)

temperature <- sapply(region, function(reg) {
  p   <- temp_params[[reg]]
  val <- rnorm(1, mean = p["mean"], sd = p["sd"])
  val <- max(5, min(45, val))
  return(val)
})
# -----------------------------------------------------------
# 3. RAINFALL  
# -----------------------------------------------------------
rainfall_params <- list(
  "Free State" = c(mean = 600, shape = 10),  
  "North West" = c(mean = 520, shape = 8),   
  "Mpumalanga" = c(mean = 780, shape = 12)   
)

rainfall <- sapply(region, function(reg) {
  p    <- rainfall_params[[reg]]
  rate <- p["shape"] / p["mean"]
  val  <- rgamma(1, shape = p["shape"], rate = rate)
  return(val)
})

# -----------------------------------------------------------
# 5. IRRIGATION TYPE
# -----------------------------------------------------------
irrig_probs <- list("Free State" = c(0.88, 0.12), 
                    "North West" = c(0.82, 0.18), 
                    "Mpumalanga" = c(0.96, 0.04))  # not impossible (covariance issues)

irrig_labels <- c("Rainfed", "Irrigated")
irrigation <- mapply(function(reg) {
  sample(irrig_labels, 1, prob = irrig_probs[[reg]])
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
# 8. SOIL ORGANIC MATTER  
# -----------------------------------------------------------
rain_soil <- (rainfall - min(rainfall)) / (max(rainfall) - min(rainfall))
temp_soil <- (temperature - min(temperature)) / (max(temperature) - min(temperature))

som_base <- 0.15 * rain_soil - 0.10 * temp_soil  
som_noise <- rnorm(n, 0, 0.3)
soil_organic_matter <- 3.5 + som_base + som_noise
soil_organic_matter <- pmin(pmax(soil_organic_matter, 1), 6)

# -----------------------------------------------------------
# 12. SEED BRAND  
# -----------------------------------------------------------
seed_brands <- c("Pannar", "Dekalb", "Pioneer")
seed_brand <- sample(seed_brands, n, replace = TRUE, prob = c(1/3, 1/3, 1/3))

# -----------------------------------------------------------
# 13. TRACTOR BRAND  
# -----------------------------------------------------------
tractor_brands <- c("John Deere", "Case", "New Holland")
tractor_brand <- sample(tractor_brands, n, replace = TRUE, prob = c(0.6, 0.2, 0.2))

# -----------------------------------------------------------
# 14. ASSEMBLE DATASET
# -----------------------------------------------------------
maize_data <- data.frame(
  region              = region,           
  temperature_C       = round(temperature,          2),
  rainfall_mm         = round(rainfall,              1),
  irrigation          = irrigation,  
  fertiliser_kgha     = round(fertiliser,            2),
  chem_weed_control_kgha      = round(pesticide_kgha,        2),
  soil_organic_matter = round(soil_organic_matter,   3),
  seed_brand          = seed_brand,      
  tractor_brand       = tractor_brand     
)

head(maize_data)
write.csv(maize_data, file = "maize_data.csv", row.names = FALSE)
colnames(maize_data)
numeric_vars <- maize_data[, c(
  "temperature_C",
  "rainfall_mm",
  "fertiliser_kgha",
  "chem_weed_control_kgha",
  "soil_organic_matter"
)]

pairs(numeric_vars)
