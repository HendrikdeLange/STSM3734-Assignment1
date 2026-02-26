generate_maize_data <- function(n = 1080) {
  set.seed(123) # Optional: remove if you want different data every call
  
  # 1. REGION
  regions <- c("Free State", "North West", "Mpumalanga")
  region <- sample(regions, size = n, replace = TRUE, prob = c(0.6, 0.15, 0.25))
  
  # 2. AVERAGE TEMPERATURE
  temp_params <- list(
    "Free State" = c(mean = 24, sd = 4),   
    "North West" = c(mean = 28, sd = 5),   
    "Mpumalanga" = c(mean = 22, sd = 3)    
  )
  
  temperature <- sapply(region, function(reg) {
    p   <- temp_params[[reg]]
    val <- rnorm(1, mean = p["mean"], sd = p["sd"])
    return(max(5, min(45, val)))
  })
  
  # 3. RAINFALL
  rainfall_params <- list(
    "Free State" = c(mean = 600, shape = 10),  
    "North West" = c(mean = 520, shape = 8),   
    "Mpumalanga" = c(mean = 780, shape = 12)   
  )
  
  rainfall <- sapply(region, function(reg) {
    p    <- rainfall_params[[reg]]
    rate <- p["shape"] / p["mean"]
    return(rgamma(1, shape = p["shape"], rate = rate))
  })
  
  # 4. IRRIGATION TYPE
  irrig_probs <- list("Free State" = c(0.88, 0.12), 
                      "North West" = c(0.82, 0.18), 
                      "Mpumalanga" = c(0.96, 0.04))
  
  irrig_labels <- c("Rainfed", "Irrigated")
  irrigation <- sapply(region, function(reg) {
    sample(irrig_labels, 1, prob = irrig_probs[[reg]])
  })
  
  # 5. FERTILISER & PESTICIDE (Log-Normal distributions)
  calc_lnorm_params <- function(m, s) {
    mu <- log(m^2 / sqrt(m^2 + s^2))
    sigma <- sqrt(log(1 + (s / m)^2))
    return(c(mu, sigma))
  }
  
  fert_p <- calc_lnorm_params(180, 80)
  fertiliser <- pmax(0, pmin(500, rlnorm(n, fert_p[1], fert_p[2])))
  
  pest_p <- calc_lnorm_params(150, 70)
  pesticide_kgha <- pmax(0, pmin(500, rlnorm(n, pest_p[1], pest_p[2])))
  
  # 6. SOIL ORGANIC MATTER
  # Normalized factors for relationship logic
  rain_norm <- (rainfall - min(rainfall)) / (max(rainfall) - min(rainfall))
  temp_norm <- (temperature - min(temperature)) / (max(temperature) - min(temperature))
  
  som_base <- 0.15 * rain_norm - 0.10 * temp_norm  
  soil_organic_matter <- pmin(pmax(3.5 + som_base + rnorm(n, 0, 0.3), 1), 6)
  
  # 7. BRANDS
  seed_brand <- sample(c("Pannar", "Dekalb", "Pioneer"), n, replace = TRUE)
  tractor_brand <- sample(c("John Deere", "Case", "New Holland"), n, replace = TRUE, prob = c(0.6, 0.2, 0.2))
  
  # 8. ASSEMBLE
  df <- data.frame(
    region                  = region,            
    temperature_C           = round(temperature, 2),
    rainfall_mm             = round(rainfall, 1),
    irrigation              = irrigation,  
    fertiliser_kgha         = round(fertiliser, 2),
    chem_weed_control_kgha  = round(pesticide_kgha, 2),
    soil_organic_matter     = round(soil_organic_matter, 3),
    seed_brand              = seed_brand,      
    tractor_brand           = tractor_brand     
  )
  
  return(df)
}
write.csv(maize_data, file = "maize_data.csv", row.names = FALSE)

