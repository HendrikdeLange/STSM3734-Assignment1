generate_maize_data <- function(n) {
  set.seed(123) 
  
  # 1. REGION
  regions <- c("Free State", "North West", "Mpumalanga")
  region <- sample(regions, size = n, replace = TRUE, prob = c(0.6, 0.15, 0.25))
  
  # 2. AVERAGE TEMPERATURE - wider regional differences to induce correlation with rainfall
  temp_params <- list(
    "Free State" = c(mean = 22, sd = 3),   # cooler, consistent highveld
    "North West" = c(mean = 27, sd = 3),   # hotter, drier bushveld
    "Mpumalanga" = c(mean = 24, sd = 2)    # moderate, escarpment
  )
  
  temperature <- sapply(region, function(reg) {
    p   <- temp_params[[reg]]
    val <- rnorm(1, mean = p["mean"], sd = p["sd"])
    return(max(10, min(49, val)))
  })
  
  # 3. RAINFALL - after temperature so we can link them
  rainfall_params <- list(
    "Free State" = c(mean = 600, shape = 12),  
    "North West" = c(mean = 480, shape = 8),   # drier + hotter = negative corr with temp
    "Mpumalanga" = c(mean = 820, shape = 14)   # wetter + cooler
  )
  
  rainfall <- sapply(region, function(reg) {
    p    <- rainfall_params[[reg]]
    rate <- p["shape"] / p["mean"]
    return(rgamma(1, shape = p["shape"], rate = rate))
  })
  
  # Explicitly induce temperature-rainfall negative correlation
  # Drier = hotter (less evaporative cooling) - realistic for SA
  temp_rain_effect <- -0.004 * (rainfall - mean(rainfall))
  temperature <- pmax(10, pmin(49, temperature + temp_rain_effect))
  
  # Normalise
  rain_norm <- (rainfall - min(rainfall)) / (max(rainfall) - min(rainfall))
  temp_norm <- (temperature - min(temperature)) / (max(temperature) - min(temperature))
  
  # 4. IRRIGATION - drier farms irrigate more
  irrigation <- sapply(seq_along(region), function(i) {
    reg <- region[i]
    base_rainfed <- list("Free State" = 0.88,
                         "North West" = 0.78,
                         "Mpumalanga" = 0.97)[[reg]]
    
    rain_adjustment <- 0.20 * (1 - rain_norm[i])
    p_rainfed <- pmin(0.97, pmax(0.50, base_rainfed + rain_adjustment - 0.10))
    sample(c("Rainfed", "Irrigated"), 1, prob = c(p_rainfed, 1 - p_rainfed))
  })
  
  # 5. FERTILISER - linked to rainfall and irrigation
  calc_lnorm_params <- function(m, s) {
    mu    <- log(m^2 / sqrt(m^2 + s^2))
    sigma <- sqrt(log(1 + (s / m)^2))
    return(c(mu, sigma))
  }
  
  fert_p        <- calc_lnorm_params(180, 80)
  fert_base     <- rlnorm(n, fert_p[1], fert_p[2])
  rain_boost    <- 80 * rain_norm                             # wetter = higher yield potential = more fertiliser
  irrig_boost   <- ifelse(irrigation == "Irrigated", 50, 0)  # irrigated farms apply more
  fertiliser    <- pmax(0, pmin(500, fert_base + rain_boost + irrig_boost))
  
  # 6. SOIL ORGANIC MATTER - driven by rainfall, temperature, fertiliser
  som_base <- 0.55 * rain_norm - 0.30 * temp_norm + 0.25 * (fertiliser / max(fertiliser))
  soil_organic_matter <- pmin(pmax(3.5 + som_base + rnorm(n, 0, 0.20), 1), 6)
  som_norm <- (soil_organic_matter - min(soil_organic_matter)) / 
    (max(soil_organic_matter) - min(soil_organic_matter))
  
  # 7. CHEMICAL WEED CONTROL - driven by temperature, rainfall, SOM
  pest_p        <- calc_lnorm_params(150, 70)
  pest_base     <- rlnorm(n, pest_p[1], pest_p[2])
  temp_boost    <- 60 * temp_norm   # warmer = more pest pressure
  rain_boost2   <- 50 * rain_norm   # wetter = more weed growth
  som_boost     <- 35 * som_norm    # better managed = more inputs
  pesticide_kgha <- pmax(0, pmin(500, pest_base + temp_boost + rain_boost2 + som_boost))
  
  # 8. BRANDS
  seed_brand    <- sample(c("Pannar", "Dekalb", "Pioneer"), n, replace = TRUE)
  tractor_brand <- sample(c("John Deere", "Case", "New Holland"), n, replace = TRUE, 
                          prob = c(0.6, 0.2, 0.2))
  
  # 9. ASSEMBLE
  df <- data.frame(
    region                 = region,            
    rainfall_mm            = round(rainfall, 1),
    temperature_C          = round(temperature, 2),
    fertiliser_kgha        = round(fertiliser, 2),
    chem_weed_control_kgha = round(pesticide_kgha, 2),
    irrigation             = irrigation,  
    soil_organic_matter    = round(soil_organic_matter, 3),
    seed_brand             = seed_brand,      
    tractor_brand          = tractor_brand     
  )
  
  return(df)
}