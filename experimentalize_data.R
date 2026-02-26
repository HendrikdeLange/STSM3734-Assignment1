df <- read.csv("C:\\Users\\hendr\\OneDrive\\Documents\\STSM3734-MAIZE_SIMULATION\\maize_data_observed.csv")
library(dplyr)
install.packages("corrplot")
library(corrplot)
df_ex <- df

# ============================================================
# STEP 1: Categorise continuous variables
# ============================================================
df_ex$temperature_C_cat <- cut(df_ex$temperature_C,
                               breaks = quantile(df_ex$temperature_C, probs = c(0, 1/3, 2/3, 1)),
                               include.lowest = TRUE, labels = c("Low", "Medium", "High"))
df_ex$rainfall_mm_cat <- cut(df_ex$rainfall_mm,
                             breaks = quantile(df_ex$rainfall_mm, probs = c(0, 1/3, 2/3, 1)),
                             include.lowest = TRUE, labels = c("Low", "Medium", "High"))
df_ex$fertiliser_kgha_cat <- cut(df_ex$fertiliser_kgha,
                                 breaks = quantile(df_ex$fertiliser_kgha, probs = c(0, 1/3, 2/3, 1)),
                                 include.lowest = TRUE, labels = c("Low", "Medium", "High"))
df_ex$chem_weed_control_kgha_cat <- cut(df_ex$chem_weed_control_kgha,
                                        breaks = quantile(df_ex$chem_weed_control_kgha, probs = c(0, 1/3, 2/3, 1)),
                                        include.lowest = TRUE, labels = c("Low", "Medium", "High"))
df_ex$soil_organic_matter_cat <- cut(df_ex$soil_organic_matter,
                                     breaks = quantile(df_ex$soil_organic_matter, probs = c(0, 1/3, 2/3, 1)),
                                     include.lowest = TRUE, labels = c("Low", "Medium", "High"))

# ============================================================
# STEP 2: Compute category means
# ============================================================
temperature_C_means          <- df_ex %>% group_by(temperature_C_cat) %>% summarise(temperature_mean = mean(temperature_C))
rainfall_mm_means            <- df_ex %>% group_by(rainfall_mm_cat) %>% summarise(rainfall_mean = mean(rainfall_mm))
fertiliser_kgha_means        <- df_ex %>% group_by(fertiliser_kgha_cat) %>% summarise(fertiliser_mean = mean(fertiliser_kgha))
chem_weed_control_kgha_means <- df_ex %>% group_by(chem_weed_control_kgha_cat) %>% summarise(chem_weed_control_mean = mean(chem_weed_control_kgha))
soil_organic_matter_means    <- df_ex %>% group_by(soil_organic_matter_cat) %>% summarise(soil_organic_matter_mean = mean(soil_organic_matter))

# ============================================================
# STEP 3: Build temp_df (drop cols that will come from block)
# ============================================================
temp_df <- df_ex %>%
  select(-c(region, irrigation, seed_brand, tractor_brand,
            temperature_C, rainfall_mm, fertiliser_kgha,
            chem_weed_control_kgha, soil_organic_matter, yield_tha))

# ============================================================
# STEP 4: Create experimental block design
# ============================================================
block <- expand.grid(
  region        = unique(df_ex$region),
  irrigation    = unique(df_ex$irrigation),
  seed_brand    = unique(df_ex$seed_brand),
  tractor_brand = unique(df_ex$tractor_brand)
)

n_per_group <- 20

final_data <- temp_df %>%
  group_by(temperature_C_cat, rainfall_mm_cat, fertiliser_kgha_cat,
           chem_weed_control_kgha_cat, soil_organic_matter_cat) %>%
  group_modify(~{
    block_rep <- block[rep(1:nrow(block), length.out = n_per_group), ]
    rownames(block_rep) <- NULL
    group_rep <- .x[sample(1:nrow(.x), n_per_group, replace = TRUE), ]
    rownames(group_rep) <- NULL
    cbind(group_rep, block_rep)
  }) %>%
  ungroup()

# ============================================================
# STEP 5: Merge category means & standardise
# ============================================================
final_data <- final_data %>%
  left_join(temperature_C_means,          by = "temperature_C_cat") %>%
  left_join(rainfall_mm_means,            by = "rainfall_mm_cat") %>%
  left_join(fertiliser_kgha_means,        by = "fertiliser_kgha_cat") %>%
  left_join(chem_weed_control_kgha_means, by = "chem_weed_control_kgha_cat") %>%
  left_join(soil_organic_matter_means,    by = "soil_organic_matter_cat")

final_data$temperature_C_z          <- scale(final_data$temperature_mean)
final_data$rainfall_mm_z            <- scale(final_data$rainfall_mean)
final_data$fertiliser_kgha_z        <- scale(final_data$fertiliser_mean)
final_data$chem_weed_control_kgha_z <- scale(final_data$chem_weed_control_mean)
final_data$soil_organic_matter_z    <- scale(final_data$soil_organic_matter_mean)

# ============================================================
# STEP 6: Create dummies & generate yield
# ============================================================
Irrigated  <- ifelse(final_data$irrigation    == "Irrigated",  1, 0)
Dekalb     <- ifelse(final_data$seed_brand    == "Dekalb",     1, 0)
Pioneer    <- ifelse(final_data$seed_brand    == "Pioneer",    1, 0)
John_Deere <- ifelse(final_data$tractor_brand == "John Deere", 1, 0)

final_data$yield_tha <- (
  6.0
  + 0.80 * final_data$rainfall_mm_z
  - 0.25 * final_data$temperature_C_z
  + 0.40 * final_data$fertiliser_kgha_z
  + 0.20 * final_data$chem_weed_control_kgha_z
  + 0.40 * Irrigated
  + 0.10 * final_data$soil_organic_matter_z
  + 0.30 * Dekalb
  - 0.20 * Dekalb  * final_data$temperature_C_z
  - 0.20 * Pioneer * final_data$temperature_C_z
  + 0.25 * Pioneer * Irrigated
  + 0.20 * Pioneer * final_data$fertiliser_kgha_z
  + 0.10 * John_Deere
  + rnorm(nrow(final_data), mean = 0, sd = 0.5)
)

# ============================================================
# STEP 7: Compare observational vs experimental yield
# ============================================================
yield_obs <- df$yield_tha
yield_exp <- final_data$yield_tha

cat("Mean difference (Obs - Exp):", mean(yield_obs) - mean(yield_exp), "\n")

# Superimposed histograms
breaks_seq <- pretty(range(c(yield_obs, yield_exp)), n = 30)

hist(yield_obs,
     breaks      = breaks_seq,
     probability = TRUE,
     col         = rgb(0, 0, 1, 0.4),
     border      = NA,
     main        = "Yield Distribution: Observational vs Experimental",
     xlab        = "Yield (t/ha)",
     ylim        = c(0, max(density(yield_obs)$y, density(yield_exp)$y)))

hist(yield_exp,
     breaks      = breaks_seq,
     probability = TRUE,
     col         = rgb(1, 0, 0, 0.4),
     border      = NA,
     add         = TRUE)

legend("topright",
       legend = c("Observational", "Experimental"),
       fill   = c(rgb(0, 0, 1, 0.4), rgb(1, 0, 0, 0.4)))

# QQ Plot
qqplot(yield_obs, yield_exp,
       main = "QQ Plot: Experimental vs Observational Yield",
       xlab = "Observational Quantiles",
       ylab = "Experimental Quantiles",
       pch  = 16, col = "darkgray")
abline(0, 1, col = "red", lwd = 2)



# ============================================================
# STEP 8: Fit model & check if true coefficients are recovered
# ============================================================
final_data$irrigation    <- factor(final_data$irrigation,    levels = c("Rainfed", "Irrigated"))
final_data$seed_brand    <- factor(final_data$seed_brand,    levels = c("Pannar", "Dekalb", "Pioneer"))
final_data$tractor_brand <- factor(final_data$tractor_brand, levels = c("John Deere", "Case", "New Holland"))

model_exp <- lm(yield_tha ~
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
                data = final_data)

summary(model_exp)

# ============================================================
# STEP 9: Correlation comparison - Observational vs Experimental
# ============================================================

# Standardise observational data first
df$temperature_C_z          <- scale(df$temperature_C)
df$rainfall_mm_z            <- scale(df$rainfall_mm)
df$fertiliser_kgha_z        <- scale(df$fertiliser_kgha)
df$chem_weed_control_kgha_z <- scale(df$chem_weed_control_kgha)
df$soil_organic_matter_z    <- scale(df$soil_organic_matter)

# Side by side correlation plots
par(mfrow = c(1, 2))

df %>%
  select(temperature_C_z, rainfall_mm_z, fertiliser_kgha_z,
         chem_weed_control_kgha_z, soil_organic_matter_z) %>%
  cor() %>%
  corrplot(method = "color", type = "upper", addCoef.col = "black",
           tl.col = "black", title = "Observational", mar = c(0,0,1,0))

final_data %>%
  select(temperature_C_z, rainfall_mm_z, fertiliser_kgha_z,
         chem_weed_control_kgha_z, soil_organic_matter_z) %>%
  cor() %>%
  corrplot(method = "color", type = "upper", addCoef.col = "black",
           tl.col = "black", title = "Experimental", mar = c(0,0,1,0))

par(mfrow = c(1, 1))
write.csv(final_data, file = "experimental_data.csv", row.names = FALSE)
