df <- read.csv("C:\\Users\\hendr\\OneDrive\\Documents\\STSM3734-MAIZE_SIMULATION\\maize_data_observed.csv")
library(dplyr)
source("simulate_maize_data.R")


set.seed(123)
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
  left_join(soil_om_means,      by = "soil_organic_matter_cat") %>%
  mutate(
    rainfall_mean_z            = (rainfall_mean - mean(df_ref$rainfall_mm))                / sd(df_ref$rainfall_mm),
    temperature_mean_z         = (temperature_mean - mean(df_ref$temperature_C))           / sd(df_ref$temperature_C),
    fertiliser_mean_z          = (fertiliser_mean - mean(df_ref$fertiliser_kgha))          / sd(df_ref$fertiliser_kgha),
    weed_control_mean_z        = (weed_control_mean - mean(df_ref$chem_weed_control_kgha)) / sd(df_ref$chem_weed_control_kgha),
    soil_organic_matter_mean_z = (soil_organic_matter_mean - mean(df_ref$soil_organic_matter)) / sd(df_ref$soil_organic_matter)
  )

nrow(final_data)  # 131,220

#correct way
#Adjusting based on Experience
#RAIN
#1
beta_rain <- 0.60 * rainfall_means$rainfall_mean
beta_rain_dev <- beta_rain - beta_rain[1]
intercept_new <- 6 + beta_exp[1] #NEW INTERCEPT = OLD + LOWEST TERTILE MEAN

#2
beta_rain_2 <- beta_rain_dev[2] 
beta_rain_3 <- beta_rain_dev[3] 

# 3
rain_medium <- ifelse(final_data$rainfall_cat == "Medium", 1, 0) 
rain_high <- ifelse(final_data$rainfall_cat == "High", 1, 0)

#TEMPERATURE








#FERTILISER









#WEED CONTROL








#SOIL OM
























# --- Step 3: Create dummies ---
final_data$Irrigated  <- as.integer(final_data$irrigation == "Irrigated")
final_data$Dekalb     <- as.integer(final_data$seed_brand != "Pioneer" & final_data$seed_brand != "Pannar")
final_data$Pioneer    <- as.integer(final_data$seed_brand == "Pioneer")
final_data$John_Deere <- as.integer(final_data$tractor_brand != "Case" & final_data$tractor_brand != "New Holland")

# --- Step 4: Simulate yield ---
final_data$yield_tha <- (
  6.0
  + 0.80 * final_data$rainfall_mean_z
  - 0.25 * final_data$temperature_mean_z
  + 0.40 * final_data$fertiliser_mean_z
  + 0.20 * final_data$weed_control_mean_z
  + 0.50 * final_data$Irrigated
  + 0.20 * final_data$soil_organic_matter_mean_z
  + 0.30 * final_data$Dekalb
  - 0.20 * final_data$Dekalb   * final_data$temperature_mean_z
  - 0.20 * final_data$Pioneer  * final_data$temperature_mean_z
  + 0.25 * final_data$Pioneer  * final_data$Irrigated
  + 0.20 * final_data$Pioneer  * final_data$fertiliser_mean_z
  + 0.15 * final_data$John_Deere
  + rnorm(nrow(final_data), mean = 0, sd = 0.7)
)
final_data$yield_tha <- pmin(pmax(final_data$yield_tha, 2), 16)

# --- Step 5: Save ---
write.csv(final_data, file = "maize_experimental_data.csv", row.names = FALSE)

# --- Step 6: Model ---
model_exp <- lm(yield_tha ~ rainfall_mean_z + temperature_mean_z +
                  fertiliser_mean_z + weed_control_mean_z + Irrigated +
                  soil_organic_matter_mean_z + Dekalb + Pioneer + John_Deere +
                  Dekalb:temperature_mean_z + Pioneer:temperature_mean_z +
                  Pioneer:Irrigated + Pioneer:fertiliser_mean_z,
                data = final_data)

summary(model_exp)


#SCATTERPLOT
library(scales)
library(rlang)

# Colour-coded correlation panels
cor_coloured <- function(data, mapping, ...) {
  x <- GGally::eval_data_col(data, mapping$x)
  y <- GGally::eval_data_col(data, mapping$y)
  r <- cor(x, y, use = "complete.obs")
  
  bg <- scales::col_numeric(
    palette = c("#D55E00", "#FFFFFF", "#0072B2"),
    domain  = c(-1, 1)
  )(r)
  
  ggplot(data, mapping) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
             fill = bg, alpha = 0.7) +
    annotate("text", x = mean(range(x, na.rm = TRUE)),
             y = mean(range(y, na.rm = TRUE)),
             label = round(r, 2), size = 4.5, fontface = "bold",
             color = ifelse(abs(r) > 0.5, "white", "black")) +
    theme_void()
}

var_colours <- c(
"rainfall_mean"            = "#4A90C4",
"fertiliser_mean"          = "#26C485",
"temperature_mean"         = "#F17300",
"soil_organic_matter_mean" = "#816C61",
"weed_control_mean"        = "#A40606"
)

diag_custom <- function(data, mapping, ...) {
  varname <- rlang::as_name(mapping$x)
  col     <- var_colours[[varname]]
  if (is.null(col)) col <- "grey50"   # fallback prevents NULL error
  
  ggplot(data, mapping) +
    geom_density(fill = col, color = scales::muted(col),
                 alpha = 0.7, linewidth = 0.8) +
    theme_classic(base_size = 9) +
    theme(axis.title   = element_blank(),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y  = element_blank())
}

scatter_custom <- function(data, mapping, ...) {
  varname <- rlang::as_name(mapping$x)
  col     <- var_colours[[varname]]
  if (is.null(col)) col <- "grey50"   # fallback prevents NULL error
  
  ggplot(data, mapping) +
    geom_point(color = col, alpha = 0.35, size = 0.7) +
    theme_classic(base_size = 9) +
    theme(panel.grid = element_blank())
}

# Plot
ggpairs(
  final_data,
  columns      = c("rainfall_mean", "fertiliser_mean", "temperature_mean",
                   "soil_organic_matter_mean", "weed_control_mean"),
  columnLabels = c("Rainfall (mm)", "Fertiliser (kg/ha)", "Temperature (°C)",
                   "Soil Organic Matter", "Weed Control (kg/ha)"),
  upper        = list(continuous = cor_coloured),
  lower        = list(continuous = scatter_custom),
  diag         = list(continuous = diag_custom),
  axisLabels   = "show"
) +
  theme_classic(base_size = 11) +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 9),
    panel.grid       = element_blank(),
    axis.text        = element_text(color = "black", size = 7),
    plot.margin      = margin(5, 5, 5, 5)
  )

ggsave("scatterplot_matrix_experimental.tiff", width = 8, height = 8,
       units = "in", dpi = 600, compression = "lzw")