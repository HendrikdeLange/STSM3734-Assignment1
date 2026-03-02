df <- read.csv("C:\\Users\\Marko\\Documents\\STSM3734-Assignment1\\maize_data_observed.csv")
library(ggplot2)

ggplot(df, aes(x = rainfall_mm)) +
  geom_histogram(aes(y = after_stat(density)), bins = 10,
                 fill = "#4A90C4", color = "white", linewidth = 0.3, alpha = 0.75) +
  geom_density(color = "#1B4F72", linewidth = 0.9) +
  labs(x = "Rainfall (mm)", y = "Density") +
  theme_classic(base_size = 12) +
  theme(
    axis.line        = element_line(linewidth = 0.4, color = "black"),
    axis.ticks       = element_line(linewidth = 0.4),
    axis.text        = element_text(color = "black"),
    axis.title       = element_text(color = "black"),
    panel.grid       = element_blank(),
    plot.margin      = margin(5, 10, 5, 5)
  )

ggsave("rainfall.tiff", width = 3.5, height = 3, units = "in", dpi = 600, compression = "lzw")

ggplot(df, aes(x = fertiliser_kgha)) +
  geom_histogram(aes(y = after_stat(density)), bins = 10,
                 fill = "#26C485", color = "white", linewidth = 0.3, alpha = 0.75) +
  geom_density(color = "#31572C", linewidth = 0.9) +
  labs(x = "Fertiliser (kg/ha)", y = "Density") +
  theme_classic(base_size = 12) +
  theme(
    axis.line        = element_line(linewidth = 0.4, color = "black"),
    axis.ticks       = element_line(linewidth = 0.4),
    axis.text        = element_text(color = "black"),
    axis.title       = element_text(color = "black"),
    panel.grid       = element_blank(),
    plot.margin      = margin(5, 10, 5, 5)
  )

ggsave("fertiliser.tiff", width = 3.5, height = 3, units = "in", dpi = 600, compression = "lzw")

ggplot(df, aes(x = temperature_C)) +
  geom_histogram(aes(y = after_stat(density)), bins = 10,
                 fill = "#F17300", color = "white", linewidth = 0.3, alpha = 0.75) +
  geom_density(color = "#4C5B5C", linewidth = 0.9) +
  labs(x = "Temperature (C)", y = "Density") +
  theme_classic(base_size = 12) +
  theme(
    axis.line        = element_line(linewidth = 0.4, color = "black"),
    axis.ticks       = element_line(linewidth = 0.4),
    axis.text        = element_text(color = "black"),
    axis.title       = element_text(color = "black"),
    panel.grid       = element_blank(),
    plot.margin      = margin(5, 10, 5, 5)
  )

ggsave("temperature.tiff", width = 3.5, height = 3, units = "in", dpi = 600, compression = "lzw")


ggplot(df, aes(x = soil_organic_matter)) +
  geom_histogram(aes(y = after_stat(density)), bins = 10,
                 fill = "#816C61", color = "white", linewidth = 0.3, alpha = 0.75) +
  geom_density(color = "#280000", linewidth = 0.9) +
  labs(x = "SOM (%)", y = "Density") +
  theme_classic(base_size = 12) +
  theme(
    axis.line        = element_line(linewidth = 0.4, color = "black"),
    axis.ticks       = element_line(linewidth = 0.4),
    axis.text        = element_text(color = "black"),
    axis.title       = element_text(color = "black"),
    panel.grid       = element_blank(),
    plot.margin      = margin(5, 10, 5, 5)
  )

ggsave("soil_organic_matter.tiff", width = 3.5, height = 3, units = "in", dpi = 600, compression = "lzw")

ggplot(df, aes(x = chem_weed_control_kgha)) +
  geom_histogram(aes(y = after_stat(density)), bins = 10,
                 fill = "#A40606", color = "white", linewidth = 0.3, alpha = 0.75) +
  geom_density(color = "#AEC5EB", linewidth = 0.9) +
  labs(x = "Weed Control (kg/ha)", y = "Density") +
  theme_classic(base_size = 12) +
  theme(
    axis.line        = element_line(linewidth = 0.4, color = "black"),
    axis.ticks       = element_line(linewidth = 0.4),
    axis.text        = element_text(color = "black"),
    axis.title       = element_text(color = "black"),
    panel.grid       = element_blank(),
    plot.margin      = margin(5, 10, 5, 5)
  )

ggsave("weed_control.tiff", width = 3.5, height = 3, units = "in", dpi = 600, compression = "lzw")

library(ggplot2)
library(patchwork)

# Shared colours
region_colours <- c("Free State" = "#E8851A",
                    "North West" = "#4A90C4",
                    "Mpumalanga" = "#3A9E5F")

# Shared theme
region_theme <- function() {
  list(
    scale_color_manual(values = region_colours),
    scale_fill_manual(values  = region_colours),
    theme_classic(base_size = 11),
    theme(
      axis.line       = element_line(linewidth = 0.4),
      axis.text       = element_text(color = "black"),
      axis.title      = element_text(color = "black"),
      panel.grid      = element_blank(),
      legend.position = "none",
      plot.margin     = margin(5, 8, 5, 5)
    )
  )
}

# 1. Rainfall
p1 <- ggplot(df, aes(x = rainfall_mm, color = region, fill = region)) +
  geom_density(linewidth = 0.9, alpha = 0.15) +
  labs(x = "Rainfall (mm)", y = "Density") +
  region_theme()

# 2. Temperature
p2 <- ggplot(df, aes(x = temperature_C, color = region, fill = region)) +
  geom_density(linewidth = 0.9, alpha = 0.15) +
  labs(x = "Temperature (°C)", y = "Density") +
  region_theme()

# 3. Soil Organic Matter
p3 <- ggplot(df, aes(x = region, y = soil_organic_matter, color = region, fill = region)) +
  geom_boxplot(alpha = 0.3, linewidth = 0.7, outlier.size = 0.8) +
  labs(x = NULL, y = "Soil Organic Matter") +
  region_theme()

# 4. Weed Control
p4 <- ggplot(df, aes(x = chem_weed_control_kgha, color = region, fill = region)) +
  geom_density(linewidth = 0.9, alpha = 0.15) +
  labs(x = "Weed Control (kg/ha)", y = "Density") +
  region_theme()

combined <- ((p1 + p2) / (p3 + p4) +
               plot_layout(guides = "collect"))

combined <- combined & theme(
  legend.position = "bottom",
  legend.title    = element_blank(),
  legend.text     = element_text(size = 10, face = "bold")
)

combined

ggsave("regions_overview.tiff", combined, width = 8, height = 7,
       units = "in", dpi = 600, compression = "lzw")

#Scatterplot Matrix
library(GGally)
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

# Per-variable colours
var_colours <- c(
  "rainfall_mm"            = "#4A90C4",
  "fertiliser_kgha"        = "#26C485",
  "temperature_C"          = "#F17300",
  "soil_organic_matter"    = "#816C61",
  "chem_weed_control_kgha" = "#A40606"
)

# Custom diagonal density per variable colour
diag_custom <- function(data, mapping, ...) {
  varname <- rlang::as_name(mapping$x)   # fix: use as_name()
  col     <- var_colours[varname]
  
  ggplot(data, mapping) +
    geom_density(fill = col, color = scales::muted(col),
                 alpha = 0.7, linewidth = 0.8) +
    theme_classic(base_size = 9) +
    theme(axis.title   = element_blank(),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y  = element_blank())
}

# Custom lower scatterplot per variable colour
scatter_custom <- function(data, mapping, ...) {
  varname <- rlang::as_name(mapping$x)   # fix: use as_name()
  col     <- var_colours[varname]
  
  ggplot(data, mapping) +
    geom_point(color = col, alpha = 0.35, size = 0.7) +
    theme_classic(base_size = 9) +
    theme(panel.grid = element_blank())
}

# Plot
ggpairs(
  df,
  columns    = c("rainfall_mm", "fertiliser_kgha", "temperature_C",
                 "soil_organic_matter", "chem_weed_control_kgha"),
  columnLabels = c("Rainfall (mm)", "Fertiliser (kg/ha)", "Temperature (°C)",
                   "Soil Organic Matter", "Weed Control (kg/ha)"),
  upper      = list(continuous = cor_coloured),
  lower      = list(continuous = scatter_custom),
  diag       = list(continuous = diag_custom),
  axisLabels = "show"
) +
  theme_classic(base_size = 11) +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 9),
    panel.grid       = element_blank(),
    axis.text        = element_text(color = "black", size = 7),
    plot.margin      = margin(5, 5, 5, 5)
  )

ggsave("scatterplot_matrix.tiff", width = 8, height = 8,
       units = "in", dpi = 600, compression = "lzw")


#yield distribution

ggplot(df, aes(x = yield_tha)) +
  geom_histogram(aes(y = after_stat(density)), bins = 10,
                 fill = "#ECC30B", color = "white", linewidth = 0.3, alpha = 0.75) +
  geom_density(color = "#881600", linewidth = 0.9) +
  labs(x = "Maize Yield (t/ha)", y = "Density") +
  theme_classic(base_size = 12) +
  theme(
    axis.line        = element_line(linewidth = 0.4, color = "black"),
    axis.ticks       = element_line(linewidth = 0.4),
    axis.text        = element_text(color = "black"),
    axis.title       = element_text(color = "black"),
    panel.grid       = element_blank(),
    plot.margin      = margin(5, 10, 5, 5)
  )

ggsave("yield.tiff", width = 5, height = 3, units = "in", dpi = 600, compression = "lzw")

#stargazer table for doc x
library(stargazer)

# --- Replace 'your_data' with your actual dataframe ---
data_preview <- head(df, 10)

# Round all numeric columns to 2 decimal places
data_preview <- data_preview |>
  mutate(across(where(is.numeric), ~ round(., 2)))

# --- Replace with your actual column names (must match number of columns) ---
col_names <- c("Region","Rainfall (mm)", "Temperature (°C)", "Fertiliser (kg/ha)", 
               "Weed Control (kg/ha)", "Soil Organic Matter", "Irrigation",
               "Seed Strain",
               "Tractor_Brand", "Yield (kg/ha)")  

colnames(data_preview) <- col_names

stargazer(
  data_preview,
  type        = "html",
  title       = "Observational Data Preview",
  summary     = FALSE,
  rownames    = FALSE,
  digits      = 2,
  out         = "data_preview.html"
)