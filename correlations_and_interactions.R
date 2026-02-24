library(GGally)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)   # for correlation plot
library(patchwork)  # for combining plots
df <- read.csv("C:\\Users\\hendr\\OneDrive\\Documents\\STSM3734-MAIZE_SIMULATION\\maize_data_observed.csv")

colnames(df)
# ============================================================
# 1. DISTRIBUTION PLOTS
# ============================================================

# Numeric variables: histogram + density
numeric_vars <- c("temperature_C", "rainfall_mm", "fertiliser_kgha",
                  "chem_weed_control_kgha", "soil_organic_matter", "yield_tha")

plot_list_num <- lapply(numeric_vars, function(var) {
  ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)), bins = 30,
                   fill = "#4A90D9", colour = "white", alpha = 0.8) +
    geom_density(colour = "#D95040", linewidth = 1) +
    labs(title = paste("Distribution of", var), x = var, y = "Density") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
})

wrap_plots(plot_list_num, ncol = 3)


# Categorical variables: bar charts
cat_vars <- c("region", "irrigation", "seed_brand", "tractor_brand")

plot_list_cat <- lapply(cat_vars, function(var) {
  ggplot(df, aes(x = .data[[var]])) +
    geom_bar(fill = "#5CB85C", colour = "white", alpha = 0.85) +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.4, size = 3.5) +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal(base_size = 13) +
    theme(plot.title  = element_text(face = "bold"),
          axis.text.x = element_text(angle = 30, hjust = 1))
})

wrap_plots(plot_list_cat, ncol = 2)


# ============================================================
# 2. REGION DOUBLE PLOTS (boxplot)
# ============================================================

make_region_double <- function(y_var, y_label) {
  
  p_box <- ggplot(df, aes(x = region, y = .data[[y_var]], fill = region)) +
    geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.fill = "white") +
    scale_fill_brewer(palette = "Set2") +
    labs(title = paste("Region vs", y_label), subtitle = "Boxplot",
         x = NULL, y = y_label) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none",
          plot.title    = element_text(face = "bold"),
          axis.text.x   = element_text(angle = 30, hjust = 1))
  
  p_box 
}

make_region_double("temperature_C", "Temperature (°C)")
make_region_double("rainfall_mm",   "Rainfall (mm)")
make_region_double("yield_tha",     "Yield (t/ha)")


# ============================================================
# 3. CORRELATION PLOT
# ============================================================

cor_matrix <- df %>%
  select(all_of(numeric_vars)) %>%
  mutate(across(everything(), as.numeric)) %>%
  cor(use = "pairwise.complete.obs")

corrplot(
  cor_matrix,
  method      = "color",
  type        = "upper",
  order       = "hclust",
  addCoef.col = "black",
  number.cex  = 0.85,
  tl.col      = "black",
  tl.srt      = 45,
  col         = colorRampPalette(c("#D95040", "white", "#4A90D9"))(200),
  title       = "Correlation Matrix — Numeric Variables",
  mar         = c(0, 0, 2, 0)
)

ggpairs(df)