df <- read.csv("~/STSM3734-Assignment1/obs_data/Observational_Group14.csv", sep=";", dec=".")

# ============================================================
# Observational_Group14 - Full Analysis Script
# Scatterplots, Density, Correlation & Linear Model
# ============================================================

# ---- Set working directory to a writable location -----------
# Change this path to wherever you want the PDFs saved
output_dir <- "~/STSM3734-Assignment1/outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
setwd(output_dir)
cat("Output files will be saved to:", getwd(), "\n")

# ---- Install packages if needed -----------------------------
# install.packages(c("ggplot2", "GGally", "corrplot", "reshape2",
#                    "lmtest", "car", "nortest"))

library(ggplot2)
library(GGally)
library(corrplot)
library(reshape2)
library(lmtest)
library(car)
library(nortest)

# ---- Load Data ----------------------------------------------
df <- read.csv("~/STSM3734-Assignment1/obs_data/Observational_Group14.csv",
               sep = ";", dec = ".")

# Rename Weight column (remove special characters)
colnames(df)[colnames(df) == "Weight.kg."] <- "Weight"
colnames(df)[colnames(df) == "Weight(kg)"] <- "Weight"   # handles either version

cat("Data loaded:", nrow(df), "rows,", ncol(df), "columns\n")
cat("Columns:", paste(colnames(df), collapse = ", "), "\n\n")

# ---- Keep a NUMERIC copy for lm() and correlations ---------
df_num_raw <- df   # x_sex and x_pt remain 0/1 here — used for lm()

# ---- Factor version for plots only --------------------------
df_plot <- df
df_plot$x_sex <- factor(df_plot$x_sex, labels = c("Female", "Male"))
df_plot$x_pt  <- factor(df_plot$x_pt,  labels = c("No PT", "PT"))

# Numeric columns for density/correlation
num_vars <- c("x_visits", "x_height", "x_caloric_balance_weekly", "x_time", "Weight")
df_num <- df[, num_vars]
colnames(df_num) <- c("Visits", "Height_cm", "Caloric_Balance", "Time", "Weight_kg")

# ================================================================
# 1. SCATTERPLOTS
# ================================================================

pdf("scatterplots.pdf", width = 10, height = 8)

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

sex_col <- ifelse(df_plot$x_sex == "Male", "steelblue", "tomato")

plot(df$x_height, df$Weight,
     main = "Weight vs Height",
     xlab = "Height (cm)", ylab = "Weight (kg)",
     col = sex_col, pch = 16, cex = 0.8)
legend("topleft", legend = c("Female", "Male"),
       col = c("tomato", "steelblue"), pch = 16, cex = 0.8)
abline(lm(Weight ~ x_height, data = df), col = "darkgrey", lwd = 2)

plot(df$x_caloric_balance_weekly, df$Weight,
     main = "Weight vs Caloric Balance",
     xlab = "Caloric Balance (weekly)", ylab = "Weight (kg)",
     col = sex_col, pch = 16, cex = 0.8)
abline(lm(Weight ~ x_caloric_balance_weekly, data = df), col = "darkgrey", lwd = 2)

plot(df$x_visits, df$Weight,
     main = "Weight vs Visits",
     xlab = "Number of Visits", ylab = "Weight (kg)",
     col = sex_col, pch = 16, cex = 0.8)
abline(lm(Weight ~ x_visits, data = df), col = "darkgrey", lwd = 2)

plot(df$x_time, df$Weight,
     main = "Weight vs Time",
     xlab = "Time", ylab = "Weight (kg)",
     col = sex_col, pch = 16, cex = 0.8)
abline(lm(Weight ~ x_time, data = df), col = "darkgrey", lwd = 2)

boxplot(Weight ~ x_sex, data = df_plot,
        main = "Weight by Sex", xlab = "Sex", ylab = "Weight (kg)",
        col = c("tomato", "steelblue"))

boxplot(Weight ~ x_pt, data = df_plot,
        main = "Weight by PT Status", xlab = "Personal Trainer", ylab = "Weight (kg)",
        col = c("gold", "mediumseagreen"))

dev.off()
cat("✔ scatterplots.pdf saved\n")

# ================================================================
# 2. DENSITY PLOTS
# ================================================================

pdf("density_plots.pdf", width = 12, height = 8)

df_long <- melt(df_num)

print(
  ggplot(df_long, aes(x = value, fill = variable)) +
    geom_density(alpha = 0.5, colour = "white") +
    facet_wrap(~ variable, scales = "free", ncol = 3) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Density Plots – All Numeric Variables", x = NULL, y = "Density") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none", strip.text = element_text(face = "bold"))
)

dev.off()
cat("✔ density_plots.pdf saved\n")

# ================================================================
# 3. CORRELATION HEATMAP
# ================================================================

pdf("correlation_heatmap.pdf", width = 8, height = 7)

cor_matrix <- cor(df_num, use = "complete.obs")

corrplot(cor_matrix,
         method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("tomato", "white", "steelblue"))(200),
         title = "Correlation Matrix", mar = c(0, 0, 2, 0))

dev.off()
cat("✔ correlation_heatmap.pdf saved\n")

# ================================================================
# 4. GGPAIRS PLOT
# ================================================================

pdf("ggpairs_plot.pdf", width = 12, height = 10)

print(
  ggpairs(df_num,
          aes(alpha = 0.4),
          upper = list(continuous = wrap("cor", size = 4)),
          lower = list(continuous = wrap("points", alpha = 0.3, size = 0.8)),
          diag  = list(continuous = wrap("densityDiag", fill = "steelblue")),
          columnLabels = colnames(df_num)) +
    theme_minimal(base_size = 11) +
    labs(title = "Pairwise Scatterplot Matrix with Correlations")
)

dev.off()
cat("✔ ggpairs_plot.pdf saved\n")

# ================================================================
# STEP 1: FIT THE LINEAR MODEL (uses numeric df — NOT factor version)
# ================================================================

model <- lm(Weight ~ x_sex + x_pt + x_visits + x_height +
              x_caloric_balance_weekly + x_time,
            data = df)   # df still has x_sex/x_pt as 0/1 numeric

cat("\n================================================================\n")
cat("  FULL MODEL SUMMARY\n")
cat("================================================================\n")
print(summary(model))

cat("\n--- Beta Coefficients ---\n")
coefs <- as.data.frame(summary(model)$coefficients)
colnames(coefs) <- c("Estimate", "Std.Error", "t.value", "p.value")
print(round(coefs, 6))

cat("\n--- 95% Confidence Intervals ---\n")
print(round(confint(model), 6))

# ================================================================
# STEP 2: VALIDATE THE COEFFICIENTS
# ================================================================

# 2a. Manual OLS: beta = (X'X)^-1 X'y
cat("\n--- Manual OLS Verification: beta = (X'X)^-1 X'y ---\n")
X <- model.matrix(model)
y <- df$Weight
beta_manual <- solve(t(X) %*% X) %*% t(X) %*% y
cat("Manually computed betas:\n")
print(round(beta_manual, 6))
cat("Difference vs lm() (should be ~0):\n")
print(round(beta_manual - coef(model), 10))

# 2b. Model fit statistics
s <- summary(model)
cat(sprintf("\nR-squared         : %.6f\n", s$r.squared))
cat(sprintf("Adjusted R-squared: %.6f\n", s$adj.r.squared))
cat(sprintf("F-statistic       : %.4f (df1=%d, df2=%d)\n",
            s$fstatistic[1], s$fstatistic[2], s$fstatistic[3]))
cat(sprintf("F p-value         : %.2e\n",
            pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)))
cat(sprintf("Residual SE       : %.6f\n", s$sigma))

# 2c. Significance flags
sig <- coefs
sig$Significant <- ifelse(sig$p.value < 0.05, "YES ***", "NO")
cat("\n--- Coefficient Significance ---\n")
print(sig[, c("Estimate", "p.value", "Significant")])

# 2d. VIF
cat("\n--- VIF (Variance Inflation Factors) ---\n")
print(round(vif(model), 4))

# 2e. Normality of residuals
cat("\n--- Residual Normality Tests ---\n")
residuals_model <- residuals(model)
sw <- shapiro.test(sample(residuals_model, min(5000, length(residuals_model))))
cat(sprintf("Shapiro-Wilk     : W = %.5f, p = %.4f\n", sw$statistic, sw$p.value))
ad <- ad.test(residuals_model)
cat(sprintf("Anderson-Darling : A = %.5f, p = %.4f\n", ad$statistic, ad$p.value))
cat("(p > 0.05 → residuals approximately normal)\n")

# 2f. Homoscedasticity
cat("\n--- Breusch-Pagan Test (Homoscedasticity) ---\n")
print(bptest(model))

# ================================================================
# STEP 3: DIAGNOSTIC PLOTS
# ================================================================

pdf("model_diagnostics.pdf", width = 12, height = 10)

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
plot(model, which = 1, main = "Residuals vs Fitted")
plot(model, which = 2, main = "Normal Q-Q")
plot(model, which = 3, main = "Scale-Location")
plot(model, which = 4, main = "Cook's Distance")
plot(model, which = 5, main = "Residuals vs Leverage")

plot(fitted(model), df$Weight,
     main = "Actual vs Predicted Weight",
     xlab = "Predicted (kg)", ylab = "Actual (kg)",
     pch = 16, col = "steelblue", cex = 0.5)
abline(0, 1, col = "red", lwd = 2)
legend("topleft", legend = "y = x (perfect fit)", col = "red", lwd = 2, cex = 0.8)

dev.off()
cat("✔ model_diagnostics.pdf saved\n")

# ================================================================
# STEP 4: COEFFICIENT PLOT
# ================================================================

pdf("coefficient_plot.pdf", width = 8, height = 6)

ci <- as.data.frame(confint(model))
ci$term     <- rownames(ci)
ci$estimate <- coef(model)
colnames(ci)[1:2] <- c("lower", "upper")
ci <- ci[ci$term != "(Intercept)", ]

print(
  ggplot(ci, aes(x = reorder(term, estimate), y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0.2, colour = "steelblue", linewidth = 1) +
    geom_point(size = 3, colour = "tomato") +
    coord_flip() +
    labs(title = "Beta Coefficients with 95% Confidence Intervals",
         x = NULL, y = "Estimate") +
    theme_minimal(base_size = 13)
)

dev.off()
cat("✔ coefficient_plot.pdf saved\n")

# ================================================================
# FINAL: Print the fitted equation
# ================================================================

b <- round(coef(model), 5)
cat("\n================================================================\n")
cat("  FITTED MODEL EQUATION\n")
cat("================================================================\n")
cat(sprintf("Weight = %.5f\n", b["(Intercept)"]))
cat(sprintf("       + %.5f * x_sex\n",                     b["x_sex"]))
cat(sprintf("       + %.5f * x_pt\n",                      b["x_pt"]))
cat(sprintf("       + %.5f * x_visits\n",                  b["x_visits"]))
cat(sprintf("       + %.5f * x_height\n",                  b["x_height"]))
cat(sprintf("       + %.5f * x_caloric_balance_weekly\n",  b["x_caloric_balance_weekly"]))
cat(sprintf("       + %.5f * x_time\n",                    b["x_time"]))
cat("\nAll done! Check your output folder:", getwd(), "\n")