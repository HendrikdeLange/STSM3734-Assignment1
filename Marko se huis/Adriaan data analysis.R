df <- read.csv("~/STSM3734-Assignment1/obs_data/Observational_Group14.csv", sep=";", dec=".")

colnames(df)=c("Gender", "Personal Trainer", "Amount of visits", "Caloric Balance", "Height(cm)", "Time(minutes)", "Weight(kg)" )


# ============================================================
# PART III: Analysis of Group 14 Observational Data
# Response variable: Weight (kg)
# ============================================================

# ---- 0. Load & Prepare Data --------------------------------
df <- read.csv("~/STSM3734-Assignment1/obs_data/Observational_Group14.csv",
               sep = ";", dec = ".")
colnames(df) <- c("Gender", "Personal_Trainer", "Visits",
                  "Height_cm", "Caloric_Balance", "Time_min", "Weight_kg")

str(df)
summary(df)
colSums(is.na(df))


# ---- 1. Exploratory Visualisation --------------------------

# Scatterplot matrix with correlations
pairs(df,
      main  = "Scatterplot Matrix – Group 14 Observational Data",
      pch   = 20, cex = 0.4, col = rgb(0.2, 0.4, 0.8, 0.3),
      upper.panel = function(x, y, ...) {
        r   <- round(cor(x, y, use = "complete.obs"), 2)
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        text(0.5, 0.5, r, cex = 1.2,
             col = ifelse(abs(r) > 0.5, "red", "black"))
      })

# Histograms of all variables
par(mfrow = c(2, 4))
for (v in names(df)) {
  hist(df[[v]], main = v, xlab = v, col = "steelblue",
       border = "white", breaks = 30)
}
par(mfrow = c(1, 1))

# Boxplots for binary predictors vs Weight
par(mfrow = c(1, 2))
boxplot(Weight_kg ~ Gender,           data = df, col = c("pink","lightblue"),
        main = "Weight by Gender",    xlab = "Gender (0=F, 1=M)", ylab = "Weight (kg)")
boxplot(Weight_kg ~ Personal_Trainer, data = df, col = c("lightyellow","lightgreen"),
        main = "Weight by Personal Trainer", xlab = "Personal Trainer", ylab = "Weight (kg)")
par(mfrow = c(1, 1))

# Scatterplots of continuous predictors vs Weight
par(mfrow = c(2, 2))
plot(df$Height_cm,       df$Weight_kg, pch = 20, cex = 0.4,
     col = rgb(0,0,1,0.3),     main = "Weight ~ Height",
     xlab = "Height (cm)",     ylab = "Weight (kg)")
abline(lm(Weight_kg ~ Height_cm, data = df), col = "red", lwd = 2)

plot(df$Caloric_Balance, df$Weight_kg, pch = 20, cex = 0.4,
     col = rgb(0,0.6,0,0.3),   main = "Weight ~ Caloric Balance",
     xlab = "Caloric Balance",  ylab = "Weight (kg)")
abline(lm(Weight_kg ~ Caloric_Balance, data = df), col = "red", lwd = 2)

plot(df$Time_min,        df$Weight_kg, pch = 20, cex = 0.4,
     col = rgb(0.6,0,0.6,0.3), main = "Weight ~ Time (min)",
     xlab = "Time (minutes)",   ylab = "Weight (kg)")
abline(lm(Weight_kg ~ Time_min, data = df), col = "red", lwd = 2)

plot(df$Visits,          df$Weight_kg, pch = 20, cex = 0.4,
     col = rgb(1,0.5,0,0.3),   main = "Weight ~ Visits",
     xlab = "Number of Visits", ylab = "Weight (kg)")
abline(lm(Weight_kg ~ Visits, data = df), col = "red", lwd = 2)
par(mfrow = c(1, 1))


# ---- 2. Model 1: All Main Effects --------------------------

model1 <- lm(Weight_kg ~ Gender + Personal_Trainer + Visits +
               Height_cm + Caloric_Balance + Time_min,
             data = df)

cat("\n===== MODEL 1: All Main Effects =====\n")
summary(model1)

cat("\n--- Beta Estimates & 95% CIs (Model 1) ---\n")
print(round(coef(model1), 4))
print(round(confint(model1), 4))


# ---- 3. Assumption Checks (Model 1) ------------------------

par(mfrow = c(2, 2))
plot(model1, main = "Model 1 Diagnostics")
par(mfrow = c(1, 1))

# Normality test on residuals
shapiro_result <- shapiro.test(residuals(model1)[1:5000])
cat("\nShapiro-Wilk (first 5000 residuals):\n")
print(shapiro_result)

# Residuals vs each predictor
predictors <- c("Gender","Personal_Trainer","Visits","Height_cm","Caloric_Balance","Time_min")

par(mfrow = c(2, 3))
for (p in predictors) {
  plot(df[[p]], residuals(model1), pch = 20, cex = 0.4,
       col = rgb(0.2,0.4,0.8,0.3),
       main = paste("Residuals vs", p),
       xlab = p, ylab = "Residuals")
  abline(h = 0, col = "red", lwd = 2)
}
par(mfrow = c(1, 1))

# -----------------------------------------------------------
# INTERPRETATION NOTE (from results):
#   - Personal_Trainer (p=0.56) and Time_min (p=0.45) are NOT significant
#   - Height_cm, Gender, Caloric_Balance, Visits are all significant
#   - R² = 0.7747: model explains ~77% of variance in Weight
#   - Interactions tested in Model 2 were also non-significant (ANOVA p=0.23)
#   - Quadratic Height in Model 3 was non-significant (p=0.87)
#   -> Model 1 is therefore the best / most parsimonious model
# -----------------------------------------------------------


# ---- 4. Reduced Model: Remove Non-Significant Predictors ---
# Drop Personal_Trainer and Time_min (both p > 0.4 in Model 1)

model_final <- lm(Weight_kg ~ Gender + Visits + Height_cm + Caloric_Balance,
                  data = df)

cat("\n===== MODEL FINAL: Reduced (Significant Predictors Only) =====\n")
summary(model_final)

cat("\n--- Beta Estimates & 95% CIs (Final Model) ---\n")
print(round(coef(model_final), 4))
print(round(confint(model_final), 4))

# Compare Model 1 vs Final (nested test)
cat("\n--- ANOVA: Model 1 (full) vs Model Final (reduced) ---\n")
print(anova(model_final, model1))
# If p > 0.05, the removed predictors add nothing -> reduced model preferred

# Diagnostic plots for Final Model
par(mfrow = c(2, 2))
plot(model_final, main = "Final Model Diagnostics")
par(mfrow = c(1, 1))


# ---- 5. Model Fit Summary Table ----------------------------

cat("\n\n========================================\n")
cat("  MODEL FIT COMPARISON\n")
cat("========================================\n\n")

fit_stats <- data.frame(
  Model  = c("Model 1 (All main effects)", "Model Final (Reduced)"),
  R2     = c(summary(model1)$r.squared,     summary(model_final)$r.squared),
  Adj_R2 = c(summary(model1)$adj.r.squared, summary(model_final)$adj.r.squared),
  AIC    = c(AIC(model1),                   AIC(model_final)),
  BIC    = c(BIC(model1),                   BIC(model_final))
)

# FIX: only round numeric columns, not the Model label column
fit_stats[, 2:5] <- round(fit_stats[, 2:5], 4)
print(fit_stats, row.names = FALSE)


# ---- 6. Beta Summary Table (Both Models) -------------------

summarise_betas <- function(model, label) {
  s       <- summary(model)
  df_coef <- as.data.frame(s$coefficients)
  df_coef$Model    <- label
  df_coef$Variable <- rownames(df_coef)
  rownames(df_coef) <- NULL
  df_coef[, c("Model","Variable","Estimate","Std. Error","t value","Pr(>|t|)")]
}

beta_table <- rbind(
  summarise_betas(model1,     "Model 1 (Full)"),
  summarise_betas(model_final,"Model Final (Reduced)")
)

# FIX: only round numeric columns 3:6, not the character columns 1:2
beta_table[, 3:6] <- round(beta_table[, 3:6], 4)
print(beta_table, row.names = FALSE)


# ---- 7. Final Visualisation --------------------------------

par(mfrow = c(1, 2))
plot(fitted(model_final), df$Weight_kg,
     pch = 20, cex = 0.4, col = rgb(0.2,0.4,0.8,0.3),
     main = "Fitted vs Actual Weight (Final Model)",
     xlab = "Fitted Values", ylab = "Actual Weight (kg)")
abline(0, 1, col = "red", lwd = 2)

plot(fitted(model_final), residuals(model_final),
     pch = 20, cex = 0.4, col = rgb(0.2,0.4,0.8,0.3),
     main = "Residuals vs Fitted (Final Model)",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lwd = 2)
par(mfrow = c(1, 1))

cat("\n===== ANALYSIS COMPLETE =====\n")
cat("Best model: Weight_kg ~ Gender + Visits + Height_cm + Caloric_Balance\n")