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

pairs(df,
      main  = "Scatterplot Matrix - Group 14 Observational Data",
      pch   = 20, cex = 0.4, col = rgb(0.2, 0.4, 0.8, 0.3),
      upper.panel = function(x, y, ...) {
        r   <- round(cor(x, y, use = "complete.obs"), 2)
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        text(0.5, 0.5, r, cex = 1.2,
             col = ifelse(abs(r) > 0.5, "red", "black"))
      })

par(mfrow = c(2, 4))
for (v in names(df)) {
  hist(df[[v]], main = v, xlab = v, col = "steelblue", border = "white", breaks = 30)
}
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
boxplot(Weight_kg ~ Gender,           data = df, col = c("pink","lightblue"),
        main = "Weight by Gender",    xlab = "Gender (0=F, 1=M)", ylab = "Weight (kg)")
boxplot(Weight_kg ~ Personal_Trainer, data = df, col = c("lightyellow","lightgreen"),
        main = "Weight by Personal Trainer", xlab = "Personal Trainer", ylab = "Weight (kg)")
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
plot(df$Height_cm,       df$Weight_kg, pch = 20, cex = 0.4, col = rgb(0,0,1,0.3),
     main = "Weight ~ Height",        xlab = "Height (cm)",     ylab = "Weight (kg)")
abline(lm(Weight_kg ~ Height_cm, data = df), col = "red", lwd = 2)
plot(df$Caloric_Balance, df$Weight_kg, pch = 20, cex = 0.4, col = rgb(0,0.6,0,0.3),
     main = "Weight ~ Caloric Balance", xlab = "Caloric Balance", ylab = "Weight (kg)")
abline(lm(Weight_kg ~ Caloric_Balance, data = df), col = "red", lwd = 2)
plot(df$Time_min,        df$Weight_kg, pch = 20, cex = 0.4, col = rgb(0.6,0,0.6,0.3),
     main = "Weight ~ Time (min)",    xlab = "Time (minutes)",   ylab = "Weight (kg)")
abline(lm(Weight_kg ~ Time_min, data = df), col = "red", lwd = 2)
plot(df$Visits,          df$Weight_kg, pch = 20, cex = 0.4, col = rgb(1,0.5,0,0.3),
     main = "Weight ~ Visits",        xlab = "Number of Visits", ylab = "Weight (kg)")
abline(lm(Weight_kg ~ Visits, data = df), col = "red", lwd = 2)
par(mfrow = c(1, 1))


# ---- 2. Model 1: All Main Effects --------------------------

model1 <- lm(Weight_kg ~ Gender + Personal_Trainer + Visits +
               Height_cm + Caloric_Balance + Time_min, data = df)

cat("\n===== MODEL 1: All Main Effects =====\n")
summary(model1)
cat("\n--- Beta Estimates & 95% CIs (Model 1) ---\n")
print(round(coef(model1), 4))
print(round(confint(model1), 4))


# ---- 3. Assumption Checks (Model 1) ------------------------

par(mfrow = c(2, 2))
plot(model1, main = "Model 1 Diagnostics")
par(mfrow = c(1, 1))

shapiro_result <- shapiro.test(residuals(model1)[1:5000])
cat("\nShapiro-Wilk (first 5000 residuals):\n")
print(shapiro_result)

predictors <- c("Gender","Personal_Trainer","Visits","Height_cm","Caloric_Balance","Time_min")
par(mfrow = c(2, 3))
for (p in predictors) {
  plot(df[[p]], residuals(model1), pch = 20, cex = 0.4,
       col = rgb(0.2,0.4,0.8,0.3),
       main = paste("Residuals vs", p), xlab = p, ylab = "Residuals")
  abline(h = 0, col = "red", lwd = 2)
}
par(mfrow = c(1, 1))


# ============================================================
# ---- 3b. INTERACTION ANALYSIS (ALL 15 PAIRWISE) ------------
# ============================================================
# With 6 predictors there are C(6,2) = 15 possible pairwise interactions.
# All are tested. Biological/practical rationale for each pair:
#
#  Gender x Personal_Trainer  : trainer effect on weight may differ by sex
#                               (different goals, programming, compliance).
#  Gender x Visits            : frequency may drive hypertrophy (weight gain)
#                               in males but fat loss in females.
#  Gender x Height_cm         : weight-per-cm slope differs by sex due to
#                               muscle mass, bone density, fat distribution.
#  Gender x Caloric_Balance   : hormonal differences alter how surplus/deficit
#                               converts to weight change.
#  Gender x Time_min          : session duration may have sex-specific effects
#                               due to differences in exercise intensity.
#  Personal_Trainer x Visits  : a trainer may amplify or dampen the frequency
#                               effect through improved programming.
#  Personal_Trainer x Height_cm : taller individuals may benefit differently
#                               from a trainer (load prescription, technique).
#  Personal_Trainer x Caloric_Balance : trainers often give nutritional
#                               guidance, modifying the caloric balance effect.
#  Personal_Trainer x Time_min : trainers may raise exercise intensity,
#                               changing how session length affects weight.
#  Visits x Height_cm         : taller people expend more energy per visit;
#                               frequency effect may scale with height.
#  Visits x Caloric_Balance   : visit frequency and caloric balance may
#                               compound non-additively on weight.
#  Visits x Time_min          : total exercise volume (frequency x duration)
#                               may interact non-additively on weight.
#  Height_cm x Caloric_Balance : larger frames have higher basal caloric
#                               needs; surplus/deficit impact may scale with
#                               height.
#  Height_cm x Time_min       : taller individuals expend more energy per
#                               minute; duration effect may be height-dependent.
#  Caloric_Balance x Time_min : longer sessions may offset a surplus or deepen
#                               a deficit, moderating that relationship.
# ============================================================

cat("\n\n============================================================\n")
cat("  SECTION 3b: INTERACTION ANALYSIS (ALL 15 PAIRWISE)\n")
cat("============================================================\n")

# Programmatically generate all C(6,2) = 15 pairs
pred_vars  <- c("Gender", "Personal_Trainer", "Visits",
                "Height_cm", "Caloric_Balance", "Time_min")
int_pairs  <- combn(pred_vars, 2, simplify = FALSE)
int_labels <- sapply(int_pairs, function(p) paste(p[1], p[2], sep = ":"))

cat("\nAll", length(int_labels), "pairwise interactions to be tested:\n")
for (i in seq_along(int_labels)) cat(sprintf("  %2d) %s\n", i, int_labels[i]))
cat("\n")


# -- 3b.1  Exploratory interaction plots (3 per page) ---------

cat("--- Producing exploratory interaction plots ---\n")

plot_interaction <- function(v1, v2, data) {
  is_binary <- function(v) length(unique(data[[v]])) <= 2
  
  if (is_binary(v1)) {
    grp_var <- v1; cnt_var <- v2
  } else if (is_binary(v2)) {
    grp_var <- v2; cnt_var <- v1
  } else {
    cnt_var <- v1
    med_val <- median(data[[v2]], na.rm = TRUE)
    data$grp_tmp <- ifelse(data[[v2]] >= med_val,
                           paste0(v2, " >= med"),
                           paste0(v2, " < med"))
    grp_var <- "grp_tmp"
  }
  
  grp_vals <- sort(unique(data[[grp_var]]))
  pal      <- c("blue", "red")
  
  plot(data[[cnt_var]], data$Weight_kg,
       col  = ifelse(data[[grp_var]] == grp_vals[1],
                     adjustcolor(pal[1], 0.3), adjustcolor(pal[2], 0.3)),
       pch  = 20, cex = 0.5,
       main = paste0("Wt ~ ", cnt_var, "\nby ", grp_var),
       xlab = cnt_var, ylab = "Weight (kg)", cex.main = 0.8)
  
  for (i in seq_along(grp_vals)) {
    sub <- data[data[[grp_var]] == grp_vals[i], ]
    if (nrow(sub) > 2)
      abline(lm(Weight_kg ~ sub[[cnt_var]], data = sub),
             col = pal[i], lwd = 2)
  }
  lbl <- if (grp_var == "grp_tmp") as.character(grp_vals)
  else paste(grp_var, "=", grp_vals)
  legend("topleft", legend = lbl, col = pal[seq_along(grp_vals)],
         lwd = 2, bty = "n", cex = 0.7)
}

n_per_page <- 3
n_pages    <- ceiling(length(int_pairs) / n_per_page)
for (pg in seq_len(n_pages)) {
  i_start <- (pg - 1) * n_per_page + 1
  i_end   <- min(pg * n_per_page, length(int_pairs))
  par(mfrow = c(1, i_end - i_start + 1))
  for (i in i_start:i_end)
    plot_interaction(int_pairs[[i]][1], int_pairs[[i]][2], df)
  par(mfrow = c(1, 1))
}


# -- 3b.2  Individual interaction tests (each vs Model 1) -----

cat("\n--- Individual interaction tests (each added to Model 1) ---\n\n")

base_formula <- "Weight_kg ~ Gender + Personal_Trainer + Visits +
                  Height_cm + Caloric_Balance + Time_min"

interaction_summary <- data.frame(
  Interaction = character(), Beta_Int = numeric(), SE_Int   = numeric(),
  t_value     = numeric(),   p_value  = numeric(), R2       = numeric(),
  Adj_R2      = numeric(),   AIC      = numeric(), ANOVA_F  = numeric(),
  ANOVA_p     = numeric(),   stringsAsFactors = FALSE
)

for (int_label in int_labels) {
  fml      <- as.formula(paste(base_formula, "+", int_label))
  m_int    <- lm(fml, data = df)
  s        <- summary(m_int)
  coefs    <- s$coefficients
  int_name <- rownames(coefs)[grepl(":", rownames(coefs))]
  int_row  <- coefs[int_name, ]
  av       <- anova(model1, m_int)
  
  interaction_summary <- rbind(interaction_summary, data.frame(
    Interaction = int_label,
    Beta_Int    = round(int_row["Estimate"],   4),
    SE_Int      = round(int_row["Std. Error"], 4),
    t_value     = round(int_row["t value"],    4),
    p_value     = round(int_row["Pr(>|t|)"],   4),
    R2          = round(s$r.squared,           4),
    Adj_R2      = round(s$adj.r.squared,       4),
    AIC         = round(AIC(m_int),            2),
    ANOVA_F     = round(av$F[2],               4),
    ANOVA_p     = round(av$`Pr(>F)`[2],        4)
  ))
  
  sig_flag <- ifelse(int_row["Pr(>|t|)"] < 0.05, "  ***SIGNIFICANT***", "")
  cat("==>", int_label, sig_flag, "\n")
  cat("    Beta =", round(int_row["Estimate"],  4),
      "| SE =",   round(int_row["Std. Error"], 4),
      "| t =",    round(int_row["t value"],    4),
      "| p =",    round(int_row["Pr(>|t|)"],   6),
      "| ANOVA p =", round(av$`Pr(>F)`[2],     6), "\n\n")
}

cat("\n--- Full Interaction Summary Table (sorted by p-value) ---\n")
print(interaction_summary[order(interaction_summary$p_value), ], row.names = FALSE)


# -- 3b.3  Model with all 15 interactions --------------------

all_int_str   <- paste(int_labels, collapse = " + ")
full_int_fml  <- as.formula(paste(base_formula, "+", all_int_str))
model_all_int <- lm(full_int_fml, data = df)

cat("\n===== MODEL: All 15 Pairwise Interactions =====\n")
summary(model_all_int)

cat("\n--- ANOVA: Model 1 vs Model with All 15 Interactions ---\n")
print(anova(model1, model_all_int))


# -- 3b.4  Identify significant interaction terms -------------

get_int_pvals <- function(model) {
  coefs    <- summary(model)$coefficients
  int_rows <- grepl(":", rownames(coefs))
  coefs[int_rows, "Pr(>|t|)", drop = FALSE]
}

cat("\n--- P-values for all 15 interaction terms (sorted ascending) ---\n")
int_pval_table <- get_int_pvals(model_all_int)
int_pval_table <- int_pval_table[order(int_pval_table[, 1]), , drop = FALSE]
print(round(int_pval_table, 4))

sig_ints <- rownames(int_pval_table)[int_pval_table[, 1] < 0.05]

cat("\nSignificant interaction terms (p < 0.05):\n")
if (length(sig_ints) == 0) {
  cat("  None - no interactions reach p < 0.05.\n")
  cat("  Conclusion: main-effects model is preferred; none of the 15\n")
  cat("  pairwise interactions meaningfully improves prediction of Weight_kg.\n")
  model_best_int <- model1
} else {
  cat(paste(" ", sig_ints, collapse = "\n"), "\n")
}


# -- 3b.5  Best interaction model (significant terms only) ----

if (length(sig_ints) > 0) {
  int_formula_str <- paste(base_formula, "+", paste(sig_ints, collapse = " + "))
  model_best_int  <- lm(as.formula(int_formula_str), data = df)
  
  cat("\n===== MODEL BEST INTERACTIONS (significant only) =====\n")
  cat("Formula:", int_formula_str, "\n\n")
  summary(model_best_int)
  
  cat("\n--- Beta Estimates & 95% CIs (Best Interaction Model) ---\n")
  print(round(coef(model_best_int), 4))
  print(round(confint(model_best_int), 4))
  
  cat("\n--- ANOVA: Model 1 vs Best Interaction Model ---\n")
  print(anova(model1, model_best_int))
  
  par(mfrow = c(2, 2))
  plot(model_best_int, main = "Best Interaction Model Diagnostics")
  par(mfrow = c(1, 1))
  
  cat("\n--- Interaction plots for significant terms ---\n")
  par(mfrow = c(1, min(length(sig_ints), 3)))
  for (int_term in sig_ints) {
    vars <- strsplit(int_term, ":")[[1]]
    plot_interaction(vars[1], vars[2], df)
  }
  par(mfrow = c(1, 1))
}

cat("\n============================================================\n")
cat("  END OF INTERACTION ANALYSIS\n")
cat("============================================================\n\n")


# -----------------------------------------------------------
# INTERPRETATION NOTE (from results):
#   - Personal_Trainer (p=0.56) and Time_min (p=0.45) are NOT significant
#   - Height_cm, Gender, Caloric_Balance, Visits are all significant
#   - R^2 = 0.7747: model explains ~77% of variance in Weight
#   - Interactions tested in Model 2 were also non-significant (ANOVA p=0.23)
#   - Quadratic Height in Model 3 was non-significant (p=0.87)
#   -> Model 1 is therefore the best / most parsimonious model
# -----------------------------------------------------------


# ---- 4. Reduced Model: Remove Non-Significant Predictors ---

model_final <- lm(Weight_kg ~ Gender + Visits + Height_cm + Caloric_Balance,
                  data = df)

cat("\n===== MODEL FINAL: Reduced (Significant Predictors Only) =====\n")
summary(model_final)

cat("\n--- Beta Estimates & 95% CIs (Final Model) ---\n")
print(round(coef(model_final), 4))
print(round(confint(model_final), 4))

cat("\n--- ANOVA: Model 1 (full) vs Model Final (reduced) ---\n")
print(anova(model_final, model1))

par(mfrow = c(2, 2))
plot(model_final, main = "Final Model Diagnostics")
par(mfrow = c(1, 1))


# ---- 5. Model Fit Summary Table ----------------------------

cat("\n\n========================================\n")
cat("  MODEL FIT COMPARISON\n")
cat("========================================\n\n")

models_list <- list(
  "Model 1 (All main effects)" = model1,
  "Model Final (Reduced)"      = model_final
)
if (exists("model_best_int") && !identical(model_best_int, model1)) {
  models_list[["Best Interaction Model"]] <- model_best_int
}

fit_stats <- data.frame(
  Model  = names(models_list),
  R2     = sapply(models_list, function(m) round(summary(m)$r.squared,     4)),
  Adj_R2 = sapply(models_list, function(m) round(summary(m)$adj.r.squared, 4)),
  AIC    = sapply(models_list, function(m) round(AIC(m),                   4)),
  BIC    = sapply(models_list, function(m) round(BIC(m),                   4)),
  row.names = NULL
)
print(fit_stats, row.names = FALSE)


# ---- 6. Beta Summary Table ---------------------------------

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