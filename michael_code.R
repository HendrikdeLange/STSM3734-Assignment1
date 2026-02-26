
library(dplyr)
#Experimentalising the data
set.seed(123)


n_total <- 2160

# Continuous Age
age_cont <- rnorm(n_total, mean = 40, sd = 10)
age_cont <- pmax(age_cont, 18)

# Continuous Experience (noisy linear function)
experience_cont <- 0.5*(age_cont - 22) + rnorm(n_total, mean = 0, sd = 8)
experience_cont <- pmax(experience_cont, 0)

temp_df <- data.frame(age_cont, experience_cont)



# Add categories to data frame
temp_df <- data.frame(age_cont, experience_cont) %>%
  mutate(age_cat = ntile(age_cont, 3)) %>%
  group_by(age_cat) %>%
  mutate(exp_cat = ntile(experience_cont, 3)) %>%
  ungroup()

temp_df



# --- Save tertile means ---

age_means <- temp_df %>%
  group_by(age_cat) %>%
  summarise(age_mean = mean(age_cont))

exp_means <- temp_df %>%
  group_by(exp_cat) %>%
  summarise(exp_mean = mean(experience_cont))

age_means
exp_means

# he saves the mean of the numeric vars
# 
# 
# 
# 
# Create all POSSIBLEcombinations for remaining factors
school_levels <- c("School", "College", "Postgrad")

design_block <- expand.grid(
  schooling = school_levels,
  training = c(0,1),
  urban = c(0,1)
)
#tertiles numeric and ;eaves rest as factors

design_block

# # Repeat each row 20 times
design_block <- design_block[rep(1:nrow(design_block), each = 20), ]

#Assign structure in each block
final_data <- temp_df %>%
  group_by(age_cat, exp_cat) %>%
  group_modify(~{
    cbind(.x, design_block)
  }) %>%
  ungroup()

final_data
#Label categories
final_data$age_cat <- factor(final_data$age_cat,
                             labels = c("Young","Middle","Older"))

final_data$exp_cat <- factor(final_data$exp_cat,
                             labels = c("LowExp","MidExp","HighExp"))

final_data$schooling <- factor(final_data$schooling,
                               levels = school_levels)

final_data$training <- factor(final_data$training)
final_data$urban <- factor(final_data$urban)

#Verify balance

table(final_data$age_cat,
      final_data$exp_cat,
      final_data$schooling,
      final_data$training,
      final_data$urban)
# 
#Create the new response



#OLD MODEL
### Set error variance (controls R^2)
# sigma <- 15000
# 
# epsilon <- rnorm(n, mean = 0, sd = sigma)
# 
# ### Generate income
# income <- 20000 +
#   900 * experience +
#   100 * age +
#   4000 * training +
#   3000 * edu_college +
#   7000 * edu_postgrad +
#   3500 * urban +
#   epsilon
#Adjusting based on Experience
beta_exp <- 900 * exp_means$exp_mean #WAS 900
beta_exp # 1X3 VECTOR
beta_exp_dev <- beta_exp - beta_exp[1]
beta_exp_dev #SUBTRACTS THE FIRST TERILE MEAN FROM EACH ENTRY IN THE VECTOR
intercept_new <- 20000 + beta_exp[1] #NEW INTERCEPT = OLD + LOWEST TERTILE MEAN
intercept_new

#beta0  <- intercept_new
betaE2 <- beta_exp_dev[2] #2ND TERTILE MEAN
betaE3 <- beta_exp_dev[3] #3RD TERTILE MEAN
# Create experience dummies
exp2 <- ifelse(final_data$exp_cat == "MidExp", 1, 0) #CREATE DUMMIES FOR EXPERIENCE
exp3 <- ifelse(final_data$exp_cat == "HighExp", 1, 0)

#Adjusting based on Age
beta_age <- 100 * age_means$age_mean
beta_age #3X1 VECTOR
beta_age_dev <- beta_age - beta_age[1]
beta_age_dev #SUBTRACTS THE 1ST TERTILE MEAN FROM EACH ENTRY IN THE VECTOR
intercept_new <- intercept_new + beta_age[1] #NEW INTERCEPT = OLD + LOWEST TERTILE MEAN
intercept_new
beta0  <- intercept_new
betaA2 <- beta_age_dev[2]
betaA3 <- beta_age_dev[3]
# Create experience dummies
age2 <- ifelse(final_data$age_cat == "Middle", 1, 0) #CREATE EXPERIENCE DUMMIES
age3 <- ifelse(final_data$age_cat == "Older", 1, 0)

# Schooling dummies
college  <- ifelse(final_data$schooling == "College", 1, 0)
postgrad <- ifelse(final_data$schooling == "Postgrad", 1, 0)

# Use similar noise to observational case
sigma <- 15000
epsilon <- rnorm(nrow(final_data), 0, sigma)


# Generate income
income_exp <- beta0 +
  betaE2 * exp2 +
  betaE3 * exp3 +
  betaA2 * age2 +
  betaA3 * age3 +
  4000 * as.numeric(as.character(final_data$training)) +
  3000 * college +
  7000 * postgrad +
  3500 * as.numeric(as.character(final_data$urban)) +
  epsilon

final_data$income <- income_exp

# #This is for plotting
# # Observational income
# income_obs <- income
# 
# # Experimental income
# income_exp <- final_data$income
# 
# 
# mean(income_obs)-mean(income_exp)
# 
# 
# # Set common breaks for fair comparison
# breaks_seq <- pretty(range(c(income_obs, income_exp)), n = 30)
# 
# # Plot observational histogram
# hist(income_obs,
#      breaks = breaks_seq,
#      probability = TRUE,
#      col = rgb(0, 0, 1, 0.4),
#      border = NA,
#      main = "Superimposed Histograms",
#      xlab = "Income",
#      ylim = c(0, max(density(income_obs)$y,
#                      density(income_exp)$y)))
# 
# # Add experimental histogram
# hist(income_exp,
#      breaks = breaks_seq,
#      probability = TRUE,
#      col = rgb(1, 0, 0, 0.4),
#      border = NA,
#      add = TRUE)
# 
# legend("topright",
#        legend = c("Observational", "Experimental"),
#        fill = c(rgb(0,0,1,0.4), rgb(1,0,0,0.4)))
# 
# 
# #QQ plot
# qqplot(income_obs, income_exp,
#        main = "QQ Plot: Experimental vs Observational",
#        xlab = "Observational Quantiles",
#        ylab = "Experimental Quantiles",
#        pch = 16, col = "darkgray")
# 
# abline(0, 1, col = "red", lwd = 2)
# 
# #Estimation of the final model
# 
# #Some hectically inefficient code:
# final_data$age_cat   <- factor(final_data$age_cat)
# final_data$exp_cat   <- factor(final_data$exp_cat)
# final_data$schooling <- factor(final_data$schooling)
# final_data$training  <- factor(final_data$training)
# final_data$urban     <- factor(final_data$urban)
# final_data$age_cat   <- relevel(final_data$age_cat, ref = "Young")
# final_data$exp_cat   <- relevel(final_data$exp_cat, ref = "LowExp")
# final_data$schooling <- relevel(final_data$schooling, ref = "School")
# final_data$training  <- relevel(final_data$training, ref = "0")
# final_data$urban     <- relevel(final_data$urban, ref = "0")
# 
# model_exp <- lm(income ~ exp_cat +
#                   age_cat +
#                   schooling +
#                   training +
#                   urban,
#                 data = final_data)
# 
# summary(model_exp)
# 
