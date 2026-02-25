library(dplyr)

# Set seed for reproducibility
#set.seed(123)

# Sample size
n <- 2000

### 1. Continuous variable: Age
age <- rnorm(n, mean = 40, sd = 10)
age <- pmax(age, 18)   # enforce minimum adult age#havw this

### 2. Continuous variable correlated with age: Experience
# Noisy linear relationship with age
experience <- 0.5*(age - 22) + rnorm(n, mean = 0, sd = 8)
experience <- pmax(experience, 0) #have this

### 3. Categorical variable: Education level
education_levels <- c("School", "College", "Postgrad")
education <- sample(education_levels, n, replace = TRUE,
                    prob = c(0.4, 0.4, 0.2))
education <- factor(education)# fine if samples are not even

### 4. Binary treatment variable: Training participation
# Slightly more likely for younger and college educated
training_prob <- plogis(-0.5 + 0.02*(35 - age) + 0.5*(education == "College"))
training <- rbinom(n, 1, training_prob)

### 5. Binary explanatory variable: Urban residence
urban <- rbinom(n, 1, 0.6)

# Combine into data frame
X <- data.frame(age, experience, education, training, urban)

head(X)


par(mfrow = c(2,3))

hist(age, main = "Age", col = "lightblue")
hist(experience, main = "Experience", col = "lightgreen")
hist(training, main = "Training", col = "salmon")
hist(urban, main = "Urban", col = "orange")

# For categorical variable
barplot(table(education), main = "Education Level",
        col = "purple")

# Create numeric version for plotting
X_numeric <- data.frame(
  age = age,
  experience = experience,
  education = as.numeric(education),
  training = training,
  urban = urban
)

X_numeric

pairs(X_numeric,
      main = "Scatterplot Matrix of Covariates",
      col = "darkblue",
      pch = 16)

# Creating the response variable
### Create dummy variables for education
edu_college  <- ifelse(education == "College", 1, 0)
edu_postgrad <- ifelse(education == "Postgrad", 1, 0)

### Set error variance (controls R^2)
sigma <- 15000

epsilon <- rnorm(n, mean = 0, sd = sigma)

### Generate income
income <- 20000 +
  900 * experience +
  100 * age +
  4000 * training +
  3000 * edu_college +
  7000 * edu_postgrad +
  3500 * urban +
  epsilon

#This is where you will possibly put in an interaction or transformation, or both
#instead of maybe one or two of the main effects. #we have this

### Add to dataset
X$income <- income

hist(income,
     breaks = 30,
     col = "lightgray",
     main = "Histogram of Annual Income",
     xlab = "Income")

model1 <- lm(income ~ age + experience + education + training + urban, data = X)
summary(model1)

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

#Adjusting based on Experience
beta_exp <- 900 * exp_means$exp_mean
beta_exp
beta_exp_dev <- beta_exp - beta_exp[1]
beta_exp_dev
intercept_new <- 20000 + beta_exp[1]
intercept_new
#beta0  <- intercept_new
betaE2 <- beta_exp_dev[2]
betaE3 <- beta_exp_dev[3]
# Create experience dummies
exp2 <- ifelse(final_data$exp_cat == "MidExp", 1, 0)
exp3 <- ifelse(final_data$exp_cat == "HighExp", 1, 0)

#Adjusting based on Age
beta_age <- 100 * age_means$age_mean
beta_age
beta_age_dev <- beta_age - beta_age[1]
beta_age_dev
intercept_new <- intercept_new + beta_age[1]
intercept_new
beta0  <- intercept_new
betaA2 <- beta_age_dev[2]
betaA3 <- beta_age_dev[3]
# Create experience dummies
age2 <- ifelse(final_data$age_cat == "Middle", 1, 0)
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
