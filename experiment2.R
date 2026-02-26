library(tidyverse)
library(devtools)
set.seed(123)


# Load the function
source("simulate_maize_data.R")

# Generate 2000 rows of data
observed_maize_data <- generate_maize_data(20000)

# Check the results
print(head(observed_maize_data))
nrow(observed_maize_data)