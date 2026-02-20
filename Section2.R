data <- read.csv("C:\\Users\\hendr\\OneDrive\\Documents\\STSM3734-MAIZE_SIMULATION\\observed_data.csv")
install.packages("fastDummies")
library(fastDummies)


#CREATE THE BLOCK -> seed_brand and rehion
# Create block ID from original columns
data$block <- interaction(data$seed_brand, data$region, sep = "_")

# Check how many blocks you have and their sizes
table(data$block)


#Convert continuous to categorical vars
data$rainfall_cat <- cut(
  data$rainfall_mm,
  breaks = quantile(data$rainfall_mm, probs = c(0, 0.25, 0.5,0.75, 1)),
  labels = c("Q1","Q2", "Q3", "Q4"),
  include.lowest = TRUE
)

data$fertiliser_cat <- cut(
  data$fertiliser_kgha,
  breaks = quantile(data$fertiliser_kgha, probs = c(0, 0.33,0.66, 1)),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)

data$temperature_cat <- cut(
  data$temperature_C,
  breaks = quantile(data$temperature_C, probs = c(0, 0.33,0.66, 1)),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)

data$pesticide_cat <- cut(
  data$pesticide_kgha,
  breaks = quantile(data$pesticide_kgha, probs = c(0, 0.33,0.66, 1)),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)

data$soil_organic_matter_cat <- cut(
  data$soil_organic_matter,
  breaks = quantile(data$soil_organic_matter, probs = c(0, 0.33,0.66, 1)),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)

data <- data[, c("temperature_cat",
                 "rainfall_cat",
                 "irrigation",
                 "fertiliser_cat",
                 "pesticide_cat",
                 "soil_organic_matter_cat",
                 "seed_brand",
                 "block"
                )]



data_dummies <- dummy_cols(
  data,
  select_columns = c("temperature_cat", "rainfall_cat", "irrigation",
                     "fertiliser_cat", "pesticide_cat", "soil_organic_matter_cat",
                     "seed_brand"),
  remove_first_dummy = TRUE,
  remove_selected_columns = TRUE
)


#visualizing the blocks
block_counts <- as.data.frame(table(data$block))
colnames(block_counts) <- c("Block", "Count")

library(ggplot2)

ggplot(block_counts, aes(x = Block, y = Count, fill = Block)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Observations per Block",
       x = "Block (seed_brand _ region)",
       y = "Count") +
  theme(legend.position = "none")

block_counts$seed_brand <- sub("_.*", "", block_counts$Block)  # extract part before _
block_counts$region     <- sub(".*_", "", block_counts$Block)  # extract part after _

ggplot(block_counts, aes(x = region, y = seed_brand, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), color = "white", size = 5) +
  scale_fill_gradient(low = "steelblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Block Sizes: Seed Brand × Region",
       x = "Region", y = "Seed Brand")

#GOING TO SAVE THIS TO CSV (datadummies)
data_dummies$yield_tha <- data$yield_tha
write.csv(data_dummies, "dummy_data.csv", row.names = FALSE)