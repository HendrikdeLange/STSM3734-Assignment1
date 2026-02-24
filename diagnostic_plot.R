data <- read.csv("C:\\Users\\hendr\\OneDrive\\Documents\\STSM3734-MAIZE_SIMULATION\\observed_data.csv")
colnames(data)
cols_to_diagnose <- c("region", "temperature_C", "rainfall_mm", 
                      "irrigation", "fertiliser_kgha", "pesticide_kgha",
                      "soil_organic_matter", "seed_brand","tractor_brand","yield_tha"  