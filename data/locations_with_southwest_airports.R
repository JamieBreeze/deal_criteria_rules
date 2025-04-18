# Install readr if you haven't already (only need to do this once)
# install.packages("readr")

# Load the readr package
library(readr)

# Define the file path
locs_sw_file_path <- "/Users/jamesscerbo/Documents/GitHub/brz_southwest_airports/data/locations_with_southwest_airports.csv"

# Read the CSV file using read_csv()
locs_sw_file <- readr::read_csv(locs_sw_file_path)

# Optional: View the first few rows
head(locs_sw_file)

# Optional: View the structure
str(locs_sw_file)
