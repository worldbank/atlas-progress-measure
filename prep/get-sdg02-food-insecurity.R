# Get food insecurity data from Microdata API
# Write sdg2-food-insecurity.csv csv file to input folder (but this file is nowhere used)

# Load necessary packages
library(httr2)
library(jsonlite)
library(dplyr) 

# --- Configuration ---
BASE_URL <- "https://microdata.worldbank.org/index.php/api/tables/data/fcv/wfso"
PAGE_LIMIT <- 1000 # Maximum number of rows to retrieve per request

# Define the Initial Request Object
base_req <- request(BASE_URL) %>%
  req_url_query(limit = PAGE_LIMIT, offset = 0)

# Perform the Iterative Request (WITHOUT the 'total' argument)
# The iteration will now run until the API returns an empty response.

all_responses <- req_perform_iterative(
  req = base_req,
  
  # Use the correct way to specify the iterator for offset/limit pagination:
  next_req = iterate_with_offset(
    param_name = "offset", # Name of the query parameter to increment
    offset = PAGE_LIMIT    # Amount to increment by for each page
  ),
  
  max_reqs = 59,          # Continue until the API returns no more data
  
)

# Process and Combine All Data

# Function to extract the data frame from each response's JSON body
extract_data <- function(resp) {
  # Parse the JSON body
  data_list <- resp_body_json(resp, simplifyVector = TRUE)
  
  # Return the data, assuming it's the top-level object
  return(data_list)
}

# Apply the function to the list of responses and combine them into one data frame
final_data <- all_responses %>%
  lapply(extract_data) %>%
  bind_rows()

data_wdi <- final_data$data %>%
  filter(indicator_short == "Prevalence of Severe Food Insecurity (%)",
         year < 2025) %>%
  select(iso3c, year, value) %>%
  rename(SN.ITK.SVFI.ZS = value)

# Identify Duplicates (Marking all rows that are part of a duplicate group)
df_with_dupes <- data_wdi %>%
  group_by(!!sym("iso3c"), !!sym("year")) %>% # Group by the combination of the two columns
  mutate(n = n()) %>%                     # Count the number of rows in each group
  ungroup() %>%
  mutate(is_duplicate = n > 1)

write.csv(data_wdi, "input/sdg2-food-insecurity.csv")
