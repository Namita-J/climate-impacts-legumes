# Carr et al search terms testing
# 0) Set up workspace ####
# 0.1) Load packages #####
# Use p_load to install if not present and load the packages
pacman::p_load(data.table,openalexR)

# 0.2) Create functions #####
add_quotes <- function(vector) {
  sapply(vector, function(term) {
    if (grepl("\\s", term)) {
      return(shQuote(term, type = "cmd"))
    } else {
      return(term)
    }
  }, USE.NAMES = FALSE)
}
# Specify your main project directory on your local drive
project <- "C:/Users/JNamita/OneDrive - CGIAR/PhD- Namita/1. Synthesis_Climate change impacts on legumes/OpenAlex"

# Define the path again in case it reset
search_data_dir <- "C:/Users/JNamita/OneDrive - CGIAR/PhD- Namita/1. Synthesis_Climate change impacts on legumes/OpenAlex/search_data"

# Create the directory if it does not exist
if (!dir.exists(search_data_dir)) {
  dir.create(search_data_dir, recursive = TRUE)
}

# Check if the directory now exists
print(dir.exists(search_data_dir))  # Should print TRUE if created successfully

# 1) Create terms ####
# Read in additional animal breed terms provided by Claudia Arndt's team
climate_terms <- c("climate change", "global warming", "drought", "rainfall variability", "heat stress", 
                   "CO2 emission", "greenhouse gas", "temperature increase", "extreme event")
agriculture_terms <- c("agriculture production", "crop yield", "food security", "crop performance", 
                       "plant biomass", "harvest")
region_terms <- c("Angola", "Benin", "Botswana", "Burkina Faso", "Cameroon", "Ethiopia", "Ghana", 
                  "Kenya", "Malawi", "Nigeria", "South Africa", "Tanzania", "Uganda", "Zimbabwe")


# Convert to Boolean expressions
climate_boolean <- paste0("(", paste0(climate_terms, collapse = " OR "), ")")
agriculture_boolean <- paste0("(", paste0(agriculture_terms, collapse = " OR "), ")")
region_boolean <- paste0("(", paste0(region_terms, collapse = " OR "), ")")

# Final combined search string
final_search_string <- paste(
  climate_boolean, 
  "AND", 
  agriculture_boolean, 
  "AND", 
  region_boolean
)

# Save terms for reference
terms <- list(
  climate = climate_boolean,
  agriculture = agriculture_boolean,
  region = region_boolean,
  full_search = final_search_string
)

save(terms, file = file.path(search_data_dir, "climate_agriculture_region_search_terms.RData"))

# Set date range for the search
from_year <- "1970-01-01"
to_year <- "2024-10-30"
overwrite <- TRUE  # Set to overwrite existing files if needed

# Set prefix for file naming
prefix <- "openalex"
save_file <- file.path(search_data_dir, paste0(prefix, "_climate_agriculture_region15.csv"))

# Run OpenAlex query
if (!file.exists(save_file) | overwrite) {
  # Create the API endpoint with the search string
  api_endpoint <- oa_query(
    entity = "works",
    title_and_abstract.search = terms$full_search,
    from_publication_date = from_year,
    to_publication_date = to_year
  )
  
  # Check if the search string is within the allowed length
  if (nchar(api_endpoint) > 4000) {
    stop(paste0("Encoded search string has ", nchar(api_endpoint), " characters. Max allowed is 4000."))
  }
  
  # Execute the query and save the results
  hits <- oa_request(query_url = api_endpoint)
  hits_tab <- data.table(oa2df(hits, entity = "works"))
  
  # Select and save relevant fields
  hits_tab <- hits_tab[, .(id, display_name, doi, url, relevance_score, language, type, publication_date)]
  fwrite(hits_tab, file = save_file)
}
###########################################################################
# Test a simpler query without the region filter
test_search_string <- paste(climate_boolean, "AND", agriculture_boolean)
api_endpoint_test <- oa_query(
  entity = "works",
  title_and_abstract.search = test_search_string,
  from_publication_date = from_year,
  to_publication_date = to_year
)
# Execute and check results
test_hits <- oa_request(query_url = api_endpoint_test)
test_hits_tab <- data.table(oa2df(test_hits, entity = "works"))
print(nrow(test_hits_tab))  # Check how many results this yields


