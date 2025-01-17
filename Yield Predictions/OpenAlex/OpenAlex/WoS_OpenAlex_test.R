#Testing OpenAlex vs Wos

#Loading accepted and rejected WoS results 
# Load necessary package
library(openalexR)
library(data.table)
library(ggplot2)


# Define the file path
accepted <- "C:/Users/JNamita/OneDrive - CGIAR/PhD- Namita/1. Synthesis_Climate change impacts on legumes/WoS/To Extract.csv"
rejected <- list(
  "C:/Users/JNamita/OneDrive - CGIAR/PhD- Namita/1. Synthesis_Climate change impacts on legumes/WoS/1. No crop.csv",
  "C:/Users/JNamita/OneDrive - CGIAR/PhD- Namita/1. Synthesis_Climate change impacts on legumes/WoS/1. No future projections.csv",
  "C:/Users/JNamita/OneDrive - CGIAR/PhD- Namita/1. Synthesis_Climate change impacts on legumes/WoS/2. No future projections.csv",
  "C:/Users/JNamita/OneDrive - CGIAR/PhD- Namita/1. Synthesis_Climate change impacts on legumes/WoS/2. No outcome.csv",
  "C:/Users/JNamita/OneDrive - CGIAR/PhD- Namita/1. Synthesis_Climate change impacts on legumes/WoS/3. Not Africa.csv",
  "C:/Users/JNamita/OneDrive - CGIAR/PhD- Namita/1. Synthesis_Climate change impacts on legumes/WoS/3. Synthesis.csv",
  "C:/Users/JNamita/OneDrive - CGIAR/PhD- Namita/1. Synthesis_Climate change impacts on legumes/WoS/4. No Yield.csv",
  "C:/Users/JNamita/OneDrive - CGIAR/PhD- Namita/1. Synthesis_Climate change impacts on legumes/WoS/4. Not Africa.csv",
  "C:/Users/JNamita/OneDrive - CGIAR/PhD- Namita/1. Synthesis_Climate change impacts on legumes/WoS/5. Yield stability-suitability.csv",
  "C:/Users/JNamita/OneDrive - CGIAR/PhD- Namita/1. Synthesis_Climate change impacts on legumes/WoS/6. Non-modelled crop predictions.csv"
)


# Load the CSV file using fread from data.table for efficient reading
accepted_data <- fread(accepted, header = TRUE)

# Combine all rejected files into one data table
rejected_data <- rbindlist(lapply(rejected, function(f) fread(file = f)), use.names = TRUE, fill = TRUE)

# Preview the first few rows of the data to confirm it's loaded correctly
head(accepted_data)
head(rejected_data)

##Finding matches between accepted references and openalex hits
hits <- fread("C:/Users/JNamita/OneDrive - CGIAR/PhD- Namita/1. Synthesis_Climate change impacts on legumes/OpenAlex/search_data/openalex_climate_agriculture_region15.csv")
# Step 1: Rename columns for consistency
setnames(rejected_data, old = "Title", new = "display_name")
setnames(rejected_data, old = "DOI", new = "doi")  # Assuming DOI is the column name in accepted_data for DOI
setnames(accepted_data, old = "Title", new = "display_name")
setnames(accepted_data, old = "DOI", new = "doi")  # Assuming DOI is the column name in accepted_data for DOI

# Step 2: Clean and standardize the titles
rejected_data[, display_name := tolower(trimws(display_name))]  # Trim whitespace and make lowercase
accepted_data[, display_name := tolower(trimws(display_name))]
hits[, display_name := tolower(trimws(display_name))]     # Trim whitespace and make lowercase

# Step 3: Clean and standardize the DOIs
hits[, doi := tolower(sub("https://doi.org/", "", doi))]
rejected_data[, doi := tolower(trimws(doi))]  # Ensure lowercase and trim whitespace in accepted_data DOIs
accepted_data[, doi := tolower(trimws(doi))]

rejected_data_unique <- unique(rejected_data, by = "doi")
accepted_data_unique <- unique(accepted_data, by = "doi")
hits_unique <- unique(hits, by = "doi")

# Check a few sample DOIs that should match
print("Sample of DOIs in accepted_data:")
print(head(accepted_data$doi, 5))

print("Sample of DOIs in hits:")
print(head(hits$doi, 5))

print("Sample of Titles in accepted_data:")
print(head(accepted_data$display_name, 5))

print("Sample of Titles in hits:")
print(head(hits$display_name, 5))

# Step 3.1: Perform the merge based on display_name and doi- for rejected papers
# This will return only the rows in both datasets where the titles and DOIs match
matched_by_title <- merge(rejected_data_unique, hits_unique[, .(display_name, doi, relevance_score)], 
                          by = "display_name", all = FALSE)
# Perform the merge with allow.cartesian set to TRUE
matched_by_doi <- merge(rejected_data_unique, hits_unique[, .(display_name, doi, relevance_score)], 
                        by = "doi", all = FALSE)
dup_dois_rejected <- rejected_data[duplicated(rejected_data$doi), ]
cat("Number of duplicate DOIs in rejected_data:", nrow(dup_dois_rejected), "\n")
dup_dois_accepted <- accepted_data[duplicated(accepted_data$doi), ]
cat("Number of duplicate DOIs in accepted_data:", nrow(dup_dois_accepted), "\n")
dup_dois_hits <- hits[duplicated(hits$doi), ]
cat("Number of duplicate DOIs in hits:", nrow(dup_dois_hits), "\n")

# Combine the two data tables, allowing for missing columns in either
matched_data<- unique(rbindlist(list(matched_by_title, matched_by_doi), fill = TRUE), 
                       by = c("display_name", "doi"))

# Step 3.2: Perform the merge based on display_name and doi for accepted papers
# This will return only the rows in both datasets where the titles and DOIs match
matched_by_title_a <- merge(accepted_data_unique, hits_unique[, .(display_name, doi, relevance_score)], 
                          by = "display_name", all = FALSE)
# Perform the merge with allow.cartesian set to TRUE
matched_by_doi_a <- merge(accepted_data_unique, hits_unique[, .(display_name, doi, relevance_score)], 
                        by = "doi", all = FALSE)
dup_dois_rejected <- accepted_data[duplicated(accepted_data$doi), ]
cat("Number of duplicate DOIs in rejected_data:", nrow(dup_dois_accepted), "\n")

dup_dois_hits <- hits[duplicated(hits$doi), ]
cat("Number of duplicate DOIs in hits:", nrow(dup_dois_hits), "\n")

# Combine the two data tables, allowing for missing columns in either
matched_data_a <- unique(rbindlist(list(matched_by_title_a, matched_by_doi_a), fill = TRUE), 
                       by = c("display_name", "doi"))


# Preview the combined matched data
print("Preview of combined matched data:")
print(head(matched_data_a))


# Plot the histogram of relevance_score fir one dataset
ggplot(hits_unique, aes(x = relevance_score)) +
  geom_histogram(binwidth = 20, color = "black", fill = "grey") +
  labs(
    title = "Distribution of Relevance Score",
    x = "Relevance Score",
    y = "Frequency"
  ) +
  theme_minimal()

# Plot the histogram of relevance_score for 2 datasets
# Add a "source" column to each dataset
matched_data$source <- "matched_data"
matched_data_a$source <- "matched_data_a"
hits$source <- "hits"

# Select only the `relevance_score` and `source` columns, and combine them
combined_data <- rbind(
  matched_data[, .(relevance_score, source)],
  matched_data_a[, .(relevance_score, source)]
)

# Convert `source` to a factor with specific levels
# combined_data$source <- factor(combined_data$source, levels = c("matched_data", "matched_data_a", "hits_unique"))

# Plot the combined histogram with different colors for each dataset
# Plot the combined histogram with custom legend labels
ggplot(combined_data, aes(x = relevance_score, fill = source)) +
  geom_histogram(binwidth = 20, position = "identity", alpha = 0.6, color = "black") +
  scale_fill_manual(
    values = c("matched_data" = "skyblue", "matched_data_a" = "orange"),
    labels = c("matched_data" = "Rejected References", 
               "matched_data_a" = "Accepted References"
               )
  ) +
  labs(
    title = "Distribution of Relevance Score Across Datasets",
    x = "Relevance Score",
    y = "Frequency",
    fill = "Dataset"
  ) +
  theme_minimal()

# Assume that `accepted_data` is already cleaned and contains the relevant DOIs and Titles
# Convert DOI column to lowercase and clean up any DOI links
accepted_data_unique[, doi := tolower(trimws(gsub("https://doi.org/|http://dx.doi.org/", "", doi)))]
accepted_data_unique[, display_name := tolower(gsub("[^a-zA-Z0-9 ]", "", display_name))]  # Clean title for OpenAlex search

# Fetch OpenAlex records for DOIs
oa_dois <- data.table(oa_fetch(
  entity = "works",
  doi = accepted_data_unique[!is.na(doi), doi],
  verbose = TRUE
))[, indexed_oa := "yes"
][, .(doi, indexed_oa)]  # Keep only DOI and index status columns

# Clean up DOI format in OpenAlex data to match accepted_data format
oa_dois[, doi := trimws(gsub("https://doi.org/|http://dx.doi.org/", "", doi))]

# Fetch OpenAlex records for titles (using partial matching by title search)
oa_titles <- rbindlist(lapply(1:nrow(accepted_data), function(i) {
  data <- data.table(oa_fetch(
    entity = "works",
    display_name.search = accepted_data[i, display_name],
    verbose = TRUE
  ))[, indexed_oa := "yes"
  ][, .(title = accepted_data[i, display_name], indexed_oa)]
  data
}))

################################################################
# Ensure both datasets are data tables
library(data.table)
accepted_data <- as.data.table(accepted_data)
test_hits_tab <- as.data.table(test_hits_tab)

# Step 1: Standardize and Clean `display_name` and `doi` Columns
accepted_data[, display_name := tolower(trimws(display_name))]  # Convert titles to lowercase and trim whitespace
test_hits_tab[, display_name := tolower(trimws(display_name))]

# Clean and standardize DOIs
accepted_data[, doi := tolower(trimws(gsub("https://doi.org/", "", doi)))]
test_hits_tab[, doi := tolower(trimws(gsub("https://doi.org/", "", doi)))]

# Step 2: Match by `display_name` (Title)
matched_by_title_test <- merge(
  accepted_data, 
  test_hits_tab[, .(display_name, doi, relevance_score)],  # Keep only necessary columns
  by = "display_name", 
  all = FALSE, 
  allow.cartesian = TRUE  # Allows for multiple matches if there are duplicate titles
)

# Step 3: Match by `doi`
matched_by_doi <- merge(
  accepted_data, 
  test_hits_tab[, .(display_name, doi, relevance_score)], 
  by = "doi", 
  all = FALSE, 
  allow.cartesian = TRUE
)

# Step 4: Combine the Results and Remove Duplicates
# Use rbindlist to combine matches by title and by DOI
matched_data <- unique(rbindlist(list(matched_by_title, matched_by_doi), fill = TRUE), by = c("display_name", "doi"))

# Step 5: Preview Results
print("Preview of matched data between accepted_data and test_hits_tab:")
print(head(matched_data))
cat("Total number of matches found:", nrow(matched_data), "\n")
