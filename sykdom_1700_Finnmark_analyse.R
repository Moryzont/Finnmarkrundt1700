#### Sykdom Finnmark rett etter 1700 datasett

rm(list=ls())
options(encoding = "UTF-8")

# Packages you need to run the script
required_packages <- c("rstudioapi", "tidyr","readr", "dplyr", "ggplot2", "purrr", "stringr", "zoo")

# Install any packages that are missing
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load the packages
lapply(required_packages, library, character.only = TRUE)

# Set the Working Directory to where the script is stored
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  script_path <- rstudioapi::getActiveDocumentContext()$path
  setwd(dirname(script_path))
}



### LAGER egne DF for hvert Manntall
# Get files from Working Directory
file_path <- getwd()

# List all CSV files in Working Directory that include 'Finnmark' in the filename
file_names <- list.files(path = file_path, pattern = "Finnmark.*\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames, assuming no headers
data_list <- setNames(
  lapply(file_names, function(f) {
    read.csv(f, header = TRUE, stringsAsFactors = FALSE)
  }),
  gsub("^.*/|\\.csv$", "", file_names)  # Remove path and .csv extension for naming
)


data_list <- lapply(data_list, function(df) {
  # Convert 'Folio' to character to avoid type conflicts later, check if column exists first
  if ("Folio" %in% names(df)) {
    df$Folio <- as.character(df$Folio)
  }
  
  # Rename the first column to 'Prefix'
  names(df)[1] <- "Prefix"
  
  # Add similar checks and conversions for other columns as necessary
  # Example:
  # if ("AnotherColumn" %in% names(df)) {
  #     df$AnotherColumn <- as.character(df$AnotherColumn)
  # }
  
  return(df)
})




########Data Cleaning######


######### Normaliseringsverktøy for navn, etternavn og stedsnavn ######
#Fornavn
# Read the normalization mappings
normalization_data <- read.csv("mappings_fornavn_norsk.csv", stringsAsFactors = FALSE, sep = ";")

# Extract unique names
unique_names <- lapply(samleliste, function(tbl) {
  unique(tbl$Fornavn)
}) %>% unlist() %>% unique() %>% sort()
# Filter names that have not been normalized yet
unnormalized_names <- setdiff(unique_names, normalization_data$Original)
# Create a DataFrame for unnormalized names with empty normalizations
unnormalized_df <- data.frame(Original = unnormalized_names, Normalized = NA, stringsAsFactors = FALSE)
# Combine with the existing normalization data
combined_data <- rbind(normalization_data, unnormalized_df)
# Sort the combined data alphabetically by original names
final_data <- combined_data %>% 
  arrange(Original)
# Optionally write the final DataFrame to a new CSV file
write.csv(final_data, "final_normalized_names.csv", row.names = FALSE)
# View the final DataFrame
print(final_data)


#### Leite opp rare navn####
# Define the function to look up name details within the list of data frames
lookup_name_details <- function(name) {
  # Use map to apply a filter across all data frames in the list
  details_list <- map(samleliste, ~ {
    # Check and adjust 'Før.navn' column type if it exists
    if ("Før.navn" %in% names(.x)) {
      if (is.logical(.x$`Før.navn`)) {
        .x$`Før.navn` <- as.character(.x$`Før.navn`)
      }
    } else {
      # Add 'Før.navn' as NA_character_ if it does not exist
      .x$`Før.navn` <- NA_character_
    }
    
    # Perform filtering and selection
    .x %>%
      filter(Fornavn == name) %>%
      select(Før.navn, Fornavn, Patronym, År, Tellingsområde) %>%
      distinct()
  })
  
  # Combine the list of data frames into one data frame
  details_combined <- bind_rows(details_list)
  
  # Check if any details were found and return appropriately
  if (nrow(details_combined) > 0) {
    return(details_combined)
  } else {
    return(data.frame(Før.navn = NA, Fornavn = name, Patronym = NA, År = NA, Tellingsområde = NA))
  }
}


### Leitefunksjon 
name_to_check <- "Thorio Niels"
name_details <- lookup_name_details(name_to_check)

# Print the results
print(name_details)


######Ettervavn
# Read the normalization mappings
normalization_data <- read.csv("mappings_patronym_norsk.csv", stringsAsFactors = FALSE, sep = ";")
normalization_data <- na.omit(normalization_data, cols = "Normalized")
# Extract unique names
unique_names <- lapply(samleliste, function(tbl) {
  unique(tbl$Patronym)
}) %>% unlist() %>% unique() %>% sort()
# Filter names that have not been normalized yet
unnormalized_names <- setdiff(unique_names, normalization_data$Original)
# Create a DataFrame for unnormalized names with empty normalizations
unnormalized_df <- data.frame(Original = unnormalized_names, Normalized = NA, stringsAsFactors = FALSE)
# Combine with the existing normalization data
combined_data <- rbind(normalization_data, unnormalized_df)
# Sort the combined data alphabetically by original names
final_data <- combined_data %>% 
  arrange(Original)
# Optionally write the final DataFrame to a new CSV file
write.csv(final_data, "final_normalized_patronym.csv", row.names = FALSE)
# View the final DataFrame
print(final_data)


#### Leite opp rare navn####
# Define a function to look up name details within the list of data frames
lookup_Patronym_details <- function(name) {
  # Use map to apply a filter across all data frames in the list
  details_list <- map(samleliste, ~ {
    # Check and adjust 'Før.navn' column type if it exists
    if ("Før.navn" %in% names(.x)) {
      if (is.logical(.x$`Før.navn`)) {
        .x$`Før.navn` <- as.character(.x$`Før.navn`)
      }
    } else {
      # Add 'Før.navn' as NA_character_ if it does not exist
      .x$`Før.navn` <- NA_character_
    }
    
    # Perform filtering and selection
    .x %>%
      filter(Patronym == name) %>%
      select(Før.navn, Fornavn, Patronym, År, Tellingsområde) %>%
      distinct()
  })
  
  # Combine the list of data frames into one data frame
  details_combined <- bind_rows(details_list)
  
  # Check if any details were found and return appropriately
  if (nrow(details_combined) > 0) {
    return(details_combined)
  } else {
    return(data.frame(Før.navn = NA, Fornavn = name, Patronym = NA, År = NA, Tellingsområde = NA))
  }
}
### Leitefunksjon 
name_to_check <- "Zefzrsen"
name_details <- lookup_Patronym_details(name_to_check)

# Print the results
print(name_details)




####Stedsnavn

# Read the normalization mappings
normalization_data <- read.csv("mappings_stedsnavn_norsk.csv", stringsAsFactors = FALSE, sep = ";")
# Extract unique names from each DataFrame in the list and combine
unique_names <- lapply(samleliste, function(tbl) {
  unique(tbl$Tellingsområde)
}) %>% unlist() %>% unique() %>% sort()
# Filter names that have not been normalized yet
unnormalized_names <- setdiff(unique_names, normalization_data$Original)
# Create a DataFrame for unnormalized names with empty normalizations and additional placeholders
if (length(unnormalized_names) > 0) {
  # Create a DataFrame for unnormalized names with empty normalizations and additional placeholders
  unnormalized_df <- data.frame(Original = unnormalized_names, 
                                Normalized = NA, 
                                Type = NA, 
                                Nord = NA, 
                                Øst = NA, 
                                Kommentar = NA, 
                                stringsAsFactors = FALSE)
} else {
  # Create an empty DataFrame with the same columns but no rows
  unnormalized_df <- data.frame(Original = character(0), 
                                Normalized = character(0), 
                                Type = character(0), 
                                Nord = numeric(0), 
                                Øst = numeric(0), 
                                Kommentar = character(0), 
                                stringsAsFactors = FALSE)
}

# Combine with the existing normalization data
combined_data <- rbind(normalization_data, unnormalized_df)
# Sort the combined data alphabetically by original names
final_data <- combined_data %>% 
  arrange(Original)
# Optionally write the final DataFrame to a new CSV file
write.csv(final_data, "final_normalized_stedsnavn.csv", row.names = FALSE)
# View the final DataFrame
print(final_data)

# Define a function to look up name details within the list of data frames
lookup_Stedsnavn_details <- function(name) {
  # Use map to apply a filter across all data frames in the list
  details_list <- map(samleliste, ~ {
    # Check and adjust 'Før.navn' column type if it exists
    if ("Før.navn" %in% names(.x)) {
      if (is.logical(.x$`Før.navn`)) {
        .x$`Før.navn` <- as.character(.x$`Før.navn`)
      }
    } else {
      # Add 'Før.navn' as NA_character_ if it does not exist
      .x$`Før.navn` <- NA_character_
    }
    
    # Perform filtering and selection
    .x %>%
      filter(Tellingsområde == name) %>%
      select(Før.navn, Fornavn, Patronym, År, Tellingsområde, Tingsted) %>%
      distinct()
  })
  
  # Combine the list of data frames into one data frame
  details_combined <- bind_rows(details_list)
  
  # Check if any details were found and return appropriately
  if (nrow(details_combined) > 0) {
    return(details_combined)
  } else {
    return(data.frame(Før.navn = NA, Fornavn = name, Patronym = NA, År = NA, Tellingsområde = NA))
  }
}
### Leitefunksjon 
name_to_check <- "Stranden"
name_details <- lookup_Stedsnavn_details(name_to_check)

# Print the results
print(name_details)

#### Prefix og Tilnavn
# Combine all dataframes into a single dataframe
combined_df <- bind_rows(data_list)

# Extract unique entries from the 'Prefix' column
unique_Prefix <- combined_df %>%
  select(Prefix) %>%
  distinct(Prefix) %>%
  filter(!is.na(Prefix)) %>%
  mutate(Normalized_Prefix = "")  # Add a column for normalized values, initially empty

# Rename the column to 'Original'
unique_Prefix <- unique_Prefix %>%
  rename(Original = Prefix)

# Write the dataframe to a CSV file
#write.csv(unique_Prefix, "unique_Prefix_for_Normalization.csv", row.names = FALSE, quote = TRUE)

# Extract unique entries from the 'Tilnavn' column
unique_Tilnavn <- combined_df %>%
  select(Tilnavn) %>%
  distinct(Tilnavn) %>%
  filter(!is.na(Tilnavn)) %>%
  mutate(Normalized = "")  # Add a column for normalized values, initially empty

# Rename the column to 'Original'
unique_Tilnavn <- unique_Tilnavn %>%
  rename(Original = Tilnavn)

# Write the dataframe to a CSV file
#write.csv(unique_Tilnavn, "unique_Tilnavn_for_Normalization.csv", row.names = FALSE, quote = TRUE)



##### ANVENDELSE AV NORMALISERINGEN ########

# Load normalization and mapping tables
normalized_stedsnavn <- read.csv("final_normalized_stedsnavn.csv", stringsAsFactors = FALSE, sep = ";")
normalized_patronym <- read.csv("final_normalized_patronym.csv", stringsAsFactors = FALSE, sep = ";")
normalized_names <- read.csv("final_normalized_names.csv", stringsAsFactors = FALSE, sep = ";")
tingsted_mappings <- read.csv("Ordered_Normalized_Tellingsområde_All_Years.csv", stringsAsFactors = FALSE, sep = ";")
prefix_mappings <- read.csv("unique_Prefix_for_Normalization.csv", stringsAsFactors = FALSE, sep = ";")
Tilnavn_mappings <- read.csv("unique_Tilnavn_for_Normalization.csv", stringsAsFactors = FALSE, sep = ";")

# Ensure all mapping tables have unique 'Original' entries
normalized_stedsnavn <- normalized_stedsnavn[!duplicated(normalized_stedsnavn$Original), ]
normalized_patronym <- normalized_patronym[!duplicated(normalized_patronym$Original), ]
normalized_names <- normalized_names[!duplicated(normalized_names$Original), ]
tingsted_mappings <- tingsted_mappings[!duplicated(tingsted_mappings$Normalized_Tellingsområde), ]
prefix_mappings <- prefix_mappings[!duplicated(prefix_mappings$Original), ]
Tilnavn_mappings <- Tilnavn_mappings[!duplicated(Tilnavn_mappings$Original), ]


normalize_and_map_data <- function(df) {
  # Merge with normalized names data
  if ("Fornavn" %in% names(df)) {
    df <- df %>%
      left_join(normalized_names, by = c("Fornavn" = "Original"), suffix = c("", ".dup")) %>%
      rename(Normalized_Fornavn = Normalized) %>%
      distinct()  # Remove duplicates after join if any
  }
  
  # Merge with normalized patronym data
  if ("Patronym" %in% names(df)) {
    df <- df %>%
      left_join(normalized_patronym, by = c("Patronym" = "Original"), suffix = c("", ".dup")) %>%
      rename(Normalized_Patronym = Normalized) %>%
      distinct()  # Remove duplicates after join if any
  }
  
  # Merge with normalized stedsnavn data and add Normalized_Tingsted
  if ("Tellingsområde" %in% names(df)) {
    df <- df %>%
      left_join(normalized_stedsnavn, by = c("Tellingsområde" = "Original"), suffix = c("", ".dup")) %>%
      rename(Normalized_Tellingsområde = Normalized) %>%
      left_join(tingsted_mappings, by = c("Normalized_Tellingsområde" = "Normalized_Tellingsområde"), suffix = c("", ".dup")) %>%
      distinct()  # Remove duplicates after join if any
  }
  
  # Merge with prefix data
  if ("Prefix" %in% names(df)) {
    df <- df %>%
      left_join(prefix_mappings, by = c("Prefix" = "Original"), suffix = c("", ".dup")) %>%
      rename(Normalized_Prefix = Normalized) %>%
      distinct()  # Remove duplicates after join if any
  }
  
  # Merge with tilnavn data
  if ("Tilnavn" %in% names(df)) {
    df <- df %>%
      left_join(Tilnavn_mappings, by = c("Tilnavn" = "Original"), suffix = c("", ".dup")) %>%
      rename(Normalized_Tilnavn = Normalized) %>%
      distinct()  # Remove duplicates after join if any
  }
  
  # Dynamically select columns to remove if they exist
  unnecessary_columns <- c("Kommentar", "Order_Index")
  cols_to_remove <- unnecessary_columns[unnecessary_columns %in% names(df)]
  df <- select(df, -all_of(cols_to_remove))
  
  return(df)
}


# Apply the normalization function to each dataframe in the list
normalized_list <- map(data_list, normalize_and_map_data)


View(normalized_list[[2]])
View(samleliste[[2]])




############# NAVNEMATCHING MARERITT OMG ###### 


add_name_id <- function(df) {
  df %>%
    mutate(
      Name_ID = paste(
        coalesce(Alder.og.størrelse, ""),
        coalesce(Normalized_Fornavn, ""),
        coalesce(Normalized_Patronym, ""),
        sep = if_else(
          rowSums(across(c(Alder.og.størrelse, Normalized_Fornavn, Normalized_Patronym), is.na)) == 3,
          "",  # If all are NA, no separator
          "_"
        )
      ) %>%
        gsub(pattern = "_+", replacement = "_", x = .) %>%  # Replace multiple underscores with one
        gsub(pattern = "^_|_$", replacement = "", x = .)  # Remove trailing and leading underscores
    )
}

normalized_list_with_name_ids <- map(normalized_list, add_name_id)



View(normalized_list_with_name_ids[[6]])


#koble på tvers av år

track_people_across_years <- function(df1, df2) {
  # Perform the join on Name_ID and Normalized_Tellingsområde
  matched <- df1 %>%
    left_join(df2, by = c("Name_ID" = "Name_ID", "Normalized_Tellingsområde" = "Normalized_Tellingsområde"), suffix = c("", ".next")) %>%
    group_by(Name_ID, Normalized_Tellingsområde) %>%
    summarise(
      count = n_distinct(row.names(df2[df2$Name_ID == Name_ID & df2$Normalized_Tellingsområde == Normalized_Tellingsområde, ])),
      .groups = 'drop'
    ) %>%
    mutate(
      obs_next = case_when(
        count == 0 ~ "NO",
        count >= 1 ~ "YES",
        TRUE ~ "MULTIPLE"
      )
    )
  
  # Join back to df1 to append the obs_next results
  df1 <- left_join(df1, matched, by = c("Name_ID", "Normalized_Tellingsområde")) %>%
    mutate(obs_next = coalesce(obs_next, "NO"))  # Ensure 'NO' is default when no matches are found
  
  return(df1)
}

results_list <- map2(normalized_list_with_name_ids[-length(normalized_list_with_name_ids)],
                     normalized_list_with_name_ids[-1], 
                     ~track_people_across_years(.x, .y))


# Viewing the first result to inspect the outputs for the first dataset comparison
View(results_list[[1]])  


# Analyse, dødelighet:

# Use the existing code to filter data
filtered_list <- map(normalized_list, function(df) {
  if("Skatteklasse" %in% names(df)) {
    df %>% filter(Skatteklasse != "flyttet")
  } else {
    df  # Return the dataframe unchanged if 'Skatteklasse' column does not exist
  }
})

# Initial aggregation within each data frame
aggregated_data <- map_df(filtered_list, function(df) {
  if("År" %in% names(df) && "Skatteklasse" %in% names(df)) {
    df %>%
      group_by(År) %>%
      summarise(
        total = n(),
        dead = sum(Skatteklasse == "Død", na.rm = TRUE),
        .groups = 'drop'
      )
  } else if ("År" %in% names(df)) {
    df %>%
      group_by(År) %>%
      summarise(
        total = n(),
        dead = 0,  # Assume zero deaths if 'Skatteklasse' is missing
        .groups = 'drop'
      )
  }
}, .id = "source")

# Additional step to aggregate across all sources if years are duplicated
final_aggregated_data <- aggregated_data %>%
  group_by(År) %>%
  summarise(
    total = sum(total),
    dead = sum(dead),
    .groups = 'drop'
  )

# Create a data frame of all years in the range and exclude specific years
all_years <- tibble(År = 1689:1706) %>%
  filter(!År %in% c(1690, 1691, 1692, 1693, 1695, 1696, 1697, 1698, 1699, 1700))  # Exclude years 1692 and 1704

# Merge and handle missing years
complete_data <- all_years %>%
  left_join(aggregated_data, by = "År") %>%
  replace_na(list(total = 0, dead = 0))

# Interpolate missing data for 'total' and 'dead'
complete_data$total <- na.approx(complete_data$total, na.rm = FALSE)
complete_data$dead <- na.approx(complete_data$dead, na.rm = FALSE)

# Plot the data
ggplot(complete_data, aes(x = År)) +
  geom_line(aes(y = total, colour = "Total Observations"), size = 1.2, linetype = "solid") +
  geom_line(aes(y = dead, colour = "Deaths ('Død')"), size = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("Total Observations" = "blue", "Deaths ('Død')" = "red")) +
  labs(title = "Annual Observations and Deaths (1689-1706, Excluding 1692 and 1704)",
       subtitle = "Interpolated for missing years; 'flyttet' excluded",
       x = "Year", y = "Number of Observations",
       color = "Legend") +
  theme_minimal()

View(normalized_list[[15]])
### Finneby ####
# Initial aggregation within each data frame
aggregated_data <- map_df(filtered_list, function(df) {
  # Ensure the necessary columns are present
  if("År" %in% names(df) && "Normalized_Tellingsområde" %in% names(df) && "Type" %in% names(df)) {
    # Filter out the data for the specified years and including 'Finneby'
    df_filtered <- df %>%
      filter(År %in% c(1705, 1706), Type == "Finneby")
    
    # Group by 'Normalized_Tellingsområde' and 'År' to compute totals and deaths
    df_grouped <- df_filtered %>%
      group_by(Normalized_Tellingsområde, År) %>%
      summarise(
        total = n(),  # Count all entries including 'Finneby'
        dead = if("Skatteklasse" %in% names(df_filtered)) sum(Skatteklasse == "Død", na.rm = TRUE) else 0,
        .groups = 'drop'
      )
    
    return(df_grouped)
  } else {
    NULL  # Return NULL if essential columns are missing, to prevent errors
  }
}, .id = "source")

# Sorting data by 'Normalized_Tellingsområde' and then 'År'
sorted_data <- aggregated_data %>%
  arrange(Normalized_Tellingsområde, År)

# Optionally, aggregate further if 'Normalized_Tellingsområde' and 'År' pairs are duplicated across sources
final_aggregated_data <- sorted_data %>%
  group_by(Normalized_Tellingsområde, År) %>%
  summarise(
    total = sum(total),
    dead = sum(dead),
    .groups = 'drop'
  ) %>%
  filter(dead > 0)  # Exclude areas with zero deaths

# Display the final table
View(final_aggregated_data)


# Tingsted

# Initial aggregation within each data frame
aggregated_data <- map_df(filtered_list, function(df) {
  # Ensure the necessary columns are present
  if("År" %in% names(df) && "Normalized_Tingsted" %in% names(df) && "Type" %in% names(df)) {
    # Filter out the data for the specified years and excluding 'Finneby'
    df_filtered <- df %>%
      filter(År %in% c(1705, 1706), Type != "Finneby")
    
    # Group by 'Normalized_Tingsted' and 'År' to compute totals and deaths
    df_grouped <- df_filtered %>%
      group_by(Normalized_Tingsted, År) %>%
      summarise(
        total = n(),  # Count all entries excluding 'Finneby'
        dead = if("Skatteklasse" %in% names(df_filtered)) sum(Skatteklasse == "Død", na.rm = TRUE) else 0,
        .groups = 'drop'
      )
    
    return(df_grouped)
  } else {
    NULL  # Return NULL if essential columns are missing, to prevent errors
  }
}, .id = "source")

# Sorting data by 'Normalized_Tingsted' and then 'År'
sorted_data <- aggregated_data %>%
  arrange(Normalized_Tingsted, År)

# Optionally, aggregate further if 'Normalized_Tingsted' and 'År' pairs are duplicated across sources
final_aggregated_data <- sorted_data %>%
  group_by(Normalized_Tingsted, År) %>%
  summarise(
    total = sum(total),
    dead = sum(dead),
    .groups = 'drop'
  ) %>%
  filter(dead > 0)  # Exclude areas with zero deaths

# Display the final table
View(final_aggregated_data)

# Extracting records of dead people from 1705 & 1706
dead_people_data <- map_df(filtered_list, function(df) {
  if(all(c("År", "Skatteklasse", "Normalized_Fornavn", "Nord", "Øst", "Normalized_Tellingsområde") %in% names(df))) {
    df %>%
      filter(År %in% c(1705, 1706), Skatteklasse == "Død") %>%
      select(Normalized_Fornavn, Normalized_Tellingsområde, Nord, Øst)  # Selecting the specified columns
  } else {
    NULL  # Return NULL if essential columns are missing, to prevent errors
  }
}, .id = "Normalized_Tellingsområde")

# Print to console
print(dead_people_data)

# Save the data to a CSV file
write.csv(dead_people_data, "Dead_People_1705_1706.csv", row.names = FALSE)

write.csv(filtered_list[[13]], "1706.csv", row.names = FALSE)


# Extracting unique records for the year 1706
unique_data_1706 <- map_df(filtered_list, function(df) {
  # Check for the presence of the necessary columns
  if(all(c("År", "Normalized_Tellingsområde", "Nord", "Øst") %in% names(df))) {
    df %>%
      filter(År == 1706) %>%
      filter(Type != "Tingsted") %>%
      select(Normalized_Tellingsområde, Nord, Øst) %>%
      distinct()  # Ensure the entries are unique
  } else {
    NULL  # Return NULL if essential columns are missing, to prevent errors
  }
}, .id = "source")

# Print to console
print(unique_data_1706)

# Save the data to a CSV file
write.csv(unique_data_1706, "Unique_Tellingsomrade_1706.csv", row.names = FALSE)
