# Functions


# required library for the function to run
library(dplyr)



#' Function for cleaning all of the mycetoma data files
#'
#' Using data frame as an element in a list, this function will clean the column
#' names to remove spaces and symbols, then rename some column headers to something
#' simpler. This will also standardize column headers across all files where applicable. 
#' Then the function generates new study metadata that is cleaned up and formatted 
#' consistently, and adds that study metadata to each file (as applicable).
#'
#' @param data_list List where each element is one of the mycetoma data frames
#'
#' @return data_list with data frames cleaned up and column names standardized
#'

process_data <- function(data_list){
  
  # Assuming list_of_dfs is your list of data frames
  for (i in seq_along(data_list)) {
    
    # Replace spaces with underscores in all column names
    colnames(data_list[[i]]) <- gsub(" ", "_", colnames(data_list[[i]]))
    # Replace spaces with slashes in all column names
    colnames(data_list[[i]]) <- gsub("/", "_", colnames(data_list[[i]]))
    
    # Check if "Study ID" column exists, and if so, then rename it
    if ("Study_ID" %in% colnames(data_list[[i]])) {
      # Rename "Study ID" to "ID"
      data_list[[i]] <- data_list[[i]] %>%
        dplyr::rename(ID = "Study_ID")
    } # don't need to change if already labeled ID
    
    # Check if "Total_N_of_mycetoma_patients" column exists, and if so, then rename it
    if ("Total_N_of_mycetoma_patients" %in% colnames(data_list[[i]])) {
      # Rename "Total_N_of_mycetoma_patients" to Total_N
      data_list[[i]] <- data_list[[i]] %>%
        dplyr::rename(Total_N = "Total_N_of_mycetoma_patients")
    } else if ("Total_number_of_mycetoma_patients" %in% colnames(data_list[[i]])) {
      # Rename "Total_N_of_mycetoma_patients" to Total_N
      data_list[[i]] <- data_list[[i]] %>%
        dplyr::rename(Total_N = "Total_number_of_mycetoma_patients")
    } else if ("Total_Mycetoma_Patients" %in% colnames(data_list[[i]])) {
      # Rename "Total_Mycetoma_Patients" to Total_N
      data_list[[i]] <- data_list[[i]] %>%
        dplyr::rename(Total_N = "Total_Mycetoma_Patients")
    } else if ("Total_mycetoma_Patients" %in% colnames(data_list[[i]])) {
      # Rename "Total_mycetoma_Patients" to Total_N
      data_list[[i]] <- data_list[[i]] %>%
        dplyr::rename(Total_N = "Total_mycetoma_Patients")
    } else if ("Number of  patients" %in% colnames(data_list[[i]])) {
      # Rename "Number of  patients" to Total_N
      data_list[[i]] <- data_list[[i]] %>%
        dplyr::rename(Total_N = "Number of  patients")
    }# don't need to change if already labeled correctly
    
    # if column called "Study_Initials" in included, then remove from the data frame
    if ("Study_Initials" %in% colnames(data_list[[i]])){
      data_list[[i]] <- data_list[[i]] %>%
        dplyr::select(!Study_Initials)
    }
  } # end for loop
  
  # create a data frame with just the study meta data (to be added to all other data frames so it is consistently formatted)
  study_metadata <- data_list[[2]] %>%
    dplyr::mutate(Study_meta = paste(First_Author_Initials, Country, Publication_Year, sep = ", ")) %>%
    dplyr::select(ID, Study_meta, Total_N)

  # add in the study meta-data for each list element, and reorder the columns
  for(x in seq_along(data_list)){
    if("ID" %in% colnames(data_list[[x]])){
      data_list[[x]] <- dplyr::left_join(data_list[[x]], study_metadata) %>%
        dplyr::select(ID, Study_meta, Total_N, everything())
    }
  }

  # add study meta data frame to data_list
  data_list <- c(data_list, study_metadata = list(study_metadata))

  return(data_list)
}

