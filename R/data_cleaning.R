# Cleaning data


# Getting set up --------------------------------------------------------------
# install libraries (only needs to be run the first time you use this code)
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("fs")
install.packages("purrr")
install.packages("here") # this is a link to how the here package works: https://here.r-lib.org/articles/here.html



# load libraries (run this each time you open R)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fs)
library(purrr)
library(here)

here() # this should give you the file pathway for the main mycetoma-analysis zip folder that I sent you (unzipped). 

# if it is not automatically set to that file path, then set the working directory 
setwd("project_path") # where you update the "project path" to the "mycetoma-analysis" folder in your documents (you can copy this from your file explorer if needed)
# the project path can look something like this "C:/user-name/Documents/Analysis/mycetoma-analysis" but update it for your file structure


here::i_am("R/data_cleaning.R") # declare the pathway to this script file

# source custom data processing function
source(here("R/functions.R")) # this just adds the functions that I wrote into your environment so that they can be used




# Read in data excel files and do some standardization of the headers -------------

# List all the Excel files in the data_input folder
file_path <- list.files(here("data_input"), pattern = "\\.xlsx$", full.names = TRUE)

# Read the Excel files in the data_input folder and combine them in a list
data_list <- purrr::map(file_path, ~ read_xlsx(.x, col_names = TRUE))

# add file names to each list element list
file_name <- tools::file_path_sans_ext(path_file(file_path))
names(data_list) <- file_name

# clean all data, make IDs the same, clean up header names
data_clean <- process_data(data_list)


# Unlist the data frames and assign them to individual objects with names
list2env(data_clean, envir = .GlobalEnv)
# Now you can access each data frame separately by its name (e.g. Age)


# Check each individual data frame for specific cleaning needed --------------------------

# All data sets need to have ID 1210 removed due to it being a duplicated paper with ID 5603. Only ID 5603 should remain.
# All demographic or clinical data sets should have ID 5411, ID 5772, and ID 4884 removed due to
# being unable to differentiate the non-mycetoma patients from the mycetoma patients in the study population.They can be included for the causative organism analysis.
# ID 2946 is a modeling study and not an epidemiological study, and should not be included in the demographic analyses, but it can still be included in the causative organism analysis.
# ID 4884 is not in the causative organism data, so it is fully removed from the study


## Basic Characteristics table --------------------------------------------------
#View(Basic_Characteristics_table)

Basic_Characteristics_table <- Basic_Characteristics_table %>%
  filter(ID != "ID 1210", ID != "ID 4884") %>% # remove this study, as described above
  select(ID, Study_meta, First_Author_Initials,Publication_Year, Country, Total_N, Total_study_population, Study_Design, Study_Population_Source, Tool_of_diagnosis, Citation)

nrow(Basic_Characteristics_table) # total of 72 studies included
write.csv(Basic_Characteristics_table, here("data_clean/Basic_Characteristics_table_clean.csv")) 

# Get country for each study ID
country_df <- Basic_Characteristics_table %>%
  select(ID, Country) %>%
  dplyr::rename(Country_clean = "Country")

unique(country_df$Country_clean)



n_studies_per_country <- Basic_Characteristics_table %>%
  group_by(Country) %>%
  count(.)
write.csv(n_studies_per_country, here("data_clean/N_studies_per_country.csv")) 


n_studies_per_study_design <- Basic_Characteristics_table %>%
  group_by(Study_Design) %>%
  count(.)
write.csv(n_studies_per_study_design, here("data_clean/N_studies_per_study_design.csv")) 

n_studies_per_study_population <- Basic_Characteristics_table %>%
  group_by(Study_Population_Source) %>%
  count(.)
write.csv(n_studies_per_study_design, here("data_clean/N_studies_per_study_population.csv")) 


## Quality assessment score --------------------------------------------------------

Quality_scores <- Quality_scores %>%
  filter(ID != "ID 1210", ID != "ID 4884") %>% # remove this study, as described above
  select(ID, Study_meta, Total_Selection, Total_Comparability, Total_Outcome, Total_Overall) %>%
  mutate(Cohort_Selection_Risk = case_when(
      Total_Selection >= 3 ~ "Low",
      Total_Selection == 2 ~ "Some Concerns",
      Total_Selection <= 1 ~ "High"),
    Comparability_Risk = case_when(
      Total_Comparability == 2 ~ "Low",
      Total_Comparability == 1 ~ "Some Concerns",
      Total_Comparability == 0 ~ "High"),
    Outcome_Risk = case_when(
      Total_Outcome == 3 ~ "Low",
      Total_Outcome == 2 ~ "Some Concerns",
      Total_Outcome <= 1 ~ "High"),
    Overall_Risk = case_when(
      Total_Overall >= 7 ~ "Low",
      Total_Overall > 4 & Total_Overall< 7 ~ "Some Concerns",
      Total_Overall <= 4 ~ "High"),
    Overall_Quality = case_when(
      Total_Overall >= 7 ~ "High",
      Total_Overall > 4 & Total_Overall< 7 ~ "Fair",
      Total_Overall <= 4 ~ "Low")) %>%
  arrange(Study_meta)

n_studies_per_quality <- Quality_scores %>%
  group_by(Overall_Quality) %>%
  count(.)


#View(Quality_scores)
write.csv(Quality_scores, here("data_clean/Quality_scores_clean.csv")) 


## Age data -----------------------------------------------------------------------
#View(Age)

Age <- Age %>%
  filter(ID != "ID 1210", ID != "ID 5411", ID != "ID 5772", ID != "ID 4884", ID != "ID 2946")  # remove these studies, as described above

nrow(Age)
mean(Age$Age_mean)
median(Age$Age_mean)
# Looks fairly complete with mean and sd of age for 24 studies
write.csv(Age, here("data_clean/Age_clean.csv")) 



## Sex data ---------------------------------------------------------------------
#View(Sex)

Sex_clean <- Sex %>%
  filter(ID != "ID 1210", ID != "ID 5411", ID != "ID 5772", ID != "ID 4884", ID != "ID 2946") %>% # remove these studies, as described above
  mutate(Male_percent = (Male_n/Total_N)*100,
         Female_percent = (Female_n/Total_N)*100,
         Not_reported_percent = (Not_reported/Total_N)*100) %>%
  dplyr::rename(Not_reported_n = "Not_reported") %>%
  select(ID:Not_reported_n, Male_percent, Female_percent, Not_reported_percent, Notes)
# Looks fairly complete with the number of males and females for 72 studies
write.csv(Sex_clean, here("data_clean/Sex_clean.csv"))

# convert the data table to a long format with separate columns for the n and percent
Sex_long <- Sex_clean %>%
  select(!Notes) %>%
  pivot_longer(cols = Male_n:Not_reported_percent, names_to = c("Sex", ".value"), names_pattern = "(.*)_(.*)")
write.csv(Sex_long, here("data_clean/Sex_long_clean.csv"))


# This code produces a file that only include studies where the total N of mycetoma patients does not equal the sum of the male and female
# Need to double check these values to ensure there are not typos from data entry, and add in patients to the unknown category if needed
Sex_to_check <- Sex_clean %>%
  mutate(Sum_Total_N = Male_n + Female_n + Not_reported_n,  # sum male and female
         check_values = if_else(Total_N == Sum_Total_N, TRUE, FALSE)) %>% # check if summed total equals the reported total from the paper
  select(ID:Not_reported_n, Sum_Total_N, check_values) %>% # reorder the columns
  filter(check_values == FALSE)  # filter to only include the studies that mismatched the total number of patients and the sum
# write a cvs file to export a data table. It gets saved to the data_output folder.
#write.csv(Sex_to_check, here("data_output/Sex_to_check.csv"))







# Education level ------------------------------------------------------------------
#View(Educational_level)



# Convert this data into a long format and calculate the percent of patients, which is  easier for making figures and analyses
Educational_level_clean <- Educational_level %>%
 filter(ID != "ID 1210", ID != "ID 5411", ID != "ID 5772", ID != "ID 4884", ID != "ID 2946") %>% # remove these studies, as described above
 select(ID:Literate) %>%
 rename(Illiterate_n = "Illiterate", Literate_n = "Literate") %>%
 mutate(Illiterate_percent = (Illiterate_n/Total_N)*100,
        Literate_percent = (Literate_n/Total_N)*100) %>%
 pivot_longer(cols = Illiterate_n:Literate_percent, names_to = c("Education_level", ".value"), names_pattern = "(.*)_(.*)")
write.csv(Educational_level_clean, here("data_clean/Educational_level_clean.csv"))




# Residency -----------------------------------------------------------------------
#View(Residency)

str(Residency)
Residency$Urban <- as.double(Residency$Urban)

# need to check the counts match to total N as shown above

# Convert this data into a long format and calculate the percent of patients, which is  easier for making figures and analyses
Residency_clean <- Residency %>%
 filter(ID != "ID 1210", ID != "ID 5411", ID != "ID 5772", ID != "ID 4884", ID != "ID 2946") %>% # remove these studies, as described above
 select(ID:Not_reported) %>%
 rename(Rural_n = "Rural", Urban_n = "Urban", Not_reported_n = "Not_reported") %>%
 mutate(Urban_n = if_else(is.na(Urban_n), 0.0, Urban_n), 
        Rural_percent = (Rural_n/Total_N)*100,
        Urban_percent = (Urban_n/Total_N)*100,
        Not_reported_percent = (Not_reported_n/Total_N)*100) %>%
 pivot_longer(cols = Rural_n:Not_reported_percent, names_to = c("Residency", ".value"), names_pattern = "(.*)_(.*)")
write.csv(Residency_clean, here("data_clean/Residency_clean.csv"))




## Occupation Data ---------------------------------------------------------------
#View(Occupation)

# This code produces a file that only include studies where the total N of mycetoma patients does not equal the sum of all the occupations
# Need to double check these values to ensure there are not typos from data entry, and add in patients to the unknown category if needed
Occupation_to_check <- Occupation %>%
  filter(ID != "ID 1210", ID != "ID 5411", ID != "ID 5772", ID != "ID 4884", ID != "ID 2946") %>% # remove these studies, as described above
  rowwise() %>%
  mutate(Sum_Total_N = sum(c_across(Professional:Not_reported), na.rm = T),
         check_values = if_else(Total_N == Sum_Total_N, T, F)) %>%
  select(ID, Study_meta, Professional:Not_reported, Sum_Total_N,  Total_N, check_values, Notes) %>%
  filter(check_values == FALSE)
# looks like everything matches up here - great!
# write a cvs file to export a data table. It gets saved to the data_output folder.
#write.csv(Occupation_to_check, here("data_output/Occupation_to_check.csv"))




# Convert this data into a long format, which is  easier for making figures and analyses
Occupation_long <- Occupation %>%
  filter(ID != "ID 1210", ID != "ID 5411", ID != "ID 5772", ID != "ID 4884", ID != "ID 2946") %>% # remove these studies, as described above
  select(ID:Not_reported) %>% # remove the notes column
  rename(Skilfull_Worker = "Skillfull_Worker", NonSkilful_Worker = "NonSkillful_Worker") %>%  # fix typo in names
  pivot_longer(cols = Professional:Not_reported, names_to = "Occupation_type", values_to = "n") %>% # convert the data table to a long format based on all of the occupations
  mutate(percent = (n/Total_N)*100,
         n = if_else(n == 0, NA, n),  # remove zeros and convert to NAs because none reported
         percent = if_else(percent == 0, NA, percent)) # remove zeros and convert to NAs because none reported)
write.csv(Occupation_long, here("data_clean/Occupation_long_clean.csv"))






# Co_morbidity ---------------------------------------------------------------------
#View(Co_morbidities)


# Convert this data into a long format and calculate the percent of patients, which is  easier for making figures and analyses
Co_morbidities_clean <- Co_morbidities %>%
  filter(ID != "ID 1210", ID != "ID 5411", ID != "ID 5772", ID != "ID 4884", ID != "ID 2946") %>% # remove these studies, as described above
  mutate_at(vars(-ID, -Study_meta, -Total_N, -Note), as.double) %>%
  select(ID:Asthma, Leprosy:Thyroid, Not_mentioned) %>%
  mutate(reported = rowSums(select(.,Renal_Disease:Thyroid), na.rm = T),
         Not_reported = Total_N - reported) %>%
  select(ID:Thyroid, Not_reported) %>%
  pivot_longer(cols = Renal_Disease:Thyroid, names_to = "Co_morbidities", values_to = "n") %>% # convert the data table to a long format based on all of the co-morbidities
  mutate(percent = (n/Total_N)*100, 
         Co_morbidities = factor(Co_morbidities, levels = c("Hypertension", "Diabetes", "Renal_Disease", "HIV", "TB", "Asthma", "Leprosy", "Thyroid", 
                                                            "Not_reported")))
write.csv(Co_morbidities_clean, here("data_clean/Co_morbidities_clean.csv"))



# Disease severity --------------------------------------------------------------------
#View(Disease_severity)

# clean up data and calculate the percent of patients with severe disease
Disease_severity_clean <- Disease_severity %>%
  filter(ID != "ID 1210", ID != "ID 5411", ID != "ID 5772", ID != "ID 4884", ID != "ID 2946") %>% # remove these studies, as described above
  mutate(percent = (Severe_disease/Total_N)*100) # calculate the percent of patients that have severe disease
write.csv(Disease_severity_clean, here("data_clean/Disease_severity_clean.csv")) 





# Involved body part ---------------------------------------------------------------
#View(Involved_body_part)


Involved_body_part_clean <- Involved_body_part %>%
   filter(ID != "ID 1210", ID != "ID 5411", ID != "ID 5772", ID != "ID 4884", ID != "ID 2946") %>% # remove these studies, as described above
   select(ID:UNSPECIFIED, Multiple_sites) %>%
   pivot_longer(cols = foot:Multiple_sites, names_to = "Body_part", values_to = "n") %>%
   left_join(., Involved_body_part_classifications) %>%
   select(ID:Total_N, Body_region, Body_part, n) %>%
   group_by(ID, Study_meta, Total_N, Body_region) %>%  
   summarize(n = sum(n, na.rm = T)) %>%
   mutate(n = if_else(n == 0, NA, n),
     percent = (n/Total_N)*100) 
write.csv(Involved_body_part_clean, here("data_clean/Involved_body_part_clean.csv"))


# Outcomes --------------------------------------------------------------------------
#View(Outcome)


# This code produces a file that only include studies where the total N of mycetoma patients does not equal the sum of all the occupations
# Need to double check these values to ensure there are not typos from data entry, and add in patients to the unknown category if needed
Outcome_to_check <- Outcome %>%
  filter(ID != "ID 1210", ID != "ID 5411", ID != "ID 5772", ID != "ID 4884", ID != "ID 2946") %>% # remove these studies, as described above
  select(ID:Recurrence, Lost_to_Followup, Not_reported) %>% # remove the notes column
  rowwise() %>%
  mutate(Sum_Total_N = sum(c_across(Cured:Not_reported), na.rm = T),
         check_values = if_else(Total_N == Sum_Total_N, T, F)) %>%
  select(ID, Study_meta, Cured:Lost_to_Followup, Sum_Total_N,  Total_N, check_values) %>%
  filter(check_values == FALSE)
# write a cvs file to export a data table. It gets saved to the data_output folder.
# write.csv(Outcome_to_check, here("data_output/Outcome_to_check.csv"))




# convert the data frame to a long format
Outcome_long <- Outcome %>%
  filter(ID != "ID 1210", ID != "ID 5411", ID != "ID 5772", ID != "ID 4884", ID != "ID 2946") %>% # remove these studies, as described above
  select(ID:Recurrence, Lost_to_Followup, Not_reported) %>% # remove the notes column
  pivot_longer(cols = Cured:Not_reported, names_to = "Outcome", values_to = "n") %>% # convert the data table to a long format based on all of the occupations
  mutate(percent = (n/Total_N)*100, # calculate the percent
         Outcome = factor(Outcome, levels = c("Cured", "Remission", "Improved", "Amputation", "Not_Improved", "Recurrence", "Lost_to_Followup", "Not_reported"))) # reset the order of the categories (important for the x-axis of the figure below)
write.csv(Outcome_long, here("data_clean/Outcomes_clean.csv"))




# Treatment received -----------------------------------------------------------------
#View(Treatment_received)

Treatment_classifications <- tibble(Treatment_classification = c("Pharmaceutical", rep("Surgical", 4), rep("Non-traditional",2), "Not reported"),
                                    Treatment_type = c("Pharmaceutical_treatment", "Surgical_debridement", "Surgical_excision", "Surgical_amputation",     
                                                       "Surgical_resection", "Religious", "Herbal_traditional", "Not_reported"))

Treatment_received_clean <- Treatment_received %>%
  filter(ID != "ID 1210", ID != "ID 5411", ID != "ID 5772", ID != "ID 4884", ID != "ID 2946") %>% # remove these studies, as described above
  select(ID:Total_N, Pharmaceutical_treatment:Not_reported) %>%
  pivot_longer(cols = Pharmaceutical_treatment:Not_reported, names_to = "Treatment_type", values_to = "n") %>%
  full_join(.,Treatment_classifications) %>%
  mutate(percent = (n/Total_N)*100) %>%
  select(ID:Total_N, Treatment_classification, Treatment_type, n, percent)
write.csv(Treatment_received_clean, here("data_clean/Treatment_received_clean.csv"))





# Causative Organism ------------------------------------------------------------
#View(Causative_Organisms_Taxonomy)

# check for mismatch in Total N and number of cases counted for each causative organism
Causative_Organisms_Taxonomy_check <- Causative_Organisms_Taxonomy %>%
  filter(ID != "ID 1210") %>% # remove this study, as described above
  left_join(., country_df) %>%
  rowwise() %>%
  mutate(check2 = sum(c_across(Actinomadura_spp:Not_reported), na.rm = T),
         check_binary = if_else(Total_N == check2, T, F)) %>%
  filter(check_binary == F) %>%
  select(ID, Study_meta, Country_clean, Actinomadura_spp:Not_reported, Total_N, check2, check_binary, Notes)
write.csv(Causative_Organisms_Taxonomy_check, here("data_output/Causative_Organisms_Taxonomy_check.csv"))

# change species names to match the Causative_Organisms_Taxonomy species names
Organism_classifications <- Organism_classifications %>%
  mutate(Species_name = gsub(" ", "_", Species_name))

# create a cleaned version of the causative organism with linked Eitology
Causative_Organisms_Taxonomy_clean <- Causative_Organisms_Taxonomy %>%
  filter(ID != "ID 1210") %>% # remove this study, as described above
  left_join(., country_df) %>%
  select(ID, Study_meta, Country_clean, Total_N, Actinomadura_spp:Not_reported) %>%
  pivot_longer(cols = Actinomadura_spp:Not_reported, names_to = "Species_name", values_to = "Organism_n") %>% # convert the data table to a long format based on all of the organisms
  left_join(., Organism_classifications) %>%
  mutate(Taxonomic_group = case_when(
    Taxonomic_group == "Bacteria" ~ "Bacteria",
    Taxonomic_group == "Fungus" ~ "Fungus",
    Species_name == "Unidentified" ~ "Unidentified",
    Species_name == "Not_reported" ~ "Not_reported"),
    Etiology = case_when(
      Taxonomic_group == "Bacteria" ~ "Actinomycetoma",
      Taxonomic_group == "Fungus" ~ "Eumycetoma",
      Species_name == "Unidentified" ~ "Unidentified",
      Species_name == "Not_reported" ~ "Not_reported"),
    Genus_name = if_else(Species_name == "Not_reported", "Not_reported", str_extract(Species_name, "^[^_]+")),
    Genus_name = if_else(Genus_name == "Eumycetoma", "Unidentified_Eumycetoma", Genus_name),
    Genus_name = if_else(Genus_name == "Actinomycetoma", "Unidentified_Actinomycetoma", Genus_name)) %>%
  select(ID:Total_N, Genus_name, Species_name:Etiology)
write.csv(Causative_Organisms_Taxonomy_clean, here("data_clean/Causative_Organisms_Taxonomy_clean.csv"))



