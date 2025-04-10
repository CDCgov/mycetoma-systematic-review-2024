# Summary statistics and figures for the Mycetoma literature data

# Getting set up --------------------------------------------------------------
# install libraries (only needs to be run the first time you use this code)
install.packages("dplyr")
install.packages("tidyr")
install.packages("here")
install.packages("ggplot2")
install.packages("pals")
install.packages("colorblindcheck")
install.packages("ggpubr")

# load libraries (run this each time you open R)
library(dplyr)
library(tidyr)
library(here)
library(ggplot2)
library(pals)
library(colorblindcheck)
library(ggpubr)


# load in the clean data (need to run the clean data code first) -------------------
Age_clean <- read.csv(here("data_clean/Age_clean.csv"), header = T)
Sex_clean <- read.csv(here("data_clean/Sex_clean.csv"), header = T)
Sex_long <- read.csv(here("data_clean/Sex_long_clean.csv"), header = T)
Co_morbidities_clean <- read.csv(here("data_clean/Co_morbidities_clean.csv"), header = T)
Disease_severity_clean <- read.csv(here("data_clean/Disease_severity_clean.csv"), header = T)
Educational_level_clean <- read.csv(here("data_clean/Educational_level_clean.csv"), header = T)
Occupation_clean  <- read.csv(here("data_clean/Occupation_long_clean.csv"), header = T)
Outcome_clean <- read.csv(here("data_clean/Outcomes_clean.csv"), header = T)
Residency_clean <- read.csv(here("data_clean/Residency_clean.csv"), header = T)
Involved_body_part_clean <- read.csv(here("data_clean/Involved_body_part_clean.csv"), header = T)
Treatment_received_clean <- read.csv(here("data_clean/Treatment_received_clean.csv"), header = T)
Basic_Characteristics_table_final <- read.csv(here("data_clean/Basic_Characteristics_table_clean.csv"), header = T) 
Causative_Organisms_Taxonomy_clean <- read.csv(here("data_clean/Causative_Organisms_Taxonomy_clean.csv"), header = T, stringsAsFactors = T)


# Final version of the Basic Characteristics Table in Manuscript
# Table 1

Basic_Characteristics_table_final <- Basic_Characteristics_table_final %>%
  mutate(Age = if_else(ID %in% Age_clean$ID, 1, 0),
         Sex = if_else(ID %in% Sex_clean$ID, 1, 0),
         Education = if_else(ID %in% unique(Educational_level_clean$ID), 1, 0),
         Residency = if_else(ID %in% unique(Residency_clean$ID), 1, 0),
         Occupation = if_else(ID %in% unique(Occupation_clean$ID), 1, 0),
         Comorbidities = if_else(ID %in% unique(Co_morbidities_clean$ID), 1, 0),
         Severe_disease = if_else(ID %in% unique(Disease_severity_clean$ID), 1, 0),
         Body_region = if_else(ID %in% unique(Involved_body_part_clean$ID), 1, 0),
         Outcomes = if_else(ID %in% unique(Outcome_clean$ID), 1, 0),
         Treatment = if_else(ID %in% unique(Treatment_received_clean$ID), 1, 0),
         Causative_Organism = if_else(ID %in% unique(Causative_Organisms_Taxonomy_clean$ID), 1, 0),
         Study_meta = paste(First_Author_Initials, "et al.", Publication_Year, sep = " ")) %>%
  select(ID, Study_meta, Country, Total_N:Causative_Organism)
write.csv(Basic_Characteristics_table_final, here("data_summary/Basic_Characteristics_table_final.csv"))




# Summary statistics for Age ----------------------------------------------------------------

# calculate summary statistics for age
Age_summary <- Age_clean %>%
  summarize(mean = mean(Age_mean),
            sd = sd(Age_mean),
            min = min(Age_mean),
            max = max(Age_mean),
            n_studies = sum(!is.na(Age_mean)))
write.csv(Age_summary, here("data_summary/Age_summary.csv"))

#number of studies included 
length(unique(Age_clean$ID))  #20

# number of patients included
age_patients <- Age_clean %>%
  summarize(sum = sum(Total_N))
# 8238

Age_clean <- mutate(Age_clean, Age = "Age")

# example violin and boxplot of age
age_plot <- ggplot(Age_clean, aes(x = Age, y = Age_mean)) +
  geom_violin(aes(color = Age), show.legend = F) +   # produces the violin plot (lines on the outside of the boxplot)
  geom_boxplot(aes(color = Age), width = 0.2, show.legend = F) +  # produces the boxplot inside the boxplot
  geom_jitter(aes(color = Age), width = 0.05, height = 0.01, alpha = 0.6, show.legend = F) +  # produces the points in the plot
  scale_color_manual(values = tol(1)) +
  scale_x_discrete(labels = NULL) +
  labs(y = "Mean age of Mycetoma patients") +
  theme_bw()  + # changes the theme, there are many to choose from in ggplot2
  theme(axis.text = element_text(size=9, color = "black"), axis.title.y = element_text(size=11, color="black"), 
        axis.title.x = element_text(size=11, color="black")) # resize text and make it black
age_plot
ggsave(here("figures/age_plot.png"), plot = age_plot) # saves the figure



# Summary statistics for Sex -------------------------------------------------------------

# filter out the not reported for analysis
Sex_long_mf <- Sex_long %>% 
  filter(Sex != "Not_reported") %>%
  mutate(unique_code = c(seq(nrow(.))),
         Sex = factor(Sex, levels = c("Male", "Female"))) 

# calculate summary statistics for Sex
Sex_summary <- Sex_long %>%
  group_by(Sex) %>%  # group the data by each category in Sex column
  summarize(mean_percent = mean(percent),  # calculate the mean percent for each category in the Sex column, and so on...
            lower_percent = min(percent),
            upper_percent = max(percent),
            n_studies = sum(!is.na(percent)))
write.csv(Sex_summary, here("data_summary/Sex_summary.csv"))


#number of studies included 
length(unique(Sex_clean$ID))  #68

# number of patients included
sex_patients <- Sex_clean %>%
  summarize(sum = sum(Total_N))
    # 22045

# set color palette for occupation types
sex_colors <- tol(2)
names(sex_colors) <- c("Male", "Female")
# Set color palette for colorblindness
palette_check(sex_colors, plot = TRUE)

# example violin and boxplot of Sex
sex_plot <- ggplot(Sex_long_mf, aes(x = Sex, y = percent)) +
  geom_violin(aes(color = Sex), show.legend = F) +   # produces the violin plot (lines on the outside of the boxplot)
  geom_boxplot(aes(color = Sex), width = 0.2,show.legend = F) +  # produces the boxplot inside the boxplot
  geom_jitter(aes(color = Sex), width = 0.05, height = 0.01, alpha = 0.6, show.legend = F) +  # produces the points in the plot
  scale_color_manual(values = Sex_colors) +
  labs(y = "Percent of Mycetoma patients (%)", x = "Sex") +
  theme_bw() +  # changes the theme, there are many to choose from in ggplot2
  theme(axis.text = element_text(size=9, color = "black"), axis.title.y = element_text(size=11, color="black"), 
        axis.title.x = element_text(size=11, color="black")) # resize text and make it black
sex_plot
ggsave(here("figures/sex_plot.png"), plot = sex_plot) # saves the figure
# note, that we should display the not-reported or at least report that value somewhere in the text


# Summary statistics for Educational Level ----------------------------------------------------------------

# calculate summary statistics for education level
Education_summary <- Educational_level_clean %>%
  group_by(Education_level) %>%  # group the data by each category in Education_level column
  summarize(mean_percent = mean(percent),  # calculate the mean percent for each category in the Education_level column, and so on...
            lower_percent = min(percent),
            upper_percent = max(percent),
            n_studies = sum(!is.na(percent)))
write.csv(Education_summary, here("data_summary/Education_summary.csv"))


#number of studies included 
length(unique(Educational_level_clean$ID))  # 3

# number of patients included
education_patients <- Educational_level_clean %>%
  select(ID:Total_N) %>%
  distinct() %>%
  summarize(sum = sum(Total_N))
# 774


# probably won't use this figure because too few studies
# example violin and boxplot of education level
education_plot <- ggplot(Educational_level_clean, aes(x = Education_level, y = percent)) +
  geom_violin(aes(color = Education_level), show.legend = F) +   # produces the violin plot (lines on the outside of the boxplot)
  geom_boxplot(aes(color = Education_level), width = 0.1) +  # produces the boxplot inside the  violin plot
  geom_jitter(aes(color = Education_level), width = 0.05, height = 0.01, alpha = 0.6) +  # produces the points in the plot
  scale_color_manual(values = tol(2)) +
  labs(y = "Percent of Mycetoma patients (%)") +
  theme_bw()   # changes the theme, there are many to choose from in ggplot2
education_plot
ggsave(here("figures/education_plot.png"), plot = education_plot) # saves the figure




# Summary statistics for Residency ----------------------------------------------------------------

# calculate summary statistics for Residency
Residency_summary <- Residency_clean %>%
  group_by(Residency) %>%  # group the data by each category in Residency column
  summarize(mean_percent = mean(percent),  # calculate the mean percent for each category in the Residency column, and so on...
            lower_percent = min(percent),
            upper_percent = max(percent),
            n_studies = sum(!is.na(percent)))
write.csv(Residency_summary, here("data_summary/Residency_summary.csv"))


#number of studies included 
length(unique(Residency_clean$ID))  # 9

# number of patients included
residency_patients <- Residency_clean %>%
  select(ID:Total_N) %>%
  distinct() %>%
  summarize(sum = sum(Total_N))
# 871

# specify the order of the levels
Residency_clean$Residency <- factor(Residency_clean$Residency, levels = c("Rural", "Urban", "Not_reported"))

# probably won't use this figure because too few studies
# example violin and boxplot of education level
residency_plot <- ggplot(Residency_clean, aes(x = Residency, y = percent)) +
  #geom_violin(aes(color = Residency), show.legend = F) +   # produces the violin plot (lines on the outside of the boxplot)
  geom_boxplot(aes(color = Residency), width = 0.2) +  # produces the boxplot inside the  violin plot
  geom_jitter(aes(color = Residency), width = 0.05, height = 0.01, alpha = 0.6) +  # produces the points in the plot
  scale_color_manual(values = tol(3), labels = c("Rural", "Urban", "Not reported")) +
  scale_x_discrete(labels = c("Rural", "Urban", "Not reported")) +
  labs(y = "Percent of Mycetoma patients (%)") +
  theme_bw()   # changes the theme, there are many to choose from in ggplot2
residency_plot
ggsave(here("figures/residency_plot.png"), plot = residency_plot) # saves the figure



# Summary statistics for Occupation ----------------------------------------------------------------

# re-order the Occupations from greatest to smallest medians
Occupation_clean <- Occupation_clean %>%
  mutate(Occupation_type = factor(Occupation_type, levels = c("Farmer", "Housewife", "Animal_Breeder", "Skilfull_Worker",  "Student",  "NonSkilful_Worker", "Freelancer",          
                                                              "Unemployed", "Government", "Professional", "Other_Jobs_Not_specified", "Not_reported")))


# generate a summary data set with the mean, median, sd, and min and max values for each occupation
Occupation_summary <- Occupation_clean %>%
  group_by(Occupation_type) %>%
  summarize(mean_percent = mean(percent, na.rm = T),
            lower_percent = min(percent, na.rm = T),
            upper_percent = max(percent, na.rm = T),
            n_studies = sum(!is.na(percent)))
write.csv(Occupation_summary, here("data_summary/Occupation_summary.csv"))


#number of studies included 
length(unique(Occupation_clean$ID))  # 36

# number of patients included
occupation_patients <- Occupation_clean %>%
  select(ID:Total_N) %>%
  distinct() %>%
  summarize(sum = sum(Total_N))
# 17722


# set color palette for occupation types
occupation_colors <- tol(12)
names(occupation_colors) <- c("Farmer", "Housewife", "Animal_Breeder", "Skilfull_Worker",  "Student",  "NonSkilful_Worker", "Freelancer",          
                              "Unemployed", "Government", "Professional", "Other_Jobs_Not_specified", "Not_reported")
# Set color palette for colorblindness
palette_check(occupation_colors, plot = TRUE)


# example boxplot figure code
occupation_boxplot <- ggplot(data = Occupation_clean, aes(x = Occupation_type, y = percent, color = Occupation_type)) + # set the data source, and x and y columns, set color based on categorical variable
  geom_boxplot(show.legend = F) +
  geom_point( position = position_jitter(width = 0.1), alpha = 0.6, show.legend = F) +  # generate the points, jitter makes it so that you can see points that are the same value a little easier
  scale_color_manual(values = occupation_colors, name = "Occupation", labels = c("Farmer", "Housewife", "Animal Breeder", "Skillful Worker",  "Student",  "NonSkillful Worker", "Freelancer",          
                                                            "Unemployed", "Government", "Professional", "Other Jobs, Not specified", "Not reported")) +
  scale_x_discrete(labels = c("Farmer", "Housewife", "Animal Breeder", "Skilful Worker",  "Student",  "NonSkilful Worker", "Freelancer",          
                              "Unemployed", "Government", "Professional", "Other Jobs, Not specified", "Not reported")) +
  labs(x = "Occupation", y = "Percent of Mycetoma patients (%)") + # label axes
  theme_bw() + # change the theme, there are many options out there
  theme(axis.text.x = element_text(angle = 30, hjust = 1), axis.text = element_text(size=9, color = "black"), axis.title.y = element_text(size=11, color="black"), 
        axis.title.x = element_text(size=11, color="black")) # get x axis labels to be at an angle so they are legible, resize text and make it black
occupation_boxplot  # view the plot
ggsave(here("figures/occupation_boxplot.png"), plot = occupation_boxplot)  # save the file to the figures folder
# note that I like the boxplot with the data points because it shows that range and median of the data, but it also
# indicates the number of raw data points that went into each category (i.e. number of studies that report that occupation).



# Summary statistics for Co-morbidities ----------------------------------------------------------------

# generate a summary data set with the mean, median, sd, and min and max values for each occupation
Co_morbidities_summary <- Co_morbidities_clean %>%
  group_by(Co_morbidities) %>%
  summarize(mean_percent = mean(percent, na.rm = T),
            lower_percent = min(percent, na.rm = T),
            upper_percent = max(percent, na.rm = T),
            n_studies = sum(!is.na(percent)))
write.csv(Co_morbidities_summary, here("data_summary/Co_morbidities_summary.csv"))


#number of studies included 
length(unique(Co_morbidities_clean$ID))  # 5

# number of patients included
co_morbidities_patients <- Co_morbidities_clean %>%
  select(ID:Total_N) %>%
  distinct() %>%
  summarize(sum = sum(Total_N))
# 7412


# set color palette for comorbidities types
comorbidities_colors <- tol(8)
names(comorbidities_colors) <- c("Hypertension", "Diabetes", "Renal_Disease", "HIV",  "TB",  "Asthma", "Leprosy",          
                              "Thyroid")
# Set color palette for colorblindness
palette_check(comorbidities_colors, plot = TRUE)


# example boxplot figure code
co_morbidities_boxplot <- ggplot(data = Co_morbidities_clean, aes(x = Co_morbidities, y = percent, color = Co_morbidities)) + # set the data source, and x and y columns, set color based on categorical variable
  geom_boxplot() +
  geom_point(pch = 1, position = position_jitter(width = 0.1)) +  # generate the points, jitter makes it so that you can see points that are the same value a little easier
  scale_color_manual(values = comorbidities_colors, name = "Comorbidities", labels = c("Hypertension", "Diabetes", "Renal Disease", "HIV",  "TB",  "Asthma", "Leprosy",          
                                                                                 "Thyroid")) +
  scale_x_discrete(labels = c("Hypertension", "Diabetes", "Renal Disease", "HIV",  "TB",  "Asthma", "Leprosy",          
                              "Thyroid")) +
  labs(x = "Comorbidities", y = "Percent of Mycetoma patients (%)") + # label axes
  theme_bw() + # change the theme, there are many options out there
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # get x axis labels to be at an angle so they are legible 
co_morbidities_boxplot  # view the plot
ggsave(here("figures/co_morbidities_boxplot.png"), plot = co_morbidities_boxplot)  # save the file to the figures folder



# Summary statistics for Disease Severity ----------------------------------------------------------------

# generate a summary data set with the mean, median, sd, and min and max values for each occupation
Disease_severity_summary <- Disease_severity_clean %>%
  summarize(mean_percent = mean(percent, na.rm = T),
            lower_percent = min(percent, na.rm = T),
            upper_percent = max(percent, na.rm = T),
            n_studies = sum(!is.na(percent)))
write.csv(Disease_severity_summary, here("data_summary/Disease_severity_summary.csv"))


#number of studies included 
length(unique(Disease_severity_clean$ID))  # 19

# number of patients included
disease_severe_patients <- Disease_severity_clean %>%
  summarize(sum = sum(Total_N))
# 1208



Disease_severity_clean <- mutate(Disease_severity_clean, Disease_severity = "Disease_severity")

# example violin and boxplot of Sex
disease_severe_plot <- ggplot(Disease_severity_clean, aes(x = Disease_severity, y = percent)) +
  geom_violin(aes(color = Disease_severity), show.legend = F) +   # produces the violin plot (lines on the outside of the boxplot)
  geom_boxplot(aes(color = Disease_severity), width = 0.2, show.legend = F) +  # produces the boxplot inside the violin plot
  geom_jitter(aes(color = Disease_severity), width = 0.05, height = 0.01, alpha = 0.6, show.legend = F) +  # produces the points in the plot
  scale_color_manual(values = tol(1)) +
  scale_x_discrete(labels = NULL) +
  labs(y = "Percent of Mycetoma patients (%)", x = "Severe disease") +
  theme_bw() +   # changes the theme, there are many to choose from in ggplot2
  theme(axis.text = element_text(size=9, color = "black"), axis.title.y = element_text(size=11, color="black"), 
        axis.title.x = element_text(size=11, color="black")) # resize text and make it black
disease_severe_plot
ggsave(here("figures/disease_severe_plot.png"), plot = disease_severe_plot) # saves the figure




# Summary statistics for Involved body parts -----------------------------------------------------

# generate a summary data set with the percent of body sites recorded 
# calculate summary statistics for education level
Involved_body_part_summary <- Involved_body_part_clean %>%
  group_by(Body_region) %>% # group the data by each category in Body_region column
  summarize(mean_percent = mean(percent, na.rm = T),  # calculate the mean percent for each category in the Body_region column, and so on...
            lower_percent = min(percent, na.rm = T),
            upper_percent = max(percent, na.rm = T),
            n_studies = sum(!is.na(percent)))
write.csv(Involved_body_part_summary, here("data_summary/Involved_body_part_summary.csv"))


#number of studies included 
length(unique(Involved_body_part_clean$ID)) # 57

# number of patients included
involved_body_patients <- Involved_body_part_clean %>%
  select(ID:Total_N) %>%
  distinct() %>%
  ungroup() %>%
  summarize(sum = sum(Total_N))
# 17594


# specify body region order
Involved_body_part_clean$Body_region <- factor(Involved_body_part_clean$Body_region, c("Lower_Limbs", "Trunk", "Upper_Limbs", "Buttocks_Groin", "Face_Neck", "Multiple_sites", "Unspecified"))


# set color palette for body regions types
body_regions_colors <- tol(7)
names(body_regions_colors) <- c("Lower_Limbs", "Trunk", "Upper_Limbs", "Buttocks_Groin", "Face_Neck", "Multiple_sites", "Unspecified")
# Set color palette for colorblindness
palette_check(body_regions_colors, plot = TRUE)


# plot with boxplot and points overlaid for each body region
body_parts_boxplot <- ggplot(data = Involved_body_part_clean, aes(x = Body_region, y = percent, color = Body_region)) + # set the data source, and x and y columns, set color based on categorical variable
  geom_boxplot(show.legend = F) +  # generate the boxplot
  geom_point(position = position_jitter(width = 0.15), alpha = 0.6, show.legend = F) +  # generate the points, jitter makes it so that you can see points that are the same value a little easier
  scale_color_manual(values = body_regions_colors, name = "Body Regions", labels = c("Lower Limbs", "Trunk", "Upper Limbs", "Buttocks & Groin", "Face & Neck", "Multiple sites", "Unspecified")) +
  scale_x_discrete(labels = c("Lower Limbs", "Trunk", "Upper Limbs", "Buttocks & Groin", "Face & Neck", "Multiple sites", "Unspecified")) +
  labs(y = "Percent of Mycetoma patients (%)", x = "Body Regions") + # label axes
  theme_bw() + # change the theme, there are many options out there
  theme(axis.text.x = element_text(angle = 30, hjust = 1), axis.text = element_text(size=9, color = "black"), axis.title.y = element_text(size=11, color="black"), 
        axis.title.x = element_text(size=11, color="black")) # get x axis labels to be at an angle so they are legible, resize text and make it black
body_parts_boxplot  # view the plot
ggsave(here("figures/body_parts_boxplot.png"), plot = body_parts_boxplot, dpi = 300, width = 5, height = 4)  # save the file to the figures folder






# Summary statistics for Outcome ----------------------------------------------------------------

# generate a summary data set with the mean, median, sd, and min and max values for each occupation
Outcome_summary <- Outcome_clean %>%
  group_by(Outcome) %>%
  summarize(mean_percent = mean(percent, na.rm = T),
            lower_percent = min(percent, na.rm = T),
            upper_percent = max(percent, na.rm = T),
            n_studies = sum(!is.na(percent)))
write.csv(Outcome_summary, here("data_summary/Outcome_summary.csv"))


#number of studies included 
length(unique(Outcome_clean$ID))  # 28

# number of patients included
outcome_patients <- Outcome_clean %>%
  select(ID:Total_N) %>%
  distinct() %>%
  summarize(sum = sum(Total_N))
# 10912



# re-order levels
Outcome_clean$Outcome <- factor(Outcome_clean$Outcome, levels = c("Cured", "Remission", "Improved", "Amputation", "Recurrence", "Not_Improved", "Lost_to_Followup", "Not_reported"))

# set color palette for outcomes
outcomes_colors <- tol(8)
names(outcomes_colors) <- c("Cured", "Remission", "Improved", "Amputation", "Recurrence", "Not_Improved", "Lost_to_Followup", "Not_reported")
# Set color palette for colorblindness
palette_check(outcomes_colors, plot = TRUE)


# plot with boxplot and points overlaid for each outcome
outcome_boxplot <- ggplot(data = Outcome_clean, aes(x = Outcome, y = percent, color = Outcome)) + # set the data source, and x and y columns, set color based on categorical variable
  geom_boxplot(show.legend = F) +  # generate the boxplot
  geom_point(position = position_jitter(width = 0.15), alpha = 0.6, show.legend = F) +  # generate the points, jitter makes it so that you can see points that are the same value a little easier
  labs(y = "Percent of Mycetoma patients (%)", x = "Disease Outcomes") + # label axes
  scale_color_manual(values = outcomes_colors, name = "Outcomes", labels =  c("Cured", "Remission", "Improved", "Amputation", "Recurrence", "Not improved", "Lost to followup", "Not reported")) +
  scale_x_discrete(labels = c("Cured", "Remission", "Improved", "Amputation", "Recurrence", "Not improved", "Lost to followup", "Not reported")) +
  theme_bw() + # change the theme, there are many options out there
  theme(axis.text.x = element_text(angle = 30, hjust = 1), axis.text = element_text(size=9, color = "black"), axis.title.y = element_text(size=11, color="black"), 
        axis.title.x = element_text(size=11, color="black")) # get x axis labels to be at an angle so they are legible, resize text and make it black
outcome_boxplot  # view the plot
ggsave(here("figures/outcome_boxplot.png"), plot = outcome_boxplot, dpi = 300, width = 5, height = 4)  # save the file to the figures folder





# Summary statistics for Treatment Received ----------------------------------------------------------------

# re-order the Treatment categories from greatest to smallest medians
Treatment_received_clean <- Treatment_received_clean %>%
  mutate(Treatment_classification = factor(Treatment_classification, levels = c("Pharmaceutical", "Surgical", "Non-traditional", "Not reported")),
         Treatment_type = factor(Treatment_type, levels = c("Pharmaceutical_treatment", "Surgical_debridement", "Surgical_excision", "Surgical_amputation",     
                                                            "Surgical_resection", "Religious", "Herbal_traditional", "Not_reported")))


# generate a summary data set with the mean, median, sd, and min and max values for each Treatment type
Treatment_summary <- Treatment_received_clean %>%
  group_by(Treatment_classification, Treatment_type) %>%
  summarize(mean_percent = mean(percent, na.rm = T),
            lower_percent = min(percent, na.rm = T),
            upper_percent = max(percent, na.rm = T),
            n_studies = sum(!is.na(percent)))
write.csv(Treatment_summary, here("data_summary/Treatment_summary.csv"))


#number of studies included 
length(unique(Treatment_received_clean$ID))  # 30

# number of patients included
treatment_patients <- Treatment_received_clean %>%
  select(ID:Total_N) %>%
  distinct() %>%
  summarize(sum = sum(Total_N))
# 9058


# set color palette for outcomes
treatments_colors <- tol(4)
names(treatments_colors) <- c("Pharmaceutical", "Surgical", "Non-traditional", "Not reported")
# Set color palette for colorblindness
palette_check(treatments_colors, plot = TRUE)


# example boxplot figure code
treatment_boxplot <- ggplot(data = Treatment_received_clean, aes(x = Treatment_type, y = percent, color = Treatment_classification)) + # set the data source, and x and y columns, set color based on categorical variable
  geom_boxplot() +
  geom_point(pch = 1, position = position_jitter(width = 0.1)) +  # generate the points, jitter makes it so that you can see points that are the same value a little easier
  scale_color_manual(values = treatments_colors, name = "Treatment", labels = c("Pharmaceutical", "Surgical", "Non-traditional", "Not reported")) +
  scale_x_discrete(labels = c("Pharmaceutical", "Surgical debridement", "Surgical excision", "Surgical amputation", "Surgical resection",      
                              "Religious", "Herbal traditional", "Not reported")) +
  labs(x = "Treatment types", y = "Percent of Mycetoma patients (%)") + # label axes
  theme_bw() + # change the theme, there are many options out there
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # get x axis labels to be at an angle so they are legible 
treatment_boxplot  # view the plot
ggsave(here("figures/treatment_boxplot.png"), plot = treatment_boxplot)  # save the file to the figures folder




# combining figure panels for final manuscript figures ---------------------------------

# Figure 3 in manuscript: Sociodemographic characteristics
Figure3 <- ggarrange(age_plot, sex_plot, occupation_boxplot, labels = "auto",
                     ncol = 3, nrow = 1, widths = c(1,2,3), heights = c(3,3,4), legend = "right", common.legend = F)
Figure3
ggsave("figures/manuscript_figs/Fig3.tiff", plot = Figure3, dpi = 600, width = 10, height = 4, units = "in", compression="lzw")



# Figure 4 in manuscript: Clinical characteristics
Figure4 <- ggarrange(body_parts_boxplot, outcome_boxplot, labels = "auto",
                     ncol = 2, nrow = 1, widths = c(3,3), heights = c(4,4), legend = "right", common.legend = F)
Figure4
ggsave("figures/manuscript_figs/Fig4.tiff", plot = Figure4, dpi = 600, width = 8, height = 4, units = "in", compression="lzw")


