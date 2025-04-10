# Global Maps of Causative Organisms

# Getting set up --------------------------------------------------------------
# install libraries (only needs to be run the first time you use this code)
install.packages("tidyverse")
install.packages("here")
install.packages("ggplot2")
install.packages("maps")
install.packages("scatterpie")
install.packages("ggnewscale")
install.packages("rcartocolor")
install.packages("pals")
install.packages("colorblindcheck")
install.packages(c("cowplot", "googleway", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))


# load libraries (run this each time you open R)
library(tidyverse)
library(here)
library(ggplot2)
library(maps)
library(scatterpie)
library(ggnewscale)
library(rcartocolor)
library(pals)
library(colorblindcheck)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# load in the clean data (need to run the clean data code first) -------------------
Causative_Organisms_Taxonomy_clean <- read.csv(here("data_clean/Causative_Organisms_Taxonomy_clean.csv"), header = T, stringsAsFactors = T)

#remove first column with repeated column IDs
Causative_Organisms_Taxonomy_clean <- select(Causative_Organisms_Taxonomy_clean, ID:Etiology)

# check that all variables were read in correctly
str(Causative_Organisms_Taxonomy_clean)


unique(Causative_Organisms_Taxonomy_clean$Country_clean)
length(unique(Causative_Organisms_Taxonomy_clean$ID)) # 58 studies included

# May need to exclude the study that only indicates West Africa rather than individual countries, unless we group by region

# Table S2
# get summmed totals of mycetoma patients per country
mycetoma_pt_totals_per_country <- Causative_Organisms_Taxonomy_clean %>%
  select(ID:Total_N) %>%
  distinct() %>%
  group_by(Country_clean) %>%
  summarize(Total_N = sum(Total_N, na.rm = T))
write.csv(mycetoma_pt_totals_per_country, here("data_summary/Patient_totals_per_country.csv"))

# total patients included in data set
sum(mycetoma_pt_totals_per_country$Total_N)


# # Table S3
# get summed totals of number of patients per causative organism and per species
mycetoma_pt_totals_spp <- Causative_Organisms_Taxonomy_clean %>%
  group_by(Etiology, Genus_name, Species_name) %>%
  summarize(Organism_n = sum(Organism_n, na.rm = T)) %>%
  filter(Organism_n > 0)
write.csv(mycetoma_pt_totals_spp, here("data_summary/Patient_totals_per_spp.csv"))

# get summed totals of number of patients per causative organism and per genus
mycetoma_pt_totals_genus <- Causative_Organisms_Taxonomy_clean %>%
  group_by(Etiology, Genus_name) %>%
  summarize(Organism_n = sum(Organism_n, na.rm = T)) %>%
  filter(Organism_n > 0)
write.csv(mycetoma_pt_totals_genus, here("data_summary/Patient_totals_per_genus.csv"))



# get total N of number of patients per causative organism species and country
country_spp_totals <- Causative_Organisms_Taxonomy_clean %>%
  group_by(Country_clean, Taxonomic_group, Etiology, Species_name) %>%
  summarize(Total_N = sum(Total_N, na.rm = T),
            Organism_n = sum(Organism_n, na.rm = T)) %>%
  mutate(prop = Organism_n/Total_N)

# get total N of number of patients per causative organism genus and country
country_genus_totals <- Causative_Organisms_Taxonomy_clean %>%
  mutate(Genus_name = if_else(Genus_name == "Eumycetoma", "Unidentified_Eumycetoma", Genus_name)) %>%
  filter(Genus_name %in% genus_groups) %>%
  group_by(Country_clean, Taxonomic_group, Etiology, Genus_name) %>%
  summarize(Organism_n = sum(Organism_n, na.rm = T)) %>%
  full_join(.,mycetoma_pt_toals_per_country) %>%
  mutate(prop = Organism_n/Total_N)
write.csv(country_genus_totals, here("data_summary/Patient_totals_per_genus_and_country.csv"))

# pivot data frame wide to work with the scatterpie package
country_genus_totals_n <- country_genus_totals %>%
  pivot_wider(id_cols = Country_clean, names_from = Genus_name, values_from = c(Organism_n)) %>%
  ungroup() %>%
  mutate(diversity = rowSums(across(Actinomadura:Xenoacremonium) > 0))



# get total N of number of patients per causative organism etiology and country
country_etiology_totals <- Causative_Organisms_Taxonomy_clean %>%
  group_by(Country_clean, Taxonomic_group, Etiology) %>%
  summarize(Organism_n = sum(Organism_n, na.rm = T)) %>%
  full_join(.,mycetoma_pt_toals_per_country) %>%
  mutate(prop = Organism_n/Total_N)
write.csv(country_etiology_totals, here("data_summary/Patient_totals_per_etiology_and_country.csv"))

# pivot data frame wide to work with the scatterpie package
country_etiology_totals_n <- country_etiology_totals %>%
  pivot_wider(id_cols = Country_clean, names_from = Etiology, values_from = c(Organism_n)) %>%
  ungroup()





# world map figure ---------------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

world_new <- full_join(world, mycetoma_pt_toals_per_country, by =  c("name" = "Country_clean"))


# plot of the total reported mycetoma patients per country
mycetoma_per_country <- ggplot(data = world_new) +
  geom_sf(aes(fill = Total_N)) +
  scale_fill_viridis_c(option = "plasma", trans = "log", breaks = c(15, 150, 1500, 15000), name = "Log Mycetoma Patients") +
  labs(title = "Mycetoma patients reported per country")+
  theme_bw()
mycetoma_per_country
ggsave(here("figures/mycetoma_per_country.png"), plot = mycetoma_per_country, width = 7.5, height = 3)



world_new <- full_join(world_new, country_etiology_totals_n, by =  c("name" = "Country_clean")) %>%
  mutate(mycetoma_detected = if_else(is.na(Total_N), "Not reported", "Present"))


# plot of the total reported Actinomycetoma mycetoma patients per country
Actinomycetoma_mycetoma_per_country <- ggplot(data = world_new) +
  geom_sf(aes(fill = Actinomycetoma)) +
  scale_fill_viridis_c(option = "plasma", trans = "log", breaks = c(1,10,100,1000,5000), name = "Log Mycetoma Patients") +
  labs(title = "Mycetoma patients reported with Actinomycetoma etiology per country")+
  theme_bw()
Actinomycetoma_mycetoma_per_country
ggsave(here("figures/Actinomycetoma_mycetoma_per_country.png"), plot = Actinomycetoma_mycetoma_per_country, width = 7.5, height = 3)


# plot of the total reported Eumycetoma mycetoma patients per country
Eumycetoma_mycetoma_per_country <- ggplot(data = world_new) +
  geom_sf(aes(fill = Eumycetoma)) +
  scale_fill_viridis_c(option = "plasma", trans = "log", breaks = c(1,10,100,1000,10000), name = "Log Mycetoma Patients") +
  labs(title = "Mycetoma patients reported with Eumycetoma etiology per country")+
  theme_bw()
Eumycetoma_mycetoma_per_country
ggsave(here("figures/Eumycetoma_mycetoma_per_country.png"), plot = Eumycetoma_mycetoma_per_country, width = 7.5, height = 3)



## Maps with pie charts for Etiology

etiology_groups <- colnames(country_etiology_totals_n[ , c(2:5)])
world_selected <- as.data.frame(world_new) %>%
  filter(!is.na(Total_N), !is.na(label_x)) %>%
  select(name, label_x, label_y, Total_N:Unidentified)

etiology_colors <- carto_pal(4, "Safe")
etiology_colors <- c("#88CCEE", "#CC6677", "#DDCC77", "#332288")

#check that the palette is colorblind friendly - yes!
palette_check(etiology_colors, plot = TRUE)


# plot of the pie charts for Eumycetoma and Actinomycetoma mycetoma patients per country - scaled by N patients, with detected marked
detected_mycetoma_per_country <- ggplot(data = world_new) +
  geom_sf(aes(fill = mycetoma_detected)) +
  scale_fill_manual(values = c("gray95", "gray40"), name = "Mycetoma Detection") +
  theme_bw()

### Figure 5 in the manuscript text
pie_scale_mycetoma_per_country_etiology <- detected_mycetoma_per_country +
  ggnewscale::new_scale_fill() +
  geom_scatterpie(aes(x = label_x, y = label_y, group = name, r = log(Total_N)), data = world_selected, cols = etiology_groups, alpha = 0.7, linewidth = 0.1, legend_name = "Aetiology") +
  geom_scatterpie_legend(log(world_selected$Total_N), x=-160, y=-55, n = 4, breaks = c(2.302585, 4.60517, 6.907755, 9.21034), labeller = function(x) round(exp(x)), size = 2) +
  scale_fill_manual(values = etiology_colors, labels = c("Actinomycetoma", "Eumycetoma", "Not reported", "Unidentified")) +
  labs(x = NULL, y = NULL, title = "Proportion of Mycetoma patients reported by aetiology per country")+
  theme_bw()
pie_scale_mycetoma_per_country_etiology
#ggsave(here("figures/pie_scale etiology_mycetoma_per_country_detected.png"), plot = pie_scale_mycetoma_per_country_etiology, width = 11, height = 5)
ggsave(here("figures/manuscript_figs/Fig5.tiff"), plot = pie_scale_mycetoma_per_country_etiology, width = 11, height = 5, dpi = 600, compression = "lzw")




### Make a map for each genus

length(unique(country_genus_totals$Genus_name))


genus_groups <- unique(mycetoma_pt_toals_genus$Genus_name)
# remove the unidentified and not reported categories from the list of genera
genus_groups <- c("Actinomadura", "Nocardia", "Streptomyces", "Acremonium", "Aspergillus",           
                  "Cladophialophora", "Cladosporium",  "Exophiala", "Falcifomispora", "Fusarium",            
                  "Madurella", "Medicopsis", "Microsporum", "Neoscytalidium", "Nigrograna",        
                  "Penicillium", "Phaeoacremonium", "Sarocladium", "Scedosporium", "Trematosphaeria",        
                  "Trichophyton", "Xenoacremonium")
length(genus_groups)

# get 36 color palette for each genus
#genus_colors <- rev(watlington(25))
genus_colors <- c("#0075DC", "#88CCEE", "#003380", "#005C31", "#FFFF80",  "#740AFF", "#81C57A", "#AA4499",  "#117733", "#5EF1F2", "#CC6670", "#FFA8BB", 
                  "#FFA405", "#C20088", "#94FFB2", "#814A19", "#FFCE85", "#000000", "#661100", "#FF0010", "#4C005C", "#882255")
names(genus_colors) <- genus_groups

#check that the palette is colorblind friendly - yes!
palette_check(genus_colors, plot = TRUE)
palette_dist(genus_colors, cvd = "deu")
palette_check(watlington(25), plot = TRUE)
palette_check(tol(12), plot = TRUE)
palette_check(stepped(22), plot = TRUE)

world_genus <- full_join(world, mycetoma_pt_toals_per_country, by =  c("name" = "Country_clean")) %>%
  full_join(., country_genus_totals_n, by =  c("name" = "Country_clean")) %>%
  mutate(mycetoma_detected = if_else(is.na(Total_N), "Not reported", "Present"))


world_genus_selected <- as.data.frame(world_genus) %>%
  filter(!is.na(Total_N), !is.na(label_x)) %>%
  select(name, label_x, label_y, Total_N, Actinomadura:Trichophyton, Xenoacremonium) # Total_N:Unidentified

### Figure 6 in the manuscript text
pie_scale_genus_mycetoma_per_country <- detected_mycetoma_per_country +
  ggnewscale::new_scale_fill() +
  geom_scatterpie(aes(x = label_x, y = label_y, group = name, r = log(Total_N)), data = world_genus_selected, cols = genus_groups, alpha = 0.7, linewidth = 0.05, legend_name = "Genus") +
  geom_scatterpie_legend(log(world_genus_selected$Total_N), x=-160, y=-55, n = 4, breaks = c(2.302585, 4.60517, 6.907755, 9.21034), labeller = function(x) round(exp(x)), size = 2) +
  scale_fill_manual(values = genus_colors) +
  labs(x = NULL, y = NULL, title = "Proportion of Mycetoma patients identified to genus per country")+
  theme_bw()
pie_scale_genus_mycetoma_per_country
#ggsave(here("figures/pie_scale_genus_mycetoma_per_country_detected.png"), plot = pie_scale_genus_mycetoma_per_country, width = 11, height = 5)
ggsave(here("figures/manuscript_figs/Fig6.tiff"), plot = pie_scale_genus_mycetoma_per_country, width = 11, height = 5, dpi = 600, compression = "lzw")

# Note: Ethiopia and UAE do not show up in this figure because they do not have any isolate identified beyond 
# actinomycetoma and eumyectoma eitology (i.e. no genera recorded)

