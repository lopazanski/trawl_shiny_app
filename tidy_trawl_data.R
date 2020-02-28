# Tidy trawl data
library(tidyverse)
library(janitor)

trawl_test <- read_csv("trawl_shiny_master_mar2018.csv") %>% 
  clean_names()

trawl <- trawl_test %>% 
  select(tow_id, date, project, site, habitat:distance_m, depth_m:sal_ppt, num_aca_bah:wt_jellyfish) %>% 
  pivot_longer(num_aca_bah:wt_jellyfish,
               names_to = "names",
               values_to = "values") %>%
  separate(names,
           into = c("type", "genus", "species"),
           sep = "_") %>%
  unite(col = "species_name",
        genus:species,
        sep = "_",
        na.rm = TRUE) %>%
  mutate(sp_id = rep(1:(length(values)/2), each = 2))  # create column identifying each individual species iteration (important for next step) 

# Create a widened dataframe so that number and weight have their own column
trawl_wide <- trawl %>%
  pivot_wider(id_cols = sp_id, names_from = type, values_from = values) 

# Filter the id dataset so each ID is only listed once
trawl_filter <- trawl %>%
  filter(type == "num")

# Join the filtered ID dataset with the widened subset
# For pres_abs column: if number is zero = absent (0), if number is nonzero = present
trawl_tidy <- full_join(trawl_filter, trawl_wide, by = "sp_id") %>%
  select(-sp_id, -values, -type) %>%
  mutate(pres_abs = ifelse(num == 0, 0, 1))
