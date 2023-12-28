# This file constructs school district shapefiles. Codes below is embedded in `01a`
# chunk `r construct school district files`. Consider taking off from `01a` and 
# add this as a separate file.

#identify relevant fips codes
my_state_fip <- 
  fips_codes %>% 
  filter(state == my_state_abbr) %>% 
  pull(state_code) %>% 
  unique()

# Declare which vintage of Census TIGER shapefiles should be pulled
tiger_year <- base_year

# Download school district file from Census using tigris package
el <- school_districts(state = {my_state_fip}, type = 'elementary', year = tiger_year)
sc <- school_districts(state = {my_state_fip}, type = 'secondary',  year = tiger_year)
un <- school_districts(state = {my_state_fip}, type = 'unified',    year = tiger_year)

# Clean data
sd <- 
  bind_rows(el, sc, un)
rm(el, sc, un)
sd <- sd %>%
  rename(school = NAME)

# Display them
ggplot() + 
  geom_sf(data = sd,
          fill = NA) +
  theme_void()

# Check Size
object.size(sd) %>% format(unit = "Mb")
if (FALSE) {
  sd <- st_simplify(sd, dTolerance = 500) #dTolerance needs to be decimal degrees to run this
  object.size(sd) %>% format(unit = "Mb")
}

# Write an output shapefile
st_write(sd, glue("{input_path}{my_state_abbr}_combined_schooldistricts.shp"),
         delete_layer = TRUE)




