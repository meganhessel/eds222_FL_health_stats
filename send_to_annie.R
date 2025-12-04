Ethics 
- rainfall everywhere equally 
- flooding is not equally distributed 
- duration of flood is not equally 
- extend and duration 
- lack of infastructure 
- poor drainage 
- flood control investment 
- link to sociodemiogranic information 


library(pscl)
library(glmmTMB)

# Load in Data 
flooding_FL <- read_csv(here::here('data', 'hurrican_ian_flooding.csv')) %>% 
  filter(`State/Province`== "FL") %>% 
  select(City, `State/Province`, `Latitude (°)`, `Longitude (°)`, `Totals (inches)`) %>% 
  clean_names()

fl_cond <- read_sf(here::here('data', 'ejscreen', 'EJSCREEN_2023.gdb')) %>% 
  filter(ST_ABBREV == 'FL') %>% 
  clean_names()

acs_2022 <- read_csv(here::here('data', 'ACSDP5Y2022.DP05-Data.csv'))


hurr_ian <- read_sf(here::here('data', 'Hurricane_Ian_Track', 'Hurricane_Ian_Track.shp')) 

  

# Look at the point with ggplot - Looks like FL  
ggplot(flooding_FL, 
       aes(x = longitude,  # PROBLEM: looks like FL when long is the x ?? 
           y = latitude)) + 
  geom_point() 

# Change flooding data to sf  
flooding_FL_sf <- st_as_sf(flooding_FL, coords = c("longitude", "latitude"), crs = 'EPSG:4326')

# Check columns 
colnames(flooding_FL_sf)

# Make sure all points are valid 
which(!st_is_valid(flooding_FL_sf))
flooding_FL_sf <- st_make_valid(flooding_FL_sf)


# Ploting with tmap of low income and flooding 

tm_shape(fl_cond) + 
  tm_polygons(fill = 'lowincpct') +
  tm_shape(flooding_FL_sf) + 
  tm_dots(fill = 'darkgreen', 
          col = 'black') 

map1 <- tm_shape(fl_cond) + 
  tm_polygons(fill = 'lowincpct', 
              fill.legend = tm_legend(orientation = 'landscape'))

map2 <- tm_shape(flooding_FL_sf) +
  tm_dots(fill = 'darkgreen', 
          col = 'black') + 
  tm_basemap("OpenTopoMap") 

map2
tmap_arrange(map1, map2)


tm_shape(join_df) %>% 
  tm_polygons()


# Create a buffer (hurrican diameter 240 miles)
ian_buffer <- st_buffer(hurr_ian, dist = units::set_units(120, "miles"))

tm_shape(hurr_ian) + 
  tm_lines() + 
  tm_shape(ian_buffer) + 
  tm_polygons() +
  tm_basemap("OpenTopoMap") 


# Buffer and point intersect 

intersect <- st_intersects(flooding_FL_sf, ian_buffer) #sparse = FALSE)
len <- data.frame(lengths(intersect) > 0) # See which rows did and didn't intersect 

# Combine dataframes 
flooding_FL_sf <- cbind(flooding_FL_sf, len) %>% 
  rename('hur_path' = `lengths.intersect....0` ) 
# Change to 1 and 0s 
flooding_FL_sf$hur_path <- as.numeric(flooding_FL_sf$hur_path)  




# Check crs 
st_crs(flooding_FL_sf) == st_crs(fl_cond)

flooding_FL_sf <- st_transform(flooding_FL_sf, crs = st_crs(fl_cond))

# Full merge 
#zonal stats: zonal(x = flooding_FL_sf, z = vect(fl_cond))
join_df <- st_join(flooding_FL_sf, fl_cond, join = st_within)
#drop_na(state_name)






# Ggplot 
ggplot(join_df, 
       aes(totals_inches, p_peopcolorpct)) + 
  geom_point() + 
  geom_smooth()


subset <- join_df %>% 
  filter(cnty_name == c('Hillsborough County', 
                        'DeSoto County', 
                        'Sarasota County', 
                        'Charlotte County',
                        '	Pinellas County',
                        'Pasco County',
                        'Citrus County',
                        'Polk County', 
                        'Hardee County', 
                        'Highlands County',
                        'Sumter County', 
                        'Lake County', 
                        'Orange County', 
                        'Osceola County', 
                        'Okeechobee County', 
                        'Seminole County', 
                        'Volusia County', 
                        'Brevard County', 
                        'Indian River', 
                        'St. Lucie County', 
                        'Martin County'
                        ))








# Looking at where the 0s are 
join_df %>% 
  filter(totals_inches == 0) %>% 
  tm_shape() +
  tm_dots() +
  tm_basemap("OpenTopoMap") 




# Filter for eye of hurricane 
ggplot(subset, 
       aes(totals_inches, p_peopcolorpct)) + 
  geom_point() + 
  geom_smooth()

# people of color 
ggplot(join_df, 
       aes(totals_inches, p_peopcolorpct)) + 
  geom_point() + 
  geom_smooth()

# low income 
ggplot(join_df, 
       aes(totals_inches, p_lowincpct)) + 
  geom_point() + 
  geom_smooth()

# Deomgrahic index 
ggplot(join_df, 
       aes(totals_inches, demogidx_2)) + 
  geom_point() + 
  geom_smooth()

class(join_df$region)




# modeling 
model <- glm(totals_inches ~ p_peopcolorpct * p_lowincpct + demogidx_2 + cnty_name, 
             data = join_df, 
             family = binomial(link = 'logit')
             )

summary(model)



hurdle(totals_inches ~ p_peopcolorpct * p_lowincpct + demogidx_2 + cnty_name, 
       data = join_df, 
       dist = ,
       zero.dist = 'binomial', 
       link = 'logit')



model <- glmmTMB(totals_inches ~ p_peopcolorpct * p_lowincpct + hur_path + acstothu + cnty_name, 
        data = join_df, 
        ziformula = ~ hur_path
        )



summary(model)



