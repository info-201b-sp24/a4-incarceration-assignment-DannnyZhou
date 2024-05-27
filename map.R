library("dplyr")
library("ggplot2")
library("tigris")
library("sf")
library("tmap")

# Load the data
prison_pop <- read.csv(
  file = "https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true",
  stringsAsFactors = FALSE
)

# Filter and summarize the data
map_data <- prison_pop %>%
  filter(!is.na(total_prison_pop), year > 2000) %>%
  group_by(state) %>%
  summarise(avg_prison_pop = mean(total_prison_pop, na.rm = TRUE), .groups = "drop")

# Load US states shapefile
us_states <- states(cb = TRUE)

# Merge map data with shapefile data
map_data <- merge(us_states, map_data, by.x = "STUSPS", by.y = "state")

# Create a map using tmap
tm_shape(map_data) +
  tm_polygons("avg_prison_pop", title = "Average Prison Population") +
  tm_layout(
    title = "Average Prison Population by State (2001-2016)",
    frame = FALSE, 
    legend.outside = TRUE
  )
