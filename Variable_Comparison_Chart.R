library("dplyr")
library("ggplot2")
library("plotly")

# Load the data
prison_pop <- read.csv(
  file = "https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true",
  stringsAsFactors = FALSE
)

# Filter and select the data
plot_data <- prison_pop %>%
  filter(!is.na(total_prison_pop), !is.na(total_pop), year > 2000) %>%
  group_by(state, year) %>%
  summarise(
    total_prison_pop = sum(total_prison_pop, na.rm = TRUE),
    total_pop = sum(total_pop, na.rm = TRUE),
    .groups = "drop"
  )

# Create a scatter plot using ggplot2
comparison_plot <- ggplot(plot_data) +
  geom_point(mapping = aes(x = total_pop, y = total_prison_pop, color = state)) +
  labs(
    title = "Total Prison Population vs Total Population in US States (2001-2016)",
    x = "Total Population",
    y = "Total Prison Population",
    color = "State"
  ) +
  theme_minimal()

# Make the plot interactive using ggplotly
ggplotly(comparison_plot)
