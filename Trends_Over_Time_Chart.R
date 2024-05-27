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
  filter(!is.na(total_prison_pop), year > 2000) %>%
  group_by(state, year) %>%
  summarise(total_prison_pop = sum(total_prison_pop, na.rm = TRUE)) %>%
  ungroup()

# Calculate summary statistics
current_year <- max(plot_data$year)

# Average value of the variable across all states in the current year
avg_current_year <- plot_data %>%
  filter(year == current_year) %>%
  summarise(average_prison_pop = mean(total_prison_pop, na.rm = TRUE))
print(avg_current_year)

# State with the highest value in the current year
highest_current_year <- plot_data %>%
  filter(year == current_year) %>%
  top_n(1, total_prison_pop)
print(highest_current_year)

# State with the lowest value in the current year
lowest_current_year <- plot_data %>%
  filter(year == current_year) %>%
  top_n(-1, total_prison_pop)
print(lowest_current_year)

# Change in the variable over the last N years (e.g., 10 years)
n_years <- 10
start_year <- current_year - n_years

change_over_years <- plot_data %>%
  filter(year %in% c(start_year, current_year)) %>%
  group_by(state) %>%
  reframe(change = total_prison_pop[year == current_year] - total_prison_pop[year == start_year])
print(change_over_years)

# Calculate the overall change
total_change_over_years <- change_over_years %>%
  summarise(total_change = sum(change, na.rm = TRUE))
print(total_change_over_years)

# Extract summary values
avg_value <- avg_current_year$average_prison_pop
highest_state <- highest_current_year$state
highest_value <- highest_current_year$total_prison_pop
lowest_state <- lowest_current_year$state
lowest_value <- lowest_current_year$total_prison_pop
total_change <- total_change_over_years$total_change

# Create a grouped bar chart using ggplot2
prison_plot <- ggplot(plot_data) +
  geom_col(
    mapping = aes(x = year, y = total_prison_pop, fill = state),
    position = "dodge"
  )

# Create a grouped bar chart using ggplot2
prison_plot <- ggplot(plot_data) +
  geom_col(
    mapping = aes(x = year, y = total_prison_pop, fill = state),
    position = "dodge"
  )

# Make the plot interactive using ggplotly
ggplotly(prison_plot)

# Alternatively, create the same plot using plotly directly
p <- plot_ly(
  data = plot_data, 
  x = ~year, 
  y = ~total_prison_pop, 
  color = ~state, 
  type="bar"
)

# Add layout to make the plot more presentable
p <- p %>% layout(
  title = "Total Prison Population in US States (2001-2016)",
  xaxis = list(title = "Years"),
  yaxis = list(title = "Total Prison Population")
)

