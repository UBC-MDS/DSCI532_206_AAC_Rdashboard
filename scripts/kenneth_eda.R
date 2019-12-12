library(tidyverse)
library(plotly)
library(scales)

df <- as_tibble(read_csv("../data/aac_data_cleaned.csv"))

# General wrangling
df <- df %>% mutate(age_years = `age_upon_intake_(days)`/365)

# Plot 4
make_plot4 <- function(animal_type_choice = "All"){
  
  # Title strings
  title_string = ifelse(animal_type_choice %in% "All", "All Animals", paste0(animal_type_choice,"s"))
  
  # Filtering condition
  if (animal_type_choice %in% "All") {
    df4 <- df 
  } else {
    animal_type_quo = enquo(animal_type_choice)
    df4 <- df %>% filter(animal_type %in% !!animal_type_quo)
  }
  # Plotting
  df4 %>%
    ggplot(aes(x = age_years, y = stat(count)))+
    geom_histogram(color = "blue", fill = "blue", binwidth = 0.5) +
    labs(title = paste0("Age Distribution of ", title_string),
         x = "Intake Age (Year)",
         y = "Count") +
    theme(plot.title = element_text(hjust = 0.5))
  
}


# Plot 4
make_plotly4 <- function(animal_type_choice = "All"){
  
  # Title strings
  title_string = ifelse(animal_type_choice %in% "All", "All Animals", paste0(animal_type_choice,"s"))
  
  # Filtering condition
  if (animal_type_choice %in% "All") {
    df4 <- df 
  } else {
    animal_type_quo = enquo(animal_type_choice)
    df4 <- df %>% filter(animal_type %in% !!animal_type_quo)
  }
  # Plotting
  p4 <- df4 %>%
    ggplot(aes(x = age_years, y = stat(count)))+
    geom_histogram(color = "blue", fill = "blue", binwidth = 0.5) +
    labs(title = paste0("Age Distribution of ", title_string),
         x = "Intake Age (Year)",
         y = "Count") +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggplotly(p4) %>%  
    config(modeBarButtonsToRemove = c("lasso2d",
                                      "pan2d",
                                      "autoScale2d",
                                      "zoom2d"))
}

make_plotly4()

# Plot 5
make_plot5 <-function(intake_cond = "All"){

  # Filtering condition
  if (intake_cond %in% "All") {
    df5 <- df
  } else {
    intake_condition_quo  <- rlang::enquo(intake_cond)
    df5 <- df %>% filter(intake_condition %in% !!intake_condition_quo)
  }
  
  # Plotting
  df5 %>%
    ggplot(aes(x = factor(animal_type), y = total_time_in_shelter_days)) +
    geom_boxplot(outlier.alpha = 0.5, outlier.stroke = 0.1) +
    scale_y_continuous(breaks = c(0.5, 1, 10, 50, 100, 200, 400, 600, 1000), 
                       labels = scales::label_comma(accuracy = 0.1))+
    coord_trans(y = "log10") +
    labs(title = paste0("Days spent in shelter for ",intake_cond," animals"),
         y = "Days", 
         x = "") +
    theme(plot.title = element_text(hjust = 0.5))
  
}

# Sample test
make_plot4()

make_plot5()


# Plot 5
# Plot 5
# Plot 5
make_plotly5 <-function(year_range = list(2013, 2017), intake_cond = "All"){
  
  df5 <- df %>% 
    filter(intake_year > year_range[1] & intake_year < year_range[2])
  
  # Filtering condition
  if (intake_cond %in% "All") {
    df5 <- df5
  } else {
    intake_condition_quo  <- rlang::enquo(intake_cond)
    df5 <- df5 %>% filter(intake_condition %in% !!intake_condition_quo)
  }
  
  # Plotting
  p5 <- df5 %>%
    ggplot(aes(x = factor(animal_type), y = total_time_in_shelter_days)) +
    geom_boxplot(outlier.alpha = 0.5, outlier.stroke = 0.1) +
    scale_y_continuous(trans = "log10", breaks = c(0.5, 1, 10, 50, 100, 200, 400, 600, 1000), 
                       labels = scales::label_comma(accuracy = 0.1))+
    # coord_trans(y = "log10") +
    labs(title = paste0("Days spent in shelter for ",intake_cond," Animals"),
         y = "Days", 
         x = "") +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggplotly(p5,tooltip = c("total_time_in_shelter_days")) %>%  
    config(modeBarButtonsToRemove = c("zoomIn2d", 
                                      "zoomOut2d",
                                      "autoScale2d",
                                      "zoom2d"))
  
}
make_plotly5()
