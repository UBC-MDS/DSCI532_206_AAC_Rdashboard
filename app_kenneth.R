# This version uses text styles, markdown, and ggplotly

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)
library(lubridate)

app <- Dash$new()

df <- as_tibble(read_csv("data/aac_data_cleaned.csv"))

# General wrangling
df <- df %>% mutate(age_years = `age_upon_intake_(days)`/365)

# Plot 4
make_plot4 <- function(year_range = list(2013, 2017), animal_type_choice = "All"){
  
  # Title strings
  title_string = ifelse(animal_type_choice %in% "All", 
                            "All Animals", 
                            ifelse(animal_type_choice %in% "Others", 
                                "Others", 
                                paste0(animal_type_choice,"s")))
  
  df4 <- df %>% 
    filter(intake_year >= year_range[1] & intake_year <= year_range[2])

  # Filtering condition
  if (animal_type_choice %in% "All") {
    df4 <- df4 
  } else {
    animal_type_quo = enquo(animal_type_choice)
    df4 <- df4 %>% filter(animal_type %in% !!animal_type_quo)
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

# Plot 5
make_plot5 <-function(year_range = list(2013, 2017), intake_cond = "All"){

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
        scale_y_continuous(trans = "log10",
                          breaks = c(0.5, 1, 10, 50, 100, 200, 400, 600, 1000), 
                            labels = scales::label_comma(accuracy = 0.1))+
        labs(title = paste0("Days spent in shelter for ",intake_cond," Animals"),
                y = "Days", 
                x = "") +
        theme(plot.title = element_text(hjust = 0.5))

    ggplotly(p5) %>%  
      config(modeBarButtonsToRemove = c("zoomIn2d", 
                                        "zoomOut2d",
                                        "autoScale2d",
                                        "zoom2d"))
  
}
yearMarks <- map(unique(df$intake_year), as.character)
names(yearMarks) <- unique(df$intake_year)
yearSlider <- dccRangeSlider(
  id = "year",
  marks = yearMarks,
  min = 2013,
  max = 2018,
  step = 1,
  value = list(2014, 2016)
)

plot4_radio <- dccRadioItems(
    id = "plot4-radio",
    options = list(list(label = "All", value = "All"),
                    list(label = "Cats", value = "Cat"),
                    list(label = "Dogs", value = "Dog"),
                    list(label = "Birds", value = "Bird"),
                    list(label = "Others", value = "Others")),
    value = 'All'
)

plot5_droplistkey <- tibble(label = c('All','Healthy','Injured','Sick','Feral','Pregnant','Nursing','Other'),
                   value = c('All','Healthy','Injured','Sick','Feral','Pregnant','Nursing','Other'))

plot5_drop <- dccDropdown(
    id = "plot5-drop",
    options=  map(
    1:nrow(plot5_droplistkey), function(i){
      list(label=plot5_droplistkey$label[i], value=plot5_droplistkey$value[i])
    }),
    value = 'All'
)

plot4 <- dccGraph(
  id = 'plot4',
  figure=make_plot4() # gets initial data using argument defaults
)

plot5 <- dccGraph(
  id = 'plot5',
  figure=make_plot5() # gets initial data using argument defaults
)

##################################################


app$layout(
  htmlDiv(
    list(
      htmlH1('Gapminder Dash Demo'),
      htmlH2('Looking at country data interactively'),
      #selection components
      htmlLabel('Select a year range:'),
      yearSlider,
      htmlIframe(height=45, width=10, style=list(borderWidth = 0)), #space
      htmlDiv(list(
        htmlLabel('Select buttons:'),
        plot4_radio
      ), style = list(display = "inline", width = "250")),
      htmlDiv(list(
        htmlLabel('plot4'),
        plot4
      ), style = list(display = "inline", width = "250px")),
      htmlIframe(height=15, width=10, style=list(borderWidth = 0)), #space
      htmlLabel('Select intake conditions:'),
      plot5_drop,
      htmlLabel('plot5'),
      htmlIframe(height=250, width=10, style=list(borderWidth = 0)), #space
      plot5,
      #graph and table
      htmlIframe(height=20, width=10, style=list(borderWidth = 0)), #space
      dccMarkdown("[Data Source](https://cran.r-project.org/web/packages/gapminder/README.html)")
    )
  )
)
############################################


# Adding callbacks for interactivity
app$callback(
  # update figure of gap-graph
  output=list(id = 'plot4', property='figure'),
  
  # based on values of year, continent, y-axis components
  # TODO: Update the IDs of the components (note: remember that order matters!!)
  params=list(input(id = 'year', property='value'),
              input(id = 'plot4-radio', property='value')),

  # this translates your list of params into function arguments
  function(year_range, animal_type_choice) {
    make_plot4(year_range, animal_type_choice)
  })

app$callback(
  # update figure of gap-graph
  output=list(id = 'plot5', property='figure'),
  
  # based on values of year, continent, y-axis components
  # TODO: Update the IDs of the components (note: remember that order matters!!)
  params=list(input(id = 'year', property='value'),
              input(id = 'plot5-drop', property='value')),

  # this translates your list of params into function arguments
  function(year_range, intake_cond) {
    make_plot5(year_range, intake_cond)
  })

app$run_server()
