library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashTable)
library(ggplot2)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(plotly)
library(lubridate)


app <- Dash$new(external_stylesheets = "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css")


df <- read_csv("https://raw.githubusercontent.com/Keanna-K/DSCI532_206_AAC_Rdashboard/master/data/aac_data_cleaned.csv")

######### Data Wrangling ##########

# General wrangling
df <- df %>% mutate(age_years = `age_upon_intake_(days)`/365)

# Wrangling for plot1
# wrangle the income data counts
df1_in <- df %>% 
    group_by(intake_monthyear) %>%
    summarise('count' = n()) %>%
    mutate('type' = 'intake',
          'monthyear' = intake_monthyear) 
df1_in <- df1_in[-1]

# wrangle the outcome data counts
df1_out <- df %>% 
    group_by(outcome_monthyear) %>%
    summarise('count' = n()) %>%
    mutate('type' = 'outake',
          'monthyear' = outcome_monthyear)
df1_out <- df1_out[-1]

# join outcome and income counts 
df1 <- full_join(df1_in, df1_out)
df1['year'] <- year(df1[['monthyear']])
df1 <- df1[1:108,]

######### Create plot selectors ##########
# global year slider (get the years from the dataset)
yearMarks <- map(unique(df$intake_year), as.character)
names(yearMarks) <- unique(df$intake_year)
yearSlider <- dccRangeSlider(
  id = 'year-slider',
  marks = yearMarks,
  min = 2013,
  max = 2018,
  step= 1,
  value = list(2014, 2017)
)

# plot 2 and 3 dropdown selctors for animal type and month
animalKey <- tibble(label = c("All", "Dog", "Cat", "Bird","Other"),
                   value = c("All", "Dog", "Cat", "Bird", "Other"))


monthKey <- tibble(label = c("All", "Oct", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                             "Aug", "Sep", "Jan", "Nov", "Dec"),
                   value = seq(0,12))

monthDropdown <- dccDropdown(
  id = "month",
  options = map(
    1:nrow(monthKey), function(i){
      list(label=monthKey$label[i], value=monthKey$value[i])
    }),
  value = 0
)

animalDropdown <- dccDropdown(
  id = "animalType",
  options = map(
    1:nrow(animalKey), function(i){
      list(label=animalKey$label[i], value=animalKey$value[i])
    }),
  value = "All"
)

# plot 4 radio selector
plot4_radio <- dccRadioItems(
    id = "plot4-radio",
    options = list(list(label = "All", value = "All"),
                    list(label = "Cats", value = "Cat"),
                    list(label = "Dogs", value = "Dog"),
                    list(label = "Birds", value = "Bird"),
                    list(label = "Others", value = "Others")),
    value = 'All'
)

# plot 5 drop list
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

######### Create plotting functions ##########

# Plot 1
make_plot_1 <- function(years= c(2013, 2016)){

  #filter our data based on the year selections
  df1 <- df1 %>%
   filter(year >= years[1] & year <= years[2])
       

    # create line plot
    p <- df1 %>% ggplot() + 
        geom_line(aes(x=monthyear, y=count, color=type)) + 
        labs(title = 'Trends in Intakes & Outtakes of Shelter Animals',
                 x = "",
                 y = "Count") +
        expand_limits(y=0) +
        scale_x_datetime(date_labels = format('%Y %b'), date_breaks = "4 months") +
        theme(plot.title = element_text(size = 20, face = "bold",hjust = 0.5),
             axis.text.x = element_text(size = 12, angle = 45),
             axis.title.x = element_text(size = 20),
             axis.title.y = element_text(size = 20),
             axis.text.y = element_text(size = 14),
             panel.background = element_blank()) 

  ggplotly(p)
}

graph1 <- dccGraph(
  id = 'graph1',
  figure = make_plot_1()
)

# plot 2
make_graph_plot2 <- function(year = c(2014, 2015), 
                       animal = "All", month=0){
  
  
  # filtering conditions 
  data_intake <- df %>%
    filter(intake_year >= year[1] & intake_year <= year[2]) %>%
    group_by(intake_year,animal_type,intake_weekday,intake_month) %>%
    summarise(cnt = n()) 
  
  # update plot title based on animal type
  if (animal == "All"){
    data_intake_animal <- data_intake
    title = "Average Animal Intake by Week Day"
  }else {
    data_intake_animal <- data_intake %>%
      filter(animal_type == animal)
    title = paste0("Average ", animal, " Intake by Week Day")
  }
  
  if (month != 0){
    data_intake_animal <- data_intake_animal %>%
            filter(intake_month == month)
  }
  
  
  data_intake_animal <- data_intake_animal %>%
    group_by(intake_weekday) %>%
    summarise(count = round(mean(cnt),0))
  
  data_intake_animal$intake_weekday <- factor(data_intake_animal$intake_weekday,levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday", "Saturday"))
  
  
  p <- ggplot(data_intake_animal, aes(y=count, x=intake_weekday)) + 
    geom_bar(position="dodge", stat="identity", fill="dodgerblue3") +
    labs(title = title, x = "Week day") +
    theme(plot.title = element_text(size = 10, face = "bold",hjust = 0.5),
          axis.text.x = element_text(size = 8, angle = 45),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text.y = element_text(size = 8),
          panel.background = element_blank()) 
  
  ggplotly(p)
  
}

graph_2 <- dccGraph(
  id = 'graph2',
  figure=make_graph_plot2() 
)

# plot 3
make_graph_plot3 <- function(year = c(2014, 2015), 
                             animal = "All", month=0){
  
  
  # filtering conditions
  data_outtake <- df %>%
    filter(outtake_year >= year[1] & outtake_year <= year[2]) %>%
    group_by(outtake_year,animal_type,intake_weekday,intake_month) %>%
    summarise(cnt = n()) 
  
  # update plot title based on animal type
  if (animal == "All"){
    data_outtake_animal <- data_outtake
    title = "Average Animal Outtake by Week Day"
  }else {
    data_outtake_animal <- data_outtake %>%
      filter(animal_type == animal)
    title = paste0("Average ", animal, " Outtake by Week Day")
  }
  
  if (month != 0){
    data_outtake_animal <- data_outtake_animal %>%
      filter(intake_month == month)
  }
  
  
  data_outtake_animal <- data_outtake_animal %>%
    group_by(intake_weekday) %>%
    summarise(count = round(mean(cnt),0))
  
  data_outtake_animal$intake_weekday <- factor(data_outtake_animal$intake_weekday,levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday", "Saturday"))
  
  
  p <- ggplot(data_outtake_animal, aes(y=count, x=intake_weekday)) + 
    geom_bar(position="dodge", stat="identity", fill="dodgerblue3") +
    labs(title = title, x = "Week day") +
    theme(plot.title = element_text(size = 10, face = "bold",hjust = 0.5),
          axis.text.x = element_text(size = 8, angle = 45),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text.y = element_text(size = 8),
          panel.background = element_blank()) 
  
  ggplotly(p)
  
}

graph_3 <- dccGraph(
  id = 'graph3',
  figure=make_graph_plot3() 
)

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

plot4 <- dccGraph(
  id = 'plot4',
  figure=make_plot4() # gets initial data using argument defaults
)

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

plot5 <- dccGraph(
  id = 'plot5',
  figure=make_plot5() # gets initial data using argument defaults
)


############### APP LAYOUT BEGINS ##############

app$layout(
  htmlDiv(
      list(
      # Logo
      htmlImg(src='https://raw.githubusercontent.com/UBC-MDS/DSCI_532_L02_group206_ms1/master/img/aac_logo.jpg'), 
      # Main app title 
      htmlH1("Animals Sheltered at the Austin Animal Center"),
      grey_line <- htmlDiv(list(htmlHr(className = "greyline"))), 
      htmlIframe(height=15, width=10, style=list(borderWidth = 0)), 
      htmlLabel("Select Animal Intake Year Range:"), 
      yearSlider,
      htmlIframe(height=45, width=10, style=list(borderWidth = 0)), #space
      graph1,
      # plot 2 and 3 row
      htmlDiv(                    
        list(
          htmlDiv(list(graph_2, htmlBr()), class="col-4"),
          htmlDiv(list(htmlLabel('Select Animal Type:'), animalDropdown), class="col-2"),
          htmlDiv(list(htmlLabel('Select month:'), monthDropdown), class="col-2"),
          htmlDiv(list(graph_3,htmlBr()),class="col-4")
        ),
        class="row"
      ),
      # plot 4 and 5 row
      htmlDiv(
        list(
          htmlDiv(list(plot4, htmlBr()), class="col-4"),
          htmlDiv(list(htmlLabel('Select buttons:'), plot4_radio), class="col-2"),
          htmlDiv(list(htmlLabel('Select intake conditions:'), plot5_drop), class="col-2"),
          htmlDiv(list(plot5, htmlBr()), class="col-4")
        ),
        class="row"
      )),
      class="container"
))
      
 
# Add callbacks for plot1 interactivity
app$callback(
    # update figure of graph1 
    output=list(id = 'graph1', property='figure'),
  
    # based on values of year
    params=list(input(id = 'year-slider', property='value')),

   # translate list of params into function arguments
    function(year_value) {
        make_plot_1(year_value)
   })

# Add callbacks for plot2 interactivity
app$callback(
  #update figure of graph2
  output=list(id = 'graph2', property='figure'),
  #based on values of year, month, animal type
  params=list(input(id = 'year-slider', property='value'),
              input(id = 'animalType', property='value'),
              input(id = 'month', property='value')),
  #this translates your list of params into function arguments
  function(year_value, animalType_value, month_value) {
    make_graph_plot2(year_value, animalType_value, month_value)
  })

# Add callbacks for plot3 interactivity
app$callback(
  #update figure of graph3
  output=list(id = 'graph3', property='figure'),
  #based on values of year, month, animal type
  params=list(input(id = 'year-slider', property='value'),
              input(id = 'animalType', property='value'),
              input(id = 'month', property='value')),
  #this translates your list of params into function arguments
  function(year_value, animalType_value, month_value) {
    make_graph_plot3(year_value, animalType_value, month_value)
  })


# Add callbacks for plot4 interactivity
app$callback(
  # update figure of plot 4
  output=list(id = 'plot4', property='figure'),
  
  # based on values of year and animal type
  params=list(input(id = 'year-slider', property='value'),
              input(id = 'plot4-radio', property='value')),

  # this translates your list of params into function arguments
  function(year_range, animal_type_choice) {
    make_plot4(year_range, animal_type_choice)
  })

# Add callbacks for plot5 interactivity
app$callback(
  # update figure of plot5
  output=list(id = 'plot5', property='figure'),
  
  # based on values of year and intake conditions
  params=list(input(id = 'year-slider', property='value'),
              input(id = 'plot5-drop', property='value')),

  # this translates your list of params into function arguments
  function(year_range, intake_cond) {
    make_plot5(year_range, intake_cond)
  })


app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050))