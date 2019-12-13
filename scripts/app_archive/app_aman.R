library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashTable)
library(tidyverse)
library(plotly)
library(gapminder)

#app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

app <- Dash$new(external_stylesheets = "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css")

df <- read_csv("./data/aac_data_cleaned.csv")

yearMarks <- map(unique(df$intake_year), as.character)
names(yearMarks) <- unique(df$intake_year)

yearSlider <- dccRangeSlider(
  id = "year",
  marks = yearMarks,
  min = 2013,
  max = 2018,
  step = 1,
  value = list(2014, 2015)
)

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



make_graph_plot2 <- function(year = c(2014, 2015), 
                       animal = "All", month=0){
  
  
  
  data_intake <- df %>%
  filter(intake_year >= year[1] & intake_year <= year[2]) %>%
  group_by(intake_year,animal_type,intake_weekday,intake_month) %>%
  summarise(cnt = n()) 
  
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


make_graph_plot3 <- function(year = c(2014, 2015), 
                             animal = "All", month=0){
  
  
  
  data_outtake <- df %>%
    filter(outtake_year >= year[1] & outtake_year <= year[2]) %>%
    group_by(outtake_year,animal_type,intake_weekday,intake_month) %>%
    summarise(cnt = n()) 
  
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


graph_2 <- dccGraph(
  id = 'graph2',
  figure=make_graph_plot2() #
)

graph_3 <- dccGraph(
  id = 'graph3',
  figure=make_graph_plot3() #
)

app$layout(
  
  htmlDiv(
    list(
      # Row 0
      htmlCenter(htmlLabel('Select a year range:')),
      yearSlider,
      htmlBr(),
      htmlBr(),
      # Row 1
      htmlDiv(                    
        list(
          htmlDiv(
            list(
              graph_2,
              htmlBr()
            ),
            class="col-4"
          ),
          htmlDiv(
            list(
              htmlLabel('Select Animal Type:'),
              animalDropdown
            ),
            class="col-2"
          ),
          htmlDiv(
            list(
              htmlLabel('Select month:'),
              monthDropdown
            ),
            class="col-2"
          ),
          htmlDiv(
            list(
              graph_3,
              htmlBr()
            ),
            class="col-4"
          )
        ),
        class="row"
      )
      
    ),
    class="container"
  )
)

#app$layout(
#  htmlDiv(
#    list(
#      htmlH1('Dash Demo plot2 an dplot 3'),
#      #selection components
#      htmlLabel('Select a year range:'),
#      yearSlider,
#      htmlIframe(height=15, width=10, style=list(borderWidth = 0)), #space
#      htmlLabel('Select month:'),
#      monthDropdown,
#      htmlLabel('Select Animal Type:'),
#      animalDropdown,
#      #graph and table
#      htmlLabel('Graph2'),
#      graph_2, 
#      htmlIframe(height=20, width=10, style=list(borderWidth = 0)), #space
#      htmlLabel('Graph3'),
#      graph_3
#      
#    ), style = list('columnCount' = 2)
#  )
#)

app$callback(
  #update figure of graph2
  output=list(id = 'graph2', property='figure'),
  #based on values of year, month, animal type
  params=list(input(id = 'year', property='value'),
              input(id = 'animalType', property='value'),
              input(id = 'month', property='value')),
  #this translates your list of params into function arguments
  function(year_value, animalType_value, month_value) {
    make_graph_plot2(year_value, animalType_value, month_value)
  })

app$callback(
  #update figure of graph3
  output=list(id = 'graph3', property='figure'),
  #based on values of year, month, animal type
  params=list(input(id = 'year', property='value'),
              input(id = 'animalType', property='value'),
              input(id = 'month', property='value')),
  #this translates your list of params into function arguments
  function(year_value, animalType_value, month_value) {
    make_graph_plot3(year_value, animalType_value, month_value)
  })




app$run_server()
