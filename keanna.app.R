library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashTable)
library(tidyverse)
library(plotly)
library(lubridate)


app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")



df <- read_csv("./data/aac_data_cleaned.csv")

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


# Plot 1
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



######### Create plotting functions ##########

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


############### APP LAYOUT BEGINS ##############

app$layout(
  htmlDiv(
      list(
      # Logo
      #htmlImg(src='https://raw.githubusercontent.com/UBC-MDS/DSCI_532_L02_group206_ms1/master/img/aac_logo.jpg'), 
      htmlIframe(height=5, width=10, style=list(borderWidth = 0)), #space
      # Main app title 
      htmlH1("Animals Sheltered at the Austin Animal Center"),
      grey_line <- htmlDiv(list(htmlHr(className = "greyline"))), 
      htmlIframe(height=15, width=10, style=list(borderWidth = 0)), #space #style={"textAlign":"center"}),
      # htmlHr([],style={'height': 10,'background-color':'#2B3856'}),
      htmlLabel("Select Animal Intake Year Range:"), #style={"textAlign":"center"}),
      yearSlider,
      htmlIframe(height=45, width=10, style=list(borderWidth = 0)), #space
      graph1)))
 
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

app$run_server()