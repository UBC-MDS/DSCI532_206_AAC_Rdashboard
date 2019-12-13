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
# library(cowplot)

##################
# code for cowplot library replacement
theme_cowplot <- function(font_size = 14, font_family = "", line_size = .5,
                          rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14) {
  half_line <- font_size / 2
  small_size <- rel_small * font_size

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
      text              = element_text(family = font_family, face = "plain", color = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = margin(), debug = FALSE),

      axis.line         = element_line(color = "black", size = line_size, lineend = "square"),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_text(color = "black", size = small_size),
      axis.text.x       = element_text(margin = margin(t = small_size / 4), vjust = 1),
      axis.text.x.top   = element_text(margin = margin(b = small_size / 4), vjust = 0),
      axis.text.y       = element_text(margin = margin(r = small_size / 4), hjust = 1),
      axis.text.y.right = element_text(margin = margin(l = small_size / 4), hjust = 0),
      axis.ticks        = element_line(color = "black", size = line_size),
      axis.ticks.length = unit(half_line / 2, "pt"),
      axis.title.x      = element_text(
                            margin = margin(t = half_line / 2),
                            vjust = 1
                          ),
      axis.title.x.top  = element_text(
                            margin = margin(b = half_line / 2),
                            vjust = 0
                          ),
      axis.title.y      = element_text(
                            angle = 90,
                            margin = margin(r = half_line / 2),
                            vjust = 1
                          ),
      axis.title.y.right = element_text(
                             angle = -90,
                             margin = margin(l = half_line / 2),
                             vjust = 0
                           ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = rel(rel_small)),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position   = "right",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid        = element_blank(),
      panel.grid.major  = NULL,
      panel.grid.minor  = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
                            size = rel(rel_small),
                            margin = margin(half_line / 2, half_line / 2,
                                            half_line / 2, half_line / 2)
                          ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_text(
                            face = "bold",
                            size = rel(rel_large),
                            hjust = 0, vjust = 1,
                            margin = margin(b = half_line)
                          ),
      plot.subtitle     = element_text(
                            size = rel(rel_small),
                            hjust = 0, vjust = 1,
                            margin = margin(b = half_line)
                          ),
      plot.caption      = element_text(
                            size = rel(rel_tiny),
                            hjust = 1, vjust = 1,
                            margin = margin(t = half_line)
                          ),
      plot.tag           = element_text(
                             face = "bold",
                             hjust = 0, vjust = 0.7
                           ),
      plot.tag.position = c(0, 1),
      plot.margin       = margin(half_line, half_line, half_line, half_line),

      complete = TRUE
    )
}

# To use theme_half_open which is essentially theme_cowplot
theme_half_open <- theme_cowplot

# theme_minimal_grid is basis for theme_minimal_hgrid
theme_minimal_grid <- function(font_size = 14, font_family = "", line_size = .5,
                               rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14,
                               color = "grey85", colour) {
  if (!missing(colour)) {
    color <- colour
  }

  # Starts with theme_cowplot and then modifies some parts
  theme_cowplot(font_size = font_size, font_family = font_family, line_size = line_size,
                rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large) %+replace%
    theme(
      # make grid lines
      panel.grid        = element_line(color = color,
                                       size = line_size),
      panel.grid.minor  = element_blank(),

      # adjust axis tickmarks
      axis.ticks        = element_line(color = color, size = line_size),

      # no x or y axis lines
      axis.line.x       = element_blank(),
      axis.line.y       = element_blank(),

      # no filled background for facted plots
      strip.background = element_blank(),

      complete = TRUE
    )
}

# To use theme_minimal_hgrid
theme_minimal_hgrid <- function(font_size = 14, font_family = "", line_size = .5,
                                rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14,
                                color = "grey85", colour) {
  if (!missing(colour)) {
    color <- colour
  }

  # Starts with theme_grid and then modifies some parts
  theme_minimal_grid(font_size = font_size, font_family = font_family, line_size = line_size,
                     rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large,
                     color = color) %+replace%
    theme (
      # no vertical grid lines
      panel.grid.major.x = element_blank(),

      # add a x axis line
      axis.line.x       = element_line(color = color, size = line_size),

      complete = TRUE
    )
}

# To use background_grid
background_grid <- function(major = c("xy", "x", "y", "only_minor", "none"),
                            minor = c("none", "xy", "x", "y"),
                            size.major = 0.5, size.minor = 0.2,
                            color.major = "grey85", color.minor = "grey85",
                            colour.major, colour.minor){
  if (!missing(colour.major)) {
    color.major <- colour.major
  }

  if (!missing(colour.minor)) {
    color.minor <- colour.minor
  }


  # start with a defined theme that corresponds to the default settings
  t <- theme(
    panel.grid = element_line(
      color = color.major,
      size = size.major,
      linetype = 1,
      lineend = "butt"
    ),
    panel.grid.major = NULL,
    panel.grid.major.x = NULL,
    panel.grid.major.y = NULL,
    panel.grid.minor = element_line(
      color = color.minor,
      size = size.minor,
      linetype = 1,
      lineend = "butt"
    ),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

  t <- switch(major[1],
    x = t + theme(panel.grid.major.y = element_blank()),
    y = t + theme(panel.grid.major.x = element_blank()),
    xy = t,
    yx = t,
    t + theme(panel.grid.major = element_blank())
  )

  t <- switch(minor[1],
    x = t + theme(panel.grid.minor.x = NULL),
    y = t + theme(panel.grid.minor.y = NULL),
    xy = t + theme(
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL),
    yx = t + theme(
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL),
    t
  )

  t
}
#################

#### Use specific css styling sheets for various classes based on dashr gallery examples.
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


monthKey <- tibble(label = c("All", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                             "Aug", "Sep", "Oct", "Nov", "Dec"),
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
    options = list(list(label = " All", value = "All"),
                    list(label = " Cats", value = "Cat"),
                    list(label = " Dogs", value = "Dog"),
                    list(label = " Birds", value = "Bird"),
                    list(label = " Others", value = "Other")),
    value = 'All'
)

# plot 5 drop list
plot5_droplistkey <- tibble(label = c('All','Healthy','Injured','Sick','Feral','Pregnant','Nursing','Other'),
                   value = c('All','Normal','Injured','Sick','Feral','Pregnant','Nursing','Other'))

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
        scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
        scale_color_discrete(labels = c("Intake","Outtake"))+
        theme_half_open()+
        background_grid()+
        labs(color = "Trend") +
        theme(plot.title = element_text(size = 20, face = "bold",hjust = 0.5),
             axis.text.x = element_text(size = 12, angle = 45),
             axis.title.x = element_text(size = 20),
             axis.title.y = element_text(size = 20),
             axis.text.y = element_text(size = 14),
             #panel.background = element_blank()) 
        )
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
    geom_bar(position="dodge", stat="identity", fill="#56B4E9", alpha = 0.8) +
    labs(title = title, x = "Week day") +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05)))+
    theme_minimal_hgrid() +
    theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5),
          axis.text.x = element_text(size = 8, angle = 45),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text.y = element_text(size = 8))
    
  
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
    geom_bar(position="dodge", stat="identity", fill="#56B4E9", alpha = 0.8) +
    labs(title = title, x = "Week day") +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05)))+
    theme_minimal_hgrid() +
    theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5),
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
    geom_histogram(color = "dodgerblue", fill = "#56B4E9", binwidth = 0.5) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    labs(title = paste0("Age Distribution of ", title_string),
         x = "Intake Age (Year)",
         y = "Count") +
    theme_half_open()+
    background_grid() +
    theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5),
          axis.text.y = element_text(size = 8),
          panel.background = element_blank())
    
  
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
        geom_boxplot(outlier.alpha = 0.3, outlier.stroke = 0.1, fill ="#56B4E9" ) +
        scale_y_continuous(trans = "log10",
                          breaks = c(0.5, 1, 10, 50, 100, 200, 400, 1000), 
                            labels = scales::label_comma(accuracy = 0.1))+
        labs(title = paste0("Days Spent in Shelter for ",intake_cond," Animals"),
                y = "Days", 
                x = "") +
        theme_half_open()+
        theme_minimal_hgrid() +
        theme(plot.title = element_text(size = 12, hjust = 0.5),
              axis.text.y = element_text(size = 8),
              panel.background = element_blank())

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
      htmlDiv(class = "col-1"),
      htmlDiv(
        list(
          htmlBr(),
          # Logo
          htmlImg(src='https://cdn-images.threadless.com/threadless-media/artist_shops/shops/austinanimalcenter/profile/logo-1458338907-6b959d2b197869a6fccd76be60246ba6.png?v=3&d=eyJvbmx5X21ldGEiOiBmYWxzZSwgImZvcmNlIjogZmFsc2UsICJvcHMiOiBbWyJyZXNpemUiLCBbNDUwXSwge31dXX0=',
            style=list("objectFit"="contain", "height" = 300)
          ),
          htmlBr()          
        ), class = "col-2"
      ),
      #htmlDiv(class = "col-1"),
      htmlDiv(
        list(
          htmlDiv(
            list(
              # Main app title 
              htmlBr(),
              htmlH1("Animals Sheltered at the Austin Animal Center")
            )
          ),
          htmlDiv(
            list(
              # Content
              dccMarkdown("This application is targeted at shelter staff at the Austin Animal Center to obtain insights on their operations. With the app, users can explore and understand:"),
              dccMarkdown("- the overall intake/outtake trends of shelter animals across different time periods \n - the intake/outake volumes on weekdays for different animals for specific months \n - the age distribution of animals upon intake \n - the time spent by animals in the shelter before outtake based on their intake health conditions"),
              
              dccMarkdown(children = "This project is proudly brought to you by Keanna Knebel, Aman Kumar Garg, and Kenneth Foo. The project repository can be found [here](https://github.com/UBC-MDS/DSCI532_206_AAC_Rdashboard)."), 
              dccMarkdown(children = "For more information on volunteering, donations, or adoptions, please visit [Austin Animal Center](http://www.austintexas.gov/department/aac)."),
              htmlBr()

            )
          )
          
        ), class = "col-7"
      )
  ), class = "row", style = list("backgroundColor"="#CEF0F2")),
  htmlDiv(
    list(
      #grey_line <- htmlDiv(list(htmlHr(className = "greyline"))), 
      htmlIframe(height=15, width=10, style=list(borderWidth = 0)), 
      htmlDiv("Select Animal Intake Year Range For All Plots:", style=list(textAlign="center")), 
      htmlBr(),
      yearSlider,
      htmlIframe(height=45, width=10, style=list(borderWidth = 0)), #space
      graph1,
      htmlBr(),
      htmlDiv(list(htmlHr(className = "greyline"))),
      htmlBr(),
      # plot 2 and 3 row
      htmlDiv(                    
        list(
          htmlDiv(list(graph_2, htmlBr()), class="col-4"),
          htmlDiv(list(htmlLabel('Select Animal Type:'), animalDropdown), class="col-2"),
          htmlDiv(list(htmlLabel('Select Month:'), monthDropdown), class="col-2"),
          htmlDiv(list(graph_3,htmlBr()),class="col-4")
        ),
        class="row"
      ),
      htmlDiv(list(htmlHr(className = "greyline"))),
      htmlBr(),
      # plot 4 and 5 row
      htmlDiv(
        list(
          htmlDiv(list(plot4, htmlBr()), class="col-4"),
          htmlDiv(list(
            htmlDiv(list(
              htmlDiv(list(htmlLabel('Select Animals:'))), 
              htmlDiv(list(plot4_radio),style=list(width="40%"))), 
              style=list(width = "49%","display"="inline-block",verticalAlign = "top")),
            
            # vertical separator
            htmlDiv(style=list(height = "400px", width="2px","backgroundColor"="#D3D3D3","display"="inline-block",verticalAlign = "top")),
            # buffer space padding between vertical line and plot5 droplist
            htmlDiv(style=list(height = "400px", width="15px","display"="inline-block",verticalAlign = "top")),

            htmlDiv(list(
              htmlDiv(list(htmlLabel('Select Intake Health Conditions:'))), 
              htmlDiv(list(plot5_drop))
              ), style=list(width = "45%","display"="inline-block",verticalAlign = "top", marginRight = "0px"))
          ), class = "col-4"),
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