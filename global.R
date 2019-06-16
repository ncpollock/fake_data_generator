
# scikit learn websites make blobs

#initialize colors
v_light_gray <- '#a3a3a3' #'#CDCDCD'
v_light_gray2 <- '#bebebe'
med_gray <- '#696969' #"#808080"
v_dark_gray <- '#323232' #'#252525'
v_dark_gray2 <- '#595959'
plum <- '#24292e' # '#8c001a'
chocolate <- '#7d430e'

#test color pallette
electric_lime <- '#88D317'
electric_blue <- '#4be0f6'
sunshine <- '#fda302' # orange
shadow <- '#535353'
cyan <- '#43c0f5'
charcoal <- '#3d3d3d'

library(shiny)
library(shinyBS) # tooltips
library(shinydashboard)
library(shinyWidgets)
useShinydashboard() # am I still using this?
library(shinythemes)
# library(shinyBS)
# library(DT) # datatable breaks input capturing!
# library(googleVis)
library(ggplot2) # graphics library
# library(scales)
library(dplyr) # data wrangling
library(tidyr)
# library(maps)
library(kableExtra) # table formatting
library(rpart) # decision trees
library(rpart.plot) # decision tree plots

# load file resources
lorem_ipsum <- readLines("lorem_ipsum.txt") %>%
  strsplit(".",fixed=TRUE) %>%
  sapply(trimws)

names_df <- read.csv("names.csv"
                     , stringsAsFactors = FALSE)

normalize <- function(x,range_min,range_max){
   (range_max - range_min)*(
    x - min(x)
    )/(
      max(x) - min(x)
      ) + range_min
}

# weekdays
weekday_full <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
weekday_abb <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

# should sequential primary key be under Numeric?
  # grouped sequence, then user picks a character variable?
var_type_selections <- list(
  'Numeric' 
  , 'Character' = list("Nominal/Categorical","Long Filler Text")
  , 'Date' = list("Date Range","Month","Day of Week")
  , 'Demographic' = list("Name","Race","Phone Number","Email Address","State")
  , 'Other' = list("Sequential Primary Key","Custom R Code")
)

# year, College Major, Companies, Illnesses?

var_id <- 1
init_var <- function(x,var_id){
  column(12,id = paste0("div_var_",var_id)
         , fluidRow(class = "variable-row"
                    , column(2
                             , style = "margin-top: 25px; border-right: 1px dashed black;"
                             , textInput(paste0("var_name_",var_id), NULL, x))
                    , column(3,id = paste0("var_type_col_",var_id)
                             , style = "margin-top: 25px; border-right: 1px dashed black;"
                             , selectInput(paste0("var_type_",var_id), NULL
                                           , var_type_selections))
                    , column(6,id = paste0("var_input_col_",var_id),p(""))
                    , column(1
                             , style = "margin-top: 25px;"
                             , actionButton(paste0("var_delete_",var_id), "",icon=icon("trash"),style="background-color: red;")
                             # , dynamic help buttons/tooltips based on variable type selection!
                    )
         )
  )
}

init_ML <- function(ML_id){
  column(12,id = paste0("div_ML_",ML_id)
         , fluidRow(class = "variable-row"
                    , column(3
                             , style = "margin-top: 25px; border-right: 1px dashed black;"
                             , selectInput(paste0("ML_predictor_",ML_id),''
                                           , state.name))
                    , column(4,id = paste0("outcome_",ML_id)
                             , style = "margin-top: 25px; border-right: 1px dashed black;"
                             , selectInput(paste0("ML_outcome_",ML_id), NULL
                                           # two types of var types: atomic (numeric, character, factor) vs pre-defined (primary key, names, phone numbers)
                                           , state.name))
                    , column(4,id = paste0("ML_input_col_",ML_id),p(""))
                    , column(1
                             , style = "margin-top: 25px;"
                             , actionButton(paste0("ML_delete_",ML_id), "",icon=icon("trash"),style="background-color: red;")
                    )
         )
  )
}

my_navbar_info <- gsub("[\r\n]", "",
                    div(
                      a(href="https://ncpollock.github.io/"
                        ,target="_blank"
                      , img(src="headshot.jpg",id="face-img",align="right"))
                      # icon("user fa-pull-right fa-3x") # generic user icon instead of my face
                      , strong("Developed by: "),
                    br(),
                    a(href="https://ncpollock.github.io/"
                      ,target="_blank"
                      ,"Noah C. Pollock")
                    ,style = "float:right;padding-top:5px;white-space:nowrap;"))


# set general theme for ggplots
my_theme <- theme(panel.background = element_blank(),
                  axis.text = element_text(size = '15'),
                  axis.title = element_text(size = '18'),
                  axis.line = element_line(color = 'black'),
                  strip.background = element_rect(fill = 'black'),
                  strip.text = element_text(color = 'white',size = '18'),
                  legend.position = "bottom",
                  legend.text = element_text(size = '18'),
                  legend.title = element_blank())
