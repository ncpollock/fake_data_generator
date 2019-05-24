

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
library(shinydashboard)
library(shinyWidgets)
useShinydashboard()
library(shinythemes)
# library(shinyBS)
# library(DT) # datatable breaks input capturing!
# library(googleVis)
# library(ggplot2)
# library(scales)
library(dplyr)
library(tidyr)
# library(maps)
library(kableExtra)

# load file resources
lorem_ipsum <- readLines("lorem_ipsum.txt") %>%
  strsplit(".",fixed=TRUE) %>%
  sapply(trimws)

names_df <- read.csv("names.csv"
                     , stringsAsFactors = FALSE)

# weekdays
# weekdays(seq.Date(as.Date("2017-01-01"),as.Date("2017-01-07"),1))
# weekdays(seq.Date(as.Date("2017-01-01"),as.Date("2017-01-07"),1),abbreviate = TRUE)
weekday_full <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
weekday_abb <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

# style column names / header for datatables
# dt_column_head <- JS(
#   "function(settings, json) {",
#   "$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
#   "}")

# allow box collapse on title click
title_collapse <- function(x){
  HTML(
    paste0('<strong class="box-title" data-widget="collapse" style="cursor: pointer;">'
           ,x
           ,'</strong>'))
}

# var_type_selections <- c("Sequential Primary Key","Numeric","Date Range","Character String: Nominal","Character String: Long Text")

# should sequential primary key be under Numeric?
  # grouped sequence, then user picks a character variable?
var_type_selections <- list(
  'Numeric' 
  , 'Character' = list("Nominal/Categorical","Long Filler Text")
  , 'Date' = list("Date Range","Days of Week","Month")
  , 'Other' = list("Sequential Primary Key","Names","Phone Numbers","States")
)

var_id <- 1
init_var <- function(x,var_id){
  column(12,id = paste0("div_var_",var_id)
         , fluidRow(class = "variable-row"
                    , column(3
                             , style = "margin-top: 25px; border-right: 1px dashed black;"
                             , textInput(paste0("var_name_",var_id), NULL, x))
                    , column(4,id = paste0("var_type_col_",var_id)
                             , style = "margin-top: 25px; border-right: 1px dashed black;"
                             , selectInput(paste0("var_type_",var_id), NULL
                                           # two types of var types: atomic (numeric, character, factor) vs pre-defined (primary key, names, phone numbers)
                                           , var_type_selections))
                    , column(4,id = paste0("var_input_col_",var_id),p(""))
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
                                           , state.name
                                           , multiple=TRUE))
                    , column(4,id = paste0("outcome_",ML_id)
                             , style = "margin-top: 25px; border-right: 1px dashed black;"
                             , selectInput(paste0("ML_outcome_",ML_id), NULL
                                           # two types of var types: atomic (numeric, character, factor) vs pre-defined (primary key, names, phone numbers)
                                           , state.name))
                    , column(4,id = paste0("ML_input_col_",ML_id),p(""))
                    , column(1
                             , style = "margin-top: 25px;"
                             , actionButton(paste0("ML_delete_",ML_id), "",icon=icon("trash"),style="background-color: red;")
                             # , dynamic help buttons/tooltips based on variable type selection!
                    )
         )
  )
}

my_navbar_info <- gsub("[\r\n]", "",
                    div(
                      icon("user fa-pull-right fa-3x")
                      , strong("Developed by: "),
                    br(),
                    a(href="https://ncpollock.github.io/"
                      ,target="_blank"
                      ,"Noah C. Pollock")
                    ,style = "float:right;padding-top:5px;white-space:nowrap;"))


# set general theme for ggplots
# my_theme <- theme(panel.background = element_blank(),
#                   axis.text = element_text(size = '15'),
#                   axis.title = element_text(size = '18'),
#                   axis.line = element_line(color = 'black'),
#                   strip.background = element_rect(fill = 'black'),
#                   strip.text = element_text(color = 'white',size = '18'),
#                   legend.position = "bottom",
#                   legend.text = element_text(size = '18'),
#                   legend.title = element_blank())
