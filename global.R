

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
library(shinyBS)
library(DT)
library(googleVis)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(maps)

# specify the height of a container
# could make an input to find the right height dynamically!
static_height <- div(style = "height:50px !importanat;background-color: yellow;")

# style column names / header for datatables
dt_column_head <- JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
  "}")

# allow box collapse on title click
title_collapse <- function(x){
  HTML(
    paste0('<strong class="box-title" data-widget="collapse" style="cursor: pointer;">'
           ,x
           ,'</strong>'))
}

# var_type_selections <- c("Sequential Primary Key","Numeric","Date Range","Character String: Nominal","Character String: Long Text")

var_type_selections <- list(
  'Numeric' 
  , 'Character' = list("Nominal/Categorical","Long Filler Text")
  , 'Date' = list("Date Range","Days of Week","Month")
  , 'Other' = list("Sequential Primary Key","Names","Phone Numbers","States")
)

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

# # keep a count of all variables trashed
# removed_vars <- 0

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

custom_colors <- ""
# 
# # use custom color pallette across app
# custom_colors <- HTML(paste0('
#                                          /* logo */
#                              .skin-blue .main-header .logo {
#                              background-color:',maroon1,';
#                              }
#                              
#                              /* logo when hovered */
#                              .skin-blue .main-header .logo:hover {
#                              background-color:',maroon1,';
#                              }
#                              
#                              /* toggle button when hovered  */
#                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
#                              background-color:',v_dark_gray,';
#                              }
#                              
#                              /* navbar (rest of the header) */
#                              .skin-blue .main-header .navbar {
#                              background-color:',maroon1,';
#                              }
#                              
#                              /* main sidebar */
#                              .skin-blue .main-sidebar {
#                              background-color:',v_dark_gray,';
#                              }
#                              
#                              .skin-blue .sidebar-menu > li:hover > a,
#                              .skin-blue .sidebar-menu > li.active > a {
#                              color: white;
#                              background:',maroon1,';
#                              border-left-color:',maroon1,';
#                              }
#                              .skin-blue .sidebar-menu > li > .treeview-menu {
#                              margin: 0 1px;
#                              background:',med_gray,';
#                              }
#                              .skin-blue .treeview-menu > li.active > a,
#                              .skin-blue .treeview-menu > li > a:hover {
#                              color: white;
#                              background:',maroon1,';
#                              }
#                              
#                              .skin-blue .sidebar a {
#                              color: white;
#                              }
#                              .skin-blue .treeview-menu > li > a {
#                              color: white;
#                              }
#                              
#                              .small-box h3 {
#                              font-size: 38px;
#                              font-weight: 700;
#                              margin: 0 0 10px;
#                              white-space: nowrap;
#                              padding: 0;
#                              }
#                              .bg-primary {
#                              color: #fff;
#                              background-color: #337ab7;
#                              }
#                              '))
