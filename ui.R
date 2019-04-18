# START
shinyUI(
  tagList(
    # tags$link(rel = "stylesheet", type = "text/css", href = "my_style.css"),
    navbarPage(id = "tabs",
               theme = shinytheme("darkly"),
               title = p(strong("Data Generator"),style=paste0("color: white;")),
               windowTitle = "Data Generator",
               # footer = div(id="site-footer"),
               tabPanel(
                 'Generate Data', icon = icon("sliders")
                 , fluidRow(
                   useShinydashboard()
                   , verbatimTextOutput("test")
                   , infoBoxOutput('df_columns')
                   , infoBoxOutput('df_rows')
                   , infoBoxOutput('df_size')
                   , box(background = "black"
                         , icon("columns fa-pull-left fa-5x")
                         , "Number of Columns"
                         , p("10",style="font-size:200%;"))
                 )
                 , uiOutput("dynamic_inputs")
                 , dataTableOutput('preview_fake_df')
                 ),
               
               tabPanel('About/Help', icon = icon("question-circle"),tags$style("float: right;")
                        ,p(strong("Developed by: "),
                           br(),
                           a(href="https://ncpollock.github.io/"
                             ,target="_blank"
                             ,"Noah C. Pollock"),
                           br(),
                           a(href = "https://github.com/ncpollock/"
                             ,target="_blank"
                             ,"Code on GitHub"),
                           align="center"))
               )))