# START
shinyUI(
  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "my_style.css"),
    navbarPage(id = "tabs"
               , theme = shinytheme("darkly")
               , title = p(strong("Data Generator")
                         # ,style=paste0("color: white;")
                         )
               , windowTitle = "Data Generator"
               , footer = div(id="site-footer")
               , tabPanel(
                 'Generate Data', icon = icon("sliders")
                 , fluidRow(
                   useShinydashboard()
                 )
                 , fluidRow(id="GlobalTools",class="sticky"
                            , column(3,actionButton("add","Add New Variable"
                                                                     ,width = "100%"
                                                                     ,style="background-color: green;"
                                                                     ,icon = icon("plus-square")))
                            , column(1, p(icon("align-justify"),"Rows:",style = "font-size: 20px;text-align:right;padding-top:10px;"))
                            , column(2,sliderInput("df_rows",label=NA,1,1000,value=100,ticks = FALSE))
                            , column(2, p(icon("align-justify",class="fa-rotate-90")
                                         ,"Variables: "
                                         ,textOutput("df_variables",inline = TRUE)
                                         ,style = "font-size: 20px;text-align:right;padding-top:10px;"))
                            , column(2,actionButton("preview","Preview Data"
                                                    ,width = "100%"
                                                    ,icon = icon("binoculars"))
                                     ,style="float:right;")
                            , column(2,downloadButton("downloadData","Download",style="float:right;background-color: blue;"))
                 )
                 , fluidRow(
                   br()
                   , column(12,id = "var_header",column(3,"Variable Name"),column(4,"Type"),column(4,"Options / Details"))

                   # test creating my own infobox
                   , box(background = "black"
                         , icon("columns fa-pull-left fa-5x")
                         , "Number of Columns"
                         , p("10",style="font-size:200%;"))
                 , tableOutput("show_inputs")
                 )),
               
               tabPanel('About/Help', icon = icon("question-circle"),tags$style("float: right;")
                        ,p("Some inspirations:"
                           ,br(),"https://mockaroo.com/"
                           ,br(),"https://ebsubudhi.shinyapps.io/DataGeneration/"
                           ,br(),"https://www.onlinedatagenerator.com/")
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