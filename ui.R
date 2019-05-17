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
                            , column(3,actionButton("preview","Preview Data"
                                                    ,width = "100%"
                                                    ,icon = icon("binoculars"))
                            )
                            , column(1, p(icon("align-justify"),"Rows:",style = "font-size: 20px;text-align:right;padding-top:10px;"))
                            , column(3,sliderInput("df_rows",label=NA,1,1000,value=100,ticks = FALSE))
                            , column(2,downloadButton("downloadData","Download",style="float:right;background-color: blue;"))
                 )
                 , fluidRow(
                   br()
                   , column(12,id = "var_header",column(3,"Variable Name"),column(4,"Type"),column(4,"Options / Details"))
                   )
                 # , box("test"
                 #     # , title_collapse("Variable Associations")
                 #         , width = 12, background = "black",solidHeader = TRUE,collapsible = TRUE
                 #         , p("some content!")
                 #         , h3("Some more content!"))
                   , tableOutput("show_inputs")
                 )
               
               , tabPanel("Variable Associations", icon = icon("chain")
                          , fluidRow(id="GlobalTools_ML",class="sticky"
                                     , column(6,actionButton("add_ML","Add New Association"
                                                             ,width = "100%"
                                                             ,style="background-color: green;"
                                                             ,icon = icon("plus-square")))
                                     , column(6,actionButton("preview_ML","Visualize Associations"
                                                             ,width = "100%"
                                                             ,icon = icon("bar-chart"))
                                     )
                                     # , column(2,downloadButton("downloadData","Download",style="float:right;background-color: blue;"))
                          )
                          , fluidRow(
                            br()
                            , column(12,id = "var_header_ML",column(3,"Predictor(s)"),column(4,"Outcome"),column(4,"Options / Details"))
                          )
                          # use selectize input
                          # first variable with have greatest predictive power?
                            # or can selectize be ordered?
                          , p("dynamic UI based on variables created.")
                          # , column(12,id = paste0("div_ML_",var_id)
                          #        , fluidRow(class = "variable-row"
                          #                   , column(4
                          #                            , style = "margin-top: 25px; border-right: 1px dashed black;"
                          #                            , selectInput(paste0("predictor_",var_id), 'Options', state.name, multiple=TRUE, selectize=TRUE))
                          #                   , column(4,id = paste0("outcome_",var_id)
                          #                            , style = "margin-top: 25px; border-right: 1px dashed black;"
                          #                            , selectInput(paste0("outcome_",var_id), 'Options', state.name, multiple=TRUE, selectize=TRUE)
                          #                            )
                          #                   , column(3,id = paste0("ML_input_col_",var_id),p(""))
                          #                   , column(1
                          #                            , style = "margin-top: 25px;"
                          #                            , actionButton(paste0("ML_delete_",var_id), "",icon=icon("trash"),style="background-color: red;")
                          #                            # , dynamic help buttons/tooltips based on variable type selection!
                          #                   )
                          #        )
                          # )
                )
               
               , tabPanel('About/Help', icon = icon("question-circle")
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
               # add my name in navbar
               , tags$script(
                 HTML(
                   paste0("var header = $('.navbar> .container-fluid');header.append('"
                          , my_navbar_info,"');console.log(header)")))
               )))