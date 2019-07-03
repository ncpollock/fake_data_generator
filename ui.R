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
               , footer = div(id="site-footer"
                              , p(strong("Developed by: "),
                                  br(),
                                  a(href="https://ncpollock.github.io/"
                                    ,target="_blank"
                                    ,"Noah C. Pollock"),
                                  br(),
                                  a(href = "https://github.com/ncpollock/"
                                    ,target="_blank"
                                    ,"Code on GitHub"),
                                  align="center"))
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
                            , column(3,sliderInput("df_rows",label=NA,50,1000,value=100,ticks = FALSE))
                            , column(2,downloadButton("downloadData","Download",style="float:right;background-color: blue;"))
                 )
                 , fluidRow(
                   br()
                   , column(12,id = "var_header",column(3,"Variable Name"),column(4,"Type"),column(4,"Options / Details"))
                   )
                 , h2("Define Variable Relationships (Beta)")
                 , fluidRow(
                   br()
                   , column(12,id = "var_header_ML",column(1,align="center","Active?"),column(3,"Predictor(s)"),column(3,"Outcome"),column(1,"Preview"),column(4,"Options"))
                 )
                 , uiOutput("Associations")
                 # , h2(id = "no_assoc","There are no Variable Associations defined yet.",style="align:center;color:gray;")
                 
                 # retain for testing
                 # , tableOutput("show_inputs")
                 )
               
               , tabPanel('About', icon = icon("question-circle")
                          , div(class= "padded_page"
                          , h2("What Is Fake Data and Why Generate It?")
                            , p("On this site, fake data is data that looks real but has been fabricated rather than observed/recorded. 
                                It does not accurately represent any person, place, or other entity that it mimics. 
                                Although real data is increasingly easier to obtain (e.g., data.gov, kaggle.com), there are times when
                                you may need a dataset with very specific attributes. This app was inspired by an onslought of course projects
                                that, in order to begin the relevant work, required finding a suitable dataset first.")
                          , h4("A few good reasons to create your own data:")
                            , tags$ul(
                              tags$li("Mocking up visualizations")
                              , tags$li("Testing software applications")
                              , tags$li("Testing database designs or queries")
                              , tags$li("Learning statistics, data science, or business analytics")
                              , tags$li("Simulations (e.g., probabilities or algorithms)")
                              , tags$li("Teaching")
                            )
                          , h4("A few bad reasons:")
                            , tags$ul(
                              tags$li("Quickly reach your NIH grant funded survey research ",tags$i("response")," quota")
                              , tags$li("Increase study effect sizes and minimize p-values so you can publish in ",tags$i("Nature"))
                            )
                        , h4("Other Data Generators:")
                        , p("Here are a few other data generators I found across the web and used as inspiration. I am not necessarily endorsing any of these.
                            I'm simply offering them up as other potential sources or solutions.")
                        , tags$ul(
                           tags$li("https://mockaroo.com/")
                           , tags$li("https://ebsubudhi.shinyapps.io/DataGeneration/")
                           , tags$li("https://www.onlinedatagenerator.com/")
                             )
                        , tags$script( # adds my name in navbar
                          HTML(
                            paste0("var header = $('.navbar> .container-fluid');header.append('"
                                   , my_navbar_info,"');console.log(header)"))))
               )
               
               )))