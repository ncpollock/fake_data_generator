
# eval(parse(text = "1 + 1"))
# sprintf("this is a %s for the %s","test","win")

# phone number generator
# paste(sample(100:999,1),sample(100:999,1),sample(1000:9990,1),sep="-")

# Define server logic required to draw a datatable
shinyServer(function(input, output, clientData, session) {
    
  fake_df <- reactive({
      
    n_participants <- 100 # the number of study participants

    fake_df <- data.frame(
      participant_id = 1:n_participants # arbitrary unique identifier
      , condition = sample(c("Control","Low Dose","High Dose"),n_participants,replace=TRUE)
      , weight = rnorm(n_participants,145,10)
      , notes = "Lorem Ipsum"
      , stringsAsFactors = FALSE
    )
    
    # force difference means between control and experimental
    fake_df$weight <- ifelse(fake_df$condition=="Control"
                         , rnorm(n_participants,189,15)
                         , rnorm(n_participants,145,8))
      
      fake_df
    })
    
    output$preview_fake_df <- renderDataTable({
      
      datatable(head(fake_df(),50),filter='top',options=list(scrollX=TRUE)) %>%
        formatStyle(names(fake_df())
          , color="black")

    })
    
    output$df_columns <- renderInfoBox({
      
      infoBox(ncol(fake_df()),
               title = "Number of Columns"
               , icon=icon("columns")
               , color="black"
               , fill = TRUE)
    })
    
    output$df_rows <- renderInfoBox({
      
      infoBox(nrow(fake_df()),
               title = "Number of Rows",
               icon=icon("align-justify"),
               color="yellow",
              fill=TRUE)
    })
    
    output$df_size <- renderInfoBox({

      infoBox(format(object.size(fake_df()),units="Mb"),
               title="Dataset Size",
               icon=icon("expand-arrows-alt"),
               color="blue", fill=TRUE)
    })
    
# dynamic ui ########################################################
    
    output$dynamic_inputs <- renderUI({
      
      lapply(names(fake_df()),function(x){
      tagList(
        fluidRow(class = "variable-row"
          , column(4
                 , style = "margin-top: 25px;"
                 , textInput(paste0(x,grep(x,names(fake_df()))), NULL, x))
          , column(4
                   , style = "margin-top: 25px;"
                   , selectInput(paste0("var_type",grep(x,names(fake_df()))), NULL
                                 # two types of var types: atomic (numeric, character, factor) vs pre-defined (primary key, names, phone numbers)
                                 , c("Sequential Primary Key","Numeric","Date Range","Character String: Nominal","Character String: Long Text")))
        , column(4
                 , style = "margin-top: 25px;"
                 , actionButton(paste0("delete_column",x), "Delete Column",icon=icon("trash"),style="background-color: red;")
                   # , dynamic help buttons based on variable type selection!
                   )
      )
      )
      })
    })
    
    output$dynamic_inputs_2 <- renderUI({
      lapply(names(fake_df()),function(x){
        # if (is.null(input[[paste0("var_type",grep(x,names(fake_df())))]]))
        #   return()
        fluidRow(class = "variable-row"
          , switch(
          input[[paste0("var_type",grep(x,names(fake_df())))]] # may need this in a different chunk
          , "Sequential Primary Key" = , column(12, style = "padding-top: 25px;",p("Sequential integers from 1 to the number of rows. Can serve as a unique ID."))
          , "Numeric" = column(12,sliderInput("dynamic", "",min = 1, max = 20, value = 10))
          , "Date Range" = column(12,dateRangeInput("dynamic", ""))
        ))
        # )
      })
    })

    output$test <- renderText({input$var_type2 })
    
    # init a few columns
    observe({

        lapply(names(fake_df()),function(x){
          
          # eg names(mtcars); grep("wt",names(mtcars))
          var_id <- grep(x,names(fake_df()))
          
          # put in function so I can reuse on add button! x = variables$counter + 1 # + 1 would create conflicts...
          insertUI(
            selector = "#add",
            where = "afterEnd",
            ui = column(12,id = paste0("div_",x)
            , fluidRow(class = "variable-row"
                     , column(3
                              , style = "margin-top: 25px; border-right: 1px dashed black;"
                              , textInput(paste0("var_name_",var_id), NULL, x))
                     , column(4,id = paste0("var_type_col_",var_id)
                              , style = "margin-top: 25px; border-right: 1px dashed black;"
                              , selectInput(paste0("var_type_",var_id), NULL
                                            # two types of var types: atomic (numeric, character, factor) vs pre-defined (primary key, names, phone numbers)
                                            , c("Sequential Primary Key","Numeric","Date Range","Character String: Nominal","Character String: Long Text")))
                     , column(4,id = paste0("var_input_col_",var_id),sliderInput(paste0("var_input_",var_id), "",min = 1, max = 20, value = 10))
                     , column(1
                              , style = "margin-top: 25px;"
                              , actionButton(paste0("var_delete_",var_id), "",icon=icon("trash"),style="background-color: red;")
                              # , dynamic help buttons/tooltips based on variable type selection!
                     )
            )
          ))
          
          # when a row trash icon / remove button is clicked
          observeEvent(input[[paste0("var_delete_",var_id)]], {
            removeUI(
              selector = paste0("#div_",x)
            )
            variables$count <- variables$count - 1 # decrement the counter by 1
          })
          
          observeEvent(input[[paste0("var_type_",var_id)]], {
            removeUI(
              selector = paste0("#var_input_col_",var_id)
            )
            insertUI(
              # selector = paste0("#var_type_",var_id),
              selector = paste0("#var_type_col_",var_id)
              , where = "afterEnd"
              , ui = column(4,id = paste0("var_input_col_",var_id),switch(
                input[[paste0("var_type_",var_id)]]
                , "Sequential Primary Key" = , p("Sequential integers from 1 to the number of rows. Can serve as a unique ID.")
                , "Numeric" = sliderInput(paste0("var_input_",var_id), "",min = 1, max = 20, value = 10)
                , "Date Range" = dateRangeInput(paste0("var_input_",var_id), "")
                , "Character String: Nominal" = textInput(paste0("var_input_",var_id),"","experimental,low dose,high dose")
                , "Character String: Long Text" = textInput(paste0("var_input_",var_id),"","Lorem ipsum dolor sit amet, consectetur adipiscing elit")
              ))
            )
          })
          
        })

    })
    
    AllInputs <- reactive({
      x <- reactiveValuesToList(input)
      data.frame(
        names = names(x),
        values = unlist(x, use.names = FALSE)
      )
    })

    output$all_inputs_df <- renderDataTable({
      datatable(
        data.frame(
          names = names(reactiveValuesToList(input)),
          values = reactiveValuesToList(input)
        )
      )
    })
    
    output$counter <- renderText({variables$count})
    
    output$all_inputs <- renderText({
      names(reactiveValuesToList(input))
      
      # pull only var_names
      # grepl("var_type_",c("a","var_type_","c"))
      # find number of variables
      # max(as.numeric(gsub("\\D", "", c("var_name_1","var_type_2"))))
      # build the data frame on download or on preview via observe
    }) 
    
    # keep a counter instead of reading reactive values?
    variables <- reactiveValues(count = 4) # Defining & initializing the reactiveValues object
    
    observeEvent(input$add, {
      variables$count <- variables$count + 1     # if the add button is clicked, increment the value by 1 and update it
    })

    
# SANDBOX ###########################################################
#     
#     output$inspect_vars <- renderUI({
#       
#       variable_output <- lapply(names(file_df()), function(i) {
# 
#         level_counts <- paste0("table1_",i)
#         level_detail <- paste0("table1_1_",i)
#         stat_summaries <- paste0("table2_",i)
#         textname_var <- paste("text", i, sep="")
#         selected_row <- paste("sr", i, sep="")
#         
#         inspect_histogram <- paste0("plot1_",i)
#         inspect_bar <- paste0("plot2_",i)
#         
#           if(is.numeric(file_df()[[i]])){
#             list(box(width = 12,collapsible = TRUE,collapsed = FALSE,solidHeader = TRUE,status = 'success',
#                        title=p(title_collapse(i),": Variable is numeric"),
#                        box(width=4,DT::dataTableOutput(stat_summaries)),
#                      box(width=8,plotOutput(inspect_histogram),
#                          sliderInput(paste0("inspect_bin",i),
#                                      "Bins:",
#                                      # should be number of non-missing values, not number of rows...
#                                      min = 1,  max = 50,value = ifelse(2*nrow(file_df())^(1/3)>50,50,2*nrow(file_df())^(1/3))
#                                      ))
#                      ))
# 
#           } else {
#             list(
#               box(width = 12,collapsible = TRUE,collapsed = FALSE,solidHeader = TRUE,status = 'warning',
#                   title=p(title_collapse(i),": Variable is not numeric"),
#                   box(DT::dataTableOutput(level_detail),width=4),
#                   box(DT::dataTableOutput(level_counts),width=8)
#               # ,box(width=8,plotOutput(inspect_bar)) # if I want to go back to plots for character vars
#               )) }
#         
#         # could add in other checks for dates, booleans, lat/lon/geo, etc
#         
#           # ,list(p("Each variable gets either a plot or a table, but every variable gets this nice paragraphs.")))
#       })
#       local_reactive_inspect_vars()
#       do.call(tagList, variable_output)
#       # }
#     })
#     
#     
#     local_reactive_inspect_vars <- reactive({
#       for (i in names(file_df())) {
# 
#         # Need local so that each item gets its own number. Without it, the value
#         # of i in the renderPlot() will be the same across all instances, because
#         # of when the expression is evaluated.
#         local({
#           my_i <- i
#           j <- sym(i) # symbolize quoted variable name for use in dplyr programming
#           
#           #get variable name
#           textname_var <- paste("text", my_i, sep="")
#           output[[textname_var]] <- renderText(my_i)
#           
#           level_counts <- paste0("table1_", my_i)
#           level_detail <- paste0("table1_1_", my_i)
#           stat_summaries <- paste0("table2_", my_i)
#           inspect_histogram <- paste0("plot1_",my_i)
#           inspect_bar <- paste0("plot2_",my_i)
#           
#           #get selected table indice
#           selected_row <- paste("sr", my_i, sep="")
#           output[[selected_row]] <- renderText(
#             as.character(
#               distinct(file_df(),!!j)[input[[paste0("table1_",my_i,"_rows_selected")]],]
#             )
#             )
#           
#           #initialize empty space
#           output$none <- renderPlot({})
#           
#           output[[level_detail]] <- DT::renderDataTable({
#             
#             tdata <- file_df() %>% 
#               count(!!j)
#             tdata <- bind_rows(
#               c(detail = "Levels",
#                 value = nrow(tdata)),
#               c(detail = "Level Count Range",
#                 value = paste0(range(tdata$n),collapse = " - ")),
#               c(detail = "Missing/Blank Values",
#                 value = (tdata %>% filter(is.na(!!j)))$n))
#             
#             datatable(tdata
#                       ,rownames = FALSE
#                       ,colnames = c(paste0(my_i," Detail"),
#                                     "Value")
#                       ,options = list(dom='t'
#                                       ,initComplete = dt_column_head
#                                       ,ordering=FALSE
#                                       ,columnDefs = list(list(className = 'dt-center', targets = 1))))
#           })
#           
#           output[[level_counts]] <- DT::renderDataTable({
#             tdata <- file_df() %>% count(!!j) %>% mutate(pct = percent(n/sum(n),2)) %>% arrange(desc(n))
#             datatable(tdata # should save this as a more global var, used several times
#                       ,rownames = FALSE
#                       ,colnames = c(paste0(my_i," Level"),
#                                     "Count",
#                                     "Percent")
#                       ,options = list(dom='tpf'
#                                       ,initComplete = dt_column_head
#                                       ,search = list(regex = TRUE, caseInsensitive = FALSE)
#                                       ,columnDefs = list(list(className = 'dt-center', targets = 1:2)))) %>%
#               formatStyle(
#                 my_i,'n',
#                 background = styleColorBar(range(0,max(tdata$n)), v_light_gray2,angle=270),
#                 backgroundSize = '100% 75%',
#                 backgroundRepeat = 'no-repeat',
#                 backgroundPosition = 'center',
#                 fontWeight = 'bold')
#           })
#           
#           output[[stat_summaries]] <- DT::renderDataTable({
#             
#             tdata <- file_df() %>%
#               summarise(Min = min(!!j,na.rm = TRUE),
#                         Max = max(!!j,na.rm = TRUE),
#                         Mean = mean(!!j,na.rm = TRUE),
#                         SD = sd(!!j,na.rm = TRUE),
#                         Median = median(!!j,na.rm = TRUE),
#                         Distinct = n_distinct(!!j),
#                         Missing = sum(ifelse(is.na(!!j),1,0))) %>% 
#               gather(stat,value) %>% 
#               filter(!is.na(value))
#             
#             datatable(tdata
#                       ,rownames = FALSE
#                       ,colnames = c("Statistic",
#                                     "Value")
#                       ,options = list(
#                         paging = FALSE
#                         ,searching = FALSE
#                         ,pageLength = nrow(tdata) 
#                         ,initComplete = dt_column_head
#                       ))
#           })
#           
#           output[[inspect_histogram]] <- renderPlot({
#             
#             pdata <- file_df() %>%
#               mutate(fill_val = ifelse(abs(!!j) > (mean(!!j) + (sd(!!j))),"Tail","Not"))
#             
#             plot_grid(ggplot(pdata,aes(x=!!j)) +
#                         geom_histogram(bins = input[[paste0("inspect_bin",my_i)]]) +
#                         #stat lines
#                         geom_vline(aes(xintercept = median(!!j,na.rm = TRUE),color="median"),size = 2) +
#                         geom_vline(aes(xintercept = mean(!!j,na.rm = TRUE),color="mean"),size = 2) +
#                         
#                         # cutoff lines
#                         geom_vline(aes(xintercept=(mean(!!j) + sd(!!j)),color="cutoff"),size=1.5) +
#                         geom_vline(aes(xintercept=(mean(!!j) - sd(!!j)),color="cutoff"),size=1.5) +
#                         
#                         scale_color_manual(name = "Stats", 
#                                            values = c(median = "black", mean = "orange",cutoff = "red")) +
#                         # to maintain alignment with boxplot
#                         scale_x_continuous(limits = c(min(file_df()[[my_i]],na.rm = TRUE)
#                                                       ,max(file_df()[[my_i]],na.rm = TRUE))) +
#                         my_theme +
#                         theme(legend.position = 'top'),
#                       ggplot(pdata) +
#                         geom_boxplot(aes(y=!!j,x=1),fill = "gray50") +
#                         geom_hline(aes(yintercept = median(!!j,na.rm = TRUE),color="median"),size = 1.7) +
#                         geom_hline(aes(yintercept = mean(!!j,na.rm = TRUE),color="mean"),size = 2) +
#                         scale_color_manual(name = "Stats", values = c(median = "black", mean = "orange")) +
#                         coord_flip() +
#                         my_theme +
#                         theme(axis.title = element_blank()),
#                       rel_heights=c(3,1),
#                       align = "v",
#                       nrow = 2
#             )
#           })
#           
# 
#         })
#       }
#     })
#     
#     
})