
#TO DO:
  # prevent user from deleting the last variable?
  # put dataset size on the preview page so it doesn't re-run with every change!

# eval(parse(text = "1 + 1"))
# sprintf("this is a %s for the %s","test","win")

# phone number generator
# paste(sample(100:999,1),sample(100:999,1),sample(1000:9990,1),sep="-")

# Define server logic required to draw a datatable
shinyServer(function(input, output, clientData, session) {
    
  init_df <- reactive({
      
    n_participants <- 100 # the number of study participants

    init_df <- data.frame(
      participant_id = 1:n_participants # arbitrary unique identifier
      , condition = sample(c("Control","Low Dose","High Dose"),n_participants,replace=TRUE)
      , weight = rnorm(n_participants,145,10)
      , notes = "Lorem Ipsum"
      , stringsAsFactors = FALSE
    )
    
    # force difference means between control and experimental
    init_df$weight <- ifelse(init_df$condition=="Control"
                         , rnorm(n_participants,189,15)
                         , rnorm(n_participants,145,8))
      
      init_df
    })
  
  user_df <- reactive({
    
    # get variable names specified by user
    variables <- All_Inputs() %>%
      filter(grepl("var_name_",input_name,fixed = TRUE))
    
    # duplicate identifiers # need dummy method
    # test_wide <- All_Inputs() %>%
    #   select(-input_name) %>%
    #   spread(input_type,input_value)
    
    user_df <- data.frame(temp_var_placeholder = 1:input$df_rows) # replace with rows specified!
    for(i in variables$input_number){

      #if I organize All_Inputs longways, this would be reduced to one line?
      var_name <- All_Inputs() %>% filter(input_name == paste0("var_name_",i))
      var_type <- All_Inputs() %>% filter(input_name == paste0("var_type_",i))
      
      #going straight to as.integer and as.numeric both caused strange behavior!
      var_min <- as.numeric(as.character((All_Inputs() %>% filter(input_name == paste0("var_min_",i)))$input_value))
      var_max <- as.numeric(as.character((All_Inputs() %>% filter(input_name == paste0("var_max_",i)))$input_value))
      var_mean <- as.numeric(as.character((All_Inputs() %>% filter(input_name == paste0("var_mean_",i)))$input_value))
      var_sd <- as.numeric(as.character((All_Inputs() %>% filter(input_name == paste0("var_sd_",i)))$input_value))

      var_input <- as.character((All_Inputs() %>% filter(input_name == paste0("var_input_",i)))$input_value)
      
      var <- as.character(var_name$input_value)
      # put most frequently used at top to maximize performance!
      if(input[[paste0("var_type_",i)]] == "Phone Numbers" ){
        user_df <- user_df %>% 
          mutate(!!var := sapply(1:input$df_rows,function(x){
            paste(sample(100:999,1),sample(100:999,1),sample(1000:9990,1),sep="-")
            }))

      } else if (input[[paste0("var_type_",i)]] == "Numeric"){
        user_df <- user_df %>% 
          # mutate(!!var := var_max)
          mutate(!!var := qnorm(
            runif(input$df_rows
                  , pnorm(var_min, mean=var_mean, sd=var_sd)
                  , pnorm(var_max, mean=var_mean, sd=var_sd))
            , mean=var_mean, sd=var_sd) )
        
      } else if (input[[paste0("var_type_",i)]] == "Month"){
        if(input[[paste0("var_month_abb_",i)]] == 0){
          month_var <- month.name
        } else {
          month_var <- month.abb
        }
        user_df <- user_df %>% 
          mutate(!!var := sample(month_var,input$df_rows,replace = TRUE))
        
      } else if (input[[paste0("var_type_",i)]] == "States"){
        if(input[[paste0("var_state_abb_",i)]] == 0){
          state_var <- state.name
        } else {
          state_var <- state.abb
        }
        user_df <- user_df %>% 
          mutate(!!var := sample(state_var,input$df_rows,replace = TRUE))
        
      } else if (input[[paste0("var_type_",i)]] == "Sequential Primary Key"){
        user_df <- user_df %>% 
          mutate(!!var := 1:input$df_rows)
        
      }  else if (input[[paste0("var_type_",i)]] == "Long Filler Text"){
        user_df <- user_df %>% 
          mutate(!!var := lapply(1:input$df_rows,function(x) lorem_ipsum[sample(1:length(lorem_ipsum),1)] ))
        
      } else if (input[[paste0("var_type_",i)]] == "Days of Week"){
        if(input[[paste0("var_days_abb_",i)]] == 0){
          day_var <- weekday_full
        } else {
          day_var <- weekday_abb
        }
        user_df <- user_df %>% 
          mutate(!!var := sample(day_var,input$df_rows,replace = TRUE))
        
      } else if (input[[paste0("var_type_",i)]] == "Nominal/Categorical"){
        user_df <- user_df %>% 
          mutate(!!var := lapply(1:input$df_rows,function(x) sample(unlist(strsplit(var_input,",|, | ,")),1) ))
        
      } else if (input[[paste0("var_type_",i)]] == "Date Range"){
        date_range <- input[[paste0("var_input_",i)]]
        user_df <- user_df %>% 
          mutate(!!var := lapply(1:input$df_rows,function(x) sample(seq.Date(as.Date(date_range[1], origin = "1970-01-01")
                                                                                     ,as.Date(date_range[2], origin = "1970-01-01"),1),1)))
      } else if (input[[paste0("var_type_",i)]] == "Names"){
        user_df <- user_df %>% 
          mutate(!!var := paste(sample(names_df$First,input$df_rows,replace=TRUE), sample(names_df$Last,input$df_rows,replace=TRUE)))
      }
      
      

    }
    
    user_df <- user_df %>% select(-temp_var_placeholder)
    # user_df <- apply(user_df,2,as.character) # to flatten out lists
    user_df
  })
  
  # there's probably a better solution than this...
  dl_df <- reactive({
    dl_df <- apply(user_df(),2,as.character) # to flatten out lists
    dl_df
  })
  
    
  # http://haozhu233.github.io/kableExtra/awesome_table_in_html.html
    output$preview_data <- function() {
      user_df() %>%
        slice(1:50) %>%
        knitr::kable("html") %>%
        row_spec(0, bold = T, color = "white", background = "black") %>%
        kable_styling(bootstrap_options = c("striped", "hover")) %>%
        footnote("Only the first 100 rows are shown in the preview.")
    }
    
    observeEvent(input$preview,{
      showModal(modalDialog(easyClose = TRUE,size = "l"
        , title = tags$b("Data Preview")
        , infoBoxOutput('df_columns',width=6)
        # , infoBoxOutput('df_rows')
        , infoBoxOutput('df_size',width = 6)
        , tableOutput('preview_data')
        , class = "on_top"
      ))
    })
    
    output$df_columns <- renderInfoBox({ # number of variables / columns
    
      variables <- (
        All_Inputs() %>%
          filter(grepl("var_name_",input_name,fixed = TRUE)) %>%
          summarise(input_name = n_distinct(input_name)))$input_name

      infoBox(variables,
               title = "Number of Variables"
               , icon=icon("columns")
               , color="black"
               , fill = TRUE)
    })
    
    output$df_variables <- renderText({ # number of variables / columns
        (All_Inputs() %>%
          filter(grepl("var_name_",input_name,fixed = TRUE)) %>%
          summarise(input_name = n_distinct(input_name)))$input_name
    })
    
    # output$df_rows <- renderInfoBox({
    # 
    #   infoBox(
    #     # nrow(init_df()),
    #     sliderInput("df_rows",label=NA,1,1000,value=100,ticks = FALSE),
    #            title = "Number of Rows",
    #            icon=icon("align-justify"),
    #            color="yellow",
    #           fill=TRUE)
    # })
    
    output$df_size <- renderInfoBox({

      infoBox(format(object.size(init_df()),units="Mb"),
               title="Dataset Size",
               icon=icon("expand-arrows-alt"),
               color="blue", fill=TRUE)
    })
    
    All_Inputs <- reactive({
      myvalues <- NULL
      for(i in 1:length(names(input))){
        myvalues <- as.data.frame(rbind(myvalues,(cbind(names(input)[i],input[[names(input)[i]]]))))
      }
      names(myvalues) <- c("input_name","input_value")
      myvalues %>%
        filter(!is.null(input_value)
               , input_value != "") %>%
        mutate(input_number = as.integer(gsub("\\D", "",input_name))
               , input_type = gsub("_[1-9]","",input_name))
      # collect numbers from var_name_
      # filter all inputs to include only those with the ids that exist from var_name_
    })
    
    output$show_inputs <- renderTable({
      All_Inputs() 
    })
    
    # Downloadable csv of selected dataset ----
      # check that no two vars have same name
      # other integrity constraints?
    output$downloadData <- downloadHandler(
      filename = "NP_FDG.csv"
      , content = function(file) {
        write.csv(dl_df(), file, row.names = FALSE)
      }
    )
# dynamic ui ########################################################

    # init a few columns
    observe({
        lapply(names(init_df()),function(x){
          
          # get the number of each init_df column
          # eg names(mtcars); grep("wt",names(mtcars))
          var_id <- grep(x,names(init_df()))
          
          insertUI(
            selector = "#var_header",
            where = "afterEnd",
            ui = init_var(x = x, var_id = var_id)
          )
          
        })
    })
    
    observe({ # init the page with a few types selected
      if(!is.null(input$var_name_4)){
        updateSelectInput(session, "var_type_1",NULL,var_type_selections
                          ,selected = "Sequential Primary Key")
        updateSelectInput(session, "var_type_2",NULL,var_type_selections
                          ,selected = "Nominal/Categorical")
        updateSelectInput(session, "var_type_4",NULL,var_type_selections
                          ,selected = "Long Filler Text")
      }
    })
    
    # use the add button counter to count the number of variables on the page
    variable_count <- reactive({
      input$add + ncol(init_df()) # give the new variable the next available id number
      # inputs <- names(reactiveValuesToList(input))
      # # inputs <- 4
      # max(as.numeric(
      #   gsub(
      #     "\\D", "",inputs))
      #   , na.rm = TRUE) 
    })
    
    # add a new variable row when add button clicked
    # use add button increment value!
    observeEvent(input$add, {
      new_id <- variable_count()
      
      insertUI(
        selector = "#var_header"
        , where = "afterEnd"
        , ui = init_var(x = paste0("variable_",new_id), var_id = new_id)
      )
    })

    # separate observer for adding new variables or removing existing variables
    observe({
      # isolate({
      lapply(1:(variable_count()+1),function(var_id){ 
    
        var_input_id <- paste0("var_input_",var_id)
        # do I need to isolate these?
        # when a row trash icon / remove button is clicked
        observeEvent(input[[paste0("var_delete_",var_id)]], {
          removeUI(
            selector = paste0("#div_var_",var_id)
          )
          # nullify inputs that are removed from ui
          # need to add each input id name to remove...
          # lapply would work here...
          session$sendInputMessage(paste0("var_name_",var_id), list(value = NULL))
          session$sendInputMessage(paste0("var_type_",var_id), list(value = NULL))
          
        })
        
        # do I need isolate() here?
        # or maybe a local reactive?
        # when the variable type is changed
        observeEvent(input[[paste0("var_type_",var_id)]], {
          removeUI(
            selector = paste0("#var_input_col_",var_id)
          )

          insertUI(
            selector = paste0("#var_type_col_",var_id)
            , where = "afterEnd"
            , ui = column(4,id = paste0("var_input_col_",var_id),switch(
              input[[paste0("var_type_",var_id)]]
              , "Sequential Primary Key" = , h6("Sequential integers from 1 to the number of rows. Can serve as a unique ID.")
              , "Numeric" = fluidRow(
                                   column(3,numericInput(paste0("var_min_",var_id), "Min:", value = 0,width='100%'))
                                   ,column(3,numericInput(paste0("var_max_",var_id), "Max:", value = 10,width='100%'))
                                   ,column(3,numericInput(paste0("var_mean_",var_id), "Mean:", value = 5,width='100%'))
                                   ,column(3,numericInput(paste0("var_sd_",var_id), "SD:", value = 1,width='100%'))
                              )

              # I think I should make the ps h6 instead and define custom style for them! eg padding, light gray, etc
              , "Date Range" = dateRangeInput(var_input_id, "")
              , "Nominal/Categorical" = textInput(var_input_id,"","experimental,low dose,high dose")
              , "Phone Numbers" = h6("U.S. Phone Numbers in the format 123-123-1234.")
              , "Long Filler Text" = h6("Sentences from Lorem Ipsum.")
              , "Names" = h6('First and Last names. For example, "John Smith" or "Jane Doe".')
              , "Month" = fluidRow(
                column(4,radioButtons(paste0("var_month_abb_",var_id), label = ""
                                      , choices = list("Full Names" = 0, "Abbreviations" = 1)
                                      , selected = 0))
                , column(8,h6("Month names or abbreviations. For example, 'January' or 'Jan'"))
              )
              , "States" = fluidRow(
                column(4,radioButtons(paste0("var_state_abb_",var_id), label = ""
                                      , choices = list("Full Names" = 0, "Abbreviations" = 1)
                                      , selected = 0))
                , column(8,h6("State names or abbreviations. For example, 'Michigan' or 'MI'"))
              )
              , "Days of Week" = fluidRow(
                column(4,radioButtons(paste0("var_days_abb_",var_id), label = ""
                                      , choices = list("Full Names" = 0, "Abbreviations" = 1)
                                      , selected = 0))
                , column(8,h6("Day names or abbreviations. For example, 'Monday' or 'Mon'"))
              )
            ))
          )
        
        })
        
      }) # lapply
      # }) # isolate
    }) #observe
    
# SANDBOX ###########################################################
#     
    
    # output$dynamic_inputs <- renderUI({
    #   
    #   lapply(names(init_df()),function(x){
    #   tagList(
    #     fluidRow(class = "variable-row"
    #       , column(4
    #              , style = "margin-top: 25px;"
    #              , textInput(paste0(x,grep(x,names(init_df()))), NULL, x))
    #       , column(4
    #                , style = "margin-top: 25px;"
    #                , selectInput(paste0("var_type",grep(x,names(init_df()))), NULL
    #                              # two types of var types: atomic (numeric, character, factor) vs pre-defined (primary key, names, phone numbers)
    #                              , c("Sequential Primary Key","Numeric","Date Range","Character String: Nominal","Character String: Long Text")))
    #     , column(4
    #              , style = "margin-top: 25px;"
    #              , actionButton(paste0("delete_column",x), "Delete Column",icon=icon("trash"),style="background-color: red;")
    #                # , dynamic help buttons based on variable type selection!
    #                )
    #   )
    #   )
    #   })
    # })
    # 
    # output$dynamic_inputs_2 <- renderUI({
    #   lapply(names(init_df()),function(x){
    #     # if (is.null(input[[paste0("var_type",grep(x,names(init_df())))]]))
    #     #   return()
    #     fluidRow(class = "variable-row"
    #       , switch(
    #       input[[paste0("var_type",grep(x,names(init_df())))]] # may need this in a different chunk
    #       , "Sequential Primary Key" = , column(12, style = "padding-top: 25px;",p("Sequential integers from 1 to the number of rows. Can serve as a unique ID."))
    #       , "Numeric" = column(12,sliderInput("dynamic", "",min = 1, max = 20, value = 10))
    #       , "Date Range" = column(12,dateRangeInput("dynamic", ""))
    #     ))
    #     # )
    #   })
    # })
    
    # observeEvent(input$add, {
    #   var_id <- variable_count() + 1
    #   insertUI(
    #     selector = paste0("#var_type_col_",var_id)
    #     , where = "afterEnd"
    #     , ui = column(4,id = paste0("var_input_col_",var_id),switch(
    #       input[[paste0("var_type_",var_id)]]
    #       , "Sequential Primary Key" = , p("Sequential integers from 1 to the number of rows. Can serve as a unique ID.")
    #       , "Numeric" = fluidRow(
    #         column(4,numericInput(paste0("var_input_min_",var_id), "Min:", value = 0,width='100%'))
    #         ,column(4,numericInput(paste0("var_input_max_",var_id), "Max:", value = 10,width='100%'))
    #         ,column(4,numericInput(paste0("var_input_mean_",var_id), "Mean:", value = 10,width='100%'))
    #       )
    #       
    #       , "Date Range" = dateRangeInput(paste0("var_input_",var_id), "")
    #       , "Nominal/Categorical" = textInput(paste0("var_input_",var_id),"","experimental,low dose,high dose")
    #       
    #       # should pull randomly from a full lorem ipsum implementation!
    #       # instead just provide descriptive text that each row will containe random filler text, no need for an input here
    #       , "Long Filler Text" = textInput(paste0("var_input_",var_id),"","Lorem ipsum dolor sit amet, consectetur adipiscing elit")
    #     ))
    #   )
    #   
    # })
    
})