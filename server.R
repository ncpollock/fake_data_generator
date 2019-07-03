
#TO DO:
  # prevent user from deleting the last variable?

# eval(parse(text = "1 + 1"))
# sprintf("this is a %s for the %s","test","win")

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
    
    # force different means between control and experimental
    init_df$weight <- ifelse(init_df$condition=="Control"
                         , rnorm(n_participants,189,15)
                         , rnorm(n_participants,145,8))
      
      init_df
    })
  
  user_df <- reactive({
    
    # get variable names specified by user
    variables <- All_Inputs() %>%
      filter(grepl("var_name_",input_name,fixed = TRUE))
    
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
      if(input[[paste0("var_type_",i)]] == "Phone Number" ){
        user_df <- user_df %>% 
          mutate(!!var := sapply(1:input$df_rows,function(x){
            paste(sample(100:999,1),sample(100:999,1),sample(1000:9990,1),sep="-")
            }))

      } else if (input[[paste0("var_type_",i)]] == "Numeric"){
        user_df <- user_df %>% 
          mutate(!!var := qnorm(
            runif(input$df_rows
                  , pnorm(var_min, mean=var_mean, sd=var_sd)
                  , pnorm(var_max, mean=var_mean, sd=var_sd))
            , mean=var_mean, sd=var_sd) )
        
          if(input[[paste0("var_num_type_",i)]]==1) user_df[[var]] <- round(user_df[[var]])
        
      } else if (input[[paste0("var_type_",i)]] == "Month"){
        if(input[[paste0("var_month_abb_",i)]] == 0){
          month_var <- month.name
        } else {
          month_var <- month.abb
        }
        user_df <- user_df %>% 
          mutate(!!var := sample(month_var,input$df_rows,replace = TRUE))
        
      } else if (input[[paste0("var_type_",i)]] == "State"){
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
        
      } else if (input[[paste0("var_type_",i)]] == "Day of Week"){
        if(input[[paste0("var_days_abb_",i)]] == 0){
          day_var <- weekday_full
        } else {
          day_var <- weekday_abb
        }
        user_df <- user_df %>% 
          mutate(!!var := sample(day_var,input$df_rows,replace = TRUE))
        
      } else if (input[[paste0("var_type_",i)]] == "Nominal/Categorical"){
        user_df <- user_df %>% 
          mutate(!!var := as.factor(
            unlist(lapply(1:input$df_rows,function(x) sample(unlist(strsplit(var_input,",|, | ,")),1) )))
          )
      } else if (input[[paste0("var_type_",i)]] == "Date Range"){
        date_range <- input[[paste0("var_input_",i)]]
        user_df <- user_df %>% 
          mutate(!!var := format(
            as.Date(unlist(lapply(1:input$df_rows,function(x) sample(seq.Date(date_range[1]
                                                                              , date_range[2],1),1))), origin = "1970-01-01")
            , input[[paste0("var_date_format_",i)]])
            )
      } else if (input[[paste0("var_type_",i)]] == "Name"){
        user_df <- user_df %>% 
          mutate(!!var := paste(sample(names_df$First,input$df_rows,replace=TRUE), sample(names_df$Last,input$df_rows,replace=TRUE)))
      } else if (input[[paste0("var_type_",i)]] == "Custom R Code"){
        eval(parse(
          text = paste0("user_df <- user_df %>%
                        mutate(!!var := ",var_input,")")
          ))
      } else if (input[[paste0("var_type_",i)]] == "Race"){
        user_df <- user_df %>% 
          mutate(!!var := sample(
            c("White","Black","Hispanic","Asian","Native Hawaiian/Pacific Islander","American Indian/Alaska Native","Two or more races")
            , input$df_rows, replace = TRUE))
      } else if (input[[paste0("var_type_",i)]] == "Email Address"){
        # use words from lorem ipsum for email usernames
        words <- lorem_ipsum %>% strsplit(" ") %>% unlist() %>% gsub(pattern = "\\W",replacement = "")
        usernames <- words[nchar(words) > 5]
        
        user_df <- user_df %>% 
          mutate(!!var := paste0(
            sample(
              c(paste0(names_df$First,sample(1:99,length(names_df$First))),names_df$Last,usernames),input$df_rows,replace=TRUE)
            , sample(c("@gmail.com","@hotmail.com","@school.edu","@army.gov","@store.com","@wowway.com","@state.gov")
              , input$df_rows, replace = TRUE)))
      }
      
      
    } # loop through variables
    
    # HARDCODE TEST 1 ##############################
    if(input$activate_ML_1){
    out_value <- user_df[[input$ML_outcome_1]]
    # assoc_strength <- max(out_value)/10 # 1:10?
    # assoc_strength <- min(out_value) + (max(out_value)*(input$ML_strength_1/100))
    # assoc_strength <- mean(out_value)*(input$ML_strength_1/100)
    assoc_strength <- (max(out_value)-min(out_value))*(input$ML_strength_1/100)
    # assoc_strength <- input$ML_strength_1/100
    
    # variability <- sd(out_value)
    # variability <- mean(out_value)*((input$ML_variability_1/100)*4)
    variability <- mean(out_value)*input$ML_variability_1 * 0.4
    
    # adjusted_error <- data.frame(error = rnorm(nrow(user_df),0,variability)) 
    error <- rnorm(nrow(user_df),0,variability)
    # n_violations <- nrow(adjusted_error %>% filter(error < min(out_value) | error > max(out_value)))
    # adjusted_error <- adjusted_error %>%
    #   mutate(error = ifelse(error < min(out_value)
    #                         | error > max(out_value)
    #                         , sample(min(out_value):max(out_value),n_violations,replace = TRUE)
    #                         , error))
      # user_df[[input$ML_predictor_1]] <- as.factor(user_df[[input$ML_predictor_1]])
      user_df[[input$ML_outcome_1]] <- normalize(assoc_strength*as.numeric(as.factor(user_df[[input$ML_predictor_1]])) + error
                                                 , range_min = var_min
                                                 , range_max = var_max)
                                                 # , range_min = min(out_value)
                                                 # , range_max = min(out_value) + (max(out_value)*(input$ML_strength_1/100))) + error

      # HARDCODE TEST 2 ###############################
      
      if(input$activate_ML_2){
      eval(parse(text = paste0(
        "tree_m <- rpart("
        , input$ML_outcome_2
        , " ~ "
        , paste(input$ML_predictor_2, collapse = " + ")
        , ", data=user_df)"
      )))
      
        out_value <- user_df[[input$ML_outcome_2]]
      # variability <- mean(out_value)*input$ML_strength_2
      
      # adjusted_error <- data.frame(error = rnorm(nrow(user_df),0,variability)) 
      # error <- rnorm(nrow(user_df),0,variability)
      error <- sample(
        c(0,rnorm(nrow(user_df),0,sd(out_value)))
        , replace = TRUE
        , size = nrow(user_df)
        , prob = c(input$ML_strength_2,rep(100-input$ML_strength_2,nrow(user_df))))
        
      ## Get fitted class values
      fitclass_tree <- predict(tree_m)
      user_df[[input$ML_outcome_2]] <- fitclass_tree + error
      }
    }
    
    user_df <- user_df %>% select(-temp_var_placeholder)
    user_df
  })
  
  # there's probably a better solution than this...
  dl_df <- reactive({
    dl_df <- apply(user_df(),2,as.character) # to flatten out lists
    dl_df
  })
  
  output$lm_plot <- renderPlot({ # visualize forced association, put in tabset?
    validate(
      need(!is.null(input$ML_predictor_1), "")
    )
    
    if(is.numeric(user_df()[[input$ML_predictor_1]])) {
    
    ggplot(user_df(),aes_string(input$ML_predictor_1,input$ML_outcome_1)) +
      geom_point() +
      # coord_cartesian(ylim = c(90, 230)) + # for testing
      geom_smooth(method='lm') +
      my_theme
  } else {
    plot(user_df()[[input$ML_predictor_1]],user_df()[[input$ML_outcome_1]])
  }
  })
  
  output$tree_plot <- renderPlot({
    
    # tree_test <- rpart(price ~ cut + color + clarity + depth + table + x + y + z,data=diamonds)
    
    eval(parse(text = paste0(
      "tree_preview <- rpart("
      , input$ML_outcome_2
      , " ~ "
      , paste(input$ML_predictor_2, collapse = " + ")
      , ", data=user_df())"
    )))
    
    prp(tree_preview,type=2, extra="auto",branch=.5)
    
  })
  
    
  # http://haozhu233.github.io/kableExtra/awesome_table_in_html.html
    output$preview_data <- function() {
      user_df() %>%
        slice(1:100) %>%
        knitr::kable("html") %>%
        row_spec(0, bold = T, color = "white", background = "black") %>%
        kable_styling(bootstrap_options = c("striped", "hover")) %>%
        scroll_box(width = "100%",height = "700px") %>%
        footnote("Only the first 100 rows are shown in the preview.")
    }
    
    # output$preview_data_dt <- DT::renderDataTable({
    #   datatable(user_df())
    # })
    
    observeEvent(input$preview,{
      showModal(modalDialog(easyClose = TRUE,size = "l"
        , title = tags$b("Data Preview")
        , infoBoxOutput('df_columns',width=6)
        # , infoBoxOutput('df_rows')
        , infoBoxOutput('df_size',width = 6)
        , p("Note: Only the first 100 rows are shown in the preview.")
        , tableOutput('preview_data')
        # , DT::dataTableOutput('preview_data_dt')
        , class = "on_top"
      ))
    })
    
    observeEvent(input$ML_preview_1,{
      showModal(modalDialog(easyClose = TRUE,size = "l"
                            , title = tags$b("Preview Simple Bivariate Association")
                            , if(input$activate_ML_1==TRUE){
                              plotOutput("lm_plot")
                              } else {p("You did not yet mark this association as active.")}
                            , class = "on_top"
      ))
    })
    
    observeEvent(input$ML_preview_2,{
      showModal(modalDialog(easyClose = TRUE,size = "l"
                            , title = tags$b("Preview Association")
                            , if(input$activate_ML_2 == TRUE){
                              plotOutput("tree_plot")
                            } else { p("You did not yet mark this association as active.") }
                            , class = "on_top"
      ))
    })
    
    output$df_columns <- renderInfoBox({ # number of variables / columns
    
      variables <- (
        All_Inputs() %>%
          filter(grepl("var_name_",input_name,fixed = TRUE)) %>%
          summarise(input_name = n_distinct(input_name)))$input_name

      infoBox(variables,
               title = "Variables"
               , icon=icon("columns")
               , color="black"
               , fill = TRUE)
    })
    
    output$df_variables <- renderText({ # number of variables / columns
        (All_Inputs() %>%
          filter(grepl("var_name_",input_name,fixed = TRUE)) %>%
          summarise(input_name = n_distinct(input_name)))$input_name
    })
    
    output$df_size <- renderInfoBox({

      infoBox(format(object.size(user_df()),units="auto"),
               title="Dataset Size",
               icon=icon("expand-arrows-alt"),
               color="blue", fill=TRUE)
    })
    
    All_Inputs <- reactive({
      myvalues <- NULL
      for(i in 1:length(names(input))){
        if(ncol(cbind(names(input)[i],input[[names(input)[i]]])) == 2){ # only if input value is not null
        myvalues <- as.data.frame(rbind(myvalues,(cbind(names(input)[i],input[[names(input)[i]]]))))
        }
      }
      names(myvalues) <- c("input_name","input_value")
      myvalues %>%
        filter(!is.null(input_value)
               , input_value != "") %>%
        mutate(input_number = as.integer(gsub("\\D", "",input_name))
               , input_type = gsub("_[1-9]","",input_name))
    })
    
    output$show_inputs <- renderTable({
      All_Inputs() 
    })
    
    # Downloadable csv of selected dataset
      # check that no two vars have same name?
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
    
    observe({
    
      row_ids <- 1:(variable_count()+1)
      lapply(row_ids,function(var_id){ 
    
        var_input_id <- paste0("var_input_",var_id)

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
        
        # when the variable type is changed
        observeEvent(input[[paste0("var_type_",var_id)]], {
          removeUI(
            selector = paste0("#var_input_col_",var_id)
          )

          insertUI(
            selector = paste0("#var_type_col_",var_id)
            , where = "afterEnd"
            , ui = column(6,id = paste0("var_input_col_",var_id),switch(
              input[[paste0("var_type_",var_id)]]
              , "Email Address" = h6("Random Email Addresses with various domains e.g., example@gmail.com, example@hotmail.com, example@school.edu, etc.")
              , "Race" = h6("Race and Ethnicity descriptions conforming to the "
                            , a(href="https://nces.ed.gov/ipeds/report-your-data/race-ethnicity-collecting-data-for-reporting-purposes"
                                , target="_blank"
                                , "U.S. Department of Education guidelines.")
                            )
              , "Custom R Code" = fluidRow(
                column(11,textInput(var_input_id,"Code is assigned in a dplyr::mutate() function eg mutate(!!var = [Your code here])",width="100%"
                                            , sample(
                                              c("ifelse(condition=='control','Check this out!',scales::dollar(weight))"
                                                , "n():1"
                                                , "pi"),1)))
                , column(1, style = "margin-top: 25px;",actionButton(paste0("var_R_help_",var_id), "",icon=icon("question"),style="background-color: gray;")))
              , "Sequential Primary Key" = h6("Sequential integers from 1 to the number of rows. Can serve as a unique ID.")
              , "Numeric" = fluidRow(
                                   column(2,numericInput(paste0("var_min_",var_id), "Min:", value = 90,width='100%'))
                                   ,column(2,numericInput(paste0("var_max_",var_id), "Max:", value = 230,width='100%'))
                                   ,column(2,numericInput(paste0("var_mean_",var_id), "Mean:", value = 165,width='100%'))
                                   ,column(2,numericInput(paste0("var_sd_",var_id), "SD:", value = 15,width='100%'))
                                   ,column(4,radioButtons(paste0("var_num_type_",var_id), label = ""
                                                          , choices = list("Integer (eg 1234)" = 1, "Float (eg 1.234)" = 0)
                                                          , selected = 1))
                              )
              , "Date Range" = fluidRow(
                column(6,dateRangeInput(var_input_id, "",start = Sys.Date() - 365,end = Sys.Date()))
                , column(6,selectInput(paste0("var_date_format_",var_id), "Date Format:"
                                       , list(
                                        "2019-06-15" = "%Y-%m-%d"
                                        , "06/15/2019" = "%m/%d/%Y"
                                        , "06/15/19" = "%D"
                                        , "Saturday, June 15, 2019" = "%A, %B %d, %Y"
                                        , "Sat Jun 15" = "%a %b %d"
                                       )
                                       )))
              , "Nominal/Categorical" = fluidRow(
                column(11,textInput(var_input_id,"Enter each category separated by a comma. eg Dog,Cat,Fish",width="100%","control,low dose,high dose"))
                , column(1, style = "margin-top: 25px;",actionButton(paste0("var_cat_help_",var_id), "",icon=icon("question"),style="background-color: gray;")))
              , "Phone Number" = h6("U.S. Phone Numbers in the format 123-123-1234.")
              , "Long Filler Text" = h6("Sentences from Lorem Ipsum.")
              , "Name" = h6('First and Last names. For example, "John Smith" or "Jane Doe".')
              , "Month" = fluidRow(
                column(4,radioButtons(paste0("var_month_abb_",var_id), label = ""
                                      , choices = list("Full Names" = 0, "Abbreviations" = 1)
                                      , selected = 0))
                , column(8,h6("Month names or abbreviations. For example, 'January' or 'Jan'"))
              )
              , "State" = fluidRow(
                column(4,radioButtons(paste0("var_state_abb_",var_id), label = ""
                                      , choices = list("Full Names" = 0, "Abbreviations" = 1)
                                      , selected = 0))
                , column(8,h6("State names or abbreviations. For example, 'Michigan' or 'MI'"))
              )
              , "Day of Week" = fluidRow(
                column(4,radioButtons(paste0("var_days_abb_",var_id), label = ""
                                      , choices = list("Full Names" = 0, "Abbreviations" = 1)
                                      , selected = 0))
                , column(8,h6("Day names or abbreviations. For example, 'Monday' or 'Mon'"))
              )
            ))
          )
        }) # observeEvent when variable type is changed
        
        # modal help boxes
        observeEvent(input[[paste0("var_cat_help_",var_id)]],{
          showModal(modalDialog(easyClose = TRUE,size = "l"
                                , title = tags$b("Instructions")
                                , p("Enter each category separated by a comma. 
                                    For example, if you wanted to create a variable to contain types of household pets,
                                    you might enter: Dog,Cat,Fish")
                                , class = "on_top"
          ))
        })
        observeEvent(input[[paste0("var_R_help_",var_id)]],{
          showModal(modalDialog(easyClose = TRUE,size = "l"
                                , title = tags$b("Instructions")
                                , p("The Custom R Code variable type allows you to create your own custom variables using the R language! 
                                    The text you enter is evaluated in a dplyr::mutate() statement as follows: 
                                    mutate(data, [my_variable] = [The Custom R Code enterred]). Some possible use cases
                                    include feature engineering (you can refer to other variables you have defined by name without quotes)
                                    , custom relationships (eg using conditional logic or linear regression predictions)
                                    , or custom formatting (eg scales::dollar([my_numeric_variable]).")
                                , class = "on_top"
                                ))
        })
        
        # update ML options when variable names are updated
        observeEvent(input[[paste0("var_name_",var_id)]], {
          
            isolate({
              all_variables <- (
                All_Inputs() %>%
                  filter(input_type == "var_name")
              )$input_value
              
              ok_variables <- (
                All_Inputs() %>%
                  filter(input_type == "var_type"
                         & !(input_value %in% c(
                           "Long Filler Text","Names","Phone Numbers","Email Address")))
              )$input_number
              
              num_variables <- (
                All_Inputs() %>%
                  filter(input_type == "var_type"
                         & (input_value %in% c(
                           "Numeric")))
              )$input_number
              
              num_outcome_variables <- (
                All_Inputs() %>%
                  filter(input_type == "var_name"
                         & input_number %in% num_variables)
              )$input_value
              
              simple_variables <- (
                All_Inputs() %>%
                  filter(input_type == "var_name"
                         & input_number %in% ok_variables)
              )$input_value
            })

          for(i in 1){
          updateSelectInput(session, paste0("ML_predictor_",i),NULL,simple_variables
                            ,selected = simple_variables[1])
            updateSelectInput(session, paste0("ML_outcome_",i),NULL,num_outcome_variables
                              ,selected = num_outcome_variables[1])
          }
            
            for(i in 2){
              updateSelectInput(session, paste0("ML_predictor_",i),NULL,simple_variables
                                ,selected = simple_variables[1])
              updateSelectInput(session, paste0("ML_outcome_",i),NULL,simple_variables
                                ,selected = simple_variables[1])
            }

        }) # observeEvent when variable names change
        
      }) # lapply
    }) #observe
    
    
    # Associations ###########################################################
    
    output$Associations <- renderUI({
      
      # so right vars populate at start
      input$var_type_1
      
      isolate({
        ok_variables <- (
          All_Inputs() %>%
            filter(input_type == "var_type"
                   & !is.null(input_type)
                   & !is.na(input_type)
                   & !(input_value %in% c(
                     "Long Filler Text","Names","Phone Numbers","Email Address")))
        )$input_number
        
        simple_variables <- (
          All_Inputs() %>%
            filter(input_type == "var_name"
                   , input_number %in% ok_variables)
        )$input_value
        
        num_variables <- (
          All_Inputs() %>%
            filter(input_type == "var_type"
                   & (input_value %in% c(
                     "Numeric")))
        )$input_number

        num_outcome_variables <- (
          All_Inputs() %>%
            filter(input_type == "var_name"
                   & input_number %in% num_variables)
        )$input_value
      })
      
      fluidRow(column(12,id = paste0("div_ML_",1)
             , fluidRow(class = "variable-row"
                        , column(1, align="center"
                                 , style = "margin-top: 25px; border-right: 1px dashed black;"
                                 , checkboxInput("activate_ML_1", label = "",width="100%"))
                        , column(3, style = "margin-top: 25px; border-right: 1px dashed black;"
                                 , selectInput(paste0("ML_predictor_",1),NULL
                                               , simple_variables
                                               , multiple=FALSE))
                        , column(3
                                 , style = "margin-top: 25px; border-right: 1px dashed black;"
                                 , selectInput(paste0("ML_outcome_",1),NULL
                                               , num_outcome_variables))
                        , column(1, style = "margin-top: 25px; border-right: 1px dashed black;"
                                 , actionButton(paste0("ML_preview_",1), "",icon=icon("eye"),style="background-color: gray;"))
                        , column(2, sliderInput(paste0("ML_strength_",1), "Strength",min = 0, max = 100, value = 80)
                        )
                        , column(2
                                 , sliderInput(paste0("ML_variability_",1), "Variability",min = 0, max = 20, value = 3)
                        )
                        
                        # , column(1
                        #          , style = "margin-top: 25px;"
                        #          , actionButton(paste0("ML_delete_",1), "",icon=icon("trash"),style="background-color: red;")
                        # )
             )
      )
      , column(12,id = paste0("div_ML_",2)
               , fluidRow(class = "variable-row"
                          , column(1,align="center"
                                   , style = "margin-top: 25px; border-right: 1px dashed black;"
                                   , checkboxInput("activate_ML_2", label = "",width="100%"))
                          , column(3
                                   , style = "margin-top: 25px; border-right: 1px dashed black;"
                                   , selectInput(paste0("ML_predictor_",2),NULL
                                                 , simple_variables
                                                 , multiple=TRUE))
                          , column(3,id = paste0("outcome_",2)
                                   , style = "margin-top: 25px; border-right: 1px dashed black;"
                                   , selectInput(paste0("ML_outcome_",2), NULL
                                                 , simple_variables))
                          , column(1, style = "margin-top: 25px; border-right: 1px dashed black;"
                                   , actionButton(paste0("ML_preview_",2), "",icon=icon("eye"),style="background-color: gray;"))
                          , column(4,id = paste0("ML_input_col_",2)
                                   , sliderInput(paste0("ML_strength_",2), "Strength",min = 0, max = 100, value = 80)
                          )
                          # , column(1
                          #          , style = "margin-top: 25px;"
                          #          , actionButton(paste0("ML_delete_",2), "",icon=icon("trash"),style="background-color: red;")
                          # )
               )
      ))
    })
    
    
}) # END