
# eval(parse(text = "1 + 1"))
# sprintf("this is a %s for the %s","test","win")

# phone number generator
# paste(sample(100:999,1),sample(100:999,1),sample(1000:9990,1),sep="-")

# Define server logic required to draw a datatable
shinyServer(function(input, output, clientData, session) {
    
  fake_df <- reactive({
      
    n_students <- 1000 # the number of grads per cohort
    
    # five years of grads where:
    # the suffix 10 = winter semester grads
    # the suffix 40 = fall semester grads
    terms <- sort(rep(c(paste0(2014:2018,"10"),paste0(2013:2017,"40")),n_students))
    n_terms <- length(unique(terms)) #number of terms generated
    
    air_df <- data.frame( # make it a dataframe
      student_id = 1:(n_students*n_terms), # arbitrary unique identifier
      term_code = terms, # assign terms as term_code
      
      # randomly assign one of the three schools, do it for every grad for every term
      # eg "BA" = Business Administration
      school = sample(c("BA","AS","EG"),size=n_students*n_terms,replace=TRUE),
      
      # randomly assign an admit code, do it for every grad for every term,
      # but repeat the same order term after term
      ATYP = rep(sample(c("F","T"),size=n_students,replace=TRUE),n_terms),
      
      # randomly assign a binary sex code, do it for every grad for every term,
      # but repeat the same order term after term 
      # and force the porportion to be %55 Male
      sex = rep(sample(c("M","F"),prob=c(.55,.45),size=n_students,replace=TRUE),n_terms),
      
      # randomly assign an residency code (eg in-state), do it for every grad for every term,
      # but repeat the same order term after term
      residency = rep(sample(c("I","O"),size=n_students,replace=TRUE),n_terms),
      
      # number of times a grad met with career services (from 0 to 10 times)
      # force a random uniform distribution across the dataset.
      career_use = round(runif(n_students*n_terms,0,10)),
      
      # randomly assign a post-graduation outcome, do it for every grad for every term,
      # and force the porportion to be %64 Employed, %27 Grad School, etc
      post_grad_outcome = sample(c("Employed","Grad School","Still Seeking","Other"),prob=c(.64,.27,.07,.02),size=n_students*n_terms,replace=TRUE),
      
      # these vars are placeholders
      gpa = 0,
      salary = 0,
      
      # let the strings be chracters, not factors
      stringsAsFactors = FALSE
    )
    
    # use a linear regression equation to force relationship
    # between career_use and gpa for Freshman admits
    air_df$gpa <- ifelse(air_df$ATYP=="F",
                         #intercept + slope*value + error/noise
                         #different slopes for a nice interaction effect
                         2.5 + .15*air_df$career_use + rnorm(n_students*n_terms,0,1),
                         3.7 + .01*air_df$career_use + rnorm(n_students*n_terms,0,1))
    # adjust for impossible values (ie a GPA greater than 4.0 or less than zer0)
    air_df$gpa = ifelse(air_df$gpa>4,runif(n_students*n_terms,3.2,4),air_df$gpa) # keep gpa under 4
    air_df$gpa = ifelse(air_df$gpa<0,runif(n_students*n_terms,0,1),air_df$gpa) # keep gpa above 0
    
    air_df$career_use <- ifelse(air_df$post_grad_outcome=="Employed",
                                #intercept + slope*value + error/noise
                                #different slopes for a nice interaction effect
                                sample(0:10
                                       ,prob=c(3:13)
                                       ,replace=TRUE
                                       ,size=length(
                                         air_df$career_use[air_df$post_grad_outcome=="Employed"])),
                                air_df$career_use)
    
    air_df$salary <- ifelse(air_df$school=="EG",
                            #intercept + slope*value + error/noise
                            #different slopes for a nice interaction effect
                            64000 + 750*air_df$career_use + rnorm(n_students*n_terms,0,1000),
                            ifelse(air_df$school=="AS",
                                   48000 + 10*air_df$career_use + rnorm(n_students*n_terms,0,4000),
                                   51000 + 400*air_df$career_use + rnorm(n_students*n_terms,0,2700)))
    air_df$salary <- ifelse(air_df$post_grad_outcome=="Employed",air_df$salary,NA)
      
    fake_df <- air_df
      fake_df
    })
    
    output$preview_fake_df <- renderDataTable({
      
      datatable(head(fake_df(),50),filter='top',options=list(scrollX=TRUE))

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
      
      tdata <- ifelse(is.null(fake_df()),0,
                      format(object.size(fake_df()),units="b"))

      infoBox(value = p(tdata,br(),
              format(object.size(fake_df()),units="Mb")),
               title="Dataset Size",
               icon=icon("expand-arrows-alt"),
               color="blue", fill=TRUE)
    })
    
# dynamic ui ########################################################
    
    # this is where most of the work will happen!
    # clicking "Delete Column" will update fake_df()
    # a datatable may be better suited to this...?
    # render separate UIs outside of column() eg renderUI({lapply(selectInput())})
      # then in actual ui file, put column(4,textui),column(4,selectui),column(4,optionui) ?
      # I think this is why I need local({}) ...
    output$dynamic_inputs <- renderUI({
      
      lapply(names(fake_df()),function(x){
      tagList(
        fluidRow(
          static_height
          , column(4,textInput(paste0(x,grep(x,names(fake_df()))), NULL, x))
          , column(4,selectInput(paste0("var_type",grep(x,names(fake_df()))), NULL
                                 # two types of var types: atomic (numeric, character, factor) vs pre-defined (primary key, names, phone numbers)
                                 , c("Sequential Primary Key","Numeric","Date Range","Character String: Nominal","Character String: Long Text")))
          # , uiOutput("ui")
          , column(4
                   , if (!is.null(input[[paste0("var_type",grep(x,names(fake_df())))]])) {switch(
                     # "Numeric"
                     input[[paste0("var_type",grep(x,names(fake_df())))]] # may need this in a different chunk
                     , "Sequential Primary Key" = p("Sequential integers from 1 to the number of rows. Can serve as a unique ID.")
                     , "Numeric" = sliderInput("dynamic", "Dynamic",
                                               min = 1, max = 20, value = 10)
                     , "Date Range" = dateRangeInput("dynamic", "Dynamic")
                   )})
        )
        , fluidRow(actionButton(paste0("delete_column",x), "Delete Column")
                   # , dynamic help buttons based on variable type selection! 
                   )
      )
      })
      # local_reactive_inspect_vars()
      # do.call(tagList, variable_output)
    })
    
    output$ui <- renderUI({
      lapply(names(fake_df()),function(x){
        if (is.null(input[[paste0("var_type",grep(x,names(fake_df())))]]))
          return()

        column(12
               , static_height
               , switch(
          # "Numeric"
          input[[paste0("var_type",grep(x,names(fake_df())))]] # may need this in a different chunk
          , "Sequential Primary Key" = p("test")
          , "Numeric" = sliderInput("dynamic", "Dynamic",
                                    min = 1, max = 20, value = 10)
          , "Date Range" = dateRangeInput("dynamic", "Dynamic")
        ))
      })
    })

    output$test <- renderText({input$var_type2 })
    
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