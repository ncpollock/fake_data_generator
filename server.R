
# eval(parse(text = "1 + 1"))
# sprintf("this is a %s for the %s","test","win")

# Define server logic required to draw a datatable
shinyServer(function(input, output, clientData, session) {
    
  fake_df <- reactive({
      
    n_students <- 1000 # the number of number of grads per cohort
    
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
               color="blue")
    })
    
# dynamic ui ########################################################
    
    # this is where most of the meat will happen!
    # clicking "Add Column" or "Delete Column" will update fake_df() which will in turn trigger this to run
    output$dynamic_inputs <- renderUI({
      lapply(names(fake_df()),function(x){
      tagList(
        fluidRow(
          column(3,textInput(paste0(x,grep(x,names(fake_df()))), 'Variable Name', x))
          , column(3,selectInput(paste0("var_type",grep(x,names(fake_df()))), 'Variable Type', "numeric"))
          , column(3,p("Options Placeholder e.g., Provide levels for nominal variable, range for dates or numbers, etc"))
        )
        , fluidRow(p("Delete Column (Button Placeholder)"))
      )
      })
    })


    output$explore_chart <- renderPlot({

      # there's probably a better way to accomplish this
      # i have a lot of conditional logic that repeats code
      
      plot_df <- file_df()
      
      #transform text vars into symbols for dplyr programming
      x_var <- input$x_var
      x_var_sym <- sym(x_var)
      
      y_var <- input$y_var
      y_var_sym <- sym(y_var)

      color_var <- input$color_var
      color_var_sym <- sym(color_var)
      
      facet_var <- input$facet_var
      facet_var_sym <- sym(facet_var)
      
      # simplest might be if(){} else {} instead
      plot_df <- plot_df %>%
        mutate(x_var = !!x_var_sym,
               y_var = ifelse(rep(TRUE,nrow(plot_df)) & #so ifelse evaluates for every row
                                input$y_var=="None",1,!!y_var_sym),
               color_var = ifelse(rep(TRUE,nrow(plot_df)) & 
                                    input$color_var == "None","None",as.character(!!color_var_sym)),
               facet_var = ifelse(rep(TRUE,nrow(plot_df)) & 
                                    input$facet_var=="None","None",as.character(!!facet_var_sym)))
      
      # put color and facet groups into vector for grouping
      group_vars <- NULL
      if(input$color_var != "None"){
        group_vars <- c(group_vars,"color_var")
        }
      if(input$facet_var != "None"){ 
        group_vars <- c(group_vars,"facet_var")
        }
        
      plot_df <- plot_df %>%
      # combine color and facet in vector and groub_by_at
        group_by_at(.vars = group_vars)
      
      # apply chosen stat and init ggplot
      if(input$y_var == "None"){
        plot_df <- plot_df %>%
          group_by_at(.vars = vars(x_var,group_vars)) %>%
          summarise(y_var = n())
        gp <- ggplot(plot_df,aes(x=x_var,y=y_var,fill=color_var,group=color_var,color=color_var))
      }
      
      if(input$y_var != "None" &
         input$plot_stats == "Value"){
        gp <- ggplot(plot_df,aes(x=x_var,y=y_var,fill=color_var,group=color_var,color=color_var))
      }
      
      if(input$y_var != "None" &
         input$plot_stats == "Average"){
        plot_df <- plot_df %>%
          group_by_at(.vars = vars(x_var,group_vars)) %>%
          summarise(y_var = mean(!!y_var_sym,na.rm = TRUE))
        gp <- ggplot(plot_df,aes(x=x_var,y=y_var,fill=color_var,group=color_var,color=color_var))
      }
      
      if(input$y_var != "None" &
         input$plot_stats == "Sum"){
        plot_df <- plot_df %>%
          group_by_at(.vars = vars(x_var,group_vars)) %>%
          summarise(y_var = sum(!!y_var_sym,na.rm = TRUE))
        gp <- ggplot(plot_df,aes(x=x_var,y=y_var,fill=color_var,group=color_var,color=color_var))
      }
      
      # apply facet if applicable
      if(input$facet_var != "None") gp <- gp + facet_wrap(~facet_var)
      
      # remove legend if not grouped
      # need a solution for when faceted but not colored!
      if(is.null(group_vars)) gp <- gp + guides(fill=FALSE,color=FALSE)
      
      
      # fix axis labels
      gp <- gp + 
        xlab(input$x_var) +
        ylab(ifelse(input$y_var=="None","Frequency",""))
      
      # apply chosen plot type
      if(input$plot_type == "Column") gp <- gp + geom_col(width=input$expl_size)
      if(input$plot_type == "Bar") gp <- gp + geom_col(width=input$expl_size) + coord_flip()
      if(input$plot_type == "Histogram") gp <- ggplot(plot_df,aes(x=x_var,fill=color_var,group=color_var,color=color_var)) + geom_histogram()
      if(input$plot_type == "Heatmap") gp <- gp + geom_bin2d()
      if(input$plot_type == "Line") gp <- gp + geom_line(size=input$expl_size*8)
      if(input$plot_type == "Point") gp <- gp + geom_point(size=input$expl_size*8)
      # if(input$plot_type == "Boxplot") gp <- gp + geom_boxplot(color="black")
      if(input$plot_type == "Boxplot"){
        if(is.null(group_vars)){
          gp <- gp + geom_boxplot(aes(x=as.character(x_var),fill = NULL, group = NULL, color = NULL),color="black")
        } else {
          gp <- gp + geom_boxplot(aes(x=as.character(x_var),group=as.character(x_var)),color="black")
        }
      } 
      
        # geom_boxplot(aes(group=x_var),color="black")
        # geom_boxplot(aes(x=y_var,y=x_var),color="black")
      
      #apply custom theme
      gp <- gp + my_theme
      
      # add features
      if(input$expl_label == TRUE) gp <- gp + geom_label(aes(label=y_var)
                                                         ,color="black"
                                                         ,position = position_stack(vjust = 0.5))
      if(input$expl_legend == FALSE) gp <- gp + theme(legend.position="none")
      if(!("X-Axis" %in% input$expl_theme)) gp <- gp + theme(axis.title.x = element_blank())
      if(!("Y-Axis" %in% input$expl_theme)) gp <- gp + theme(axis.title.y = element_blank())
      
      gp
      
    })
    
    output$explore_chart_table <- renderDataTable({
      
      # eventually allow the user to choose specific points?
      plot_df <- file_df()
      
      #transform text vars into symbols for dplyr programming
      x_var <- input$x_var
      x_var_sym <- sym(x_var)
      
      y_var <- input$y_var
      y_var_sym <- sym(y_var)
      
      color_var <- input$color_var
      color_var_sym <- sym(color_var)
      
      facet_var <- input$facet_var
      facet_var_sym <- sym(facet_var)
      
      # create dataframe for plots
      plot_df <- plot_df %>%
        mutate(x_var = !!x_var_sym,
               y_var = ifelse(rep(TRUE,nrow(plot_df)) & #so ifelse evaluates for every row
                                input$y_var=="None",1,!!y_var_sym),
               color_var = ifelse(rep(TRUE,nrow(plot_df)) & 
                                    input$color_var == "None","None",as.character(!!color_var_sym)),
               facet_var = ifelse(rep(TRUE,nrow(plot_df)) & 
                                    input$facet_var=="None","None",as.character(!!facet_var_sym)))
      
      # put color and facet groups into vector for grouping
      group_vars <- NULL
      if(input$color_var != "None"){
        group_vars <- c(group_vars,"color_var")
      }
      if(input$facet_var != "None"){ 
        group_vars <- c(group_vars,"facet_var")
      }
      
      plot_df <- plot_df %>%
        # combine color and facet in vector and groub_by_at
        group_by_at(.vars = group_vars)
      
      # apply chosen stat and init ggplot
      if(input$y_var == "None"){
        plot_df <- count(plot_df,x_var)
      }
      if(input$y_var != "None" &
         input$plot_stats == "Value"){
      }
      if(input$y_var != "None" &
         input$plot_stats == "Average"){
        plot_df <- summarise(plot_df,y_var = mean(!!y_var_sym))
      }
      if(input$y_var != "None" &
         input$plot_stats == "Sum"){
        plot_df <- summarise(plot_df,y_var = sum(!!y_var_sym))
      }
      datatable(plot_df)
    })
    
# inspect.tab ###########################################################
    output$inspect_vars <- renderUI({
      
      variable_output <- lapply(names(file_df()), function(i) {

        level_counts <- paste0("table1_",i)
        level_detail <- paste0("table1_1_",i)
        stat_summaries <- paste0("table2_",i)
        textname_var <- paste("text", i, sep="")
        selected_row <- paste("sr", i, sep="")
        
        inspect_histogram <- paste0("plot1_",i)
        inspect_bar <- paste0("plot2_",i)
        
          if(is.numeric(file_df()[[i]])){
            list(box(width = 12,collapsible = TRUE,collapsed = FALSE,solidHeader = TRUE,status = 'success',
                       title=p(title_collapse(i),": Variable is numeric"),
                       box(width=4,DT::dataTableOutput(stat_summaries)),
                     box(width=8,plotOutput(inspect_histogram),
                         sliderInput(paste0("inspect_bin",i),
                                     "Bins:",
                                     # should be number of non-missing values, not number of rows...
                                     min = 1,  max = 50,value = ifelse(2*nrow(file_df())^(1/3)>50,50,2*nrow(file_df())^(1/3))
                                     ))
                     ))

          } else {
            list(
              box(width = 12,collapsible = TRUE,collapsed = FALSE,solidHeader = TRUE,status = 'warning',
                  title=p(title_collapse(i),": Variable is not numeric"),
                  box(DT::dataTableOutput(level_detail),width=4),
                  box(DT::dataTableOutput(level_counts),width=8)
              # ,box(width=8,plotOutput(inspect_bar)) # if I want to go back to plots for character vars
              )) }
        
        # could add in other checks for dates, booleans, lat/lon/geo, etc
        
          # ,list(p("Each variable gets either a plot or a table, but every variable gets this nice paragraphs.")))
      })
      local_reactive_inspect_vars()
      do.call(tagList, variable_output)
      # }
    })
    
    
    local_reactive_inspect_vars <- reactive({
      for (i in names(file_df())) {

        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
          my_i <- i
          j <- sym(i) # symbolize quoted variable name for use in dplyr programming
          
          #get variable name
          textname_var <- paste("text", my_i, sep="")
          output[[textname_var]] <- renderText(my_i)
          
          level_counts <- paste0("table1_", my_i)
          level_detail <- paste0("table1_1_", my_i)
          stat_summaries <- paste0("table2_", my_i)
          inspect_histogram <- paste0("plot1_",my_i)
          inspect_bar <- paste0("plot2_",my_i)
          
          #get selected table indice
          selected_row <- paste("sr", my_i, sep="")
          output[[selected_row]] <- renderText(
            as.character(
              distinct(file_df(),!!j)[input[[paste0("table1_",my_i,"_rows_selected")]],]
            )
            )
          
          #initialize empty space
          output$none <- renderPlot({})
          
          output[[level_detail]] <- DT::renderDataTable({
            
            tdata <- file_df() %>% 
              count(!!j)
            tdata <- bind_rows(
              c(detail = "Levels",
                value = nrow(tdata)),
              c(detail = "Level Count Range",
                value = paste0(range(tdata$n),collapse = " - ")),
              c(detail = "Missing/Blank Values",
                value = (tdata %>% filter(is.na(!!j)))$n))
            
            datatable(tdata
                      ,rownames = FALSE
                      ,colnames = c(paste0(my_i," Detail"),
                                    "Value")
                      ,options = list(dom='t'
                                      ,initComplete = dt_column_head
                                      ,ordering=FALSE
                                      ,columnDefs = list(list(className = 'dt-center', targets = 1))))
          })
          
          output[[level_counts]] <- DT::renderDataTable({
            tdata <- file_df() %>% count(!!j) %>% mutate(pct = percent(n/sum(n),2)) %>% arrange(desc(n))
            datatable(tdata # should save this as a more global var, used several times
                      ,rownames = FALSE
                      ,colnames = c(paste0(my_i," Level"),
                                    "Count",
                                    "Percent")
                      ,options = list(dom='tpf'
                                      ,initComplete = dt_column_head
                                      ,search = list(regex = TRUE, caseInsensitive = FALSE)
                                      ,columnDefs = list(list(className = 'dt-center', targets = 1:2)))) %>%
              formatStyle(
                my_i,'n',
                background = styleColorBar(range(0,max(tdata$n)), v_light_gray2,angle=270),
                backgroundSize = '100% 75%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center',
                fontWeight = 'bold')
          })
          
          output[[stat_summaries]] <- DT::renderDataTable({
            
            tdata <- file_df() %>%
              summarise(Min = min(!!j,na.rm = TRUE),
                        Max = max(!!j,na.rm = TRUE),
                        Mean = mean(!!j,na.rm = TRUE),
                        SD = sd(!!j,na.rm = TRUE),
                        Median = median(!!j,na.rm = TRUE),
                        Distinct = n_distinct(!!j),
                        Missing = sum(ifelse(is.na(!!j),1,0))) %>% 
              gather(stat,value) %>% 
              filter(!is.na(value))
            
            datatable(tdata
                      ,rownames = FALSE
                      ,colnames = c("Statistic",
                                    "Value")
                      ,options = list(
                        paging = FALSE
                        ,searching = FALSE
                        ,pageLength = nrow(tdata) 
                        ,initComplete = dt_column_head
                      ))
          })
          
          output[[inspect_histogram]] <- renderPlot({
            
            pdata <- file_df() %>%
              mutate(fill_val = ifelse(abs(!!j) > (mean(!!j) + (sd(!!j))),"Tail","Not"))
            
            plot_grid(ggplot(pdata,aes(x=!!j)) +
                        geom_histogram(bins = input[[paste0("inspect_bin",my_i)]]) +
                        #stat lines
                        geom_vline(aes(xintercept = median(!!j,na.rm = TRUE),color="median"),size = 2) +
                        geom_vline(aes(xintercept = mean(!!j,na.rm = TRUE),color="mean"),size = 2) +
                        
                        # cutoff lines
                        geom_vline(aes(xintercept=(mean(!!j) + sd(!!j)),color="cutoff"),size=1.5) +
                        geom_vline(aes(xintercept=(mean(!!j) - sd(!!j)),color="cutoff"),size=1.5) +
                        
                        scale_color_manual(name = "Stats", 
                                           values = c(median = "black", mean = "orange",cutoff = "red")) +
                        # to maintain alignment with boxplot
                        scale_x_continuous(limits = c(min(file_df()[[my_i]],na.rm = TRUE)
                                                      ,max(file_df()[[my_i]],na.rm = TRUE))) +
                        my_theme +
                        theme(legend.position = 'top'),
                      ggplot(pdata) +
                        geom_boxplot(aes(y=!!j,x=1),fill = "gray50") +
                        geom_hline(aes(yintercept = median(!!j,na.rm = TRUE),color="median"),size = 1.7) +
                        geom_hline(aes(yintercept = mean(!!j,na.rm = TRUE),color="mean"),size = 2) +
                        scale_color_manual(name = "Stats", values = c(median = "black", mean = "orange")) +
                        coord_flip() +
                        my_theme +
                        theme(axis.title = element_blank()),
                      rel_heights=c(3,1),
                      align = "v",
                      nrow = 2
            )
          })
          
          output[[inspect_bar]] <- renderPlot({
            
            row_indice <- input[[paste0("table1_",my_i,"_rows_selected")]]
            row_value <- (
              (file_df() %>%
                count(!!j) %>%
                arrange(desc(n)) %>%
                distinct(!!j))[row_indice,])[[my_i]]
            # row_value <- as.character(distinct(file_df(),!!j)[row_indice,])
          
            pdata <- file_df() %>%
              count(!!j) %>%
              mutate(selected = ifelse(!!j %in% row_value,"Selected","Not")) %>%
              arrange(n) %>%
              mutate(x_var = factor(!!j,levels = !!j))
            
            ggplot(pdata,aes(x=x_var,y=n,fill=selected)) +
              scale_fill_manual( values = c("Selected" = "red", "Not" = "gray"), guide = FALSE ) +
              geom_col() +
              coord_flip() +
              my_theme +
              theme(axis.title = element_blank())
          })
        })
      }
    })
    
    
})