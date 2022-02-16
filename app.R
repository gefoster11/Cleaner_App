#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# This application is meant to load a breath-by-breath or beat-by-beat data file with 
# time in the first column and a one row header.  It can then be used to "clean" your 
# data file by either point and click or brush method or using an autoclean function
# The autoclean function will exclude values beyond 1.5*IQR over a moving time window
# The moving time window can be set by the user.
# Once finished the selected data is saved as a long dataframe with a column called 
# 'exclude' which provides a TRUE/FALSE for exclusion and can be used as input data for
# your individual subject analysis.


# Sets max file size to 30 Mb
options(shiny.maxRequestSize = 30*1024^2, scipen = 999)

# Package Dependency
packages = c("shiny",
             "shinyBS",
             "tidyverse",
             "thematic",
             "shinythemes"
            )

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


source("./functions/autoclean.R")

# ---- Define UI for data upload app ----
ui <- fluidPage(
  theme = shinytheme("united"),
    # ---- App title ----
    titlePanel("Data Cleaner"),
    
    # ---- Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # ---- Sidebar panel for inputs ----
        sidebarPanel(width = 3,
            
            # ---- Input: Select a file ----
            fileInput("file", "Choose File",
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"
                                 )),

            # ---- Input: Data Ready? ----
            actionButton("load", "Data Loaded", width = "100%"),
            bsTooltip("load", "Click this button once data parsing has completed successfully",
                      "right", options = list(container = "body")),
            
            # ---- Horizontal line ----
            tags$hr(),
            
            # ---- Input: delimiter ----
            radioButtons("sep", "Delimiter",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            bsTooltip("sep", "select the delimiter for parsing your file",
                      "right", options = list(container = "body")),
            
            # ---- Horizontal line ----
            tags$hr(),
            
            # ---- Input: Head display? ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            bsTooltip("disp", "toggle to display whole file",
                      "right", options = list(container = "body")),
            
            # ---- Horizontal line ----
            tags$hr(),
            
            # ---- Input: start and end times ----
            numericInput("start", label = "Start (s)", value = NA),
            numericInput("end", label = "End (s)", value = NA),
            selectInput("time_ave", label = "AutoClean Time Interval", 
                        choices = list("15 s" = 15, "30 s" = 30, "60 s" = 60, "90 s" = 90, "120 s" = 120), 
                        selected = 30),
            bsTooltip("start", "set start time of your experiment",
                      "right", options = list(container = "body")),
            bsTooltip("end", "set end time of your experiment",
                      "right", options = list(container = "body")),
            bsTooltip("time_ave", "select the desired time window if using Auto Clean function",
                      "right", options = list(container = "body")),
            
            # ---- Horizontal line ----
            tags$hr(),
            
            # ---- Input: AutoClean ----
            actionButton("AutoClean", "Auto Clean", width = "100%"),
            bsTooltip("AutoClean", "click to remove outliers over moving time average window (outliers = Q1-IQR*1.5 and Q3+IQR*1.5)",
                      "right", options = list(container = "body")),
            
            tags$hr(),
            
            # ---- Input: ManualClean ----
            actionButton("toggle", "Remove Points", width = "100%"),
            bsTooltip("toggle", "click to exclude points from your plot selection",
                      "right", options = list(container = "body")),
            
            tags$hr(),
            
            actionButton("reset", "Reset", width = "100%"),
            bsTooltip("reset", "click to reset excluded points",
                      "right", options = list(container = "body")),
            
            tags$hr(),
            
            # ---- Input: Save Output File ----
            downloadButton("save", "Download"),
            bsTooltip("save", "saves the selected data to a csv file", "right", options = list(container = "body")),
            
            # ---- Reference ----
            tags$hr(),
            
            tags$div(
              HTML("<p>2021; Created by Glen Foster</p><br>")),

        ),
        
        # ---- Main panel ----
        mainPanel(
            
            # ---- Output: Tabset w/ parsing table, selected data, and plot ----
            tabsetPanel(type = "tabs",
                        tabPanel("Parsing Table", dataTableOutput("data")),
                        tabPanel("Selected Data", dataTableOutput("data2")),
                        tabPanel("Plot", 
                                 numericInput("height", label = "Plot Height", value = 800, width = '10%'),
                                 plotOutput("plot", 
                                                    click = "plot_click",
                                                    brush = brushOpts(
                                                        id = "plot_brush"
                                                    )))
                        )
            )
    )
)

# ---- Define server logic ----
server <- function(input, output) {
    
    # ---- initialize reactive values ----
    values <- reactiveValues(
        df = NULL,
        df2 = NULL)
    
    # ---- Listen: delimiter inputs ----
    loadlisten <- reactive({
        list(input$file$datapath, input$sep, input$quote)
    })
    
    # ---- Observe: data load - df ----
    observeEvent(loadlisten(), {
        
        req(input$file$datapath)
        
        tryCatch( {
        df <- read.csv(input$file$datapath,
                       header = TRUE,
                       sep = input$sep,
                       quote = '"')
        
        df$exclude <- FALSE # create exclude column and set all to FALSE
        
        values$df <- df
        }, error = function(e) {
            stop(safeError(e))
        }
        )
    })
    
    # ---- Listen: Select times ----
    selectlisten <- reactive({
        list(input$start, input$end)
    })
    
    # ---- Observe: data loaded set df1 ----
    observeEvent(input$load, {
        
        df <- values$df
        
        colnames(df)[1] <- "time" # to keep name consistent and align with functions
        
        df <- df %>% pivot_longer(!c(time, exclude)) %>% 
                select(name, time, value, exclude) %>% 
                    arrange(name, time) # arrange in long format
        
        values$df2 <- df # set to reactive value df2
        })
    
    # ---- Observe: select data - df1 ----
    observeEvent(selectlisten(), {
        if (is.na(input$start) | is.na(input$end)) {
            # do nothing
        } else {
            df <- values$df
            
            colnames(df)[1] <- "time" # to keep name consistent and align with functions
            
            df <- df %>% pivot_longer(!c(time, exclude)) %>% 
                    filter(time >= input$start & time <= input$end) %>%
                        select(name, time, value, exclude) %>% arrange(name, time) # arrange in long format
            
            values$df2 <- df # set to reactive value df2
        }
    })

    # ---- Observe: autoclean ----
    observeEvent(input$AutoClean, {
        withProgress(message = "Calculation in Progress", {
            df <- values$df2
            
            time_ave <- as.numeric(input$time_ave) # convert string to numeric
            
            df <- df %>% 
                group_by(name) %>% 
                nest() # group by variable name and nest
            
            var_num <- length(df$name)
            
            df <- df %>% 
                mutate(data = map(data, function(.x) { # adds a progress bar which increments with each variable iteration
                    incProgress(amount = 1/var_num)
                    autoclean(.x, time_ave)
                } )) %>% 
                unnest(data) # apply function autoclean.R    
            
        }
        )
        values$df2 <- df # set to reactive value df2
    })
    
    
   # map(aliases, function(x) paste(x, collapse = " | ")) 
    
    # ---- Observe: reset ----
    observeEvent(input$reset, {
        df <- values$df2
        df$exclude <- FALSE
        values$df2 <- df
    })
    
    
    # ---- Output: Parsing Table ----    
    output$data <- renderDataTable({
        
        req(input$file)    
        
       if (input$disp == "head") { # display head only
            return(head(values$df))
        }
        else { # display full df
            return(values$df)
        }
    })
    
    # ---- Output: Create Selected Data Table ----
    output$data2 <- renderDataTable({
        
        req(input$load)    
        
        if (input$disp == "head") {
            return(head(values$df2))
        }
        else {
            return(values$df2)
        }
    })
    
    # ---- Output: Create Plot ----
    observe(output$plot <- renderPlot({
        
        req(input$load)
        
        browser()
      
        clean <- values$df2[values$df2$exclude != TRUE, ]
        removed <- values$df2[values$df2$exclude == TRUE, ]
        
        max <- clean$time %>% max()
        min <- clean$time %>% min()
        
        summary <- clean %>% 
            mutate(cut = cut(time, breaks = seq(from = min, to = max + 30, by = 30), include.lowest = TRUE)) %>%
                group_by(name, cut) %>% 
                    summarise_at(c("time", "value"), list("mean" = mean, "sd" = sd), na.rm = TRUE)
    
        ggplot(clean, aes(time, value)) + 
            geom_point(shape = 21, alpha = 0.65, fill = "black", color = "black") +
              geom_point(data = removed, shape = 21, fill = "red", color = "red", alpha = 0.5) +
               geom_ribbon(data = summary, aes(x = time_mean, y = value_mean, ymin = value_mean-value_sd, 
                                            ymax = value_mean+value_sd), fill = "seagreen3", alpha = 0.3) +   
                geom_step(data = summary, aes(x = time_mean, y = value_mean), size = 1) +
                facet_wrap(~name, scales = "free", ncol = 1)
               
        }, height = input$height))
    
    
    
    
    
    # ---- Toggle points that are clicked ----
    observeEvent(input$plot_click, {
        df <- values$df2
        res <- nearPoints(df, input$plot_click, allRows = TRUE, maxpoints = 1)
        df$exclude <- xor(df$exclude, res$selected_)
        
        values$df2 <- df
        
    })
    
    # ---- Toggle points that are brushed, when button is clicked ----
    observeEvent(input$toggle, {
        df <- values$df2
        res <- brushedPoints(df, input$plot_brush, allRows = TRUE)
        df$exclude <- xor(df$exclude, res$selected_)
        
        values$df2 <- df
    })
    
    # ---- Downloadable csv of selected dataset ----
    output$save <- downloadHandler(
      
        filename = function() {
            paste(tools::file_path_sans_ext(input$file), "-clean.csv", sep = "")
        },
        content = function(file) {
          df <- values$df2
          df$value[df$exclude == TRUE] <- NA
          df <- df %>% select(!exclude) %>% pivot_wider()
          colnames(df)[1] <- "Time"
          
          write.csv(df, file, row.names = FALSE)
        }
    )
    
    
}

# Create Shiny app ----
thematic::thematic_shiny()
shinyApp(ui, server)
