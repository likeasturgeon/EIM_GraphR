ui <- fluidPage(
        
        titlePanel("EIM Time-Series Reviewer"),
        
        sidebarLayout(
            
            sidebarPanel(
                
                fileInput("file1", "Choose CSV File", accept = ".csv")
                
                
                
            ),
            
            mainPanel(
                
                dygraphOutput("dygraph")
                
                )
            )
    )

server <- function(input, output) {
    
        output$dygraph <- renderDygraph({
            
            req(input$file1)
            
                df <- read.csv(file = input$file1$datapath)
                df$Start_Date <- gsub(" .*$", "", df$Start_Date)
                ts_df <- df %>%																			 
                        mutate(my_time = lubridate::mdy_hms(
                            paste(df$Start_Date, " ", df$Start_Time))) %>%
                        select(my_time, Result_Value, Result_Data_Qualifier)
                    results <- xts(ts_df$Result_Value, order.by=ts_df$my_time)
                    flagged <- xts(ts_df$Result_Value[!is.na(ts_df$Result_Data_Qualifier)], 
                                   order.by=ts_df$my_time)
                
            dygraph(cbind(results,flagged), 
                    main = paste(unique(df$Parameter_Name), "at Location ID:", unique(df$Location)),
                    xlab = "Date_Time", 
                    ylab = paste(df$Parameter_Name[1], df$Result_Unit[1])
            ) %>%
                dySeries("flagged", color = "red") %>%
                dyRangeSelector() 
        })
    }
    
    shinyApp(ui, server)
