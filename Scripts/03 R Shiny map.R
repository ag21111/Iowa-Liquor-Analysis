
# R Shiny tool ------------------------------------------------------------
  
  # Load input (total sales by county)
  sale_data <- readRDS(OUTPUT("Sale Data.rds"))
  sale_data[, `Total Sales` := round(`Total Sales`, digits = 0)]

  ### Create Shiny   
  # Get Iowa county shapefile
    iowa_counties <- counties("IA") %>% 
      mutate(County = toupper(NAME))
    
    # Generate UI for RShiny. 
    # Options:
    #   1.) Toggle year
    #   2.) Quit application
    ui <- fluidPage(
      selectInput("year", "Year", choices = unique(sale_data[, Year])),
      actionButton("quit", "Quit Application"),
      leafletOutput("map")
    )
    
    # Generate server 
    server <- function(input, output) {
      output$map <- renderLeaflet({
        filtered_data <- sale_data %>%
          filter(Year == input$year)
        
        # Join sales data with Iowa county information
        merged_data <- left_join(iowa_counties, filtered_data, by = "County")
        
        # Generate map output
        leaflet() %>%
          addTiles() %>%
          addPolygons(data = merged_data,
                      fillColor = ~colorNumeric("YlGnBu", merged_data$`Total Sales`)(merged_data$`Total Sales`),
                      color = "black",
                      weight = 2,
                      popup = ~paste0("County: ", merged_data$County, "<br>",
                                      "Year: ", merged_data$Year, "<br>",
                                      "Total Sales: ", merged_data$`Total Sales`))
      })
      
      # Quit application option
      observeEvent(input$quit, {
        stopApp()
      })
    }

# Launch Shiny ------------------------------------------------------------

    shiny_tool <- shinyApp(ui = ui, server = server)
    runApp(shiny_tool)
   