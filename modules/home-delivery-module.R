Home_Delivery_UI <- function(id) {
  ns <- NS(id)
  # Define UI for data upload app ---- home delivery wednesday
  ui <- fluidPage(
    fluidRow(
      column(
        12,
        titlePanel("Home Delivery"),
        
        sidebarLayout(
          
          sidebarPanel(
            
            h4("Step 1: Select Delivery Day"),
            
            selectInput(ns("delivery_day"), NULL,
                        choices = delivery_day_levels),
            
            br(),
            h4("Step 2: Download Routesavvy File"),
            
            ## The text on the button changes according to delivery_day
            uiOutput(ns("btn_downloadRoutesavvyFile")),
            
            ## Warning message when selecting a wrong delivery day
            uiOutput(ns("message_home_delivery_route"), 
                     class = "warning-message"),
            br(),
            
            h4("Step 3: Upload Drivers' Routes", tags$u(strong("All at Once"))),
            icon("info-circle"),
            
            ## Prompt message for file name format
            span("File Name Format: [Driver's Name] - [yyyymmdd].csv", br(), 
                 "Example: David - 20210101.csv", 
                 class = "prompt-message"),
            
            fileInput(ns("file"), NULL,
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                      multiple = TRUE),
            
            uiOutput(ns("message_file_name_format"),
                     class = "warning-message"),
            br(),
            
            h4("Step 4: Download Combined Route"),
            icon("info-circle"),
            span("A button will show up when it is available for download",
                 class = "prompt-message"),
            uiOutput(ns("btn_downloadRoute"))
          ),
          
          mainPanel(
            tabsetPanel(
              tabPanel("Check Addresses",     dataTableOutput(ns("check_address"))),
              tabPanel("Routesavvy File",     dataTableOutput(ns("deliveries_routesavvy"))),
              tabPanel("No delivery",         dataTableOutput(ns("check_no_delivery"))),
              tabPanel("Home Delivery Route", dataTableOutput(ns("home_delivery_route")))
            )
          )
        )
      )
    )
  )
  
}


Home_Delivery_Server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {
      delivery_day <- reactive({ input$delivery_day })
      file <- reactive({ input$file })
      
      ## get delivery day abbreviation
      delivery_day_abb <- reactive( {
        toupper(substr(delivery_day(), 1, 3))
      })
      
      ## combine multiple uploaded files into a single data frame
      data_input <- reactive({
        lapply(1:length(file()$datapath), function(i) {
          read.csv(file()$datapath[i]) %>% 
            mutate(driver = str_extract(file()$name[i], "^[\\w]+(?= - )")) %>%
            mutate(driver = ifelse(is.na(driver), paste("Driver", i), driver))%>%
            mutate(route_index = i)
        }) %>% 
          do.call(rbind, .)
      })
      
      ## 
      deliveryRoute <- reactive({
        data_input() %>%
          unite(address, Address1, City, State, Zip, sep = ", ") %>% 
          mutate(text = paste(StopOrder, Name, address, sep = ". "))%>% 
          select(stop = StopOrder, customer_name = Name, address, 
                 note = Note, phone = Note2, text, driver, route_index) %>% 
          merge(deliveries_label %>% filter_delivery_day(delivery_day()),
                by = "customer_name")%>% 
          mutate(driver_stop = as.numeric(trimws(stop)))%>%
          rename(date = delivery_date, size = share_size) %>%
          arrange(route_index, driver_stop) %>% 
          mutate(driver_stop_label = paste(driver, driver_stop, sep = " - "))%>%
          mutate(name_label = customer_name,
                 address_label = address,
                 driver_stop_route = driver_stop_label)%>%
          mutate(bagsDropped = "", bagsTaken = "") %>%
          select(bagsDropped, bagsTaken, dryGoods = all_dry, 
                 driver_stop_route, customer_name, phone, address, 
                 note, driver_stop_label, spacer_1, size, species, spacer_2, 
                 name_label, spacer_label, address_label, date, text)
      })
      
      # Output ===============================================================
      
      # downloadButtons & messages -------------------------------------------
      
      ## Button: Download Routesavvy File
      output$btn_downloadRoutesavvyFile <- renderUI({
        downloadButton(session$ns("downloadRoutesavvyFile"), 
                       paste("Download", delivery_day_abb(), "Routesavvy File"))
      })
      
      
      ## Button: Download Home Delivery Route
      output$btn_downloadRoute <- renderUI({
        if(!is.null(req(file())) & (nrow(deliveryRoute()) > 0)) {
          downloadButton(session$ns("downloadRoute"), 
                         paste("Download", delivery_day_abb(), "Home Delivery Route"))
        }
      })
      
      ## Warning message when selecting a wrong delivery day
      output$message_home_delivery_route <- renderUI({
        if(!is.null(req(file())) & (nrow(deliveryRoute()) == 0)) {
          span("Please check if correct", tags$u("delivery day"), " is selected")
        }
      })
      
      ## Warning message when file name formats are incorrect
      output$message_file_name_format <- renderUI({
        if(!is.null(req(file())) & any(is.na(str_extract(file()$name, "^[\\w]+(?= - )")))) {
          span("Please check if the file names are in the correct format") 
        }
      })
      
      
      # Datatables ======================================================
      
      ## Check Addresses
      output$check_address <- renderDataTable({
        datatable(
          check_address %>% filter_delivery_day(delivery_day()),
          options = datatable_options)
      })
      
      ## Routesavvy File
      output$deliveries_routesavvy <- renderDataTable({
        datatable(
          deliveries_routesavvy %>% filter_delivery_day(delivery_day()), 
          options = datatable_options)
      })
      
      ## No Delivery
      output$check_no_delivery <- renderDataTable({
        datatable(
          check_no_delivery %>% filter_delivery_day(delivery_day()), 
          options = datatable_options
        )
      })
      
      ## Home Delivery Route
      output$home_delivery_route <- renderDataTable({
        req(file())
        datatable(req(deliveryRoute()), options = datatable_options)
      })
      
      # Downloads ===============================================================
      
      ## Download Routesavvy File -----------------------------------------------
      
      output$downloadRoutesavvyFile <- downloadHandler(
        filename = function() {
          paste0("routesavvy_", delivery_day(), ".csv")
        },
        content = function(file) {
          write.csv(
            deliveries_routesavvy %>% filter_delivery_day(delivery_day()),
            file, row.names = FALSE)
        }
      )
      
      ## Download Home Delivery Route -------------------------------------------
      
      output$downloadRoute <- downloadHandler(
        filename = function() {
          paste0("home_deliveries_", delivery_day(), ".csv")
        },
        content = function(file) {
          write.csv(req(deliveryRoute()), file, row.names = FALSE)
        }
      )
    }
  )
}
