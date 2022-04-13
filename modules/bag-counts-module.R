Bag_Counts_UI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      
      column(
        12,
        
        titlePanel("Bag Counts"),

        fluidRow(
          actionButton(ns("reloadBagCounts"), "Reload Bag Counts Data",
                       icon = icon("sync-alt"), class = "btn-reload")
        ),
        
        
        sidebarLayout(
          
          sidebarPanel(
            
            includeMarkdown("www/Rmd/bag-counts-instructions.Rmd"),
            
            br(),
            
            downloadButton(ns("downloadBagSum"), "Download Bag Sum",
                           icon = icon("download")),
            
            br(),
            
            downloadButton(ns("downloadBagsNeeded"), "Download Bags Needed",
                           icon = icon("download"))),
          
          
          mainPanel(
            tabsetPanel(
              tabPanel("Check Bag Count", dataTableOutput(ns("current_bags"))),
              tabPanel("Already Charged?", dataTableOutput(ns("check_bags_needed"))),
              tabPanel("Check Members", dataTableOutput(ns("check_point"))),
              tabPanel("Delete from Bag Deposit Paid Sheet", dataTableOutput(ns("bags_paid_delete")))
            )
          )
        )
      )
    )
  )
}


Bag_Counts_Server <- function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      
      values <- reactiveValues(bag_counts_data = bag_counts_initial)
      
      observeEvent(input$reloadBagCounts, {
        showModal(modalDialog("Loading...", footer=NULL))
        Home_Delivery_new <- tempfile()
        
        download.file(gs_url(ss = ss["home_delivery"], is_xlsx = TRUE),
                      destfile = Home_Delivery_new, mode = "wb")
        
        values$bag_counts_data <- bag_counts_calculate(
          Home_Delivery_temp = Home_Delivery_new)
        
        unlink(Home_Delivery_new)
        removeModal()
        session$reload()
      })
      
      
      # Tab: Check Bag Count ----------------------------------------------
      output$current_bags <- renderDataTable({
        datatable(values$bag_counts_data$current_bags, options = datatable_options)
      })
      
      # Tab: Already Charged? ---------------------------------------------
      output$check_bags_needed <- renderDataTable({
        datatable(values$bag_counts_data$check_bags_needed, options = datatable_options)
      })
      
      # Tab: Check Members ------------------------------------------------
      output$check_members_bag_counts <- renderDataTable({
        datatable(values$bag_counts_data$check_members_bag_counts, options = datatable_options)
      })
      
      # Tab: Delete from Bag Deposit Paid Sheet ---------------------------
      output$bags_paid_delete <- renderDataTable({
        datatable(values$bag_counts_data$bags_paid_delete, options = datatable_options)
      })
      
      # Button: Download Bag Sum ------------------------------------------
      output$downloadBagSum <- downloadHandler(
        filename = "New_Bag_Sum.csv",
        content = function(file) {
          write.csv(values$bag_counts_data$bag_sum_new, file, row.names = FALSE)
        }
      )
      
      # Button: Download Bags Needed --------------------------------------
      output$downloadBagsNeeded <- downloadHandler(
        filename = "New_Bags_Needed.csv",
        content = function(file) {
          write.csv(values$bag_counts_data$bags_needed_new, file, row.names = FALSE)
        }
      )
      
    }
  )
}