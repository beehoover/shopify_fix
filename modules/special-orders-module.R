Special_Orders_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidPage(
    
    column(
      12,
      
      titlePanel(title_SO),
      
      sidebarLayout(
        sidebarPanel(
#          includeMarkdown("www/Rmd/early-special-orders-instructions.Rmd"),
          br(),
          
          h4("Download Labels"),
          selectInput(
            ns("Labels_category"), "Select Category:",
            choices = c("All Categories",
                        "Fresh Seafood", 
                        "Frozen Products", 
                        "Dry Products")),
          uiOutput(ns("btn_Labels")),
          br(),
          h4("Download Labels with Routes"),
          uiOutput(ns("btn_RouteLabels"))
        ),
        
        mainPanel(
          uiOutput(ns("tabs"))
        )
      )
    )
  )
}

Special_Orders_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      Labels_category <- reactive({input$Labels_category})
      
      ## subsetting data frame if selected category is not "All Categories"
      Labels_content <- reactive({
        if(input$Labels_category == "All Categories") {do.call(rbind, special_orders_all)}
        else {special_orders_all[[input$Labels_category]]}
      })
      
      # Tabset Panel -----------------------------------------------------------------------------
      output$tabs <- renderUI({
        lapply(delivery_day_levels, function(deliv_day) {
          tabPanel(paste(deliv_day, "Orders"), 
                   dataTableOutput(session$ns(paste0("all_preorders_", deliv_day))))
        }) %>%
        
        ## appending tabs other than Tuesday/Wednesday/Thursday/LA Orders
        append(list(tabPanel("Fresh Product Needed", dataTableOutput(session$ns("fresh_product_all"))),
                    tabPanel("Fresh Product by Day", dataTableOutput(session$ns("fresh_product_by_day"))),
                    tabPanel("Missed Cutoff/Order Errors", dataTableOutput(session$ns("flashsale_error"))))) %>%
        do.call(tabsetPanel, .)
      })
        
        
      # Fresh Product Needed ---------------------------------------------------------------------
      output$fresh_product_all <- renderDataTable({
        datatable_export(fresh_product_all, title = paste(title_SO, "- Fresh Product Needed"))
      })
      
      # Fresh Product by Day ---------------------------------------------------------------------
      output$fresh_product_by_day <- renderDataTable({
        datatable_export(fresh_product_by_day, title = paste(title_SO, "- Fresh Product Needed"))
      })
      
      # Missed Cutoff and Order Errors -----------------------------------------------------------
      output$flashsale_error <- renderDataTable({
        datatable_export(flashsale_error, title = paste(title_SO, "- Missed Cutoff and Order Errors"))
      })

      # Buttons: download Labels -----------------------------------------------------------------
      output$btn_Labels <- renderUI({
        lapply(delivery_day_levels_abb, function(abb) {
          downloadButton(session$ns(paste0("download_Labels_", abb)), 
                         label = paste("Download", abb, "Labels -", Labels_category()),
                         class = "btn-grey")}
        )
      })
      
      # Buttons: download Labels with Routes -----------------------------------------------------
      ## lapply to automatically generate button UI according to delivery day
      output$btn_RouteLabels <- renderUI({
        lapply(delivery_day_levels_abb, function(abb) {
          downloadButton(session$ns(paste0("download_RouteLabels_", abb)), 
                         label = paste("Download", abb, "Labels with Routes"))}
        )
      })
      
      ## lapply to automatically generate tabs and buttons according to delivery day
      
      lapply(delivery_day_levels, function(deliv_day){
        
        abb <- toupper(substr(deliv_day, 1, 3))
        
        ## Tab: TUE/WED/THU/LA Orders
        
        output[[paste0("all_preorders_", deliv_day)]] <- renderDataTable({
          datatable_export(all_preorders_SO[[deliv_day]],
                           title = paste(title_SO, "-", deliv_day, "Orders"))
        })
        
        ## downloadHandler for TUE/WED/THU/LA Labels
        
        output[[paste0("download_Labels_", abb)]] <- downloadHandler(
          filename = function() {paste0("GH_flashsales_", abb, "_", Labels_category(), ".csv")},
          content = function(file) {
            write.csv(filter(Labels_content(), delivery_day == deliv_day), file, row.names = FALSE)
          })
        
        ## downloadHandler for TUE/WED/THU/LA Labels with Routes
        
        output[[paste0("download_RouteLabels_", abb)]] <- downloadHandler(
          filename = paste0("dry_goods_", abb,".csv"),
          content = function(file) {
            write.csv(route_labels[[deliv_day]], file, row.names = FALSE)
          })
        
      })
       
        
    })
  
}