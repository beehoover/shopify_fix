Early_Orders_UI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    
    column(
      12,
      
      titlePanel(title_ED),
      
      sidebarLayout(
        sidebarPanel(
          
  #        includeMarkdown("www/Rmd/early-special-orders-instructions.Rmd"),
          
          br(),
          
          h4("Download Early Labels"),
          
          uiOutput(ns("btn_EarlyLables")),
          
          br(),
          
          h4("Download Preorders"),
          
          uiOutput(ns("btn_Preorders")),
          
          br(),
          
          icon("info-circle"),
          span(strong("FS"), " = Fresh Seafood;",
               strong("VO"), " = Vendor Orders",
               class = "prompt-message")
          ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Early Order Tally",  dataTableOutput(ns("all_preorders"))),
            tabPanel("Unmatched Products", dataTableOutput(ns("no_category")))
                        )
          )
        )
      )
    )
}


Early_Orders_Server <- function(id) {
  
  moduleServer(
    id, 
    function(input, output, session) {
      
      # Buttons: Download Early Labels -------------------------------------
      output$btn_EarlyLables <- renderUI({
        lapply(ED_category, function(category) {
          downloadButton(session$ns(paste0("downloadEarlyLabels_", category)),
                         label = paste("Download Early Labels -",
                                       shiny_category_full(category)),
                         class = 'btn-grey')
          })
        }) 
      
      # Buttons: Download Preorders ---------------------------------------
      output$btn_Preorders <- renderUI({
        lapply(ED_category, function(category) {
          downloadButton(session$ns(paste0("downloadPreorder_", category)), 
                         label = paste("Download Preorders -", 
                                       shiny_category_full(category)))
        }) 
      })
      
      
      lapply(ED_category, function(category) {
        
        output[[paste0("downloadEarlyLabels_", category)]] <- downloadHandler(
          filename = function() {paste0("GH_flashsales_ED_", category, ".csv")},
          content = function(file) {
            write.csv(filter_ED_SO(all_ED_SO, paste0("ED/", category)), 
                      file, row.names = FALSE)
          }
        )
        
        output[[paste0("downloadPreorder_", category)]] <- downloadHandler(
          filename = function() {paste0("preorders_ED_", category, ".csv")},
          content = function(file) {
            write.csv(filter_ED_SO(all_preorders_ED, paste0("ED/", category)), 
                      file, row.names = FALSE)
          }
        )
      })



      
      # Tab: Unmatched Products ---------------------------------------------
      output$no_category <- renderDataTable({
        datatable(no_category, options = datatable_options)
      })
      
      # Tab: Early Order Tally ----------------------------------------------
      output$all_preorders <- renderDataTable({
        datatable_export(all_preorders_ED, 
                         title = paste(title_ED, "- Early Deadline Tally"))
      })
      
      
      
    }
  )
}