Dash_UI <- function(id) {
  ns <- NS(id)
  
   fluidPage(
     titlePanel("Dashboard"),
     
     setBackgroundColor(color = "ghostwhite"),
     useShinydashboard(),
     
     fluidRow(
       valueBox(
         htmlOutput("progress"), "Progress", icon = icon("users"), color = "purple"
       )
     ),
     )
#     title = "Dash",
    #  tags$style(HTML("
    #   #first {
    #       border: 4px double red;
    #   }
    #   #second {
    #       border: 2px dashed blue;
    #   }
    # ")),
     # fluidRow(id = "first",
     #          textOutput(ns("subscription_n"))
     # ),
     fluidRow(id = "second",
              textOutput(ns("subscription_n"))
     )
     
   )
}


Dash_Server <- function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      
      utput$subscription_n <- renderText({
        prettyNum(input$orders)
      })
      
      # Value Box: Subscription Order Count ----------------------------------------------
      output$subscription_n <- renderText({
        "hi"

      })
      

      
    }
  )
}  