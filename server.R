server <- function(input, output, session) {
  
  observeEvent(input$restart, {
    req(session$userData$user()$email)
    showModal(modalDialog("Restarting...", footer=NULL))
    sign_out_from_shiny(session)
    rsconnect::restartApp("shopify_app", account = "gethookedseafood")
    removeModal()
  })
  
 #   Dash_Server("Panel_Dash")
    Main_Shares_Server("Panel_Main_Shares")
    Species_Assignment_Server("Panel_Species_Assignment")
    Early_Orders_Server("Panel_Early_Orders")
    Special_Orders_Server("Panel_Special_Orders")
    Checklists_Server("Panel_Checklists")
    Home_Delivery_Server("Panel_Home_Delivery")
  
}
