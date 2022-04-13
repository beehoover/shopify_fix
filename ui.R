header <- dashboardHeader(
  title = tagList(
    tags$img(
      src = "logos/logo-tealblue.png",
      alt = "Get Hooked Seafood",
      class = "image_full"
    ),
    tags$img(
      src = "logos/logo-white.png",
      alt = "Get Hooked Seafood",
      class = "image_mobile"
    )
  ),
  
  tags$li(
    class = "dropdown",
    
    fluidRow(
      
      actionButton(
        "sign_out", "Sign Out",
        icon = icon("sign-out-alt"),
        class = "pull-right"
        ),
      
      actionButton(
        "restart", "Restart",
        icon = icon("power-off"),
        class = "pull-right"
        )
      )
    )
  )

sidebar <- dashboardSidebar(
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem(
        "Prep and Production",  
        icon = icon("fish"), 
        startExpanded = TRUE,

        # menuSubItem("Dashboard",
        #             icon = icon("angle-right"),
        #             tabName = "Tab_Dash"
        # ),
        
        menuSubItem("Main Shares",
                    icon = icon("angle-right"),
                    tabName = "Tab_Main_Shares"
                    ),

        menuSubItem("Species Assignment",
                    icon = icon("angle-right"),
                    tabName = "Tab_Species_Assignment"),

        menuSubItem("Early Deadline Orders",
                     icon = icon("angle-right"),
                     tabName = "Tab_Early_Orders"),

        menuSubItem("Special Orders",
                    icon = icon("angle-right"),
                    tabName = "Tab_Special_Orders"),

        menuSubItem("Home Delivery",
                    icon = icon("angle-right"),
                    tabName = "Tab_Home_Delivery"),

        menuSubItem("Checklists",
                    icon = icon("angle-right"),
                    tab = "Tab_Checklists")
        )
      # ,
      # 
      # menuItem(
      #   "Data", 
      #   icon = icon("table"), 
      #   startExpanded = TRUE,
      #   
      #   menuSubItem("Monthly Bag Counts", 
      #               icon = icon("angle-right"),
      #               tab = "Tab_Bag_Counts")
      # )
    )
  )
)

body <- dashboardBody(
  # css
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  
  # js
  tags$head(tags$script(src="add-on.js")),
  
  # favicon
  tags$head(tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon/favicon-32x32.png")),
  tags$head(tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon/favicon-16x16.png")),
  tags$head(tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "favicon/apple-touch-icon.png")),
  tags$head(tags$link(rel = "manifest", href = "favicon/site.webmanifest")),
  tags$head(tags$link(rel = "mask-icon", href = "favicon/safari-pinned-tab.svg", color="#006169")),
  tags$head(tags$link(rel = "mask-icon", href = "favicon/safari-pinned-tab.svg", color="#006169")),
  
  useShinyjs(),
  
  tabItems(
#     tabItem(tabName = "Tab_Dash", Dash_UI("Panel_Dash")),
     tabItem(tabName = "Tab_Main_Shares", Main_Shares_UI("Panel_Main_Shares")),
     tabItem(tabName = "Tab_Species_Assignment", Species_Assignment_UI("Panel_Species_Assignment")),
     tabItem(tabName = "Tab_Early_Orders", Early_Orders_UI("Panel_Early_Orders")),
     tabItem(tabName = "Tab_Special_Orders", Special_Orders_UI("Panel_Special_Orders")),
     tabItem(tabName = "Tab_Checklists", Checklists_UI("Panel_Checklists")),
     tabItem(tabName = "Tab_Home_Delivery", Home_Delivery_UI("Panel_Home_Delivery"))
  )
)

ui <- dashboardPage(
  title = "Get Hooked Seafood Shiny Apps",
  header = header, 
  sidebar = sidebar, 
  body = body, 
  skin = "black")
