# 1. Species Selection in Main Shares/Species Assignment ======================

## 1.1 Species Selection UI ---------------------------------------------------
## index: species index (1 for Species 1, 2 for Species 2, etc.)

species_selection_UI <- function(id, index) {
  ns <- NS(id)
  tagList(
    h4(paste("Species", index)),
    
    ## location checkbox 
    strong(paste0('Choose locations to assign species ', index, ':'),
           style = "margin-bottom: 1em; display: block;"),
    uiOutput(ns("sites_checkbox")),
  
    ## select/deselect all locations
    actionButton(
      ns('selectall'),
      label = "Select/Deselect All Locations",
      icon = icon("check-double"),
      class = "btn-grey"
    ),
    
    br(),
    br(),
    
    ## share size selection (see sever)
    uiOutput(ns('share_size_selectInput')),
    
    ## species selection
    selectInput(
      ns('species'),
      label = "Choose a species:",
      choices = species_options,
      selected = "None"
      ),
    
    br(),
    
    ## "Reset Last Species" button
    uiOutput(ns("clear_species")),
    hr()
  )
  
}

## 1.2 Species Selection Server ---------------------------------------------------
## is_species_assignment: is it used in species assignment app (FALSE: used in Main Shares)
## next_species: reactive value, Species 4 for Species 3, Species 3 for Species 2, etc.
##               only next_species() == "None" will "Reset Last Species Selection" button show up
## clear_all:    reactive value, "Clear All Species Selection" button

species_selection_Server <- function(id, is_species_assignment = FALSE,
                                     next_species = NULL, clear_all) {
  moduleServer(
    id,
    function(input, output, session) {
      
#      schedule <- reactive({ req(input$schedule) })
      species <- reactive({ req(input$species) })
      
      # Location checkbox --------------------------------------------------------------
      ## lapply to generate length(delivery_day_levels) sets of checkbox 
      ## (one set of checkbox for each delivery day)
      output$sites_checkbox <- renderUI({
        lapply(delivery_day_levels, function(deliv_day) {
          abb <- toupper(substr(deliv_day, 1, 3))
          tagList(
            fluidRow(
              column(9, strong(paste(deliv_day, "Sites"))), 
              column(3, checkboxInput(session$ns(paste0("select", abb)),
                                      label = NULL, value = FALSE),
                     align = "right"),
              class = 'checkbox-sites'
            ),
            checkboxGroupInput(
              session$ns(paste0('sites', abb)),
              label = NULL,
              choices = sites_list[[abb]],
              selected = ""
            )
          )
        })
      })
      
      # Button: Select/Deselect all sites --------------------------------------------
      
      ## combine selections from length(delivery_day_levels) set of checkbox into a single vector
      sites_selected <- reactive({ 
        v = sapply(delivery_day_levels_abb, 
                   function(abb) {input[[paste0("sites", abb)]]}) %>% 
          unlist %>% unname
        
        ## "" if none of the locations are selected
        ## exclude "" of there are locations selected
        if(is.null(v) | length(v) == 0) {""}  
        else {v[v != ""]}
      })
      
      ## select/deselect all sites when clicking the button
      observeEvent(input$selectall, {
        lapply(delivery_day_levels_abb, function(abb) {
          
          ## select all if not all of the sites are selected
          ## deselect all if all sites are already selected
          if (length(sites_selected()) < length(sites)) { 
            selected = sites_list[[abb]]
            check = TRUE}
          else { 
            selected = ""
            check = FALSE}
          
          updateCheckboxGroupInput(
            session = getDefaultReactiveDomain(), 
            inputId = paste0('sites', abb),
            choices = sites_list[[abb]],
            selected = selected
          )
          
          updateCheckboxInput(
            session = getDefaultReactiveDomain(), 
            inputId = paste0('select', abb),
            value = check
          )
        })
      })
      
      
      # Buttons: select sites by production day --------------------------------------
      lapply(delivery_day_levels_abb, function(abb) {
        
        observeEvent(input[[paste0('select', abb)]], {
          if (input[[paste0('select', abb)]]) {selected = sites_list[[abb]]}
          else {selected = ""}
          
          updateCheckboxGroupInput(
            session = getDefaultReactiveDomain(),
            inputId = paste0('sites', abb),
            choices = sites_list[[abb]],
            selected = selected
          )
        }, ignoreInit = TRUE)
      })
      
      
      # Species Assignment: Choose a share size -------------------------------------
      
      if(is_species_assignment) {share_size <- reactive({input$share_size})}
      else {share_size = "All"}
      
      output$share_size_selectInput <- renderUI( {
        if(is_species_assignment) {
          selectInput(
            session$ns('share_size'),
            label = "Choose a share size:",
            choices = c("ExtraLarge", "Large", "Medium", "Small", "All"),
            selected = "All")}
        else {span()}
      })
      
      ## "Reset Last Species Selection" Button --------------------------------------
      ## Hided when the next
      output$clear_species <- renderUI({
        req(next_species)
        if(next_species() == 'None') {
          actionButton(session$ns('clear'),
                       label = "Reset Last Species Selection",
                       icon = icon('sync-alt'))
        }
        else {span()}
      })
      
      ## Reset all inputs when clicking "Reset Last Species Selection" or 
      ## "Clear All Species Selection" button
      observeEvent(input$clear | clear_all(), {
        
          
        updateSelectInput(
          session = getDefaultReactiveDomain(),
          inputId = "species",
          choices = species_options,
          selected = "None")
        
        ## clear sites and "select by delivery_day" checkbox
        ## only when clicking "Select/Deselect All" button
        
        if(clear_all()) {
          lapply(delivery_day_levels_abb, function(abb) {
            updateCheckboxGroupInput(
              session = getDefaultReactiveDomain(), 
              inputId = paste0('sites', abb),
              choices = sites_list[[abb]],
              selected = "")
            
            updateCheckboxInput(
              session = getDefaultReactiveDomain(), 
              inputId = paste0('select', abb),
              value = FALSE
            )})
          }
        })
      
      
      # Return values ---------------------------------------------------------------
      return(list(sites = sites_selected, 
#                  schedule = schedule,
                  share_size = share_size,
                  species = species))
    }
  )
}


# 2. Modification in Species Assignment =============================================

# 2.1 Modify UI --------------------------------------------------------------------
## index: species index (1 for Species 1, 2 for Species 2, etc.)

modifiy_UI <- function(id, index) {
  ns <- NS(id)
  
  tagList(
    h4(paste("Species", index)),
    selectInput(ns('species'),
                "Choose species:",
                flash_list, selected = "None"),
    
    textInput(ns('changeSpecies'),
              "Name:", "New species name"),
    
    textInput(ns('fisherman'),
              paste0("Fisherman ", index, ":")),
    
    textInput(ns('gear'),
              paste0("Gear Type ", index, ":")),
    
    textInput(ns('port'),
              paste0("Landing Port ", index, ":")),
    
    textInput(ns('expiration'),
              paste0("Expiration Date ", index, ":"),
              "Cook or freeze by [day, date]"),
    
    textInput(ns('instruction'),
              paste0("Instructions ", index, ":")),
    hr()
  )
  
}

## 2.1 Modify Server ---------------------------------------------------

modify_Server <- function(id, species_list) {
  moduleServer(
    id,
    function(input, output, session) {
      species <- reactive({ input$species })
      changeSpecies <- reactive({ input$changeSpecies })
      fisherman <- reactive({ input$fisherman })
      gear <- reactive({ input$gear })
      port <- reactive({ input$port })
      expiration <- reactive({ input$expiration })
      instruction <- reactive({ input$instruction })
      
      ## select from flash_list if the list of species is empty
      observe({
        if (length(species_list()) > 0) {
          choices <- species_list()
        } else if (length(species_list()) <= 0) {
          choices <- flash_list
        }
        updateSelectInput(session = getDefaultReactiveDomain(), 
                          inputId = 'species',
                          choices = choices,
                          selected = "None")
      })
      
      
      return(list(species = species, 
                  changeSpecies = changeSpecies, 
                  fisherman = fisherman,
                  gear = gear,
                  port = port,
                  expiration = expiration,
                  instruction = instruction))
    }
  )
}


# 3. tabsetPanels in Checklists ===============================================
## 3.1 tabsetPanel UI ---------------------------------------------------------

tabPanel_checklists_UI <- function(id, abb, type, choices) {
  ns <- NS(id)
  
  tabPanel(
    title = paste(abb, type, "List"),
    br(),
    sidebarLayout(
      sidebarPanel(
        radioButtons(ns("radio"), label = "Pickup Site", choices = choices),
        downloadButton(ns("download"), paste('Download', abb, type, 'List')
        ),
        width = 3
      ),
      mainPanel(
        dataTableOutput(ns("dt")),
        width = 9
      )
    )
  )
}

## 3.2 tabsetPanel Server ---------------------------------------------------------
## abb: abbreviation of delivery day
## type: kitchen or site
## dt:   datatable to be displayed in the main panel
tabPanel_checklists_Server <- function(id, abb, type, dt) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ## radio buttons in the sidebar for selecting locations -------------------
      output$dt <- renderDataTable({
        req(dt[[input$radio]])
        checklists_datatable(
          dt[[input$radio]], 
          title = input$radio, 
          type = type)
      })
      
      ## Button: Print TUE/WED/THU/LA Checklist ---------------------------------
      output$download <- downloadHandler(
        filename = paste0(abb, "_Checklist_", type, ".pdf"),
        content = function(f) {
          rmarkdown::render('www/Rmd/print-checklists.Rmd',
                            output_format = rmarkdown::pdf_document(),
                            output_file = f,
                            params = list(df_list = dt, type = type))
        }
      )
    }
  )
}