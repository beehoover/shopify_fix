Main_Shares_UI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    
    fluidRow(
      column(
        12,
        
        # Application title
        titlePanel("Main Shares"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
          sidebarPanel(
#            includeMarkdown("www/Rmd/main-shares-instructions.Rmd"),
            
            species_selection_UI(ns("species_select1"), 1),
            
            conditionalPanel(
              condition = "input['species_select1-species'] != 'All one species' & input['species_select1-species'] != 'None'",
              ns = ns,
              species_selection_UI(ns("species_select2"), 2)
              ),
            
            conditionalPanel(
              condition = "input['species_select2-species'] != 'None'",
              ns = ns,
              species_selection_UI(ns("species_select3"), 3)
              ),
            
            conditionalPanel(
              condition = "input['species_select3-species'] != 'None'",
              ns = ns,
              species_selection_UI(ns("species_select4"), 4)
              ),

            actionButton(
              ns("clear_all"),
              "Clear All Species Selections",
              icon = icon("broom"),
              class = "btn-clear-species"
              ),
            
            selectInput(
              ns("weight"),
              label = paste("Choose the share type. Either choice will also ensure",
                            "that the selected species are matched with applicable share weight/number",
                            "and 'Lobster' with the number of lobster as given by customers in the form:"),
              choices = c("Fillet" = "fillet", "Harbinger Meal Tray" = "harbinger"),
              selected = "Fillet"
              )
          ),

          mainPanel(
            
            tabsetPanel(
              
              tabPanel("Amounts Assigned", dataTableOutput(ns("share_fillet"))),
              tabPanel("Share Counts", dataTableOutput(ns("share_count"))),
              tabPanel("Share Pounds by Site", dataTableOutput(ns("fillet_weight_by_site"))),
              tabPanel("No Assignment", dataTableOutput(ns("no_assignment"))),
              tabPanel("All Main Shares", dataTableOutput(ns("weekly_species"))),
              tabPanel("Missed Cutoff", dataTableOutput(ns("subscription_nextweek"))),
              tabPanel("Duplicate Subscription Orders", dataTableOutput(ns("duplicate_subscriptions")))
              
              
            )
          )
        ),
        
        fixedPanel(
          
          bottom = 10, left = "45%",
          id = "share-size-fixedPanel",
          draggable = TRUE,
          h5("Share Size"),
          dataTableOutput(ns("share_size")),
          width = "200px"
        )
      ),
    )
  )
}

Main_Shares_Server <- function(id) {
  
  moduleServer(
    id, 
    function(input, output, session) {
      

      # Call Module -------------------------------------------------------------------
      clear_all <- reactive({input$clear_all})
      
      species_select4 <- species_selection_Server("species_select4", clear_all = clear_all)
      species_select3 <- species_selection_Server("species_select3", clear_all = clear_all, 
                                                  next_species = species_select4$species)
      species_select2 <- species_selection_Server("species_select2", clear_all = clear_all, 
                                                  next_species = species_select3$species)
      species_select1 <- species_selection_Server("species_select1", clear_all = clear_all, 
                                                  next_species = species_select2$species)
      

      weekly_species1 <- reactive({
        get_weekly_species(weekly_species_prev = weekly_species11,
                           species_selected = species_select1$species(),
                           sites_selected = species_select1$sites(),
                           is_1st_species = TRUE)
      })
      
      weekly_species2 <- reactive({
        get_weekly_species(weekly_species_prev = weekly_species1(), 
                           species_selected = species_select2$species(),
                           sites_selected = species_select2$sites(),
                           is_1st_species = FALSE)
      })
      
      weekly_species3 <- reactive({
        get_weekly_species(weekly_species_prev = weekly_species2(), 
                           species_selected = species_select3$species(),
                           sites_selected = species_select3$sites(),
                           is_1st_species = FALSE)
      })
      
      weekly_species4 <- reactive({
        get_weekly_species(weekly_species_prev = weekly_species3(), 
                           species_selected = species_select4$species(),
                           sites_selected = species_select4$sites(),
                           is_1st_species = FALSE) 
      })
      
      
      subs_all <- reactive({
        temp <- weekly_species4() %>%
  #      temp2<-weekly_species11 %>% 
          left_join(flashsales_Main_shares %>% filter(species_choice != "")) %>% 
          mutate(species_choice = ifelse(!is.na(share_upgrade), 
                                         as.character(share_upgrade), ""))%>%
          mutate(species = if_else(is.na(species_choice) | species_choice == "", 
                                   species, species_choice)) %>% 
          mutate(species = trimws(species))
        
      })
      
      # Tab: All Main Shares ----------------------------------------------------------------
      weekly_species <- reactive({
        subs_all() %>%
 #       temp3<-  temp2 %>% 
          select(customer_name, species, share_size, pickup_site, delivery_day) %>% 
          arrange(delivery_day, species, share_size, pickup_site)
      })
      
      # Tab: Share Counts -------------------------------------------------------------------
      share_count <- reactive({
        weekly_species() %>%
#          temp4<-  temp3 %>% 
          group_by(delivery_day, share_size, species) %>% 
          tally() %>% 
          arrange(delivery_day, species)
      })
      
      # Tab: Amounts Assigned  --------------------------------------------------------------
      share_fillet <- reactive({
        share_count() %>%
#          temp5<-temp4 %>% 
          mutate(type = str_extract(tolower(species), type_list)) %>%
          mutate(type = ifelse(tolower(species) %in% flash_fillet, NA_character_, type)) %>%
          mutate(type_singular = singularize(type)) %>%
          ## "F" sign: total weights are calculated using fillet portions
          mutate(fillet_sign = ifelse(is.na(type_singular),"<sup class='sup-fillet'>F</sup>", ""))%>%
          ## Co
          mutate(type_singular = ifelse(is.na(type_singular), input$weight, type_singular)) %>%
          left_join(share_size_variant, by = c("type_singular", "share_size"))%>%
          mutate(share_size_estimate = ifelse(type_singular != "lobster", 
                                              share_size_estimate, as.numeric(as.character(share_size)))) %>%
          mutate(total_amount = n * as.numeric(share_size_estimate))%>%
          group_by(delivery_day, species, fillet_sign) %>% 
          summarise(sum = sum(total_amount))
        
      })
      
      # Tab: No Assignment ----------------------------------------------------------------------
      no_assignment <- reactive({
        weekly_species() %>% 
          filter(species == "unassigned")
      })
      
      # Tab: Ordered a share but none scheduled -------------------------------------------------
      # no_match <- reactive({
      #   flashsales_Main_shares %>%
      #     mutate(name = as.character(name)) %>% 
      #     left_join(weekly_species4(), by = "name") %>% 
      #     filter(is.na(species)) %>% 
      #     select(name, share_upgrade)
      # })
    
      
      
      # Floating Window -------------------------------------------------------------------------
      share_size <- reactive({
        species <- tolower(c(input$weight, 
                             species_select1$species(),
                             species_select2$species(),
                             species_select3$species(),
                             species_select4$species()))
        species <- species[!is.na(species)] %>% singularize
        
        share_size_variant %>%
          filter(type_singular %in% species | type %in% tolower(flash_list)) %>%
          mutate(type = str_to_title(type))%>%
          select(type, share_size, share_size_estimate)%>%
          pivot_wider(values_from = "share_size_estimate", names_from = "type")%>%
          rename("Size" = "share_size")
      })
      
      # Output ====================================================================================
      
      ## Amounts Assigned
      output$share_fillet <- renderDataTable({
        datatable_export(share_fillet(), title = "Main Shares - Amounts Assigned",
                         escape = FALSE, remove_F = TRUE)
      })
      
      ## Share Counts
      output$share_count <- renderDataTable({
        datatable_export(share_count(), title = "Main Shares - Share Counts")
      })
      
      ## Share Pounds by Site
      output$fillet_weight_by_site <- renderDataTable({
        datatable_export(fillet_weight_by_site, title = "Main Shares - Share Pounds by Site")
      })
      
      ## No Assignment
      output$no_assignment <- renderDataTable({
        datatable_export(no_assignment(), title = "Main Shares - No Assignment")
      })
      
      ## All Main Shares
      output$weekly_species <- renderDataTable({
        datatable_export(weekly_species(), title = "Main Shares - All Main Shares")
      })
 
      
      ## Check new member cutoff 
      output$subscription_nextweek <- renderDataTable({
        datatable_export(subscription_nextweek, title = "Missed Cutoff - New Member Subscription")
      })
      
      ## Check new member cutoff 
      output$duplicate_subscriptions <- renderDataTable({
        datatable_export(double_order_check, title = "Check - Dublicate Subscription Orders")
      })
      
      
      # Share Size Floating Window =========================================================
      output$share_size <- renderDataTable({
        datatable(share_size(), rownames = FALSE,
                  options = list(paging = FALSE, searching = FALSE, info = FALSE))}
      )
      
      
    })
}