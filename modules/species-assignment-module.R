Species_Assignment_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        12,
        
        # Application title
        titlePanel("Species Assignment"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
          sidebarPanel(
            
            h4("Data Source"),
            tags$div(
              a("Shopify Orders - Subscriptions",
                href = "https://docs.google.com/spreadsheets/d/1SIOuuBBOXQ9-oCWbN-Hv9bHirxZH9S1RrJSGiaphK1Y/edit#gid=16853773")
            ),
            br(),
            
            h3("Step 1: Assign"),
            tags$div(
              "See ",
              a("Pick Up Site Key",
                href = "https://docs.google.com/spreadsheets/d/12uKbv9IXcIXMIFGZNHUMgjlAb8i1zbIPFYzUrtDQXRk/edit#gid=0"),
              "for details on site and delivery day."
            ),
            br(),
            
            ## species selection 
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
            
            conditionalPanel(
              condition = "input['species_select4-species'] != 'None'",
              ns = ns,
              species_selection_UI(ns("species_select5"), 5)
            ),
            
            conditionalPanel(
              condition = "input['species_select5-species'] != 'None'",
              ns = ns,
              species_selection_UI(ns("species_select6"), 6)
            ),
            
            actionButton(
              ns("clear_all"),
              "Clear All Species Selections",
              icon = icon("broom"),
              class = "btn-clear-species"
            ),
            
            
            h3("Step 2: Calculate"),
            
            selectInput(
              ns("weight"),
              label = paste("Choose the share type. Either choice will also ensure",
                            "that the selected species are matched with applicable share weight/number"),
              choices = c("Fillet" = "fillet", "Harbinger Meal Tray" = "harbinger"),
              selected = "Fillet"
            ),
            
            br(),
            
            h3("Step 3: Modify"),
            
            modifiy_UI(ns("modify1"), 1),
            
            conditionalPanel(
              condition = "input['modify1-species'] != 'None'",
              ns = ns,
              modifiy_UI(ns("modify2"), 2)
            ),
            
            conditionalPanel(
              condition = "input['modify2-species'] != 'None'",
              ns = ns,
              modifiy_UI(ns("modify3"), 3)
            ),
            
            conditionalPanel(
              condition = "input['modify3-species'] != 'None'",
              ns = ns,
              modifiy_UI(ns("modify4"), 4)
            ),
            
            conditionalPanel(
              condition = "input['modify4-species'] != 'None'",
              ns = ns,
              modifiy_UI(ns("modify5"), 5)
            ),
            
            conditionalPanel(
              condition = "input['modify5-species'] != 'None'",
              ns = ns,
              modifiy_UI(ns("modify6"), 6)
            ),
            
            conditionalPanel(
              condition = "input['modify6-species'] != 'None'",
              ns = ns,
              modifiy_UI(ns("modify7"), 7)
            ),
            
            conditionalPanel(
              condition = "input['modify7-species'] != 'None'",
              ns = ns,
              modifiy_UI(ns("modify8"), 8)
            ),
            
            conditionalPanel(
              condition = "input['modify8-species'] != 'None'",
              ns = ns,
              modifiy_UI(ns("modify9"), 9)
            ),
            
            conditionalPanel(
              condition = "input['modify9-species'] != 'None'",
              ns = ns,
              modifiy_UI(ns("modify10"), 10)
            ),
            
            conditionalPanel(
              condition = "input['modify10-species'] != 'None'",
              ns = ns,
              modifiy_UI(ns("modify11"), 11)
            ),
            
            conditionalPanel(
              condition = "input['modify11-species'] != 'None'",
              ns = ns,
              modifiy_UI(ns("modify12"), 12)
            ),
            
            
            h3("Step 4: Download"),
            
            strong("Main Share Labels"),
            uiOutput(ns("downloadLabels")),
            
            
            strong("OP Email List"),
            downloadButton(ns("downloadOP"), "Download OP Email List"),
            
            strong("New Member Bag Labels"),
            downloadButton(ns("downloadCoolerLabels"), "Download New Member Bag Labels")
          ),
          mainPanel(
            
            tabsetPanel(
              tabPanel("Amounts Assigned",                   dataTableOutput(ns("share_fillet"))),
              tabPanel("Share Counts",                       dataTableOutput(ns("share_count"))),
              tabPanel("Share Pounds by Site",               dataTableOutput(ns("fillet_weight_by_site"))),
              tabPanel("No Assignment",                      dataTableOutput(ns("no_assignment"))),
              tabPanel("All Main Shares",                    dataTableOutput(ns("weekly_species")))            
              
              
            )
          )
        )
      )
    ),
    
    fixedPanel(
      bottom = 10, left = "45%",
      draggable = TRUE,
      h5("Share Size"),
      dataTableOutput(ns("share_size")),
      width = "200px",
      id = "share-size-fixedPanel"
    )
  )
}

Species_Assignment_Server <- function(id) {
  moduleServer(
    id, 
    
    # Define server logic required for species assignment
    server <- function(input, output, session) {
      
      # Call Species Selection Module ----------------------------------------------------------
      clear_all <- reactive({input$clear_all})
      
      # Below are in reversed order (6 -> 1) because "Clear Last Species" button
      # is based on the newest species
      
      species_select6 <- species_selection_Server(
        "species_select6", 
        is_species_assignment = TRUE,
        clear_all = clear_all)

      species_select5 <- species_selection_Server(
        "species_select5", 
        is_species_assignment = TRUE,
        next_species = species_select6$species, 
        clear_all = clear_all)      
            
      species_select4 <- species_selection_Server(
        "species_select4", 
        is_species_assignment = TRUE,
        next_species = species_select5$species, 
        clear_all = clear_all)
            
      species_select3 <- species_selection_Server(
        "species_select3", 
        is_species_assignment = TRUE,
        next_species = species_select4$species, 
        clear_all = clear_all)
      
      species_select2 <- species_selection_Server(
        "species_select2", 
        is_species_assignment = TRUE,
        next_species = species_select3$species, 
        clear_all = clear_all)
      
      species_select1 <- species_selection_Server(
        "species_select1", 
        is_species_assignment = TRUE,
        next_species = species_select2$species, 
        clear_all = clear_all)
      
      
      # Data input ============================================================================
      
      #removed because no longer using imported sheets
      
      weekly_species1 <- reactive({
        get_weekly_species(weekly_species_prev = weekly_species11,
                           species_selected = species_select1$species(),
                           sites_selected = species_select1$sites(),
                           share_size_selected = species_select1$share_size(),
                           is_1st_species = TRUE)
      })
      
      weekly_species2 <- reactive({
        get_weekly_species(weekly_species_prev = weekly_species1(), 
                           species_selected = species_select2$species(),
                           sites_selected = species_select2$sites(),
                           share_size_selected = species_select2$share_size(),
                           is_1st_species = FALSE)
      })
      
      weekly_species3 <- reactive({
        get_weekly_species(weekly_species_prev = weekly_species2(), 
                           species_selected = species_select3$species(),
                           sites_selected = species_select3$sites(),
                           share_size_selected = species_select3$share_size(),
                           is_1st_species = FALSE)
      })
      
      weekly_species4 <- reactive({
        get_weekly_species(weekly_species_prev = weekly_species3(), 
                           species_selected = species_select4$species(),
                           sites_selected = species_select4$sites(),
                           share_size_selected = species_select4$share_size(),
                           is_1st_species = FALSE)
      })
      
      weekly_species5 <- reactive({
        get_weekly_species(weekly_species_prev = weekly_species4(), 
                           species_selected = species_select5$species(),
                           sites_selected = species_select5$sites(),
                           share_size_selected = species_select5$share_size(),
                           is_1st_species = FALSE)
      })
      
      weekly_species6 <- reactive({
        get_weekly_species(weekly_species_prev = weekly_species5(), 
                           species_selected = species_select6$species(),
                           sites_selected = species_select6$sites(),
                           share_size_selected = species_select6$share_size(),
                           is_1st_species = FALSE)
      })
      
      
      subs_all <- reactive({
        temp <- weekly_species6() %>%
          left_join(flashsales_Main_shares %>% filter(species_choice != "")) %>% 
          mutate(species_choice = ifelse(!is.na(share_upgrade), 
                                         as.character(share_upgrade), ""))%>%
          mutate(species = if_else(is.na(species_choice) | species_choice == "", species, species_choice)) %>% 
          mutate(species = trimws(species))
          # left_join(flashsales_Main_shares %>% filter(species_choice != ""), by = "customer_email") %>%
          # mutate(species_choice = ifelse(!is.na(share_upgrade) & !(str_detect(share_upgrade, "Stick")), 
          #                                as.character(share_upgrade), "")) %>% 
          # mutate(species = ifelse(species_choice == "", species, str_remove(species_choice, "(?=\\-).+")),
          #        species = trimws(species)) 
        
        
      })
      
      # Tabs ==============================================================================
      
      ## Tab: All Main Shares -------------------------------------------------------------
      weekly_species <- reactive({
        subs_all() %>% 
          select(customer_name, species, share_size, delivery_day, 
                 pickup_site, pickup_site_label, customer_email) %>% 
          arrange(delivery_day, species, share_size, pickup_site_label)
      })
      
      ## Tab: Share Counts -----------------------------------------------------------------
      share_count <- reactive({
        weekly_species() %>%
#          filter(share_size != "FishHead") %>% 
          group_by(delivery_day, share_size, species) %>% 
          tally() %>% 
          arrange(species)
      })
      
      ## Tab: Amounts Assigned  ------------------------------------------------------------
      share_fillet <- reactive({
        share_count() %>% 
          mutate(type = str_extract(tolower(species), type_list)) %>%
          mutate(type_singular = singularize(type)) %>%
          mutate(fillet_sign = ifelse(is.na(type_singular),"<sup class='sup-fillet'>F</sup>", ""))%>%
          mutate(type_singular = ifelse(is.na(type_singular), input$weight, 
                                        as.character(type_singular))) %>%
          left_join(share_size_variant, by = c("type_singular", "share_size"))%>%
          mutate(share_size_estimate = ifelse(type_singular == "lobster",
                                              as.numeric(as.character(share_size)),
                                              share_size_estimate)) %>%
          mutate(total_amount = n * as.numeric(share_size_estimate))%>%
          group_by(delivery_day, species, fillet_sign) %>% 
          summarise(sum = sum(total_amount))
      })
      
      ## Tab: Share Pound by Sites --------------------------------------------------------
      #fillet_weight_by_site 
      
      
      ## Tab: No Assignment ---------------------------------------------------------------
      no_assignment <- reactive({
        weekly_species() %>% 
          filter(species == "unassigned")
      })
      
      #
      # Floating Window ===================================================================
      share_size <- reactive({
        species <- tolower(c(input$weight, 
                             species_select1$species(),
                             species_select2$species(),
                             species_select3$species(),
                             species_select4$species(),
                             species_select5$species(),
                             species_select6$species()))
        
        species <- species[!is.na(species)] %>% singularize
        
        share_size_variant %>%
          filter(type_singular %in% species | type %in% tolower(flash_list)) %>%
          mutate(type = str_to_title(type))%>%
          select(type, share_size, share_size_estimate)%>%
          pivot_wider(values_from = 3, names_from = 1)%>%
          rename("Size" = "share_size")
      })
      
      # Call Modification Module & Modify Species Selections ===============================
      
      species_list <- reactive({
        weekly_species()$species %>% 
          unique %>% sort %>% c("None")
      })
      
      modify1 <- modify_Server("modify1", species_list)
      modify2 <- modify_Server("modify2", species_list)
      modify3 <- modify_Server("modify3", species_list)
      modify4 <- modify_Server("modify4", species_list)
      modify5 <- modify_Server("modify5", species_list)
      modify6 <- modify_Server("modify6", species_list)
      modify7 <- modify_Server("modify7", species_list)
      modify8 <- modify_Server("modify8", species_list)
      modify9 <- modify_Server("modify9", species_list)
      modify10 <- modify_Server("modify10", species_list)
      modify11 <- modify_Server("modify11", species_list)
      modify12 <- modify_Server("modify12", species_list)
      
      
      weekly_species_final <- reactive({
        weekly_species() %>% 
          mutate(species_change = case_when(
            species == modify1$species() & modify1$changeSpecies() != "New species name" ~ modify1$changeSpecies(),
            species == modify2$species() & modify2$changeSpecies() != "New species name" ~ modify2$changeSpecies(),
            species == modify3$species() & modify3$changeSpecies() != "New species name" ~ modify3$changeSpecies(),
            species == modify4$species() & modify4$changeSpecies() != "New species name" ~ modify4$changeSpecies(),
            species == modify5$species() & modify5$changeSpecies() != "New species name" ~ modify5$changeSpecies(),
            species == modify6$species() & modify6$changeSpecies() != "New species name" ~ modify6$changeSpecies(),
            species == modify7$species() & modify7$changeSpecies() != "New species name" ~ modify7$changeSpecies(),
            species == modify8$species() & modify8$changeSpecies() != "New species name" ~ modify8$changeSpecies(),
            species == modify9$species() & modify9$changeSpecies() != "New species name" ~ modify9$changeSpecies(),
            species == modify10$species() & modify10$changeSpecies() != "New species name" ~ modify10$changeSpecies(),
            species == modify11$species() & modify11$changeSpecies() != "New species name" ~ modify11$changeSpecies(),
            species == modify12$species() & modify12$changeSpecies() != "New species name" ~ modify12$changeSpecies(),
            TRUE ~ species)
          ) %>%
          mutate(caught_by = case_when(
            species == modify1$species() ~ modify1$fisherman(),
            species == modify2$species() ~ modify2$fisherman(),
            species == modify3$species() ~ modify3$fisherman(),
            species == modify4$species() ~ modify4$fisherman(),
            species == modify5$species() ~ modify5$fisherman(),
            species == modify6$species() ~ modify6$fisherman(),
            species == modify7$species() ~ modify7$fisherman(),
            species == modify8$species() ~ modify8$fisherman(),
            species == modify9$species() ~ modify9$fisherman(),
            species == modify10$species() ~ modify10$fisherman(),
            species == modify11$species() ~ modify11$fisherman(),
            species == modify12$species() ~ modify12$fisherman(),
            TRUE ~ "")
          ) %>%
          mutate(gear_type = case_when(
            species == modify1$species() ~ paste("Gear type:", modify1$gear()),
            species == modify2$species() ~ paste("Gear type:", modify2$gear()),
            species == modify3$species() ~ paste("Gear type:", modify3$gear()),
            species == modify4$species() ~ paste("Gear type:", modify4$gear()),
            species == modify5$species() ~ paste("Gear type:", modify5$gear()),
            species == modify6$species() ~ paste("Gear type:", modify6$gear()),
            species == modify7$species() ~ paste("Gear type:", modify7$gear()),
            species == modify8$species() ~ paste("Gear type:", modify8$gear()),
            species == modify9$species() ~ paste("Gear type:", modify9$gear()),
            species == modify10$species() ~ paste("Gear type:", modify10$gear()),
            species == modify11$species() ~ paste("Gear type:", modify11$gear()),
            species == modify12$species() ~ paste("Gear type:", modify12$gear()),
            TRUE ~ "Gear type:")
          ) %>%
          mutate(expiration_day = case_when(
            species == modify1$species() ~ modify1$expiration(),
            species == modify2$species() ~ modify2$expiration(),
            species == modify3$species() ~ modify3$expiration(),
            species == modify4$species() ~ modify4$expiration(),
            species == modify5$species() ~ modify5$expiration(),
            species == modify6$species() ~ modify6$expiration(),
            species == modify7$species() ~ modify7$expiration(),
            species == modify8$species() ~ modify8$expiration(),
            species == modify9$species() ~ modify9$expiration(),
            species == modify10$species() ~ modify10$expiration(),
            species == modify11$species() ~ modify11$expiration(),
            species == modify12$species() ~ modify12$expiration(),
            TRUE ~ "")
          ) %>%
          mutate(landing_port = case_when(
            species == modify1$species() ~ paste("Port of landing:", modify1$port()),
            species == modify2$species() ~ paste("Port of landing:", modify2$port()),
            species == modify3$species() ~ paste("Port of landing:", modify3$port()),
            species == modify4$species() ~ paste("Port of landing:", modify4$port()),
            species == modify5$species() ~ paste("Port of landing:", modify5$port()),
            species == modify6$species() ~ paste("Port of landing:", modify6$port()),
            species == modify7$species() ~ paste("Port of landing:", modify7$port()),
            species == modify8$species() ~ paste("Port of landing:", modify8$port()),
            species == modify9$species() ~ paste("Port of landing:", modify9$port()),
            species == modify10$species() ~ paste("Port of landing:", modify10$port()),
            species == modify11$species() ~ paste("Port of landing:", modify11$port()),
            species == modify12$species() ~ paste("Port of landing:", modify12$port()),
            TRUE ~ "Port of landing:")
          ) %>%
          mutate(instructions = case_when(
            species == modify1$species() ~ modify1$instruction(),
            species == modify2$species() ~ modify2$instruction(),
            species == modify3$species() ~ modify3$instruction(),
            species == modify4$species() ~ modify4$instruction(),
            species == modify5$species() ~ modify5$instruction(),
            species == modify6$species() ~ modify6$instruction(),
            species == modify7$species() ~ modify7$instruction(),
            species == modify8$species() ~ modify8$instruction(),
            species == modify9$species() ~ modify9$instruction(),
            species == modify10$species() ~ modify10$instruction(),
            species == modify11$species() ~ modify11$instruction(),
            species == modify12$species() ~ modify12$instruction(),
            TRUE ~ "")
          ) %>%
          mutate(next_delivery = get_delivery_date(delivery_day)) %>%
          mutate(next_delivery = as.Date(next_delivery, origin = lubridate::origin)) %>% 
          mutate(next_delivery = format(next_delivery, "%m/%d/%y")) %>%
          mutate(species_name = species) %>% 
          mutate(species = species_change) %>% 
          select(-species_change)
      })
      
      
      # Output ========================================================================
      
      ## Output: Datatables -----------------------------------------------------------
      
      ### Amounts Assigned
      output$share_fillet <- renderDataTable({
        datatable_export(share_fillet(), title = "Species Assignment - Amounts Assigned", 
                         escape = FALSE, remove_F = TRUE)
      })
      
      ### Share Counts
      output$share_count <- renderDataTable({
        datatable_export(share_count(), title = "Species Assignment - Share Counts")
      })
      
      ## Share Pounds by Site
      output$fillet_weight_by_site <- renderDataTable({
        datatable_export(fillet_weight_by_site, title = "Species Assignment - Share Pounds by Site")
      })
      
      ### No Assignment
      output$no_assignment <- renderDataTable({
        datatable_export(no_assignment(), title = "Species Assignment - No Assignment")
      })
      
      ### All Main Shares
      output$weekly_species <- renderDataTable({
        datatable_export(weekly_species(), title = "Species Assignment - All Main Shares")
      })
      
      
      ## Output: Floating Window -------------------------------------------------------------
      output$share_size <- renderDataTable({
        datatable(share_size(), rownames = FALSE,
                  options = list(paging = FALSE, searching = FALSE, info = FALSE))
      })
      
      ## Output: Downloads Buttons ------------------------------------------------------------
      
      ### Button: Download TUE/WED/THU/LA Main Share Labels -----------------------------------
      
      labels_print <- reactive({
        weekly_species_final() %>% 
          mutate(home_delivery_name = ifelse(str_detect(pickup_site_label, "Home Delivery"), customer_name, " ")) %>% 
          mutate(customer_name = ifelse(str_detect(pickup_site_label, "Home Delivery"), 
                               "Home Delivery", customer_name)) %>%
          mutate(spacer_1 = "~", spacer_2 = "~", spacer_3 = "~~~~~~~~~~") %>%
          left_join(share_size_list) %>%
          select(customer_name, spacer_1, share_size_label, species, spacer_1, caught_by, gear_type,
                 landing_port, spacer_2, expiration_day, instructions, spacer_3,
                 pickup_site_label, next_delivery, home_delivery_name, delivery_day) %>%
          arrange(next_delivery, species, share_size_label, pickup_site_label,
                  customer_name, home_delivery_name) %>%
          mutate(delivery_day = toupper(substr(delivery_day, 1, 3)))%>%
          split(.$delivery_day) %>% 
          lapply(function(df) {df %>% select(-delivery_day)})
      })
      
      ### button UI; lapply to generate buttons automatically according to values in delivery_day_levels
      output$downloadLabels <- renderUI({
        lapply(delivery_day_levels_abb, function(abb) {
          downloadButton(session$ns(paste0("downloadLabels", abb)),
                         label = paste("Download", abb, "Main Share Labels"),
                         class = "btn-grey")})
      })
      
      ### download handler
      lapply(delivery_day_levels_abb, function(abb) {
        output[[paste0("downloadLabels", abb)]] <- downloadHandler(
          filename = paste0("Main_Share_Labels_", abb, ".csv"),
          content = function(file) {
            write.csv(labels_print()[[abb]], file, row.names = FALSE)
          })
      })
      
      ### Button: Download OP Email List  -----------------------------------------------
      op_file <- reactive({
        
        primary_subscription <- weekly_species_final() %>%
          separate(customer_name, c("first_name", "last_name")) %>% 
          select(last_name, first_name, customer_email, pickup_site, share_size, 
                 delivery_day, species, next_delivery) %>%
          mutate(type = "subscriber") %>% 
          mutate(species = ifelse(str_detect(species, "Lobster"), "Live Lobster", species)) %>% 
          select(-share_size) %>% 
          distinct() %>% 
          mutate(day_tag = format(today(), "%m.%d.%y")) %>% 
          mutate(species = paste0(day_tag, species)) %>%
          select(-day_tag) 
        
        partner_subscription <- primary_subscription %>% 
          left_join(partner_email) %>% 
          filter(!is.na(partner_email)) %>% 
          mutate(type = "partner") %>% 
          mutate(customer_email = partner_email) %>% 
          select(last_name, first_name, customer_email, pickup_site,	delivery_day,	species,	next_delivery,	type)
        
        rbind(primary_subscription, partner_subscription)
          
      
      })
      
      output$downloadOP <- downloadHandler(
        filename = "OP_file.csv",
        content = function(file) {
          write.csv(op_file(), file, row.names = FALSE)
        }
      )
      
      ### Button: Download New Member Labels ----------------------------------------------
      
      output$downloadCoolerLabels <- downloadHandler(
        filename = "New_Member_Bag_Labels.csv",
        content = function(file) {
          write.csv(cooler_bag_label, file, row.names = FALSE)
        }
      )
      
    }
  )
}