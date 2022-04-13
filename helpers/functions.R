# title_with_date: generate titles for Special Orders & Early Deadline Orders -------------
title_with_date <- function(title) {
  paste(title, wday_date(2), "to", wday_date(4)) 
}

# gs_url: generate Google Sheet publishing link
gs_url <- function(ss, sheets_id = "0", is_xlsx = FALSE) {
  if(is_xlsx) {pub <- "output=xlsx"}
  else {pub <- paste0("gid=", sheets_id, "&single=true&output=csv")}
  
  paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1v", ss,
         "/pub?", pub)
}

# read_gs_para: function to parallelize file reading process-------------------------------
read_gs_para <- function(ss, sheets_id, sheets_names) {
  
  cl <- makeCluster(mc <- getOption("cl.cores", min(4, detectCores())))
  
  url <- gs_url(ss, sheets_id)
  
  ls <- parLapply(cl = cl, url, function(x){
    read.csv(x, check.names = FALSE, stringsAsFactors = FALSE,
             encoding = "UTF-8")})%>%
    `names<-`(sheets_names)
  
  stopCluster(cl)
  
  return(ls)
}

# get_sheet_name: Search sheet name by keyword -------------------------------------------------
get_sheet_name <- function(sheet_names, keyword) {
  sheet_names[str_detect(sheet_names, keyword)]
}



# singularize: Function to obtain the singular form to a word in lower case -------------
singularize <- function(x) {
  ifelse(substr(x, nchar(x) - 2, nchar(x)) == "ies", 
         paste0(substr(x, 1, nchar(x) - 3), "y"),
         ifelse(substr(x, nchar(x), nchar(x)) == "s",
                substr(x, 1, nchar(x) - 1), x)
  )
}

# get_weekly_species: functions to obtain weekly species --------------------------------
# App(s): Main Shares & Species Assignment
get_weekly_species <- function(weekly_species_prev, 
                               species_selected, 
                               sites_selected,
                               share_size_selected = "All",
                               is_1st_species = TRUE) {
  
    if(species_selected == "None") {
      weekly_species_prev
    } 
    else {

      if(share_size_selected == "All") {
      share_size_selected = c("ExtraLarge", "Large", "Medium", "Small")
    }
    
    species_selected_category <- data.frame(type = species_selected) %>% 
      left_join(opt_out_extra, by = "type") %>%
      mutate(opt_out_category = ifelse(is.na(opt_out_category), 
                                       species_selected, opt_out_category))
    
    weekly_species_prev %>%
      mutate(species = ifelse((is_1st_species | species == "unassigned") 
                              & !(str_detect(opt_out, species_selected_category$opt_out_category)) 
                              & (pickup_site %in% sites_selected) 
                              # & str_detect(tolower(subscription), schedule_str)
                              & (share_size %in% share_size_selected), 
                              species_selected, species))
    }
}

# Filter shiny_category in Early Deadline Orders/Special Orders ---------------------------
filter_ED_SO <- function(df, category) {
  df %>%
    filter(str_detect(shiny_category, category)) %>%
    select(-shiny_category)
}



# filter_delivery_day: Define server logic to read selected file -------------------------
# App(s): Home Delivery
filter_delivery_day <- function(df, delivery_day_selected) {
  df %>% 
    filter(delivery_day == delivery_day_selected) %>% 
    select(-delivery_day)
}


# checklists_datatable: datatable formatter -----------------------------------------------
# App(s): Checklists

checklists_datatable <- function(dt, title, type) {
  if(type == "Site") {messageTop = checklist_message}
  else if (type == "Kitchen") {messageTop = ""}
  
  dt %>%
    mutate(` ` = ifelse(row_number() == 1, n(), "")) %>% 
    group_by(Name) %>%
    mutate(Name = ifelse(row_number() == 1, Name, ""))%>%
    datatable(
      rownames = FALSE,
      extensions = 'Buttons',
      class = "datatable-checklist row-border",
      options = list(
        scrollY = "370px",
        scrollCollapse = TRUE,
        paging = FALSE,
        # hide orders counts
        columnDefs = list(list(visible = FALSE, targets = 0)), 
        dom = 'Bfrtip',
        buttons = list(list(extend = "print", 
                            title = title,
                            text = "<i class=\"fa fa-print\"></i>Print for Individual Site",
                            messageTop = messageTop,
                            customize = printJS)
                       )
        )
      )
                      
}

# generate_checklists: generate checklists --------------------------------------------------
# App(s): Checklists

generate_checklists <- function(deliv_day = delivery_day_levels) {
  
  abb <- toupper(substr(deliv_day, 1, 3))

  subscription_check <- Active_Deliveries[[paste0("Labels_", abb)]] %>%
    ## Fix column names if there are accidental edits
    `colnames<-`(c("customer_name", 
                   "spacer_1", 
                   "share_size", 
                   "species",
                   "caught_by",   
                   "gear_type", 
                   "landing_port", 
                   "spacer_2", 
                   "expiration_day", 
                   "instructions",  
                   "spacer_3", 
                   "pickup_site_label", 
                   "next_delivery", 
                   "home_delivery_name"))%>%
    mutate(type = "subscription") %>% 
    mutate(customer_name = as.character(customer_name)) %>% 
    mutate(home_delivery_name = as.character(home_delivery_name)) %>% 
    mutate(customer_name = ifelse(customer_name == "Home Delivery", home_delivery_name, customer_name)) %>% 
    select(pickup_site_label, customer_name, species, share_size, type)
  
#  flashsale_check <- Active_Deliveries[[paste0("Flashsales_", "THU")]] %>%
  flashsale_check <- Active_Deliveries[[paste0("Flashsales_", abb)]] %>%
    ## Fix column names if there are accidental edits
    `colnames<-`(c("Timestamp",
                   "customer_email",
                   "customer_name",
                   "spacer_1",
                   "share_type",
                   "share_size",
                   "spacer_2",
                   "pickup_site_label",
                   "date",
                   "instructions_1",
                   "instructions_2",
                   "schedule",
                   "source",
                   "name_form",
                   "order",
                   "price",
                   "delivery_day")) %>%
    mutate(Timestamp = as.character(Timestamp),
           customer_name = as.character(customer_name)) %>%
    ## New member labels are sometimes pasted into Flashsalse spreadsheets for printing purposes.
    ## The first column will be ~ Welcome to Get Hooked ~. Filter out those labels according to this
    filter(!str_detect(Timestamp, "~") | !Timestamp == "welcome" | is.na(Timestamp)) %>%
    filter(!str_detect(pickup_site_label, "- [0-9]")) %>%
    mutate(type = "Add-on") %>%
    select(pickup_site_label, customer_name, species = share_type, share_size, type)
  
  deliveries_complete <- rbind(subscription_check, flashsale_check)%>%
    mutate(pickup_site_label = trimws(pickup_site_label))
  
#   wrong_list <- rbind(subscription_check %>% mutate(source = paste("Labels_", abb)), 
#                       flashsale_check %>% mutate(source = paste("Flashsales_", abb))) %>% 
#     mutate(pickup_site_label = trimws(pickup_site_label)) %>% 
#     left_join(pickup_sites_key, by = "pickup_site_label") %>%
# #    filter(production_day != deliv_day) %>%
#     select(source, everything())
  
  if(nrow(deliveries_complete) > 0) {
    
    Kitchen_list <- deliveries_complete %>%
      filter(pickup_site_label != "") %>%
      mutate(customer_name = str_to_title(customer_name),
             type = str_to_title(type))%>%
      `colnames<-`(c("Pickup Site", "Name", "Item", "Size", "Type")) %>%
      arrange(`Pickup Site`, Name) %>%
      split(f = .$`Pickup Site`)%>%
      lapply(function(df) {df %>% mutate(`Pickup Site` = " ") %>% 
          rename(" " = "Pickup Site")})
    
    Site_list <- Kitchen_list[which(!(str_detect(names(Kitchen_list), "Home Delivery")))]

    if(length(Kitchen_list) == 0) {
      Kitchen_list <- list(NULL) %>% `names<-`("No Data")
      Site_list <- Kitchen_list
    } 
    
    if(length(Site_list) == 0) {
      Site_list <- list(NULL) %>% `names<-`("No Data")
    } 
  }
  else{
    Kitchen_list <- list(NULL) %>% `names<-`("No Data")
    Site_list <- Kitchen_list
  }
  
  return(list(Kitchen_list = Kitchen_list, 
              Site_list = Site_list
              #wrong_list = wrong_list
              ))
}


## Combine sheets in Active Deliveries by rows
rbind_active_deliveries <- function(type = c("Labels", "Flashsales")) {
  
  if(type == "Labels") {
    colNames <- c(
      "customer_name", "spacer_1", "share_size", "species", "caught_by",
      "gear_type", "landing_port", "spacer_2", "expiration_day", "instructions",
      "spacer_3", "pickup_site_label", "next_delivery", "home_delivery_name"
    )
  }
  else {
    colNames <- (c("Timestamp",
                   "customer_email",
                   "customer_name",
                   "spacer_1",
                   "share_type",
                   "share_size",
                   "spacer_2",
                   "pickup_site_label",
                   "date",
                   "instructions_1",
                   "instructions_2",
                   "schedule",
                   "source",
                   "name_form",
                   "order",
                   "price",
                   "delivery_day"))
  }

  lapply(delivery_day_levels, function(deliv_day) {
           Active_Deliveries[[paste0(type, "_", toupper(substr(deliv_day, 1, 3)))]] %>%
             `colnames<-`(colNames) %>% mutate(delivery_day = deliv_day)}) %>%
    do.call(rbind, .) %>%
    mutate(delivery_day = factor(delivery_day, levels = delivery_day_levels))
}

## datatable_export: datatable with "Export to CSV" button
datatable_export <- function(dt, title, escape = TRUE, remove_F = FALSE) {
  
  
  if(remove_F) {
    export_options <- list(columns = c(1, 2, 4))
  }
  else {
    export_options <- list(columns = 1:ncol(dt))
  }
    
  datatable(
    dt, 
    escape = escape,
    extensions = 'Buttons',
    options = list(
      scrollX = TRUE, 
      scrollY = "400px",
      scrollCollapse = TRUE, 
      paging = FALSE,
      dom = 'Bfrtip',
      buttons = list(
        list(extend = "csv",
             title = title,
             text = "<i class=\"fas fa-file-csv\"></i>Export to CSV",
             exportOptions = export_options
             )
        )
      )
    )
}