library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(janitor)
library(DT)
library(polished)
library(parallel)
library(readxl)

# 1. Configuration ==========================================================================

## Configure ShinyApps account
rsconnect::setAccountInfo(name='gethookedseafood',
                          token='2404697D4F5E382C8A08C04F99BAD497',
                          secret='UyQyJ38YJgS+EkreQkiQtYhUjXmVVLeZ+sVlSb9E')

# 2. Load Functions and Constant Values =====================================================
source(file.path("helpers", "functions.R"))
source(file.path("helpers", "data cleaning.R"))
source(file.path("www/data", "constants.R"))

# 3. Read Google Sheets =====================================================================

### Load Multiple worksheets from one document ----------------------------------------------
## sheet ids (the string of numbers after pub?gid=)305766482
Product_KEYS_id      <- c("633942957", "1819169532")
Active_Deliveries_id <- c("684811568", "346693116", "1627046312", "544947288",
                          "1544807263",  "1232985667",  "1443610327", "1160831953")

## Sheet names
Product_KEYS_names      <- c("Share_Size_Variant", "Species_Options")
Active_Deliveries_names <- c(paste0("Labels_",     delivery_day_levels_abb), 
                             paste0("Flashsales_", delivery_day_levels_abb))


## Read data
Product_KEYS      <- read_gs_para(ss = ss["product_keys"], Product_KEYS_id, Product_KEYS_names) 
Active_Deliveries <- read_gs_para(ss = ss["active_deliveries"], Active_Deliveries_id, Active_Deliveries_names)

## Weekly Orders --------------------------------------------------------------------
Shopify_Orders <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSeeQbMhljx46LSRodW91lORPYFa9y4CIAS2rvRf4hk79JMebqRlP7DaNZCGeOj2MhNhMkVxREjhal5/pub?gid=1727009410&single=true&output=csv")


## Only one worksheet is used --------------------------------------------------------------------
customer_raw <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSAnOCUG04TybjFkJSQnLK09R64Hi51FAylgoEVfAk5VEujZts1SSCVNwu_ij8LUH_2V3s6rcvxc8tY/pub?gid=1076032319&single=true&output=csv")


# 4. Data Cleaning ==============================================================================

## * customer details  ----------------------------------------------------------

shopify_customers <- customer_raw %>% 
  clean_colname(type = "Customer")

## * Orders  ----------------------------------------------------------

orders_df <- Shopify_Orders %>% 
  clean_colname(type = "Orders") %>% 
  mutate(index = row_number())

  ## * Customer Details to orders this week -------------------------------------------------------------
    id_match <- orders_df %>% 
      select(-customer_email, -customer_name) %>% 
      left_join(shopify_customers %>% mutate(match = "customer_id"))
    
    email_match <- orders_df %>%
      select(-customer_id, -customer_name) %>% 
      left_join(shopify_customers %>% mutate(match = "customer_email"))
    
    name_match <- orders_df %>%
      select(-customer_id, -customer_email) %>% 
      left_join(shopify_customers %>% mutate(match = "customer_name"))
    
    order_details <- rbind(id_match, email_match, name_match) %>% 
      filter(!is.na(match)) %>% 
      group_by(index) %>% 
      mutate(match_index = row_number()) %>% 
      ungroup() %>% 
      filter(match_index == 1) %>% 
      select(-match_index) %>% 
      clean_customer() %>%
      
      #identifying new members that signed up passed monday cutoff 
      clean_date() %>% 
    
     #remove test accounts
      filter(!str_detect(customer_tags, "Test Account"))
    
    
    # * Missing Details Match
    no_customer_match <- orders_df %>% 
      left_join(order_details %>% select(index, match) %>% mutate(index = as.integer(index))) %>% 
      filter(is.na(match))


## * subscriptions for Active Members  ----------------------------------------------------------

shopify_subscription <- order_details %>%
  
  filter(inventory_type == "Subscription Main Share") %>%
  clean_subscription() %>%
    
  #new members
  new_member_cutoff() 

  ## Active subscriptions for the week - used in mainshare & species assignment  
  subscription <- shopify_subscription  %>% 
     select(-delivery_week)

  ## New subscriptions for next week - to be exported and fulfilled the following week 
  subscription_nextweek <- shopify_subscription %>% 
    filter(delivery_week == "Next Week") %>% 
    mutate(error_type = "Missed Cutoff")
  
  ## check for multiple of the same subscription order
  double_order_check <- shopify_subscription %>% 
    group_by(customer_email, share_size) %>% 
    tally() %>% 
    filter( n > 1)


  
## * Incoming orders ---------------------------------------------------------------------------


all_shopify_orders  <- order_details %>%
  filter(inventory_type != "Subscription Main Share") %>%
#  filter(str_detect(order_id, "#")) %>%
  new_member_cutoff() %>%

  #route notation for checklists if order is "dry goods"
  mutate(route_notation = ifelse(packing_method == "Bag", "yes", ""),
         share_class = checklist_notation) %>% 
  mutate(share_type = product_name) %>%
  
  #shiny categorys to sort deadlines, fresh & inventoried products 
  left_join(shiny_category_df) %>%
  mutate_at(vars(quantity, size), ~as.numeric(.))


shopify_orders <- all_shopify_orders %>% 
#  filter(delivery_week == "") %>% 
  select(-delivery_week)
 

## * Order Errors --------------------------------------------------------

#Flashsale order but no subscription order 
error_no_sub <- all_shopify_orders %>% 
  left_join(shopify_subscription %>% select(customer_email) %>% mutate(source = "subscription") %>% unique()) %>% 
  filter(is.na(source)) %>% 
  mutate(error_type = "no subscription order") %>% 
  select(-source, -route_notation, -share_class, -shiny_category, -delivery_week)

#Product is missing category
error_missing_category <- order_details %>% 
  filter(is.na(inventory_type)|is.na(deadline_type)) %>% 
  mutate(error_type = "missing category")


flashsale_error <- rbind(error_no_sub, error_missing_category)

# 4.0 Dashboard =================================================================================

# Subscription count
# sub_n <- nrow(shopify_subscription)

# New customer count
new_customer_df <- shopify_customers %>% 
  filter(customer_created_at > week_start)

#new_customer_n <- nrow(new_customer_df)

# Active Customer count
active_customer_df <- shopify_customers %>% 
  filter(str_detect(customer_tags, "Active Subscriber"))

#active_customer_n <- nrow(active_customer_df)



# 4.1 Main Shares & Species Assignment ==========================================================

## * Share Size Variant & Species Options --------------------------------------------------------

#For adding share size (weight/piece count etc...) to species assignment labels
share_size_variant_list<- Product_KEYS$Share_Size_Variant %>% 
  select(type, share_size = size, label_weight)

fillet_options <- Product_KEYS$Species_Options %>%
  select(options, type = option_type) %>% 
  left_join(share_size_variant_list) %>% 
  filter(!is.na(label_weight)) %>% 
  select(-type) %>% 
  select(type = options, share_size, label_weight)

share_size_list <- rbind(share_size_variant_list, fillet_options) %>% 
  select(species_name = type, share_size, label_weight) %>% 
  mutate(share_size_label = paste0(as.character(share_size), " ", "(", as.character(label_weight), ")"))


share_size_variant <- Product_KEYS$Share_Size_Variant %>%
  filter(size_key == "share") %>%
  filter(size %in% c("Small", "Medium", "Large", "ExtraLarge"))%>%
  mutate(type = tolower(type)) %>%
  mutate(type_singular = singularize(type)) %>%
  mutate(share_size_estimate = ifelse(is.na(weight), pieces, weight + 0.02))%>%
  select(type, type_singular, size, share_size_estimate)%>%
  rename("share_size" = "size")

## Used in Main_Shares_Server and Species_Assignment_Server
type_list <- unique(c(share_size_variant$type,
                      share_size_variant$type_singular))%>% 
  paste(collapse = "|")


## Choices of "Select Species" drop-down list in the sidebar
species_options_raw <- Product_KEYS$Species_Options$options
share_size_variant_type <- str_to_title(unique(share_size_variant$type))
not_species         <- str_detect(tolower(species_options_raw),
                                  "\\bother species\\b|\\bnone\\b|all one species")

species_list         <- c(species_options_raw[!not_species], 
                          share_size_variant_type[share_size_variant_type!="fillet"]) %>% unique %>% sort

species_options     <- append(sort(species_options_raw[not_species]),
                              species_list, after = 1)

## Used in clean_subscription to specify opt outs
### Some opt outs correspond to multiple species in the species selection drop-down list
### (e.g. Shrimp -> Ridgeback Shrimp, Pink Shrimp, Spot Prawn)
opt_out_extra <- Product_KEYS$Share_Size_Variant %>% 
  select(type, opt_out_category) %>% unique() %>%
  mutate(opt_out_category = str_to_title(opt_out_category),
         type = str_to_title(type)) %>%
  filter(type != opt_out_category) %>%
  filter(opt_out_category != "") 



### Choices for "Select Delivery Locations" in the sidebar

sites_df <- order_details %>% 
  distinct(pickup_site, .keep_all = TRUE)

sites <- sites_df$pickup_site 
  

### Split sites into a list by delivery day
sites_list <- split(sites, sites_df $delivery_day_abb) 


## Add extra catch of the day
extra_share<-all_shopify_orders %>% 
  filter(inventory_type == "Extra Main Share") %>% 
  mutate(product_name = str_extract(variant_name, "Small|Medium|Large|XL")) %>% 
#  select(order_id, order_date, order_time, customer_name, customer_email, product_name) %>% 
  mutate(order_tag = "extra share") %>% 
  clean_subscription() %>% 
  mutate(customer_email = paste0(customer_email, "(+1)"),
         customer_name = paste0(customer_name, "(+1)")) 

## Core dataset for Main Shares app
weekly_species11 <- rbind(subscription, extra_share) %>% 
  mutate(species = "unassigned") %>% 
  arrange(desc(order_id)) %>% 
  group_by(customer_email, share_size) %>% 
  filter(row_number() == 1) %>% 
  ungroup() 
  

# * Tab: Share Pounds by Site ----------------------------------------------------------------
fillet_weight_by_site <- weekly_species11 %>% 
  group_by(share_size, delivery_day, pickup_site_label) %>% 
  tally() %>% 
  arrange(pickup_site_label) %>%
  left_join(share_size_variant %>% filter(type_singular == "fillet"),
            by = "share_size") %>%
  mutate(total_fillet = n * share_size_estimate)%>%
  group_by(delivery_day, pickup_site_label) %>%
  summarise(sum = sum(total_fillet))



# * Flash Orders -----------------------------------------------------------------------------
 flashsales_Main_shares <- all_shopify_orders %>%

  ## get main share orders
  filter(inventory_type == "Main Share Upgrade") %>%
  
  mutate(share_size = str_extract(product_name, "Small|Medium|Large|XL")) %>%
  mutate(share_size = ifelse(share_size == "XL", "ExtraLarge", share_size)) %>% 
  
  #select most recent order if customers ordered multiple
  arrange(desc(order_id)) %>% 
  group_by(customer_email, share_size) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 

  ## remove "pick for me" options
  filter(variant_name != "Pick for me") %>% 

   ## e.g. Fresh Halibut Fillet -> Halibut
  mutate(share_upgrade = str_remove_all(variant_name, "Fresh | Fillet")) %>%
  select(customer_email, share_size, share_upgrade) %>%
  mutate(species_choice = share_upgrade) %>% 
  unique() 




 flash_list <- unique(flashsales_Main_shares$species_choice) %>%
   setdiff("") %>% c("None")

 flash_fillet <- setdiff(flash_list, c(species_options, "")) %>% tolower
 

 # * Button: OP Email List (Partner Email) ----------------------------------------------
 
 partner_email <- order_details %>% 
   select(customer_email, partner_email) %>% 
   unique() %>% 
   filter(!is.na(partner_email))

 # * Button: Download New Member Labels ----------------------------------------------
 
 cooler_bag_label <- subscription %>% 
   filter(str_detect(order_tags, "1st Order")) %>%
   unique() %>%
   mutate(welcome = "~ Welcome to Get Hooked ~") %>% 
   mutate(spacer_1= "~~~~~~~~",
          spacer_2= "~~~~~~~~") %>% 
   select(welcome, spacer_1, customer_name, spacer_2, pickup_site_label) %>% 
   filter(!str_detect(pickup_site_label, "Home Delivery")) %>%
   filter(!is.na(pickup_site_label)) %>% 
   arrange(pickup_site_label)
 
 
# 4.2 Checklists ==============================================================================

 label_list <- lapply(delivery_day_levels, generate_checklists) %>%
   `names<-`(delivery_day_levels_abb)


 # wrong_list <- lapply(label_list, function(x) {x$wrong_list}) %>%
 #  do.call(rbind, .) %>% `row.names<-`(NULL)

# 4.3 Home Deliveries =========================================================================

 all_home_delivery <- subscription %>% 
   filter(str_detect(pickup_site_label, "Home Delivery")) %>% 
#   select(email, location, delivery_day) %>% 
#   left_join(shopify_customers, by = "email") %>% 
   select(customer_name, address, city, state, zip, phone, notes = delivery_notes, customer_email, delivery_day)

  

## Main Share Labels

labels <- rbind_active_deliveries(type = "Labels") %>% 
  mutate(type = "subscription",
         check = " ") %>% 
  mutate(customer_name = as.character(customer_name)) %>% 
  mutate(home_delivery_name = as.character(home_delivery_name)) %>% 
  mutate(customer_name = ifelse(customer_name == "Home Delivery", home_delivery_name, customer_name)) %>% 
  mutate(customer_name = as.character(customer_name)) %>%
  mutate(pickup_site_label = str_trim(pickup_site_label, side = "both"))%>%
  mutate_all(as.character)


share <- labels %>% 
  group_by(delivery_day, customer_name, species, share_size) %>% 
  summarise(species = toString(unique(species)))


## Flashsales

flashsales <- rbind_active_deliveries(type = "Flashsales") %>%
  mutate(share_type = as.character(share_type),
         share_size = as.character(share_size),
         Timestamp = as.character(Timestamp)) %>% 
  ## New member labels are sometimes pasted into Flashsalse spreadsheets for printing purposes.
  ## The first column will be ~ Welcome to Get Hooked ~. Filter out those labels according to these keywords
  filter(!str_detect(Timestamp, "~") | !Timestamp == "welcome" | is.na(Timestamp)) %>%
  mutate(delivery_day = as.character(delivery_day)) %>%
  group_by(delivery_day, customer_name) %>%
  left_join(shopify_orders %>% select(share_type, size = variant_name, route_notation, product_type = checklist_notation) %>% unique(),
            by = "share_type") %>%
  ungroup() %>% 
  mutate(check = " ") %>%
  mutate(customer_name = as.character(customer_name)) %>%
  filter(share_type != "")


has_drygoods <- flashsales %>%
  group_by(delivery_day, customer_name, product_type) %>%
  filter(!is.na(route_notation)) %>%
  tally() %>%
  mutate(dry_list = paste(n, product_type, sep = " ")) %>%
  mutate(dry_list = ifelse(str_detect(dry_list, "Pick up"), "Pick up bags only", dry_list)) %>%
  group_by(delivery_day, customer_name) %>%
  summarise(all_dry = toString(unique(dry_list)))

# All Home Deliveries *******************************************************************************

deliveries_to_complete <- rbind(labels     %>% select(customer_name, pickup_site_label, check),
                                flashsales %>% select(customer_name, pickup_site_label, check)) %>% #day, name, pickupsite label
  filter(str_detect(pickup_site_label, "Home Delivery")) %>%
  left_join(subscription %>% select(customer_name, address, city, state, zip, phone, notes = delivery_notes, 
                                    customer_email, delivery_day)) %>% #pickup_day, restriction, production day
  left_join(share,             by = c("delivery_day", "customer_name")) %>% #species, sharesize
  left_join(has_drygoods,      by = c("delivery_day", "customer_name")) %>%
#  left_join(all_home_delivery, by = c("delivery_day", "name")) %>% #customer details
  mutate(delivery_date = today()) %>%
  mutate(all_dry  = replace_na(all_dry, "")) %>%
  distinct()



# * Button: Download Delivery Route -> home_delivery_module -----------------------------------------
deliveries_label <- deliveries_to_complete %>%
  select(delivery_day, share_size, species, customer_name, delivery_date, all_dry) %>%
  mutate(spacer_1 = "~", spacer_2 = "~", spacer_label = "~")
# 
# 
# # * Tab: Check Address ------------------------------------------------------------------------------
check_address <- deliveries_to_complete %>%
  filter(is.na(address) | address == "")
# 
# 
# # * Tab: Routesavvy File/Button: Download Routesavvy File ------------------------------------------
deliveries_routesavvy <- deliveries_to_complete %>%
  select(delivery_day, folder = delivery_date, customer_name, address,
         city, state, zip, note2 = phone, notes)



# 4.4 Early Deadline Orders & Special Orders ----------------------------------------------------------------

orders_ED_SO <- shopify_orders %>% 
  # filter out main share orders
  filter(deadline_type %in% c("Early Deadline", "Special Order")) %>% 
  unite("timestamp", order_date, order_time, sep = " ") %>% 

  #turning multiples to individal lines except oysters
  mutate(variant_name = ifelse(str_detect(tolower(product_name), "oyster"), quantity, variant_name),
         quantity = ifelse(str_detect(tolower(product_name), "oyster"), 1, quantity)) %>% 
  uncount(quantity) %>% 
  
  #data cleaning
  mutate(share_type = replace_na(product_name, "") %>% trimws) %>% 
  mutate(share_size = ifelse(variant_name == "", 1, variant_name) %>% trimws) %>% 
  mutate(source = "Store")



#data set used for ED/SO labels
all_ED_SO <- orders_ED_SO %>%
  mutate(spacer_1 = "~~~~~~~~",
         spacer_2 = "~~~~~~~~",
         date = get_delivery_date(delivery_day) %>% as.Date(format = "%m/%d/%y")) %>% 
  mutate(instructions_1 = ifelse(str_detect(share_type, "Frozen"), 
                                 "Important: Keep frozen until used.", 
                                 ifelse(str_detect(shiny_category, "FS"), 
                                        "Perishable. Keep refrigerated", " "))) %>%
  mutate(instructions_2 = ifelse(str_detect(share_type, "Frozen"), 
                                 "Thaw under refrigeration immediately before use", " ")) %>% 
  arrange(delivery_day, customer_name, share_type, share_size) %>% 
  select(shiny_category, timestamp, customer_email, customer_name, spacer_1, share_type, share_size, spacer_2, 
         pickup_site_label, date, instructions_1, instructions_2, source, delivery_day) 


# 4.4.1 Early Orders ----------------------------------------------------------------------------------------
title_ED <- title_with_date("Early Deadline Orders")


# * Tab: Early Order Tally/Button: Download Preorders -------------------------------------------------------
all_preorders_ED <- orders_ED_SO %>%
  filter(deadline_type == "Early Deadline") %>%
  group_by(shiny_category, delivery_day, vendor, share_type, size_unit) %>% 
  # mutate(share_size = ifelse(is.na(size), str_remove_all(share_size, "[^0-9]"), size)) %>% 
  # mutate(share_size = as.numeric(share_size)) %>%
  summarise(total = sum(size)) %>% 
  ungroup() %>%
  # mutate(multiplier = case_when(str_detect(share_type, "(two)") ~ 2,
  #                               str_detect(share_type, "(four)")  ~ 4,
  #                               TRUE ~ 1)) %>%
  # mutate(total = share_size_total * multiplier) %>% 
  mutate(share_type = str_remove(share_type, "(two)|(four)") %>% trimws) %>%
  mutate(vendor = ifelse(str_detect(shiny_category, "FS"), "-", vendor))%>%
  select(shiny_category, vendor, delivery_day, share_type, total, size_unit) %>% 
  arrange(vendor, delivery_day, share_type)


# 4.4.2 Special Orders --------------------------------------------------------------------------------------
title_SO <- title_with_date("Special Orders")


# * Tab: Fresh Product by Day -------------------------------------------------------------------------------
fresh_product_by_day <- orders_ED_SO %>% 
  filter(inventory_type == "Fresh Seafood") %>%
  mutate(delivery_day = factor(delivery_day, levels = delivery_day_levels)) %>%
#  mutate(weight_lb = ifelse(str_detect(product_name, "Oyster"), as.numeric(variant_name), weight_lb)) %>% 
  group_by(delivery_day, share_type, size_unit) %>% 
  summarise(total = sum(size)) %>% 
  mutate(total_number = ifelse(size_unit == "oz", total/16, total)) %>% 
  mutate(size_unit = ifelse(size_unit == "oz", "lb", size_unit)) %>% 
  select(delivery_day, share_type, total_number, size_unit)

# * Tab: Fresh Product Needed -------------------------------------------------------------------------------
fresh_product_all <- fresh_product_by_day %>% 
  group_by(share_type, size_unit) %>% 
  summarise(total_number = sum(total_number)) %>%
  select(share_type, total_number, size_unit) %>% 
  arrange(share_type)
  
# * Tab: TUE/WED/THU/LA orders ------------------------------------------------------------------------------
all_preorders_SO <- orders_ED_SO %>%
  filter(deadline_type == "Special Order") %>%
  count(delivery_day, share_type, share_size) %>%
  split(.$delivery_day)

# * Button: Download TUE/WED/THU/LA Labels ------------------------------------------------------------------
special_orders_all <- all_ED_SO %>%
  filter(str_detect(shiny_category, "SO")) %>%
  mutate(shiny_category = shiny_category_full(shiny_category)) %>%
  split(.$shiny_category) %>%
  lapply(function(df) {
    df %>% select(-shiny_category)})

# * Buttons: Download WED/THU/LA Labels with routes ---------------------------------------------------------

dry_goods <- rbind_active_deliveries(type = "Flashsales") %>%
  filter(!str_detect(Timestamp, "~") | !Timestamp == "welcome" | is.na(Timestamp)) %>%
  filter(share_type != "") %>%
  mutate(share_type = ifelse(!is.na(order) & str_detect(order, "Fillet"), 
                             str_remove(order, " - .*"), share_type)) %>%
  mutate(instructions_1 = "",
         instructions_2 = "") %>%
  mutate(customer_name = as.character(customer_name)) %>% 
  mutate(PSL = pickup_site_label) %>% 
  select(Timestamp, customer_email, customer_name, spacer_1, share_type, share_size, 
         spacer_2, pickup_site_label, date, instructions_1, instructions_2, source, 
         order, price, delivery_day, PSL)

route_labels <- dry_goods %>% 
  split(.$delivery_day)

# 5. Unlink temporary files ===============================================================
#unlink(Special_Order_temp)
#unlink(Home_Delivery_temp)


# 6. Import Modules =======================================================================

 source(file.path("modules", "main-shares-module.R"))
 source(file.path("modules", "species-assignment-module.R"))
 source(file.path("modules", "early-orders-module.R"))
 source(file.path("modules", "special-orders-module.R"))
 source(file.path("modules", "checklists-module.R"))
 source(file.path("modules", "home-delivery-module.R"))
# source(file.path("modules", "dashboard-module.R"))
 source(file.path("modules", "submodules.R"))

