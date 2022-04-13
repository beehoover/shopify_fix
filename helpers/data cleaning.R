# clean_date: Cleaning up and formatting phone numbers -----------------------------------------
clean_date <- function(df) {
  
  df %>% 
    separate(order_date, c("order_date","order_day"), sep = ',') %>% 
    mutate(order_date = as.Date(paste0(order_date, " ", format(Sys.Date(), "%Y")), format = "%B %d %Y")) %>%
    select(-order_day)
    
}

new_member_cutoff <- function(df) {
  
  df %>% 
    mutate(delivery_week = ifelse(str_detect(order_tags, "1st Order") & order_date > cutoff_date, "Next Week", ""))
}


# App(s): Main Shares, Species Assignment, Home Delivery
clean_colname <- function(df, type) {
  
  
  if(type == "Orders") {
    
    colNames <- c(
      "order_id", 
      "order_date", 
      "order_time",
      "customer_id",
      "customer_name",
      "customer_email",
      "product_name", 
      "variant_name",
      "vendor",
      "quantity",
      "price",
      "deadline_type",
      "inventory_type",
      "size",
      "size_unit",
      "packing_method",
      "checklist_notation",
      "sku",
      "order_tags",
      "prod_day",
      "product_tags"
    )
    
    position <- c(1:21)
    
  }
  else if(type == "Subscription") {
    
    colNames <- c(
      "order_id", 
      "order_date", 
      "order_time", 
      "customer_email",
      "subscription_size",
      "product_tag",
      "order_tag",
      "customer_name"
    )
    
    position <- c(1:8)
  }
  
  else {
    colNames <- c(
      "customer_id",
      "customer_email",
      "customer_name",
      "share_size",
      "pickup_location",
      "pickup_site",
      "opt_out",
      "delivery_day",
      "address1",
      "address2",
      "city",
      "state",
      "zip",
      "phone",
      "email_verified",
      "customer_tags",
      "partner_email", 
      "deliv_week_day",
      "order_count",
      "homedelivery_instructions",
      "customer_created_at",
      "customer_updated_at"
    )
    
    position <- c(1:22)
    
  }
  
  df %>%
    select(all_of(position)) %>% 
    `colnames<-`(colNames) 
  
}



clean_customer <- function(df) {
  
  df %>%
    
    #pickup site labels
    mutate(location_abb = ifelse(str_detect(pickup_location, "Los Angeles"), "LA", 
                                   ifelse(str_detect(pickup_location, "Santa Barbara"), "SB", 
                                          ifelse(str_detect(pickup_location, "Santa Ynez Valley"), "SYV", pickup_location)))) %>% 
      
    mutate(pickup_site = ifelse(str_detect(pickup_site, "Home Delivery"), paste0(pickup_site, " ",location_abb), 
                                  ifelse(str_detect(pickup_site, "Topa"), paste0("Topa Topa", " ",location_abb), 
                                         ifelse(str_detect(pickup_site, "Rincon"), paste(pickup_site, substr(pickup_location, 1, 4)), 
                                                                                         pickup_site)))) %>% 
      
    mutate(pickup_site_label = ifelse(str_detect(pickup_site, regex('santa monica', ignore_case = T)), word(pickup_site, 1,3),
                                        ifelse(str_detect(pickup_site, ":"), sub(":.*", "", pickup_site), pickup_site))) %>% 
    select(-location_abb) %>% 
    
    #delivery day
    mutate(delivery_day = factor(delivery_day, levels = delivery_day_levels)) %>%
    mutate(delivery_day_abb = toupper(substr(delivery_day, 1, 3))) %>% 
    
    #opt-out
    mutate(opt_out = gsub("No ", "", opt_out)) %>%
    mutate(opt_out = gsub("\\|", ", ", opt_out)) %>% 
    
    #definte share type
    mutate(share_type = "Fillet") %>%
    
    #clean data
    mutate_at(vars(zip), ~as.numeric(.)) %>%
    mutate_if(is.character, str_trim) %>% 
    lapply(gsub, pattern = "&#039;", replacement = "'", fixed = TRUE) %>%
    lapply(gsub, pattern = "&quot;", replacement = "", fixed = TRUE) %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    mutate(opt_out = ifelse(is.na(opt_out), "None",
                            ifelse(str_detect(opt_out, "try it all"), "None", opt_out))) %>% 
    mutate(address = paste0(address1, ", ", address2))
  
}

# clean_subscription: data cleaning for shopify_orders -------------------------------------
# App(s): Main Shares, Species Assignment
  
clean_subscription <- function(df) {
  
  df %>%
    
    #share_size
    mutate(share_size = str_extract(product_name,"(\\w+)")) %>% 
    mutate(share_size = ifelse(share_size == "XL", "ExtraLarge", share_size)) %>%
    mutate(delivery_notes = homedelivery_instructions) %>% 
    filter(str_detect(order_id, "#")) %>% 
    
    #select necessary columns
    select(
      order_id,
      order_date,
      order_time,
      customer_email,
      share_size,
      order_tags,
      customer_name,
      pickup_location,
      pickup_site,
      pickup_site_label,
      opt_out,
      delivery_day,
      address,
      city,
      state,
      zip,
      phone,
      delivery_notes,
      customer_tags,
      partner_email
    )
    
}

# Clean up store orders and retrieve unmatched emails ---------------------------------------------
# App(s): Main Shares, Species Assignment, Early Deadline Orders
clean_weight_unit <- function(df) {
  
  df %>%
    mutate(weight_unit = ifelse(str_detect(tolower(product_name), "oyster"), "pcs", 
                                ifelse(str_detect(tolower(variant_name), "pint"), "pint", 
                                       ifelse(is.na(weight_lb), "-", weight_unit)))) %>% 
    mutate(weight_lb = ifelse(str_detect(tolower(product_name), "oyster"), quantity, weight_lb)) %>% 
    mutate(weight_lb = as.numeric(weight_lb))

}


# shiny_category_full: Get full forms of shiny_category from abbreviations -------------------------------------
# App(s): Special Orders & Early Deadline Orders

shiny_category_full <- function(x) {
  case_when(
    str_detect(x, "VO") ~ "Vendor Orders",
    str_detect(x, "FS") ~ "Fresh Seafood",
    str_detect(x, "IF") ~ "Frozen Products", 
    str_detect(x, "ID") ~ "Dry Products",
    TRUE                ~ NA_character_)
}


# get_delivery_date: Get dates according to delivery day ------------------------------------
# global.R, species-assignment-module.R
# wday_date function is in www/data/contants.R
get_delivery_date <- function(x) {
  case_when(
    x == "Monday"              ~ wday_date(1),
    x == "Tuesday"             ~ wday_date(2),
    x == "Wednesday"           ~ wday_date(3),
    x %in% c("Thursday", "LA") ~ wday_date(4),
    x == "Friday"              ~ wday_date(5))
}


