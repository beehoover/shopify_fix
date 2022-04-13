# Global: datatable options ------------------------------------------------------------------------------------
datatable_options <- list(scrollX = TRUE, scrollY = "400px", 
                          scrollCollapse = TRUE, paging = FALSE)

# Global: TUE/WED/THU Dates ----------------------------------------------------------------------------------------
# wday_date <- function(w) {
#   sunday_date <- floor_date(today() - 1, "weeks")
#   sunday_date + w
# }


wday_date <- function(w) {
  
  currentDay <- weekdays(Sys.Date())
  
  if (currentDay %in% c("Monday", "Tuesday", "Wednesday", "Thursday")){
    sunday_date <- floor_date(today(), "weeks")
  }
  
  else {
    sunday_date <- floor_date(today() + 2, "weeks")
  }
  
  sunday_date + w

}


# # dates of Tuesday/Wednesday/Thursday
# nextTUE <- wday_date(2)
# nextWED <- wday_date(3)
# nextTHU <- wday_date(4)

# Cutoff date for new members in Species Assignment (Monday before last)
cutoff <- wday_date(3) - 17
cutoff_date <- wday_date(0)

# Start of this production week
week_start <- wday_date(5)

# delivery day
delivery_day_levels <- c("Tuesday", "Wednesday", "Thursday", "LA")

delivery_day_levels_abb <- c("TUE", "WED", "THU", "LA") # abbreviations
delivery_day_levels_abb_named <- delivery_day_levels_abb %>% 
  `names<-`(paste(delivery_day_levels_abb, "Sites"))

#shiny_category with ED/SO, VO/FS/IF/ID
shiny_category_df <- data.frame(deadline_type = c("Early Deadline" , "Early Deadline" ,"Special Order", "Special Order", "Special Order"),
                                inventory_type = c("Vendor Order", "Fresh Seafood", "Fresh Seafood", "Inventory Frozen", "Inventory Dry"),
                                shiny_category = c("ED/VO", "ED/FS","SO/FS", "SO/ID", "SO/IF"))

ED_category <- c("FS", "VO")

# Global: Google Sheet ss ---------------------------------------------------------------------------------------

ss <- c("shopify"           = "SeeQbMhljx46LSRodW91lORPYFa9y4CIAS2rvRf4hk79JMebqRlP7DaNZCGeOj2MhNhMkVxREjhal5",
        "special_order"     = "REhI8zlPZNyaaz7n3KjXQEjaLAv_6nthU-k9lTmhvQMPOAaOBT8OwTeK9vjLMDb_uPxZFzxc_lBEgG",
        "delivery_days"     = "Qg-zyRyXFRkFW4qDenJRYlrMI3BX-MJb4QN_PeNwD5N25aPewdxk1A26abgCyWDTFL4WYJ4Ut-H_HF",
        "home_delivery"     = "SKrkfrQ0dr-okv3d41pe5wEriiO2E-Q3k9fc62nA5t7r7UIUsXWqjveCGPdIqDDoZkZ10xvIaOCsi_",
        "active_deliveries" = "Q0z9MkHFbM9eIJyNVmpT3lUNyQj9PyubFRm_MpLYnT_re3LsMvB85y7jMk9mziP6_PnS4zlluArYX1",
        "standing_orders"   = "RczO6pIdIZruJs4eXmOm7uzRp7yLHPoQiZKMHu7y1nUdYzqH1fb2BmjolRoLviTgzEXGfAlm6-h97G",
        "referral_orders"   = "TdZtW7kRNnNxeb5dZ_G67l5I1u6hosr9nQCfurEMgy-GN2VXHYNuNaSHplhp85V-5qzV8w-4_43Fid",
        "bag_deposit_paid"  = "RS5ZuZa-o0uhoQULnaA_s2PcloMhjAgFzEqPTZttFOdsQdFVFmw1s-Ws_KtBOPi-uw15Fx3F-RG1Yd",
        "product_keys"      = "TIWyHE9aYmuzro5uHydwNf6dXVVJp5Um-FKBEYFCRViGgN1WfpSI5eAEsHD_Yz23dettw9v6j9ydr8",
        "reference_keys"    = "S2MdyVlgrpNd1TUuVYmuLyz3zXTwLyiLXTXvKyy8cgm-n3tmR-rsF7I97EmR05EBQ8Tf7_Yj42Re-0")


# Checkist ------------------------------------------------------------------------------------------------------
checklist_message <- "<center><p style='font-size: 1.1em; font-family:system-ui'>Get Hooked Deliveries</p></center>
                 <ul style='font-size: 1em; font-family:system-ui'> 
                 <li>Hand sanitizer is provided, please use before opening coolers</li>
                 <li>Dry goods are in zip-cloth bag next to coolers.</li>
                 <li>Check your name off the list.</li>
                 <li>Keep all seafood covered in ice & close cooler lid fully.</li>
                 </ul>
                 <i style='margin: 1em 1.5em; font-size: 1em; font-weight: bold; font-family:system-ui'>New members take a tote bag with your name on it.</i>
                 "
printJS <- JS("function ( win ) {
              $(win.document.body).find( 'table' )
                .addClass( 'compact' )
                .css( 'font-size', 'inherit' )
                .css( 'margin-top', '1.5em' );
                                 
              $(win.document.body).find( 'h1' )
                .css( 'font-size', '2em' )
                .css( 'font-family', 'system-ui' )
                .wrap( '<center></center>');
                
              $(win.document.body).find( 'table tr td:first-child:empty, table tr td:nth-child(2):empty' )
                .css( 'border-top', '0' );
              }")
