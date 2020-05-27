#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Top page----
  df.all.clean <- reactive({
  
    
  ### LIC:adjustment costs by changing tradable ratio --------------------------------
    
    df <- lic
    df <- df %>% 
      filter(`Original Unit Cost` != "NA")
    
    #Adjusted unit cost calculation
    gni_selected_country <- as.numeric(GNI[GNI$Country == "LIC", c("X2015")])
    final_UC <- data.frame( Name = character(),
                            unit_cost = numeric(),
                            stringsAsFactors=FALSE)
    
    tradable_ratio <- input$tradable2
    UC_Years <- 2016  # The output year
    ancillary_HF_cost <- 0.5
    above_HF <- 0.17
    
    no <- nrow(df)
    adjustment <- data.frame(ID = 1:no, exchange = NA, country = NA, year = NA, n = NA, 
                             cpi_study = NA, cpi_end = NA, cpi_adjust = NA,
                             global_inflation = NA,
                             exchange_end = NA, gni = NA)
    
    for (i in 1:no){
      #Country
      country <- as.character(df[i,"Country"])
      adjustment[i,"country"] <- country
      
      #Original UC_Year
      year <- df[i,"Year"]
      adjustment[i,"year"] <- year
      
      #Output year
      n <- as.numeric(year) - 1960 +2
      end_year <- as.numeric(UC_Years - 1960 +2)
      
      # Exchange of original UC
      adjustment[i,"n"] <- n
      rate <- exchange[exchange$Country == country, n]
      adjustment[i,"exchange"] <- as.numeric(rate)
      
      # consumer price index
      cpi_year <- as.numeric(year) - 1960 + 6
      cpi_end_year <- as.numeric(UC_Years - 1960 +6)
      
      adjustment[i,"cpi_study"] <- as.numeric(cpi[cpi$Country == country, cpi_year])
      adjustment[i,"cpi_end"] <- as.numeric(cpi[cpi$Country == country, cpi_end_year])
      
      # Global inflation calculation
      col_n <- as.numeric(year) - 1960 + 5
      col_end <- as.numeric(UC_Years - 1960 +5)
      
      #Global inflation of 2016, 2017 or 2018
      adjustment[i,"global_inflation"] <-
        as.numeric(prod(as.matrix(regional_inflation[regional_inflation$Country == "World", col_n:col_end])))
      
      # Exchange rate of 2016, 2017 or 2018
      adjustment[i,"exchange_end"] <- as.numeric(exchange[exchange$Country == country, end_year])
      
      #GNI
      GNI_year <- UC_Years - 1990 +5
      adjustment[i,"gni"] <- as.numeric(GNI[GNI$Country == country, c("X2015")])
    }
    
    # GNI adjustment
    # GNI selected country = LIC or LMIC
    adjustment$gni_selected_country <- gni_selected_country
    adjustment$gni_adjust <- adjustment$gni_selected_country/ adjustment$gni
    
    # CPI adjustment rate
    adjustment[,"cpi_adjust"] <- adjustment$cpi_end / adjustment$cpi_study
    
    # Merge adjustments with uc
    ucm <- cbind(df, adjustment)
    
    # Tradable
    ucm$tradable_uc <- NA
    for (i in 1:no){
      ucm$tradable_uc[i] <- tradable_conversion(
        unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
        tradable_ratio = tradable_ratio,
        currency = ucm$`Original Currency`[i],
        global_inflation = ucm$global_inflation[i],
        exchange = ucm$exchange[i])
    }
    
    
    for (i in 1:no){
      ucm$nontradable_uc[i] <- nontradable_conversion(
        unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
        tradable_ratio = tradable_ratio,
        currency = ucm$`Original Currency`[i],
        cpi_adjust = ucm$cpi_adjust[i],
        country = ucm$country[i],
        exchange = ucm$exchange[i],
        exchange_end = ucm$exchange_end[i],
        gni = ucm$gni[i],
        gni_selected_country = ucm$gni_selected_country[i])
    }
    
    # Add ancillary + above HF costs
    ucm$adjusted_uc <- (ucm$tradable_uc + ucm$nontradable_uc)
    
    ###
    #Change all tradable and non-tradable
    
    
    
    
    for (i in 1:nrow(ucm)){
      if(ucm$Code[i] %in% all_tradable$Code & !(ucm$Code[i] %in% all_nontradable$Code)) {
        tradable_ratio <- 1.0
        ucm$adjusted_uc[i] <- tradable_conversion(
          unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
          tradable_ratio = tradable_ratio,
          currency = ucm$`Original Currency`[i],
          global_inflation = ucm$global_inflation[i],
          exchange = ucm$exchange[i])
        ucm$nontradable_uc[i] <- 0
        ucm$tradable_uc[i] <- tradable_conversion(
          unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
          tradable_ratio = tradable_ratio,
          currency = ucm$`Original Currency`[i],
          global_inflation = ucm$global_inflation[i],
          exchange = ucm$exchange[i])
        
      } else if (!(ucm$Code[i] %in% all_tradable$Code) & ucm$Code[i] %in% all_nontradable$Code) {
        tradable_ratio <- 0
        ucm$nontradable_uc[i] <- nontradable_conversion(
          unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
          tradable_ratio = tradable_ratio,
          currency = ucm$`Original Currency`[i],
          cpi_adjust = ucm$cpi_adjust[i],
          country = ucm$country[i],
          exchange = ucm$exchange[i],
          exchange_end = ucm$exchange_end[i],
          gni = ucm$gni[i],
          gni_selected_country = ucm$gni_selected_country[i])
        ucm$tradable_uc[i] <- 0
        ucm$adjusted_uc[i] <- nontradable_conversion(
          unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
          tradable_ratio = tradable_ratio,
          currency = ucm$`Original Currency`[i],
          cpi_adjust = ucm$cpi_adjust[i],
          country = ucm$country[i],
          exchange = ucm$exchange[i],
          exchange_end = ucm$exchange_end[i],
          gni = ucm$gni[i],
          gni_selected_country = ucm$gni_selected_country[i])
      }  else{
        ucm$adjusted_uc[i] <- ucm$adjusted_uc[i]
      }
    }
    
    
    lic2 <- merge(lic, ucm[,c("Code", "adjusted_uc")], by = "Code")
    
    lic.selected <- lic2[,c("Code", "Intervention", "Original Unit Cost", "adjusted_uc","PIN","Coverage")] 
    lic.selected <- lic.selected  %>% 
      rename("LIC.original.UC" = "Original Unit Cost",
             "LIC.adjusted.UC" = "adjusted_uc",
             "LIC.PIN" = "PIN",
             "LIC.Coverage" = "Coverage")
    
    
    
    ### LMIC:adjustment costs by changing tradable ratio --------------------------------
    
    df <- lmic
    df <- df %>% 
      filter(`Original Unit Cost` != "NA")
    
    gni_selected_country <- as.numeric(GNI[GNI$Country == "LMIC", c("X2015")])


    final_UC <- data.frame(Name = character(),
                            unit_cost = numeric(),
                            stringsAsFactors=FALSE)
    
    
    #Setting
    tradable_ratio2 <- input$tradable2
    UC_Years <- 2016  # The output year
    ancillary_HF_cost <- 0.5
    above_HF <- 0.17
    #setting end 
    
    
    
    #Adjusted UC
    # Adjustment database
    no <- nrow(df)
    adjustment <- data.frame(ID = 1:no, exchange = NA, country = NA, year = NA, n = NA, 
                             cpi_study = NA, cpi_end = NA, cpi_adjust = NA,
                             global_inflation = NA,
                             exchange_end = NA, gni = NA)
    
    
    for (i in 1:no){
      #Country
      country <- as.character(df[i,"Country"])
      adjustment[i,"country"] <- country
      
      #Original UC_Year
      year <- df[i,"Year"]
      adjustment[i,"year"] <- year
      
      #Output year
      n <- as.numeric(year) - 1960 +2
      end_year <- as.numeric(UC_Years - 1960 +2)
      
      # Exchange of original UC
      adjustment[i,"n"] <- n
      rate <- exchange[exchange$Country == country, n]
      adjustment[i,"exchange"] <- as.numeric(rate)
      
      # consumer price index
      cpi_year <- as.numeric(year) - 1960 + 6
      cpi_end_year <- as.numeric(UC_Years - 1960 +6)
      
      adjustment[i,"cpi_study"] <- as.numeric(cpi[cpi$Country == country, cpi_year])
      adjustment[i,"cpi_end"] <- as.numeric(cpi[cpi$Country == country, cpi_end_year])
      
      # Global inflation calculation
      col_n <- as.numeric(year) - 1960 + 5
      col_end <- as.numeric(UC_Years - 1960 +5)
      
      #Global inflation of 2016, 2017 or 2018
      adjustment[i,"global_inflation"] <-
        as.numeric(prod(as.matrix(regional_inflation[regional_inflation$Country == "World", col_n:col_end])))
      
      # Exchange rate of 2016, 2017 or 2018
      adjustment[i,"exchange_end"] <- as.numeric(exchange[exchange$Country == country, end_year])
      
      #GNI
      GNI_year <- UC_Years - 1990 +5
      adjustment[i,"gni"] <- as.numeric(GNI[GNI$Country == country, c("X2015")])
    }
    
    # GNI adjustment
    # GNI selected country = LIC or LMIC
    adjustment$gni_selected_country <- gni_selected_country
    adjustment$gni_adjust <- adjustment$gni_selected_country/ adjustment$gni
    
    # CPI adjustment rate
    adjustment[,"cpi_adjust"] <- adjustment$cpi_end / adjustment$cpi_study
    
    # Merge adjustments with uc
    ucm <- cbind(df, adjustment)
    
    # Tradable
    ucm$tradable_uc <- NA
    for (i in 1:no){
      ucm$tradable_uc[i] <- tradable_conversion(
        unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
        tradable_ratio = tradable_ratio2,
        currency = ucm$`Original Currency`[i],
        global_inflation = ucm$global_inflation[i],
        exchange = ucm$exchange[i])
    }
    
    
    for (i in 1:no){
      ucm$nontradable_uc[i] <- nontradable_conversion(
        unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
        tradable_ratio = tradable_ratio2,
        currency = ucm$`Original Currency`[i],
        cpi_adjust = ucm$cpi_adjust[i],
        exchange = ucm$exchange[i],
        country = ucm$country[i],
        exchange_end = ucm$exchange_end[i],
        gni = ucm$gni[i],
        gni_selected_country = ucm$gni_selected_country[i])
    }
    
    # Add ancillary + above HF costs
    ucm$adjusted_uc <- (ucm$tradable_uc + ucm$nontradable_uc)
    
    # Change tradable ratio
    
    for (i in 1:nrow(ucm)){
      if(ucm$Code[i] %in% all_tradable$Code & !(ucm$Code[i] %in% all_nontradable$Code)) {
        tradable_ratio2 <- 1.0
        ucm$adjusted_uc[i] <- tradable_conversion(
          unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
          tradable_ratio = tradable_ratio2,
          currency = ucm$`Original Currency`[i],
          global_inflation = ucm$global_inflation[i],
          exchange = ucm$exchange[i])
        ucm$nontradable_uc[i] <- 0
        ucm$tradable_uc[i] <- tradable_conversion(
          unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
          tradable_ratio = tradable_ratio2,
          currency = ucm$`Original Currency`[i],
          global_inflation = ucm$global_inflation[i],
          exchange = ucm$exchange[i])
        
      } else if (!(ucm$Code[i] %in% all_tradable$Code) & ucm$Code[i] %in% all_nontradable$Code) {
        tradable_ratio2 <- 0
        ucm$nontradable_uc[i] <- nontradable_conversion(
          unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
          tradable_ratio = tradable_ratio2,
          currency = ucm$`Original Currency`[i],
          cpi_adjust = ucm$cpi_adjust[i],
          country = ucm$country[i],
          exchange = ucm$exchange[i],
          exchange_end = ucm$exchange_end[i],
          gni = ucm$gni[i],
          gni_selected_country = ucm$gni_selected_country[i])
        ucm$tradable_uc[i] <- 0
        ucm$adjusted_uc[i] <- nontradable_conversion(
          unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
          tradable_ratio = tradable_ratio2,
          currency = ucm$`Original Currency`[i],
          cpi_adjust = ucm$cpi_adjust[i],
          country = ucm$country[i],
          exchange = ucm$exchange[i],
          exchange_end = ucm$exchange_end[i],
          gni = ucm$gni[i],
          gni_selected_country = ucm$gni_selected_country[i])
      }  else{
        ucm$adjusted_uc[i] <- ucm$adjusted_uc[i]
      }
    }
    
    
    lmic2 <- merge(lmic, ucm[,c("Code", "adjusted_uc")], by = "Code")
    
    lmic.selected <- lmic2[,c("Code", "Original Unit Cost", "adjusted_uc", "PIN","Coverage")] 
    lmic.selected <- lmic.selected  %>% 
      rename("LMIC.original.UC" = "Original Unit Cost",
             "LMIC.adjusted.UC" = "adjusted_uc",
             "LMIC.PIN" = "PIN",
             "LMIC.Coverage" = "Coverage")
    
    
    # Merge LIC and LMIC -----
    df.all.clean <- merge(lic.selected, lmic.selected, by = "Code")
    
    cols.num <- c("LIC.original.UC","LMIC.original.UC", "LIC.adjusted.UC", "LMIC.adjusted.UC","LIC.PIN","LIC.Coverage", "LMIC.PIN","LMIC.Coverage")
    df.all.clean[cols.num] <- sapply(df.all.clean[cols.num],as.numeric)
    
    #Formatting
    df.all.clean <- df.all.clean %>% 
      mutate(LIC.PIN.million = signif(LIC.PIN/1000000,2),
             LMIC.PIN.million = signif(LMIC.PIN/1000000,2),
             LIC.original.UC2 = signif(LIC.original.UC,2),
             LIC.adjusted.UC2 = signif(LIC.adjusted.UC, 2),
             LIC.Coverage2 = signif(LIC.Coverage*100,2),
             LMIC.original.UC2 = signif(LMIC.original.UC,2),
             LMIC.adjusted.UC2 = signif(LMIC.adjusted.UC,2),
             LMIC.Coverage2 = signif(LMIC.Coverage*100,2)
             ) %>% 
      select(Code, Intervention, LIC.original.UC2, LIC.adjusted.UC2 ,LIC.PIN.million, LIC.Coverage2,
             LMIC.original.UC2, LMIC.adjusted.UC2,LMIC.PIN.million, LMIC.Coverage2) %>% 
      rename("Original unit cost in LIC (original currency)" = LIC.original.UC2, 
             "Adjusted unit cost in LIC (USD 2016)" = LIC.adjusted.UC2,
             "Population in need in LIC (million)"= LIC.PIN.million,
             "Baseline coverage in LIC (%)" = LIC.Coverage2, 
             
             "Original unit cost in LMIC (original currency)" = LMIC.original.UC2, 
             "Adjusted unit cost in LMIC (USD 2016)" = LMIC.adjusted.UC2,
             "Population in need in LMIC (million)"= LMIC.PIN.million,
             "Baseline coverage in LMIC (%)" = LMIC.Coverage2)
    
    df.all.clean
    
  })
  
  
  df.all.clean.merge_final <- reactive({
    
    more.info.select <- more.info %>% 
      select("Code","Unit cost description", "Cost calculated", "Currency", "UC source(s)", "PIN description", "PIN source(s)", "Coverage description", "Coverage source(s)")
    
    df.all.clean.merge_re <- merge(df.all.clean(), more.info.select, by = "Code")

  })
  
  output$raw.table <- DT::renderDataTable(rownames=FALSE, filter="top",
                                          datatable2(
                                            x = df.all.clean.merge_final(), 
                                            vars = c("Unit cost description", "Cost calculated", "Currency", "UC source(s)", "PIN description", "PIN source(s)", "Coverage description", "Coverage source(s)"),
                                            opts = list(pageLength = 250)
                                          ))
  
  
  output$downloadinputdata <- downloadHandler(
    filename = function() {
      paste("DCP3_","Costing_input_data", ".csv", sep="")
    },
    content = function(file) {
      write.csv(df.all.clean.merge_final(), file)
    }
  )

  
  # Editable table:LIC ----------
  #Maybe intervention name will be inserted as comments.
  
  output$hot <- renderRHandsontable({
    rhandsontable(df.lic3[,-1],  rowHeaders = NULL, width = '100%', height = 550,  stretchH = "all", 
                  colHeaders = c("Code", "Intervention","HPP", "Custom package", "Platform", "Urgency", "Objective",
                                 "Population in need", "Baseline coverage", "Adjusted unit cost (USD 2016)","Target coverage")) %>% 
      #Lock the col
      hot_col("Code", readOnly = TRUE, colWidths = 60) %>% 
      hot_col("Intervention", readOnly = TRUE, colWidths = 400) %>% 
      hot_col("HPP", readOnly = TRUE, colWidths = 50) %>% 
      hot_col("Custom package", colWidths = 60) %>%
      hot_col("Platform", readOnly = TRUE, colWidths = 100) %>% 
      hot_col("Urgency", readOnly = TRUE, colWidths = 80) %>%
      hot_col("Objective", readOnly = TRUE, colWidths = 80) %>% 
      hot_col("Population in need", format = "0,000,000", colWidths = 100) %>% 
      hot_col("Baseline coverage", colWidths = 80) %>% 
      hot_col("Adjusted unit cost (USD 2016)", colWidths = 83) %>%
      hot_col("Target coverage", colWidths = 80) %>%
      #Sorting
      hot_cols(columnSorting = TRUE)  

  })
  
  data.lic <- reactive({
    if (is.null(input$hot)) { 
      hot <- df.lic3[,-1]
    } else {
      hot <- input$hot
      hot_to_r(hot)
    }
  })

  
  
  # Editable table:LMIC ----------
  #Maybe intervention name will be inserted as comments.
  output$hot.lmic <- renderRHandsontable({
    rhandsontable(df.lmic[,-1], rowHeaders = NULL, width = '100%', height = 550,  stretchH = "all", 
                  colHeaders = c("Code", "Intervention","HPP", "Custom package", "Platform", "Urgency", "Objective",
                                 "Population in need", "Baseline coverage", "Adjusted unit cost (USD 2016)","Target coverage")) %>% 
      #Lock the col
      hot_col("Code", readOnly = TRUE, colWidths = 60) %>% 
      hot_col("Intervention", readOnly = TRUE, colWidths = 400) %>% 
      hot_col("HPP", readOnly = TRUE, colWidths = 50) %>% 
      hot_col("Custom package", colWidths = 60) %>%
      hot_col("Platform", readOnly = TRUE, colWidths = 100) %>% 
      hot_col("Urgency", readOnly = TRUE, colWidths = 80) %>%
      hot_col("Objective", readOnly = TRUE, colWidths = 80) %>% 
      hot_col("Population in need", format = "0,000,000", colWidths = 100) %>% 
      hot_col("Baseline coverage", colWidths = 80) %>% 
      hot_col("Adjusted unit cost (USD 2016)", colWidths = 83) %>%
      hot_col("Target coverage", colWidths = 80) %>%
      
      #Sorting
      hot_cols(columnSorting = TRUE)
  })
  
  data.lmic <- reactive({
    if (is.null(input$hot.lmic)) { 
      hot.lmic <- df.lmic[,-1]
    } else {
      hot.lmic <- input$hot.lmic
      hot_to_r(hot.lmic)
    }
    
  })
  
  # Download LIC edicatable table ----
  output$downloadeditData_LIC <- downloadHandler(
    filename = function() {
      paste("DCP3_","Costing_data_edit_table_LIC", ".csv", sep="")
    },
    content = function(file) {
      write.csv(result.user.lic.edit.table.download(), file)
    }
  )
  
  # Download LMIC edicatable table ----
  output$downloadeditData_LMIC <- downloadHandler(
    filename = function() {
      paste("DCP3_","Costing_data_edit_table_LMIC", ".csv", sep="")
    },
    content = function(file) {
      write.csv(result.user.lmic.edit.table.download(), file)
    }
  )
  
  # Summary calculation based on the editable tables -----
  # LIC
  result.user.lic.edit.table <- reactive({
    
    lic.full2 <- data.lic()
    lic.full2$Coverage <- as.numeric(lic.full2$Coverage)
    lic.full2$adjusted_uc <- as.numeric(lic.full2$adjusted_uc)
    lic.full2$PIN <- as.numeric(lic.full2$PIN)
     
       
    lic.full2$total_cost <- ifelse(lic.full2$Coverage >= 0.8,
                                   lic.full2$adjusted_uc*(1+0.5)*(1+0.17)*(lic.full2$Coverage)*lic.full2$PIN,
                                   lic.full2$adjusted_uc*(1+0.5)*(1+0.17)*(0.8)*lic.full2$PIN)

    lic.full2$current_cost <- lic.full2$adjusted_uc*(1+0.5)*(1+0.17)*(lic.full2$Coverage)*lic.full2$PIN
    
    lic.full2$increment_cost <- lic.full2$total_cost - lic.full2$current_cost
     
     lic.full2
  })
  
  result.user.lic.edit.table.download <- reactive({
    result.user.lic.edit.table2 <- result.user.lic.edit.table() %>% 
      select(-Intervention)
    
    lic_multi <- lmic[,c("Code", "Intervention", "Multipliers")]
    lic.full2 <- merge(lic_multi, result.user.lic.edit.table2,  by = "Code", all.x = T)
    
  })
  
  
  #LMIC
  result.user.lmic.edit.table <- reactive({
    
    lmic.full2 <- data.lmic()
    lmic.full2$Coverage <- as.numeric(lmic.full2$Coverage)
    lmic.full2$adjusted_uc <- as.numeric(lmic.full2$adjusted_uc)
    lmic.full2$PIN <- as.numeric(lmic.full2$PIN)
    
    
    lmic.full2$total_cost <- ifelse(lmic.full2$Coverage >= 0.8,
                                   lmic.full2$adjusted_uc*(1+0.5)*(1+0.17)*(lmic.full2$Coverage)*lmic.full2$PIN,
                                   lmic.full2$adjusted_uc*(1+0.5)*(1+0.17)*(0.8)*lmic.full2$PIN)
    
    lmic.full2$current_cost <- lmic.full2$adjusted_uc*(1+0.5)*(1+0.17)*(lmic.full2$Coverage)*lmic.full2$PIN
    
    lmic.full2$increment_cost <- lmic.full2$total_cost - lmic.full2$current_cost
    
    lmic.full2
    
  })
  
  result.user.lmic.edit.table.download <- reactive({
    result.user.lmic.edit.table2 <- result.user.lmic.edit.table() %>% 
      select(-Intervention)
    
    lmic_multi <- lmic[,c("Code", "Intervention","Multipliers")]
    lmic.full2 <- merge(lmic_multi, result.user.lmic.edit.table2,  by = "Code", all.x = T)
  })
  
  
  
  # Final table based on the editable tables in LIC and LMIC------
  final.table <- reactive({
    
    # IIC----result.user.lic
    
    df.sum.lic <- result.user.lic.edit.table() %>%
      summarise(LIC_increment_sum = round(sum(increment_cost, na.rm = T)/1000000000, 4),
                LIC_increment_per = round(sum(increment_cost/LIC_total_pop, na.rm = T), 2),
                LIC_Total = round(sum(total_cost, na.rm = T)/1000000000, 2 ),
                LIC_Total_per = round(sum(total_cost, na.rm = T)/LIC_total_pop, 2 ),
                LIC_GNI_increment = round(((sum(increment_cost, na.rm = T)/LIC_total_pop) / LIC_GNI_2015)*100,2),
                LIC_GNI_total = round(((sum(total_cost, na.rm = T)/LIC_total_pop) / LIC_GNI_2015)*100,2)
      )
    
    
    df.sum.lic <- data.frame(t(df.sum.lic))
    
    df.sum.lic.HPP <- result.user.lic.edit.table() %>%
      filter(Include == TRUE) %>%
      summarise(LIC_increment_sum = round(sum(increment_cost, na.rm = T)/1000000000, 2),
                LIC_increment_per = round(sum(increment_cost/LIC_total_pop, na.rm = T), 2),
                LIC_Total = round(sum(total_cost, na.rm = T)/1000000000, 2 ),
                LIC_Total_per = round(sum(total_cost, na.rm = T)/LIC_total_pop, 2 ),
                LIC_GNI_increment = round(((sum(increment_cost, na.rm = T)/LIC_total_pop) / LIC_GNI_2015)*100,2),
                LIC_GNI_total = round(((sum(total_cost, na.rm = T)/LIC_total_pop) / LIC_GNI_2015)*100,2)
      )
    
    #df.sum.lic.HPP <- df.sum.lic.HPP[2,-1]
    df.sum.lic.HPP2 <- data.frame(t(df.sum.lic.HPP)) 
    
    #result.user.lmic
    
    # IMIC
    df.sum.lmic <- result.user.lmic.edit.table() %>%
      summarise(LMIC_increment_sum = round(sum(increment_cost, na.rm = T)/1000000000, 2),
                LMIC_increment_per = round(sum(increment_cost/LMIC_total_pop, na.rm = T), 2),
                LMIC_Total = round(sum(total_cost, na.rm = T)/1000000000, 2 ),
                LMIC_Total_per = round(sum(total_cost, na.rm = T)/LMIC_total_pop, 2 ),
                LMIC_GNI_increment = round(((sum(increment_cost, na.rm = T)/LMIC_total_pop) / LMIC_GNI_2015)*100,2),
                LMIC_GNI_total = round(((sum(total_cost, na.rm = T)/LMIC_total_pop) / LMIC_GNI_2015)*100,2)
      )
    
    df.sum.lmic <- data.frame(t(df.sum.lmic))
    
    df.sum.lmic.HPP <- result.user.lmic.edit.table() %>%
      filter(Include == TRUE) %>%
      summarise(LMIC_increment_sum = round(sum(increment_cost, na.rm = T)/1000000000, 2),
                LMIC_increment_per = round(sum(increment_cost, na.rm = T)/LMIC_total_pop, 2),
                LMIC_Total = round(sum(total_cost, na.rm = T)/1000000000, 2 ),
                LMIC_Total_per = round(sum(total_cost, na.rm = T)/LMIC_total_pop, 2 ),
                LMIC_GNI_increment = round(((sum(increment_cost, na.rm = T)/LMIC_total_pop) / LMIC_GNI_2015)*100,2),
                LMIC_GNI_total = round(((sum(total_cost, na.rm = T)/LMIC_total_pop) / LMIC_GNI_2015)*100,2)
      )
    
    #df.sum.lmic.HPP <- df.sum.lmic.HPP[2,-1]
    df.sum.lmic.HPP2 <- data.frame(t(df.sum.lmic.HPP))
    
    # result--
    result.df <- as.data.frame(cbind(df.sum.lic.HPP2, df.sum.lic, df.sum.lmic.HPP2, df.sum.lmic))
    
    result.df <- result.df %>%
      rename("LIC.HPP" = t.df.sum.lic.HPP. , "LIC.EUHC" = t.df.sum.lic. , "LMIC.HPP" = t.df.sum.lmic.HPP. , "LMIC.EUHC" = t.df.sum.lmic.)
    
    result.df$Indicators <- c("Incremental annual cost (US$, billions)",
                              "Incremental annual cost per person (US$)",
                              "Total annual cost (US$, billions)",
                              "Total annual cost per person (US$)",
                              "Incremental annual cost as a share of 2015 GNI per person (%)",
                              "Total annual cost as a share of 2015 GNI per person (%)")
    
    result.df <- result.df %>% 
      select(Indicators ,LIC.HPP, LIC.EUHC, LMIC.HPP, LMIC.EUHC)
    
    result.df$LIC.HPP <- signif(as.numeric(result.df$LIC.HPP) , digits = 2)
    result.df$LIC.EUHC <- signif(as.numeric(result.df$LIC.EUHC) , digits = 2)
    result.df$LMIC.HPP <- signif(as.numeric(result.df$LMIC.HPP) , digits = 2)
    result.df$LMIC.EUHC <- signif(as.numeric(result.df$LMIC.EUHC) , digits = 2)
    
    row.names(result.df) <- c("Incremental annual cost (US$, billions)",
                              "Incremental annual cost per person (US$)",
                              "Total annual cost (US$, billions)",
                              "Total annual cost per person (US$)",
                              "Incremental annual cost as a share of 2015 GNI per person (%)",
                              "Total annual cost as a share of 2015 GNI per person (%)")
    #Changed
    result.df %>% select(LIC.HPP, LMIC.HPP)
    
  })
  
  # Merge a static table in the paper and the custom table ----
  final.table2 <- reactive({
    final.table2 <- cbind(result_table_article, final.table())
    
    final.table2$LIC.HPP <- signif(as.numeric(final.table2$LIC.HPP) , digits = 2)
    final.table2$LIC.EUHC <- signif(as.numeric(final.table2$LIC.EUHC) , digits = 2)
    final.table2$LMIC.HPP <- signif(as.numeric(final.table2$LMIC.HPP) , digits = 2)
    final.table2$LMIC.EUHC <- signif(as.numeric(final.table2$LMIC.EUHC) , digits = 2)
    
    
    final.table2 <- final.table2[,-1] %>% 
      select(LIC.HPP, LIC.EUHC, LIC.HPP.1, LMIC.HPP, LMIC.EUHC, LMIC.HPP.1) %>% 
      rename(HPP = "LIC.HPP",
             EUHC = "LIC.EUHC",
             HPP = "LMIC.HPP",
             EUHC = "LMIC.EUHC",
             "Custom package" = "LIC.HPP.1",
             "Custom package" = "LMIC.HPP.1")
      
  } )
  
  # Result: final table output -----
  output$final.result <- function() {
    final.table2() %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F) %>%
      add_header_above(c(" ", "Default setting" = 2, " " = 1, "Default setting" = 2, " " = 1)) %>% 
      add_header_above(c(" ", "LICs" = 3, "LMICs" = 3)) 
    #%>% 
    # footnote(general = "The total and incremental costs are shown with two significant digits")
  }
  
  
  # Result: Figure 1 by urgency -----
  output$figure_urgency <- renderPlot({
    
    result.user.lic.edit.table3 <- result.user.lic.edit.table()
    result.user.lmic.edit.table3 <- result.user.lmic.edit.table()   
    result.user.both <- rbind(result.user.lic.edit.table3 ,result.user.lmic.edit.table3)
    
    # LIC: result.user.lic.edit.table3
    sum_increment <- sum(result.user.lic.edit.table3$increment_cost, na.rm = T)
    sum_total <- sum(result.user.lic.edit.table3$total_cost, na.rm = T)
    
    
    result.user.lic.edit.table3.sum <- result.user.lic.edit.table3 %>%
      group_by(Platform, Urgency) %>%
      summarise(increment_sum = (sum(increment_cost, na.rm = T)/sum_increment)*100,
                Total_sum = (sum(total_cost, na.rm = T)/sum_total)*100)
     
    #Platform: level change
    result.user.lic.edit.table3.sum$Platform <- fct_relevel(result.user.lic.edit.table3.sum$Platform,
                                                           "Referral and speacialty hospitals",
                                                           "First-level hospital",
                                                           "Health center",
                                                           "Community",
                                                           "Population based")

    # result.user.lic.edit.table3.sum$Platform <- fct_recode(result.user.lic.edit.table3.sum$Platform,
    #                                      "Population based" = "Population-based Health Interventions")

    result.user.lic.edit.table3.sum$Urgency<- fct_relevel(result.user.lic.edit.table3.sum$Urgency,
                                                          "Time-bound (non-urgent)",
                                                          "Chronic",
                                                          "Urgent")
    
    # result.user.lic.edit.table3.sum$Urgency <- fct_recode(result.user.lic.edit.table3.sum$Urgency,
    #                                     "Time-bound (non-urgent)" = "Routine",
    #                                     "Chronic" = "Ongoing")
    
    f1 <- ggplot(result.user.lic.edit.table3.sum, 
                             aes(y = increment_sum, x = Platform, fill = Urgency)) + 
                  coord_flip() + 
                  geom_bar(stat = "identity", position = "stack", width = 0.525) +
                  theme(legend.position="top", legend.title = element_blank(), 
                        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 10))+ 
                  theme_minimal() +
                  ylab("") +
                  xlab("LIC")
    
    f1add <- f1 + scale_fill_manual(values=c("#999999", "#0068b7", "#ff8000"))
    
    
    #LMIC
    sum_increment <- sum(result.user.lmic.edit.table3$increment_cost, na.rm = T)
    sum_total <- sum(result.user.lmic.edit.table3$total_cost, na.rm = T)
    
    
    result.user.lmic.edit.table3.sum <- result.user.lmic.edit.table3 %>%
      group_by(Platform, Urgency) %>%
      summarise(increment_sum = (sum(increment_cost, na.rm = T)/sum_increment)*100,
                Total_sum = (sum(total_cost, na.rm = T)/sum_total)*100)
    
    #Platform: level change
    result.user.lmic.edit.table3.sum$Platform<- fct_relevel(result.user.lmic.edit.table3.sum$Platform,
                                                            "Referral and speacialty hospitals",
                                                            "First-level hospital",
                                                            "Health center",
                                                            "Community",
                                                            "Population based")
    
    result.user.lmic.edit.table3.sum$Urgency<- fct_relevel(result.user.lmic.edit.table3.sum$Urgency,
                                                           "Time-bound (non-urgent)",
                                                           "Chronic",
                                                           "Urgent")
    
    
    f2 <- ggplot(result.user.lmic.edit.table3.sum, 
                 aes(y = increment_sum, x = Platform, fill = Urgency)) + 
      coord_flip() + 
      geom_bar(stat = "identity", position = "stack", width = 0.525) +
      theme(legend.position="top", legend.title = element_blank(), 
            axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 10))+ 
      theme_minimal() +
      ylab("") +
      xlab("LMIC")
    
    f2add <- f2 + scale_fill_manual(values=c("#999999", "#0068b7", "#ff8000"))    
  
  #Grid arrange
    
    grid.arrange(f1add, f2add)
  })
  
  
  # Result: Figure 2 by objective ----
  
  output$figure_objective <- renderPlot({
    
    # LIC
    result.user.lic.edit.table4 <- result.user.lic.edit.table()
    
    # LIC: data development
    # LIC: custom pakage LIC
    result.user.lic.edit.table4.sum1 <- result.user.lic.edit.table4 %>%
      filter(Include == T) %>% 
      group_by(Objective) %>%
      summarise(total = sum(increment_cost, na.rm = T)/1000000000)
    
    result.user.lic.edit.table4.sum1$type <- "Incremental cost"
    result.user.lic.edit.table4.sum1$setting <- "Custom"
    
    result.user.lic.edit.table4.sum2 <- result.user.lic.edit.table4 %>%
      filter(Include == T) %>% 
      group_by(Objective) %>%
      summarise(total = sum(current_cost, na.rm = T)/1000000000)
    
    result.user.lic.edit.table4.sum2$type <- "Current spending"
    result.user.lic.edit.table4.sum2$setting <- "Custom"
    
    # LIC: EUHC
    df.lic3.add <- df.lic3
    df.lic3.add$Coverage <- as.numeric(df.lic3.add$Coverage)
    df.lic3.add$adjusted_uc <- as.numeric(df.lic3.add$adjusted_uc)
    df.lic3.add$PIN <- as.numeric(df.lic3.add$PIN)
    
    
    df.lic3.add$total_cost <- ifelse(df.lic3.add$Coverage >= 0.8,
                                     df.lic3.add$adjusted_uc*(1+0.5)*(1+0.17)*(df.lic3.add$Coverage)*df.lic3.add$PIN,
                                     df.lic3.add$adjusted_uc*(1+0.5)*(1+0.17)*(0.8)*df.lic3.add$PIN)
    
    df.lic3.add$current_cost <- df.lic3.add$adjusted_uc*(1+0.5)*(1+0.17)*(df.lic3.add$Coverage)*df.lic3.add$PIN
    
    df.lic3.add$increment_cost <- df.lic3.add$total_cost - df.lic3.add$current_cost
    
    result.user.lic.edit.table4.sum3 <- df.lic3.add %>%
      filter(HPP == 1) %>% 
      group_by(Objective) %>%
      summarise(total = sum(increment_cost, na.rm = T)/1000000000)
    
    result.user.lic.edit.table4.sum3$type <- "Incremental cost"
    result.user.lic.edit.table4.sum3$setting <- "HPP"
    
    result.user.lic.edit.table4.sum4 <- df.lic3.add %>%
      filter(HPP == 1) %>% 
      group_by(Objective) %>%
      summarise(total = sum(current_cost, na.rm = T)/1000000000)
    
    result.user.lic.edit.table4.sum4$type <- "Current spending"
    result.user.lic.edit.table4.sum4$setting <- "HPP"
    
    # ALL
    result.user.lic.edit.table4.sum5 <- df.lic3.add %>%
      group_by(Objective) %>%
      summarise(total = sum(increment_cost, na.rm = T)/1000000000)
    
    result.user.lic.edit.table4.sum5$type <- "Incremental cost"
    result.user.lic.edit.table4.sum5$setting <- "EUHC"
    
    result.user.lic.edit.table4.sum6 <- df.lic3.add %>%
      group_by(Objective) %>%
      summarise(total = sum(current_cost, na.rm = T)/1000000000)
    
    result.user.lic.edit.table4.sum6$type <- "Current spending"
    result.user.lic.edit.table4.sum6$setting <- "EUHC"
    
    # merge
    all.table.both <- rbind(result.user.lic.edit.table4.sum6, 
                            result.user.lic.edit.table4.sum5,
                            result.user.lic.edit.table4.sum4,
                            result.user.lic.edit.table4.sum3,
                            result.user.lic.edit.table4.sum2,
                            result.user.lic.edit.table4.sum1)
    
    
    all.table.both$type <- fct_relevel(all.table.both$type,"Incremental cost",
                                                           "Current spending"
                                                            )
    
    all.table.both$setting <- fct_relevel(all.table.both$setting,"HPP","EUHC", "Custom")
    
    all.table.both$Objective <- fct_relevel(all.table.both$Objective,
                                             "Mortality reduction; under-5",
                                             "Mortality reduction: age 5+; group I causes",
                                             "Mortality reduction: age 5+; group II-III causes",
                                             "Reduction in disability" , 
                                             "Non-health outcomes")
    
    f4 <- ggplot(all.table.both, 
                 aes(y = total, x = setting, fill = type)) + 
      #coord_flip() + 
      geom_bar(stat = "identity", position = "stack", width = 0.525) +
      theme_minimal() + 
      theme(legend.position=c(0.9,0.9), legend.title = element_blank(),
            legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="white"),
            axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 10))+ 
      facet_wrap(~ Objective, nrow = 1, strip.position="bottom", labeller = label_wrap_gen(20)) +
      ylab("") +
      xlab("LIC") 
    
    
    
    
    # LMIC
    result.user.lmic.edit.table4 <- result.user.lmic.edit.table()
    
    # LMIC: data development
    # LMIC: custom pakage LMIC
    result.user.lmic.edit.table4.sum1 <- result.user.lmic.edit.table4 %>%
      filter(Include == T) %>% 
      group_by(Objective) %>%
      summarise(total = sum(increment_cost, na.rm = T)/1000000000)
    
    result.user.lmic.edit.table4.sum1$type <- "Incremental cost"
    result.user.lmic.edit.table4.sum1$setting <- "Custom"
    
    result.user.lmic.edit.table4.sum2 <- result.user.lmic.edit.table4 %>%
      filter(Include == T) %>% 
      group_by(Objective) %>%
      summarise(total = sum(current_cost, na.rm = T)/1000000000)
    
    result.user.lmic.edit.table4.sum2$type <- "Current spending"
    result.user.lmic.edit.table4.sum2$setting <- "Custom"
    
    # LMIC: EUHC
    df.lmic.add <- df.lmic
    df.lmic.add$Coverage <- as.numeric(df.lmic.add$Coverage)
    df.lmic.add$adjusted_uc <- as.numeric(df.lmic.add$adjusted_uc)
    df.lmic.add$PIN <- as.numeric(df.lmic.add$PIN)
    
    
    df.lmic.add$total_cost <- ifelse(df.lmic.add$Coverage >= 0.8,
                                     df.lmic.add$adjusted_uc*(1+0.5)*(1+0.17)*(df.lmic.add$Coverage)*df.lmic.add$PIN,
                                     df.lmic.add$adjusted_uc*(1+0.5)*(1+0.17)*(0.8)*df.lmic.add$PIN)
    
    df.lmic.add$current_cost <- df.lmic.add$adjusted_uc*(1+0.5)*(1+0.17)*(df.lmic.add$Coverage)*df.lmic.add$PIN
    
    df.lmic.add$increment_cost <- df.lmic.add$total_cost - df.lmic.add$current_cost
    
    result.user.lmic.edit.table4.sum3 <- df.lmic.add %>%
      filter(HPP == 1) %>% 
      group_by(Objective) %>%
      summarise(total = sum(increment_cost, na.rm = T)/1000000000)
    
    result.user.lmic.edit.table4.sum3$type <- "Incremental cost"
    result.user.lmic.edit.table4.sum3$setting <- "HPP"
    
    result.user.lmic.edit.table4.sum4 <- df.lmic.add %>%
      filter(HPP == 1) %>% 
      group_by(Objective) %>%
      summarise(total = sum(current_cost, na.rm = T)/1000000000)
    
    result.user.lmic.edit.table4.sum4$type <- "Current spending"
    result.user.lmic.edit.table4.sum4$setting <- "HPP"
    
    # ALL
    result.user.lmic.edit.table4.sum5 <- df.lmic.add %>%
      group_by(Objective) %>%
      summarise(total = sum(increment_cost, na.rm = T)/1000000000)
    
    result.user.lmic.edit.table4.sum5$type <- "Incremental cost"
    result.user.lmic.edit.table4.sum5$setting <- "EUHC"
    
    result.user.lmic.edit.table4.sum6 <- df.lmic.add %>%
      group_by(Objective) %>%
      summarise(total = sum(current_cost, na.rm = T)/1000000000)
    
    result.user.lmic.edit.table4.sum6$type <- "Current spending"
    result.user.lmic.edit.table4.sum6$setting <- "EUHC"
    
    # merge
    all.table.both2 <- rbind(result.user.lmic.edit.table4.sum1, 
                            result.user.lmic.edit.table4.sum2,
                            result.user.lmic.edit.table4.sum3,
                            result.user.lmic.edit.table4.sum4,
                            result.user.lmic.edit.table4.sum5,
                            result.user.lmic.edit.table4.sum6)
    
    
    all.table.both2$type <- fct_relevel(all.table.both2$type,"Incremental cost",
                                       "Current spending"
    )
    
    all.table.both2$setting <- fct_relevel(all.table.both2$setting,"HPP","EUHC", "Custom")
    
    all.table.both2$Objective <- fct_relevel(all.table.both2$Objective,
                                             "Mortality reduction; under-5",
                                             "Mortality reduction: age 5+; group I causes",
                                             "Mortality reduction: age 5+; group II-III causes",
                                             "Reduction in disability" , 
                                             "Non-health outcomes")
    
    
    f5 <- ggplot(all.table.both2, 
                 aes(y = total, x = setting, fill = type)) + 
      #coord_flip() + 
      geom_bar(stat = "identity", position = "stack", width = 0.525) +
      theme_minimal() + 
      theme(legend.position=c(0.9,0.9), legend.title = element_blank(),
            legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="white"), 
            axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 10))+ 
      facet_wrap(~ Objective, nrow = 1, strip.position="bottom", labeller = label_wrap_gen(20)) +
      ylab("") +
      xlab("LMIC") 
    
    grid.arrange(f4,f5, nrow = 1)
  })
    
    
  # Senario analysis--------
  # 1. Calculate adjusted unit cost based on the inputs
  
  df.lic_adjusted <- reactive({
    
    df <- lic
    df <- df %>% 
      filter(`Original Unit Cost` != "NA")
    
    gni_selected_country <- as.numeric(GNI[GNI$Country == "LIC", c("X2015")])
    
    ###
    
    final_UC <- data.frame( Name = character(),
                            unit_cost = numeric(),
                            stringsAsFactors=FALSE)
    
    
    ######setting##
    #Setting
    tradable_ratio <- as.numeric(input$tradable)
    UC_Years <- 2016  # The output year
    #####setting end##
    
    
    #Adjusted UC ------------------------------------------
    # Adjustment database
    no <- nrow(df)
    adjustment <- data.frame(ID = 1:no, exchange = NA, country = NA, year = NA, n = NA, 
                             cpi_study = NA, cpi_end = NA, cpi_adjust = NA,
                             global_inflation = NA,
                             exchange_end = NA, gni = NA)

    for (i in 1:no){
      #Country
      country <- as.character(df[i,"Country"])
      adjustment[i,"country"] <- country
      
      #Original UC_Year
      year <- df[i,"Year"]
      adjustment[i,"year"] <- year
      
      #Output year
      n <- as.numeric(year) - 1960 +2
      end_year <- as.numeric(UC_Years - 1960 +2)
      
      # Exchange of original UC
      adjustment[i,"n"] <- n
      rate <- exchange[exchange$Country == country, n]
      adjustment[i,"exchange"] <- as.numeric(rate)
      
      # consumer price index
      cpi_year <- as.numeric(year) - 1960 + 6
      cpi_end_year <- as.numeric(UC_Years - 1960 +6)
      
      adjustment[i,"cpi_study"] <- as.numeric(cpi[cpi$Country == country, cpi_year])
      adjustment[i,"cpi_end"] <- as.numeric(cpi[cpi$Country == country, cpi_end_year])
      
      # Global inflation calculation
      col_n <- as.numeric(year) - 1960 + 5
      col_end <- as.numeric(UC_Years - 1960 +5)
      
      #Global inflation of 2016, 2017 or 2018
      adjustment[i,"global_inflation"] <-
        as.numeric(prod(as.matrix(regional_inflation[regional_inflation$Country == "World", col_n:col_end])))
      
      # Exchange rate of 2016, 2017 or 2018
      adjustment[i,"exchange_end"] <- as.numeric(exchange[exchange$Country == country, end_year])
      
      #GNI
      GNI_year <- UC_Years - 1990 +5
      adjustment[i,"gni"] <- as.numeric(GNI[GNI$Country == country, c("X2015")])
    }
    
    # GNI adjustment
    # GNI selected country = LIC or LMIC
    adjustment$gni_selected_country <- gni_selected_country
    adjustment$gni_adjust <- adjustment$gni_selected_country/ adjustment$gni
    
    # CPI adjustment rate
    adjustment[,"cpi_adjust"] <- adjustment$cpi_end / adjustment$cpi_study
    
    # Merge adjustments with uc
    ucm <- cbind(df, adjustment)
    
    # Tradable
    ucm$tradable_uc <- NA
    for (i in 1:no){
      ucm$tradable_uc[i] <- tradable_conversion(
        unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
        tradable_ratio = tradable_ratio,
        currency = ucm$`Original Currency`[i],
        global_inflation = ucm$global_inflation[i],
        exchange = ucm$exchange[i])
    }
    
    
    for (i in 1:no){
      ucm$nontradable_uc[i] <- nontradable_conversion(
        unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
        tradable_ratio = tradable_ratio,
        currency = ucm$`Original Currency`[i],
        cpi_adjust = ucm$cpi_adjust[i],
        country = ucm$country[i],
        exchange = ucm$exchange[i],
        exchange_end = ucm$exchange_end[i],
        gni = ucm$gni[i],
        gni_selected_country = ucm$gni_selected_country[i])
    }
    
    # Add ancillary + above HF costs
    ucm$adjusted_uc <- (ucm$tradable_uc + ucm$nontradable_uc)
    
    ###
    #Change all tradable and non-tradable
    
    for (i in 1:nrow(ucm)){
      if(ucm$Code[i] %in% all_tradable$Code & !(ucm$Code[i] %in% all_nontradable$Code)) {
        tradable_ratio <- 1.0
        ucm$adjusted_uc[i] <- tradable_conversion(
          unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
          tradable_ratio = tradable_ratio,
          currency = ucm$`Original Currency`[i],
          global_inflation = ucm$global_inflation[i],
          exchange = ucm$exchange[i])
        ucm$nontradable_uc[i] <- 0
        ucm$tradable_uc[i] <- tradable_conversion(
          unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
          tradable_ratio = tradable_ratio,
          currency = ucm$`Original Currency`[i],
          global_inflation = ucm$global_inflation[i],
          exchange = ucm$exchange[i])
        
      } else if (!(ucm$Code[i] %in% all_tradable$Code) & ucm$Code[i] %in% all_nontradable$Code) {
        tradable_ratio <- 0
        ucm$nontradable_uc[i] <- nontradable_conversion(
          unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
          tradable_ratio = tradable_ratio,
          currency = ucm$`Original Currency`[i],
          cpi_adjust = ucm$cpi_adjust[i],
          country = ucm$country[i],
          exchange = ucm$exchange[i],
          exchange_end = ucm$exchange_end[i],
          gni = ucm$gni[i],
          gni_selected_country = ucm$gni_selected_country[i])
        ucm$tradable_uc[i] <- 0
        ucm$adjusted_uc[i] <- nontradable_conversion(
          unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
          tradable_ratio = tradable_ratio,
          currency = ucm$`Original Currency`[i],
          cpi_adjust = ucm$cpi_adjust[i],
          country = ucm$country[i],
          exchange = ucm$exchange[i],
          exchange_end = ucm$exchange_end[i],
          gni = ucm$gni[i],
          gni_selected_country = ucm$gni_selected_country[i])
      }  else{
        ucm$adjusted_uc[i] <- ucm$adjusted_uc[i]
      }
    }
    
    ###
    ucm_lic <- ucm
    
    ucm_lic$Include <- ifelse(ucm_lic$HPP == 1, TRUE, FALSE)

    ucm_lic[,c("Code","Intervention","HPP", "Include","Platform", "Urgency","Objective","PIN", "Coverage", "adjusted_uc", "Multipliers")]
  })
  
  
  df.lmic_adjusted <- reactive({
    
    df <- lmic
    df <- df %>% 
      filter(`Original Unit Cost` != "NA")
    
    gni_selected_country <- as.numeric(GNI[GNI$Country == "LMIC", c("X2015")])
    
    ###
    
    final_UC <- data.frame( Name = character(),
                            unit_cost = numeric(),
                            stringsAsFactors=FALSE)
    
    
    #setting##
    #Setting
    tradable_ratio <- as.numeric(input$tradable)
    UC_Years <- 2016  # The output year
    #setting end##
    
    
    #Adjusted UC------------------------------------------
    # Adjustment database
    no <- nrow(df)
    adjustment <- data.frame(ID = 1:no, exchange = NA, country = NA, year = NA, n = NA, 
                             cpi_study = NA, cpi_end = NA, cpi_adjust = NA,
                             global_inflation = NA,
                             exchange_end = NA, gni = NA)
    
    for (i in 1:no){
      #Country
      country <- as.character(df[i,"Country"])
      adjustment[i,"country"] <- country
      
      #Original UC_Year
      year <- df[i,"Year"]
      adjustment[i,"year"] <- year
      
      #Output year
      n <- as.numeric(year) - 1960 +2
      end_year <- as.numeric(UC_Years - 1960 +2)
      
      # Exchange of original UC
      adjustment[i,"n"] <- n
      rate <- exchange[exchange$Country == country, n]
      adjustment[i,"exchange"] <- as.numeric(rate)
      
      # consumer price index
      cpi_year <- as.numeric(year) - 1960 + 6
      cpi_end_year <- as.numeric(UC_Years - 1960 +6)
      
      adjustment[i,"cpi_study"] <- as.numeric(cpi[cpi$Country == country, cpi_year])
      adjustment[i,"cpi_end"] <- as.numeric(cpi[cpi$Country == country, cpi_end_year])
      
      # Global inflation calculation
      col_n <- as.numeric(year) - 1960 + 5
      col_end <- as.numeric(UC_Years - 1960 +5)
      
      #Global inflation of 2016, 2017 or 2018
      adjustment[i,"global_inflation"] <-
        as.numeric(prod(as.matrix(regional_inflation[regional_inflation$Country == "World", col_n:col_end])))
      
      # Exchange rate of 2016, 2017 or 2018
      adjustment[i,"exchange_end"] <- as.numeric(exchange[exchange$Country == country, end_year])
      
      #GNI
      GNI_year <- UC_Years - 1990 +5
      adjustment[i,"gni"] <- as.numeric(GNI[GNI$Country == country, c("X2015")])
    }
    
    # GNI adjustment
    # GNI selected country = LIC or LMIC
    adjustment$gni_selected_country <- gni_selected_country
    adjustment$gni_adjust <- adjustment$gni_selected_country/ adjustment$gni
    
    # CPI adjustment rate
    adjustment[,"cpi_adjust"] <- adjustment$cpi_end / adjustment$cpi_study
    
    # Merge adjustments with uc
    ucm <- cbind(df, adjustment)
    
    # Tradable
    ucm$tradable_uc <- NA
    for (i in 1:no){
      ucm$tradable_uc[i] <- tradable_conversion(
        unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
        tradable_ratio = tradable_ratio,
        currency = ucm$`Original Currency`[i],
        global_inflation = ucm$global_inflation[i],
        exchange = ucm$exchange[i])
    }
    
    
    for (i in 1:no){
      ucm$nontradable_uc[i] <- nontradable_conversion(
        unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
        tradable_ratio = tradable_ratio,
        currency = ucm$`Original Currency`[i],
        cpi_adjust = ucm$cpi_adjust[i],
        country = ucm$country[i],
        exchange = ucm$exchange[i],
        exchange_end = ucm$exchange_end[i],
        gni = ucm$gni[i],
        gni_selected_country = ucm$gni_selected_country[i])
    }
    
    # Add ancillary + above HF costs
    ucm$adjusted_uc <- (ucm$tradable_uc + ucm$nontradable_uc)
    
    ###
    #Change all tradable and non-tradable
    
    for (i in 1:nrow(ucm)){
      if(ucm$Code[i] %in% all_tradable$Code & !(ucm$Code[i] %in% all_nontradable$Code)) {
        tradable_ratio <- 1.0
        ucm$adjusted_uc[i] <- tradable_conversion(
          unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
          tradable_ratio = tradable_ratio,
          currency = ucm$`Original Currency`[i],
          global_inflation = ucm$global_inflation[i],
          exchange = ucm$exchange[i])
        ucm$nontradable_uc[i] <- 0
        ucm$tradable_uc[i] <- tradable_conversion(
          unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
          tradable_ratio = tradable_ratio,
          currency = ucm$`Original Currency`[i],
          global_inflation = ucm$global_inflation[i],
          exchange = ucm$exchange[i])
        
      } else if (!(ucm$Code[i] %in% all_tradable$Code) & ucm$Code[i] %in% all_nontradable$Code) {
        tradable_ratio <- 0
        ucm$nontradable_uc[i] <- nontradable_conversion(
          unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
          tradable_ratio = tradable_ratio,
          currency = ucm$`Original Currency`[i],
          cpi_adjust = ucm$cpi_adjust[i],
          country = ucm$country[i],
          exchange = ucm$exchange[i],
          exchange_end = ucm$exchange_end[i],
          gni = ucm$gni[i],
          gni_selected_country = ucm$gni_selected_country[i])
        ucm$tradable_uc[i] <- 0
        ucm$adjusted_uc[i] <- nontradable_conversion(
          unit_cost = as.numeric(ucm$`Original Unit Cost`[i]),
          tradable_ratio = tradable_ratio,
          currency = ucm$`Original Currency`[i],
          cpi_adjust = ucm$cpi_adjust[i],
          country = ucm$country[i],
          exchange = ucm$exchange[i],
          exchange_end = ucm$exchange_end[i],
          gni = ucm$gni[i],
          gni_selected_country = ucm$gni_selected_country[i])
      }  else{
        ucm$adjusted_uc[i] <- ucm$adjusted_uc[i]
      }
    }
    
    ###
    ucm_lmic <- ucm
    
    ucm_lmic$Include <- ifelse(ucm_lmic$HPP == 1, TRUE, FALSE)
    
    ucm_lmic[,c("Code","Intervention","HPP", "Include","Platform", "Urgency","Objective","PIN", "Coverage", "adjusted_uc", "Multipliers")]
  })
  
  
  
  
  # Summary: senario Analysis ----------
  # LICs----- 
  result.user.lic <- reactive({
    
    lic.full <- df.lic_adjusted()
    
    lic.full$Coverage <- as.numeric(lic.full$Coverage)
    lic.full$PIN <- as.numeric(lic.full$PIN)
    
    #LICs
    # Default setting
    lic_multi <- lic[,c("Code", "Multipliers")]
    
    lic.full.default <- merge(df.lic3, lic_multi, by = "Code", all.y = T)
    
    lic.full.default$default_total_cost <- ifelse(lic.full.default$Coverage >= 0.8,
                                                  lic.full.default$adjusted_uc*(1+0.5)*(1+0.17)*(lic.full.default$Coverage)*lic.full.default$PIN,
                                                  lic.full.default$adjusted_uc*(1+0.5)*(1+0.17)*(0.8)*lic.full.default$PIN)
    
    lic.full.default$default_current_cost <- lic.full.default$adjusted_uc*(1+0.5)*(1+0.17)*(lic.full.default$Coverage)*lic.full.default$PIN
    
    lic.full.default$default_increment_cost <- lic.full.default$default_total_cost - lic.full.default$default_current_cost
    lic.full.default <- lic.full.default %>% 
      rename(default_adjusted_uc = adjusted_uc)
    
    lic.full <- merge(lic.full, lic.full.default[,c("Code","default_adjusted_uc","default_total_cost", "default_current_cost", "default_increment_cost")], by = "Code", all.y = T)
    
    # User setting
    lic.full$total_cost <- ifelse(lic.full$Multipliers == "None",
                            (lic.full$adjusted_uc*input$UC_multi)*(1+(input$HF_cost/100))*(1+(input$above_cost/100))*(input$target/100)*lic.full$PIN,
                            ifelse(lic.full$Multipliers == "Fertility",
                                   lic.full$adjusted_uc*input$UC_multi*(1+(input$HF_cost/100))*(1+(input$above_cost/100))*(input$target/100)*lic.full$PIN*input$fertility_multi_lic,
                                   ifelse(lic.full$Multipliers == "Population",
                                          lic.full$adjusted_uc*input$UC_multi*(1+(input$HF_cost/100))*(1+(input$above_cost/100))*(input$target/100)*lic.full$PIN*input$PIN_multi,
                                          ifelse(lic.full$Multipliers == "Both",
                                                 lic.full$adjusted_uc*input$UC_multi*(1+(input$HF_cost/100))*(1+(input$above_cost/100))*(input$target/100)*lic.full$PIN*input$PIN_multi*input$fertility_multi_lic,
                                                 NA))))
    
    # Current cost to achieve current baseline coverage

    lic.full$current_cost <- ifelse(lic.full$Multipliers == "None",
                                      lic.full$adjusted_uc*input$UC_multi*(1+(input$HF_cost/100))*(1+(input$above_cost/100))*(lic.full$Coverage+input$coverage_multi)*lic.full$PIN,
                                      ifelse(lic.full$Multipliers == "Fertility",
                                             lic.full$adjusted_uc*input$UC_multi*(1+(input$HF_cost/100))*(1+(input$above_cost/100))*(lic.full$Coverage+input$coverage_multi)*lic.full$PIN*input$fertility_multi_lic,
                                             ifelse(lic.full$Multipliers == "Population",
                                                    lic.full$adjusted_uc*input$UC_multi*(1+(input$HF_cost/100))*(1+(input$above_cost/100))*(lic.full$Coverage+input$coverage_multi)*lic.full$PIN*input$PIN_multi,
                                                    ifelse(lic.full$Multipliers == "Both",
                                                           lic.full$adjusted_uc*input$UC_multi*(1+(input$HF_cost/100))*(1+(input$above_cost/100))*(lic.full$Coverage+input$coverage_multi)*lic.full$PIN*input$PIN_multi*input$fertility_multi_lic, 
                                                           NA))))
    
    lic.full$total_cost <- ifelse(lic.full$current_cost >= lic.full$total_cost, 
                                  lic.full$current_cost,
                                  lic.full$total_cost)
      
      
    lic.full$increment_cost <- lic.full$total_cost - lic.full$current_cost
    
    #lic.full$increment_cost <- round(lic.full$increment_cost, 6)
    # Diff between default and user setting
    lic.full$diff_cost <- lic.full$increment_cost - lic.full$default_increment_cost
    
    lic.full$diff_cost <- ifelse(lic.full$diff_cost <= 0.00001 & lic.full$diff_cost >= -0.0001,
                                 0,
                                 lic.full$diff_cost
                                 )
    
    # Diff between default and user setting
    lic.full$diff_cost_total <- lic.full$total_cost - lic.full$default_total_cost
    
    lic.full$diff_cost_total <- ifelse(lic.full$diff_cost_total <= 0.00001 & lic.full$diff_cost_total >= -0.0001,
                                 0,
                                 lic.full$diff_cost_total
    )
    
    lic.full
  })
  
  
  # LMICs-----
  result.user.lmic <- reactive({
    
    #LMICs
    full.df.lmic <- df.lmic_adjusted()
    
    full.df.lmic$Coverage <- as.numeric(full.df.lmic$Coverage)
    full.df.lmic$PIN <- as.numeric(full.df.lmic$PIN)
    
    #Default setting
    lmic_multi <- lmic[,c("Code", "Multipliers")]
    lmic.full.default <- merge(df.lmic, lmic_multi, by = "Code", all.y = T)
    
    lmic.full.default$default_total_cost <- ifelse(lmic.full.default$Coverage >= 0.8,
                                                  lmic.full.default$adjusted_uc*(1+0.5)*(1+0.17)*(lmic.full.default$Coverage)*lmic.full.default$PIN,
                                                  lmic.full.default$adjusted_uc*(1+0.5)*(1+0.17)*(0.8)*lmic.full.default$PIN)
    
    lmic.full.default$default_current_cost <- lmic.full.default$adjusted_uc*(1+0.5)*(1+0.17)*(lmic.full.default$Coverage)*lmic.full.default$PIN
    
    lmic.full.default$default_increment_cost <- lmic.full.default$default_total_cost - lmic.full.default$default_current_cost
    lmic.full.default <- lmic.full.default %>% 
      rename(default_adjusted_uc = adjusted_uc)
    
    full.df.lmic <- merge(full.df.lmic, lmic.full.default[,c("Code","default_adjusted_uc","default_total_cost", "default_current_cost", "default_increment_cost")], by = "Code", all.y = T)
    
    # User setting

    full.df.lmic$total_cost <- ifelse(full.df.lmic$Multipliers == "None",
                                 full.df.lmic$adjusted_uc*input$UC_multi*(1+(input$HF_cost/100))*(1+(input$above_cost/100))*(input$target/100)*full.df.lmic$PIN,
                                 ifelse(full.df.lmic$Multipliers == "Fertility",
                                        full.df.lmic$adjusted_uc*input$UC_multi*(1+(input$HF_cost/100))*(1+(input$above_cost/100))*(input$target/100)*full.df.lmic$PIN*input$fertility_multi_lmic,
                                        ifelse(full.df.lmic$Multipliers == "Population",
                                               full.df.lmic$adjusted_uc*input$UC_multi*(1+(input$HF_cost/100))*(1+(input$above_cost/100))*(input$target/100)*full.df.lmic$PIN*input$PIN_multi,
                                               ifelse(full.df.lmic$Multipliers == "Both",
                                                      full.df.lmic$adjusted_uc*input$UC_multi*(1+(input$HF_cost/100))*(1+(input$above_cost/100))*(input$target/100)*full.df.lmic$PIN*input$PIN_multi*input$fertility_multi_lmic,
                                                      NA))))
    
    # Current cost to achieve current baseline coverage
    
    full.df.lmic$current_cost <- ifelse(full.df.lmic$Multipliers == "None",
                                   full.df.lmic$adjusted_uc*input$UC_multi*(1+(input$HF_cost/100))*(1+(input$above_cost/100))*(full.df.lmic$Coverage+input$coverage_multi)*full.df.lmic$PIN,
                                   ifelse(full.df.lmic$Multipliers == "Fertility",
                                          full.df.lmic$adjusted_uc*input$UC_multi*(1+(input$HF_cost/100))*(1+(input$above_cost/100))*(full.df.lmic$Coverage+input$coverage_multi)*full.df.lmic$PIN*input$fertility_multi_lmic,
                                          ifelse(full.df.lmic$Multipliers == "Population",
                                                 full.df.lmic$adjusted_uc*input$UC_multi*(1+(input$HF_cost/100))*(1+(input$above_cost/100))*(full.df.lmic$Coverage+input$coverage_multi)*full.df.lmic$PIN*input$PIN_multi,
                                                 ifelse(full.df.lmic$Multipliers == "Both",
                                                        full.df.lmic$adjusted_uc*input$UC_multi*(1+(input$HF_cost/100))*(1+(input$above_cost/100))*(full.df.lmic$Coverage+input$coverage_multi)*full.df.lmic$PIN*input$PIN_multi*input$fertility_multi_lmic, 
                                                        NA))))
    
    full.df.lmic$total_cost <- ifelse(full.df.lmic$current_cost >= full.df.lmic$total_cost, 
                                      full.df.lmic$current_cost,
                                      full.df.lmic$total_cost)
    
    full.df.lmic$increment_cost <- full.df.lmic$total_cost - full.df.lmic$current_cost
    
    # Diff between default and user setting
    full.df.lmic$diff_cost <- full.df.lmic$increment_cost - full.df.lmic$default_increment_cost
    
    full.df.lmic$diff_cost <- ifelse(full.df.lmic$diff_cost <= 0.0001 & full.df.lmic$diff_cost >= -0.0001,
                                 0,
                                 full.df.lmic$diff_cost)
    
    # Diff between default and user setting
    full.df.lmic$diff_cost_total <- full.df.lmic$total_cost - full.df.lmic$default_total_cost
    
    full.df.lmic$diff_cost_total <- ifelse(full.df.lmic$diff_cost_total <= 0.0001 & full.df.lmic$diff_cost_total >= -0.0001,
                                     0,
                                     full.df.lmic$diff_cost_total)


    full.df.lmic
    
    
  })
  
  

  # Summary table by platform:LIC
  result.user.lic.sum <-  reactive({
    result.user.lic() %>% 
      filter(is.na(Intervention) == F) %>% 
      group_by(Platform) %>% 
      summarise(increment_cost_platform = signif(sum(increment_cost, na.rm = T)/1000000000, digits = 2),
                diff_cost_platform = signif(sum(diff_cost, na.rm = T)/1000000000, digits = 2),
                total_cost_platform = signif(sum(total_cost, na.rm = T)/1000000000, digits = 2),
                diff_total_cost_platform = signif(sum(diff_cost_total, na.rm = T)/1000000000, digits = 2)
                ) %>% 
      rename(Incremental_cost_LIC = increment_cost_platform,
             Diff_LIC = diff_cost_platform,
             Total_cost_LIC = total_cost_platform,
             Diff_total_LIC = diff_total_cost_platform)
  })  
  
  
  
  # Summary table by platform:LMIC
  result.user.lmic.sum <-  reactive({
    result.user.lmic() %>% 
      #fct_explicit_na(Platform) %>% 
      group_by(Platform) %>% 
      summarise(increment_cost_platform = signif(sum(increment_cost, na.rm = T)/1000000000, digits = 2),
                diff_cost_platform = signif(sum(diff_cost, na.rm = T)/1000000000, digits = 2),
                total_cost_platform = signif(sum(total_cost, na.rm = T)/1000000000, digits = 2),
                diff_total_cost_platform = signif(sum(diff_cost_total, na.rm = T)/1000000000, digits = 2)
                ) %>% 
      rename(Incremental_cost_LMIC = increment_cost_platform,
             Diff_LMIC = diff_cost_platform,
             Total_cost_LMIC = total_cost_platform,
             Diff_total_LMIC = diff_total_cost_platform)
  })
  
  

  
  
  result.user.both.sum <- reactive({
    result.user.both.sum <- cbind(result.user.lic.sum()[-6,], result.user.lmic.sum()[-6,-1])
    
    result.user.both.sum[5,1] <- "Referral and specialty hospitals"
    
    result.user.both.sum$Incremental_cost_LIC <- as.character(result.user.both.sum$Incremental_cost_LIC)
    result.user.both.sum$Diff_LIC <- as.character(result.user.both.sum$Diff_LIC)
    result.user.both.sum$Total_cost_LIC <- as.character(result.user.both.sum$Total_cost_LIC)
    result.user.both.sum$Diff_total_LIC <- as.character(result.user.both.sum$Diff_total_LIC)
    result.user.both.sum$Incremental_cost_LMIC <- as.character(result.user.both.sum$Incremental_cost_LMIC)
    result.user.both.sum$Diff_LMIC <- as.character(result.user.both.sum$Diff_LMIC)
    result.user.both.sum$Total_cost_LMIC <- as.character(result.user.both.sum$Total_cost_LMIC)
    result.user.both.sum$Diff_total_LMIC <- as.character(result.user.both.sum$Diff_total_LMIC)
    
    
    result.user.both.sum %>%
      rename("Incremental cost (US$, billions)" = Incremental_cost_LIC,
             "Diff. from the default setting (US$, billions)" = Diff_LIC,
             "Total cost (US$, billions)" = Total_cost_LIC,
             "Diff. from the default setting (US$, billions)" = Diff_total_LIC,
             "Incremental cost (US$, billions)" = Incremental_cost_LMIC,
             "Diff. from the default setting (US$, billions)" = Diff_LMIC,
             "Total cost (US$, billions)1" = Total_cost_LMIC,
             "Diff. from the default setting (US$, billions)" = Diff_total_LMIC)
    
  })
  
  output$result.user.table <- function() {
    result.user.both.sum() %>%
      knitr::kable("html", align='r') %>%
      kable_styling("striped", full_width = F) %>%
      add_header_above(c(" ","LICs" = 4, "LMICs" = 4))
  }
  
  # Senario analysis: figure ----
  output$result.figure1 <- renderPlot({
    
    df.lic.sen <- subset(result.user.lic.sum(), Platform != "NA")
    
    df.lic.sen[5,1] <- "Referral and specialty hospitals"
    
    df.lic.sen <- df.lic.sen %>% 
      gather(condition, measurement, Incremental_cost_LIC:Diff_total_LIC, factor_key = TRUE)
    
    df.lic.sen <- df.lic.sen %>% 
      filter(condition == "Diff_LIC" | condition == "Diff_total_LIC")
    
    
    f1 <- ggplot(df.lic.sen, 
                 aes(y = measurement, x = Platform, fill = condition)) + 
      coord_flip() + 
      geom_bar(stat = "identity", position = "dodge", width = 0.525) +
      theme(legend.position="top", axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 10))+ 
      theme_minimal() +
      ylab("Difference from the default setting in LICs (US$, billions)") +
      xlab("") + scale_fill_manual(
        name = "",
        label = c("Diff of incremental cost","Diff. of total cost"),
        values = c("#619CFF", "#F8766D"))
    
    
    df.lmic.sen <- subset(result.user.lmic.sum(), Platform != "NA")
    df.lmic.sen <- df.lmic.sen %>% 
      gather(condition, measurement, Incremental_cost_LMIC:Diff_total_LMIC, factor_key = TRUE)
    
    df.lmic.sen <- df.lmic.sen %>% 
      filter(condition == "Diff_LMIC" | condition == "Diff_total_LMIC")
    
    f2 <- ggplot(df.lmic.sen, aes(y = measurement, x = Platform, fill = condition)) + 
      coord_flip() + 
      geom_bar(stat = "identity", position = "dodge", width = 0.525) +
      theme(legend.position="top", axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 10)) +
      theme_minimal() +
      ylab("Difference from the default setting in LMICs (US$, billions)") +
      xlab("") +
      theme(legend.position= "none",
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) + scale_fill_manual(
              name = "",
              label = c("",""),
              values = c("#619CFF", "#F8766D"))
      
    
    grid.arrange(f1,f2, nrow = 1, widths = c(1.3,0.8))
    
    
  })
  

  
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("DCP3_","Costing_data", ".csv", sep="")
    },
    content = function(file) {
      write.csv(final.table2(), file)
    }
  )
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData_detail_LIC <- downloadHandler(
    filename = function() {
      paste("DCP3_","Costing_data_detail_LIC", ".csv", sep="")
    },
    content = function(file) {
      write.csv(result.user.lic(), file)
    }
  )
  
  output$downloadData_detail_LMIC <- downloadHandler(
    filename = function() {
      paste("DCP3_","Costing_data_detail_LMIC", ".csv", sep="")
    },
    content = function(file) {
      write.csv(result.user.lmic(), file)
    }
  )
  
  

  
})
