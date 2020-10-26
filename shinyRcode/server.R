function(input, output) { 
  
  # -- Get data and select variables ----
  
  # Data needed
  rv_data <- reactiveValues(procprod_raw0 = data.table(),
                            procprod_raw = data.table(),
                            procprod_imp0 = data.table(),
                            procprod_imp = data.table(),
                            commodityDB = data.table(),
                            commodity_label = data.table(),
                            globalProduction = data.table()
  )
  
  # Mappings needed
  rv_mappingtable <- reactiveValues(map_prod_exp0 = data.table(),
                                    map_prod_exp = data.table(),
                                    map_prod_prod0 = data.table(),
                                    map_prod_prod = data.table()
  )
  
  checkPassed <- reactiveValues(Exp = 'No',
                                Prim = 'No')
  
  # Select INITIAL needed dataset
  
  observeEvent(input$btn_country, { 
  
    # Selected country
    sel_country <- country_input[label == input$btn_country, code]
    
    if(sel_country != "-") {
      # form to get country correspondent datatable chunk
      where <- paste("geographicaream49_fi = '", sel_country, "' ", 
                     sep = "")
      
      withProgress(message = 'Loading raw data',
                   value = 0, {
                     
                     Sys.sleep(0.25)
                     incProgress(0.25)
                    
                     # Original datatable
                     procprodraw0 <- ReadDatatable('processed_prod_national_detail_raw', 
                                                   where = where)
                     rv_data$procprod_raw0 <- procprodraw0
                     
                     validate(need(nrow(procprodraw0)>0,
                                   "Processed production data not available for this country. Choose another country.")
                     )
                     
                     Encoding(procprodraw0$nationaldescription) <- "unknown" 
                     Encoding(procprodraw0$remarks) <- "unknown" 
                     
                     # Copied datatable to work on
                     procprodraw <- copy(procprodraw0)
                     setnames(procprodraw, 
                              c("geographicaream49_fi", 
                                "measuredelement", 
                                "timepointyears", 
                                "measureditemisscfc",
                                "quantitymt", 
                                "flagobservationstatus", 
                                "flagmethod", 
                                "id_nationalcode"), 
                              c("geographicAreaM49_fi", 
                                "measuredElement", 
                                "timePointYears", 
                                "measuredItemISSCFC",
                                "Value", 
                                "flagObservationStatus", 
                                "flagMethod", 
                                "Scheda"))
                     
                     procprodraw$timePointYears <- as.integer(procprodraw$timePointYears)
                     start_year <- min(as.integer(procprodraw$timePointYears))
                     end_year <- as.integer(max(as.integer(procprodraw$timePointYears))+1) 
                     # year to expand to
                     
                     # expand datatable to have also missing values otherwise omitted in 
                     # the datatable
                     procprodrawExp <- expandYear(procprodraw[as.numeric(timePointYears) < end_year, ], 
                                                  areaVar = "geographicAreaM49_fi", 
                                                  elementVar = "measuredElement", 
                                                  itemVar = c("measuredItemISSCFC", "id_isscfc", "Scheda", "nationaldescription"), 
                                                  yearVar = "timePointYears" , 
                                                  valueVar = "Value",
                                                  newYears = end_year)
                     
                     if(!is.numeric(procprodrawExp$Value)){
                       procprodrawExp$Value <- as.numeric(procprodrawExp$Value)
                     }
                     
                     # Identify data with no info
                     procprodrawExp <- removeNoInfo(procprodrawExp, value = "Value",
                                                    observationFlag = "flagObservationStatus", 
                                                    byKey = c("geographicAreaM49_fi", "Scheda"))
                     
                     procprodrawExp$flagObservationStatus <- factor(procprodrawExp$flagObservationStatus, 
                                                                    levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                                                    ordered = TRUE)
                     
                     procprodrawExp$timePointYears<- as.character(procprodrawExp$timePointYears)
                     
                     Sys.sleep(0.75)
                     incProgress(0.95)
                     
                     rv_data$procprod_raw0 <- procprodraw0
                     rv_data$procprod_raw <- procprodrawExp
                     
                     
                   })
      
    }
    
  })
  
  
  # -- Year ----
  output$btn_year <- renderUI({
    # country button required
    validate(need(country_input[country_input$label == input$btn_country, code] != "-", 
                  "Please choose a country."))
    #input$btn_country
    req(input$btn_country)
    
    input$btn_country
    
    # select country code
    sel_country <- country_input[country_input$label == input$btn_country, code]
    if(sel_country != "-") {
      # get raw data for processed production
      procprodraw <- rv_data$procprod_raw
      
      validate(need(nrow(rv_data$procprod_raw0)>0,
                    "Processed production data not available for this country. Choose another country.")
      )
      # Check if data available for the chosen country
      country_selected <- procprodraw[geographicAreaM49_fi == sel_country, ]
      
      # Get years for this country in decreasing order
      years_input <- sort(as.numeric(country_selected[, unique(timePointYears)]), decreasing = T)
      
      # Input details
      selectInput(inputId = "btn_year",
                  label = 'Imputation year',
                  choices = c("", years_input)
      )
    }
    
  })
  
  # -- Start year ----
  output$btn_start_year <- renderUI({
    
    # Country and year buttons required
    req(input$btn_country, input$btn_year)
    
    # Same process as for year button but only earlier years than imputation years proposed
    sel_country <- country_input[country_input$label == input$btn_country, code]
    if(sel_country != "-" & input$btn_year != "") {
      procprodraw <- rv_data$procprod_raw
      country_selected <- procprodraw[geographicAreaM49_fi == sel_country ]
      years_input <- sort(as.numeric(country_selected[, unique(timePointYears)]), decreasing = T)
      start_year_input <- years_input[years_input < input$btn_year]
      
      selectInput(inputId = "btn_start_year",
                  label = 'Starting year',
                  choices = c("", start_year_input)
      )
    }
  })
  
  
  # -- Charge imputed table ----
  observeEvent(input$btn_start_year, { 

    sel_country <- country_input[country_input$label == input$btn_country, code]
    
    if(!is.null(input$btn_start_year) & input$btn_start_year != ""){
      sel_years <- input$btn_start_year:input$btn_year
      procprodraw <- rv_data$procprod_raw
      procprodraw <- procprodraw[timePointYears %in% sel_years, ]
      where <- paste("geographicaream49_fi = '", sel_country, "' ", sep = "")

      withProgress(message = 'Loading complete (official + imputed) data',
                   value = 0, {
                     
                     Sys.sleep(0.25)
                     incProgress(0.25)
                     # Get Processed production datatable imputed
                     procprod_imp0 <- ReadDatatable('processed_prod_national_detail_imputed', 
                                                    where = where, readOnly = FALSE)
                     procprod_imp0 <- procprod_imp0[timepointyears %in% sel_years, ]
                     
                     procprod_imp <- copy(procprod_imp0)
                     
                     procprod_imp <- procprod_imp[ , c("__id", "__ts") := NULL]
                     
                     setnames(procprod_imp, 
                              c("geographicaream49_fi", 
                                "measuredelement", 
                                "timepointyears", 
                                "measureditemisscfc",
                                "quantitymt", 
                                "flagobservationstatus", 
                                "flagmethod", 
                                "id_nationalcode"), 
                              c("geographicAreaM49_fi", 
                                "measuredElement", 
                                "timePointYears", 
                                "measuredItemISSCFC",
                                "Value", 
                                "flagObservationStatus", 
                                "flagMethod", 
                                "Scheda"))
                     
                     procprod_imp$flagObservationStatus <- factor(procprod_imp$flagObservationStatus, 
                                                                  levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                                                  ordered = TRUE)
                     
                     Encoding(procprod_imp0$nationaldescription) <- "unknown" 
                     Encoding(procprod_imp0$remarks) <- "unknown" 
                     Encoding(procprod_imp$nationaldescription) <- "unknown" 
                     Encoding(procprod_imp$remarks) <- "unknown" 
                     Encoding(procprodraw$nationaldescription) <- "unknown" 
                     Encoding(procprodraw$remarks) <- "unknown" 
                     
                     procprod_imp_upd <- merge(procprod_imp[ , .(geographicAreaM49_fi, 
                                                                 measuredElement, 
                                                                 timePointYears, 
                                                                 measuredItemISSCFC, 
                                                                 remarks, Value, 
                                                                 flagObservationStatus,
                                                                 flagMethod, Scheda, 
                                                                 measureditemnational,
                                                                 approach)], 
                                               procprodraw, 
                                               by = c( "geographicAreaM49_fi", 
                                                       "measuredElement", 
                                                       "timePointYears", 
                                                       "measuredItemISSCFC", 
                                                       "Scheda"), 
                                               all = TRUE, suffixes = c("_old", "_upd"))
                     
                     
                     # Remarks are the old ones, if the row was empty in the imputed table, 
                     # i.e. the remark cell was empty then the raw updated one is assigned
                     procprod_imp_upd[ , remarks := remarks_old]
                     procprod_imp_upd[is.na(remarks_old), remarks := remarks_upd]
                     
                     # Values are the old ones, if the row was empty in the imputed table, 
                     # i.e. the Values cell was empty then the raw updated one is assigned
                     procprod_imp_upd[ , Value := as.numeric(Value_old)]
                     procprod_imp_upd[is.na(Value_old), Value := as.numeric(Value_upd)]
                     
                     # flagObservationStatus are the old ones, if the row was empty in the imputed table, 
                     # i.e. the flagObservationStatus cell was empty then the raw updated one is assigned
                     procprod_imp_upd[ , flagObservationStatus := flagObservationStatus_old]
                     procprod_imp_upd[is.na(procprod_imp_upd$flagObservationStatus), ]$flagObservationStatus <- procprod_imp_upd[is.na(procprod_imp_upd$flagObservationStatus), ]$flagObservationStatus_upd
                     
                     procprod_imp_upd[, flagMethod := flagMethod_old]
                     procprod_imp_upd[is.na(flagMethod_old), flagMethod := flagMethod_upd]
                     
                     # If official data in updated table than it overwrites 
                     procprod_imp_upd[flagObservationStatus_upd %in% c('', 'X'), 
                                      c('Value', 'flagObservationStatus', 'flagMethod', 'remarks') := list(Value_upd,
                                                                                                           flagObservationStatus_upd,
                                                                                                           flagMethod_upd,
                                                                                                           remarks_upd)]
                     
                     
                     procprod_imp_upd$flagObservationStatus <- factor(procprod_imp_upd$flagObservationStatus, 
                                                                      levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                                                      ordered = TRUE)
                    
                     procprod_imp_upd[ , c('remarks_old', 'Value_old', 'flagObservationStatus_old', 'flagMethod_old',
                                           'remarks_upd', 'Value_upd', 'flagObservationStatus_upd', 'flagMethod_upd') := NULL]
                   
                     # If previous year the data was (O, -) then if this year it was considered (M, u) the figure is set back to (O, -)
                     imputYear <- input$btn_year
                     missingO <- procprod_imp_upd[flagObservationStatus == 'O' & timePointYears == as.character((as.numeric(imputYear) - 1)), ]
                     
                     if(nrow(missingO) > 0){
                       schede2change <- unique(missingO$Scheda)
                       procprod_imp_upd[timePointYears == imputYear & Scheda %in% schede2change &
                                          flagObservationStatus == 'M' & flagMethod == 'u', 
                                        c("Value", "flagObservationStatus", "flagMethod") := list(0, 'O', '-')]
                     }
                     
                     
                     rv_data$procprod_imp0 <- procprod_imp0
                     rv_data$procprod_imp <- procprod_imp_upd
                     
                     Sys.sleep(0.75)
                     incProgress(0.95)
                   })
      
    }
    
  })
  
  # -- Missing ----
  output$btn_missing <- renderUI({
    
    # Country and years buttons required
    req(input$btn_country, input$btn_year, input$btn_start_year)
    
    # Button details, three possibilities
    selectInput(inputId = "btn_missing",
                label = 'Missing',
                choices = c("Yes", "No", "Not to impute")
    )
    
  })
  
  # -- Commodities ----
  output$btn_commodity <- renderUI({
    # Country, years and missing buttons required
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_missing)
  
    input$btn_imputation
    
    # load parameters and raw data
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_year <- input$btn_year
    
    procprodimp <- rv_data$procprod_imp
    
    # create data.table with list of product description, ISSCFC code and scheda.
    completeMap <- merge(map_isscfc, procprodimp[ , .( measuredItemISSCFC, Scheda, nationaldescription)],
                         by.x = 'code',
                         by.y = 'measuredItemISSCFC', all.y = TRUE)
    setkey(completeMap)
    completeMap <- completeMap[!duplicated(completeMap)]
    commodity_label <- sprintf("%s - %s - %s", completeMap$code, completeMap$description, completeMap$Scheda)
    commodity_label <- data.table(isscfc = completeMap$code, label = commodity_label, code = completeMap$Scheda, M49 = sel_country)
    
    setkey(commodity_label)
    commodity_label <- unique(commodity_label)
    
    # Assign the list to the rv_data list
    rv_data$commodity_label <-  commodity_label
    
    # Select right data depending on missin button chosen
    
    if(input$btn_missing == "Yes"){ # if missing to impute so flags (M, u)
      commodity_input_list <- procprodimp[geographicAreaM49_fi == sel_country & 
                                            timePointYears == sel_year &
                                            flagObservationStatus %in% c('M') &
                                            flagMethod == 'u', 
                                          unique(Scheda)]
    } else if (input$btn_missing == "No"){ # all data with flag different from M, O or Q
      commodity_input_list <- procprodimp[geographicAreaM49_fi == sel_country & 
                                            timePointYears == sel_year &
                                            !flagObservationStatus %in% c('M', 'O', 'Q'),
                                          unique(Scheda)]
    } else { # data with flags (M, -) and (O, -)
      commodity_input_list <- procprodimp[geographicAreaM49_fi == sel_country &
                                            timePointYears == sel_year &
                                            flagObservationStatus %in% c('M', 'O') &
                                            flagMethod == '-',
                                          unique(Scheda)]
    }
    
    
    # list of commodities available
    commodity_input <- commodity_label[ M49 %in% sel_country &
                                          code %in% commodity_input_list, label]
    
    selectInput(inputId = "btn_commodity",
                label = 'Commodity',
                choices = c("",commodity_input)
    )
    
  })
  
  # -- First tab showing imputed table----
  output$imputed_data <- DT::renderDataTable(server = FALSE, {
    req(input$btn_country, input$btn_year, input$btn_start_year)
    if(is.null(input$btn_start_year)) return(NULL)
    
    tab2display <- copy(rv_data$procprod_imp)
    
    if(nrow(tab2display) > 0){
    setcolorder(tab2display, c("geographicAreaM49_fi", "Scheda", "timePointYears", 
                               "id_isscfc", "measuredItemISSCFC", "nationalquantity",
                               "nationalquantityunit", "Value", "flagObservationStatus",
                               "flagMethod", "approach", "nationalcode", "nationaldescription",
                               "remarks", "measureditemnational", "measuredElement"))
    setnames(tab2display, c("geographicAreaM49_fi", "Scheda", "timePointYears", 
                            "id_isscfc", "measuredItemISSCFC", "nationalquantity",
                            "nationalquantityunit", "Value", "flagObservationStatus",
                            "flagMethod", "approach", "nationalcode", "nationaldescription",
                            "remarks", "measureditemnational", "measuredElement"),
             c("Country", "Scheda", "Year", 
               "id_isscfc", "ISSCFC", "N.quantity",
               "N. unit", "Value", "Obs.flag",
               "Met.flag", "Approach", "N.code", "N.description",
               "Remarks", "N.Item", "Element"))
    }
    DT::datatable(tab2display, extensions = 'Buttons', filter = 'top',
                  options = list(
                    dom = 'Bfrtip',
                    buttons = c('csv', 'excel', 'pdf')) # , editable = TRUE
    )
    
  })
  
  
  observeEvent(input$check_new_items, {
    
    req(input$btn_country, input$btn_year, input$btn_start_year)  
    procprodraw <- rv_data$procprod_raw
    procprod_imp0 <- rv_data$procprod_imp0
    
    newComm <- unique(procprodraw[ !measuredItemISSCFC %in% unique(procprod_imp0$measureditemisscfc)]$measuredItemISSCFC)
    newScheda <- unique(procprodraw[ !Scheda %in% unique(procprod_imp0$id_nationalcode)]$Scheda)
    
    
    if(length(newComm) > 0){
      
      newComm_aux <- newComm
      
      if( length(newComm) > 5 ){
        newComm_aux <- c(newComm[1:5], '...')
      }
      
      id_comm <<- showNotification(sprintf("A new commodity has appeared in the processed production table since last update. Commodity: %s",
                                           paste(newComm_aux, collapse = ", ")),duration = 0)
    }
    
    if(length(newScheda) > 0){
      
      newScheda_aux <- newScheda
      
      if( length(newScheda) > 5 ){
        newScheda_aux <- c(newScheda[1:5], '...')
      }
      
      id_comm <<- showNotification(sprintf("A new Scheda has appeared in the processed production table since last update. Commodity: %s",
                                           paste(newScheda_aux, collapse = ", ")),duration = 0)
    }
    
    if(length(newComm) == 0 & length(newScheda) == 0){
      id_comm <<- showNotification('No new commodity or scheda has appeared since last update.',duration = 0)
      
    }
    
  })
  
  # -- Get commodity and global production datasets ----
  observeEvent(input$btn_start_year, { 
    req(input$btn_country, input$btn_year, input$btn_start_year)
    commodity_label <- rv_data$commodity_label
    
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_years <- input$btn_start_year:input$btn_year
    # sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]

    #if(input$btn_commodity != ""){
      ## Get Global production
      # KeyGlobal <- DatasetKey(domain = "Fisheries", dataset = "fi_global_production", dimensions = list(
      #   geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country), # GetCodeList("Fisheries", "fi_global_production","geographicAreaM49_fi" )[,code]),
      #   fisheriesAsfis = Dimension(name = "fisheriesAsfis", keys = GetCodeList("Fisheries", "fi_global_production","fisheriesAsfis" )[,code]),
      #   fisheriesCatchArea = Dimension(name = "fisheriesCatchArea", keys = GetCodeList("Fisheries", "fi_global_production","fisheriesCatchArea" )[,code]),
      #   measuredElement = Dimension(name = "measuredElement", keys = c("FI_001")),
      #   timePointYears = Dimension(name = "timePointYears", keys =  as.character(sel_years) )))
      browser()
      # Now getting capture and aquaculture frome production and merging
    if(local){
      
      if(CheckDebug()){
        
        library(faoswsModules)
        SETTINGS = ReadSettings("sws1.yml")
        
        ## If you're not on the system, your settings will overwrite any others
        R_SWS_SHARE_PATH = SETTINGS[["share"]]
        
        ## Define where your certificates are stored
        SetClientFiles(SETTINGS[["certdir"]])
        
        ## Get session information from SWS. Token must be obtained from web interface
        GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                           token = 'b830f39c-253d-4d6f-9978-ff6f00d47765')#'27ded447-71ec-413b-bcd4-87669ac20c70')
      }
      
    } else {
      R_SWS_SHARE_PATH = "Z:"
      SetClientFiles("/srv/shiny-server/.R/PROD/")
      GetTestEnvironment(baseUrl = "https://sws.fao.org:8181",
                         token = "04fc0c00-a4f3-4640-bee6-49a906863095")
    }  
    
    
      withProgress(message = 'Loading Global Production data',
                   value = 0, {
                    
                     Sys.sleep(0.05)
                     incProgress(0.15)
                  
                     dim1aqua <- Dimension(name = "geographicAreaM49_fi", keys = sel_country) # GetCodeList("Fisheries", "aqua", "geographicAreaM49_fi")$code)
                     dim2aqua <- Dimension(name = "fisheriesAsfis", keys = GetCodeList("Fisheries", "aqua", "fisheriesAsfis")$code)
                     dim3aqua <- Dimension(name = "fisheriesProductionSource", keys = GetCodeList("Fisheries", "aqua", "fisheriesProductionSource")$code)
                     dim4aqua <- Dimension(name = "fisheriesCatchArea", keys = GetCodeList("Fisheries", "aqua", "fisheriesCatchArea")$code)
                     dim5aqua <- Dimension(name = "measuredElement", keys = "FI_001")
                     dim6aqua <- Dimension(name = "timePointYears", keys = as.character(sel_years)) # GetCodeList("Fisheries", "aqua", "timePointYears")$code) 
                     key1 <- DatasetKey(domain = "Fisheries", dataset = "aqua", dimensions = list(dim1aqua, dim2aqua, dim3aqua, dim4aqua, dim5aqua, dim6aqua))
                     aqua_quantity <- GetData(key1)
                     
                     Sys.sleep(0.05)
                     incProgress(0.35)
                     #[1] "geographicAreaM49_fi" "fisheriesAsfis"       "fisheriesCatchArea"   "measuredElement"     
                     #[5] "timePointYears"   
                     dim1capture <- Dimension(name = "geographicAreaM49_fi", keys = sel_country) #  GetCodeList("Fisheries", "capture", "geographicAreaM49_fi")$code)
                     dim2capture <- Dimension(name = "fisheriesAsfis", keys = GetCodeList("Fisheries", "capture", "fisheriesAsfis")$code)
                     dim3capture <- Dimension(name = "fisheriesCatchArea", keys = GetCodeList("Fisheries", "capture", "fisheriesCatchArea")$code)
                     dim4capture <- Dimension(name = "measuredElement", keys = "FI_001")
                     dim5capture <- Dimension(name = "timePointYears", keys = as.character(sel_years)) # GetCodeList("Fisheries", "aqua", "timePointYears")$code) 
                     key2 <- DatasetKey(domain = "Fisheries", dataset = "capture", dimensions = list(dim1capture, dim2capture, dim3capture, dim4capture, dim5capture))
                     cap_quantity <- GetData(key2)
                     
                     Sys.sleep(0.05)
                     incProgress(0.55)
                     
                     dim4capture = Dimension(name = "measuredElement", keys = "FI_002")
                     key2 <- DatasetKey(domain = "Fisheries", dataset = "capture", dimensions = list(dim1capture, dim2capture, dim3capture, dim4capture, dim5capture))
                     cap_numbers <- GetData(key2)
                     
                     aqua_quantity$flagCurrency <- NULL
                     aqua_quantity$fisheriesProductionSource <- NULL
                    
                     globalprod_new <- rbind (aqua_quantity, cap_quantity) 
                     globalprod_new <- rbind (globalprod_new, cap_numbers)
                     
                     globalprod_new <- globalprod_new %>% 
                       group_by(geographicAreaM49_fi,fisheriesAsfis,fisheriesCatchArea,measuredElement,timePointYears) %>% 
                       summarise(Flag=myAggregate(Value,flagObservationStatus), Quantity=sum(Value))
                     globalprod_new <- globalprod_new %>% ungroup()
                     
                     colnames(globalprod_new)[colnames(globalprod_new)=="Quantity"]  <- "Value"
                     colnames(globalprod_new)[colnames(globalprod_new)=="Flag"]  <- "flagObservationStatus"
                     globalprod_new$flagMethod <- "-"
                     
                     # for aggregated Values>0,status=N is no longer correct
                     globalprod_new$flagObservationStatus[which(globalprod_new$Value>0 & globalprod_new$flagObservationStatus=='N')] <- ''
                     Sys.sleep(0.05)
                     incProgress(0.75)
                     # order columns
                     globalprod_new <- globalprod_new[,c('geographicAreaM49_fi','fisheriesAsfis','fisheriesCatchArea','measuredElement','timePointYears','Value','flagObservationStatus','flagMethod') ]
                     
                     # remove grouping
                     globalProduction <- as.data.table(globalprod_new)
                     
                     # Convert flags into ordinal factor so that simple aggregation is possible
                     # The function aggregateObservationFlag is too slow so flag are transformed into factors
                     globalProduction$flagObservationStatus <- ifelse(is.na(globalProduction$flagObservationStatus), '', globalProduction$flagObservationStatus)
                     globalProduction$flagObservationStatus <- factor(globalProduction$flagObservationStatus, levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), ordered = TRUE)
                     
                     # Aggregate by fisheriesCatchArea
                     globalProduction <- globalProduction[, list(ValueAggr = sum(Value, na.rm = TRUE),
                                                                 flagObservationStatusAggr = max(flagObservationStatus),
                                                                 flagMethodAggr = "s"),
                                                          by=c("geographicAreaM49_fi",
                                                               "fisheriesAsfis",
                                                               "measuredElement",
                                                               "timePointYears")]
                     Sys.sleep(0.05)
                     incProgress(0.95)
                     setnames(globalProduction, 
                              c("ValueAggr", "flagObservationStatusAggr", "flagMethodAggr"),
                              c("Value", "flagObservationStatus", "flagMethod"))
                     
                     globalProduction[ , c("measuredElement"):=NULL]     
      })
    
      if(local){
        
        if(CheckDebug()){
          
          library(faoswsModules)
          SETTINGS = ReadSettings("sws.yml")
          
          ## If you're not on the system, your settings will overwrite any others
          R_SWS_SHARE_PATH = SETTINGS[["share"]]
          
          ## Define where your certificates are stored
          SetClientFiles(SETTINGS[["certdir"]])
          
          ## Get session information from SWS. Token must be obtained from web interface
          GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                             token = 'dd142b17-6b58-440c-8b94-d578c7500e50')#'27ded447-71ec-413b-bcd4-87669ac20c70')
        }
        
      } else {
        R_SWS_SHARE_PATH = "Z:"
        SetClientFiles("/srv/shiny-server/.R/QA/")
        GetTestEnvironment(baseUrl = "https://swsqa.aws.fao.org:8181",
                           token = "04fc0c00-a4f3-4640-bee6-49a906863095")
      }
      
      withProgress(message = 'Loading Commodity data',
                                value = 0, {
                                  ## Get export mapping table
      whereMap <- paste("geographic_area_m49_fi = '", sel_country, "' ", sep = "") 
      
      Sys.sleep(0.10)
      incProgress(0.15)
      
      map_prod_exp0 <- ReadDatatable('isscfc_mapping_export_approach', readOnly = FALSE, where = whereMap)
      map_prod_exp <- copy(map_prod_exp0)
      map_prod_exp <- setnames(map_prod_exp, 
                               old = c("geographic_area_m49_fi",
                                       "start_year", "end_year", "measured_item_isscfc",
                                       "measured_item_isscfc_exp", "selection", "type"), 
                               new = c("geographicAreaM49_fi", 
                                       "start_year", "end_year", "measuredItemISSCFC", 
                                       "measuredItemISSCFC_exp", "Selection", "type"))
      
      # Avoid duplicates
      setkey(map_prod_exp, geographicAreaM49_fi, type, start_year, end_year,
             measuredItemISSCFC, measuredItemISSCFC_exp, Selection)
      
      if(nrow(map_prod_exp[duplicated(map_prod_exp)])> 0 ) {
        message("Duplicates in the ISSCFC production-export mapping!")
      }
  
      ##Get Commodity Data
      
      onlyExport <- c('5910', '5912')
      # commodity2load <- as.vector(map_prod_exp[measuredItemISSCFC == sel_isscfc, ]$measuredItemISSCFC_exp)
      # commodity2load <- as.vector(map_prod_exp$measuredItemISSCFC_exp)
      Sys.sleep(0.10)
      incProgress(0.25)
      
      KeyComm <- DatasetKey(domain = "Fisheries Commodities", dataset = "commodities_total", dimensions = list(
        geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country), 
        measuredItemISSCFC = Dimension(name = "measuredItemISSCFC", keys =  GetCodeList("FisheriesCommodities", 
                                                                                        "commodities_total",
                                                                                        "measuredItemISSCFC")$code ),#commodity2load),
        measuredElement = Dimension(name = "measuredElement", keys = onlyExport),
        timePointYears = Dimension(name = "timePointYears", keys = as.character(sel_years) ))) 
      
                     
                     Sys.sleep(0.10)
                     incProgress(0.4)
                     6
                     commodityDB <- GetData(KeyComm)
                     Sys.sleep(0.75)
                     incProgress(0.95)
                   })
      
      commodityDB$flagObservationStatus <- factor(commodityDB$flagObservationStatus, levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), ordered = TRUE)
      
      
      commodityDB <- commodityDB[ , list(Value = sum(Value, na.rm = TRUE),
                                         measuredElement = '5910',
                                         flagObservationStatus = max(flagObservationStatus),
                                         flagMethod = "-"),
                                  by=c("geographicAreaM49_fi",
                                       "measuredItemISSCFC",
                                       "timePointYears")]
      
      # Save into the rv_data object so to recall it when needed
      rv_data$globalProduction <- globalProduction
      rv_data$commodityDB <- commodityDB
      
      rv_mappingtable$map_prod_exp0 <- map_prod_exp0
      rv_mappingtable$map_prod_exp <- map_prod_exp
      
    
      
      
      # }
  })
  
  
  # -- Export method approach ----
  
  #-- Get first tab with mapping
  isscfc_check_reac <- reactive({
    
    # all buttons required
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    if(is.null(input$btn_commodity) | is.null(input$btn_country) | 
       is.null(input$btn_year) | is.null(input$btn_start_year) |
       is.null(input$btn_commodity) ) return(NULL)
    
    # get needed data and tables
    commodity_label <- rv_data$commodity_label
    procprodimp <- rv_data$procprod_imp
    globalProduction <- rv_data$globalProduction
    commodityDB <- rv_data$commodityDB
    map_prod_exp <- rv_mappingtable$map_prod_exp

    validate(
      need(nrow(map_prod_exp) > 0,
           "Export approach not applicable. No available mapping for the chosen country.")
    )
    
    # parameters explicited from buttons
    sel_years <- input$btn_start_year:input$btn_year
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]
    
    ## Checks
    # Select export commodities linked to the selected commodity to check if there are export data available 
    map_prod_exp_mod <- copy(map_prod_exp)
    map_prod_exp_mod <- map_prod_exp_mod[ end_year == 'LAST', end_year := '9999']
    commodities2check <- map_prod_exp_mod[ start_year <= input$btn_year & input$btn_year < end_year &
                                         measuredItemISSCFC %in% sel_isscfc, ]
    
    commodities2check <-  commodities2check[start_year == max(as.numeric(start_year))]$measuredItemISSCFC_exp
    
    # Filter commodity db chunk needed
    commodityDB_filtered <- commodityDB[geographicAreaM49_fi == sel_country & 
                                          measuredElement == "5910" &
                                          measuredItemISSCFC %in% commodities2check, ]  # &
                                        #  !flagObservationStatus %in% c('O', 'M', 'Q'), ]
    
    # Commodity DB with needed data change dimension name so not confunded with production codes
    setnames(commodityDB_filtered, old = "measuredItemISSCFC", new = "measuredItemISSCFC_exp")
    
    # Years available among those selected 
    avail_years <- unique(commodityDB_filtered$timePointYears)
    
    # Processed production chunk needed
    procprod_filtered <- procprodimp[geographicAreaM49_fi %in% sel_country & 
                                       timePointYears %in% sel_years &
                                       Scheda %in% sel_commodity, ]
    
    # Mapping chunk needed
    map_prod_exp_filtered <- map_prod_exp[start_year <= input$btn_year & 
                                            input$btn_year < end_year & 
                                            measuredItemISSCFC %in% sel_isscfc, ]
    
    # Delete unnecessary columns
    map_prod_exp_filtered[ , c('__id', '__ts') := NULL]
    
    # Create merged table with production data and mapping
    prod_data_mapping_tab <- merge(procprod_filtered, map_prod_exp_filtered, 
                                   by= c("geographicAreaM49_fi", "measuredItemISSCFC"),
                                   all.x = TRUE,
                                   allow.cartesian = TRUE)
    
    #  Create merged table with export and production data according to the last available mapping
    prod_exp_data_tab <- merge(prod_data_mapping_tab, commodityDB_filtered, 
                               by= c("geographicAreaM49_fi", "timePointYears", "measuredItemISSCFC_exp"),
                               suffixes = c("_prod", "_exp"),all.x = TRUE, allow.cartesian = TRUE)
    
    #  prod_exp_data_tab <- prod_exp_data_tab[ , measuredElement_prod := NULL ]
    
    # Select imputed year values and columns to display
    imputed_year_data <- prod_exp_data_tab[timePointYears == input$btn_year,
                                           .(Selection, timePointYears,
                                             type, Scheda,
                                             nationaldescription,
                                             remarks,
                                             measuredItemISSCFC,
                                             measuredItemISSCFC_exp,
                                             Value_exp, 
                                             flagObservationStatus_exp, 
                                             flagMethod_exp)]
    
    # Showing the corresponding export commodities names
    
    imputed_year_data <- merge(imputed_year_data, map_isscfc, 
                               by.x = "measuredItemISSCFC_exp",
                               by.y = "code", all.x = TRUE)
    
    # setkey(imputed_year_data)
    # imputed_year_data <- unique(imputed_year_data)
    
    setcolorder(imputed_year_data, c("Selection", "type", "description", 
                                     "measuredItemISSCFC_exp", "Value_exp", 
                                     "flagObservationStatus_exp", 
                                     "flagMethod_exp", "timePointYears",
                                     "nationaldescription",
                                     "remarks", "Scheda", 
                                     "measuredItemISSCFC"
    ))
    
    if(nrow(map_prod_exp) == 0| length(avail_years) == 0| any(!sel_years %in% avail_years)){
    imputed_year_data <- NA
    prod_exp_data_tab <- NA
    checkPassed$Exp <- 'No'
    } else {
      checkPassed$Exp <- 'Yes'
    }
    
    out_exp <- list(DF_display = imputed_year_data, DF_full = prod_exp_data_tab)
    
    validate(
      need(length(avail_years) > 0,
           "Export approach not applicable. No available export value for the chosen input combination.")
    )
    
    validate(
      need(all(sel_years %in% avail_years),
           sprintf("Export data for the time range and commodity selected are not available. Years available: %s",
                   ifelse(length(avail_years)>0, paste(avail_years, collapse = ", "),"None")))
    )
    
    return(out_exp)
  })
  
  #-- Export selection table
  
  output$isscfc_check_data <- renderRHandsontable({
    data_out <- isscfc_check_reac()
    rhandsontable(data_out$DF_display, rowHeaders = NULL, width = 'auto', height = 'auto') 
  })
  
  #-- Ratio button
  output$ratio_choice <- renderUI({
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    if(is.null(input$btn_commodity)) return(NULL)
    
    # Button labels
    ratioExp_names <- paste(c('Original data', 'Average ratio', 'Manual ratio'))
    
    # Button creation
    btn_ratioExp <- radioGroupButtons(
      inputId = "btn_ratioExp",
      individual = FALSE,
      label = "Ratio choice",
      choiceNames = ratioExp_names,
      choiceValues = 1:3,
      status = "info",
      justified = FALSE,
      direction = "vertical",
      checkIcon = list(
        yes = icon("ok", lib = "glyphicon"),
        no = icon("remove", lib = "glyphicon"))
    )
    
    btn_ratioExp
  })
  
  #-- Manual button
  output$out_btn_manual_exp <- renderUI({
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    if(is.null(input$btn_commodity)) return(NULL)
    
    # Button for manual ratio input
    numericInput(inputId = 'btn_manual_exp', label = 'Manual ratio', value = NA)
    
  })
  
  #-- Create table with potentially imputable value ----
  output$table_exp_estimates <- renderRHandsontable({
    
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    
    commodity_label <- rv_data$commodity_label
    procprodimp <- rv_data$procprod_imp
    globalProduction <- rv_data$globalProduction
    commodityDB <- rv_data$commodityDB
    
    if(is.null(input$isscfc_check_data)) return(NULL)
    
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_years <- input$btn_start_year:input$btn_year
    sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]
    
    # Displayed tab reactive with all changes
    tab_updated <- rhandsontable::hot_to_r(input$isscfc_check_data)
    # ISSCFC description code no more needed
    tab_updated <- tab_updated[ , description := NULL]
    tab_updated$measuredItemISSCFC_exp <- gsub(' ', '', tab_updated$measuredItemISSCFC_exp)
    tab_updated$measuredItemISSCFC <- gsub(' ', '', tab_updated$measuredItemISSCFC)

    # Tables as from 'out_exp'
    data_out <- isscfc_check_reac()
    # Complete table for imputed year
    data4update <- data_out$DF_full[timePointYears == input$btn_year, ]
    data4update$measuredItemISSCFC_exp <- gsub(' ', '', data4update$measuredItemISSCFC_exp)
    data4update$measuredItemISSCFC <- gsub(' ', '', data4update$measuredItemISSCFC)
    
    data_prev <- data_out$DF_full[timePointYears != input$btn_year, ]
    data_prev$measuredItemISSCFC_exp <- gsub(' ', '', data_prev$measuredItemISSCFC_exp)
    data_prev$measuredItemISSCFC <- gsub(' ', '', data_prev$measuredItemISSCFC)
    
    ## check the commodities inserted in the new table
    
    # If unknown commodity inserted in production codes
    validate(
      need (all(tab_updated$measuredItemISSCFC %in% unlist(map_isscfc[, .(code)])),
            paste0("The commodity inserted in the measuredItemISSCFC column is not in the commodity list. ",
                   tab_updated$measuredItemISSCFC[ which(!tab_updated$measuredItemISSCFC %in% unlist(map_isscfc[, .(code)]))],
                   " is not a valid code." )
      )
    )
    
    # If unknown commodity inserted in export codes
    validate(
      need (all(tab_updated$measuredItemISSCFC_exp %in% unlist(map_isscfc[, .(code)])),
            paste0("The commodity inserted in the measuredItemISSCFC_exp column is not in the commodity list. ",
                   tab_updated$measuredItemISSCFC_exp[ which(!tab_updated$measuredItemISSCFC_exp %in% unlist(map_isscfc[, .(code)]))],
                   " is not a valid code." )
      )
    )
    
    
    # merge with all data table
    tab_comparison <- merge(tab_updated, data4update[ , .(timePointYears, Scheda, measuredItemISSCFC,
                                                          measuredElement_exp, measuredItemISSCFC_exp,
                                                          geographicAreaM49_fi, measuredElement_prod,
                                                          nationalquantity, nationalquantityunit, id_isscfc, nationalcode,
                                                          measureditemnational, approach, Value_prod, flagObservationStatus_prod,
                                                          flagMethod_prod, start_year, end_year)], 
                            by = c("timePointYears", "Scheda", "measuredItemISSCFC", "measuredItemISSCFC_exp"),
                            all.x = TRUE, suffixes = c('_new', '_old'), allow.cartesian = TRUE)
    # Make sure all standard dimensions are not NAs
    tab_comparison[ , c("timePointYears", "Scheda", "measuredItemISSCFC",
                        "geographicAreaM49_fi", "measuredElement_prod", 
                        "flagObservationStatus_prod",
                        "flagMethod_prod") := list(ifelse(is.na(timePointYears),input$btn_year, timePointYears), 
                                                   ifelse(is.na(Scheda),sel_commodity, Scheda), 
                                                   ifelse(is.na(measuredItemISSCFC),sel_isscfc, measuredItemISSCFC), 
                                                   ifelse(is.na(geographicAreaM49_fi),sel_country, geographicAreaM49_fi), 
                                                   ifelse(is.na(measuredElement_prod),'5510', measuredElement_prod),
                                                   ifelse(is.na(flagObservationStatus_prod),unique(data4update$flagObservationStatus_prod), flagObservationStatus_prod),
                                                   ifelse(is.na(flagMethod_prod),unique(data4update$flagMethod_prod), flagMethod_prod)) ]
    
    setkey(tab_comparison)
    tab_comparison <- tab_comparison[!duplicated(tab_comparison)]
    # put together older data and new ones
    tab_updated_complete <- rbind(data_prev, tab_comparison)
    tab_updated_complete$Value_prod <- as.numeric(tab_updated_complete$Value_prod)
    
    # Manual ratio if inserted
    manual_ratio_exp_input <- ifelse(!is.null(input$btn_manual_exp) & input$btn_ratioExp == 3, input$btn_manual_exp, NA)
    
    # Value to consider
    missing_data <- ifelse(input$btn_missing == "No" & input$btn_ratioExp == 1,
                           as.numeric(procprodimp[geographicAreaM49_fi == sel_country & 
                                                    timePointYears == input$btn_year & 
                                                    measuredElement == "5510" &
                                                    Scheda == sel_commodity, ]$Value), NA)
    
    export_out <- export_imputation(datatab = tab_updated_complete[Scheda == sel_commodity & 
                                                                     measuredItemISSCFC == sel_isscfc & 
                                                                     !is.na(Value_exp), ], 
                                    sel_year = input$btn_year,
                                    manual_ratio_exp_input = manual_ratio_exp_input,
                                    missing_data = missing_data)
    
    colSelexp <- which(colnames(export_out) == input$btn_year) -1
    
    rhandsontable(export_out, rowHeaders = NULL, width = 'auto', height = 'auto',
                  customBorders = list(list(
                    range = list(from = list(row = 0, col = colSelexp),
                                 to = list(row = 0, col = colSelexp)),
                    top = list(width = 3, color = "red"),
                    left = list(width = 2, color = "red"),
                    bottom = list(width = 2, color = "red"),
                    right = list(width = 2, color = "red"))))
    
  })
  
  #-- Export approach plot ----
  output$gg_exp_estimates <- renderPlot({
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    
    commodity_label <- rv_data$commodity_label
    procprodimp <- rv_data$procprod_imp
    globalProduction <- rv_data$globalProduction
    commodityDB <- rv_data$commodityDB
    
    if(is.null(input$isscfc_check_data)) return(NULL)
    
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_years <- input$btn_start_year:input$btn_year
    sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]
    
    # Displayed tab reactive with all changes
    tab_updated <- rhandsontable::hot_to_r(input$isscfc_check_data)
    # ISSCFC description code no more needed
    tab_updated <- tab_updated[ , description := NULL]
    
    # Tables as from 'out_exp'
    data_out <- isscfc_check_reac()
    # Complete table for imputed year
    data4update <- data_out$DF_full[timePointYears == input$btn_year, ]
    data_prev <- data_out$DF_full[timePointYears != input$btn_year, ]
    ## check the commodities inserted in the new table
    
    # If unknown commodity inserted in production codes
    validate(
      need (all(tab_updated$measuredItemISSCFC %in% unlist(map_isscfc[, .(code)])),
            paste0("The commodity inserted in the measuredItemISSCFC column is not in the commodity list. ",
                   tab_updated$measuredItemISSCFC[ which(!tab_updated$measuredItemISSCFC %in% unlist(map_isscfc[, .(code)]))],
                   " is not a valid code." )
      )
    )
    
    # If unknown commodity inserted in export codes
    validate(
      need (all(tab_updated$measuredItemISSCFC_exp %in% unlist(map_isscfc[, .(code)])),
            paste0("The commodity inserted in the measuredItemISSCFC_exp column is not in the commodity list. ",
                   tab_updated$measuredItemISSCFC_exp[ which(!tab_updated$measuredItemISSCFC_exp %in% unlist(map_isscfc[, .(code)]))],
                   " is not a valid code." )
      )
    )
    
    validate(
      need (all(!is.na(tab_updated[ , .(type, measuredItemISSCFC_exp, timePointYears, measuredItemISSCFC)])),
            "One of the key mapping information is missing (type, measuredItemISSCFC_exp, timePointYears, measuredItemISSCFC)."
      )
    )
    
    # # Fill missing columns and update start and end year. Start year is the year of imputation and end_year is last by default
    # tab_updated <- tab_updated[, c("start_year", "end_year"):= list(input$btn_year, 'LAST')]
    
    # merge with all data table
    tab_comparison <- merge(tab_updated, data4update[ , .(timePointYears, Scheda, measuredItemISSCFC,
                                                          measuredElement_exp, measuredItemISSCFC_exp,
                                                          geographicAreaM49_fi, measuredElement_prod,
                                                          nationalquantity, nationalquantityunit, id_isscfc, nationalcode,
                                                          measureditemnational, approach, Value_prod, flagObservationStatus_prod,
                                                          flagMethod_prod, start_year, end_year)], 
                            by = c("timePointYears", "Scheda", "measuredItemISSCFC", "measuredItemISSCFC_exp"),
                            all.x = TRUE, suffixes = c('_new', '_old'), allow.cartesian = TRUE)
    # Make sure all standard dimensions are not NAs
    tab_comparison[ , c("timePointYears", "Scheda", "measuredItemISSCFC",
                        "geographicAreaM49_fi", "measuredElement_prod", 
                        "flagObservationStatus_prod",
                        "flagMethod_prod") := list(ifelse(is.na(timePointYears),input$btn_year, timePointYears), 
                                                   ifelse(is.na(Scheda),sel_commodity, Scheda), 
                                                   ifelse(is.na(measuredItemISSCFC),sel_isscfc, measuredItemISSCFC), 
                                                   ifelse(is.na(geographicAreaM49_fi),sel_country, geographicAreaM49_fi), 
                                                   ifelse(is.na(measuredElement_prod),'5510', measuredElement_prod),
                                                   ifelse(is.na(flagObservationStatus_prod),unique(data4update$flagObservationStatus_prod), flagObservationStatus_prod),
                                                   ifelse(is.na(flagMethod_prod),unique(data4update$flagMethod_prod), flagMethod_prod)) ]
    
    setkey(tab_comparison)
    tab_comparison <- tab_comparison[!duplicated(tab_comparison)]
    # put together older data and new ones
    tab_updated_complete <- rbind(data_prev, tab_comparison)
    tab_updated_complete$Value_prod <- as.numeric(tab_updated_complete$Value_prod)
    
    # Manual ratio if inserted
    manual_ratio_exp_input <- ifelse(!is.null(input$btn_manual_exp) & input$btn_ratioExp == 3, input$btn_manual_exp, NA)
    
    # Value to consider
    missing_data <- ifelse(input$btn_missing == "No" & input$btn_ratioExp == 1,
                           as.numeric(procprodimp[geographicAreaM49_fi == sel_country & 
                                                    timePointYears == input$btn_year & 
                                                    measuredElement == "5510" &
                                                    Scheda == sel_commodity, ]$Value), NA)
    
    export_out <- export_imputation(datatab = tab_updated_complete[Scheda == sel_commodity & 
                                                                     measuredItemISSCFC == sel_isscfc & 
                                                                     !is.na(Value_exp), ], 
                                    sel_year = input$btn_year,
                                    manual_ratio_exp_input = manual_ratio_exp_input,
                                    missing_data = missing_data)
    
    # Prepare table for plot
    export_out2 <- export_out[, -which(names(export_out) == "Flag"), with = FALSE]
    export_out_aux <- melt(export_out2, 1, variable.name = 'Year')

    suppressWarnings(export_out_aux[, Year := as.numeric(as.character(Year))])
    suppressWarnings(export_out_aux[, value := as.numeric(as.character(value))])
    
    ggplot(data = export_out_aux[Stats != 'Ratio'], aes(x = Year, y = value)) +
      geom_line(aes(group = Stats, color = Stats), size = 1) +
      geom_point(aes(color = Stats), size = 2) +
      scale_color_manual(values=c("blue","red")) +
      labs(y = 'Quantity (in tonnes)', color = '', title = 'Export approach imputation') +
      theme_minimal() +
      theme(legend.position = 'bottom')
    
  })
  
  # -- Check export mapping ----
  observeEvent(input$check_consistency_exp, {
    # If there's currently a notification, don't add another
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    if(is.null(input$isscfc_check_data)) return(NULL)
    # get needed data and tables
    commodity_label <- rv_data$commodity_label
    procprodimp <- rv_data$procprod_imp
    globalProduction <- rv_data$globalProduction
    commodityDB <- rv_data$commodityDB
    map_prod_exp <- rv_mappingtable$map_prod_exp
    
    # parameters explicited from buttons
    sel_years <- input$btn_start_year:input$btn_year
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]
    
    # Check if the mapping table has been update after the current chosen imputation year.
    # i.e., the user want to impute 2015 data but there is already a mapping for the same commodity starting from 2016 
    
    # Check the starting year of the last commodity chosen
    startYearLastMapping <- unique(map_prod_exp[geographicAreaM49_fi == sel_country &
                                                  measuredItemISSCFC == sel_isscfc &
                                                  end_year == "LAST", ]$start_year)
    if(length(startYearLastMapping) >1) {
      id_exp <<- showNotification("INCONSISTENCY! The chosen commodity has two start years for the same end year (LAST). Please check in the SWS mapping datatable.",
                                  duration = 0)
    }
    
    if(as.numeric(input$btn_year) < as.numeric(startYearLastMapping)){
      
      id_exp <<- showNotification("The mapping has already changed since the chosen imputation year.
                                  Please update the mapping manually coherently to avoid automatic errors.",
                                  duration = 0)
    }
    
    
    # Check if an exported commodity has been allocated twice so there is a double allocation of the same quantity
    # the check is made on geaographic area, same end year, same export commodity and same Selection (only TRUE-TRUE)
    # If the commodity has not been allocated at all there is a different check
    onlytrue <- map_prod_exp[Selection == TRUE, ]
    if(nrow(onlytrue[duplicated(setkey(onlytrue, geographicAreaM49_fi,
                                       end_year,
                                       measuredItemISSCFC_exp,
                                       Selection)),]) > 0){
      
      list <- unique(onlytrue[duplicated(setkey(onlytrue, geographicAreaM49_fi,
                                                end_year,
                                                measuredItemISSCFC_exp,
                                                Selection)),]$measuredItemISSCFC_exp)
      if(length(list) > 5){
        list <- c(list[1:5], "...")
      }
      
      id_exp <<- showNotification( sprintf("The mapping implies a duplication. Please check commodity(ies): %s.",
                                           paste(list, collapse = ", ")),
                                   duration = 0)
    }
    
    
    # Check for duplicates and mapping consistency:
    
    # Check if new commodities and/or new standing alone commodities
    
    # Comparing if there are commodities in the commodity db that are not in the mapping among production codes
    # i.e. codes are either not in the commodity list or they are in the export list and included in the mapping
    # linked to a different commodity
    
    # Commodities in the production part of the mapping table
    mappingCommProd <- unique(map_prod_exp[geographicAreaM49_fi == sel_country , .(measuredItemISSCFC)])
    mappingCommExp <- unique(map_prod_exp[geographicAreaM49_fi == sel_country , .(measuredItemISSCFC_exp)])
    
    # commodities in the procprod table and commodity DB
    ppComm <- unique(procprodimp[geographicAreaM49_fi == sel_country, .(measuredItemISSCFC)])
    
    # New production code, i.e. commodities in the procprod table but not in the mapping table (production side)
    newProdCode <- ppComm[!measuredItemISSCFC %in% mappingCommProd$measuredItemISSCFC]
    
    # Check if any commodity is not in the mapping table but it is in the proc prod table
    if(nrow(newProdCode) > 0){
      
      list1 <- newProdCode$measuredItemISSCFC
      
      if(length(list1) > 5){
        list1 <- c(list1[1:5], "...")
      }
      
      id_exp <<- showNotification(paste("The following items have production figures but are not included in the export approach mapping table: ",
                                        paste(list1, collapse = ", ")), duration = 0)
    }
    
    # Check if a commodity for which there is production has export linked to other commodities
    if(nrow(newProdCode) > 0 & nrow(newProdCode[measuredItemISSCFC %in% mappingCommExp$measuredItemISSCFC_exp]) > 0){
      
      list2 <- unlist(newProdCode[measuredItemISSCFC %in% mappingCommExp$measuredItemISSCFC_exp, .(measuredItemISSCFC)])
      
      if(length(list2) > 5){
        list2 <- c(list2[1:5], "...")
      }
      
      id_exp <<- showNotification(paste("The following commodity(ies) have export quantities linked to another commodity code in the mapping table: ",
                                        paste(list2, collapse = ", ")), duration = 0)
    }
    
    
    # If a commodity has been forgotten (i.e. no TRUE selection is associated for the last period)
    # Take list of unique commodities (export side) + selection 
    commoditiesIncluded <- unique(map_prod_exp[geographicAreaM49_fi == sel_country &
                                                 end_year =="LAST", .(measuredItemISSCFC_exp, Selection)])
    # Take list of commodities export side
    commodities <- unique(commoditiesIncluded[ , .(measuredItemISSCFC_exp)])
    
    # If the number of TRUE selection is not equal to the number of commodities (export side) it means there is 
    # at least a commodity that has not been included
    if(nrow(commoditiesIncluded[Selection == TRUE]) != nrow(commodities)) {
      
      list3 <- commodities[ !measuredItemISSCFC_exp %in% commoditiesIncluded[Selection == TRUE]$measuredItemISSCFC_exp, ]$measuredItemISSCFC_exp
      
      if(length(list3) > 5){
        list3 <- c(list3[1:5], "...")
      }
      
      id_exp <<- showNotification(sprintf("The following commodity(ies) have not been included in the mapping: %s.",
                                          paste0(list3, collapse = ", ") ), duration = 0)
      
    }
    
    # Check mapping period: 
    # If there is a duplication (i.e. a commodity in the export list in same country has been selected (TRUE), or unselected (FALSE), twice with the same end year)
    # It could happen that years overlap, i.e. duplicates have for example a row valid for period 2000-2010 and the other with period 2005-LAST, 
    # in this case there are 6 years of overlapping but such an error would come from a previous inconsistency it is therefore not accounted for here
    
    mappingperiods <- unique(map_prod_exp[ , .(measuredItemISSCFC, start_year, end_year)])
    ff <- function(x){x$start_year:x$end_year}
    
    overlap_list <- list()
    
    for(i in 1:length(mappingCommProd)){
      
      if(nrow(mappingperiods[measuredItemISSCFC== mappingCommProd[i], ]) > 1){
        start_years <- as.numeric(mappingperiods$start_year)
        end_years <- ifelse(mappingperiods$end_year == "LAST", max(as.numeric(procprodimp$timePointYears)), mappingperiods$end_year)
        
        chunk <- mappingperiods[measuredItemISSCFC== mappingCommProd[i], ]
        chunk$start_year <- as.numeric(chunk$start_year)
        chunk$end_year <- ifelse(chunk$end_year == "LAST", max(as.numeric(procprodimp$timePointYears)+1), chunk$end_year)
        aux_list <- list()
        
        for(j in 1:nrow(chunk)){
          aux_list[[j]] <- ff(chunk[j,])
        }
        
        int_list <- list()
        
        for(k in 2:nrow(chunk)){ int_list[[k]] <- (intersect(aux_list[[k-1]], aux_list[[k]]))}
        
        overlap_vec <- unlist(int_list)
        
        if(length(overlap_vec) > 0){
          overlap_list[[1]] <- mappingCommProd[i]
        }
      }  
    }
    
    comm2signal <- unlist(overlap_list)  
    if(length(comm2signal) > 0){
      
      comm2signal_aux <- comm2signal
      
      if(length(comm2signal) > 5){
        comm2signal_aux <- c(comm2signal[1:5], "...")
      }
      
      id_exp <<- showNotification(paste("This current mapping implies an inconsistent allocation.
                                          Two validity periods overlap for commodity(ies): ",
                                        paste(comm2signal_aux, collapse = " ")), duration = 0)
      
    }
    
    # If none of the above is true:  
    if(length(comm2signal) == 0 & nrow(commoditiesIncluded[Selection == TRUE]) == nrow(commodities) &
       nrow(newProdCode) == 0 & as.numeric(input$btn_year) >= as.numeric(startYearLastMapping) &
       nrow(onlytrue[duplicated(setkey(onlytrue, geographicAreaM49_fi,
                                       end_year,
                                       measuredItemISSCFC_exp,
                                       Selection)),]) == 0){
      id_exp <<- showNotification("The current mapping is consistent.", duration = 0)
    }
    
  })
  
  # -- Primary production approach ----
  
  # Get primary production mapping table 
  
  observeEvent(input$btn_country, {
    
    sel_country <- country_input[country_input$label == input$btn_country, code]
    whereMap <- paste("geographic_area_m49_fi = '", sel_country, "' ", sep = "") 
   
    # Get primary production mapping
    map_prod_prod0 <-  ReadDatatable('isscfc_mapping_prod_approach', readOnly = FALSE, where = whereMap)
    map_prod_prod <- copy(map_prod_prod0)
    map_prod_prod <- setnames(map_prod_prod, 
                              old = c("geographic_area_m49_fi", "measured_item_isscfc", 
                                      "asfis", "ratio", "selection"), 
                              new = c("geographicAreaM49_fi", "measuredItemISSCFC", 
                                      "fisheriesAsfis", "Ratio", "Selection"))
    
    map_prod_prod$Ratio <- as.numeric(map_prod_prod$Ratio)
    
    # Avoid duplicates
    setkey(map_prod_prod, geographicAreaM49_fi, type, start_year, end_year, 
           measuredItemISSCFC, isscaap, fisheriesAsfis, Ratio, Selection)
    
    if(nrow(map_prod_prod[duplicated(map_prod_prod)])> 0 ) {
      message("Duplicates in the ISSCFC primary production mapping!")
    }
    
    rv_mappingtable$map_prod_prod0 <- map_prod_prod0
    rv_mappingtable$map_prod_prod <- map_prod_prod
    
  })
  
  
  #-- Get first tab with mapping
  
  asfis_check_reac <- reactive({
    
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)

    commodity_label <- rv_data$commodity_label
    procprodimp <- rv_data$procprod_imp
    globalProduction <- rv_data$globalProduction
    commodityDB <- rv_data$commodityDB
  
    map_prod_prod <- rv_mappingtable$map_prod_prod
    
    # Buttons
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_years <- input$btn_start_year:input$btn_year
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]
    
    #
    
    avail_years <- unique(globalProduction$timePointYears)

    # As calculations are slow, a progress bar is needed
    withProgress(message = 'Calculation in progress',
                 value = 0, {
                   
                   Sys.sleep(0.25)
                   incProgress(0.25)
                   
                   mapforfunction <- copy(map_prod_prod)
                   mapforfunction <- mapforfunction[end_year == "LAST", end_year := gsub('-.*', '', Sys.Date())]
                   
                   # The output of this function consists of a list of two items 
                   # (associated to cdb_prod_isscaap and cdb_cfc_asfis_full in the following lines)
                   data_out_prod <- primaryprod_imputation1(commodityDB = commodityDB[measuredItemISSCFC %in% sel_isscfc &
                                                                                        !flagObservationStatus %in% c('O', 'M', 'Q')], 
                                                            procprod = procprodimp[Scheda %in% sel_commodity],
                                                            globalProduction = globalProduction[geographicAreaM49_fi %in% sel_country &
                                                                                                  !flagObservationStatus %in% c('O', 'M', 'Q')], # Dataset 
                                                            sel_year = input$btn_year,
                                                            map_asfis = map_asfis,
                                                            mappingTable  = mapforfunction[end_year >= input$btn_start_year &
                                                                                          end_year >= input$btn_year & measuredItemISSCFC %in% sel_isscfc ])
                   
                   
                   Sys.sleep(0.75)
                   incProgress(0.75)
                   
                   # table from merge of procprodimp and mapping
                   cdb_prod_isscaap <- data_out_prod$cdb_isscaap
                   
                   # table where commodity processed production and associated primary production are merged for imputation year
                   cdb_cfc_asfis_full <- data_out_prod$cdb_ASFIS 
                   
                   # select only rows where primary production is available and columns to display
                   # Show only species for which there is production, i.e. different from NA and 0
                   cdb_cfc_asfis <- cdb_cfc_asfis_full[ , .(Selection, type, Ratio,
                                                            isscaap, description,
                                                            fisheriesAsfis, Value_PrimaryProd, 
                                                            flagObservationStatus_PrimaryProd,
                                                            flagMethod_PrimaryProd, timePointYears,
                                                            nationaldescription, remarks,
                                                            Scheda, measuredItemISSCFC)]
                   
                   # If no primary production is available for the chosen commodity an error is displayed
                   
                   # Add imputation year and select needed columns
                   cdb_cfc_asfis <- cdb_cfc_asfis[timePointYears == input$btn_year]
                   
                   setkey(cdb_cfc_asfis)
                   cdb_cfc_asfis <- unique(cdb_cfc_asfis)
                  
                   if(nrow(map_prod_prod) == 0| nrow(cdb_cfc_asfis) == 0| any(!sel_years %in% avail_years)){
                     cdb_prod_isscaap <- NA
                     cdb_cfc_asfis_full <- NA
                     cdb_cfc_asfis <- NA
                     checkPassed$Prim <- 'No'
                   } else {
                     checkPassed$Prim <- 'Yes'
                   }
                   
                   out_prod <- list(prod_isscaap = cdb_prod_isscaap, # processed production for selected years with mapping
                                    prod_asfis_full = cdb_cfc_asfis_full, # full table with processed and primary production
                                    prod_asfis_display = cdb_cfc_asfis) # table to display in the shiny
                   
                   
                   validate(
                     need(nrow(map_prod_prod) > 0,
                          "Primary production approach not applicable. No available mapping for the chosen country.")
                   )
                   
                   validate(
                     need(all(sel_years %in% avail_years),
                          sprintf("Primary production data for the time range and commodity selected are not available. Years available: %s",
                                  ifelse(length(avail_years)>0, paste(avail_years, collapse = ", "),"None")))
                   )
                   
                   validate(
                     need( nrow(cdb_cfc_asfis) > 0,
                           paste0("Primary production data for the country, time range and commodity 
                                  selected are not available. Please select different inputs or 
                                  ignore this approach."))
                   )
                   
                   Sys.sleep(0.75)
                   incProgress(0.95)
                   
                 })
    
    return(out_prod)
    
  })
  
  
  ##-- Primary production selection table ----
  output$asfis_check_data <- renderRHandsontable({
    data_out_prod <- asfis_check_reac()
    rhandsontable(data_out_prod$prod_asfis_display, rowHeaders = NULL, width = 'auto', height = 'auto') 
  })
  
  # Ratio button
  output$ratio_choice_prod <- renderUI({
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    
    
    ratioProd_names <- paste(c('Original data', 'Average ratio', 'Manual ratio'))
    
    # Button creation
    
    btn_ratioProd <- radioGroupButtons(
      inputId = "btn_ratioProd",
      individual = FALSE,
      label = "Ratio choice",
      choiceNames = ratioProd_names,
      choiceValues = 1:3,
      status = "info",
      justified = FALSE,
      direction = "vertical",
      checkIcon = list(
        yes = icon("ok",
                   lib = "glyphicon"),
        no = icon("remove",
                  lib = "glyphicon"))
    )
    
    btn_ratioProd
    
  })
  
  # Manual button
  output$out_btn_manual_prod <- renderUI({
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    if(is.null(input$btn_commodity)) return(NULL)
    numericInput(inputId = 'btn_manual_prod', label = 'Manual ratio', value = NA)
  })
  
  # Create table with potentially imputable value
  output$table_prod_estimates <- renderRHandsontable({
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    if(is.null(input$asfis_check_data)) return(NULL)
  
    commodity_label <- rv_data$commodity_label
    procprodimp <- rv_data$procprod_imp
    globalProduction <- rv_data$globalProduction
    commodityDB <- rv_data$commodityDB
    
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_years <- input$btn_start_year:input$btn_year
    
    tab_updated_prod <- rhandsontable::hot_to_r(input$asfis_check_data)
    
    #-- CHECKS ----
    # Check if the commodity inserted is in the commodity list
    validate(
      need (all(tab_updated_prod$measuredItemISSCFC %in% unlist(map_isscfc[ , .(code)])),
            paste("One of the commodity inserted in the measuredItemISSCFC column is not in the commodity list. ",
                  paste(tab_updated_prod$measuredItemISSCFC[ which(!tab_updated_prod$measuredItemISSCFC %in% unlist(map_isscfc[, .(code)]))], collapse = ","),
                  " is not a valid code." )
      )
    )
    
    # Check if the species inserted is in the Asfis list
    validate(
      need (all(tab_updated_prod$fisheriesAsfis %in% c('all', map_asfis$fisheriesAsfis)),
            paste("One of the Asfis code inserted in the fisheriesAsfis column is not in the Asfis list. ",
                  paste(tab_updated_prod$fisheriesAsfis[ which(!tab_updated_prod$fisheriesAsfis %in% c('all', map_asfis$fisheriesAsfis) )], collapse = ","),
                  " is not a valid code." )
      )
    )
    
    validate(
      need(all(!is.na(tab_updated_prod$isscaap)),
           paste('Please enter isscaap group for species ', paste(tab_updated_prod[is.na(isscaap), ]$fisheriesAsfis, sep = ', ') ) )
      
    )
    
    result <- c()
    tab2check <- tab_updated_prod[fisheriesAsfis != 'all', ]
    for(i in 1:nrow(tab2check)){
      species <- tab2check[i, ]$fisheriesAsfis 
      group <- map_asfis[fisheriesAsfis == species ]$isscaap
      result[i] <- tab2check[i, ]$isscaap == group
    }
    
    validate(
      need(all(result == TRUE),
           paste('Please assign species to the right ISSCAAP group(s).')
           
      )
    )
    
    #-- After first checks ----
    
    data_out_prod <- asfis_check_reac()
    prod_asfis_full <- data_out_prod$prod_asfis_full
    prod_isscaap <-  data_out_prod$prod_isscaap
    
    tab_updated_prod <- tab_updated_prod[, c("geographicAreaM49_fi", "timePointYears", "Scheda",
                                             "Value_ProcessedProd", "flagObservationStatus_ProcessedProd",
                                             "flagMethod_ProcessedProd", "measuredElement"):= list(sel_country, input$btn_year, sel_commodity,
                                                                                                   unique(prod_asfis_full[timePointYears == input$btn_year,]$Value_ProcessedProd),
                                                                                                   unique(prod_asfis_full[timePointYears == input$btn_year,]$flagObservationStatus_ProcessedProd),
                                                                                                   unique(prod_asfis_full[timePointYears == input$btn_year,]$flagMethod_ProcessedProd),
                                                                                                   unique(prod_asfis_full[timePointYears == input$btn_year,]$measuredElement))]
    
    tab_updated_prod[ , description := NULL]
    
    # in this way the selection column is updated according to the user changes
    # all dimensions in tab_updated_prod are contained in prod_asfis_full2
    full_updated <- merge(prod_asfis_full,
                          tab_updated_prod,
                          by = c("timePointYears", 
                                 "isscaap",
                                 "Scheda",
                                 "measuredItemISSCFC",
                                 "fisheriesAsfis",
                                 "geographicAreaM49_fi",
                                 "Value_ProcessedProd", "flagObservationStatus_ProcessedProd",
                                 "flagMethod_ProcessedProd", "measuredElement"
                          ),
                          all = TRUE, suffixes = c("_old", "_new"))
    
    full_updated$Value_ProcessedProd <- as.numeric(full_updated$Value_ProcessedProd)
    
    full_updated[ , flagObservationStatus_PrimaryProd := flagObservationStatus_PrimaryProd_old]
    full_updated[! is.na(flagObservationStatus_PrimaryProd_new) , flagObservationStatus_PrimaryProd := flagObservationStatus_PrimaryProd_new]
    
    # All modifiable in the table so if the new value is not NA 
    # then it will be replaced to the previous value
    full_updated[ , c("nationaldescription", "remarks", "Ratio",
                      "Selection", "type", "Value_PrimaryProd",
                      "flagMethod_PrimaryProd") := list(
                        ifelse(!is.na(nationaldescription_new), nationaldescription_new, nationaldescription_old),
                        ifelse(!is.na(remarks_new), remarks_new, remarks_old),
                        ifelse(!is.na(Ratio_new), Ratio_new, Ratio_old),
                        ifelse(is.na(Selection_new) & timePointYears != input$btn_year, Selection_old, Selection_new),
                        ifelse(!is.na(type_new), type_new, type_old),
                        ifelse(!is.na(Value_PrimaryProd_new), Value_PrimaryProd_new, Value_PrimaryProd_old),
                        ifelse(!is.na(flagMethod_PrimaryProd_new), flagMethod_PrimaryProd_new, flagMethod_PrimaryProd_old))] 
    
    
    full_updated <- full_updated[ , c("Ratio_old", "Selection_old", "type_old", "Value_PrimaryProd_old",
                                      "flagObservationStatus_PrimaryProd_old", "flagMethod_PrimaryProd_old",
                                      "nationaldescription_old", "remarks_old", 
                                      "Selection_new", "type_new", "Ratio_new",
                                      "Value_PrimaryProd_new", "flagObservationStatus_PrimaryProd_new",
                                      "flagMethod_PrimaryProd_new", "nationaldescription_new", "remarks_new") := NULL]
    
    manual_ratio_prod_input <- ifelse(!is.null(input$btn_manual_prod) & input$btn_ratioProd == 3, input$btn_manual_prod, NA)
    
    missing_data <- ifelse(input$btn_missing == "No" & input$btn_ratioProd == 1,
                           as.numeric(procprodimp[geographicAreaM49_fi == sel_country & 
                                                    timePointYears == input$btn_year &
                                                    Scheda == sel_commodity, ]$Value), NA)
    
    # Aggregation for primary production and rates calculation
    
    prim_prod_out <- primaryprod_imputation2(full_prod = full_updated,
                                             cdb_prod_isscaap = prod_isscaap, # Datasets
                                             sel_year = input$btn_year,
                                             manual_ratio = manual_ratio_prod_input,
                                             missing_data = missing_data) # Parameters
    
    
    # Select column to highlight
    colSel <- which(colnames(prim_prod_out) == input$btn_year)-1
    
    rhandsontable(prim_prod_out, rowHeaders = NULL, width = 'auto', height = 'auto',
                  digits = 3, rownames = TRUE, striped = TRUE,
                  customBorders = list(list(
                    range = list(from = list(row = 0, col = colSel),
                                 to = list(row = 0, col = colSel)), 
                    top = list(width = 2, color = "red"),
                    left = list(width = 2, color = "red"),
                    bottom = list(width = 2, color = "red"),
                    right = list(width = 2, color = "red"))) )
    
    
  })
  
  output$gg_prod_estimates <- renderPlot({
    
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    if(is.null(input$asfis_check_data)) return(NULL)
    
    commodity_label <- rv_data$commodity_label
    procprodimp <- rv_data$procprod_imp
    globalProduction <- rv_data$globalProduction
    commodityDB <- rv_data$commodityDB
    
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_years <- input$btn_start_year:input$btn_year
    
    tab_updated_prod <- rhandsontable::hot_to_r(input$asfis_check_data)
    
    #-- CHECKS ----
    # Check if the commodity inserted is in the commodity list
    validate(
      need (all(tab_updated_prod$measuredItemISSCFC %in% unlist(map_isscfc[ , .(code)])),
            paste("One of the commodity inserted in the measuredItemISSCFC column is not in the commodity list. ",
                  paste(tab_updated_prod$measuredItemISSCFC[ which(!tab_updated_prod$measuredItemISSCFC %in% unlist(map_isscfc[, .(code)]))], collapse = ","),
                  " is not a valid code." )
      )
    )
    
    # Check if the species inserted is in the Asfis list
    validate(
      need (all(tab_updated_prod$fisheriesAsfis %in% c('all', map_asfis$fisheriesAsfis)),
            paste("One of the Asfis code inserted in the fisheriesAsfis column is not in the Asfis list. ",
                  paste(tab_updated_prod$fisheriesAsfis[ which(!tab_updated_prod$fisheriesAsfis %in% c('all', map_asfis$fisheriesAsfis) )], collapse = ","),
                  " is not a valid code." )
      )
    )
    
    #-- After first checks ----
    
    data_out_prod <- asfis_check_reac()
    prod_asfis_full <- data_out_prod$prod_asfis_full
    prod_isscaap <-  data_out_prod$prod_isscaap
    #tab_updated_prod <- tab_updated_prod[, c("ics", "description") := NULL]
    
    tab_updated_prod <- tab_updated_prod[, c("geographicAreaM49_fi",
                                             "Value_ProcessedProd", "flagObservationStatus_ProcessedProd",
                                             "flagMethod_ProcessedProd", "measuredElement"):= list(sel_country,
                                                                                                   unique(prod_asfis_full[timePointYears == input$btn_year,]$Value_ProcessedProd),
                                                                                                   unique(prod_asfis_full[timePointYears == input$btn_year,]$flagObservationStatus_ProcessedProd),
                                                                                                   unique(prod_asfis_full[timePointYears == input$btn_year,]$flagMethod_ProcessedProd),
                                                                                                   unique(prod_asfis_full[timePointYears == input$btn_year,]$measuredElement))]
    
    
    tab_updated_prod[ , description := NULL]
    
    # in this way the selection column is updated according to the user changes
    # all dimensions in tab_updated_prod are contained in prod_asfis_full2
    full_updated <- merge(prod_asfis_full,
                          tab_updated_prod,
                          by = c("timePointYears",
                                 "fisheriesAsfis", 
                                 "isscaap",
                                 "Scheda",
                                 "measuredItemISSCFC",
                                 "geographicAreaM49_fi",
                                 "Value_ProcessedProd", "flagObservationStatus_ProcessedProd",
                                 "flagMethod_ProcessedProd", "measuredElement"
                          ),
                          all = TRUE, suffixes = c("_old", "_new"))
    
    full_updated$Value_ProcessedProd <- as.numeric(full_updated$Value_ProcessedProd)
    
    full_updated[ , flagObservationStatus_PrimaryProd := flagObservationStatus_PrimaryProd_old]
    full_updated[! is.na(flagObservationStatus_PrimaryProd_new) , flagObservationStatus_PrimaryProd := flagObservationStatus_PrimaryProd_new]
    
    # All modifiable in the table so if the new value is not NA 
    # then it will be replaced to the previous value
    full_updated[ , c("nationaldescription", "remarks", "Ratio",
                      "Selection", "type", "Value_PrimaryProd",
                      "flagMethod_PrimaryProd") := list(ifelse(!is.na(nationaldescription_new), nationaldescription_new, nationaldescription_old),
                                                        ifelse(!is.na(remarks_new), remarks_new, remarks_old),
                                                        ifelse(!is.na(Ratio_new), Ratio_new, Ratio_old),
                                                        ifelse(!is.na(Selection_new), Selection_new, Selection_old),
                                                        ifelse(!is.na(type_new), type_new, type_old),
                                                        ifelse(!is.na(Value_PrimaryProd_new), Value_PrimaryProd_new, Value_PrimaryProd_old),
                                                        ifelse(!is.na(flagMethod_PrimaryProd_new), flagMethod_PrimaryProd_new, flagMethod_PrimaryProd_old))] 
    
    
    full_updated[ , c("Ratio_old", "Selection_old", "type_old", "Value_PrimaryProd_old",
                      "flagObservationStatus_PrimaryProd_old", "flagMethod_PrimaryProd_old",
                      "Selection_new", "type_new", "Ratio_new",
                      "Value_PrimaryProd_new", "flagObservationStatus_PrimaryProd_new",
                      "flagMethod_PrimaryProd_new", "nationaldescription_new", "remarks_new") := NULL]
    
    manual_ratio_prod_input <- ifelse(!is.null(input$btn_manual_prod) & input$btn_ratioProd == 3, input$btn_manual_prod, NA)
    
    missing_data <- ifelse(input$btn_missing == "No" & input$btn_ratioProd == 1,
                           as.numeric(procprodimp[geographicAreaM49_fi == sel_country & 
                                                    timePointYears == input$btn_year &
                                                    Scheda == sel_commodity, ]$Value), NA)
    
    # Aggregation for primary production and rates calculation
    
    prim_prod_out <- primaryprod_imputation2(full_prod = full_updated,
                                             cdb_prod_isscaap = prod_isscaap, # Datasets
                                             sel_year = input$btn_year,
                                             manual_ratio = manual_ratio_prod_input,
                                             missing_data = missing_data) # Parameters
    
    prim_prod_out <- as.data.table(prim_prod_out)
    prim_prod_out2 <- prim_prod_out[, -which(names(prim_prod_out) == "Flag"), with = FALSE]
    prim_prod_out_aux <- melt(prim_prod_out2, 1, variable.name = 'Year')
    
    suppressWarnings(prim_prod_out_aux[, Year := as.numeric(as.character(Year))])
    suppressWarnings(prim_prod_out_aux[, value := as.numeric(as.character(value))])
    
    ggplot(data = prim_prod_out_aux[Stats != 'Ratio'], aes(x = Year, y = value)) +
      geom_line(aes(group = Stats, color = Stats), size = 1) +
      geom_point(aes(color = Stats), size = 2) +
      scale_color_manual(values=c("blue","red")) +
      labs(y = 'Quantity (in tonnes)', color = '', title = 'Primary production approach imputation') +
      theme_minimal() +
      theme(legend.position = 'bottom')
    
  })
  
  
  # -- Check production mapping ----
  observeEvent(input$check_consistency_prod, {
    
    # If there's currently a notification, don't add another
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    if(is.null(input$asfis_check_data)) return(NULL)
    
    # get needed data and tables
    commodity_label <- rv_data$commodity_label
    procprodimp <- rv_data$procprod_imp
    globalProduction <- rv_data$globalProduction
    commodityDB <- rv_data$commodityDB
    map_prod_prod <- rv_mappingtable$map_prod_prod
    
    # parameters explicited from buttons
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_years <- input$btn_start_year:input$btn_year
    
    startYearLastMapping <- unique(map_prod_prod[geographicAreaM49_fi == sel_country &
                                                   measuredItemISSCFC == sel_isscfc &
                                                   end_year == "LAST", ]$start_year)
    if(length(startYearLastMapping) >1) {
      id_prod <<- showNotification("INCONSISTENCY! The chosen commodity has two start years for the same end year (LAST). Please check in the SWS mapping datatable.",
                                   duration = 0)
    }
    
    if(as.numeric(input$btn_year) < as.numeric(startYearLastMapping)){
      
      id_prod <<- showNotification("The mapping has already changed since the chosen imputation year.
                                   Please update the mapping manually coherently to avoid automatic errors.",
                                   duration = 0)
    }
    
    # Check if an ISSCAAP group has been allocated twice so there is a double allocation of the same quantity
    # the check is made on geographic area, same end year, same ISSCAAP group and same Selection (only TRUE-TRUE)
    # No check for no allocation
    onlytrue <- map_prod_prod[Selection == TRUE, ]
    if(nrow(onlytrue[duplicated(setkey(onlytrue, geographicAreaM49_fi,
                                       end_year,
                                       isscaap,
                                       Selection)),]) > 0){
      
      listp <- unique(onlytrue[duplicated(setkey(onlytrue, geographicAreaM49_fi,
                                                 end_year,
                                                 isscaap,
                                                 Selection)),]$isscaap)
      if( length(listp) > 5){
        listp <- c(listp[1:5], '...')
      }
      
      id_prod <<- showNotification( sprintf("ISSCAAP group(s) %s is (are) mapped to more than one commodity for this country.",
                                            paste(listp, collapse = ",")),
                                    duration = 0)
    }
    
    
    
    
    # Check for duplicates and mapping consistency:
    # check if new commodities and/or new standing alone commodities
    
    # Comparing if there are commodities in the commodity db that are not in the mapping among production codes
    # i.e. codes are either not in the commodity list or they are in the export list and included in the mapping
    # linked to a different commodity
    
    mappingCommProd <- unique(map_prod_prod[geographicAreaM49_fi == sel_country , .(measuredItemISSCFC)])
    ppComm <- unique(procprodimp[geographicAreaM49_fi == sel_country, .(measuredItemISSCFC)])
    
    newProdCode <- ppComm[!measuredItemISSCFC %in% mappingCommProd$measuredItemISSCFC]
    
    if(nrow(newProdCode) > 0){
      
      listp1 <- unique(newProdCode$measuredItemISSCFC)
      
      if( length(listp1) > 5){
        listp1 <- c(listp1[1:5], '...')
      }
      
      id_prod <<- showNotification(paste("The following items are not included in the primary production approach mapping table:",
                                         paste(listp1, collapse = ",")),
                                   duration = 0)
    }    
    
    # Check if ratios are less than one
    map_prod_prod_use <- copy(map_prod_prod)
    
    map_prod_prod_use[ , Use := sum(Ratio*Selection, na.rm = TRUE),
                       by = c("geographicAreaM49_fi",
                              "end_year", "fisheriesAsfis")]
    
    if(nrow(map_prod_prod_use[Use > 1, ]) > 0){
      
      listp2 <- unique(map_prod_prod_use[Use > 1, ]$fisheriesAsfis)
      if( length(listp2) > 5){
        listp2 <- c(listp2[1:5], '...')
      }
      
      id_prod <<- showNotification(paste('More than 100% of the available primary production has been allocated for species ',
                                         paste(listp2, collapse = ",") ) ,
                                   duration = 0)
    }
    
    # Check if there is production for species in one ISSCAAP group that have been excluded because of a mapping update
    
    
    if(as.numeric(input$btn_year) >= as.numeric(startYearLastMapping) &
       nrow(onlytrue[duplicated(setkey(onlytrue, geographicAreaM49_fi,
                                       end_year,
                                       isscaap,
                                       Selection)),]) == 0 &
       nrow(newProdCode) == 0 &
       nrow(map_prod_prod_use[Use > 1, ]) == 0){
      id_prod <<- showNotification("The current mapping is consistent.", duration = 0)
    }
    
  })
  
  
  # -- Summary ----
  
  #-- Manual button summary
  output$out_btn_manual_remark <- renderUI({
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    if(is.null(input$btn_commodity)) return(NULL)
    procprodimp <- rv_data$procprod_imp
    commodity_label <- rv_data$commodity_label
    
    sel_years <- input$btn_start_year:input$btn_year
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]
    
    remarkCurrent <- procprodimp[geographicAreaM49_fi == sel_country & 
                                   timePointYears == input$btn_year & 
                                   measuredElement == "5510" &
                                   Scheda == sel_commodity]$remarks
    # Button for manual ratio input
    textInput(inputId = 'btn_manual_remark', label = 'Remark', value = remarkCurrent)
    
  })
  
  output$summary_check_data <- renderUI({
    
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    commodity_label <- rv_data$commodity_label
    procprodimp <- rv_data$procprod_imp
    globalProduction <- rv_data$globalProduction
    commodityDB <- rv_data$commodityDB
 
    sel_years <- input$btn_start_year:input$btn_year
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]
    procprodimp$Value <- as.numeric(procprodimp$Value)
    # if(sel_commodity == 1.1112){
    #   browser()
    # }
    procprodimp$flagObservationStatus <- as.character(procprodimp$flagObservationStatus) 
    procprodimp[is.na(procprodimp$flagObservationStatus), ]$flagObservationStatus <- ''
    
    # avoid errors if export approach not applied
    if(is.null(input$table_exp_estimates) | checkPassed$Exp == 'No'){
      tab_exp_prod <- NA
      export_app <- NA
    } else {
      tab_exp_prod <- rhandsontable::hot_to_r(input$table_exp_estimates)
      export_app <- tab_exp_prod[1, input$btn_year, with = F]
    }
    
 
    # avoid errors if export approach not applied
    primary_app <- ifelse(!is.null(input$table_prod_estimates) & checkPassed$Prim != 'No',
                          unlist(input$table_prod_estimates$data[[1]])[(length(sel_years)*2)],
                          NA)
    # If no results from previous approaches then the time series for the ensemble method has to be complete
    
    rows <- !combineFlag(procprodimp, "flagObservationStatus", "flagMethod") %in% c('(M, -)', '(O, -)')
    mod_imp_data <<- method_imputation(procprod = procprodimp[rows,], # Datasets
                                       sel_country = sel_country,
                                       sel_years = sel_years,
                                       sel_commodity = sel_commodity) # Parameters
    
    
    model_based_app <- ifelse(nrow(mod_imp_data) > 0,
                              mod_imp_data[timePointYears == input$btn_year, ]$Value,
                              NA)
    
    # If data are not missing then they are isolated in this object
    dataValue <- ifelse(nrow(procprodimp[geographicAreaM49_fi == sel_country & 
                                           timePointYears == input$btn_year & 
                                           measuredElement == "5510" &
                                           Scheda == sel_commodity])>0,
                        procprodimp[geographicAreaM49_fi == sel_country & 
                                      timePointYears == input$btn_year & 
                                      measuredElement == "5510" &
                                      Scheda == sel_commodity, Value], NA)
    
    choices_names <- paste(c('Model-based:', 'Exports:', 'Primary Prod.:', 'Manual input', 'Data', 'None'),
                           c(model_based_app, export_app, primary_app, '', dataValue, ''))
    
    
    btn_radio <- radioGroupButtons(
      inputId = "btn_approach",
      individual = FALSE,
      label = "Approach",
      selected = 6,
      choiceNames = choices_names, 
      choiceValues = 1:6,
      status = "info",
      justified = FALSE,
      direction = "vertical",
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"),
        no = icon("remove",
                  lib = "glyphicon"))
    )
    
    btn_radio
    
  })
  
  output$out_btn_manual <- renderUI({
    if(input$btn_approach != 4) return(NULL)
    numericInput(inputId = 'btn_manual', label = 'Manual approach', value = NA)
    
    
  })
  
  # Plot
  
  output$gg_methods <- renderPlot({
    
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    
    commodity_label <- rv_data$commodity_label
    procprodimp <- rv_data$procprod_imp
    globalProduction <- rv_data$globalProduction
    commodityDB <- rv_data$commodityDB
    
    sel_years <- input$btn_start_year:input$btn_year
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]
    
    if(is.null(input$table_exp_estimates) | checkPassed$Exp == 'No'){
      tab_exp_prod <- NA
      export_app <- NA
    } else {
      tab_exp_prod <- rhandsontable::hot_to_r(input$table_exp_estimates)
      export_app <- tab_exp_prod[1, input$btn_year, with = F]
    }
    
    primary_app <- ifelse(!is.null(input$table_prod_estimates) & checkPassed$Prim != 'No',
                          unlist(input$table_prod_estimates$data[[1]])[(length(sel_years)*2)],
                          NA)
    
    model_based_app <- ifelse(nrow(mod_imp_data) > 0,
                              mod_imp_data[timePointYears == input$btn_year, ]$Value,
                              NA)
    
    dataValue <- ifelse(nrow(procprodimp[geographicAreaM49_fi == sel_country & 
                                           timePointYears == input$btn_year & 
                                           measuredElement == "5510" &
                                           Scheda == sel_commodity])>0,
                        procprodimp[geographicAreaM49_fi == sel_country & 
                                      timePointYears == input$btn_year & 
                                      measuredElement == "5510" &
                                      Scheda == sel_commodity, Value], NA)
    
    if(!is.na(primary_app)){
      tbl_prim_estimates <- rhandsontable::hot_to_r(input$table_prod_estimates)
      tbl_prim_estimates <- tbl_prim_estimates[, -which(names(tbl_prim_estimates) == "Flag"), with = FALSE]
      gg_prod_df <- tbl_prim_estimates[1, 2:(ncol(tbl_prim_estimates) - 2), with = F]
      gg_prod_df <- cbind(aux = NA, gg_prod_df)
      gg_prod_df <- melt(gg_prod_df, 1, variable.name = 'Year')
      gg_prod_df[, Year := as.numeric(as.character(Year))]
      
      
      gg_prod_df[, value := as.numeric(as.character(value))]
      
      manual_input <- ifelse(is.null(input$btn_manual), NA, input$btn_manual)
      
      gg_estimate <<- data.frame(Approach = c('Model-based', 'Exports', 'Primary Prod.', 'Manual input', 'Data'),
                                 Value = as.numeric(c(model_based_app, export_app, primary_app, 
                                                      manual_input, dataValue)),
                                 Year = as.numeric(input$btn_year))
      
      ggplot() +
        geom_line(data = gg_prod_df, aes(x = Year, y = value), size = 1) +
        geom_point(data = gg_prod_df, aes(x = Year, y = value), size = 2) +
        geom_point(data = gg_estimate, aes(x = Year, y = Value, label = Approach, color = Approach), size = 3) +
        geom_text(data = gg_estimate, 
                  aes(x = Year, y = Value, label = Approach),
                  hjust= 1.1, vjust= -0.5,
                  check_overlap = TRUE) +
        labs(y = 'Quantity (in tonnes)', title = 'Approach comparison imputation') +
        theme_minimal() +
        theme(legend.position = 'bottom')
      
    } else {
      
      
      gg_prod_df <- procprodimp[geographicAreaM49_fi == sel_country & 
                                  timePointYears %in% sel_years & 
                                  measuredElement == "5510" &
                                  Scheda == sel_commodity, .(timePointYears, Value)]
      
      setnames(gg_prod_df, c('timePointYears', 'Value'), c('Year', 'value'))
      gg_prod_df[, Year := as.numeric(as.character(Year))]
      gg_prod_df[, value := as.numeric(as.character(value))]
      gg_prod_df <- gg_prod_df[!Year %in% input$btn_year, ]
      
      manual_input <- ifelse(is.null(input$btn_manual), NA, input$btn_manual)
      
      gg_estimate <<- data.frame(Approach = c('Model-based', 'Exports', 'Primary Prod.', 'Manual input', 'Data'),
                                 Value = as.numeric(c(model_based_app, export_app, primary_app, 
                                                      manual_input, dataValue)),
                                 Year = as.numeric(input$btn_year))
      
      ggplot() +
        geom_line(data = gg_prod_df, aes(x = Year, y = value), size = 1) +
        geom_point(data = gg_prod_df, aes(x = Year, y = value), size = 2) +
        geom_point(data = gg_estimate, aes(x = Year, y = Value,label = Approach, color = Approach), size = 3) +
        geom_text(data = gg_estimate, 
                  aes(x = Year, y = Value, label = Approach),
                  hjust= 1.1, vjust= -0.5,
                  check_overlap = TRUE) +
        labs(y = 'Quantity (in tonnes)', title = 'Approach comparison imputation') +
        theme_minimal() +
        theme(legend.position = 'bottom')
      
      
    }
    
  })
  
  
  # -- Update export mapping ----
  
  observeEvent(input$update_export_mapping, {
    
    if(is.null(input$isscfc_check_data)) return(NULL)
    # Needed element (transform from label + code to just code)
    commodity_label <- rv_data$commodity_label
    
    map_prod_exp <- rv_mappingtable$map_prod_exp
    
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]
    
    ## Select the export mapping part updated and reshape it according to the initial mapping
    # copy of the table on the shiny and customize it according to the mapping table
    exp_part_upd <- rhandsontable::hot_to_r(input$isscfc_check_data)
    exp_part_upd <- exp_part_upd[, c("description", "Value_exp", "flagObservationStatus_exp", "flagMethod_exp" ):= NULL]
    exp_part_upd <- exp_part_upd[, c("geographicAreaM49_fi", "end_year", "start_year") := list(sel_country, "LAST", input$btn_year) ]
    
    exp_part_upd <- exp_part_upd[, .( geographicAreaM49_fi,
                                      start_year,
                                      end_year,
                                      measuredItemISSCFC,
                                      measuredItemISSCFC_exp,
                                      Selection, type)]
    
    # Take the corresponding part of the original mapping to compare
    corresponding_mapping_export <- map_prod_exp[ geographicAreaM49_fi == sel_country &
                                                    measuredItemISSCFC %in% unique(exp_part_upd$measuredItemISSCFC) &
                                                    end_year == "LAST", .(geographicAreaM49_fi,
                                                                          end_year,
                                                                          measuredItemISSCFC,
                                                                          measuredItemISSCFC_exp,
                                                                          Selection, type)]
    
    # Update mapping, 4 cases considered: 
    # 1. The mapping does not change
    # 2. The mapping for the selected commodity does not change but other row are added for new commodities
    # N.B. if a mapping already existing is added but already existing nothing happens since nrow(setdiff)=0 
    # 3. The mapping changes but for the same period, i.e. the user changes his/her mind about the mapping
    # 4. Everything changes
    
    
    if(nrow(setdiff(exp_part_upd[, .(geographicAreaM49_fi, 
                                     measuredItemISSCFC, 
                                     measuredItemISSCFC_exp,
                                     end_year,
                                     Selection, type)], 
                    corresponding_mapping_export[, .(geographicAreaM49_fi, 
                                                     measuredItemISSCFC, 
                                                     measuredItemISSCFC_exp,
                                                     end_year,
                                                     Selection, type)]))==0  & 
       nrow(exp_part_upd) == nrow(corresponding_mapping_export) &
       all(exp_part_upd$measuredItemISSCFC_exp == corresponding_mapping_export$measuredItemISSCFC_exp)){ 
      # case of no update (no selection or unselection and no new entry)
      # nothing changes
      export_mapping_upd <- map_prod_exp
      
    } else if(nrow(setdiff(exp_part_upd[, .(geographicAreaM49_fi, measuredItemISSCFC, 
                                            measuredItemISSCFC_exp, end_year,
                                            Selection, type)], 
                           corresponding_mapping_export[ , .(geographicAreaM49_fi, measuredItemISSCFC, 
                                                             measuredItemISSCFC_exp, end_year, Selection, type)])) == 0 & 
              nrow(exp_part_upd) != nrow(corresponding_mapping_export) ) { 
      # case of adding commodities without changing mapping of selected commodity
      
      # identify additional part
      exp_part2upd <- exp_part_upd[!measuredItemISSCFC %in% unique(corresponding_mapping_export$measuredItemISSCFC), ]
      #if an export commodity has been added
      exp_part3upd <- exp_part_upd[!measuredItemISSCFC_exp %in% unique(corresponding_mapping_export$measuredItemISSCFC_exp), ]
      
      # if a row has been deleted then FALSE is placed at the corresponding row
      if(corresponding_mapping_export[!measuredItemISSCFC_exp %in% exp_part_upd$measuredItemISSCFC_exp] |
         corresponding_mapping_export[!measuredItemISSCFC %in% exp_part_upd$measuredItemISSCFC] ){
        
        isscfc_expNo <- corresponding_mapping_export[!measuredItemISSCFC_exp %in% exp_part_upd$measuredItemISSCFC_exp]$measuredItemISSCFC_exp
        isscfcNo <- corresponding_mapping_export[!measuredItemISSCFC %in% exp_part_upd$measuredItemISSCFC]$measuredItemISSCFC
        
        map_prod_exp[geographicAreaM49_fi == sel_country &
                       measuredItemISSCFC == isscfcNo &
                       end_year == "LAST", ]$Selection <- FALSE
        
        map_prod_exp[geographicAreaM49_fi == sel_country &
                       measuredItemISSCFC_exp == isscfc_expNo &
                       end_year == "LAST", ]$Selection <- FALSE
      }
      
      export_mapping_upd <- rbind(map_prod_exp[ , .(geographicAreaM49_fi, start_year, end_year, measuredItemISSCFC, measuredItemISSCFC_exp, Selection, type)], 
                                  exp_part2upd, exp_part3upd)
      
    } else if(any(exp_part_upd$start_year %in% map_prod_exp[ geographicAreaM49_fi == sel_country &
                                                             measuredItemISSCFC == unique(exp_part_upd$measuredItemISSCFC) &
                                                             end_year == "LAST",]$start_year)){ 
      # changing mapping for the same imputation year
      
      new_map <- merge(map_prod_exp, exp_part_upd, by = c("geographicAreaM49_fi",
                                                          "start_year",
                                                          "end_year",
                                                          "measuredItemISSCFC",
                                                          "measuredItemISSCFC_exp"),
                       all = TRUE, suffixes = c("_previous", "_updated"))
      
      new_map$Selection <- ifelse(is.na(new_map$Selection_updated), new_map$Selection_previous, new_map$Selection_updated)
      new_map$type <- ifelse(is.na(new_map$type_updated), new_map$type_previous, new_map$type_updated)
      
      new_map[ , c("Selection_previous", "Selection_updated", "type_previous", "type_updated") := NULL]
      
      startyear <- exp_part_upd[measuredItemISSCFC == sel_isscfc, ]$start_year
      
      new_map[geographicAreaM49_fi == sel_country &
                measuredItemISSCFC %in% unique(exp_part_upd$measuredItemISSCFC) &
                end_year == "LAST" & start_year == startyear &
                !measuredItemISSCFC_exp %in% exp_part_upd$measuredItemISSCFC_exp ]$Selection <- FALSE
      
      
      export_mapping_upd <- new_map
      
    } else {
      
      map_prod_exp_mod <- copy(map_prod_exp)
      
      map_prod_exp_mod[geographicAreaM49_fi == sel_country &
                         measuredItemISSCFC %in% unique(exp_part_upd$measuredItemISSCFC) & 
                         end_year == "LAST", end_year := as.character(as.numeric(input$btn_year)-1)]
      
      export_mapping_upd <- rbind(map_prod_exp_mod[ , .(geographicAreaM49_fi, type, start_year, end_year, measuredItemISSCFC, measuredItemISSCFC_exp, Selection)],
                                  exp_part_upd)
      
    }
    
    rv_mappingtable$map_prod_exp <- export_mapping_upd
    
    showModal(modalDialog(
      title = "Update export-based approach mapping." ,
      sprintf("The mapping table has been updated and it is diplayed in the tab: Save Export Mapping.")
    ))
    
  })
  
  # -- Update primary production mapping ----
  
  observeEvent(input$update_production_mapping, {
    
    if(is.null(input$asfis_check_data)) return(NULL)
    
    commodity_label <- rv_data$commodity_label
    map_prod_prod <- rv_mappingtable$map_prod_prod
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_years <- input$btn_start_year:input$btn_year
    sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]
    
    tab_updated_prod <- rhandsontable::hot_to_r(input$asfis_check_data)
    
    tab_updated_prod <- tab_updated_prod[, c("geographicAreaM49_fi", "end_year", "start_year",
                                             "Ratio") := list(sel_country, "LAST", input$btn_year,
                                                              ifelse(is.na(Ratio), 1, Ratio)) ]
    tab_updated_prod[is.na(Selection) , Selection := FALSE]
    tab_updated_prod[is.na(fisheriesAsfis) , fisheriesAsfis := 'all']
    tab_updated_prod[is.na(measuredItemISSCFC) , measuredItemISSCFC := sel_isscfc]
    tab_updated_prod[is.na(type) , type := 'provided']
    
    corresponding_mapping_prod <- map_prod_prod[ geographicAreaM49_fi == sel_country &
                                                   measuredItemISSCFC %in% unique(tab_updated_prod$measuredItemISSCFC) &
                                                   end_year == "LAST", .(geographicAreaM49_fi,
                                                                         end_year,
                                                                         measuredItemISSCFC,
                                                                         isscaap,
                                                                         fisheriesAsfis,
                                                                         Ratio,
                                                                         Selection,
                                                                         type)]
    
    
    # Update cases:
    # 1) all the ISSCAAP group are selected in the mapping and nothing changes in the selection (i.e. all selected)
    # or in the Ratios
    # 2) adding an isscaap
    # 3) changes in the current period mapping
    # 4) all other cases
    names(corresponding_mapping_prod)
    
    if(all(length(tab_updated_prod[ , unique(Selection), by = isscaap]$V1) == length(corresponding_mapping_prod$Selection)) & 
       all(tab_updated_prod[ , unique(Selection), by = isscaap] == corresponding_mapping_prod$Selection) &
       all(corresponding_mapping_prod$fisheriesAsfis == 'all') &
       all(unique(tab_updated_prod[ ,.(geographicAreaM49_fi, 
                                       end_year, 
                                       measuredItemISSCFC, 
                                       isscaap, 
                                       Ratio)]$Ratio) == unique(corresponding_mapping_prod$Ratio)) & 
       nrow(tab_updated_prod[!isscaap %in% unique(corresponding_mapping_prod$isscaap), ]) == 0){
      
      prod_mapping_upd <- map_prod_prod
      
    } else if (nrow(tab_updated_prod[!isscaap %in% unique(corresponding_mapping_prod$isscaap), ]) > 0){
      
      tab2add <- tab_updated_prod[!isscaap %in% unique(corresponding_mapping_prod$isscaap), ]
      
      # if it is the same one commodity that is modified then the mapping has to change start date also with the old commodity
      if(length(unique(corresponding_mapping_prod$measuredItemISSCFC)) == 1 & unique(corresponding_mapping_prod$measuredItemISSCFC) == unique(tab_updated_prod$measuredItemISSCFC)){
      
        map_prod_prod <- map_prod_prod[geographicAreaM49_fi == sel_country &
                            measuredItemISSCFC %in% unique(tab_updated_prod$measuredItemISSCFC) & 
                            end_year == "LAST", end_year := as.character(as.numeric(input$btn_year)-1)]
          
      }
      
      corresponding_mapping_prod_renewed <- copy(corresponding_mapping_prod)
      corresponding_mapping_prod_renewed[ , start_year := input$btn_year]
      
      # Isscaap not new
      groups <- unique(corresponding_mapping_prod_renewed$isscaap)
      
      # For each pre-mapped ISSCAAP see if:
      # 1. Selection stays the same, nothing happens
      # 2. Selection 'all' changes 
      # 3. Selection not 'all' anymore
      for(i in seq_len(length(unique(corresponding_mapping_prod_renewed$isscaap)))){
      
      if(length(unique(tab_updated_prod[isscaap == groups[i]]$Selection)) == 1 &
         unique(corresponding_mapping_prod_renewed[isscaap == groups[i]]$Selection) == unique(tab_updated_prod[isscaap == groups[i]]$Selection)){
        
        corresponding_mapping_prod_renewed[isscaap == groups[i]] <- corresponding_mapping_prod_renewed[isscaap == groups[i]]
        
      } else if(length(unique(tab_updated_prod[isscaap == groups[i]]$Selection)) == 1 &
                unique(corresponding_mapping_prod_renewed[isscaap == groups[i]]$Selection) != unique(tab_updated_prod[isscaap == groups[i]]$Selection)){
        
        Selupd <- unique(tab_updated_prod[isscaap == groups[i]]$Selection)
        
        corresponding_mapping_prod_renewed[isscaap == groups[i]]$Selection <- Selupd 
        
      } else if(length(unique(tab_updated_prod[isscaap == groups[i]]$Selection)) > 1){
        
        corresponding_mapping_prod_renewed[isscaap == groups[i]] <- tab_updated_prod[isscaap == groups[i]]
        
      }
    
      }
      prod_mapping_upd <- rbind(map_prod_prod[ , .(geographicAreaM49_fi, start_year, end_year, measuredItemISSCFC, isscaap, 
                                                   fisheriesAsfis, Ratio, Selection, type)], 
                                tab2add[ , .(geographicAreaM49_fi, start_year, end_year, measuredItemISSCFC, isscaap, 
                                             fisheriesAsfis, Ratio, Selection, type)],
                                corresponding_mapping_prod_renewed)

      setkey(prod_mapping_upd)
      prod_mapping_upd <- unique(prod_mapping_upd)       
      
      
      
      
    } else if(all(tab_updated_prod$start_year %in% 
                  unique(map_prod_prod[ geographicAreaM49_fi == sel_country &
                                        measuredItemISSCFC %in% unique(tab_updated_prod$measuredItemISSCFC) &
                                        end_year == "LAST",]$start_year))){
      
      new_map_prod <- merge(map_prod_prod, tab_updated_prod, by = c("geographicAreaM49_fi",
                                                                    "start_year",
                                                                    "end_year",
                                                                    "measuredItemISSCFC",
                                                                    "isscaap", 
                                                                    "fisheriesAsfis"),
                            all = TRUE, suffixes = c("_previous", "_updated"))
      
      
      new_map_prod_last <- new_map_prod[end_year == 'LAST' ]
      groups <- unique(tab_updated_prod$isscaap)

      for(i in seq_len(length(unique(tab_updated_prod$isscaap)))){
        
        
        #If 'all' species i.e. selection  all FALSE or all TRUE
      if(length(unique(new_map_prod_last[!is.na(Selection_updated) & isscaap == groups[i]]$Selection_updated)) == 1 &
         any(new_map_prod_last[isscaap == groups[i]]$fisheriesAsfis == 'all')){
        
        Selupd <- unique(new_map_prod_last[!is.na(Selection_updated) & isscaap == groups[i]]$Selection_updated)
        
        new_map_prod <- new_map_prod[!new_map_prod_last[isscaap == groups[i] & fisheriesAsfis != 'all']]
        new_map_prod <- new_map_prod[new_map_prod_last[isscaap == groups[i] & fisheriesAsfis == 'all'], Selection_updated := Selupd ]
        
      } else if(length(unique(tab_updated_prod[isscaap == groups[i]]$Selection)) > 1 &
                any(new_map_prod_last[isscaap == groups[i]]$fisheriesAsfis == 'all')){
        
        new_map_prod <- new_map_prod[!new_map_prod_last[isscaap == groups[i] & fisheriesAsfis == 'all']]
        
      } else {
        new_map_prod <- new_map_prod
      }
        
      }
      
      new_map_prod$Selection <- ifelse(is.na(new_map_prod$Selection_updated), new_map_prod$Selection_previous, new_map_prod$Selection_updated)
      new_map_prod$Ratio <- ifelse(is.na(new_map_prod$Ratio_updated), new_map_prod$Ratio_previous, new_map_prod$Ratio_updated)
      new_map_prod$type <- ifelse(is.na(new_map_prod$type_updated), new_map_prod$type_previous, new_map_prod$type_updated)
      
      new_map_prod[ , c("Selection_previous", "Selection_updated", 
                        "Ratio_previous", "Ratio_updated",
                        "type_previous", "type_updated") := NULL]
      
      startyear <- unique(tab_updated_prod$start_year)
      
      # new_map_prod[geographicAreaM49_fi == sel_country &
      #                measuredItemISSCFC == sel_isscfc &
      #                end_year == "LAST" & start_year == startyear &
      #                !fisheriesAsfis %in% tab_updated_prod$fisheriesAsfis ]$Selection <- FALSE
      
      
      prod_mapping_upd <- new_map_prod[ , .(geographicAreaM49_fi,
                                            start_year,
                                            end_year,
                                            measuredItemISSCFC,
                                            isscaap,
                                            fisheriesAsfis,
                                            Ratio,
                                            Selection,
                                            type)]
      
    } else {
      
      map_prod_prod_mod <- copy(map_prod_prod)
      map_prod_prod_mod[geographicAreaM49_fi == sel_country &
                          measuredItemISSCFC %in% unique(tab_updated_prod$measuredItemISSCFC) & 
                          end_year == "LAST", end_year := as.character(as.numeric(input$btn_year)-1)]
      
      
      
      prod_mapping_upd <- rbind(map_prod_prod_mod[ , .(geographicAreaM49_fi, start_year, end_year, 
                                                       measuredItemISSCFC, isscaap, fisheriesAsfis, 
                                                       Ratio, Selection, type)],
                                tab_updated_prod[ , .(geographicAreaM49_fi, start_year, end_year, 
                                                      measuredItemISSCFC, isscaap, fisheriesAsfis, 
                                                      Ratio, Selection, type)])
    }
    
    rv_mappingtable$map_prod_prod <- prod_mapping_upd
    
    showModal(modalDialog(
      title = "Update primary production-based approach mapping." ,
      sprintf("The mapping table has been updated and it is diplayed in the tab: Save Primary Prod. Mapping.")
    ))
    
  })
  
  # -- Impute value ----
  
  observeEvent(input$btn_imputation, {
   
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    input$btn_approach
   
    imput_value <- ifelse(input$btn_approach == 4, input$btn_manual,
                          gg_estimate[input$btn_approach, 'Value'])
    
    commodity_label <- rv_data$commodity_label
    procprodimp <- rv_data$procprod_imp
    globalProduction <- rv_data$globalProduction
    commodityDB <- rv_data$commodityDB
    procprodimp0 <- rv_data$procprod_imp0
    
    procprod_copy <- copy(procprodimp)
    
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]
    sel_years <- input$btn_start_year:input$btn_year
    
    sel_approach <- as.character(gg_estimate[input$btn_approach, 'Approach'])
    approach_number <- input$btn_approach
    
    flagValue <- procprodimp[geographicAreaM49_fi == sel_country & 
                               timePointYears == input$btn_year &
                               Scheda == sel_commodity, .(flagObservationStatus, flagMethod)]
    
    sel_flagObs <- ifelse(approach_number == 1,  'I',
                          ifelse(approach_number == 2,  'I',
                                 ifelse(approach_number == 3, 'I',
                                        ifelse(approach_number == 4,'E',
                                               ifelse(approach_number == 5, flagValue$flagObservationStatus, NA)))))
    
    sel_flagMeth <- ifelse(approach_number == 1, "e",
                           ifelse(approach_number == 2, "i",
                                  ifelse(approach_number == 3, "i",
                                         ifelse(approach_number == 4, "f",
                                                ifelse(approach_number == 5, flagValue$flagMethod, NA)))))
    
    procprod_copy$flagObservationStatus <- as.character(procprod_copy$flagObservationStatus)
    
    
    procprod_copy$Value <- as.numeric(procprod_copy$Value)
    procprod_copy <- procprod_copy[timePointYears == as.character(input$btn_year) &
                                     Scheda == sel_commodity &
                                     geographicAreaM49_fi == sel_country &
                                     measuredElement == "5510", c("remarks", "Value",
                                                                  "flagObservationStatus",
                                                                  "flagMethod", "approach",
                                                                  "measureditemnational") := list(input$btn_manual_remark, imput_value, sel_flagObs,
                                                                                                  sel_flagMeth, sel_approach,
                                                                                                  paste(sel_country, sel_commodity, sep = "."))]
    
    # Updated SWS datatable
    setnames(procprod_copy, c("geographicAreaM49_fi", "measuredElement", "timePointYears",
                              "measuredItemISSCFC", "Scheda", "Value", 
                              "flagObservationStatus", "flagMethod"), 
             c("geographicaream49_fi", "measuredelement", "timepointyears",
               "measureditemisscfc", "id_nationalcode", "quantitymt", 
               "flagobservationstatus", "flagmethod"))
    
    newImputedData <- merge(procprod_copy, procprodimp0[ , .(`__id`, `__ts`, geographicaream49_fi, measuredelement,
                                                             timepointyears, id_isscfc, measureditemisscfc, id_nationalcode)], 
                            by = c("geographicaream49_fi", "measuredelement", "timepointyears", 
                                   "id_isscfc", "measureditemisscfc", "id_nationalcode"), 
                            all = TRUE)
    newImputedData$timepointyears <- as.character(newImputedData$timepointyears)
    newImputedData <- newImputedData[flagmethod != 'u']
    # needed as for some reason the merge is not working properly
    setkey(newImputedData)
    newImputedData <- unique(newImputedData)
    
    withProgress(message = 'Updating data',
                 value = 0, {
                   
                   Sys.sleep(0.25)
                   incProgress(0.25)
                   changeset <- Changeset('processed_prod_national_detail_imputed')
                   AddModifications(changeset, newImputedData)
                   Finalise(changeset)
                   
                   Sys.sleep(0.25)
                   incProgress(0.5)
                   
                   where <- paste("geographicaream49_fi = '", sel_country, "' ", sep = "")
                   
                   # Get Processed production datatable imputed
                   procprod_imp0 <- ReadDatatable('processed_prod_national_detail_imputed', where = where, readOnly = FALSE)
                   procprod_imp0 <- procprod_imp0[timepointyears %in% sel_years, ]
                   
                   procprod_imp <- copy(procprod_imp0)
                   
                   procprod_imp <- procprod_imp[ , c("__id", "__ts") := NULL]
                   
                   setnames(procprod_imp, c("geographicaream49_fi", "measuredelement", "timepointyears", "measureditemisscfc",
                                            "quantitymt", "flagobservationstatus", "flagmethod", "id_nationalcode"), 
                            c("geographicAreaM49_fi", "measuredElement", "timePointYears", "measuredItemISSCFC",
                              "Value", "flagObservationStatus", "flagMethod", "Scheda"))
                   
                   procprod_imp$flagObservationStatus <- factor(procprod_imp$flagObservationStatus, 
                                                                levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                                                ordered = TRUE)
                   ################
                   procprod_imp_upd <- merge(procprod_imp[ , .(geographicAreaM49_fi, 
                                                               measuredElement, 
                                                               timePointYears, 
                                                               measuredItemISSCFC, 
                                                               remarks, Value, 
                                                               flagObservationStatus,
                                                               flagMethod, Scheda, 
                                                               measureditemnational,
                                                               approach)], 
                                             procprodimp, 
                                             by = c( "geographicAreaM49_fi", 
                                                     "measuredElement", 
                                                     "timePointYears", 
                                                     "measuredItemISSCFC", 
                                                     "Scheda"), 
                                             all = TRUE, suffixes = c("_upd", "_old"))
                   
                   
                   # If upd is NA then leave old one (whether it is NA or not)
                   procprod_imp_upd[, remarks := remarks_upd]
                   procprod_imp_upd[is.na(remarks_upd), remarks := remarks_old]
                   procprod_imp_upd[, Value := Value_upd]
                   procprod_imp_upd[is.na(Value_upd), Value := Value_old]
                   procprod_imp_upd[, flagObservationStatus := flagObservationStatus_upd]
                   procprod_imp_upd[is.na(procprod_imp_upd$flagObservationStatus), ]$flagObservationStatus <- procprod_imp_upd[is.na(procprod_imp_upd$flagObservationStatus), ]$flagObservationStatus_old
                  # procprod_imp_upd[flagObservationStatus_upd %in% c('', 'X'), ]$flagObservationStatus <- procprod_imp_upd[flagObservationStatus_upd %in% c('', 'X'), ]$flagObservationStatus_upd
                   
                   procprod_imp_upd$flagObservationStatus <- factor(procprod_imp_upd$flagObservationStatus, 
                                                                    levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                                                    ordered = TRUE)
                   procprod_imp_upd <- procprod_imp_upd[, flagMethod := flagMethod_upd]
                   procprod_imp_upd <- procprod_imp_upd[is.na(flagMethod_upd), flagMethod := flagMethod_old]
                   
                   procprod_imp_upd <- procprod_imp_upd[ , measureditemnational := measureditemnational_upd]
                   procprod_imp_upd <- procprod_imp_upd[is.na(measureditemnational_upd) , measureditemnational := measureditemnational_old]
                   
                   procprod_imp_upd <- procprod_imp_upd[ , approach := approach_upd]
                   procprod_imp_upd <- procprod_imp_upd[is.na(approach_upd) , approach := approach_old]
                   
                   procprod_imp_upd <- procprod_imp_upd[ , c('remarks_old', 'Value_old', 'flagObservationStatus_old', 'flagMethod_old',
                                                             'approach_old', 'measureditemnational_old',
                                         'remarks_upd', 'Value_upd', 'flagObservationStatus_upd', 'flagMethod_upd', 'approach_upd',
                                         'measureditemnational_upd') := NULL]
                   
                   imputYear <- input$btn_year
                   # missingO <- procprod_imp_upd[flagObservationStatus == 'O' & timePointYears == as.character((as.numeric(imputYear) - 1)), ]
                   # 
                   # if(nrow(missingO) > 0){
                   #   schede2change <- unique(missingO$Scheda)
                   #   procprod_imp_upd[timePointYears == imputYear & Scheda %in% schede2change &
                   #                      flagObservationStatus == 'M' & flagMethod == 'u', 
                   #                    c("Value", "flagObservationStatus", "flagMethod") := list(0, 'O', '-')]
                   # }
                   
                   rv_data$procprod_imp0 <- procprod_imp0
                   rv_data$procprod_imp <- procprod_imp_upd
                   
                   ###################
               
                   # # Update rv_data
                   # rv_data$procprod_imp0 <- procprod_imp0
                   # rv_data$procprod_imp <- procprod_imp
                   
                   Sys.sleep(0.25)
                   incProgress(0.95)
                   
                 })
    
    # Add new row (if any)
    # AddInsertions(changeset, newImputedData)
    # Finalise(changeset)
    
    showModal(modalDialog(
      title = "Updated national commodity datatable.",
      sprintf("Approach: %s. SWS datatable name: processed_prod_national_detail_imputed", 
              as.character(gg_estimate[input$btn_approach, 'Approach']))
    ))
    
  })
  
  # -- Save export mapping ----
  
  mapping_rv <- reactiveValues(mapping_table_exp = data.table(),
                               mapping_table_prod = data.table())
  
  check_export_mapping_reac <- reactive({
    
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    
    sel_country <- country_input[country_input$label == input$btn_country, code]
    map_prod_exp <- rv_mappingtable$map_prod_exp
    
    return(map_prod_exp[geographicAreaM49_fi == sel_country, ])
  })
  
  output$check_export_mapping_data <- DT::renderDataTable( server = FALSE, { # renderRHandsontable({
    
    mapping_rv$mapping_table_exp <- check_export_mapping_reac()
    
    DT::datatable(mapping_rv$mapping_table_exp[ , .(geographicAreaM49_fi, start_year,
                                                    end_year, measuredItemISSCFC,
                                                    measuredItemISSCFC_exp, Selection, type)], 
                  extensions = 'Buttons', filter = 'top',
                  rownames = FALSE,
                  options = list(
                    dom = 'Bfrtip',
                    buttons = c('csv', 'excel', 'pdf')) # , editable = TRUE
    )
    # rhandsontable(table_out, rowHeaders = NULL, width = 'auto', height = 'auto') 
  })
  
  
  #-- Add row to Export mapping ----
  
  expAdd_reac <- reactive({ 
    
    req(input$btn_country, input$btn_year, input$btn_commodity)

    # parameters explicited from buttons
    commodity_label <- rv_data$commodity_label
    sel_years <- input$btn_start_year:input$btn_year
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]
    
    if(nrow(rv_mappingtable$map_prod_exp)  > 0){
    map_prod_exp <- rv_mappingtable$map_prod_exp
    
    map_prod_exp_filtered <- map_prod_exp[start_year <= input$btn_year & 
                                            input$btn_year < end_year & 
                                            measuredItemISSCFC %in% sel_isscfc, ]
    
    if(nrow(map_prod_exp_filtered) > 0){
      exp_map <- map_prod_exp_filtered[ ,.(# geographicAreaM49_fi,
                                           # start_year,
                                           # end_year,
                                           measuredItemISSCFC,
                                           measuredItemISSCFC_exp,
                                           Selection, type)]
    } else{
      
      exp_map <-  data.table(# geographicAreaM49_fi = sel_country,
                             # start_year = input$btn_year, 
                             # end_year = 'LAST',
                             measuredItemISSCFC = '',
                             measuredItemISSCFC_exp = '',
                             Selection = FALSE,
                             type = 'provided') 
    }
    
    } else {
    exp_map <-  data.table(# geographicAreaM49_fi = sel_country,
                           # start_year = input$btn_year, 
                           # end_year = 'LAST',
                           measuredItemISSCFC = '',
                           measuredItemISSCFC_exp = '',
                           Selection = FALSE,
                           type = 'provided') 
    }
    
  return(exp_map)
  
})
  
  output$expAdd <-  renderRHandsontable({
    
    req(input$btn_country, input$btn_year, input$btn_commodity)
    
    exp_map <- expAdd_reac()
    rhandsontable(exp_map, rowHeaders = NULL, width = 'auto', height = 'auto')
    
  })
  
  # -- Update add export mapping ----
  
  observeEvent(input$add_export_mapping, {
    
    if(is.null(input$expAdd)) return(NULL)
    # Needed element (transform from label + code to just code)
    commodity_label <- rv_data$commodity_label
    
    map_prod_exp <- rv_mappingtable$map_prod_exp
    
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]
    
    ## Select the export mapping part updated and reshape it according to the initial mapping
    # copy of the table on the shiny and customize it according to the mapping table
    exp_part_upd <- rhandsontable::hot_to_r(input$expAdd)
    # exp_part_upd <- exp_part_upd[, c("description", "Value_exp", "flagObservationStatus_exp", "flagMethod_exp" ):= NULL]
    exp_part_upd <- exp_part_upd[, c("geographicAreaM49_fi", "end_year", "start_year") := list(sel_country, "LAST", input$btn_year) ]
    
    exp_part_upd <- exp_part_upd[, .( geographicAreaM49_fi,
                                      start_year,
                                      end_year,
                                      measuredItemISSCFC,
                                      measuredItemISSCFC_exp,
                                      Selection, type)]
    
    # Take the corresponding part of the original mapping to compare
    corresponding_mapping_export <- map_prod_exp[ geographicAreaM49_fi == sel_country &
                                                    measuredItemISSCFC %in% unique(exp_part_upd$measuredItemISSCFC) &
                                                    end_year == "LAST", .(geographicAreaM49_fi,
                                                                          end_year,
                                                                          measuredItemISSCFC,
                                                                          measuredItemISSCFC_exp,
                                                                          Selection, type)]
    
    # Update mapping, 4 cases considered: 
    # 1. The mapping does not change
    # 2. The mapping for the selected commodity does not change but other row are added for new commodities
    # N.B. if a mapping already existing is added but already existing nothing happens since nrow(setdiff)=0 
    # 3. The mapping changes but for the same period, i.e. the user changes his/her mind about the mapping
    # 4. Everything changes
    
    
    if(nrow(setdiff(exp_part_upd[, .(geographicAreaM49_fi, 
                                     measuredItemISSCFC, 
                                     measuredItemISSCFC_exp,
                                     end_year,
                                     Selection, type)], 
                    corresponding_mapping_export[, .(geographicAreaM49_fi, 
                                                     measuredItemISSCFC, 
                                                     measuredItemISSCFC_exp,
                                                     end_year,
                                                     Selection, type)]))==0  & 
       nrow(exp_part_upd) == nrow(corresponding_mapping_export) &
       all(exp_part_upd$measuredItemISSCFC_exp == corresponding_mapping_export$measuredItemISSCFC_exp)){ 
      # case of no update (no selection or unselection and no new entry)
      # nothing changes
      export_mapping_upd <- map_prod_exp
      
    } else if(nrow(setdiff(exp_part_upd[, .(geographicAreaM49_fi, measuredItemISSCFC, 
                                            measuredItemISSCFC_exp, end_year,
                                            Selection, type)], 
                           corresponding_mapping_export[ , .(geographicAreaM49_fi, measuredItemISSCFC, 
                                                             measuredItemISSCFC_exp, end_year, Selection, type)])) == 0 & 
              nrow(exp_part_upd) != nrow(corresponding_mapping_export) ) { 
      # case of adding commodities without changing mapping of selected commodity
      
      # identify additional part
      exp_part2upd <- exp_part_upd[!measuredItemISSCFC %in% unique(corresponding_mapping_export$measuredItemISSCFC), ]
      #if an export commodity has been added
      exp_part3upd <- exp_part_upd[!measuredItemISSCFC_exp %in% unique(corresponding_mapping_export$measuredItemISSCFC_exp), ]
      
      # if a row has been deleted then FALSE is placed at the corresponding row
      if(corresponding_mapping_export[!measuredItemISSCFC_exp %in% exp_part_upd$measuredItemISSCFC_exp] |
         corresponding_mapping_export[!measuredItemISSCFC %in% exp_part_upd$measuredItemISSCFC] ){
        
        isscfc_expNo <- corresponding_mapping_export[!measuredItemISSCFC_exp %in% exp_part_upd$measuredItemISSCFC_exp]$measuredItemISSCFC_exp
        isscfcNo <- corresponding_mapping_export[!measuredItemISSCFC %in% exp_part_upd$measuredItemISSCFC]$measuredItemISSCFC
        
        map_prod_exp[geographicAreaM49_fi == sel_country &
                       measuredItemISSCFC == isscfcNo &
                       end_year == "LAST", ]$Selection <- FALSE
        
        map_prod_exp[geographicAreaM49_fi == sel_country &
                       measuredItemISSCFC_exp == isscfc_expNo &
                       end_year == "LAST", ]$Selection <- FALSE
      }
      
      export_mapping_upd <- rbind(map_prod_exp[ , .(geographicAreaM49_fi, start_year, end_year, measuredItemISSCFC, measuredItemISSCFC_exp, Selection, type)], 
                                  exp_part2upd, exp_part3upd)
      
    } else if(any(exp_part_upd$start_year %in% map_prod_exp[ geographicAreaM49_fi == sel_country &
                                                             measuredItemISSCFC == unique(exp_part_upd$measuredItemISSCFC) &
                                                             end_year == "LAST",]$start_year)){ 
      # changing mapping for the same imputation year
      
      new_map <- merge(map_prod_exp, exp_part_upd, by = c("geographicAreaM49_fi",
                                                          "start_year",
                                                          "end_year",
                                                          "measuredItemISSCFC",
                                                          "measuredItemISSCFC_exp"),
                       all = TRUE, suffixes = c("_previous", "_updated"))
      
      new_map$Selection <- ifelse(is.na(new_map$Selection_updated), new_map$Selection_previous, new_map$Selection_updated)
      new_map$type <- ifelse(is.na(new_map$type_updated), new_map$type_previous, new_map$type_updated)
      
      new_map[ , c("Selection_previous", "Selection_updated", "type_previous", "type_updated") := NULL]
      
      startyear <- exp_part_upd[measuredItemISSCFC == sel_isscfc, ]$start_year
      
      new_map[geographicAreaM49_fi == sel_country &
                measuredItemISSCFC %in% unique(exp_part_upd$measuredItemISSCFC) &
                end_year == "LAST" & start_year == startyear &
                !measuredItemISSCFC_exp %in% exp_part_upd$measuredItemISSCFC_exp ]$Selection <- FALSE
      
      
      export_mapping_upd <- new_map
      
    } else {
      
      map_prod_exp_mod <- copy(map_prod_exp)
      
      map_prod_exp_mod[geographicAreaM49_fi == sel_country &
                         measuredItemISSCFC %in% unique(exp_part_upd$measuredItemISSCFC) & 
                         end_year == "LAST", end_year := as.character(as.numeric(input$btn_year)-1)]
      
      export_mapping_upd <- rbind(map_prod_exp_mod[ , .(geographicAreaM49_fi, type, start_year, end_year, measuredItemISSCFC, measuredItemISSCFC_exp, Selection)],
                                  exp_part_upd)
      
    }
    
    rv_mappingtable$map_prod_exp <- export_mapping_upd
    
    showModal(modalDialog(
      title = "Update export-based approach mapping." ,
      sprintf("The mapping table has been updated and it is diplayed in the bottom table.")
    ))
    
  })
  
  #-- Save export mapping in SWS ----
  observeEvent(input$save_export_mapping, {
    
    if(is.null(input$isscfc_check_data)) return(NULL)
    
    commodity_label <- rv_data$commodity_label
    
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
 
    map_prod_exp0 <- rv_mappingtable$map_prod_exp0
    
    export_mapping_upd <- mapping_rv$mapping_table_exp #input$check_export_mapping_data # rhandsontable::hot_to_r(input$check_export_mapping_data)
    
    export_mapping_upd_sws_compliant <- copy(export_mapping_upd)
    export_mapping_upd_sws_compliant <- setnames(export_mapping_upd_sws_compliant,
                                                 c("geographicAreaM49_fi", "start_year",
                                                   "end_year", "measuredItemISSCFC",
                                                   "measuredItemISSCFC_exp", "Selection"),
                                                 c("geographic_area_m49_fi", "start_year",
                                                   "end_year", "measured_item_isscfc",
                                                   "measured_item_isscfc_exp", "selection"))
    setkey(export_mapping_upd_sws_compliant, geographic_area_m49_fi, start_year, end_year, measured_item_isscfc, measured_item_isscfc_exp, selection, type)
    export_mapping_upd_sws_compliant <- unique(export_mapping_upd_sws_compliant)
    
    withProgress(message = 'Updating Export Mapping',
                 value = 0, {
                   
                   Sys.sleep(0.25)
                   incProgress(0.25)
                   
                   changeset <- Changeset('isscfc_mapping_export_approach')
                   AddDeletions(changeset, map_prod_exp0[ geographic_area_m49_fi == sel_country, ])
                   Finalise(changeset)
                   
                   Sys.sleep(0.25)
                   incProgress(0.5)
                   
                   changeset <- Changeset('isscfc_mapping_export_approach')
                   AddInsertions(changeset, export_mapping_upd_sws_compliant)
                   Finalise(changeset)
                   
                   Sys.sleep(0.25)
                   incProgress(0.65)
                   
                   whereMap <- paste("geographic_area_m49_fi = '", sel_country, "' ", sep = "") 
                   
                   map_prod_exp0 <- ReadDatatable('isscfc_mapping_export_approach', readOnly = FALSE, where = whereMap)
                   rv_mappingtable$map_prod_exp0 <- map_prod_exp0
                   
                   map_prod_exp <- copy(map_prod_exp0)
                   map_prod_exp <- setnames(map_prod_exp, 
                                            old = c("geographic_area_m49_fi",
                                                    "start_year", "end_year", "measured_item_isscfc",
                                                    "measured_item_isscfc_exp", "selection", "type"), 
                                            new = c("geographicAreaM49_fi", 
                                                    "start_year", "end_year", "measuredItemISSCFC", 
                                                    "measuredItemISSCFC_exp", "Selection", "type"))
                   
                   rv_mappingtable$map_prod_exp <- map_prod_exp
                   
                   
                   Sys.sleep(0.25)
                   incProgress(0.95)
                   
                 })
    
    showModal(modalDialog(
      title = "Update export-based approach mapping." ,
      sprintf("The export approach mapping table has been updated in the SWS. SWS datatable name: Save Export Mapping.")
    ))
    
  })
  
  
  #-- Add row to Prod mapping ----
  
  prodAdd_reac <- reactive({ 
    
    req(input$btn_country, input$btn_year, input$btn_commodity)
    
    # parameters explicited from buttons
    commodity_label <- rv_data$commodity_label
    sel_years <- input$btn_start_year:input$btn_year
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]

    if(nrow(rv_mappingtable$map_prod_prod)  > 0){
      map_prod_prod <- rv_mappingtable$map_prod_prod
      
      map_prod_filtered <- map_prod_prod[start_year <= input$btn_year & 
                                              input$btn_year < end_year & 
                                              measuredItemISSCFC %in% sel_isscfc, ]
      
      if(nrow(map_prod_filtered) > 0){
        prod_map <- map_prod_filtered[ ,.(# geographicAreaM49_fi,
          # start_year,
          # end_year,
          measuredItemISSCFC,
          isscaap,
          fisheriesAsfis,
          Ratio,
          Selection, type)]
      } else{
        
        prod_map <-  data.table(# geographicAreaM49_fi = sel_country,
          # start_year = input$btn_year, 
          # end_year = 'LAST',
          measuredItemISSCFC = '',
          isscaap = '',
          fisheriesAsfis = 'all',
          Ratio = 1,
          Selection = FALSE,
          type = 'provided') 
      }
      
    } else {
      prod_map <-  data.table(# geographicAreaM49_fi = sel_country,
        # start_year = input$btn_year, 
        # end_year = 'LAST',
        measuredItemISSCFC = '',
        isscaap = '',
        fisheriesAsfis = 'all',
        Ratio = 1,
        Selection = FALSE,
        type = 'provided') 
    }
    
    return(prod_map)
    
  })
  
  output$prodAdd <-  renderRHandsontable({
    
    req(input$btn_country, input$btn_year, input$btn_commodity)
    
    prod_map <- prodAdd_reac()
    rhandsontable(prod_map, rowHeaders = NULL, width = 'auto', height = 'auto')
    
  })
  
  # -- Update Prod mapping ----
  
  observeEvent(input$add_prod_mapping, {
    
    if(is.null(input$prodAdd)) return(NULL)
    
    commodity_label <- rv_data$commodity_label
    map_prod_prod <- rv_mappingtable$map_prod_prod
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
    sel_years <- input$btn_start_year:input$btn_year
    sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]
    imputYear <- input$btn_year
    
    prod_part_upd <- rhandsontable::hot_to_r(input$prodAdd)
    prod_part_upd[is.na(fisheriesAsfis), fisheriesAsfis := 'all']
    prod_part_upd[is.na(Ratio), Ratio := 1]
    prod_part_upd[is.na(type), type := 'provided']
    
    prod_part_upd <- prod_part_upd[, c("geographicAreaM49_fi", "end_year", "start_year") := list(sel_country, "LAST", input$btn_year) ]
    
    prod_part_upd <- prod_part_upd[, .(geographicAreaM49_fi,
                                      start_year,
                                      end_year,
                                      measuredItemISSCFC,
                                      isscaap,
                                      fisheriesAsfis, Ratio,
                                      Selection, type)]
    
    corresponding_mapping_prod <- map_prod_prod[ geographicAreaM49_fi == sel_country &
                                                   measuredItemISSCFC %in% unique(prod_part_upd$measuredItemISSCFC) &
                                                   end_year == "LAST", .(geographicAreaM49_fi,
                                                                         start_year,
                                                                         end_year,
                                                                         measuredItemISSCFC,
                                                                         isscaap,
                                                                         fisheriesAsfis,
                                                                         Ratio,
                                                                         Selection,
                                                                         type)]
    
    if(nrow(corresponding_mapping_prod) > 0 & all(corresponding_mapping_prod$start_year != prod_part_upd$start_year)){
      
      map_prod_prod[measuredItemISSCFC %in% unique(prod_part_upd$measuredItemISSCFC), end_year :=  as.character((as.numeric(imputYear) - 1))]
      
    } 
    
    if(all(corresponding_mapping_prod$start_year != prod_part_upd$start_year)){
    prod_mapping_upd <- rbind(map_prod_prod[ , .(geographicAreaM49_fi, start_year, end_year, 
                                                     measuredItemISSCFC, isscaap, fisheriesAsfis, 
                                                     Ratio, Selection, type)],
                              prod_part_upd[ , .(geographicAreaM49_fi, start_year, end_year, 
                                                    measuredItemISSCFC, isscaap, fisheriesAsfis, 
                                                    Ratio, Selection, type)])
    } else {
      
      
      new_map_prod <- merge(corresponding_mapping_prod, prod_part_upd, by = c("geographicAreaM49_fi",
                                                                              "start_year",
                                                                              "end_year",
                                                                              "measuredItemISSCFC",
                                                                              "isscaap", 
                                                                              "fisheriesAsfis"),
                            all.y = TRUE, suffixes = c("_previous", "_updated"))
    
      
      
      new_map_prod$Selection <- ifelse(is.na(new_map_prod$Selection_updated), new_map_prod$Selection_previous, new_map_prod$Selection_updated)
      new_map_prod$Ratio <- ifelse(is.na(new_map_prod$Ratio_updated), new_map_prod$Ratio_previous, new_map_prod$Ratio_updated)
      new_map_prod$type <- ifelse(is.na(new_map_prod$type_updated), new_map_prod$type_previous, new_map_prod$type_updated)
      
      new_map_prod[ , c("Selection_previous", "Selection_updated", 
                        "Ratio_previous", "Ratio_updated",
                        "type_previous", "type_updated") := NULL]
      
      new_map_prod <- new_map_prod[ , .(geographicAreaM49_fi,
                                            start_year,
                                            end_year,
                                            measuredItemISSCFC,
                                            isscaap,
                                            fisheriesAsfis,
                                            Ratio,
                                            Selection,
                                            type)]
      
      setkey(corresponding_mapping_prod)
      setkey(map_prod_prod)
      prod_mapping_upd <- rbind(map_prod_prod[!corresponding_mapping_prod, .(geographicAreaM49_fi, start_year, end_year, 
                                                   measuredItemISSCFC, isscaap, fisheriesAsfis, 
                                                   Ratio, Selection, type)],
                                new_map_prod[ , .(geographicAreaM49_fi, start_year, end_year, 
                                                   measuredItemISSCFC, isscaap, fisheriesAsfis, 
                                                   Ratio, Selection, type)])
    }
    
    rv_mappingtable$map_prod_prod <- prod_mapping_upd
    
    showModal(modalDialog(
      title = "Update Primary Production-based approach mapping." ,
      sprintf("The mapping table has been updated and it is diplayed in the bottom table.")
    ))
    
  })
  
  
  # -- Save production mapping ----
  
  check_prod_mapping_reac <- reactive({
    
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    sel_country <- country_input[country_input$label == input$btn_country, code]
    
    map_prod_prod <- rv_mappingtable$map_prod_prod
    
    return(map_prod_prod[geographicAreaM49_fi == sel_country, ])
  })
  
  
  output$check_prod_mapping_data <- DT::renderDataTable( server = FALSE, {
    mapping_rv$mapping_table_prod <- check_prod_mapping_reac()
    DT::datatable(mapping_rv$mapping_table_prod[ , .(geographicAreaM49_fi, start_year,
                                                     end_year, measuredItemISSCFC,
                                                     isscaap, fisheriesAsfis, Ratio, Selection, type)], 
                  extensions = 'Buttons', filter = 'top',
                  options = list(
                    dom = 'Bfrtip',
                    buttons = c('csv', 'excel', 'pdf')) # , editable = TRUE
    )
    
  })
  
  
  observeEvent(input$save_prod_mapping, {
    
    if(nrow(rv_mappingtable$map_prod_prod) == 0) return(NULL)
    # Needed element (transform from label + code to just code)
    sel_country <- country_input[country_input$label == input$btn_country, code]
    
    map_prod_prod0 <- rv_mappingtable$map_prod_prod0
    
    tab_updated_prod <- mapping_rv$mapping_table_prod
    
    tab_updated_prod_sws_compliant <- copy(tab_updated_prod)
    tab_updated_prod_sws_compliant <- setnames(tab_updated_prod_sws_compliant, 
                                               c("geographicAreaM49_fi", "start_year",
                                                  "end_year", "measuredItemISSCFC", 
                                                  "isscaap", "fisheriesAsfis", "Ratio", "Selection"),
                                               c("geographic_area_m49_fi", "start_year",
                                                 "end_year", "measured_item_isscfc",
                                                 "isscaap", "asfis", "ratio", "selection"))
    
    setkey(tab_updated_prod_sws_compliant, geographic_area_m49_fi, start_year, end_year, measured_item_isscfc, isscaap, asfis)
    
    if(nrow(tab_updated_prod_sws_compliant[duplicated(tab_updated_prod_sws_compliant)]) >0){
      showModal(modalDialog(
        title = "Saving not possible. Duplicates in primary production mapping table." ,
        paste('Check mapping rows:', 
              tab_updated_prod_sws_compliant[duplicated(tab_updated_prod_sws_compliant)])
        ))
    } else {
    
    setkey(tab_updated_prod_sws_compliant, geographic_area_m49_fi, start_year, end_year, measured_item_isscfc, isscaap, asfis, ratio, selection, type)
    tab_updated_prod_sws_compliant <- unique(tab_updated_prod_sws_compliant)
    
    withProgress(message = 'Updating Primary production Mapping',
                 value = 0, {
                   
                   Sys.sleep(0.25)
                   incProgress(0.25)
                   
                   changeset <- Changeset('isscfc_mapping_prod_approach')
                   AddDeletions(changeset, map_prod_prod0[ geographic_area_m49_fi == sel_country, ])
                   Finalise(changeset)
                   
                   Sys.sleep(0.25)
                   incProgress(0.5)
                   
                   changeset <- Changeset('isscfc_mapping_prod_approach')
                   AddInsertions(changeset, tab_updated_prod_sws_compliant)
                   Finalise(changeset)
                   
                   Sys.sleep(0.25)
                   incProgress(0.75)
                   
                   whereMap <- paste("geographic_area_m49_fi = '", sel_country, "' ", sep = "") 
                   
                   map_prod_prod0 <-  ReadDatatable('isscfc_mapping_prod_approach', readOnly = FALSE, where = whereMap)
                   
                   rv_mappingtable$map_prod_prod0 <- map_prod_prod0
                   
                   map_prod_prod <- copy(map_prod_prod0)
                   map_prod_prod <- setnames(map_prod_prod, 
                                             old = c("geographic_area_m49_fi", "measured_item_isscfc", 
                                                     "asfis", "ratio", "selection"), 
                                             new = c("geographicAreaM49_fi", "measuredItemISSCFC", 
                                                     "fisheriesAsfis", "Ratio", "Selection"))
                   
                   map_prod_prod$Ratio <- as.numeric(map_prod_prod$Ratio)
                   rv_mappingtable$map_prod_prod <- map_prod_prod
                   
                   Sys.sleep(0.25)
                   incProgress(0.95)
                   
                 })
    showModal(modalDialog(
      title = "Update primary production-based approach mapping." ,
      paste("The primary production approach mapping table has been updated in the SWS. 
            SWS datatable name: Save Primary Prod. Mapping.")
    ))
    }
    
  })
  
  
  # -- Total by ISSCFC ----
  
  aggregates_reac <- reactive({
    
    req(input$btn_country, input$btn_year, input$btn_start_year)
    
    data <- rv_data$procprod_imp
    data$Value <- as.numeric(data$Value)
    if(input$btn_total == 'Grand Total'){
      # Grand total
      grandtotal <- copy(data)
      grandtotal <- grandtotal[ , Value := sum(Value, na.rm = TRUE), by = c("geographicAreaM49_fi", 
                                                                            "measuredElement",
                                                                            "timePointYears")]
      grandtotal <- unique(grandtotal[ , .(geographicAreaM49_fi, timePointYears, Value)])
      grandtotal[ , measuredElement := 'Grand Total']
      
      tab2show <- grandtotal
      tab2show <-  dcast(tab2show, geographicAreaM49_fi + measuredElement ~ timePointYears, value.var = c("Value"))
      
    } else if(input$btn_total == 'Yearbook Total'){
      # Yearbook Total
      
      YBtotal <- copy(data)
      YBtotal <- YBtotal[ , Group := sub("\\..*",'',  YBtotal$Scheda)]
      YBtotal$Group <- as.numeric(YBtotal$Group)
      YBtotal <- YBtotal[ Group <= 7 ]
      YBtotal[, Value := sum(Value, na.rm = TRUE), by = c("geographicAreaM49_fi",
                                                          "measuredElement", 
                                                          "timePointYears")]
      
      YBtotal <- unique(YBtotal[ , .(geographicAreaM49_fi, timePointYears, Value)])
      YBtotal[ , measuredElement := 'Yearbook Total']
      tab2show <- YBtotal
      tab2show <-  dcast(tab2show, geographicAreaM49_fi + measuredElement ~ timePointYears, value.var = c("Value"))
      
    } else if(input$btn_total == 'Main groups'){
      
      # Main groups
      FAOtotal <- copy(data)
      FAOtotal <- FAOtotal[ , Group := sub("\\..*",'',  FAOtotal$Scheda)]
      FAOtotal <- FAOtotal[ , Value := sum(Value, na.rm = TRUE), by = c("geographicAreaM49_fi",
                                                                        "measuredElement", 
                                                                        "timePointYears",
                                                                        "Group")]
      
      FAOtotal <- unique(FAOtotal[ , .(geographicAreaM49_fi, timePointYears, Group, Value)])
      FAOtotal[ , measuredElement := 'Main groups']
      tab2show <- FAOtotal
      tab2show <-  dcast(tab2show, geographicAreaM49_fi + measuredElement + Group ~ timePointYears, value.var = c("Value"))
      
    } else if(input$btn_total == 'ISSCFC Total'){
      # Isscfc total
      ISSCFCtotal <- copy(data)
      ISSCFCtotal <- ISSCFCtotal[ , Value := sum(Value, na.rm = TRUE), by = c("geographicAreaM49_fi", 
                                                                              "measuredElement",
                                                                              "timePointYears", 
                                                                              "measuredItemISSCFC")]
      
      ISSCFCtotal <- unique(ISSCFCtotal[ , .(geographicAreaM49_fi, timePointYears, measuredItemISSCFC, Value)])
      ISSCFCtotal[ , measuredElement := 'ISSCFC Total']
      tab2show <- ISSCFCtotal
      tab2show <-  dcast(tab2show, geographicAreaM49_fi + measuredItemISSCFC ~ timePointYears, value.var = c("Value"))
      
    } else if(input$btn_total == ''){
      #  # NEED NEW DATATABLE FOR ISSCFC GROUPS!!! isscfc2fao <- ReadDatatable('fi_commodity_aggregates_comm')
      tab2show <- data.table()
    }
    
    return(tab2show)
    
  })
  
  
  output$aggregates_data <- DT::renderDataTable( server = FALSE, {
    agg <- aggregates_reac()
    DT::datatable(agg, extensions = 'Buttons', filter = 'top', rownames = FALSE)
    
  })
  
  
  
  output$gg_tot_isscfc <- renderPlot({ 
    
    req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_commodity, input$btn_missing)
    if(input$btn_total == 'ISSCFC Total'){
      commodity_label <- rv_data$commodity_label
      procprodimp <- rv_data$procprod_imp
      commodityDB <- rv_data$commodityDB
      
      # if(is.null(input$isscfc_check_data)) return(NULL)
      
      sel_country <- country_input[country_input$label == input$btn_country, code]
      sel_commodity <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, code]
      sel_years <- input$btn_start_year:input$btn_year
      sel_isscfc <- commodity_label[M49 == sel_country & commodity_label$label == input$btn_commodity, isscfc]
      
      procprod_isscfc <- procprodimp[measuredItemISSCFC == sel_isscfc, ]
      procprod_isscfc$Value <- as.numeric(procprod_isscfc$Value)
      
      ggplot(procprod_isscfc, aes(timePointYears, Value, group = Scheda, fill = Scheda)) +
        geom_bar(stat="identity") +
        geom_text(aes(label = Value), position = position_stack(vjust = .5), size = 3)  +
        stat_summary(fun.y = sum, aes(label = ..y.., group = timePointYears), geom = "text", vjust = -.5) +
        labs(y = 'Quantity (in tonnes)', color = '', title = 'Scheda aggregation by ISSCFC series') +
        theme_minimal()
    }
    # ggplot(data = xxx, aes(x = timePointYears, y = Value)) +
    #   geom_line(aes(group = Scheda, color = Scheda), size = 1) +
    #   labs(y = 'Quantity (in tonnes)', color = '', title = 'Scheda aggregation by ISSCFC series') +
    #   theme_minimal() +
    #   theme(legend.position = 'bottom')
    
    
  })
  
} # end of the function