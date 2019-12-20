#-- myAggregate by Thomas Berger to aggregate Capture and Aqualculture data ----

myAggregate<- function(quantity, flag) {
  if (length(quantity) == 1) {
    # for a single observation, just use the flag
    return (as.character(flag))
  }
  
  # for more than 1 observation: aggregate by flags
  agg1 <- aggregate(quantity, by=list(flag), FUN=sum)
  # agg1 has 2 variables: group, num
  
  # order by quantity, largest quantity first
  agg1 <- agg1[ order(-agg1[,2]), ]
  
  
  # if the total quantity>0 and the flag of te largest value is 'E' or '' then use it
  if (sum(quantity)>0  & (agg1[1,1] == "E" | agg1[1,1] == "" | agg1[1,1] == "I")) {
    return (as.character(agg1[1,1]))
  }
  
  # if there is a single "N" use it (takes precedence)
  if (nrow(agg1[agg1[,1] == "N",])>0) {
    return("N")
  }
  
  # if there is one "M" use it
  if (nrow(agg1[agg1[,1] == "M",])>0) {
    return("M")
  }
  
  # otherwise it's ' '
  return("")
}


#-- expandYear function ----
# This function is sourced from this loca file instead of the proper package faoswsProcessing
# because of recent updates not yet included in the CRAN version of the faoswsProcessing package.
# This file should disappear once faoswsProcessing is up-to-date in the CRAN
expandYear <- function (data, areaVar = "geographicAreaM49", elementVar = "measuredElement", 
                        itemVar = "measuredItemCPC", yearVar = "timePointYears", 
                        valueVar = "Value", obsflagVar = "flagObservationStatus", 
                        methFlagVar = "flagMethod", newYears = NULL) 
{
  key = c(elementVar, areaVar, itemVar)
  keyDataFrame = data[, key, with = FALSE]
  keyDataFrame = keyDataFrame[with(keyDataFrame, order(get(key)))]
  keyDataFrame = keyDataFrame[!duplicated(keyDataFrame)]
  yearDataFrame = unique(data[, get(yearVar)])
  if (!is.null(newYears)) {
    yearDataFrame = unique(c(yearDataFrame, newYears, newYears - 
                               1, newYears - 2))
  }
  yearDataFrame = data.table(yearVar = yearDataFrame)
  colnames(yearDataFrame) = yearVar
  completeBasis = data.table(merge.data.frame(keyDataFrame, 
                                              yearDataFrame))
  expandedData = merge(completeBasis, data, by = colnames(completeBasis), 
                       all.x = TRUE)
  expandedData = fillRecord(expandedData, areaVar = areaVar, 
                            itemVar = itemVar, yearVar = yearVar)
  seriesToBlock = expandedData[(get(methFlagVar) != "u"), ]
  seriesToBlock[, `:=`(lastYearAvailable, max(get(yearVar))), 
                by = key]
  seriesToBlock[, `:=`(flagComb, paste(get(obsflagVar), get(methFlagVar), 
                                       sep = ";"))]
  seriesToBlock = seriesToBlock[get(yearVar) == lastYearAvailable & 
                                  flagComb == "M;-"]
  if (nrow(seriesToBlock) > 0) {
    seriesToBlock = seriesToBlock[, {
      max_year = max(as.integer(.SD[, timePointYears]))
      data.table(timePointYears = seq.int(max_year + 1, 
                                          newYears), Value = NA_real_, flagObservationStatus = "M", 
                 flagMethod = "-")[max_year < newYears]
    }, by = key]
    expandedData = merge(expandedData, seriesToBlock, by = c(areaVar, 
                                                             elementVar, itemVar, yearVar), all.x = TRUE, suffixes = c("", 
                                                                                                                       "_MDash"))
    expandedData[!is.na(flagMethod_MDash), `:=`(flagMethod, 
                                                flagMethod_MDash)]
    expandedData = expandedData[, colnames(data), with = FALSE]
  }
  expandedData
}


#-- computeEnsemble ----
# This function is sourced from this loca file instead of the proper package faoswsImputation
# because of recent updates not yet included in the CRAN version of the faoswsImputation package.
# This file should disappear once faoswsImputation is up-to-date in the CRAN

computeEnsemble <- function (fits, weights, errors) 
{
  stopifnot(all(names(fits) %in% names(weights)))
  stopifnot(all(names(weights) %in% names(fits)))
  fits = fits[names(weights)]
  stopifnot(all(names(weights) == names(fits)))
  stopifnot(length(fits) == ncol(weights))
  if (!all(sapply(fits, length) == nrow(weights))) 
    stop("Length of fits do not match nrow(weights)!")
  fitsMatrix = matrix(unlist(fits), ncol = length(fits))
  weightedFit = fitsMatrix * weights
  errorFit = errors * weights
  ensemble = data.table(fit = apply(weightedFit, 1, function(x) sum(x, 
                                                                    na.rm = !all(is.na(x)))), variance = apply(errorFit, 
                                                                                                               1, sum, na.rm = TRUE))
  fitsMatrix = fitsMatrix[, !is.na(apply(fitsMatrix, 2, unique))]
  
  modelMin = apply(fitsMatrix, 2, min, na.rm = TRUE)
  if (any(modelMin < 0)) {
    negMod = which(modelMin < 0)
    stop("Imputation gave negative result")
  }
  ensemble
}


#-- Encoding ----


replaceforeignchars <- function(dat)
{
  fromto <- read.table(text="
from to
                       š s
                       Å A
                       œ oe
                       ž z
                       ß ss
                       þ y
                       à a
                       á a
                       â a
                       ã a
                       ä a
                       å a
                       æ ae
                       ç c
                       è e
                       é e
                       ê e
                       ë e
                       ì i
                       í i
                       î i
                       ï i
                       ð d
                       ñ n
                       ò o
                       ó o
                       ô o
                       õ o
                       ö o
                       ø oe
                       ù u
                       ú u
                       û u
                       ü u
                       ý y
                       ÿ y
                       ğ g",
                       header=TRUE)
  
  for(i in 1:nrow(fromto) ) {
    dat <- gsub(fromto$from[i],fromto$to[i],dat)
  }
  dat
}

#-- Export approach function ----
export_imputation <- function(datatab, sel_year, manual_ratio_exp_input = NULL,
                               missing_data){
  
  # Vector to select/unselect ISSCFC_exp to include, modifiable by user
  datatab$Selection
  
  # Sum export isscfc items that match to the same production code by some variable
  # Variables excluded are:   "nationalcode", "nationaldescription", "remarks", "measureditemnational",
  # "approach", "start_year", "end_year", "type", "measuredElement_prod", "nationalquantity",
  # "nationalquantityunit", "id_isscfc", "measuredElement_exp"
  
  aggregate_exp <- datatab[, list(Value = sum(Value_exp*Selection, na.rm = TRUE),
                                  flagObservationStatusAggr = max(flagObservationStatus_exp, na.rm = TRUE),
                                  flagMethodAggr = ifelse(nrow(datatab)>1, "s", flagMethod_exp)),
                           by = c("geographicAreaM49_fi",
                                  "Scheda",
                                  "measuredItemISSCFC",
                                  "timePointYears",
                                  "Value_prod",
                                  "flagObservationStatus_prod",
                                  "flagMethod_prod")]
  
  setnames(aggregate_exp, old = c("Value", "flagObservationStatusAggr", "flagMethodAggr") , 
           new = c("Aggregate_exp", "flagObservationStatus_exp", "flagMethod_exp"))
  
  aggregate_exp$Value_prod <- as.numeric(aggregate_exp$Value_prod)
  
  # Create ratio variable
  aggregate_exp$ratio <- ifelse(aggregate_exp$Aggregate_exp > 0 & !is.na(aggregate_exp$Value_prod), 
                                aggregate_exp$Value_prod / aggregate_exp$Aggregate_exp,
                                0)
  
  # 
  tab_prev <- aggregate_exp[timePointYears != sel_year]
  
  ## Make a table with production codes and the average ratio for years before the chosen year
  
  # Calculate average ratio
  average_ratio <- tab_prev[, mean(ratio, na.rm = TRUE), by = c("Scheda")]
  setnames(average_ratio, old = "V1", new = "AvRatio")
  
  # Calculate average production for the previous selected years
  average_prod <- tab_prev[, mean(Value_prod, na.rm = TRUE), by = c("Scheda")]
  setnames(average_prod, old = "V1", new = "AvProd")
  
  # Calculate average exports for the previous selected years
  average_exp <- tab_prev[, mean(Aggregate_exp, na.rm = TRUE), by =  c("Scheda")]
  setnames(average_exp, old = "V1", new = "AvExp")
  
  # Merge the average ratio table with the main table (tab2)
  tab_avRatio <- merge(aggregate_exp, average_ratio,
                       by = c("Scheda"),
                       all.x = TRUE, allow.cartesian = TRUE)
  
  # Adding average production and export to the table
  tab_avProd <- merge(tab_avRatio, average_prod,
                      by = c("Scheda"),
                      all.x = TRUE, allow.cartesian = TRUE)
  
  tab_avExp <- merge(tab_avProd, average_exp,
                     by = c("Scheda"),
                     all.x = TRUE, allow.cartesian = TRUE)
  
  
  # Value of the production for the selected year is computed 
  # multiplying the export of the selected year times the average ratio calculated
  
  if(is.na(manual_ratio_exp_input) & is.na(missing_data)){
    
    
    tab_avExp$Value_prod <- ifelse(tab_avExp$timePointYears == sel_year,
                                   tab_avExp$Aggregate_exp * tab_avExp$AvRatio,
                                   tab_avExp$Value_prod)
    
    # Put a dash at the average ratio of the selected year
    tab_avExp$ratio <- ifelse(tab_avExp$timePointYears == sel_year, NA, tab_avExp$ratio)
    
  } else if(is.na(manual_ratio_exp_input) & !is.na(missing_data)){
    
    # tab6$Value_prod <- tab6$Value_prod
    
    # Put a dash at the average ratio of the selected year
    tab_avExp$ratio <- ifelse(tab_avExp$timePointYears == sel_year, 
                              missing_data/tab_avExp$Aggregate_exp,
                              tab_avExp$ratio)
    
  } else {
    
    tab_avExp$Value_prod <- ifelse(tab_avExp$timePointYears == sel_year,
                                   tab_avExp$Aggregate_exp * manual_ratio_exp_input,
                                   tab_avExp$Value_prod)
    
    # Put a dash at the average ratio of the selected year
    tab_avExp$ratio <- ifelse(tab_avExp$timePointYears == sel_year, manual_ratio_exp_input, tab_avExp$ratio)
    
    
  }
  
  ExpFlags <- combineFlag(tab_avExp, "flagObservationStatus_exp", "flagMethod_exp") 
  ProdFlags <-  combineFlag(tab_avExp, "flagObservationStatus_prod", "flagMethod_prod") 
  row1 <- c(as.vector(rbind(round(tab_avExp$Value_prod), ProdFlags)), round(average_prod$AvProd[1]))
  row2 <- c(as.vector(rbind(round(tab_avExp$Aggregate_exp), ExpFlags)),  round(average_exp$AvExp[1]))
  row3 <- c(as.vector(rbind(round(tab_avExp$ratio, 3), rep("",length(tab_avExp$ratio)))) , round(average_ratio$AvRatio[1], 3))
  
  # Table that is shown on the app
  format2show <- rbind(row1, row2, row3)
  columnNames <- as.vector(rbind(tab_avExp$timePointYears, rep("Flag", length(tab_avExp$timePointYears))))
  colnames(format2show) <- c(columnNames, "Average")
  
  finaltab <- as.data.table(format2show)
  finaltab <- cbind('Stats' = c("Processed prod volume", "Exports volume", "Ratio"), finaltab)
  
  return(finaltab)
  
}


#-- Primary production approach function ----


primaryprod_imputation1 <- function(commodityDB, globalProduction, procprod, # Dataset 
                                    sel_year,
                                    map_asfis, mappingTable # mapping data.tables
){
  
  # removing unneeded columns
  # mappingTable[ , c('__id', '__ts') := NULL]
  
  # merge production data with mapping
  cdb_prod_isscaap <- merge(procprod, mappingTable, 
                            by = c("geographicAreaM49_fi", "measuredItemISSCFC"),
                            all.x = TRUE,
                            allow.cartesian = TRUE)
  
  # Merge mapping and data
  
  # Merge when all species in the iscaap group are selected
  cdb_cfc_asfis_all <- merge(cdb_prod_isscaap[fisheriesAsfis == "all", ], 
                             map_asfis, by = c("isscaap"), all.x = TRUE,
                             allow.cartesian = TRUE, suffixes = c('_selected', '_mapped')) 
  
  # If  fisheriesAsfis_selected != 'all' set Selection == FALSE to all other Asfis codes in the same isscaap 
  # but the one in fisheriesAsfis_selected
  
  # rows2change_all <- cdb_cfc_asfis_all[which(cdb_cfc_asfis_all$fisheriesAsfis_selected != "all"), ]
  # 
  # # This substitution is happening for all years selected (changes will be accounted for after the renderRHandsontable: asfis_check_data)
  # if(nrow(rows2change_all) > 0){
  #   isscaap2change_all <- unique(rows2change_all$isscaap)
  #   # change 
  #   for(i in 1:length(isscaap2change_all)){
  #     cdb_cfc_asfis_all[isscaap == isscaap2change_all[i] & !fisheriesAsfis_mapped %in% rows2change_all[isscaap == isscaap2change_all[i] ]$fisheriesAsfis_selected, Selection := FALSE]
  #   }
  # }
  
  cdb_cfc_asfis_all[ , c("fisheriesAsfis_selected") := NULL ]
  setnames(cdb_cfc_asfis_all, old = "fisheriesAsfis_mapped", new = "fisheriesAsfis")
  
  # Include only species for which there are series in the country in Global Prod dataset
  cdb_cfc_asfis_all <- cdb_cfc_asfis_all[fisheriesAsfis %in% globalProduction$fisheriesAsfis]
  
  # Merge when only some species in the iscaap group are selected
  cdb_cfc_asfis_some <- merge(cdb_prod_isscaap[fisheriesAsfis != "all",], 
                              map_asfis[isscaap %in% unique(cdb_prod_isscaap[fisheriesAsfis != "all",]$isscaap)], 
                              by = c("isscaap", "fisheriesAsfis"), all.x = TRUE,
                              allow.cartesian = TRUE) 
  # Include only species for which there are series in the country in Global Prod dataset
  cdb_cfc_asfis_some <- cdb_cfc_asfis_some[fisheriesAsfis %in% globalProduction$fisheriesAsfis]
  # For species in the isscaap group but not in the mapping Selection = FALSE
  cdb_cfc_asfis_some[ , Selection := ifelse(is.na(Selection), FALSE, Selection )]
  
  
  # rows2change_some <- cdb_cfc_asfis_some[which(cdb_cfc_asfis_some$fisheriesAsfis != "all"), ]
  # 
  # # This substitution is happening for all years selected (chenges will be accounted for after the renderRHandsontable: asfis_check_data)
  # if(nrow(rows2change_some) > 0){
  #   isscaap2change_some <- unique(rows2change_some$isscaap)
  #   # change 
  #   for(i in 1:length(isscaap2change_some)){
  #     cdb_cfc_asfis_some[isscaap == isscaap2change_some[i] & !fisheriesAsfis %in% rows2change_some[isscaap == isscaap2change_some[i] ]$fisheriesAsfis, Selection := FALSE]
  #   }
  # }
  
  cdb_cfc_asfis <- rbind(cdb_cfc_asfis_all, cdb_cfc_asfis_some)
  
  full_prod <- merge(cdb_cfc_asfis, globalProduction,
                     by = c("geographicAreaM49_fi", "timePointYears", "fisheriesAsfis"),
                     all.x = TRUE, suffixes = c("_ProcessedProd", "_PrimaryProd"))
  
  return(list(cdb_isscaap = cdb_prod_isscaap, cdb_ASFIS = full_prod))
}



##-- Second function primary production approach ----

primaryprod_imputation2 <- function(full_prod, cdb_prod_isscaap, # Datasets
                                    sel_year, # Parameters
                                    manual_ratio = NULL,
                                    missing_data = missing_data # Manual parameters
){ 
  
  # Vector from where to select what species include
  full_prod$Selection
  
  # Primary and processed production values aggregated by year
  
  prod_complete <-  full_prod[ , list(Value = sum(Value_PrimaryProd*Ratio*Selection, na.rm = TRUE),
                                      flagObservationStatusAggr = max(flagObservationStatus_PrimaryProd, na.rm = TRUE),
                                      flagMethodAggr = "s"), by = c("geographicAreaM49_fi",
                                                                    "timePointYears",
                                                                    "measuredElement",
                                                                    "Scheda",
                                                                    "Value_ProcessedProd",
                                                                    "flagObservationStatus_ProcessedProd",
                                                                    "flagMethod_ProcessedProd")]
  
  setnames(prod_complete, old = c("Value", "flagObservationStatusAggr", "flagMethodAggr") , 
           new = c("Aggregate_PrimaryProd", "flagObservationStatus_PrimaryProd", "flagMethod_PrimaryProd"))
  
  prod_complete$Aggregate_PrimaryProd <- as.numeric(prod_complete$Aggregate_PrimaryProd)
  # Primary and processed production values detailed at ASFIS species level
  
  prod_complete$ratio <-ifelse(prod_complete$Aggregate_PrimaryProd >0 & 
                                 !is.na(prod_complete$Value_ProcessedProd),
                               prod_complete$Value_ProcessedProd/prod_complete$Aggregate_PrimaryProd, 0)
  # Table for previous years
  prod_prev <- prod_complete[timePointYears != sel_year]
  
  average_ratio <- prod_prev[, mean(ratio, na.rm = TRUE), by = c("Scheda")]
  setnames(average_ratio, old = "V1", new = "AvRatio")
  
  # Calculate average production for the previous selected years
  average_prod <- prod_prev[, mean(Value_ProcessedProd, na.rm = TRUE), by = c("Scheda")]
  setnames(average_prod, old = "V1", new = "AvProd")
  
  # Calculate average exports for the previous selected years
  average_primary <- prod_prev[, mean(Aggregate_PrimaryProd, na.rm = TRUE), by =  c("Scheda")]
  setnames(average_primary, old = "V1", new = "AvPrim")
  
  # Merge the average ratio table with the main table (tab2)
  tab_avRatio <- merge(prod_complete, average_ratio,
                       by = c("Scheda"),
                       all.x = TRUE, allow.cartesian = TRUE)
  
  # Adding average production and export to the table
  tab_avProd <- merge(tab_avRatio, average_prod,
                      by = c("Scheda"),
                      all.x = TRUE, allow.cartesian = TRUE)
  
  tab_avPrim <- merge(tab_avProd, average_primary,
                      by = c("Scheda"),
                      all.x = TRUE, allow.cartesian = TRUE)
  
  if(is.na(manual_ratio) & is.na(missing_data)){
    
    tab_avPrim$Value_ProcessedProd <- ifelse(tab_avPrim$timePointYears == sel_year,
                                             tab_avPrim$Aggregate_PrimaryProd*tab_avPrim$AvRatio,
                                             tab_avPrim$Value_ProcessedProd)
    
    tab_avPrim$ratio <- ifelse(tab_avPrim$timePointYears == sel_year, NA, tab_avPrim$ratio)
    
  } else if(is.na(manual_ratio) & !is.na(missing_data)){
    
    tab_avPrim$ratio <- ifelse(tab_avPrim$timePointYears == sel_year, 
                               missing_data/tab_avPrim$Aggregate_PrimaryProd,
                               tab_avPrim$ratio)
    
  } else {
    
    tab_avPrim$Value_ProcessedProd <- ifelse(tab_avPrim$timePointYears == sel_year,
                                             tab_avPrim$Aggregate_PrimaryProd * manual_ratio,
                                             tab_avPrim$Value_ProcessedProd)
    
    # Put a dash at the average ratio of the selected year
    tab_avPrim$ratio <- ifelse(tab_avPrim$timePointYears == sel_year, manual_ratio, tab_avPrim$ratio)
    
  }
  
  # Merge observation and ,ethod flags to have just one column
  PrimFlags <- combineFlag(tab_avPrim, "flagObservationStatus_PrimaryProd", "flagMethod_PrimaryProd") 
  ProdFlags <-  combineFlag(tab_avPrim, "flagObservationStatus_ProcessedProd", "flagMethod_ProcessedProd") 
  
  row1 <- c(as.vector(rbind(round(tab_avPrim$Value_ProcessedProd), ProdFlags)), round(average_prod$AvProd[1]))
  row2 <- c(as.vector(rbind(round(tab_avPrim$Aggregate_PrimaryProd), PrimFlags)),  round(average_primary$AvPrim[1]))
  # Put the manuel ratio at the average ratio of the selected year
  row3 <- c(as.vector(rbind(round(tab_avPrim$ratio, 3), rep("",length(tab_avPrim$ratio)))) , round(average_ratio$AvRatio[1], 3))
  
  format2show <- rbind(row1, row2, row3)
  
  columnNames <- as.vector(rbind(tab_avPrim$timePointYears, 
                                 rep("Flag", length(tab_avPrim$timePointYears))))
  
  # If all values are NAs and not taken by the previous commands
  if(ncol(format2show) < length(c(columnNames, "Average")) ){
    format2show[ , Average := rep(nrow(format2show), "NA")]
    colnames(format2show) <- c(columnNames, "Average")
    
  } else {
    colnames(format2show) <- c(columnNames, "Average")
  }
  
  if(any(is.na(colnames(format2show)))){
    colnames(format2show)[is.na(colnames(format2show))] <- "Missing year"
  }
  
  finalprod <- as.data.table(format2show)
  finalprod <- cbind("Stats" = c("Processed prod volume", "Primary prod volume", "Ratio"),
                     finalprod)
  
  return(finalprod)
  
}


##-- Ensemble model imputation method ----

method_imputation <- function(procprod, # Datasets
                              sel_country, sel_years, sel_commodity# Parameters
){
  procprod <- as.data.table(procprod)
  procprod$flagObservationStatus <- as.character(procprod$flagObservationStatus)
  procprod$flagMethod <- as.character(procprod$flagMethod)
  pp_country <- procprod[geographicAreaM49_fi %in% sel_country & 
                           timePointYears %in% sel_years & 
                           Scheda %in% sel_commodity, ]
  
  # Remove duplicates if needed (should not be)
  
  pp_country_dup <- pp_country[!base::duplicated(pp_country[,.("measuredElement", 
                                                               "geographicAreaM49_fi", 
                                                               "Scheda",
                                                               "timePointYears")])]
  
  if(all.equal(pp_country_dup,pp_country)){
    pp_country <- pp_country
  } else {
    pp_country <- pp_country_dup
  }
  
  ##Imputation
  
  # Prepare list of needed element with specific function
  fishImputationParamenters <- defaultImputationParameters()
  fishImputationParamenters$imputationValueColumn="Value"
  fishImputationParamenters$imputationFlagColumn="flagObservationStatus"
  fishImputationParamenters$imputationMethodColumn="flagMethod"
  fishImputationParamenters$byKey=c("geographicAreaM49_fi","Scheda", 
                                    "measuredItemISSCFC")
  fishImputationParamenters$estimateNoData=FALSE
  
  # # If the data series contains only zero and missing value then it is considered to contain no information for imputation.
  pp_country <- removeNoInfo(pp_country,
                             value="Value",
                             observationFlag = "flagObservationStatus",
                             byKey = c(fishImputationParamenters$byKey, "measuredElement"))
  
  # If no missing data the commodityDB does not change
  if(any(is.na(pp_country$Value))){
    commodityDBImputed <- imputeVariable(data = pp_country,
                                         imputationParameters = fishImputationParamenters)
  } else {
    commodityDBImputed <- pp_country
  }
  
  commodityDBImputed$Value <- round(commodityDBImputed$Value)
  return(commodityDBImputed)
  
}
