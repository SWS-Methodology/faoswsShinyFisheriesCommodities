# packages
 
 .libPaths( c("/usr/local/lib64/R-3.1.2/library","/work/SWS_R_Share/shiny/Rlib/3.1",
              .libPaths()))

suppressMessages({
library(dplyr)
library(data.table)
library(DT)
library(faosws)
library(faoswsFlag)
library(faoswsProcessing)
library(faoswsUtil)
library(faoswsImputation)
library(ggplot2)
library(rhandsontable)
library(shiny)
library(shinyWidgets)
})

local <- TRUE

#-- Token QA ----
if(local){
  setwd('C:/Users/Taglionic/OneDrive - Food and Agriculture Organization/Github/shinyFisheriesCommodities/shinyFisheriesCommodities')
  
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

source('external_functions.R')

#-- Get the M49 countries from dimension ----
M49 <- GetCodeList(domain ="FisheriesCommodities", dataset = "commodities_total", 
                   dimension = "geographicAreaM49_fi")
M49 <- M49[ type == "country", .( description, code)]
# M49$description <- iconv(M49$description, to = 'UTF-8', '')

M49$description <- replaceforeignchars(M49$description)

country_input <-  sort(sprintf("%s - %s", M49$description, as.numeric(M49$code)))
country_input <- data.table(label = country_input, code = sub(" ", "", sub(".*-", "", 
                                                                           country_input)))
country_input <- rbind(data.table(label = "", code = "-"), country_input)
Encoding(country_input$label)
#-- Mappings ----

# Isscaap-isscfc-ics mapping
mappingItems <- ReadDatatable('fishery_item_mapping')

# Get list of species according to alphacodes
map_asfis <- ReadDatatable('map_asfis')
map_asfis[, ics := NULL]
setnames(map_asfis, 'asfis', 'fisheriesAsfis')

# Get list of commodities and create labels for shiny app
map_isscfc <- GetCodeList("FisheriesCommodities", 
                          "commodities_total",
                          "measuredItemISSCFC")[,.(code, description)]

#-- Variables for messages from action buttons ----

# variable for new commodities message
id_comm <- NULL

# variable for export approach shiny messages
id_exp <- NULL

# variable for primary production approach shiny messages
id_prod <- NULL

