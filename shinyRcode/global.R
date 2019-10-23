# packages
 
 .libPaths( c("/usr/local/lib64/R-3.1.2/library","/work/SWS_R_Share/shiny/Rlib/3.1",
              .libPaths()))

suppressMessages({
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

source('external_functions.R')

#-- Token QA ----

R_SWS_SHARE_PATH = "Z:"
SetClientFiles("/srv/shiny-server/shinyFisheriesCommodities")
GetTestEnvironment(baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
                   token = "04fc0c00-a4f3-4640-bee6-49a906863095")

#-- Get the M49 countries from dimension ----

M49 <- GetCodeList(domain ="FisheriesCommodities", dataset = "commodities_total", 
                   dimension = "geographicAreaM49_fi")
M49 <- M49[ type == "country", .( description, code)]
M49$description <- replaceforeignchars(M49$description)

country_input <-  sort(sprintf("%s - %s", M49$description, as.numeric(M49$code)))
country_input <- data.table(label = country_input, code = sub(" ", "", sub(".*-", "", 
                                                                           country_input)))
country_input <- rbind(data.table(label = "Select country", code = "-"), country_input)

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

