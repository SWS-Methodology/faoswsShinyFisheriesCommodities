fluidPage( 
  title = "Validation Tools",
  br(),
  sidebarLayout(
    conditionalPanel("input.validation != 'intro'",
                     sidebarPanel(      
                       selectInput(inputId = "btn_country", 
                                   label = 'Country', 
                                   choices = country_input$label, 
                                   selected = NULL),
                       
                       uiOutput('btn_year'),
                       uiOutput('btn_start_year'),
                       uiOutput('btn_missing'),
                       uiOutput('btn_commodity')
                       )
                     ),
    
    mainPanel(
      tabsetPanel(
        id = 'validation',
        
        tabPanel("National mapping",
                 actionButton("check_new_items", 
                              "Check new items"),
                 br(),
                 br(),
                 DT::dataTableOutput('imputed_data')
                 ),
        tabPanel("Export approach", 
                 actionButton("check_consistency_exp", 
                              "Check mapping"),
                 br(),
                 br(),
                 rHandsontableOutput('isscfc_check_data'),
                 br(),
                 br(),
                 br(),
                 br(),
                 actionBttn("update_export_mapping", 
                            label = "Update mapping",
                            color = "primary",
                            style = "bordered"),
                 br(),
                 br(),
                 uiOutput('ratio_choice'),
                 br(),
                 conditionalPanel("input.btn_ratioExp == '3' ",
                                  uiOutput('out_btn_manual_exp')),
                 br(),
                 rHandsontableOutput('table_exp_estimates'),
                 br(),
                 plotOutput('gg_exp_estimates', 
                            width = '80%')
                 ),
        tabPanel("Primary Prod. approach",
                 actionButton("check_consistency_prod", 
                              "Check mapping"),
                 br(),
                 br(),
                 rHandsontableOutput('asfis_check_data'),
                 br(),
                 br(),
                 br(),
                 actionBttn("update_production_mapping", 
                            label = "Update mapping",
                            color = "primary",
                            style = "bordered"),
                 br(),
                 br(),
                 uiOutput('ratio_choice_prod'),
                 br(),
                 conditionalPanel("input.btn_ratioProd == '3' ",
                                  uiOutput('out_btn_manual_prod')),
                 br(),
                 rHandsontableOutput('table_prod_estimates'),
                 br(),
                 plotOutput('gg_prod_estimates', width = '80%')
                 ),
        tags$head(tags$style(
          ".buttclass{background-color: #288ffe;} .buttclass{color: #fcf8e3;}")),
        tabPanel("Summary",
                 br(),
                 uiOutput('summary_check_data'),
                 conditionalPanel("input.btn_approach == '4'",
                                  uiOutput('out_btn_manual')),
                 fluidPage(
                   fluidRow(
                     column(width = 8, 
                            plotOutput('gg_methods')),
                     column(width = 6, 
                            actionBttn('btn_imputation', label = 'Impute value',
                                       style = "gradient",
                                       color = "success"))
                     )
                   )
                 ),
        tabPanel("Check mapping export approach", 
                 br(),
                 actionBttn("save_export_mapping", 
                            label = "Save mapping in SWS",
                            color = "primary",
                            style = "bordered"),
                 br(),
                 br(),
                 DT::dataTableOutput('check_export_mapping_data')
                 ),
        tabPanel("Check mapping prod approach", 
                 br(),
                 actionBttn("save_prod_mapping", 
                            label = "Save mapping in SWS",
                            color = "primary",
                            style = "bordered"),
                 br(),
                 br(),
                 DT::dataTableOutput('check_prod_mapping_data')
                 ),
        ## Tab with markdown file presenting the Shiny App
        tabPanel("About",value = "intro",
                 br(),
                 shiny::includeMarkdown("App_description.Rmd")
                 #withMathJax (includeHTML(rmarkdown::render('App_description.Rmd')))
                 )
        )
      )
    )
  )

