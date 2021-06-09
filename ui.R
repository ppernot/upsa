shinyUI(
  fixedPage(
#             responsive=FALSE,
#             theme = "cerulean.css",
            theme=shinythemes::shinytheme("cerulean"),
            HTML('<style type="text/css"> 
          .col-sm-4 .well { 
            background-color: #EFEFCD; 
           }
          .tab-pane .well { 
            background-color: #FFF; 
           }
          .table-bordered {
            font-family: Sans-Serif;
            font-size: 13px;
	          background: #EFEFCD;
	          margin: 10px;
	          margin-bottom: 10px;
           }
          .tab-content {
            overflow: visible;
            margin-bottom: 10px;
           }
          .selectize-input {
            width: 100px;
            height: 30px;
          }
          .select-input {
            width: 100px;
            height: 30px;
          }
         </style>'), 
            titlePanel(
              title = "upsa - Uncertainty Propagation & Sensitivity Analysis",
              windowTitle = "upsa - Uncertainty Propagation & Sensitivity Analysis"
            ),
            fixedRow(
              column(
                width=4,             
                wellPanel(
                  h4("Propagation Method"),
                  radioButtons(
                    inputId = "method", 
                    label   = '',
                    choices = c(
                      "Combination of Variances (GUM)" = "GUM",
                      "Monte-Carlo (GUM-Supp1)"        = "GUMS1"
                    )
                  ),
                  conditionalPanel(
                    condition = "input.method == 'GUMS1'",
                    h5("Monte-Carlo Sample"),
                    radioButtons(
                      inputId = "sampling", 
                      label   = '',
                      choices = c(
                        "Fixed"      = "fixed",
                        "Adaptative" = "adapt"
                      )
                    ),
                    conditionalPanel(
                      condition = "input.sampling == 'fixed'",
                      fixedRow(
                        column(
                          width=12,
                          sliderInput(
                            inputId = "nMCrun",
                            label=h6('Size of MC sample'),
                            min=1000, max=10000, 
                            value=1000, step=1000)
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.sampling == 'adapt'",
                      fixedRow(
                        column(
                          width=6,
                          radioButtons(
                            inputId = "adaptType", 
                            label   = h6('Algorithm'),
                            choices = c(
                              "GUM-Supp1"     = "s1",
                              "2-steps Stein" = "s2"
                            ),
                            selected= "s2"
                          )
                        ),
                        column(
                          width=6,
                          radioButtons(
                            inputId = 'ndigits',
                            label   = h6('Digits'),
                            choices = c("1"="1","2"="2"),
                            selected= "1"
                          )
                        )
                      )
                    )
                  )
                  #                ,
                  #                uiOutput("propPanel")
                ),
                wellPanel(
                  h4("Inputs Summary"),
                  h5("Model Expression"),         
                  textOutput("output_Mod"),
                  h5("Variables"),         
                  tableOutput("output_variables"),
                  h5("Correlation"),         
                  tableOutput("corrMat")
                ),
                fixedRow(
                  column(
                    width=12,
                    list(
                      em("Developped by"),
                      a(href="http://pagesperso.lcp.u-psud.fr/pernot/","Pascal PERNOT"),
                      "@",
                      a(href="http://www.cnrs.fr/",       
                        tags$img(src="logo_CNRS.jpg", width=30)),
                      a(href="http://metrologie.cnrs.fr/",
                        tags$img(src="logo_MMI.jpg",  width=24))
                    ),
                    offset=0
                  )
                )
              ),      
              column(
                width=8,
                wellPanel(
                  tabsetPanel(
                    id="mainTabset",
                    tabPanel(
                      value="modelDef",
                      title=h4("1 - Model"),
                      tabsetPanel(
                        tabPanel(
                          title="Enter Expression",
                          textInput(
                            inputId = "Fexpr",
                            label   = '',
                            value   = "x1+x2"
                          ),
                          tags$style(
                            type='text/css','#Fexpr { width: 380px; }'
                          )
                        ),
                        tabPanel(
                          title="Load Model File",
                          fileInput(
                            inputId = 'modelInFile',
                            label   = '',
                            multiple= FALSE,
                            accept  = c('.upsa')
                          )
                        ),
                        type='pills'
                      ),
                      br(),
                      actionButton(
                        inputId = "clearAll",
                        label   = "Clear All",
                        icon=icon("eraser")
                      ),
                      actionButton(
                        inputId = "updateExpr",
                        label   = "OK",
                        icon    = icon("check")
                      )
                    ), 
                    tabPanel(
                      value="variables",
                      title=h4("2 - Variables"),
                      uiOutput("variables_ui")
                    ),
                    tabPanel(
                      value="results",
                      title=h4("3 - Results"),
                      uiOutput("results_ui")
                    ),
                    tabPanel(
                      value="outputs",
                      title=h4("4 - Outputs"),
                      tabsetPanel(
                        tabPanel(
                          title="Save Model",
                          wellPanel(
                            verticalLayout(
                              h5('File Name'),
                              '(a ".upsa" extension is automatically appended)',
                              textInput(
                                inputId = 'fileName', 
                                label   = '', 
                                value   = 'myModel'
                              ),
                              downloadButton(
                                outputId = 'saveData',
                                label    = 'Save (Ctrl+Click)'
                              )
                            )
                          )
                        ),
                        tabPanel(title="Generate Report",
                                 wellPanel(
                                   checkboxGroupInput(
                                     inputId = 'inReport', 
                                     label   = h5('Include these results'), 
                                     choices = c(
                                       'GUM'                   = 'GUM',
                                       'Monte-Carlo'           = 'MC',
                                       'MC Convergence'        = 'CV',
                                       'MC Output ECDF'        = 'ECDF',
                                       'MC I/O correlations'   = 'COR',
                                       'MC Variance Gradients' = 'VG'), 
                                     selected = c('GUM','MC','COR')
                                   ),
                                   radioButtons(
                                     inputId = 'format',
                                     label   = h5('Document format'),
                                     choices = c('PDF', 'HTML', 'Word'),
                                     inline  = TRUE
                                   ),
                                   verticalLayout(
                                     textInput(
                                       inputId = 'reportName', 
                                       label   = '', 
                                       value   = "myReport"
                                     ),
                                     downloadButton(
                                       outputId = 'report',
                                       label    = 'Download  (Ctrl+Click)'
                                     )
                                   )
                                 )
                        ),
                        type='pills'
                      )
                    )
                  )
                  #                ,textOutput("debug")
                )
              )
            )
  )
)



