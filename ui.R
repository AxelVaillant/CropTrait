library(shinydashboard)

#########################
dashboardPage(
  skin = "purple",
  dashboardHeader(title= "CropTrait"),
  dashboardSidebar(
    sidebarMenu(
      HTML(paste0(
        "<br>",
        "<img style='display: block; margin-left: auto; margin-right: auto; height='50%'; width='50%' src='plant.svg'>",
        "<br>"
      )),
      menuItem("Home", tabName = "home", icon=icon("house")),
      menuItem("Visualization", tabName = "visualization", icon=icon("chart-line")),
      menuItem("Database", tabName = "database", icon=icon("database")),
      menuItem("Get Data", tabName = "downloadTab", icon=icon("download")),
      menuItem("Contribution", tabName = "contribution", icon=icon("share"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
    useShinyjs(),
    includeCSS("www/style.css"),
    tabItems(
      tabItem(tabName = "home",
              includeMarkdown("www/cropTraitHomepage.md")
              #h2("Welcome to CropTrait"),
              #p("The CropTrait Database, defined as a multiple-crops database, focuses on ecophysiological traits related to resource use and acquisition of cultivated plants"),
              #img(src = "crop.png")
              ),
      tabItem(tabName = "visualization",h2("Database visualization"),
              wellPanel(fluidRow(column(width = 6,textInput("taxon","Taxon"),
                                        pickerInput("scale",'Scale',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                                        pickerInput("Functional_group",'Functional group',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                                        pickerInput("sampling_type",'Sampling type',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                                        actionButton("runGraph","Run")),
                                 column(width = 6,plotOutput("SLAvsLNC"))))),
      tabItem(tabName = "database",h2("CropTrait Database"),
              wellPanel(fluidRow(column(width=6,actionBttn("trait","Trait",style="unite",color="royal")),
                                 shinyjs::hidden(div(id="divSearch",column(width=6,div(style="display:inline-block",textInput("traitText","Search",width='100%',placeholder = "Keyword here")),
                                        div(style="display:inline-block",actionButton("search","Search")))))),p(),
                        shinyjs::hidden(div(id="traitTable",tableOutput(('tableTrait'))))),
              wellPanel(fluidRow(column(width=6,actionBttn("species","Species",style="unite",color="royal"))),p(),
                        shinyjs::hidden(div(id="speciesTable",fluidRow(column(12,tableOutput(('tableSpecies'))))))),
              wellPanel(h4("Location"))),
      tabItem(tabName = "downloadTab",h2("Get Data"),
              wellPanel(fluidRow(column(width = 6,p("Please enter a list of comma separated taxon names. Anything else will be ignored."),textAreaInput("dlTaxon","Taxon"),
                          pickerInput("dlScale",'Scale',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                          pickerInput("dlTraits",'Trait',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                          pickerInput("dlFunctional_group",'Functional group',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                          pickerInput("dlSampling_type",'Sampling type',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                          actionButton("runQuery","Get data")))),
              shinyjs::hidden(downloadButton('queryDl', label="Download"))),
      tabItem(tabName = "contribution",h2("Contribute to the database"),
              HTML("<h3>Follow this link to contribute to the database : </h3>"),
              HTML("<a href=https://erc-constraints.cefe.cnrs.fr/database-croptraits/>Contribution process</a>")
              
    )
  )
)
)