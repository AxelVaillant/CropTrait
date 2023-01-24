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
              wellPanel(fluidRow(column(width = 6,checkboxInput("glopnetData","Glopnet Data",value = TRUE),
                                        selectizeInput("taxons","Taxons",choices = NULL,selected = NULL, multiple = TRUE, options = list(placeholder = 'Leave this field empty to match all taxons',maxOptions = 2500)),
                                        pickerInput("scale",'Scale',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                                        pickerInput("Functional_group",'Functional group',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                                        pickerInput("sampling_type",'Sampling type',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                                        actionButton("visualize","Visualize"),actionButton("reset","Reset"),p(),span(verbatimTextOutput('resText'),style="text-align:center;")),
                                 column(width = 6,plotOutput("SLAvsLNC")))),
              fluidRow(column(width = 6,wellPanel(span("Select parameters of interest then click the visualize button to display the matching data on the plot.",tags$br(),
                                                      "Several conditions can be added on the same plot like layers.",
                                                      "Previous plotting can be erased by hitting the reset button.",style="font-size:16px;"))))),
      tabItem(tabName = "downloadTab",h2("Get Data"),
              wellPanel(fluidRow(column(width = 6,p("Please enter a list of comma separated taxon names. Anything else will be ignored."),textAreaInput("dlTaxon","Taxon"),
                          pickerInput("dlScale",'Scale',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                          pickerInput("dlTraits",'Trait',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                          pickerInput("dlFunctional_group",'Functional group',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                          pickerInput("dlSampling_type",'Sampling type',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                          actionButton("runQuery","Get data")),
                          column(width=6,downloadButton('fieldDesc', label="Fields description"),downloadButton('taxonTable', label="Taxon table"),
                                       shinyjs::hidden(div(id="userInfos",style="margin-top:175px",
                                       span("Owners of some of the data you selected will be informed about the download. Please provide a valid email adress (mandatory) and a summary of your project (optional).",style="color:#9e0f08;font-size:16px;"),
                                       textInput("userMail","Email"),textAreaInput("projectSummary","Project summary"),
                                       actionButton("submit","Submit",icon("paper-plane"))))))),
              shinyjs::hidden(downloadButton('queryDl', label="Download data",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
      tabItem(tabName = "contribution",h2("Contribute to the database"),
              HTML("<h3>Follow this link to contribute to the database : </h3>"),
              HTML("<a href=https://erc-constraints.cefe.cnrs.fr/database-croptraits/>Contribution process</a>")
              
    )
  )
)
)