library(shinydashboard)

#########################
dashboardPage(
  skin = "purple",
  dashboardHeader(title= "CropTraits"),
  dashboardSidebar(
    sidebarMenu(
      HTML(paste0(
        "<br>",
        "<img style='display: block; margin-left: auto; margin-right: auto; height='50%'; width='50%' src='plant.svg'>",
        "<br>"
      )),
      menuItem("Home", tabName = "home", icon=icon("house")),
      menuItem("Browse", tabName = "browse", icon=icon("eye")),
      menuItem("Visualization", tabName = "visualization", icon=icon("chart-line")),
      menuItem("Get Data", tabName = "downloadTab", icon=icon("download")),
      menuItem("Contribution", tabName = "contribution", icon=icon("share")),
      menuItem("About", tabName = "about", icon=icon("question"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel="shortcut icon", href="faviconOld.ico"),
              tags$style(HTML("
                        #bbtr_table{
                        overflow-y:auto;
                        height:650px;
                        }
                        #bbta_table{
                        overflow-y:auto;
                        height:600px;
                        }
                        "))),
    useShinyjs(),
    includeCSS("www/style.css"),
    tabItems(
      tabItem(tabName = "home",
              HTML(paste0("<h1>Welcome to CropTraits</h1>",
              "<p style=font-size:20px;>The CropTraits Database, defined as a multiple-crops database, focuses on ecophysiological traits related to resource use and acquisition of cultivated plants </p>",
              "<br>")),fluidRow(column(width = 6,img(src ="crop.png")),column(width = 6,div(withSpinner(tableOutput('recapTable')), style="font-size:175%"))), HTML(paste0("<br><br/>",
              "<p style=font-size:20px;> The CropTraits Database is composed of multi-sources with heterogeneous data including compilated data from litterature or other databases, data from our experiments and from our collaborators. The data are currently private and only available to our collaborators.</p>",
              "<b style=font-size:20px;>The database is configured for a minimal set of traits and is flexible enough to enter more traits, species, genotypes and ancillary data.</b> <p style=font-size:20px;>The database allows collecting data at different levels from general information about plant material description, including taxonomy and origin of material, to environnemental conditions and methods of measurements.</p>",
              "<br>",img(src="tab.png"))
              #includeMarkdown("www/cropTraitHomepage.md")
              )),
      tabItem(tabName = "browse",h2("Browse database "),
              fluidRow(column(width = 6,wellPanel(h3("Browse by Traits "),HTML("<p style=font-size:15px;>Display available data per taxon for a chosen trait.</p>"),
                                        pickerInput("bbtr_Traits",'Traits',multiple=FALSE,choices=NULL,options = list(`actions-box` = TRUE))),
                                        h3(uiOutput('bbtr_title')),tableOutput('bbtr_table')),
                                 column(width = 6,wellPanel(h3("Browse by Taxon "),HTML("<p style=font-size:15px;>Display available data per trait for a chosen taxon.</p>\n
                                                                                        <p style=font-size:15px;>Choose one or multiple functional group then one taxon to display the corresponding available traits data</p>"),
                                        pickerInput("bbta_functio_group",'Functional group',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                                        selectizeInput("bbta_Taxon",'Taxon',multiple=FALSE,choices=NULL,options = list(maxOptions = 2500)),
                                        ),h3(uiOutput('bbta_title')),tableOutput('bbta_table')))),
      tabItem(tabName = "visualization",h2("Relationship between SLA and LNC "),
              ##################-VISUALIZATION TAB-###################
              ###-SLA VS LNC-###
              wellPanel(fluidRow(column(width = 6,checkboxInput("glopnetData","Glopnet Data",value = TRUE),
                                        HTML("<p style=font-size:15px;>GLOPNET is a multi-investigator group studying and accumulating global data on plants traits.
                                        The dataset comprised data for 2548 species from 175 sites around the world representing every major biome. This dataset can be used as a comparison reference.</p>"),
                                        radioButtons("visuType","Type of visualization",choices =list("By individual","By genotype")),
                                        selectizeInput("taxons","Taxon",choices = NULL,selected = NULL, multiple = TRUE, options = list(placeholder = 'Leave this field empty to match all taxon',maxOptions = 2500)),
                                        pickerInput("Functional_group",'Functional group',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                                        pickerInput("sampling_type",'Sampling type',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                                        actionButton("visualize","Visualize"),actionButton("reset","Reset"),p(),span(verbatimTextOutput('resText'),style="text-align:center;")),
                                 column(width = 6,withSpinner(plotOutput("SLAvsLNC",hover = hoverOpts("plot_hover"))),uiOutput("hover_info")))),
              fluidRow(column(width = 6,wellPanel(span("Select parameters of interest then click the visualize button to display the matching data on the plot.",tags$br(),
                                                      "Several conditions can be added on the same plot like layers.",
                                                      "Previous plotting can be erased by hitting the reset button.",style="font-size:16px;")))),
              ###-TRAIT VARIABILITY-###
              h2("Trait variability"),wellPanel(fluidRow(column(width = 6,selectInput("varTraits","Trait",choices = NULL),
                                        selectizeInput("varSpecies",'Taxon',multiple=FALSE,choices=NULL,options = list(maxOptions = 2500)),
                                        span(verbatimTextOutput('traitVarText'),style="text-align:center;")),
                                 column(width= 6,withSpinner(plotOutput("traitVariability")))))),
      tabItem(tabName = "downloadTab",h2("Get Data"),
              ##################-GET DATA TAB-###################
              wellPanel(fluidRow(column(width = 6,fluidRow(column(width = 3,radioButtons("dataAccessMode","Mode",choices =list("Public","Admin"),inline = T)),
                                                           shinyjs::hidden(div(id="admin",column(width = 4,textInput("adminPswd","Admin Password")),column(width=2,br(),actionBttn("submitPswd","Submit",style = "jelly",color="royal",size = "sm")))),column(width = 2,br(),shinyjs::hidden(div(id="pass",span(textOutput("passError"), style="color:red"))))),
                          #p("Please enter a list of comma separated taxon names. Anything else will be ignored."),
                          selectizeInput("dlTaxon",'Taxon',multiple=TRUE,choices=NULL,options = list(maxOptions = 2500)),
                          pickerInput("dlScale",'Observation levels',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                          pickerInput("dlTraits",'Trait',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                          pickerInput("dlFunctional_group",'Functional group',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                          pickerInput("dlSampling_type",'Sampling type',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                          actionButton("runQuery","Get data")),
                          column(width=6,downloadButton('fieldDesc', label="Fields description"),downloadButton('taxonTable', label="Taxon table"),
                                       HTML("<h4 style=margin-top:50px;>Filter the database regarding your needs. If a field is left empty, no filter will be applied on this field.
                                            For example : no taxon selected means that all taxon will be returned.</h4>"),
                                       shinyjs::hidden(div(id="userInfos",style="margin-top:90px",
                                       span("Owners of some of the data you selected will be informed about the download. Please provide a valid email adress (mandatory) and a summary of your project (optional).",style="color:#9e0f08;font-size:16px;"),
                                       textInput("userMail","Email"),textAreaInput("projectSummary","Project summary"),
                                       actionButton("submit","Submit",icon("paper-plane"))))))),
              shinyjs::hidden(downloadButton('queryDl', label="Download data",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
      tabItem(tabName = "contribution",
              ##################-CONTRIBUTION TAB-###################
              includeMarkdown("www/contributionPage.md")
    ),
    ##################-ABOUT TAB-###################
    tabItem(tabName = "about",
             includeMarkdown("www/about.md"))
  )
)
)