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
      menuItem("Contribution", tabName = "contribution", icon=icon("share")),
      menuItem("About", tabName = "about", icon=icon("question"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel="shortcut icon", href="faviconOld.ico")),
    useShinyjs(),
    includeCSS("www/style.css"),
    tabItems(
      tabItem(tabName = "home",
              includeMarkdown("www/cropTraitHomepage.md")
              ),
      tabItem(tabName = "visualization",h2("Relationship between SLA and LNC "),
              ##################-VISUALIZATION TAB-###################
              ###-SLA VS LNC-###
              wellPanel(fluidRow(column(width = 6,checkboxInput("glopnetData","Glopnet Data",value = TRUE),radioButtons("visuType","Type of visualization",choices =list("By individual","By genotype")),
                                        selectizeInput("taxons","Taxons",choices = NULL,selected = NULL, multiple = TRUE, options = list(placeholder = 'Leave this field empty to match all taxons',maxOptions = 2500)),
                                        pickerInput("Functional_group",'Functional group',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                                        pickerInput("sampling_type",'Sampling type',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                                        actionButton("visualize","Visualize"),actionButton("reset","Reset"),p(),span(verbatimTextOutput('resText'),style="text-align:center;")),
                                 column(width = 6,plotOutput("SLAvsLNC",hover = hoverOpts("plot_hover")),uiOutput("hover_info")))),
              fluidRow(column(width = 6,wellPanel(span("Select parameters of interest then click the visualize button to display the matching data on the plot.",tags$br(),
                                                      "Several conditions can be added on the same plot like layers.",
                                                      "Previous plotting can be erased by hitting the reset button.",style="font-size:16px;")))),
              ###-TRAIT VARIABILITY-###
              h2("Trait variability"),wellPanel(fluidRow(column(width = 6,selectInput("varTraits","Trait",choices = NULL),
                                        selectizeInput("varSpecies",'Taxons',multiple=FALSE,choices=NULL,options = list(maxOptions = 2500)),
                                        span(verbatimTextOutput('traitVarText'),style="text-align:center;")),
                                 column(width= 6,plotOutput("traitVariability"))))),
      tabItem(tabName = "downloadTab",h2("Get Data"),
              ##################-GET DATA TAB-###################
              wellPanel(fluidRow(column(width = 6,fluidRow(column(width = 3,radioButtons("dataAccessMode","Mode",choices =list("Public","Admin"),inline = T)),
                                                           shinyjs::hidden(div(id="admin",column(width = 4,textInput("adminPswd","Admin Password")),column(width=2,br(),actionBttn("submitPswd","Submit",style = "jelly",color="royal",size = "sm")))),column(width = 2,br(),shinyjs::hidden(div(id="pass",span(textOutput("passError"), style="color:red"))))),
                          #p("Please enter a list of comma separated taxon names. Anything else will be ignored."),
                          selectizeInput("dlTaxon",'Taxons',multiple=TRUE,choices=NULL,options = list(maxOptions = 2500)),
                          pickerInput("dlScale",'Scale',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                          pickerInput("dlTraits",'Trait',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                          pickerInput("dlFunctional_group",'Functional group',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                          pickerInput("dlSampling_type",'Sampling type',multiple=TRUE,choices=NULL,options = list(`actions-box` = TRUE)),
                          actionButton("runQuery","Get data")),
                          column(width=6,downloadButton('fieldDesc', label="Fields description"),downloadButton('taxonTable', label="Taxon table"),
                                       HTML("<h4 style=margin-top:50px;>Filter the database regarding you needs. If a field is left empty, no filter will be applied on this field.
                                            For example : no taxons selected means that all taxons will be returned.</h4>"),
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