function(input,output,session){
  
  #-------Get credentials----------------------#
  credentials<-read.table(file = "credentials.csv",header = TRUE,sep = "\t")
  dbHost<-credentials$dbHost
  dbPort<-credentials$dbPort
  dbUser<-credentials$dbUser
  dbPassword<-credentials$dbPassword
  
  ###
  DB <-read.table("DB.csv",header = T,sep=";", dec=".", fill=T)
  #######################################################
  ################-DATABASE MANAGEMENT-##################
  observe({
    tryCatch({
        AllDataQuery <- paste(readLines("Query.txt"), collapse="\n")
  con <- dbConnect(RPostgres::Postgres(), dbname= "CropTrait", host="localhost", port=dbPort, user="postgres", password="Sonysilex915@")

  traitQuery = "select info ->>'id_bdd' as id_bdd,info ->>'crop_name' as crop_name,info ->>'trait_name'as trait_name, info ->>'original_value'as original_value,
  info ->>'original_unit'as original_unit,info ->>'stage' as stage,info ->>'organ'as organ,
  info ->>'method'as method from croptrait;"
  
  taxonQuery ="select info ->>'id_bdd'as id_bdd, info ->>'taxon'as taxon, info ->>'taxon_accepted'as taxon_accepted,
  info ->>'family'as family,info ->>'genus' as genus,info ->>'species'as species,  info ->>'spauthor'as spauthor,
  info ->>'subtaxa'as subtaxa,  info ->>'subtauthor'as subtauthor,  info ->>'variety'as variety,
  info ->>'crop_name'as crop_name,  info ->>'gen_name'as gen_name,info ->>'select_type'as select_type from croptrait LIMIT 1000;"
  
  ###-Selectize input-###
  updateSelectizeInput(session, "taxons", choices = taxonTable)
  
  #####-Picker input-#####
  functio_group_query<-"SELECT DISTINCT info->>'functio_group' as Fonctional_group FROM croptrait  ORDER BY info->>'functio_group';"
  functio_group<-dbGetQuery(con, functio_group_query)
  updatePickerInput(session, "Functional_group", choices = functio_group)
  updatePickerInput(session, "dlFunctional_group", choices = functio_group)

  sampling_type_query<-"SELECT DISTINCT info->>'sampling_type' as Sampling_type FROM croptrait  ORDER BY info->>'sampling_type';"
  sampling_type<-dbGetQuery(con, sampling_type_query)
  updatePickerInput(session, "sampling_type", choices = sampling_type)
  updatePickerInput(session, "dlSampling_type", choices = sampling_type)
  

  trait_query<-"SELECT DISTINCT info->>'trait_name' as trait_name FROM croptrait  ORDER BY info->>'trait_name';"
  traits<-dbGetQuery(con, trait_query)
  updatePickerInput(session, "dlTraits", choices = traits)
  
  scale_query<-"SELECT DISTINCT info->>'traitmeas_scale' as traitmeas_scale FROM croptrait  ORDER BY info->>'traitmeas_scale';"
  scaleRes<-dbGetQuery(con, scale_query)
  updatePickerInput(session, "scale", choices = scaleRes)
  updatePickerInput(session, "dlScale", choices = scaleRes)
  
  resTrait <<- dbGetQuery(conn = con,statement = traitQuery)
  resSpecies <<- dbGetQuery(conn = con,statement = taxonQuery)
  #resAll <<- dbGetQuery(conn = con,statement = AllDataQuery)
  
  dbDisconnect(con)
    })
  })


  #-Data reduction-#
  dataReduction <- function(data){
  if(dim(data)[1] > 1000){
    return (resSampled <- data[1:1000,])
  } else return(data)}
  ############################-NAVIGATION-#####################################
  observeEvent(input$trait, {
    toggle(id="traitTable")
    toggle(id="divSearch")
  })
  resTraitToPrint <- dataReduction(resTrait)
  output$tableTrait<-renderTable(resTraitToPrint,bordered=TRUE)
  observeEvent(input$species,toggle(id="speciesTable"))
  output$tableSpecies<-renderTable(resSpecies,bordered=TRUE)
  
  ##########################-Searching the dataframe-###########################
  observeEvent(input$search, {
    if(input$traitText == ""){
      resTrait<-dataReduction(resTrait)
      output$tableTrait<-renderTable(resTrait,bordered=TRUE)
    } else {
    table<-data.table(resTrait)
    setkey(table,"trait_name")
    filteredTable<-table[grepl(input$traitText, table$trait_name, ignore.case = TRUE)]
    filteredTable<-dataReduction(filteredTable)
    output$tableTrait<-renderTable(filteredTable,bordered=TRUE)
    }
  })
  ##############################################################################
  ##########################-PLOTS-#############################################
  ##############################################################################
  
  ####-GLOPNET-####
    GLOPNET <-read.table("/home/vaillant/Documents/BDD_Hybride/BDDCROPTRAIT/axel/Croptrait/02_data/GLOPNETdata.csv",header = T,sep=";", dec=".",quote="", fill=FALSE)
    str(GLOPNET)
    GLOPNET$LMA <- 10^(GLOPNET$log.LMA) #g/m2
    GLOPNET$LMA <- GLOPNET$LMA *10^-3 #kg/m2
    GLOPNET$SLA <- (1/GLOPNET$LMA) #m2/kg
    GLOPNET$log.SLA <- log10(GLOPNET$SLA)
    
    taxonTable <-read.table("taxon_Table.csv",header = T,sep=";", dec=".",quote='"', fill=FALSE)

    ###-Reactive data for plotting-###
    myReact <- reactiveValues()
    buttonClicked<- reactive(input$visualize)
    
    ###-Add one condition to plot-###
    observeEvent(input$visualize,{
      Sample<-concatenateQuery()
      if(!is.null(Sample)){
        shinyjs::hide('resText')
        genList<-traitSplit(Sample)
        displayVisuResult(genList)
        myReact$toPlot<-rbind(myReact$toPlot,genList)
        shinyjs::show('resText')
        }
    })
    ###-Enable/Disable glopnet data-###
    observeEvent(input$glopnetData,{
      if(isTRUE(input$glopnetData)){
        glopnetDf<-buildGlopnet()
        myReact$toPlot<-rbind(myReact$toPlot,glopnetDf)
      } else {
        myReact$toPlot <- myReact$toPlot %>% filter(filterName != "Glopnet")
      }
    })
    
  ############-SLA vs LNC Plot-##################
  output$SLAvsLNC<-renderPlot({
    if(!is.null(myReact$toPlot)){
      filters<-myReact$toPlot %>% distinct(filterName)
      myReact$toPlot$filterName <- factor(myReact$toPlot$filterName, levels = filters[,1] )
      ggplot(myReact$toPlot ,aes(x = meanLNC, y = meanSLA, color = filterName)) + geom_point() + xlim(-0.8,1) + ylim(0,2.7) +
        labs(x = "Leaf nitrogen content (LNC; log10; %)" , y= "Specific leaf area (SLA; log10; m2/kg)", colour = "Conditions") +
        theme(
          axis.title = element_text(size=14, face="bold", colour = "black"),
          axis.text = element_text(size=12, face="bold", colour = "black"),
          legend.title = element_text(size=12, face="bold", colour = "#757574"))
    } else {
      df <- data.frame(x=rnorm(20),y=rnorm(20,1,0.5))
      ggplot(df,aes(x,y)) +geom_blank() + xlim(-0.8,1) + ylim(0,2.7) + labs(x = "Leaf nitrogen content (LNC; log10; %)" , y= "Specific leaf area (SLA; log10; m2/kg)", colour = "Conditions") +
        theme(
          axis.title = element_text(size=14, face="bold", colour = "black"),
          axis.text = element_text(size=12, face="bold", colour = "black"),
          legend.title = element_text(size=12, face="bold", colour = "#757574"))
    }
  })
  
  ############################
  #####-Build glopnet df -####
  ############################
  buildGlopnet<-function(){
    glopnetDf<-data.frame(matrix(ncol=3,nrow=0))
    colnames(glopnetDf) <-c('meanSLA','meanLNC','filterName')
    glopnetDf$filterName<-as.character(glopnetDf$filterName)
    for(i in 1:length(GLOPNET[,1])){
      glopnetDf<-glopnetDf %>% add_row(meanSLA = GLOPNET[i,]$log.SLA, meanLNC = GLOPNET[i,]$log.Nmass, filterName = "Glopnet")
    }
    return(glopnetDf)
  }
  
  #############################################################
  ######-Concatenate filters and get corresponding data-#######
  #############################################################
  concatenateQuery<-function(){
  visuInputList<-list(input$taxons,input$Functional_group,input$sampling_type)
  visuInputNames<-c("taxon","functio_group","sampling_type")
  ####-REMPLACER FOR LOOP PAR APPLY
  visuQuery<-NULL
  for(i in 1:length(visuInputList)){
    if(!is.null(visuInputList[i][[1]])){
      query <- paste(noquote(visuInputNames[i])," == ",sQuote(visuInputList[i],F),sep=" ")
      if(is.null(visuQuery)){
        visuQuery<-query
      } else {
        visuQuery<-paste(visuQuery,query,sep = " & ")
      }
          }
  }
  if(!is.null(visuQuery)){
      Sample<- resAll %>% filter(!! rlang::parse_expr(visuQuery))
      return(Sample)
  } else { return(NULL)}
    }
  ##########################################################
  ##########-Print database matching results-###########
  ##########################################################
  displayVisuResult<-function(genList){
    if(length(genList[[1]])==0){
          output$resText<-renderText({
          paste("Your filters doesn't match any SLA/LNC couple in the database.",
                     "Please retry with other criteria", sep="\n ")
        })
    } else {
        output$resText<-renderText({
        paste("Your filters match ",length(genList[[1]]) ," unique genotypes in the database.", sep="")
        })
    }}
  
  traitSplit<-function(Sample){
  filterLegend<-legendHandler()  
  genotypes<-(Sample %>% distinct(gen_name))
  genList<-data.frame(matrix(ncol=3,nrow=0))
  colnames(genList) <-c('meanSLA','meanLNC','filterName')
  genList$filterName<-as.character(genList$filterName)
  for (i in 1:length(genotypes[[1]])){
    genSample <- filter (Sample,gen_name==genotypes[[1]][i])
    genSample$trait_original_value <- as.double(genSample$trait_original_value)
    genSample <- genSample %>% drop_na(trait_original_value)
    genSampleSLA<-filter(genSample,trait_name == "SLA")
    genSampleLNC<-filter(genSample,trait_name == "LNC per leaf dry mass")
    if(length(genSampleSLA[[1]])>0 && length(genSampleLNC[[1]]>0)){
      genSampleSLA<-normalize(genSampleSLA)
      genList<-genList %>% add_row(meanSLA = mean(genSampleSLA$trait_original_value), meanLNC = mean(genSampleLNC$trait_original_value), filterName = filterLegend)
    }
  }
  genList$meanSLA<-log10(genList$meanSLA)
  genList$meanLNC<-log10(genList$meanLNC)
  ##
  return(genList)
  }
  
  normalize<-function(dataset){
  for(i in 1:length(dataset[,1])){
    if(dataset[i,]$trait_original_unit == "cm2/g"){
      dataset[i,]$trait_original_value <- dataset[i,]$trait_original_value/10
    }
    if(dataset[i,]$trait_original_unit == "cm2/mg"){
      dataset[i,]$trait_original_value <- dataset[i,]$trait_original_value*100
    }
  }
  return(dataset)
  }
  
  legendHandler<-function(){
        filterUsed<-c(input$taxons,input$Functional_group,input$sampling_type)
        legendText<-NULL
        for(i in 1:length(filterUsed)){
                if(is.null(legendText)){
            legendText<-filterUsed[i]
          } else {
            legendText<-paste(legendText,filterUsed[i],sep = " & ")
          }
        }
        return(legendText)
  }
  ###-Reset Plot-###
  observeEvent(input$reset,{
    shinyjs::hide("resText")
    if(isTRUE(input$glopnetData)){
      myReact$toPlot <- myReact$toPlot %>% filter(filterName == "Glopnet")  
    } else {
      myReact$toPlot<-NULL  
    }
  })
  ########################################################################
  #############################-Download Data-############################
  ########################################################################
  dbManagement<- function(con){
  filterList<-""
  inputList<-list(input$dlTaxon,input$scale,input$dlTraits,input$dlFunctional_group,input$dlSampling_type)
  inputNameList<-list("taxon","traitmeas_scale","trait_name","functio_group","sampling_type")
  for(i in 1:length(inputList)){
    if(!is.null(inputList[i][[1]]) &&  !inputList[i]==""){
      if(length(inputList[i][[1]]) >1){
        inputList[i][[1]]<-str_replace_all(toString(inputList[i][[1]])," ","")
        inputList[i][[1]]<-sapply(strsplit(inputList[i][[1]],","), function(x) toString(sQuote(x,F)))
        inputList[i][[1]]<-str_replace_all(inputList[i][[1]]," ","")
      } else if (i == 1){
        inputList[i][[1]]<-taxonHandler(inputList[i][[1]])
      } else { inputList[i][[1]] <- sQuote(inputList[i][[1]],F)}
      filterList<-c(filterList,paste(" info ->>'",inputNameList[i],"' in (",inputList[i],")",sep=""))
      #sql <- paste("info ->>'",inputNameList[i],"' in (?input)",sep="")
      #query<-sqlInterpolate(con,sql,input=inputList[i][[1]])
      #filterList<-c(filterList,paste(" info ->>'",inputNameList[i],"' in (",inputList[i],")",sep=""))
    }
  }
    filterList<- paste(filterList, collapse = " and ")
    filterList<-substr(filterList,5,nchar(filterList))
    return(filterList)
  }

  q<-"WHERE info ->>'taxon' in ('Daucus carota') and info->>'trait_name' in ('SLA') and info ->>'functio_group' in ('Graminoid') and
  info ->>'sampling_type' ('Field') and info ->>'traitmeas_scale' ('Plant');"  
    
  
  observeEvent(input$runQuery,{
    shinyjs::hide("queryDl")
    withProgress(message="Browsing database",detail = "Please wait", value = 0,{
      incProgress(1/5)
    con <- dbConnect(RPostgres::Postgres(), dbname= "CropTrait", host="localhost", port=dbPort, user="postgres", password="Sonysilex915@")
    filterList<-dbManagement(con)
    AllDataQuery <- paste(readLines("Query.txt"), collapse=" ")
    if(!filterList==""){
          filteredQuery<-paste(substr(AllDataQuery,1,nchar(AllDataQuery)-1),"WHERE info->>'data_access' in ('Public','Public (notify the PIs)') and",filterList," ORDER BY info->>'id_bdd';",sep =" ")
          print(filteredQuery)
    } else {filteredQuery<-paste(substr(AllDataQuery,1,nchar(AllDataQuery)-1),"WHERE info->>'data_access' in ('Public','Public (notify the PIs)') ORDER BY info->>'id_bdd';",sep =" ")}
    incProgress(1/2)
    res <<- dbGetQuery(conn = con,statement = filteredQuery)
    dbDisconnect(con)
    
    userData<<- res %>% filter(data_access == "Public (notify the PIs)")
    if(length(userData[,1])>0){
      shinyjs::show("userInfos")
    } else {
    write.table(res,"queryRes.csv",sep = ";",row.names = FALSE)
    uploadData()
    shinyjs::show("queryDl")
    }
    })
  })
  
  taxonHandler<-function(taxonInput){
    nospace<-str_replace_all(taxonInput,", ",",")
    splited<- strsplit(nospace,",")
    editedTaxonInput<-sapply(splited, function(x) toString(sQuote(x,FALSE)))
    return(editedTaxonInput)
  }
  
  #####################################################
  ###############-Check data access-###################
  isInfosFilled<-observeEvent(input$submit,{
    if(!input$userMail=="" && isValidEmail(input$userMail)){
      PiToContact<-userData %>% distinct(userData$pi_contact)
    for(i in 1:length(PiToContact[,1])){
      nospace<- str_replace_all(PiToContact[i,1]," ","")
      splited<- strsplit(nospace,",")
      for(j in 1:length(splited[[1]])){
        system(paste("Rscript --vanilla mailSender.R",input$userMail,splited[[1]][j],input$projectSummary),wait = FALSE)
      }
    }
    write.table(res,"queryRes.csv",sep = ";",row.names = FALSE)
    uploadData()
    shinyjs::show("queryDl")
    shinyjs::hide("userInfos")
    }
  })

  ###-EMAIL PATTERN TO MATCH-###
  isValidEmail <- function(x) {
    grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
  }
  #####################################################
  ############### DOWNLOAD HANDLING ###################
  uploadData <- function() {
    output$queryDl <- downloadHandler(
      filename = function() {
        paste("Dataset-", Sys.time(), ".csv", sep="")
      },
      content = function(file) {
        file.copy("queryRes.csv",file)
      }
    )
  }
      output$fieldDesc <- downloadHandler(
      filename = "fieldsDescription.xlsx",
      content = function(file) {
        file.copy("fieldsDescription.xlsx",file)
      }
    )
  }