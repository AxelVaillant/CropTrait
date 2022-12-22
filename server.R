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
        AllDataQuery <- paste(readLines("/home/vaillant/Documents/Projets R/Projet CropTrait/CropTraits/Query.txt"), collapse="\n")
  con <- dbConnect(RPostgres::Postgres(), dbname= "CropTrait", host="localhost", port=dbPort, user="postgres", password="Sonysilex915@")

  traitQuery = "select info ->>'id_bdd' as id_bdd,info ->>'crop_name' as crop_name,info ->>'trait_name'as trait_name, info ->>'original_value'as original_value,
  info ->>'original_unit'as original_unit,info ->>'stage' as stage,info ->>'organ'as organ,
  info ->>'method'as method from croptrait;"
  
  taxonQuery ="select info ->>'id_bdd'as id_bdd, info ->>'taxon'as taxon, info ->>'taxon_accepted'as taxon_accepted,
  info ->>'family'as family,info ->>'genus' as genus,info ->>'species'as species,  info ->>'spauthor'as spauthor,
  info ->>'subtaxa'as subtaxa,  info ->>'subtauthor'as subtauthor,  info ->>'variety'as variety,
  info ->>'crop_name'as crop_name,  info ->>'gen_name'as gen_name,info ->>'select_type'as select_type from croptrait LIMIT 1000;"
  
      #####-Picker input-#####
  functio_group_query<-"SELECT DISTINCT info->>'functio_group' as Fonctional_group FROM croptrait2  ORDER BY info->>'functio_group';"
  functio_group<-dbGetQuery(con, functio_group_query)
  updatePickerInput(session, "Functional_group", choices = functio_group)
  updatePickerInput(session, "dlFunctional_group", choices = functio_group)

  sampling_type_query<-"SELECT DISTINCT info->>'sampling_type' as Sampling_type FROM croptrait2  ORDER BY info->>'sampling_type';"
  sampling_type<-dbGetQuery(con, sampling_type_query)
  updatePickerInput(session, "sampling_type", choices = sampling_type)
  updatePickerInput(session, "dlSampling_type", choices = sampling_type)

  trait_query<-"SELECT DISTINCT info->>'trait_name' as trait_name FROM croptrait2  ORDER BY info->>'trait_name';"
  traits<-dbGetQuery(con, trait_query)
  updatePickerInput(session, "dlTraits", choices = traits)
  
  scale_query<-"SELECT DISTINCT info->>'traitmeas_scale' as traitmeas_scale FROM croptrait2  ORDER BY info->>'traitmeas_scale';"
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

  ############-SLA vs LNC Plot-##################
  output$SLAvsLNC<-renderPlot({
    plot(GLOPNET$log.LMA ~ GLOPNET$log.Nmass, type="n",xlim=c(-0.8,1), ylim=c(0,2.7), 
     xlab=expression(bold("Leaf nitrogen content (LNC; log10; %)")),ylab=expression(bold("Specific leaf area (SLA; log10; m?/kg)")))
    points(GLOPNET$log.SLA ~ GLOPNET$log.Nmass, type="p", pch=20, col="grey")
      legend("topleft", "Glopnet",
       col="grey", pch=20, bty="n")
    
if(!(is.null(input$sampling_type) && is.null(input$Functional_group) && !(!input$taxon=="" && input$taxon %in% taxonTable[,1]))){
  
if(is.null(input$sampling_type) && !is.null(input$Functional_group) && !(!input$taxon=="" && input$taxon %in% taxonTable[,1])){
  Sample<- resAll %>% filter(functio_group == input$Functional_group)
    traitSplit(Sample)
  points(SampleSLA$log.SLA ~ SampleLNC$log.LNC, type="p", pch=19, col="Red")
    legend("topleft", c("Glopnet",input$Functional_group),col=c("grey","red"), pch=20, bty="n")
}
if(!is.null(input$sampling_type) && is.null(input$Functional_group) && !(!input$taxon=="" && input$taxon %in% taxonTable[,1])){
  Sample<- resAll %>% filter(sampling_type == input$sampling_type)
    traitSplit(Sample)
  points(SampleSLA$log.SLA ~ SampleLNC$log.LNC, type="p", pch=19, col="Red")
    legend("topleft", c("Glopnet",input$sampling_type),col=c("grey","red"), pch=20, bty="n")
}                     
if(!is.null(input$sampling_type) && !is.null(input$Functional_group) && !(!input$taxon=="" && input$taxon %in% taxonTable[,1])){
  Sample<- resAll %>% filter(functio_group == input$Functional_group & sampling_type == input$sampling_type)
    traitSplit(Sample)
  points(SampleSLA$log.SLA ~ SampleLNC$log.LNC, type="p", pch=19, col="Red")
    legend("topleft", c("Glopnet",paste(input$Functional_group,input$sampling_type,sep = " & ")) ,col=c("grey","red","blue"), pch=20, bty="n")
} 
if((!input$taxon=="" && input$taxon %in% taxonTable[,1]) && is.null(input$sampling_type) && is.null(input$Functional_group)){
  Sample<- resAll %>% filter(taxon == input$taxon)
    traitSplit(Sample)
  points(SampleSLA$log.SLA ~ SampleLNC$log.LNC, type="p", pch=19, col="Red")
  legend("topleft", c("Glopnet",input$taxon),col=c("grey","red"), pch=20, bty="n")
}
if((!input$taxon=="" && input$taxon %in% taxonTable[,1]) && !is.null(input$sampling_type) && is.null(input$Functional_group)){
  Sample<- resAll %>% filter(taxon == input$taxon & sampling_type == input$sampling_type)
    traitSplit(Sample)
  points(SampleSLA$log.SLA ~ SampleLNC$log.LNC, type="p", pch=19, col="Red")
    legend("topleft", c("Glopnet",paste(input$taxon,input$sampling_type,sep = " & ")),col=c("grey","red","blue"), pch=20, bty="n")
  }
if((!input$taxon=="" && input$taxon %in% taxonTable[,1]) && is.null(input$sampling_type) && !is.null(input$Functional_group)){
  Sample<- resAll %>% filter(taxon == input$taxon & functio_group == input$Functional_group)
    traitSplit(Sample)
  points(SampleSLA$log.SLA ~ SampleLNC$log.LNC, type="p", pch=19, col="Red")
    legend("topleft", c("Glopnet",paste(input$taxon,input$Functional_group,ep=" & ^")),col=c("grey","red"), pch=20, bty="n")
}
}
  })
  
  observeEvent(input$runGraph,{
    if(!input$taxon==""){
        Sample<- resAll %>% filter(taxon == input$taxon)
#Sample <-filter(Sample,sender_name=="Marney Isaac")#Temporaire
  SampleSLA<-filter(Sample,trait_name == "SLA")
  SampleLNC<-filter(Sample,trait_name == "LNC per leaf dry mass")
    
  SampleSLA$trait_original_value<-as.double(SampleSLA$trait_original_value)
  SampleSLA<-normalize(SampleSLA)
  SampleSLA$log.SLA <- log10(SampleSLA$trait_original_value)
  SampleLNC$trait_original_value<-as.double(SampleLNC$trait_original_value)
  SampleLNC$log.LNC <- log10(SampleLNC$trait_original_value)
  points(SampleSLA$log.SLA ~ SampleLNC$log.LNC, type="p", pch=19, col="Red")  

  legend("topleft", c("Glopnet",input$taxon),
       col=c("grey","red"), pch=20, bty="n")
    }
  })

  
  traitSplit<-function(Sample){
  SampleSLA<<-filter(Sample,trait_name == "SLA")
  SampleLNC<<-filter(Sample,trait_name == "LNC per leaf dry mass")
    
  SampleSLA$trait_original_value<<-as.double(SampleSLA$trait_original_value)
  SampleSLA<<-normalize(SampleSLA)
  SampleSLA$log.SLA <<- log10(SampleSLA$trait_original_value)
  SampleLNC$trait_original_value<-as.double(SampleLNC$trait_original_value)
  SampleLNC$log.LNC <<- log10(SampleLNC$trait_original_value)
  }
  
  normalize<-function(dataset){
  for(i in 1:length(dataset[,1])){
    if(dataset[i,]$trait_original_unit == "cm2/g"){
      dataset[i,]$trait_original_value <- dataset[i,]$trait_original_value/10
    }
    if(dataset[i,]$trait_original_unit == "cm2/mg"){
      dataset[i,]$trait_original_value <- dataset[i,]$trait_original_value*100
    }
    if(dataset[i,]$trait_original_unit == "mm2/mg"){
      dataset[i,]$trait_original_value <- dataset[i,]$trait_original_value/1000
    }
  }
  return(dataset)
  }
  
  ########################################################################
  #############################-Download Data-############################
  ########################################################################
  dbManagement<- function(){
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
    filterList<-dbManagement()
    AllDataQuery <- paste(readLines("/home/vaillant/Documents/Projets R/Projet CropTrait/CropTraits/Query.txt"), collapse=" ")
    if(!filterList==""){
          filteredQuery<-paste(substr(AllDataQuery,1,nchar(AllDataQuery)-1),"WHERE",filterList," ORDER BY info->>'id_bdd';",sep =" ")
          print(filteredQuery)
    } else {filteredQuery<-AllDataQuery}
    con <- dbConnect(RPostgres::Postgres(), dbname= "CropTrait", host="localhost", port=dbPort, user="postgres", password="Sonysilex915@")
    incProgress(1/2)
    res <- dbGetQuery(conn = con,statement = filteredQuery)
    dbDisconnect(con)
    write.table(res,"queryRes.csv",sep = ";",row.names = FALSE)
    uploadData()
    })
    shinyjs::show("queryDl")
  })
  
  taxonHandler<-function(taxonInput){
    nospace<-str_replace_all(taxonInput,", ",",")
    splited<- strsplit(nospace,",")
    editedTaxonInput<-sapply(splited, function(x) toString(sQuote(x,FALSE)))
    return(editedTaxonInput)
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
  
  }