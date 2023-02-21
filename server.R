plan(multisession)
function(input,output,session){
  
  #-------Get credentials----------------------#
  credentials<-read.table(file = "credentials.csv",header = TRUE,sep = "\t")
  dbHost<-credentials$dbHost
  dbPort<-credentials$dbPort
  dbUser<-credentials$dbUser
  dbPassword<-credentials$dbPassword
  
  ###
  resAll <-read.table("DB.csv",header = T,sep=";", dec=".", fill=T)
  taxonTable <-read.table("taxon_Table.csv",header = T,sep=";", dec=".",quote='"', fill=FALSE)

  #######################################################
  ################-DATABASE MANAGEMENT-##################
  observe({
    tryCatch({
        AllDataQuery <- paste(readLines("Query.txt"), collapse="\n")
  #con <- dbConnect(RPostgres::Postgres(), dbname= "CropTrait", host="localhost", port=dbPort, user="postgres", password="Sonysilex915@")
  con <- dbConnect(RPostgres::Postgres(), dbname= "croptrait", host=dbHost, port=dbPort, user=dbUser, password=dbPassword)
  ###-Selectize input-###
  updateSelectizeInput(session, "taxons", choices = sort(taxonTable$taxon),server = T)
  updateSelectizeInput(session, "varSpecies", choices = c("",sort(taxonTable$taxon)),server = T)
  updateSelectizeInput(session, "dlTaxon", choices = sort(taxonTable$taxon),server = T)

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
  updateSelectInput(session, "varTraits", choices = traits)
  
  scale_query<-"SELECT DISTINCT info->>'traitmeas_scale' as traitmeas_scale FROM croptrait  ORDER BY info->>'traitmeas_scale';"
  scaleRes<-dbGetQuery(con, scale_query)
  updatePickerInput(session, "dlScale", choices = scaleRes)
  
  #resAll <<- dbGetQuery(conn = con,statement = AllDataQuery)
  
  dbDisconnect(con)
    })
  })

  ##############################################################################
  ##########################-PLOTS-#############################################
  ##############################################################################
  
  ####-GLOPNET-####
    GLOPNET <-read.table("GLOPNETdata.csv",header = T,sep=";", dec=".",quote="", fill=FALSE)
    str(GLOPNET)
    GLOPNET$LMA <- 10^(GLOPNET$log.LMA) #g/m2
    GLOPNET$LMA <- GLOPNET$LMA *10^-3 #kg/m2
    GLOPNET$SLA <- (1/GLOPNET$LMA) #m2/kg
    GLOPNET$log.SLA <- log10(GLOPNET$SLA)
    

    ###-Reactive data for plotting-###
    myReact <- reactiveValues()

        ###-Asynchrone-###
 #  observeEvent(input$visualizeR,{
 #    future_promise({
 #      return((concatenateQuery()))
 #    }) %...>%(function(Sample){
 #      genList<-traitSplitByInd(Sample)
 #      myReact$toPlot<-rbind(myReact$toPlot,genList) 
 #      })%...!%(function(error){
 #      myReact$toPlot<-NULL
 #      warning("Plot Error")
 #    }) 
 #    })
    
    
    ###-Add one condition to plot-###
    observeEvent(input$visualize,{
      withProgress(message="Processing in progress",detail = "Please wait", value = 0,{
      Sample<-concatenateQuery()
      incProgress(1/5)
      if(!is.null(Sample)){
        shinyjs::hide('resText')
        incProgress(2/5)
        if(input$visuType == "By individual"){
          genList<-traitSplitByInd(Sample)
        } else if(input$visuType == "By genotype"){
          genList<-traitSplit(Sample)
        }
        incProgress(3/5)
        if(length(genList[,1]) >0 & is.na(genList$filterName[1])){
          genList$filterName<-"All Data"
        }
        displayVisuResult(genList)
        incProgress(4/5)
        myReact$toPlot<-rbind(myReact$toPlot,genList)
        shinyjs::show('resText')
        }})
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
  if(!is.null(c(visuInputList[[1]],visuInputList[[2]],visuInputList[[3]]))){
    
  
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
  } else {
    return(resAll)
  }
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
  ###-Trait variability / Taxon res-###
  taxonByTraitResult<-function(list){
    if(length(list[,1])==0){
          output$traitVarText<-renderText({
          paste("There is no matching data of this taxon for this trait.",
                     "Please retry with another taxon", sep="\n ")
        })
    } else {
        output$traitVarText<-renderText({
        paste("There are ",length(list[,1]) ," individuals matching those filters in the database.", sep="")
        })
    }}    
    
  ##########-Filtering by genotypes-###########
  traitSplit<-function(Sample){
  filterLegend<-legendHandler()  
  genotypes<-(Sample %>% distinct(gen_name))
  genotypes<-drop_na(genotypes)
  genList<-data.frame(matrix(ncol=3,nrow=0))
  colnames(genList) <-c('meanSLA','meanLNC','filterName')
  genList$filterName<-as.character(genList$filterName)
  for (i in 1:length(genotypes[[1]])){
    genSample <- filter (Sample,gen_name==genotypes[[1]][i])
    genSample$standardized_value <- as.double(genSample$standardized_value)
    genSample <- genSample %>% drop_na(standardized_value)
    genSampleSLA<-filter(genSample,trait_name == "SLA")
    genSampleLNC<-filter(genSample,trait_name == "LNC per leaf dry mass")
    if(length(genSampleSLA[[1]])>0 && length(genSampleLNC[[1]]>0)){
      #genSampleSLA<-normalize(genSampleSLA)
      genList<-genList %>% add_row(meanSLA = mean(genSampleSLA$standardized_value), meanLNC = mean(genSampleLNC$original_value), filterName = filterLegend)
    }
  }
  genList$meanSLA<-log10(genList$meanSLA)
  genList$meanLNC<-log10(genList$meanLNC)
  ##
  return(genList)
  }
  
  ##########-Filtering by individuals-###########
  traitSplitByInd<-function(Sample){
  filterLegend<-legendHandler()  
  indList<-data.frame(matrix(ncol=3,nrow=0))
  colnames(indList) <-c('meanSLA','meanLNC','filterName')
  indList$filterName<-as.character(indList$filterName)
    indSample <-Sample
    indSample$standardized_value <- as.double(indSample$standardized_value)
    indSample <- indSample %>% drop_na(standardized_value)
    indSampleSLA<-filter(indSample,trait_name == "SLA")
    indSampleLNC<-filter(indSample,trait_name == "LNC per leaf dry mass")
    if(length(indSampleSLA[[1]])>0 && length(indSampleLNC[[1]]>0)){
    if(length(indSampleSLA[,1]) != length(indSampleLNC[,1])){
      matchingInd<-intersect(indSampleLNC$id_occurence,indSampleSLA$id_occurence)
      if(length(matchingInd)>0){
      for(i in 1:length(matchingInd)){
      indexSLA<- match(matchingInd[i],indSampleSLA$id_occurence)
      indexLNC<- match(matchingInd[i],indSampleLNC$id_occurence)
      indList<-indList %>% add_row(meanSLA = indSampleSLA[indexSLA,]$standardized_value, meanLNC = indSampleLNC[indexLNC,]$original_value, filterName = filterLegend)
      }}
    } else {
        for (i in 1:length(Sample[,1])){
          indList<-indList %>% add_row(meanSLA = indSampleSLA[i,]$standardized_value, meanLNC = indSampleLNC[i,]$original_value, filterName = filterLegend)
        }
    }
      }
  indList$meanSLA<-log10(indList$meanSLA)
  indList$meanLNC<-log10(indList$meanLNC)
  ##
  return(indList)
  }

  
  normalize<-function(dataset){
  for(i in 1:length(dataset[,1])){
    if(dataset[i,]$original_unit == "cm2/g"){
      dataset[i,]$original_value <- dataset[i,]$original_value/10
    }
    if(dataset[i,]$original_unit == "cm2/mg"){
      dataset[i,]$original_value <- dataset[i,]$original_value*100
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
  observeEvent(c(input$reset,input$visuType),{
    shinyjs::hide("resText")
    if(isTRUE(input$glopnetData)){
      myReact$toPlot <- myReact$toPlot %>% filter(filterName == "Glopnet")  
    } else {
      myReact$toPlot<-NULL  
    }
  })
  ########################################################################
  #########################-Trait variability-############################
  ########################################################################
    output$traitVariability<-renderPlot({
      trait <- resAll %>% filter(trait_name == input$varTraits)
      if(length(trait[,1]) > 0){
      #-building dataframe for general traits values-#  
      dfTrait <-data.frame(value = trait$standardized_value, unit = trait$standardized_unit, condition = trait$trait_name, dataset = "trait")
      if(input$varTraits!="Leaf Thickness"){
       dfTrait<-removeOutliers(dfTrait) 
      }
      if(input$varSpecies!=""){
      #-filter specific taxon values for the trait-#  
      specie <- trait %>% filter(taxon == input$varSpecies)
      if(length(specie[,1]) > 0){
      #-dataframes concatenation-#  
      dfTax<-data.frame(value = specie$standardized_value, unit = specie$standardized_unit, condition = specie$taxon, dataset = "taxon")
      dfTrait<-rbind(dfTrait,dfTax)
      dfTrait$dataset <-factor(dfTrait$dataset,levels=c("trait","taxon"))
      #-Plotting parameters-#
      ggplot( dfTrait,aes(x=value, fill=dataset)) +
      geom_histogram(aes(y=..density..), color="darkblue", alpha=0.5, position = 'identity',bins=30) +
      geom_density(alpha=.3) +
      scale_fill_manual(values=c("lightblue", "lightcoral"),labels=c("General",dfTax[1,]$condition)) + 
      labs(x =paste("value (",dfTrait[1,]$unit,")",sep =""),title = paste(trait[1,]$trait_name,"variability",sep=" "),fill="") +
      theme(legend.position = c(0.9,0.9)) 
      } else {
        ggplot(dfTrait, aes(x = value)) +geom_histogram(color="darkblue", fill="lightblue", bins=30) +labs(x =paste("value (",dfTrait[1,]$unit,")",sep =""),title = paste(dfTrait[1,]$condition,"variability",sep=" "),)
        }
      } else {
        ggplot(dfTrait, aes(x = value)) +geom_histogram(color="darkblue", fill="lightblue", bins=30) +labs(x =paste("value (",dfTrait[1,]$unit,")",sep =""),title = paste(dfTrait[1,]$condition,"variability",sep=" "),)
      }
      }
      })
  
  observeEvent(c(input$varSpecies,input$varTraits),{
    trait <- resAll %>% filter(trait_name == input$varTraits)
    if(input$varSpecies!=""){
      specie <- trait %>% filter(taxon == input$varSpecies)
      taxonByTraitResult(specie)
    } else {
      taxonByTraitResult(trait)
    }
  })
  
  removeOutliers<-function(df){
    variable<-na.omit(df$value)
    quartiles <- quantile(variable, probs=c(.25, .75), na.rm = FALSE)
    IQR<-IQR(variable)
    Lower <- quartiles[1] - 1.5*IQR
    Upper <- quartiles[2] + 1.5*IQR
    data_no_outliers<-subset(df,df$value > Lower & df$value < Upper)
    length(data_no_outliers)
    return(data_no_outliers)
  }
  ########################################################################
  #############################-Download Data-############################
  ########################################################################
  
  ###-Concatenate query-###
  dbManagement<- function(con){
  filterList<-""
  inputs<-NULL
  inputList<-list(input$dlTaxon,input$scale,input$dlTraits,input$dlFunctional_group,input$dlSampling_type)
  inputNameList<-list("taxon","traitmeas_scale","trait_name","functio_group","sampling_type")
  for(i in 1:length(inputList)){
    if(!is.null(inputList[i][[1]]) &&  !inputList[i]==""){
      inputList[i][[1]]<-sapply(strsplit(inputList[i][[1]],","), function(x) toString(sQuote(x,F)))
      inputs<-paste(inputList[i][[1]],collapse=",")
      filterList<-c(filterList,paste(" info ->>'",inputNameList[i],"' in (",inputs,")",sep=""))
      #sql <- paste("info ->>'",inputNameList[i],"' in (?input)",sep="")
      #query<-sqlInterpolate(con,sql,input=inputs)
    }
  }
    filterList<- paste(filterList, collapse = " and ")
    filterList<-substr(filterList,5,nchar(filterList))
    return(filterList)
  }
  
  observeEvent(input$runQuery,{
    shinyjs::hide("queryDl")
    withProgress(message="Browsing database",detail = "Please wait", value = 0,{
      incProgress(1/5)
    #con <- dbConnect(RPostgres::Postgres(), dbname= "croptrait", host="localhost", port=dbPort, user="postgres", password="Sonysilex915@")
    con <- dbConnect(RPostgres::Postgres(), dbname= "croptrait", host=dbHost, port=dbPort, user=dbUser, password=dbPassword)
    filterList<-dbManagement(con)
    AllDataQuery <- paste(readLines("Query.txt"), collapse=" ")
    if(!filterList==""){
      ifelse(isTRUE(adminMode$isAdmin),
             filteredQuery<-paste(substr(AllDataQuery,1,nchar(AllDataQuery)-1),"WHERE",filterList," ORDER BY info->>'id_bdd';",sep =" "),
             filteredQuery<-paste(substr(AllDataQuery,1,nchar(AllDataQuery)-1),"WHERE info->>'data_access' in ('Public','Public (notify the PIs)') and",filterList," ORDER BY info->>'id_bdd';",sep =" "))
      print(filteredQuery)
    } else {
      ifelse(isTRUE(adminMode$isAdmin),
             filteredQuery<-paste(substr(AllDataQuery,1,nchar(AllDataQuery)-1),"ORDER BY info->>'id_bdd';",sep =" "),
             filteredQuery<-paste(substr(AllDataQuery,1,nchar(AllDataQuery)-1),"WHERE info->>'data_access' in ('Public','Public (notify the PIs)') ORDER BY info->>'id_bdd';",sep =" "))
      }
    incProgress(1/2)
    res <<- dbGetQuery(conn = con,statement = filteredQuery)
    dbDisconnect(con)
    
    userData<<- res %>% filter(data_access == "Public (notify the PIs)")
    if(length(userData[,1])>0 & !isTRUE(adminMode$isAdmin)){
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
  ###############-Admin access to data#################
  observeEvent(input$dataAccessMode,{
    if(input$dataAccessMode == "Admin"){
      shinyjs::show("admin")
    } else {
      shinyjs::hide("admin")
      updateTextInput(session,"adminPswd",value = "")
      adminMode$pass<-""
      adminMode$isAdmin<-FALSE
      shinyjs::hide("pass")
    }
  })

  adminMode<-reactiveValues()
  adminMode$message<-""
  adminMode$isAdmin<-FALSE
  observeEvent(input$submitPswd,{
    if(input$adminPswd == "croptraitpass2023"){
      adminMode$message<- "Admin mode"
      adminMode$isAdmin<-TRUE
      shinyjs::hide("admin")
    } else {
      adminMode$message<-"Invalid password"
    }
    shinyjs::show("pass")
  })
  
  output$passError<-renderText({
    adminMode$message
  })
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
      output$taxonTable <- downloadHandler(
      filename = "taxon_Table.csv",
      content = function(file) {
        file.copy("taxon_Table.csv",file)
      }
    )
  }