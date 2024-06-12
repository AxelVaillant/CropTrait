plan(multisession)
function(input,output,session){
  options(useFancyQuotes = FALSE)
  #-------Get credentials----------------------#
  credentials<-read.table(file = "CSV/credentials.csv",header = TRUE,sep = "\t")
  dbHost<-credentials$dbHost
  dbPort<-credentials$dbPort
  dbUser<-credentials$dbUser
  dbPassword<-credentials$dbPassword
  
  ###
  taxonTable <-read.table("CSV/taxon_Table.csv",header = F,sep=";", dec=".",quote='"', fill=FALSE)

  #######################################################
  ################-DATABASE MANAGEMENT-##################
  observe({
    tryCatch({
  con <- dbConnect(RPostgres::Postgres(), dbname= "croptrait", host=dbHost, port=dbPort, user=dbUser, password=dbPassword)
  
  ###-Database recap-###
  recap_trait<-'SELECT count("Name") from "Trait";'
  recap_obs<-'SELECT COUNT(*) from "Observation";'
  recap_geno<-'SELECT count("Gen_name") from "Taxonomy_extended";'
  recap_taxons<-'SELECT count("Taxon_name") from "Taxonomy_essentials";'
    
  res_traits<-dbGetQuery(con, recap_trait)
  res_obs<-dbGetQuery(con, recap_obs)
  res_geno<-dbGetQuery(con, recap_geno)
  res_taxons<-dbGetQuery(con, recap_taxons)
  fieldName<-data.frame(c("Traits","Observations","Uniques genotypes","Known taxons"))
  colnames(fieldName)<-"Content"
  fieldRes<-data.frame(c(res_traits$count,res_obs$count,res_geno$count,res_taxons$count))
  colnames(fieldRes)<-"Total"
  recapList<-cbind(fieldName,fieldRes)
  recapList$Total<-as.integer(recapList$Total)
  output$recapTable<-renderTable(recapList,width=450,height=400,striped = T,hover = T,bordered = T)
  
  ###-Selectize input-###
  TaxTabNb_query<-paste0('SELECT "Taxon_name", COUNT(*) from "Observation" group by "Taxon_name" order by "Taxon_name";')
  resTaxTableNb<-dbGetQuery(con, TaxTabNb_query)
  TaxTabNb<-data.frame()
  for(i in 1:length(resTaxTableNb$Taxon_name)){
    x<-paste0(resTaxTableNb$Taxon_name[i],"-",resTaxTableNb$count[i])
    TaxTabNb<-rbind(TaxTabNb,x)
  }
  updateSelectizeInput(session, "taxons", choices = TaxTabNb[,1],server = T)
  updateSelectizeInput(session, "dlTaxon", choices = TaxTabNb[,1],server = T)

  #####-Picker input-#####
  functio_group_query<-'SELECT DISTINCT "Functio_group" as Functional_group FROM "Taxonomy_essentials"  ORDER BY "Functio_group";'
  functio_group<-dbGetQuery(con, functio_group_query)
  updatePickerInput(session, "Functional_group", choices = functio_group)
  updatePickerInput(session, "dlFunctional_group", choices = functio_group)

  sampling_type_query<-'SELECT DISTINCT "Type" as Sampling_type FROM "Sampling_site"  ORDER BY "Type";'
  sampling_type<-dbGetQuery(con, sampling_type_query)
  updatePickerInput(session, "sampling_type", choices = sampling_type)
  updatePickerInput(session, "dlSampling_type", choices = sampling_type)
  

  trait_query<-'SELECT DISTINCT "Name" as trait_name FROM "Trait"  ORDER BY "Name";'
  traits<-dbGetQuery(con, trait_query)
  updatePickerInput(session, "dlTraits", choices = traits)
  updateSelectInput(session, "varTraits", choices = traits)
  updatePickerInput(session, "bbtr_Traits", choices = c("",traits))
  
  scale_query<-'SELECT DISTINCT "Observation_levels" as observationLevels FROM "Observation"  ORDER BY "Observation_levels";'
  scaleRes<-dbGetQuery(con, scale_query)
  updatePickerInput(session, "dlScale", choices = scaleRes)
  
  ##-Browse database fields-##
  bbta_functio_group_query<-'SELECT DISTINCT "Functio_group" as family FROM "Taxonomy_essentials"  ORDER BY "Functio_group";'
  bbta_functio_group<-dbGetQuery(con, bbta_functio_group_query)
  updatePickerInput(session, "bbta_functio_group", choices = bbta_functio_group)
  
  dbDisconnect(con)
    })
  })
  #########################################
  ####-Browsing page dynamic filtering -###
  #########################################
  #------Browse by Traits-----#
  observeEvent(input$bbtr_Traits,{
    if(length(input$bbtr_Traits)>0 && input$bbtr_Traits!=""){
        con <- dbConnect(RPostgres::Postgres(), dbname= "croptrait", host=dbHost, port=dbPort, user=dbUser, password=dbPassword)
        bbtr_taxonQuery<-paste('SELECT "Taxon_name", count(*) FROM "Observation" where "Trait" in (',sQuote(paste(input$bbtr_Traits, collapse = "' ,'")),')
        GROUP BY "Taxon_name" ORDER BY "Taxon_name";',sep="")
        bbtr_taxonList<-dbGetQuery(con, bbtr_taxonQuery)
        dbDisconnect(con)
        bbtr_dynamicList<-data.frame(matrix(nrow=0,ncol=0))
        for(i in 1:length(bbtr_taxonList$genus)){
          bbtr_dynamicList<-rbind(bbtr_dynamicList,paste(bbtr_taxonList[i,1],bbtr_taxonList[i,2],sep=" "))
        }
        bbtr_taxonList$count<-as.integer(bbtr_taxonList$count)
        output$bbtr_table<-renderTable(bbtr_taxonList,striped = T,hover = T,bordered = T)
        output$bbtr_title<-renderText(HTML(paste0("Available data for <b>",input$bbtr_Traits,"</b>")))
    } else{output$bbtr_table<-renderTable(NULL)
          output$bbtr_title<-renderText(NULL)
}
  },ignoreNULL = FALSE)
  
  #------Browse by Taxon-----#
    observeEvent(input$bbta_functio_group,{
    if(length(input$bbta_functio_group)>0){
        con <- dbConnect(RPostgres::Postgres(), dbname= "croptrait", host=dbHost, port=dbPort, user=dbUser, password=dbPassword)
        bbta_taxonQuery<-paste('SELECT DISTINCT T1."Taxon_name" as ObsTax FROM "Observation" as T1 INNER JOIN "Taxonomy_essentials" as T2 on 
                               T1."Taxon_name" = T2."Taxon_name" AND T2."Functio_group" in (',sQuote(paste(input$bbta_functio_group, collapse = "' ,'")),
                               ') ORDER BY T1."Taxon_name";',sep="")
        bbta_taxonList<-dbGetQuery(con, bbta_taxonQuery)
        dbDisconnect(con)
        updateSelectizeInput(session, "bbta_Taxon", choices = c("",bbta_taxonList[,1]),server = T)
    } else{updateSelectizeInput(session, "bbta_Taxon", choices = "")}
  },ignoreNULL = FALSE)
    
    observeEvent(input$bbta_Taxon,{
    if(length(input$bbta_Taxon)>0 && input$bbta_Taxon!=""){
        con <- dbConnect(RPostgres::Postgres(), dbname= "croptrait", host=dbHost, port=dbPort, user=dbUser, password=dbPassword)
        bbta_traitQuery<-paste('SELECT  "Trait", count(*) FROM "Observation" where "Taxon_name" in (',sQuote(paste(input$bbta_Taxon, collapse = "' ,'")),
                               ') GROUP BY "Trait" order by "Trait";',sep="")         
        bbta_traitList<-dbGetQuery(con, bbta_traitQuery)
        dbDisconnect(con)
        bbta_traitList$count<-as.integer(bbta_traitList$count)
        output$bbta_title<-renderText(HTML(paste0("Available data for <b>",input$bbta_Taxon,"</b>")))
        output$bbta_table<-renderTable(bbta_traitList,striped = T,hover = T,bordered = T)
    } else{output$bbta_table<-renderTable(NULL)
          output$bbta_title<-renderText(NULL)}
  },ignoreNULL = FALSE)
    
  #################################################    
  ####-dynamic taxon list on trait availability-###
  observeEvent(input$varTraits,{
    if(input$varTraits!=""){
        con <- dbConnect(RPostgres::Postgres(), dbname= "croptrait", host=dbHost, port=dbPort, user=dbUser, password=dbPassword)
        taxonQuery<-paste('SELECT DISTINCT "Taxon_name" FROM "Observation" WHERE "Trait"=',sQuote(input$varTraits),' ORDER BY "Taxon_name";',sep="")
        taxonList<-dbGetQuery(con, taxonQuery)
        updateSelectizeInput(session, "varSpecies", choices = c("",taxonList$Taxon_name),server = T)
        dbDisconnect(con)
    }
  })
  ##############################################################################
  ##########################-PLOTS-#############################################
  ##############################################################################
  
  ####-GLOPNET-####
    GLOPNET <-read.table("CSV/GLOPNETdata.csv",header = T,sep=";", dec=".",quote="", fill=FALSE)
    str(GLOPNET)
    GLOPNET$LMA <- 10^(GLOPNET$log.LMA) #g/m2
    GLOPNET$LMA <- GLOPNET$LMA *10^-3 #kg/m2
    GLOPNET$SLA <- (1/GLOPNET$LMA) #m2/kg
    GLOPNET$log.SLA <- log10(GLOPNET$SLA)
    

    ###-Reactive data for plotting-###
    myReact <- reactiveValues()
    
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
        if(input$visuType == "By individual"){
          displayVisuResultInd(genList)
        } else if(input$visuType == "By genotype"){
          displayVisuResultGen(genList)
        }
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
          legend.position = c(0.1,0.9),
          axis.title = element_text(size=14, face="bold", colour = "black"),
          axis.text = element_text(size=12, face="bold", colour = "black"),
          legend.title = element_text(size=12, face="bold", colour = "#757574"),
          legend.text = element_text(size=11))
    } else {
      df <- data.frame(x=rnorm(20),y=rnorm(20,1,0.5))
      ggplot(df,aes(x,y)) +geom_blank() + xlim(-0.8,1) + ylim(0,2.7) + labs(x = "Leaf nitrogen content (LNC; log10; %)" , y= "Specific leaf area (SLA; log10; m2/kg)", colour = "Conditions") +
        theme(
          legend.position = c(0.1,0.9),
          axis.title = element_text(size=14, face="bold", colour = "black"),
          axis.text = element_text(size=12, face="bold", colour = "black"),
          legend.title = element_text(size=12, face="bold", colour = "#757574"),
          legend.text = element_text(size=11))
    }
  })
  
  ############################
  #####-Build glopnet df -####
  ############################
  buildGlopnet<-function(){
    glopnetDf<-data.frame(matrix(ncol=5,nrow=0))
    colnames(glopnetDf) <-c('meanSLA','meanLNC','filterName','taxon',"variety")
    glopnetDf$filterName<-as.character(glopnetDf$filterName)
    glopnetDf$taxon<-as.character(glopnetDf$taxon)
    glopnetDf$variety<-as.character(glopnetDf$variety)
    for(i in 1:length(GLOPNET[,1])){
      glopnetDf<-glopnetDf %>% add_row(meanSLA = GLOPNET[i,]$log.SLA, meanLNC = GLOPNET[i,]$log.Nmass, filterName = "Glopnet",taxon = GLOPNET[i,]$Species, variety = NA)
    }
    return(glopnetDf)
  }
  
  #############################################################
  ######-Concatenate filters and get corresponding data-#######
  #############################################################
  concatenateQuery<-function(){
  visuInputList<-list(input$taxons,input$Functional_group,input$sampling_type)
  visuInputNames<-c("Taxon_name","Functio_group","Type")
  con <- dbConnect(RPostgres::Postgres(), dbname= "croptrait", host=dbHost, port=dbPort, user=dbUser, password=dbPassword)
  visuQuery<-NULL
  #-Check if at least one filtered is used
  if(!is.null(c(visuInputList[[1]],visuInputList[[2]],visuInputList[[3]]))){
  for(i in 1:length(visuInputList)){
    if(!is.null(visuInputList[i][[1]])){
      ###particular case for taxons
      if(i==1){
        dfTaxons<-NameSpliterRela(input$taxons)
        if(length(dfTaxons$allTax)==1){
          query <- paste0('Obs."Taxon_name" IN (',sQuote(dfTaxons$allTax),')')
        } else {
          query<-noquote(paste(noquote(list(dfTaxons$allTax)),collapse=","))
          query<-str_replace_all(query,'"',"'")
          query<-substr(query,2,nchar(query))
          query <- paste0('Obs."Taxon_name" IN ',query)
        }
      } else {
        ###functio_group and sampling_type
         if(length(visuInputList[i][[1]])==1){
          query <- paste0(dQuote(visuInputNames[i]), " IN ('",visuInputList[i],"')")
         } else {
           filter<-noquote(paste(visuInputList[i],collapse=","))
           filter<-str_replace_all(filter,'"',"'")
           filter<-substr(filter,2,nchar(filter))
           query <- paste0(dQuote(visuInputNames[i]), " IN ",filter)
         }
      }
      if(is.null(visuQuery)){
        visuQuery<-query
      } else {
        visuQuery<-paste(visuQuery,query,sep = " AND ")
      }
          }
  }
  if(!is.null(visuQuery)){
    ###filtering
      query<-paste0('SELECT Obs."Taxon_name","Standardized_value","Standardized_unit","Original_value","Trait","Gen_name","Subtaxa","Plant_identification" from "Observation" as Obs 
                    	JOIN "General_information" ON Obs."General_informationId" = "General_information"."General_informationId" 
	                    JOIN "Sampling_site" ON "Sampling_site"."Sampling_siteId" = "General_information"."Sampling_site"
	                    JOIN "Taxonomy_essentials" as Taxess ON Taxess."Taxon_name" = Obs."Taxon_name" 
                      JOIN "Taxonomy_extended" as Taxext ON Taxext."ExtendedTaxId" = Obs."Taxonomy_extended" WHERE ',visuQuery)
      res<-dbGetQuery(con, query)
      return(res)
  } else { return(NULL)}
    #-Case when no filters have been selected so all data are selected
  } else {
    AllDataQuery <- paste(readLines("CSV/Query2.txt"), collapse=" ")
    #-Query to retrieve the whole database
    res<-dbGetQuery(con, AllDataQuery)
    names(res)[names(res) == "trait"] <- "Trait"
    return(res)
  }
  dbDisconnect(con)
    }
  ##########################################################
  ##########-Print database matching results-###########
  ##########################################################
  displayVisuResultGen<-function(genList){
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
  displayVisuResultInd<-function(genList){
    if(length(genList[[1]])==0){
          output$resText<-renderText({
          paste("Your filters doesn't match any SLA/LNC couple in the database.",
                     "Please retry with other criteria", sep="\n ")
        })
    } else {
        output$resText<-renderText({
        paste("Your filters match ",length(genList[[1]]) ," individuals in the database.", sep="")
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
  genotypes<-Sample %>% distinct(Gen_name)
  genotypes<-drop_na(genotypes)
  genList<-data.frame(matrix(ncol=5,nrow=0))
  colnames(genList) <-c('meanSLA','meanLNC','filterName',"taxon","variety")
  genList$filterName<-as.character(genList$filterName)
  genList$taxon<-as.character(genList$taxon)
  genList$variety<-as.character(genList$variety)
  if(!is.na(genotypes[1,])){
  for (i in 1:length(genotypes[[1]])){
    genSample <- filter (Sample,Gen_name==genotypes[[1]][i])
    genSample$Standardized_value <- as.double(genSample$Standardized_value)
    genSample <- genSample %>% drop_na(Standardized_value)
    genSampleSLA<-filter(genSample,Trait == "Specific leaf area")
    genSampleLNC<-filter(genSample,Trait == "LNC per leaf dry mass")
    if(length(genSampleSLA[[1]])>0 && length(genSampleLNC[[1]]>0)){
      #-We have both SLA and LNC value so we add a new row to the dataframe
      genList<-genList %>% add_row(meanSLA = mean(genSampleSLA$Standardized_value), meanLNC = mean(genSampleLNC$Original_value), filterName = filterLegend,taxon = genSample[1,]$Taxon_name, variety = genSample[1,]$Subtaxa)
    }
  }
  #-Log scale for better visualization  
  genList$meanSLA<-log10(genList$meanSLA)
  genList$meanLNC<-log10(genList$meanLNC)
  }
  ##
  return(genList)
  }
  
  ##########-Filtering by individuals-###########
  traitSplitByInd<-function(Sample){
  filterLegend<-legendHandler()  
  indList<-data.frame(matrix(ncol=5,nrow=0))
  colnames(indList) <-c('meanSLA','meanLNC','filterName',"taxon","variety")
  indList$filterName<-as.character(indList$filterName)
  indList$taxon<-as.character(indList$taxon)
  indList$variety<-as.character(indList$variety)
    indSample <-Sample
    indSample$Standardized_value <- as.double(indSample$Standardized_value)
    indSample <- indSample %>% drop_na(Standardized_value)
    indSampleSLA<-filter(indSample,Trait == "Specific leaf area")
    indSampleLNC<-filter(indSample,Trait == "LNC per leaf dry mass")
    if(length(indSampleSLA[[1]])>0 && length(indSampleLNC[[1]]>0)){
      #-Find matching individual by Plant identification
      matchingInd<-intersect(indSampleLNC$Plant_identification,indSampleSLA$Plant_identification)
      if(length(matchingInd)>0){
    if(length(indSampleSLA[,1]) != length(indSampleLNC[,1])){
      #-If size is different we have to find the matching individuals
      if(length(matchingInd)>0){
      for(i in 1:length(matchingInd)){
      indexSLA<- match(matchingInd[i],indSampleSLA$Plant_identification)
      indexLNC<- match(matchingInd[i],indSampleLNC$Plant_identification)
      indList<-indList %>% add_row(meanSLA = indSampleSLA[indexSLA,]$Standardized_value, meanLNC = indSampleLNC[indexLNC,]$Original_value, filterName = filterLegend, taxon = indSampleSLA[indexSLA,]$Taxon_name, variety = indSampleSLA[indexSLA,]$Subtaxa)
      }}
    } else {
      #If size is similar we can add all individuals
        for (i in 1:length(Sample[,1])){
          indList<-indList %>% add_row(meanSLA = indSampleSLA[i,]$Standardized_value, meanLNC = indSampleLNC[i,]$Original_value, filterName = filterLegend, taxon = indSampleSLA[i,]$Taxon_name, variety = indSampleSLA[i,]$Subtaxa)
        }
    }
      }}
  #-Log scale for better visualization
  indList$meanSLA<-log10(indList$meanSLA)
  indList$meanLNC<-log10(indList$meanLNC)
  ##
  return(indList)
  }
  
  legendHandler<-function(){
        filterUsed<-c(input$taxons,input$Functional_group,input$sampling_type)
        legendText<-NULL
        if(length(filterUsed)>0){
        for(i in 1:length(filterUsed)){
          #-Cut Numbers-#
          splited<- strsplit(filterUsed[i],"-") 
          fixedLegend<-splited[[1]][1]
          if(is.null(legendText)){
            legendText<-fixedLegend
          } else {
            legendText<-paste(legendText,fixedLegend,sep = " & ")
          }
        }
        legendText <- gsub("&","& \n",legendText)          
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
  ###-Info onMouseover
  output$hover_info <- renderUI({
    if(!is.null(myReact$toPlot)){
    if(!is.na(myReact$toPlot[1,1] & myReact$toPlot[1,2])){
    hover<-input$plot_hover
    point <- nearPoints(myReact$toPlot, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Specie: </b>", point$taxon), "<br/>",
                    "<b> Variety: </b>", point$variety, "<br/>",))) 
    }
    }
  })
  
  
  ########################################################################
  #########################-Trait variability-############################
  ########################################################################
    output$traitVariability<-renderPlot({
      con <- dbConnect(RPostgres::Postgres(), dbname= "croptrait", host=dbHost, port=dbPort, user=dbUser, password=dbPassword)
      query<-paste0('SELECT "Taxon_name","Standardized_value","Standardized_unit","Trait" from "Observation" WHERE "Trait"=',sQuote(input$varTraits))
      trait<-dbGetQuery(con, query)
      dbDisconnect(con)      
      if(length(trait[,1]) > 0){
      #-building dataframe for general traits values-#  
      dfTrait <-data.frame(value = trait$Standardized_value, unit = trait$Standardized_unit, condition = trait$Trait, dataset = "trait")
      #-Special case
      if(input$varTraits!="Leaf Thickness"){
       dfTrait<-removeOutliers(dfTrait) 
      }
      if(input$varSpecies!=""){
      #-filter specific taxon values for the trait-#  
      specie <- trait %>% filter(Taxon_name==input$varSpecies)  
      if(length(specie[,1]) > 0){
      #-dataframes concatenation-#  
      dfTax<-data.frame(value = specie$Standardized_value, unit = specie$Standardized_unit, condition = specie$Taxon_name, dataset = "taxon")
      dfTrait<-rbind(dfTrait,dfTax)
      dfTrait$dataset <-factor(dfTrait$dataset,levels=c("trait","taxon"))
      #-Plotting parameters-#
      ggplot( dfTrait,aes(x=value, fill=dataset)) +
      geom_histogram(aes(y=..density..), color="darkblue", alpha=0.5, position = 'identity',bins=30) +
      geom_density(alpha=.3) +
      scale_fill_manual(values=c("lightblue", "lightcoral"),labels=c("General",dfTax[1,]$condition)) + 
      labs(x =paste("value (",dfTrait[1,]$unit,")",sep =""),title = paste(trait[1,]$trait_name,"variability",sep=" "),fill="") +
      theme(legend.position = c(0.9,0.9),
            legend.text = element_text(size=11))
      } else {
        ggplot(dfTrait, aes(x = value)) +geom_histogram(color="darkblue", fill="lightblue", bins=30) +labs(x =paste("value (",dfTrait[1,]$unit,")",sep =""),title = paste(dfTrait[1,]$condition,"variability",sep=" "),)
        }
      } else {
        ggplot(dfTrait, aes(x = value)) +geom_histogram(color="darkblue", fill="lightblue", bins=30) +labs(x =paste("value (",dfTrait[1,]$unit,")",sep =""),title = paste(dfTrait[1,]$condition,"variability",sep=" "),)
      }
      }
      })
  #-Update trait variability results upon change of Specie/Traits selected-#
  observeEvent(c(input$varSpecies,input$varTraits),{
      con <- dbConnect(RPostgres::Postgres(), dbname= "croptrait", host=dbHost, port=dbPort, user=dbUser, password=dbPassword)
      query<-paste0('SELECT "Taxon_name","Standardized_value","Standardized_unit","Trait" from "Observation" WHERE "Trait"=',sQuote(input$varTraits))
      trait<-dbGetQuery(con, query)
      dbDisconnect(con)   
    if(input$varSpecies!=""){
      specie <- trait %>% filter(Taxon_name==input$varSpecies)  
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
  #-----------------------#
  ###-Concatenate query-###
  dbManagement<- function(con){
  filterList<-NULL
  allGenus<-NULL
  allSpecie<-NULL
  inputs<-NULL
  inputList<-list(input$dlTaxon,input$dlScale,input$dlTraits,input$dlFunctional_group,input$dlSampling_type)
  inputNameList<-list("Obs.Taxon_name","Observation_levels","Trait","Functio_group","Type")
  for(i in 1:length(inputList)){
    if(!is.null(inputList[i][[1]]) &&  !inputList[i]==""){
      #-Special case for Taxon name
      if(i==1){
      for(j in 1:length(inputList[i][[1]])){
        split=strsplit(inputList[i][[1]][j],"-")
        inputList[i][[1]][j]<-split[[1]][1]
      }  
        inputList[i][[1]]<-sapply(strsplit(inputList[i][[1]],","), function(x) toString(sQuote(x,F)))
        inputs<-paste(inputList[i][[1]],collapse=",")
        filterList<-c(filterList,paste('Obs."Taxon_name" in (',inputs,')',sep=""))
      } else {
        #-Case for the other filters
        inputList[i][[1]]<-sapply(strsplit(inputList[i][[1]],","), function(x) toString(sQuote(x,F)))
        inputs<-paste(inputList[i][[1]],collapse=",")
        filterList<-c(filterList,paste('"',inputNameList[i],'" in (',inputs,')',sep=""))
      }
    }
  }  
    filterList<- paste(filterList, collapse = " and ")
    return(filterList)
  }
  
  #######################
  #-Taxon name spliter-#
  #######################
    NameSpliterRela<-function(list){
        allTax<-NULL
        for(j in 1:length(list)){
        splited<- strsplit(list[j],"-")
        taxon<-splited[[1]][1]
        allTax<-c(allTax,taxon)
        }
        df<-data.frame(allTax)
        return(df)
    } 
  ##########################
  #-Get data main process -#
  ##########################
  observeEvent(input$runQuery,{
    system(paste("mkdir ",session$token,sep = ""))
    shinyjs::hide("queryDl")
    withProgress(message="Browsing database",detail = "Please wait", value = 0,{
      incProgress(1/5)
    con <- dbConnect(RPostgres::Postgres(), dbname= "croptrait", host=dbHost, port=dbPort, user=dbUser, password=dbPassword)
    filterList<-dbManagement(con)
    #-Load fixed SQL syntax
    AllDataQuery <- paste(readLines("CSV/Query2.txt"), collapse=" ")
    if(!filterList==""){
      ifelse(isTRUE(adminMode$isAdmin),
             filteredQuery<-paste(substr(AllDataQuery,1,nchar(AllDataQuery)-1),"WHERE",filterList,sep =" "),
             filteredQuery<-paste(substr(AllDataQuery,1,nchar(AllDataQuery)-1),'WHERE Geninfo."Data_access" in (',sQuote("Public"),",",sQuote("Public (notify the PIs)"),');',sep =" "))
      print(filteredQuery)
    } else {
    #-Case with 0 filters -> get the whole database
      ifelse(isTRUE(adminMode$isAdmin),
             filteredQuery<-paste(substr(AllDataQuery,1,nchar(AllDataQuery)-1),sep =" "),
             filteredQuery<-paste(substr(AllDataQuery,1,nchar(AllDataQuery)-1),'WHERE Geninfo."Data_access" in (',sQuote("Public"),",",sQuote("Public (notify the PIs)"),');',sep =" "))
      }
    incProgress(1/2)
    res <<- dbGetQuery(conn = con,statement = filteredQuery)
    dbDisconnect(con)
    #-Check if we have to inform PI about their data downloaded
    userData<<- res %>% filter(Data_access == "Public (notify the PIs)")
    if(length(userData[,1])>0 & !isTRUE(adminMode$isAdmin)){
      shinyjs::show("userInfos")
    } else {
    write.table(res,paste(session$token,"/queryRes.csv",sep=""),sep = ";",row.names = FALSE)
    uploadData()
    shinyjs::show("queryDl")
    }
    })
  })
  
  #####################################################
  ###############-Admin access to data#################
  observeEvent(input$dataAccessMode,{
    #-Process to toggle the password field
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
  #-Process to switch in/out admin mode
    if(input$adminPswd == credentials$adminpass){
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
  observeEvent(input$submit,{
    if(!input$userMail=="" && isValidEmail(input$userMail)){
      PiToContact<-userData %>% distinct(userData$pi_contact)
    for(i in 1:length(PiToContact[,1])){
      #-Delete unwanted space characters
      nospace<- str_replace_all(PiToContact[i,1]," ","")
      splited<- strsplit(nospace,",")
      for(j in 1:length(splited[[1]])){
        system(paste("Rscript --vanilla mailSender.R",input$userMail,splited[[1]][j],input$projectSummary),wait = FALSE)
      }
    }
    write.table(res,paste(session$token,"/queryRes.csv",sep=""),sep = ";",row.names = FALSE)
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
      #-Give a name to the file
      filename = function() {
        paste("Dataset-", Sys.time(), ".csv", sep="")
      },
      #-Give a location to the file
      content = function(file) {
        file.copy(paste(session$token,"/","queryRes.csv",sep=""),file)
      }
    )
  }
      #-Handle download of the FieldDescription file located in the app
      output$fieldDesc <- downloadHandler(
      filename = "fieldsDescription.xlsx",
      content = function(file) {
        file.copy("fieldsDescription.xlsx",file)
      }
    )
      #-Handle download of the taxon table file located in the app
      output$taxonTable <- downloadHandler(
      filename = "CSV/taxon_Table.csv",
      content = function(file) {
        file.copy("CSV/taxon_Table.csv",file)
      }
    )
      
  #Delete temporary repository
  session$onSessionEnded(function() {
  #deleting the session's folder
  system(paste("rm -Rf ",session$token,sep = ""))
  })    
  }