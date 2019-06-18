function(input, output, session) {
    
   db <- reactive({
		
		inFile <- input$popdata
		
		if(input$testdata==TRUE){
			read.csv("data_example.csv")
		}else if (is.null(inFile)){
			return(NULL)
		}else{
			read.csv(inFile$datapath)
		}
	})
	  
    
  observe({
	updateSelectInput(session, "col_psu", choices = c("None",colnames(db())),selected="None")
	updateSelectInput(session, "strata", choices = c("None",colnames(db())),selected="None")
	updateSelectInput(session, "colpop", choices = c("None",colnames(db())),selected="None")
	})
  

  
   frame<-eventReactive(input$f_apply,{
				
		cens<-db()
		cens$id_sampl<-paste0("id_",row.names(cens))
		if(input$stratified==TRUE){
			strata<-as.character(input$strata)
			names(cens)<-gsub(strata,"strata",names(cens))
		} else {
			cens$strata<-rep("all",nrow(cens))
		}
		
		if(input$samp_type=="Cluster sampling"){
			psu<-as.character(input$col_psu)
			names(cens)<-gsub(psu,"psu",names(cens))
			pop<-as.character(input$colpop)
			names(cens)<-gsub(pop,"pop",names(cens))
		}else if(input$samp_type=="2 stages random - st1"){
			cens$psu<-cens$id_sampl
			pop<-as.character(input$colpop)
			names(cens)<-gsub(pop,"pop",names(cens))
		}else{
			cens$psu<-cens$id_sampl
			cens$pop<-rep(1,nrow(cens))
		}
		
	    
		  sumdist<-ddply(cens, .(strata), summarise, SumDist = sum(pop,na.rm=T))
			print(sumdist)
		  cens<-merge(cens,sumdist,by.x="strata",by.y="strata")
		  proba<-as.numeric(cens$pop)/as.numeric(cens$SumDist)
		  cens<-cbind(cens,proba)
		  if(input$samp_type=="Cluster sampling"){
			cens$psu<-factor(cens$psu)
		  }
		  cens<-cens[!is.na(cens$proba),]
		  cens
	})
	
	output$sampling_frame <- DT::renderDataTable(
		  frame(),
		  rownames = FALSE,
		  options = list(searching = FALSE, lengthChange = FALSE)
	)
	
	cible<-eventReactive(input$f_apply,{
	
	c_lev <<- input$conf_level
	proport <<- input$pror
	error_marg<<- input$e_marg 
		
		sam<- ddply(frame(),"strata", summarise, 
			Population = max(SumDist,na.rm=T),
			target = ceiling(Ssize(Population, c_lev,proport,error_marg ))
		)		
		as.data.frame(sam)
	})
	
	output$sampling_frame <- DT::renderDataTable(
		  frame(),
		  rownames = FALSE,
		  options = list(searching = FALSE, lengthChange = T)
	)
		
	output$target_frame <- DT::renderDataTable(
		  cible(),
		  rownames = FALSE,
		  options = list(searching = FALSE, lengthChange = F)
	)
	
	out<-eventReactive(input$desButton,{

		cens<-frame()
		sam<-cible()
		rd<-"no"
		sw_rand<-NA
		if(input$samp_type=="Cluster sampling"){
			withProgress(message = 'Select PSUs randomly', style="notification", detail = "Sampling 0", value = 0, {
				Sys.sleep(0.25)
				sw_rand<-NA
				for (i in 1:nrow(sam)){
					rd<-"no"
					dist<-as.character(sam$strata[i])
					dbr<-cens[as.character(cens$strata)==dist,]
					out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(sam$target[i])/input$cls),prob=dbr$proba,replace=TRUE)
					if(nrow(dbr) < (sam$target[i] / input$cls)){
					  rd<-"yes"
					  showModal(modalDialog(
					    title = paste(dist,": Not enough clusters"),
					    "Random sampling assumption",
					    easyClose = TRUE,
					    footer = NULL
					  ))
					  sw_rand<-c(sw_rand,dist)
					  
					  out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(sam$target[i])),prob=dbr$proba,replace=TRUE)
					  
					} else {
					repeat{
						d<-as.data.frame(table(out))[,2] # counting amount of clusters sampled
						ms<-sum(d)/nrow(as.data.frame(d)) # clusters / location
						DESS<-1+(ms*input$cls-1)*input$ICC # DESS
						targ<-DESS*sam$target[i]/input$cls	# target clusters	
						
						if(sum(d)>=targ){ # check if current cluster amount is enough (higher or equal to target clusters)
							break
						}
						out<-c(out,sample(as.character(dbr$id_sampl),1,prob=dbr$proba,replace=TRUE)) # adding 1 new (cluster) sample
						rd_check<-all(unique(dbr$id_sampl)%in%unique(out))	
						if(rd_check){
							 rd<-"yes"
							  showModal(modalDialog(
									title = paste(dist,": All PSUs have been selected"),
									"Random sampling assumption",
									easyClose = TRUE,
									footer = NULL
							  ))
							  sw_rand<-c(sw_rand,dist)
							  
							 out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(sam$target[i])),prob=dbr$proba,replace=TRUE)
							 break
						}
					}
					}
					if (i==1){output<-out} else {output<-c(output,out)}
				incProgress(round(1/nrow(sam),2), detail = paste("Sampling", dist))
				}
			Sys.sleep(1)	
			})
		
	} else if (input$samp_type=="Simple random"){
		withProgress(message = 'Select PSUs randomly', detail = "Sampling 0", value = 0, {
			Sys.sleep(0.25)
			for (i in 1:nrow(sam)){
				dist<-as.character(sam$strata[i])
				dbr<-cens[as.character(cens$strata)==dist,]
				out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(sam$target[i])*(1+input$buf)),replace=FALSE)
				if (i==1){output<-out} else {output<-c(output,out)}
				incProgress(round(1/nrow(sam),2), detail = paste("Sampling", dist))
			}
			Sys.sleep(1)
		})
	} else if (input$samp_type=="2 stages random - st1"){
		withProgress(message = 'Select PSUs randomly', detail = "Sampling 0", value = 0, {
			Sys.sleep(0.25)
			for (i in 1:nrow(sam)){
				dist<-as.character(sam$strata[i])
				dbr<-cens[as.character(cens$strata)==dist,]
				out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(sam$target[i])),prob=dbr$proba,replace=TRUE)
				if (i==1){output<-out} else {output<-c(output,out)}
				incProgress(round(1/nrow(sam),2), detail = paste("Sampling", dist))
			}
			Sys.sleep(1)
		})
	}
	
	output<-as.data.frame(table(output))
	
	dbout<-merge(output,cens,by.x="output",by.y="id_sampl",all.x=T,all.y=F)
	 
	if(input$samp_type=="Cluster sampling"){  
		dbout$Freq<-ifelse(dbout$strata%in%sw_rand,dbout$Freq,dbout$Freq*input$cls)
	}
	
	names(dbout)<-recode(names(dbout),"'output'='id_sampl';'Freq'='Survey'")
	
	if(input$samp_type!="Simple random"){  
		dbout$survey_buffer<-ceiling(dbout$Survey*(1+input$buf))
	} else {
		dbout$survey_buffer<-dbout$Survey
	}
	
	
		summary_sample<- ddply(dbout,"strata", summarise, 
			Surveys = sum(Survey,na.rm=T), 
			PSUs = length(strata), 
			Cluster_size = round(Surveys/PSUs,2),
			ICC = input$ICC,
			DESS = 1+(Cluster_size-1)*input$ICC,
			Effective_sample = round(Surveys / DESS,0),
			Surveys_buffer = sum(survey_buffer,na.rm=T),
			Confidence_level = input$conf_level,
			Error_margin = input$e_marg,
			NB_Population = max(SumDist,na.rm=T),
			Sampling_type = input$samp_type
		)
	
	

	if(input$samp_type=="Cluster sampling"){
			for(i in 1:nrow(summary_sample)){
				if(summary_sample$strata[i]%in%sw_rand){
					summary_sample$Cluster_size[i]<-NA
					summary_sample$ICC[i]<-NA
					summary_sample$DESS[i]<-NA
					summary_sample$Effective_sample[i]<-NA
					summary_sample$Sampling_type[i]<-"2 stages random - st1"
				}
			}
		}
		
	if(input$samp_type!="Cluster sampling"){
			le<-nrow(summary_sample)
			summary_sample$Cluster_size<-rep(NA,le)
			summary_sample$ICC<-rep(NA,le)
			summary_sample$DESS<-rep(NA,le)
			summary_sample$Effective_sample<-rep(NA,le)
	}
	
	tab<-paste0("Sampling_output",gsub(":","_",Sys.time()),".xlsx")
	
	names(summary_sample)<-c("Stratification","# surveys", "# units to assess","CLuster size","ICC","DESS","Effective sample","# surveys (buffer)","Confidence level","Error margin","Population","Sampling type")
	
	list(dbout,summary_sample,sw_rand)

	 
   })
  	
	output$sampling_output <- DT::renderDataTable(
		  out()[[1]],
		  rownames = FALSE,
		  options = list(searching = FALSE)
	)
	
	output$test_1 <- DT::renderDataTable(
		  out()[[2]],
		  rownames = FALSE,
		  options = list(searching = FALSE, lengthChange = FALSE)
	)
		
	observeEvent(input$refresh, {
		output$test_1 <- DT::renderDataTable(
			out()[[2]],
			rownames = FALSE,
			options = list(searching = FALSE, lengthChange = FALSE)
		)
	})
	
	
	observeEvent(input$refresh, {
		output$sampling_output <- DT::renderDataTable(
			out()[[1]],
			rownames = FALSE,
			options = list(searching = FALSE, lengthChange = FALSE)
		)
	})
	
		
			
	output$downloadBtn <- downloadHandler(
		filename = function() { paste("sampling_frame",humanTime(), '.csv', sep='') },
		content = function(file) {
		  write.csv(out()[[1]], file)
		}
	)					
	output$downloadBtn2 <- downloadHandler(
		filename = function() { paste("sampling_summary",humanTime(), '.csv', sep='') },
		content = function(file) {
		  write.csv(out()[[2]], file)
		}
	)		
	

	
	
}

