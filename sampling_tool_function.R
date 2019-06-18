require(plyr)
require(dplyr)
require(car)
require(reshape2)
require(stringr)
library(shiny)
library(shinyjs)


Ssize<-function (x,A,p,E) {
  (qchisq(A,1)*x*p*(1-p)) / (E^2*(x-1)+qchisq(A,df=1)*p*(1-p))
  }
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

sampling.frame <- function(cens, samp_type=NULL, stratified=FALSE, strata=NULL, psu=NULL, pop=NULL) {
  
  cens$id_sampl<-paste0("id_",row.names(cens))
  if(stratified==TRUE){
    names(cens)<-gsub(strata,"strata",names(cens))
  } else {
    cens$strata<-rep("all",nrow(cens))
  }
  
  if(samp_type=="Cluster sampling"){
    names(cens)<-gsub(psu,"psu",names(cens))
    names(cens)<-gsub(pop,"pop",names(cens))
  }else if(samp_type=="2 stages random - st1"){
    cens$psu<-cens$id_sampl
    names(cens)<-gsub(pop,"pop",names(cens))
  }else{
    cens$psu<-cens$id_sampl
    cens$pop<-rep(1,nrow(cens))
  }
  
  
  sumdist<-ddply(cens, .(strata), summarise, SumDist = sum(pop,na.rm=T))
  cens<-merge(cens,sumdist,by.x="strata",by.y="strata")
  proba<-as.numeric(cens$pop)/as.numeric(cens$SumDist)
  cens<-cbind(cens,proba)
  if(samp_type=="Cluster sampling"){
    cens$psu<-factor(cens$psu)
  }
  cens<-cens[!is.na(cens$proba),]
  cens
}

target.frame <- function(frame, c_lev, proport, error_marg){
  
  c_lev <<- c_lev
  proport <<- proport
  error_marg <<- error_marg
  sam<- ddply(frame,"strata", summarise, 
              Population = max(SumDist,na.rm=T),
              target = ceiling(Ssize(Population, c_lev, proport, error_marg ))
  )		
  as.data.frame(sam)
}


#' generate sample sizes
#' description
#' @param cens data.frame with
#' @param 
#' @details 
#' @return 
#' @examples 
sampling.tool <- function(cens=NULL, sam=NULL, samp_type, stratified=FALSE, cls=NULL, ICC=NULL, buf,
                          conf_level, e_marg) {
  
  rd<-"no"
  sw_rand<-NA
  if(samp_type=="Cluster sampling"){
      sw_rand<-NA
      for (i in 1:nrow(sam)){
        rd<-"no"
        dist<-as.character(sam$strata[i])
        dbr<-cens[as.character(cens$strata)==dist,]
        out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(sam$target[i])/cls),prob=dbr$proba,replace=TRUE)
        if(nrow(dbr) < (sam$target[i] / cls)){
          rd<-"yes"
          sprintf("Not enough clusters: Random sampling assumption")
          sw_rand<-c(sw_rand,dist)
          
          out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(sam$target[i])),prob=dbr$proba,replace=TRUE)
          
        } else {
          repeat{
            d<-as.data.frame(table(out))[,2] # counting amount of clusters sampled
            ms<-sum(d)/nrow(as.data.frame(d)) # clusters / location
            DESS<-1+(ms*cls-1)*ICC # DESS
            targ<-DESS*sam$target[i]/cls	# target clusters	
            
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
        print(sprintf("%.0f%%, Sampling %s",i/nrow(sam)*100, dist))
      }
    
  } else if (samp_type=="Simple random"){
    withProgress(message = 'Select PSUs randomly', detail = "Sampling 0", value = 0, {
      for (i in 1:nrow(sam)){
        dist<-as.character(sam$strata[i])
        dbr<-cens[as.character(cens$strata)==dist,]
        out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(sam$target[i])*(1+buf)),replace=FALSE)
        if (i==1){output<-out} else {output<-c(output,out)}
        print(sprintf("%.0f%%, Sampling %s",i/nrow(sam)*100, dist))
      }
    })
  } else if (samp_type=="2 stages random - st1"){
    withProgress(message = 'Select PSUs randomly', detail = "Sampling 0", value = 0, {
      for (i in 1:nrow(sam)){
        dist<-as.character(sam$strata[i])
        dbr<-cens[as.character(cens$strata)==dist,]
        out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(sam$target[i])),prob=dbr$proba,replace=TRUE)
        if (i==1){output<-out} else {output<-c(output,out)}
        print(sprintf("%.0f%%, Sampling %s",i/nrow(sam)*100, dist))
      }
    })
  }
  
  output<-as.data.frame(table(output))
  
  dbout<-merge(output,cens,by.x="output",by.y="id_sampl",all.x=T,all.y=F)
  
  if(samp_type=="Cluster sampling"){  
    dbout$Freq<-ifelse(dbout$strata%in%sw_rand,dbout$Freq,dbout$Freq*cls)
  }
  
  names(dbout)<-recode(names(dbout),"'output'='id_sampl';'Freq'='Survey'")
  
  if(samp_type!="Simple random"){  
    dbout$survey_buffer<-ceiling(dbout$Survey*(1+buf))
  } else {
    dbout$survey_buffer<-dbout$Survey
  }
  
  input <<- list(ICC=ICC, conf_level=conf_level, e_marg=e_marg, samp_type=samp_type)
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
  
  
  
  if(samp_type=="Cluster sampling"){
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
  
  
}