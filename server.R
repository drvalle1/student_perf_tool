library(shiny)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {

  # setwd('U:\\natercia\\student_perf_tool')
  dat=read.csv('performance data.csv',as.is=T)
  
  #set up colors
  cores=hsv(h=c(0,0.175,0.35),1,1,alpha=0.5)
  
  #how many weeks so far?
  ind=grep('week',colnames(dat))
  nomes=colnames(dat)[ind]
  nomes1=gsub('week','',nomes)
  
  nweek=max(as.numeric(nomes1))
  nomes=paste('week',1:nweek,sep='')
  
  # input=list()
  # input$ufid=6934369
  # input$email='ebecks@ufl.edu'
  # input$passwd='KAJWI'
  
  output$plot1 <- renderPlot({
    #show results for a specific student
    cond=!is.na(dat$SIS.User.ID) & dat$SIS.User.ID==gsub('-','',input$ufid) & 
         !is.na(dat$SIS.Login.ID) & dat$SIS.Login.ID==input$email & 
         !is.na(dat$passwd) & dat$passwd==input$passwd &
         !is.na(dat$trat) & dat$trat==2 #second treatment (students selected for performance analytics)
    ind=which(cond)
    
    #if student not found
    if (sum(cond)==0) {
      plot(NA,NA,xlim=c(0,1),ylim=c(0,1))
      text(0.5,0.5,'User not found',col='red')
    }
    
    #if student found
    if (sum(cond)>0){
      quant=rep(NA,nweek)
      for (i in 1:nweek){
        quant[i]=mean(dat[ind,nomes[i]] >= dat[,nomes[i]],na.rm=T)
      }
      plot(NA,NA,xlim=c(1,nweek),ylim=c(0,1),main='Weekly performance',
           ylab='Proportion that scored the same or lower',
           xlab='Week')
      polygon(x=c(-10,nweek*10,nweek*10,-10),y=c(-10,-10,1/3,1/3),col=cores[1])
      polygon(x=c(-10,nweek*10,nweek*10,-10),y=c(1/3,1/3,2/3,2/3),col=cores[2])
      polygon(x=c(-10,nweek*10,nweek*10,-10),y=c(2/3,2/3,10,10),col=cores[3])
      
      #show trend through time
      lines(1:nweek,quant)
      points(1:nweek,quant,pch=19)
    }
  })
  
  output$plot2 <- renderPlot({
    #show results for a specific student
    cond=!is.na(dat$SIS.User.ID) & dat$SIS.User.ID==gsub('-','',input$ufid) & 
      !is.na(dat$SIS.Login.ID) & dat$SIS.Login.ID==input$email & 
      !is.na(dat$passwd) & dat$passwd==input$passwd &
      !is.na(dat$trat) & dat$trat==2 #second treatment (students selected for performance analytics)
    ind=which(cond)
    
    #if student not found
    if (sum(cond)==0) {
      plot(NA,NA,xlim=c(0,1),ylim=c(0,1))
      text(0.5,0.5,'User not found',col='red')
    }
    
    #if student found
    if (sum(cond)>0){
      #fix NA's to make cumulative sum comparable
      dat1=dat
      ind1=which(is.na(dat[ind,]))
      if (length(ind1)>0) dat1[,ind1]=0
      dat1=t(apply(dat1[,nomes],1,cumsum))
      
      quant=rep(NA,nweek)
      for (i in 1:nweek){
        quant[i]=mean(dat1[ind,nomes[i]] >= dat1[,nomes[i]],na.rm=T)
      }
      
      plot(NA,NA,xlim=c(1,nweek),ylim=c(0,1),main='Cumulative performance',
           ylab='Proportion that scored the same or lower',
           xlab='Week')
      polygon(x=c(-10,nweek*10,nweek*10,-10),y=c(-10,-10,1/3,1/3),col=cores[1])
      polygon(x=c(-10,nweek*10,nweek*10,-10),y=c(1/3,1/3,2/3,2/3),col=cores[2])
      polygon(x=c(-10,nweek*10,nweek*10,-10),y=c(2/3,2/3,10,10),col=cores[3])
      lines(1:nweek,quant)
      points(1:nweek,quant,pch=19)
    }    
  })
})
