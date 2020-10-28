# Load libraries ---------------------------------------------------------------
library(shiny)

library(knitr)

# Data read & manipulation libraries
require(dplyr)
require(lubridate)
require(scales)

library(tidyverse)
library(hrbrthemes)

# Visualization libraries

library (DT)
require(leaflet)
require(plotly)

require(ggiraph)
require(ggiraphExtra)
require(plyr)
require(ggplot2)


source('PredictiveModel.R', local = FALSE)

load("./Files/OneGram.Rdata", verbose=T)
load("./Files/TwoGram.Rdata", verbose=T)
load("./Files/TriGram.2pl.Rdata", verbose=T)
load("./Files/FourGram.2pl.Rdata", verbose=T)




# Server logic -----------------------------------------------------------------
shinyServer(function(input, output, session) {
    
  observeEvent(input$text,{
    
          session$sendCustomMessage(type="focus",message=list(NULL))
      
    })
    
    mytable <- reactive({
        
        
         table<- predWord.4grm(input$text, OneGram, TwoGram, TriGram, FourGram)
         table<-table[complete.cases(table), ]
         table$totalHist <- sapply(table$History, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
         table$`Katz Prob`<-sapply(table$`Katz Prob`, function(x) signif(x,digits=2))
         table$Pred_Hist = paste(table$History, table$Predicted, sep=" ")
         table<-table[- grep("NA", table$History),]
         rownames(table) <- dimnames(table)[[1]]
         table
    })
    
    ngrams<-reactive({
      
       table<-mytable()
      c2<-count(table[table$totalHist==1,])
      c3<-count(table[table$totalHist==2,])
      c4<-count(table[table$totalHist==3,])
      result<-""
      if(any(c2>0)) {result<- "2-grams"}
      if(any(c3>0)){
        if(result=="") result<- "3-grams"
        else result<-paste(result, "3-grams", sep="|") 
      }
      if(any(c4>0)){
        if(result=="") result<- "4-grams"
        else result<-paste(result, "4-grams", sep="|") 
      }
      
      result
      
      
    })
    
    
    
    
    # Plots on Analysis Tab -----------------------------------
    
  
    output$TwoGram<- renderPlotly({
      
      table<-mytable()
      t2<-table[table$totalHist==1,]
      if(nrow(t2)!=0){
      title<-"2-grams"
      
      p <- ggplot(t2[1:20,], aes(`Katz Prob`, Pred_Hist )) + ggtitle(title)+ theme(plot.title = element_text(hjust = 0.5))+
        geom_bar(stat = 'identity',width=0.5, fill="salmon")+
        xlab('Katz Prob')+
        ylab('Words')+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        geom_text( aes (label = paste(`Katz Prob`, "%")) , vjust = + 1.30, hjust = 1.1, size = 2.5)
      p
      } 
    })
    
    output$TriGram<- renderPlotly({
      
     
      
      table<-mytable()  
      t3<-table[table$totalHist==2,]
      if(nrow(t3)!=0){
      title<-"3-Grams"
      
      p <- ggplot(t3[1:20,], aes(`Katz Prob`, Pred_Hist)) + ggtitle(title)+ theme(plot.title = element_text(hjust = 0.5))+
        geom_bar(stat = 'identity',width=0.5, fill="lightblue")+
        xlab('Katz Prob')+
        ylab('Words')+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        geom_text( aes (label = paste(`Katz Prob`, "%") ),angle = 90 , vjust = + 1.30, hjust = 1.1, size = 2.5)
      p
      }
      
    })
    
    
    output$QuadGram<- renderPlotly({
      
      table<-mytable() 
      t4<-table[table$totalHist==3,]
      if(nrow(t4)!=0){
      title<-"4-Grams"
      
      p <- ggplot(t4[1:20,], aes(`Katz Prob`, Pred_Hist)) +  ggtitle(title)+ theme(plot.title = element_text(hjust = 0.5))+
        geom_bar(stat = 'identity',width=0.5, fill="lightgreen")+
        xlab('Katz Prob')+
        ylab('Words')+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        geom_text( aes (label = paste(`Katz Prob`, "%") ,angle = 90 , vjust = + 1.30, hjust = 1.1, size = 2.5))
      p
      }
    })
    
    
    
    
    
    
    # Output Text on Prediction Tab ------------------------------------------------
    
    output$comentInputText <- renderText({ 
      paste0(" Text input entered by user: ")
      
    })
    
    output$input <- renderText({ 
      input$text
    })
    
    output$comentNgrams <- renderText({
      
      paste0(" N-Grams used for predict the next word: " )
    })
    
   
    output$top_five_pred <- renderText({ 
        paste0("Top 5 predicted words ")
    })
    
    output$ngramstext <- renderText({ 
      
      if(input$text!="") ngrams()
      
    })
    
    output$predictionOutputError <- renderText({ 
      
      if(input$text=="") {paste0("Error: Please input at least one word.")}
      
    })
    
    output$predictionOutput<- renderText({ 
        
       if(input$text!="")
      {
       df<-mytable()
        paste(df[1,1],df[2,1], df[3,1], df[4,1], df[5,1], sep = "|")
      }
    })
    
    
    output$TablePredict<- renderText({ 
        "Table: Top 10 predicted words with Katz probability."
    })
    
    # Datatable  Predict -------------------------------------------
    
    output$table <- DT::renderDataTable({ 
     
      
      datatable(mytable()[,1:5],  extensions = 'Responsive')},
                                        rownames = F,
                                        options = list(bFilter = FALSE,
                                                       iDisplayLength = 10),
    )
    
    
  # output$report <- renderUI({
   #     includeMarkdown("documentation.Rmd")
#})
    
})


