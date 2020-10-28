###*****************************
# save files for shiny app ####
###*****************************

load("./CondP_and_GoodTuring/df1GTfreq.Rdata", verbose=T)
load("./CondP_and_GoodTuring/df2GTfreq.Rdata", verbose=T)
load("./CondP_and_GoodTuring/df3.2plGTfreq.Rdata", verbose=T)
load("./CondP_and_GoodTuring/df4.2plGTfreq.Rdata", verbose=T)

# for the 1gram we do not have history, so we create a history column populated by "NA"
OneGram <- cbind(rep("NA",times=dim(df1)[1]),df1[,c("pred", "GTfreq", "GTprob" )])
names(OneGram) <- c("hist", "pred", "GTfreq", "GTprob" )
OneGram.2pl<-OneGram[which(OneGram$GTfreq>1),]



TwoGram <- df2[,c("hist", "pred", "GTfreq", "GTprob" )]

TwoGram <- TwoGram[!grepl('^[b-hg-z]{1}$', TwoGram$hist),]
TwoGram.2pl<-TwoGram[which(TwoGram$GTfreq>1),]


TriGram <- df3.2pl[,c("hist", "pred", "GTfreq", "GTprob" )]
TriGram <- TriGram[!grepl('^[b-hg-z]{1}$', TriGram$hist),]

FourGram <- df4.2pl[,c("hist", "pred", "GTfreq", "GTprob" )]
FourGram <- FourGram[!grepl('^[b-hg-z]{1}$', FourGram$hist),]

save(OneGram, file = "PredictiveWordShinyApp/Files//OneGram.Rdata")
OneGram <- OneGram.2pl
save(OneGram, file = "PredictiveWordShinyApp/Files/OneGram.2pl.Rdata")
save(TwoGram, file = "PredictiveWordShinyApp/Files/TwoGram.Rdata")
TwoGram <- TwoGram.2pl
save(TwoGram, file = "PredictiveWordShinyApp/Files//TwoGram.2pl.Rdata")
save(TriGram, file = "PredictiveWordShinyApp/Files//TriGram.2pl.Rdata")
save(FourGram, file = "PredictiveWordShinyApp/Files//FourGram.2pl.Rdata")