library(plyr)
library(dplyr)
library(ggplot2)
library(caret)
library(pROC)


shinyServer(function(input, output, session) {
        Logistic<-reactive({
                Custom<-read.csv("riskcustom.csv")
                Tlogit<-glm(death~.,data=Custom[,-c(15:ncol(Custom))], family = "binomial")
                Tlogit
        })
       #making the data reactive, adding reactives custom rank
         NewScore<-reactive({
                input$update
                Custom<-read.csv("riskcustom.csv")
                Tlogit<-glm(death~.,data=Custom[,-c(15:ncol(Custom))], family = "binomial")
                Custom$logitprob<-round(predict(Tlogit, Custom, type="response"),2)
                isolate({
                Custom$rankscore<-c(input$age.less.4.months*Custom[,1]+input$temp.less.35.5*Custom[,2]+input$no.subjective.fever*Custom[,3]+input$pallor*Custom[,4]+input$jaundice*Custom[,5]+input$difficulty.breathing*Custom[,6]+input$deep.breathing*Custom[,7]+input$convulsions*Custom[,8]+input$unable.to.sit*Custom[,9]+input$unable.to.drink*Custom[,10]+input$altered.consciousness*Custom[,11]+input$unconsciousness*Custom[,12]+input$meningeal.signs*Custom[,13])
                Custom$authorcat<-ifelse(Custom$authorscore<1,"low risk",ifelse(Custom$authorscore>3,"High risk","Mild Risk"))
                Custom$rankcat<-ifelse(Custom$rankscore<=input$low.risk,"low risk",ifelse(Custom$rankscore>input$high.risk,"High risk","Mild Risk"))
                })
                Custom$logitcat<-ifelse(Custom$logitprob<=0.015,"low risk",ifelse(Custom$logitprob>0.1,"High Risk","Mild Risk"))
                
                Custom
        })
         
         
#author summary risk        
                AuthSummaryRisk<-reactive({
                authriskgroup<-NewScore()
                authriskgroup<-group_by(authriskgroup, authorscore)
                authrisksummary<-summarize(authriskgroup, mortality.rate = round(sum(death=="yes")/length(death),2), n=length(death), deaths = sum(death=="yes"))
                authrisksummary$risk.group<- ifelse(authrisksummary$authorscore<1,"low risk",ifelse(authrisksummary$authorscore>3,"High risk","Mild Risk"))
                authrisksummary$authorscore<-factor(authrisksummary$authorscore, labels = authrisksummary$authorscore)
                authrisksummary
               
       })
                AuthorRate<-reactive({subset(AuthSummaryRisk(), authorscore==as.numeric(AuthorScore()))})    
                
 #plotting the authors summary risk       
        AuthRiskPlot<-reactive({
                ggplot(data=AuthSummaryRisk(), aes(x= authorscore,y=mortality.rate*100))+ylab("Mortality Rate in %")+xlab("Authors Risk Score")+geom_bar(aes(fill=risk.group), stat="identity")+geom_bar(data=AuthorRate(), stat="identity", fill="yellow")
})
#making custom summary risk
                CustSummaryRisk<-reactive({
                        customriskgroup<-NewScore()
                        customriskgroup<-group_by(customriskgroup, rankscore)
                        customrisksummary<-summarize(customriskgroup, mortality.rate = round(sum(death=="yes")/length(death),2), n=length(death), deaths = sum(death=="yes"))
                        customrisksummary$risk.group<- ifelse(customrisksummary$rankscore<=input$low.risk,"low risk",ifelse(customrisksummary$rankscore>input$high.risk,"High risk","Mild Risk"))
                        customrisksummary$rankscore<-factor(customrisksummary$rankscore, labels = customrisksummary$rankscore)
                        customrisksummary
                        
                })
                CustomRate<-reactive({subset(CustSummaryRisk(), rankscore==as.numeric(CustomScore()))})   
                
#plotting the Custom summary risk       
               CustRiskPlot<-reactive({
                        ggplot(data=CustSummaryRisk(), aes(x= rankscore,y=mortality.rate*100))+ylab("Mortality Rate in %")+xlab("Custom Risk Score")+geom_bar(aes(fill=risk.group), stat="identity")+geom_bar(data=CustomRate(), stat="identity", fill="yellow")
                        
               })
#making logit summary
               LogitSummaryRisk<-reactive({
                       logitriskgroup<-NewScore()
                       logitriskgroup<-group_by(logitriskgroup, logitprob)
                       logitrisksummary<-summarize(logitriskgroup, mortality.rate = round(sum(death=="yes")/length(death),2), n=length(death), deaths = sum(death=="yes"))
                       logitrisksummary$risk.group<- ifelse(logitrisksummary$logitprob<=0.015,"low risk",ifelse(logitrisksummary$logitprob>0.1,"High Risk","Mild Risk"))
                       logitrisksummary$logitprob<-factor(logitrisksummary$logitprob, labels = logitrisksummary$logitprob)
                       logitrisksummary
                       
               })
               LogitRate<-reactive({subset(LogitSummaryRisk(), logitprob==round(LogitProbDying(),2))})
               
#making logit plot summary
               LogitRiskPlot<-reactive({
                       ggplot(data=LogitSummaryRisk(), aes(x= logitprob,y=mortality.rate*100))+ylab("Mortality Rate in %")+xlab("Logit Probability")+geom_bar(aes(fill=risk.group), stat="identity")+geom_bar(data=LogitRate(), stat="identity", fill="yellow")
                       
               })
               
#making switch summary risk
               SelectedSummary<-reactive({
                       switch(input$predictor,
                              "Author Score"=AuthSummaryRisk(),
                              "Custom Score"=CustSummaryRisk(),
                              "Logit"=LogitSummaryRisk())})
               SelectedRiskPlot<-reactive({
                       switch(input$predictor,
                              "Author Score"=AuthRiskPlot(),
                              "Custom Score"=CustRiskPlot(),
                              "Logit"=LogitRiskPlot())})
      
        
        Plot1<-reactive({roc(NewScore()$death, NewScore()$authorscore,main="Authors ROC" , plot=TRUE, col="dark green",print.auc=TRUE,print.auc.pattern="Author's Score AUC: \n%.1f%%", 
                                            print.auc.y=40,print.auc.x=40,print.auc.col="dark green",legacy.axes=FALSE,percent=TRUE,  print.thres="best", print.thres.col="dark green",
                                            print.thres.adj=c(-0.2,0.1))})
                       
        Plot2<-reactive({roc(NewScore()$death, NewScore()$rankscore,main="Custom ROC" ,plot=TRUE, col="dark red",print.auc=TRUE,print.auc.pattern="Custom AUC: \n%.1f%%", 
                                          print.auc.y=10,print.auc.x=40,print.auc.col="dark red",legacy.axes=FALSE,percent=TRUE, print.thres="best", print.thres.col="dark red")})
                       
        Plot3<-reactive({roc(NewScore()$death,NewScore()$logitprob, plot=TRUE, col="blue", percent=TRUE, print.thres="best", print.auc=TRUE,
                                                 print.auc.pattern="Logit AUC: \n%.1f%%", print.auc.col="blue",print.auc.y=25,print.auc.x=40, main= "logit ROC",print.thres.col="blue")})
                       
        Plot4<-reactive({roc(NewScore()$death,NewScore()$logitprob, plot=TRUE, col="blue", percent=TRUE, print.thres=c(0.015,0.1), print.auc=TRUE,
                                  print.auc.pattern="Logit AUC: \n%.1f%%", print.auc.col="blue",print.auc.y=25,print.auc.x=40, main= "logit vs score ROC",print.thres.col="blue")
                               roc(NewScore()$death, NewScore()$rankscore,main="ROC" ,plot=TRUE, col="dark red",print.auc=TRUE,print.auc.pattern="Custom Score AUC: \n%.1f%%", 
                                   print.auc.y=10,print.auc.x=40,print.auc.col="dark red",add=TRUE,legacy.axes=FALSE,percent=TRUE, print.thres=c(input$low.risk,input$high.risk), print.thres.col="dark red")
                               roc(NewScore()$death, NewScore()$authorscore,main="ROC" , plot=TRUE, col="dark green",print.auc=TRUE,print.auc.pattern="Author's Score AUC: \n%.1f%%", 
                                   print.auc.y=40,print.auc.x=40,print.auc.col="dark green",legacy.axes=FALSE,percent=TRUE, add=TRUE, print.thres=c(1,4), print.thres.col="dark green",
                                   print.thres.adj=c(-0.2,0.1))
                               legend("topleft", legend=c("Logit", "Custom Score", "Score"), col=c("blue", "dark red","dark green"), bty="n",lwd=2)})
         
        
        
              
        
        CustomScore<-reactive({c(input$age.less.4.months*input$vars1+input$temp.less.35.5*input$vars2+input$no.subjective.fever*input$vars3+input$pallor*input$vars4+
                                 input$jaundice*input$vars5+input$difficulty.breathing*input$vars6+input$deep.breathing*input$vars7+input$convulsions*input$vars8+input$unable.to.sit*input$vars9+
                                 input$unable.to.drink*input$vars10+input$altered.consciousness*input$vars11+input$unconsciousness*input$vars12+input$meningeal.signs*input$vars13)})
        
        AuthorScore<-reactive({c(2*input$vars1+input$vars2+input$vars3+input$vars4+input$vars5+input$vars6+input$vars7+input$vars8+input$vars9+input$vars10+input$vars11+input$vars12+input$vars13)})
        
        PatientCustomRisk<-reactive({ifelse(CustomScore()<=input$low.risk,"low risk",
                                      ifelse(CustomScore()>input$high.risk,"High risk","Mild Risk"))})
        
        PatientAuthorRisk<-reactive({ifelse(AuthorScore()<1,"low risk",
                                            ifelse(AuthorScore()>3,"High risk","Mild Risk"))})
        
        LogitProbDying<-reactive({
                selected<-data.frame(as.numeric(input$vars1),as.numeric(input$vars2),as.numeric(input$vars3),as.numeric(input$vars4),as.numeric(input$vars5),as.numeric(input$vars6),as.numeric(input$vars7),as.numeric(input$vars8),as.numeric(input$vars9),as.numeric(input$vars10),as.numeric(input$vars11),as.numeric(input$vars12),as.numeric(input$vars13))
                names(selected)<-names(NewScore()[,1:13])
                predict(Logistic(), selected, type="response")
        })
        
        LogitCat<-reactive({ifelse(LogitProbDying()<=0.015,"low risk",ifelse(LogitProbDying()>0.1,"High Risk","Mild Risk"))
                
                
        })
        
        RiskDying<-reactive({
                data.frame("Predictor"=c("Custom Score","Author Score","Logit"),
                           "Score or Prob."=c(round(CustomScore(),0),round(AuthorScore(),0),round(LogitProbDying(),2)),
                           "Risk Group"=c(PatientCustomRisk(),PatientAuthorRisk(),LogitCat()),
                           "Death Risk"=c(ifelse(is.numeric(CustomRate()$mortality.rate),CustomRate()$mortality.rate,NA),
                                          ifelse(is.numeric(AuthorRate()$mortality.rate),AuthorRate()$mortality.rate,NA),
                                          ifelse(is.numeric(LogitRate()$mortality.rate),LogitRate()$mortality.rate,NA)))
        })
        
        output$table<-renderDataTable({
                RiskDying()
                        
        })
        

        output$value<-renderPrint({
                paste("your patient Custom's Score is:", CustomScore(), ". Following this, he is in the",
                       PatientCustomRisk(), "group, and his risk of dying is:",CustomRate()$mortality.rate)
                })
            
        output$value2<-renderPrint({
                paste("your patient Authors' Score is:", AuthorScore(), ". Following this, he is in the",
                       PatientAuthorRisk(), "group, and his risk of dying is:",AuthorRate()$mortality.rate)
                })
        output$value3<-renderPrint({
                paste("your patient Logit Probability of Dying is:", format(LogitProbDying(),digits= 2), ". Following this, he is in the",
                      LogitCat(), "group, and his risk of dying is:", LogitRate()$mortality.rate )
        })

                

        output$plot1 <- renderPlot({
                Plot1()
        })
        output$plot2 <- renderPlot({
                Plot2()
        })
        output$plot3 <- renderPlot({
                Plot3()
        })
        output$plot4 <- renderPlot({
                Plot4()
        })
        
        
        #panel 2
        output$RiskSummary<-renderDataTable({
                SelectedSummary()
        })
        
        output$plot6<-renderPlot({
                SelectedRiskPlot()
        })
       
        
        
        
        
        
        
 

})