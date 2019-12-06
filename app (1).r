library(shiny)
library(shinydashboard,warn.conflicts = FALSE)
library(shinythemes,warn.conflicts = FALSE)
library(readxl)
library(ggplot2,warn.conflicts = FALSE)
library(dplyr,warn.conflicts = FALSE)
library(plotly,warn.conflicts = FALSE)
library(corrplot,warn.conflicts = FALSE)
library (Deducer,warn.conflicts = FALSE)
library(caret,warn.conflicts = FALSE)
library(ROCR,warn.conflicts = FALSE)
library(pROC,warn.conflicts = FALSE)
library(rpart,warn.conflicts = FALSE)
library(rpart.plot,warn.conflicts = FALSE)
library(randomForest,warn.conflicts = FALSE)
library(nnet,warn.conflicts = FALSE)
library(NeuralNetTools,warn.conflicts = FALSE)
library(e1071,warn.conflicts = FALSE)
library(rsconnect)



## importation de la base
library(readxl,warn.conflicts = FALSE)
data1 <- read_excel("C:/Users/User/Desktop/projet econometrie/couverture-dassurance.xlsx")


#train and test
tr=sample(1000,750)
data2_train=data1[tr,]
data2_test=data1[-tr,]
#modele logistique
LogisticModel <- glm( y~.,family = binomial(link = logit),data = data2_train)
summary(LogisticModel)

fitLog <- predict(LogisticModel,type="response",data2_test)
predlogit = prediction( fitLog, data2_test$y)
perflogit <- performance(predlogit, "tpr", "fpr")
#auc
AUCLog2=performance(predlogit, measure = "auc")@y.values[[1]]


ArbreModel<- rpart(y ~ .,data = data2_train)

fitArbre <- predict(ArbreModel,newdata=data2_test)
predarbre = prediction(fitArbre, data2_test$y)
perfarbre <- performance(predarbre, "tpr", "fpr")

AUCArbre=performance(predarbre, measure = "auc")@y.values[[1]]
## reseau de neurones
Neural<- nnet(y~ .,data = data2_train,size=20,maxit=10000,decay=.001, linout=F, trace = F)
fitNeural <- predict(Neural,
                     newdata=data2_test)
prednn = prediction( fitNeural, data2_test$y)
perfnn <- performance(prednn, "tpr", "fpr")


AUCnn=performance(prednn, measure = "auc")@y.values[[1]]
models <- c('Logistic regression','Arbre de decision','reseaux de neurone')
models_AUC <- c(AUCLog2, AUCArbre, AUCnn)
data1=data1 %>% mutate_if(is.character, as.factor)


## code de l'app shiny
a<-c("men","urban","private","marital","y")
b<-c("age","seniority")
shinyApp(
  ui = tagList(
    navbarPage(theme = shinytheme("journal"),
               "Projet Machine Learning ",
               navbarMenu ("Introduction",tabPanel("Introduction du Projet",h2("Ce projet consiste à : "),h4("1)Réaliser une analyse
                          uni-variée de toutes les variables de la base de données de crédit en Allemagne"),h4(" 2) Réaliser une analyse bi-variée ente 
                          la variable cible (GOOD) et toutes les variables significatives"),h4("3) Construire un modèle prédictif d'une base d'apprentissage (train) 
                          en mettant en compétions plusieurs méthodes comme :"),h5("*)Régression logistique "),h5("*)Réseau de Neurones "),h5("*)Arbre de Decision"),h4("4) Fournir des indicateurs de performances des prévisions réalisées sur la base test"),
                          h1("Projet réalisé par Triki Sadok"))),
               
               navbarMenu("Analyse descriptive univariee",
                          tabPanel("Variables Qualitatives",
                                   sidebarPanel(
                                     selectInput("quali","Choisir une variable Qualitative",choices=a)
                                     
                                   ),
                                   mainPanel(
                                     plotOutput("plot1")
                                     
                                     
                                   )
                          ),
                          tabPanel("Variables Quantitatives", 
                                   sidebarPanel(
                                     selectInput("quanti","Choisir une variable Quantitative",
                                                 choices=b)
                                   ),
                                   mainPanel(
                                     plotOutput("plot2"),
                                     h4("statistiques descriptives"),
                                     verbatimTextOutput("text6")
                                   )
                          )),
               
               navbarMenu("Analyse descriptive bivariee",
                          tabPanel("Variables quantitatives",
                                   sidebarPanel(
                                     selectInput("bi1","Choisir une variable quantitative",
                                                 choices =b )
                                   ),
                                   mainPanel(
                                     plotOutput("boxplot1"),
                                     h5("t.test"),
                                     tableOutput("test1")
                                   )
                                   
                          ),
                          tabPanel("Variables Qualitatives",
                                   sidebarPanel(
                                     selectInput("bi2","Choisir une variable Qualitative",
                                                 choices =a)
                                   ),
                                   mainPanel(
                                     plotOutput("plot3"),
                                     h5("fisher.test"),
                                     tableOutput("test2"),
                                     h6("khi2.test",col="red"),
                                     tableOutput("test3")
                                     
                                   )
                          ),
                          
                          tabPanel("Matrice de correlation",
                                   mainPanel(
                                     box(title = "Matrice de correlation",width = 700,height = 700,status = "primary", solidHeader = TRUE,background = "light-blue",plotOutput("cor",width = 700,height = 700)))
                          )
                          
               ),
               navbarMenu("Machine learning",
                          tabPanel("Training data",
                                   mainPanel(
                                     plotOutput("boxplot5"),
                                     h5("indice de perfermance"),
                                     textOutput("auc1")
                                     
                                   )),
                          tabPanel("Arbre de Decision",
                                   mainPanel(
                                     plotOutput("plot6"),
                                     plotOutput("plot7"),
                                     h6("indice de perfermance"),
                                     textOutput("auc2")
                                   )),
                          tabPanel("Reseau de Neurone",
                                   mainPanel(
                                     plotOutput("plot15"),
                                     h5("indice de perfermance"),
                                     textOutput("auc4")
                                   )),
                          tabPanel("Comparaison des modèles",
                                   mainPanel(
                                     plotOutput("plot9"),
                                     tableOutput("table")
                                     
                                   ))
               ))
    
    
  ),
  
  
  
  
  
  
  
  
  
  
  server = function(input, output) {
    
    output$plot1<-renderPlot({
      x10 = data1[,input$quali]
      pie(table(x10),col = rainbow(11))
      title(names(x10))
    })
    
    output$text6=renderPrint({
      
      
      
      summary(data1[,input$quanti])
      
      
    })
    
    output$plot2<-renderPlot({
      x = data1[,input$quanti]
      boxplot(x,
              main = names(x),
              ylab = names(x),
              col = "lightcyan4",
              border = "black")
    })
    
    output$statdes=renderText({summary(data1[,input$quanti])})
    
    output$boxplot1<-renderPlot({
      x1 = unlist(data1[,input$bi1])
      boxplot(formula=x1~data1$y,
              col="red",
              border="blue")
      
    })
    
    output$test1<-renderTable({
      x1 <- unlist(data1[,input$bi1])
      unlist(t.test(x1,data1$y))
    } ,rownames = TRUE)
    
    
    
    
    output$plot3=renderPlot({
      x2 = unlist(data1[,input$bi2])
      mosaicplot(table(x2,data1$y), shade = TRUE,color = TRUE,ylab = "y" )
      
    })
    
    
    output$test2<-renderTable({
      x2 <- unlist(data1[,input$bi2])
      unlist(fisher.test(x2,data1$y))
    } ,rownames = TRUE)
    
    output$test3<-renderTable({
      x2 <- unlist(data1[,input$bi2])
      unlist(chisq.test(x2,data1$y))
    } ,rownames = TRUE)
    
    
    output$cor<-renderPlot({cor1<-cor(data.matrix(data1, rownames.force = NA))
    corrplot(cor1, type="upper", order="hclust", tl.col="black", tl.srt=45)})
    
    
    output$boxplot5<-renderPlot({
      plot(perflogit, lwd=2, colorize=TRUE, main="ROC : Logistic Regression Performance")
      lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
      lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)
      
      
    })
    output$auc1=renderText({paste("AUC: ", AUCLog2 )})
    
    output$plot6<-renderPlot({prp(ArbreModel,type=2,extra=1)
      
    })
    
    
    output$plot7<-renderPlot({
      plot(perfarbre, lwd=2, colorize=TRUE, main="ROC : Arbre de decision Performance")
      lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
      lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)
    })
    output$auc2=renderText({paste("AUC: ",AUCArbre)})
    
    
    output$plot8<-renderPlot({plot(perfrandomforest, lwd=2, colorize=TRUE, main="ROC : Random Forest Performance")
      lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
      lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)})
    
    output$auc3<-renderText({paste("AUC: ",AUCRF)})
    
    output$plot15<-renderPlot({plot(perfnn, lwd=2, colorize=TRUE, main="ROC : Neural Network Performance")
      lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
      lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)
      
    })
    
    output$auc4<-renderText({paste("AUC: ",AUCnn)})
    output$plot9<-renderPlot({
      plot(perflogit, col='yellow', lty=1, main='ROCs: Model Performance Comparision') 
      plot(perfnn, col='blue',add=TRUE,lty=8);
      plot(perfarbre, col='magenta',add=TRUE,lty=9); 
      legend(0.6,0.5,
             c("logistic reg", 
               "Neural Network", 
               "Arbre de decision"),
             col=c('yellow','blue', 'magenta'),
             lwd=3);
      lines(c(0,1),c(0,1),col = "gray", lty = 4 )
    })
    
    output$table<-renderTable({
      model_performance <- as.data.frame(cbind(models, models_AUC))
      colnames(model_performance) <- c("Model", "AUC")
      model_performance
    })
    
    
    
    
    
    
  }
)


