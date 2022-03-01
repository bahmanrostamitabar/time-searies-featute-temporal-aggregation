library(fmsb)
library(shiny)

#Možda ubaciti teme

#FT <- read.table("C:/Users/Logistika/Dropbox/Dejan-Bahman/Temporal-Aggregation/paper/img/EvalTable.txt",header = TRUE)
FT <- read.table("EvalTable.txt",header = TRUE);FT<-as.matrix(FT)

#Uvesti ovdje da se bira koje osobine hoces da vidis, ako je to moguce zbog različitih skala
FT <- FT[,-26];FT <- FT[,c(1:2,11,20,25)];
FT <- FT[-1,]#Izbacen Null model

Tabela<-rbind(rep(round(max(FT[-1,])),dim(FT)[2]),rep(0,dim(FT)[2]),FT)
rownames(Tabela) <- c("max","min","LR","LDA","QDA","KNN","Lasso","GAM","RF","Boosting","SVM")
colnames(Tabela) <- c("False positive", "False negative", "F-statistics","Misclassification error","AUC")

#Ova druga tabela je dodata da bi prva kolona bili nazivi modela
Tabela2 <- cbind(rownames(Tabela),Tabela)
colnames(Tabela2) <- c("Models","False positive", "False negative", "F-statistics","Misclassification error","AUC")

#Cost and benefits tabla
table_CB <- read.delim("table_CB.txt")

ui <- fluidPage(
    
    titlePanel("Evaluation of machine learning models"),
    
    sidebarLayout(
        sidebarPanel(width = 2,
                     #checkboxGroupInput(inputId="modeli", label="Select the machine learning models that you want to compare:", choices=list("Null"=3,"LR"=4,"LDA"=5,"QDA"=6,"KNN"=7,"Lasso"=8,"GAM"=9,"RF"=10,"Boosting"=11,"SVM"=12)), #ovo je sa Null Modelom
                     
                     checkboxGroupInput(inputId="modeli", label="Select models:", choices=list("LR"=3,"LDA"=4,"QDA"=5,"KNN"=6,"Lasso"=7,"GAM"=8,"RF"=9,"Boosting"=10,"SVM"=11))
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Machine learning",
                         plotOutput("hist")
                ),
                tabPanel("Classification metrics table",
                         dataTableOutput("table")
                )
            )
        )
    )
)

server <- function(input, output) {
    
    output$hist<-renderPlot(width = 1000, height = 1000,{
        par(mfrow=c(1,1))
        
        if(length(input$modeli)!=0){
            radarchart(as.data.frame(Tabela)[c(1:2,as.numeric((input$modeli))),],axistype=2,cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,vlcex=1.5,pty = 16,calcex=1.8,plwd = 2,seg=4, pcol=c(1,"slateblue2",3,4,5,6,"red",7,"orange2"), plty = 1)
            legend("topright",legend=c(rownames(Tabela)[c(as.numeric((input$modeli)))]),pch=19,col=c(1,"slateblue2",3,4,5,6,"red",7,"orange2")[c(1:length(input$modeli))])
        }
    })
    
    output$table<-renderDataTable({
        
        
        Tabela2[c(as.numeric((input$modeli))),]
    })  
    
}

shinyApp(ui = ui, server = server)

#300 dpi za rad
#png("Fig_ML.png",  width = 300, height = 300, units = 'mm', res = 300)

#radarchart(as.data.frame(Tabela)[c(1:11),],axistype=2,cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,vlcex=1.5,pty = 16,calcex=1.8,plwd = 2,seg=4, pcol=c(1,"slateblue2",3,4,5,6,"red",7,"orange2"), plty = 1)
#legend("topright",legend=c(rownames(Tabela)[c(3:11)]),pch=19,col=c(1,"slateblue2",3,4,5,6,"red",7,"orange2")[c(1:9)])

#dev.off()
