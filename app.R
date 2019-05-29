library(shiny)
library(downloader)
library(zoo)
library(caret)
library(randomForest)
library(e1071)
library(ggplot2)


normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

TomorrowDate <- function(x) 
  format(
    as.Date(x, format = "%Y.%m.%d") + 1, 
    format = "%d.%m.%Y"
  )


download("https://coinmetrics.io/data/all.zip", dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./")


t <- read.csv("gold.csv")


file_vec <- list.files(pattern = ".csv")
sselec<- lapply(file_vec,read.csv)
names(sselec)<-file_vec
n<-lapply(sselec, colnames)




file_vec <- list.files(pattern = ".csv")
sselec<- lapply(file_vec,read.csv)
names(sselec)<-file_vec

expl1<- file_vec[-which(file_vec=="gold.csv")]


l1 <- lapply(expl1, read.csv)

n=1000
crip_to_remove1 <- sapply(l1, function(x) nrow(x) < n)

l1 <- l1[!crip_to_remove1]


l1 <- lapply(l1, function(x) as.data.frame(x[nrow(x)-(nrow(x)-n):nrow(x), ]))

l1 <- lapply(l1, function(x) as.data.frame(x[,-1  ]))


df_exp1 <- do.call(cbind, l1)

exp_gld=df_exp1[, colSums(is.na(df_exp1)) != nrow(df_exp1)]



gld=t[,2]

df_gld <- data.frame(gld[(NROW(gld)-(n-1)):NROW(gld)],exp_gld)

out_gld<- vector()

for (i in 1:NROW(df_gld[,1])) {
  
  
  out_gld[i]<-ifelse(df_gld[i,1]*1<df_gld[i+1,1],1,0)
  
  
}



df_g<-data.frame(out_gld,exp_gld)


xg<-na.aggregate(df_g[-n,])

xg<- lapply(xg, normalize)

xg <- as.data.frame(xg)



xghoy<-na.aggregate(df_g)

xghoy<- lapply(xghoy, normalize)

xghoy <- as.data.frame(xghoy)



pos=xg[xg$out_gld==1,];neg=xg[xg$out_gld==0,]
set.seed(7)
epos=sample(1:nrow(pos),floor(0.7*nrow(pos)))
post=pos[epos,]
poscv=pos[-epos,]

set.seed(7)
eneg=sample(1:nrow(neg),floor(0.7*nrow(neg)))
negt=neg[eneg,]
negcv=neg[-eneg,]
traing=rbind(post,negt); cvg=rbind(poscv,negcv)

rfgo1<-  randomForest(as.factor( traing$out_gld)~.,
                     data = traing[, -1],ntree=1000)

pgrf1<-  predict(rfgo1,cvg[,-1])

cM1 <- confusionMatrix(pgrf1,as.factor(cvg[,1]),positive = "1")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Random Forest para predecir aumentos y descensos en el precio del oro MAÑANA"),
    h5("Implementación del algoritmo randomForest de Breiman para clasificación"),
    
h5("Por favor espere 10 segundos mientras se computa el resultado"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
         
          h5("Resultados de predicciones con datos nuevos para el algoritmo"),
          h5("Porcentaje de Exactitud"),
         paste( cM1$overall[1]*100 , "%"),
         h5("F1"),
         round( cM1$byClass[7], digits = 2),
         
         h5("Porcentaje de aciertos al precedir aumentos"),
        paste( round(cM1$byClass[1],digit=2)*100,"%"),
         h5("Porcentaje de aciertos al predecir descensos"),
         paste(round( cM1$byClass[2],digits = 2)*100,"%")
      
         
       
       
        ),

        # Show a plot of the generated distribution
        mainPanel(
          
          
          textOutput("text"),
          tags$head(tags$style("#text{color: black;
                                 font-size: 40px;
                                 font-style: bold;
                                 }"
          )),
        
          plotOutput("plot"),
          
        
          h5("*Esta app está en desarrollo activo. Si quieres apoyar el proyecto, información en programandoconro.wordpress.com"),
          
          h5("App desarrollada por Rodrigo Díaz Lupanow"),
          h5("Para contrataciones, sugerencias y comentarios, escribir a programandoconro@gmail.com")
         
          
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


    output$text <- renderText({
      rfgo<-  randomForest(as.factor( traing$out_gld)~.,ntree=1000,
                           data = traing[, -1])
      
      pgrf<-  predict(rfgo,xghoy[NROW(xghoy),-1])
      
      
      ifelse( pgrf ==0, paste("El algoritmo Random Forest predice que el precio del oro mañana ",TomorrowDate(Sys.Date()),"BAJARÁ"), paste("El algoritmo RandomForest predice que el precio del oro mañana ",TomorrowDate(Sys.Date()),"AUMENTARÁ"))
  
     
    })
    
    output$plot <- renderPlot({
      
      g<- ggplot(data = t[2258:nrow(t),],aes(x=2258:nrow(t),y=t$value[2258:nrow(t)]))
      g+geom_point()+geom_line()+xlab ("Días recientes")+ylab ("Valor (USD)")
      
 
    })
    
    

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
