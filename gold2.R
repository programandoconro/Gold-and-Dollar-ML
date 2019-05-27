library(downloader)
library(ggplot2)
library(MASS)
library(zoo)
library(caret)
library(randomForest)
library(e1071)
library(reticulate)

normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

TomorrowDate <- function(x) 
  format(
    as.Date(x, format = "%Y.%m.%d") + 1, 
    format = "%Y.%m.%d"
  )

setwd("~/Dropbox/Monero/gold")

download("https://coinmetrics.io/data/all.zip", dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./")
file.remove("dataset.zip")


t <- read.csv("gold.csv")


file_vec <- list.files(pattern = ".csv")
sselec<- lapply(file_vec,read.csv)
names(sselec)<-file_vec
n<-lapply(sselec, colnames)


  yr<-format(Sys.Date(), "  %Y")
  yr<-as.numeric(yr)



g <- ggplot(data = t,aes(x=1:nrow(t),y=t[,2]))

g <- g+geom_line()+geom_smooth()+
  xlab("Año")+ylab("Valor (USD)")+
  ggtitle("Valor del oro en el tiempo")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = c(1,364,365*2,364*3,364*4,365*5,364*6), 
                     labels = c(yr-6,yr-5,yr-4,yr-3,yr-2,yr-1,yr))
g






file_vec <- list.files(pattern = ".csv")
sselec<- lapply(file_vec,read.csv)
names(sselec)<-file_vec

expl1<- file_vec[-which(file_vec=="gold.csv")]


l1 <- lapply(expl1, read.csv)


crip_to_remove1 <- sapply(l1, function(x) nrow(x) < 1000)

l1 <- l1[!crip_to_remove1]


l1 <- lapply(l1, function(x) as.data.frame(x[nrow(x)-(nrow(x)-1000):nrow(x), ]))

l1 <- lapply(l1, function(x) as.data.frame(x[,-1  ]))


df_exp1 <- do.call(cbind, l1)

exp_gld=df_exp1[, colSums(is.na(df_exp1)) != nrow(df_exp1)]



gld=t[,2]

df_gld <- data.frame(gld[(NROW(gld)-999):NROW(gld)],exp_gld)

out_gld<- vector()

for (i in 1:NROW(df_gld[,1])) {
  
  
  out_gld[i]<-ifelse(df_gld[i,1]*1<df_gld[i+1,1],1,0)
  
  
}



df_g<-data.frame(out_gld,exp_gld)


xg<-na.aggregate(df_g[-1000,])

xg<- lapply(xg, normalize)

xg <- as.data.frame(xg)


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

rfgo<-  randomForest(as.factor( traing$out_gld)~.,
                     data = traing[, -1])

pgrf<-  predict(rfgo,cvg[,-1])

cM <- confusionMatrix(pgrf,as.factor(cvg[,1]))


as.table(cM)


cM$mode



