  library(downloader)
  library(ggplot2)
  library(MASS)
  library(zoo)
  
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
  
  file_vec <- list.files(pattern = ".csv")
  sselec<- lapply(file_vec,read.csv)
  names(sselec)<-file_vec
  n<-lapply(sselec, colnames)
  
  
  
  
  t1 <- read.csv("dxy.csv")
  t2 <- read.csv("gold.csv")
  gld=t2[c(-1:-4),2]
  
  str(t1)
  summary(t1)
  
  yr<-format(Sys.Date(), "  %Y")
  yr<-as.numeric(yr)
        
        
    g1 <- ggplot(data = t1,aes(x=1:nrow(t1),y=t1[,2]))
    
    g1 <- g1+geom_line()+geom_smooth()+
      xlab("Año")+ylab("Valor de mercado")+
      ggtitle("Valor del Dólar en el tiempo")+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_x_continuous(breaks = c(1,364,365*2,364*3,364*4,365*5,364*6), 
                         labels = c(yr-6,yr-5,yr-4,yr-3,yr-2,yr-1,yr))
    
    g1
    
    
    
 
    str(t2)
    summary(t)
    
    
    
    
    g2 <- ggplot(data = t2,aes(x=1:nrow(t2),y=t2[,2]))
    
    g2 <- g2+geom_line()+geom_smooth()+
      xlab("Año")+ylab("Valor (USD)")+
      ggtitle("Valor del oro en el tiempo")+
      theme(plot.title = element_text(hjust = 0.5))+
       scale_x_continuous(breaks = c(1,364,365*2,364*3,364*4,365*5,364*6), 
                          labels = c(yr-6,yr-5,yr-4,yr-3,yr-2,yr-1,yr))
    
    g2
    
    
    
    
    
    
    
    g3 <- ggplot(data = t1,aes(x=1:nrow(t1),y=t1[,2]))
    
    g3 <- g3+geom_line()+geom_smooth(aes(col="blue"))+
      xlab("Año")+ylab("Valor de mercado")+
      ggtitle("Tendencias del Oro y el Dólar en los últimos años")+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_x_continuous(breaks = c(1,364,365*2,364*3,364*4,365*5,364*6), 
                         labels = c(yr-6,yr-5,yr-4,yr-3,yr-2,yr-1,yr))
    
    g3+geom_line(aes(y=gld/10))+geom_smooth(aes(y=gld/10,col='red'))+
      scale_color_discrete(name = "", labels = c("Oro/10","Dólar" ))
    
      
    
    file_vec <- list.files(pattern = ".csv")
    sselec<- lapply(file_vec,read.csv)
    names(sselec)<-file_vec
    
    expl1<- file_vec[-which(file_vec=="gold.csv")]
    expl2<- file_vec[-which(file_vec=="dxy.csv")]
    
    l1 <- lapply(expl1, read.csv)
    l2 <- lapply(expl2, read.csv)
    
    crip_to_remove1 <- sapply(l1, function(x) nrow(x) < 1000)
    crip_to_remove2 <- sapply(l2, function(x) nrow(x) < 1000)
    
    l1 <- l1[!crip_to_remove1]
    l2 <- l2[!crip_to_remove1]
    
    l1 <- lapply(l1, function(x) as.data.frame(x[nrow(x)-(nrow(x)-1000):nrow(x),  ]))
    l2 <- lapply(l2, function(x) as.data.frame(x[nrow(x)-(nrow(x)-1000):nrow(x),  ]))
    
    l1 <- lapply(l1, function(x) as.data.frame(x[,-1  ]))
    l2 <- lapply(l2, function(x) as.data.frame(x[,-1  ]))
    
    df_exp1 <- do.call(cbind, l1)
    df_exp2 <- do.call(cbind, l2)
    

##############################################################3

    exp_gld=df_exp1[, colSums(is.na(df_exp1)) != nrow(df_exp1)]
    exp_dlr=df_exp2[, colSums(is.na(df_exp2)) != nrow(df_exp2)]
    
    gld <- t2[c(-1:-4),2]

    dlr <- t1[,2]


df_gld <- data.frame(gld[-1001:-2350],exp_gld)
df_dlr <- data.frame(dlr[-1001:-2350],exp_dlr)


out_gld<- vector()

for (i in 1:NROW(df_gld$gld..1001..2350.)) {
  
  
  out_gld[i]<-ifelse(df_gld$gld..1001..2350.[i]*1<df_gld$gld..1001..2350.[i+1],1,0)
  
  
}

out_dlr<- vector()

for (i in 1:NROW(df_dlr$dlr..1001..2350.)) {
  
  
  out_dlr[i]<-ifelse(df_dlr$dlr..1001..2350.[i]*1<df_dlr$dlr..1001..2350.[i+1],1,0)
  
  
}


df_d<-data.frame(out_dlr,exp_dlr)
df_g<-data.frame(out_gld,exp_gld)


xd<-na.aggregate(df_d[-1000,])
xg<-na.aggregate(df_g[-1000,])

xd<-lapply(xd, normalize)
xg<- lapply(xg, normalize)

xd <- as.data.frame(xd)
xg <- as.data.frame(xg)



model_d <- stepAIC(glm(xd$out_dlr~.,data = xd[,-1],family = binomial(link = "logit")))

model_g <- stepAIC(glm(xg$out_gld~.,data = xg[,-1],family = binomial(link = "logit")))





