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
  
  
      df_gld <- data.frame(gld[-1:-1350],exp_gld)
      df_dlr <- data.frame(dlr[-1:-1350],exp_dlr)
  
  
  out_gld<- vector()
  
  for (i in 1:NROW(df_gld[,1])) {
    
    
    out_gld[i]<-ifelse(df_gld[i,1]*1<df_gld[i+1,1],1,0)
    
    
  }
  
  out_dlr<- vector()
  
  for (i in 1:NROW(df_dlr[,1])) {
    
    
    out_dlr[i]<-ifelse(df_dlr[i,1]*1<df_dlr[i+1,1],1,0)
    
    
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
        
        
   d=  glm(formula = xd$out_dlr ~ activeAddresses + paymentCount + blockSize + 
              txVolume.USD. + marketcap.USD. + price.USD. + medianFee.1 + 
              realizedCap.USD..2 + generatedCoins.2 + blockCount.2 + txCount.3 + 
              generatedCoins.3 + fees.3 + txVolume.USD..3 + txCount.4 + 
              fees.4 + activeAddresses.4 + averageDifficulty.4 + medianTxValue.USD..3 + 
              blockCount.4 + txVolume.USD..4 + txCount.5 + marketcap.USD..4 + 
              price.USD..4 + exchangeVolume.USD..4 + activeAddresses.5 + 
              txVolume.USD..5 + marketcap.USD..5 + price.USD..5 + activeAddresses.6 + 
              averageDifficulty.5 + paymentCount.5 + txCount.7 + price.USD..6 + 
              medianTxValue.USD..6 + x....1. + txVolume.USD..7 + txCount.8 + 
              exchangeVolume.USD..7 + medianFee.6 + paymentCount.6 + marketcap.USD..8 + 
              price.USD..8 + realizedCap.USD..3 + generatedCoins.6 + paymentCount.7 + 
              blockSize.6 + blockCount.6 + exchangeVolume.USD..9 + txCount.11 + 
              marketcap.USD..10 + price.USD..10 + exchangeVolume.USD..10 + 
              realizedCap.USD..4 + blockCount.7 + price.USD..11 + activeAddresses.11 + 
              medianTxValue.USD..10 + x....1..1 + txVolume.USD..12 + reward + 
              txCount.14 + price.USD..13 + realizedCap.USD..5 + generatedCoins.8 + 
              activeAddresses.12 + paymentCount.9 + medianTxValue.USD..11 + 
              blockCount.8 + txCount.15 + marketcap.USD..14 + price.USD..14 + 
              medianFee.10 + activeAddresses.13 + txCount.16 + marketcap.USD..15 + 
              price.USD..15 + exchangeVolume.USD..15 + fees.11 + medianTxValue.USD..13 + 
              medianFee.11 + txCount.17 + marketcap.USD..16 + price.USD..16 + 
              exchangeVolume.USD..17 + generatedCoins.9 + activeAddresses.16 + 
              averageDifficulty.9 + paymentCount.10 + blockCount.9 + txCount.19 + 
              paymentCount.11 + activeAddresses.17 + medianFee.13 + price.USD..18 + 
              txVolume.USD..18 + adjustedTxVolume.USD..8 + marketcap.USD..19 + 
              generatedCoins.10 + averageDifficulty.10 + paymentCount.12 + 
              medianTxValue.USD..15 + medianFee.14 + blockSize.10 + blockCount.10, 
            family = binomial(link = "logit"), data = xd[, -1])
        
        
        
        
        go<-  glm(formula = xg$out_gld ~ txCount + generatedCoins + activeAddresses + 
                     medianFee + blockSize + blockCount + txVolume.USD. + marketcap.USD. + 
                     price.USD. + activeAddresses.1 + paymentCount.1 + medianTxValue.USD. + 
                     txVolume.USD..1 + adjustedTxVolume.USD..1 + realizedCap.USD..2 + 
                     generatedCoins.2 + fees.2 + paymentCount.2 + medianTxValue.USD..1 + 
                     blockCount.2 + txVolume.USD..2 + averageDifficulty.3 + paymentCount.3 + 
                     blockCount.3 + txCount.4 + exchangeVolume.USD..3 + generatedCoins.4 + 
                     fees.4 + activeAddresses.4 + medianFee.4 + txCount.5 + marketcap.USD..4 + 
                     medianTxValue.USD..4 + medianFee.5 + averageDifficulty.5 + 
                     paymentCount.5 + marketcap.USD..5 + price.USD..5 + exchangeVolume.USD..5 + 
                     generatedCoins.6 + medianFee.6 + averageDifficulty.6 + blockSize.6 + 
                     x....1..1 + txCount.7 + price.USD..6 + fees.7 + paymentCount.7 + 
                     txVolume.USD..7 + txCount.8 + activeAddresses.8 + averageDifficulty.7 + 
                     paymentCount.8 + medianTxValue.USD..7 + txVolume.USD..8 + 
                     txCount.9 + marketcap.USD..8 + txVolume.USD..9 + adjustedTxVolume.USD..6 + 
                     txCount.10 + exchangeVolume.USD..9 + averageDifficulty.8 + 
                     paymentCount.9 + blockCount.8 + price.USD..10 + exchangeVolume.USD..10 + 
                     medianTxValue.USD..9 + x....1..2 + txCount.12 + txCount.13 + 
                     marketcap.USD..12 + price.USD..12 + exchangeVolume.USD..12 + 
                     generatedCoins.9 + fees.10 + averageDifficulty.9 + paymentCount.10 + 
                     medianFee.10 + blockSize.9 + blockCount.9 + txCount.14 + 
                     marketcap.USD..13 + price.USD..13 + fees.11 + medianFee.11 + 
                     txVolume.USD..14 + txCount.16 + price.USD..15 + txCount.17 + 
                     exchangeVolume.USD..16 + fees.14 + blockSize.10 + exchangeVolume.USD..17 + 
                     adjustedTxVolume.USD..8 + marketcap.USD..18 + realizedCap.USD..6 + 
                     fees.16 + activeAddresses.17 + paymentCount.13 + blockSize.11, 
                   family = binomial(link = "logit"), data = xg[, -1])





