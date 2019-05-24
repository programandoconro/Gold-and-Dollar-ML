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
      
      #download("https://coinmetrics.io/data/all.zip", dest="dataset.zip", mode="wb") 
      #unzip ("dataset.zip", exdir = "./")
      #file.remove("dataset.zip")
      
      file_vec <- list.files(pattern = ".csv")
      sselec<- lapply(file_vec,read.csv)
      names(sselec)<-file_vec
      n<-lapply(sselec, colnames)
        
      
      
      
        t1 <- read.csv("dxy.csv")
        t2 <- read.csv("gold.csv")
      gld=t2[c(-1:-5),2]
      
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
        
        l1 <- lapply(l1, function(x) as.data.frame(x[nrow(x)-(nrow(x)-1000):nrow(x), ]))
        l2 <- lapply(l2, function(x) as.data.frame(x[nrow(x)-(nrow(x)-1000):nrow(x),]))
        
        l1 <- lapply(l1, function(x) as.data.frame(x[,-1  ]))
        l2 <- lapply(l2, function(x) as.data.frame(x[,-1  ]))
        
        df_exp1 <- do.call(cbind, l1)
        df_exp2 <- do.call(cbind, l2)
        
    
    ##############################################################3
    
        exp_gld=df_exp1[, colSums(is.na(df_exp1)) != nrow(df_exp1)]
        exp_dlr=df_exp2[, colSums(is.na(df_exp2)) != nrow(df_exp2)]
        
      
    
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
    
    
    
        #  model_d <- stepAIC(glm(xd$out_dlr~.,data = xd[,-1],family = binomial(link = "logit")))
          
        #  model_g <- stepAIC(glm(xg$out_gld~.,data = xg[,-1],family = binomial(link = "logit")))
          
     
        
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
            
            
            pos=xd[xd$out_dlr==1,];neg=xd[xd$out_dlr==0,]
            set.seed(7)
            epos=sample(1:nrow(pos),floor(0.7*nrow(pos)))
            post=pos[epos,]
            poscv=pos[-epos,]
            
            set.seed(7)
            eneg=sample(1:nrow(neg),floor(0.7*nrow(neg)))
            negt=neg[eneg,]
            negcv=neg[-eneg,]
            traind=rbind(post,negt); cvd=rbind(poscv,negcv)
            
            
            
            
          
          do=  svm(traind$out_dlr~.,
                   data = traind[,-1],type="C-classification")
          
        
          go<-  svm( traing$out_gld~.,
                    data = traing[, -1],type="C-classification")
          
      pg<-  predict(go,cvg[,-1])
      pd<-  predict(do,cvd[,-1])
  
      
      confusionMatrix(pg,as.factor(cvg[,1]))
      confusionMatrix(pd,as.factor(cvd[,1]))
  
      
      
      rfdo=  randomForest(as.factor(traind$out_dlr)~.,
               data = traind[, -1])
      
      
      rfgo<-  randomForest(as.factor( traing$out_gld)~.,
                 data = traing[, -1])
      
      pg<-  predict(rfgo,cvg[,-1])
      pd<-  predict(rfdo,cvd[,-1])
      
      
      confusionMatrix(pg,as.factor(cvg[,1]))
      confusionMatrix(pd,as.factor(cvd[,1]))
      
     
      
       py_config
      use_python("/home/ro/anaconda3/envs/ro3/bin/python")
      
      
      k = import("sklearn.ensemble")
      
      rfd = k$RandomForestClassifier()
      
   
      
      rfd$fit (traind[,-1], traind[,1])
      
      k$RandomForestClassifier(bootstrap=TRUE, class_weight=NULL, criterion='gini',
                               max_depth=NULL, max_features='auto', max_leaf_nodes=NULL,
                               min_impurity_split=1e-07, min_samples_leaf=1,
                               min_samples_split=2, min_weight_fraction_leaf=0.0,
                               n_estimators=10, n_jobs=1, oob_score=FALSE, random_state=NULL,
                               verbose=0, warm_start=FALSE)
      
      
      rfg = k$RandomForestClassifier()
      
 
      rfg$fit (traing[,-1], traing[,1])
      
      k$RandomForestClassifier(bootstrap=TRUE, class_weight=NULL, criterion='gini',
                               max_depth=NULL, max_features='auto', max_leaf_nodes=NULL,
                               min_impurity_split=1e-07, min_samples_leaf=1,
                               min_samples_split=2, min_weight_fraction_leaf=0.0,
                               n_estimators=10, n_jobs=1, oob_score=FALSE, random_state=NULL,
                               verbose=0, warm_start=FALSE)
      
      
      
      
      y_d = rfd$predict(cvd[,-1])
      y_g = rfg$predict(cvg[,-1])
      
      
      confusionMatrix(as.factor(y_d),as.factor(cvd[,1]))
      confusionMatrix(as.factor(y_g),as.factor(cvg[,1]))
      
      
      
     

