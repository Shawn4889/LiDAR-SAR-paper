
#Do T-test on just on back scatter change for different bins.
file <- "2018-2010.csv"
dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result"
i <- file.path(dir,paste0(file))
Data = read.csv(i,header=T)
Data = Data[sample(nrow(Data), 3000), ]
Data$SAR <- Data$X2018.2010.HV
polar <- "HV"
Data$ALS <- Data$Volume
reg <- lm(SAR  ~ log2(ALS), data = Data)
Data$ALS <- predict(reg)

VC <- "Volume"
#ymin <- -0.5
#ymax <- 0.5
ymin <- -5000
ymax <- 5000

Data <- Data[Data$SAR <= 5 & Data$SAR >= -5,]
out <- file.path(dir,paste0(strsplit(basename(i),'.csv')[[1]],"_",polar,"_",VC,".jpg"))
out_table <- file.path(dir,paste0(strsplit(basename(i),'.csv')[[1]],"_",polar,"_",VC,".csv"))
Data$Type[Data$SAR >= 4 & Data$SAR < 5] = "4~5"
Data$Type[Data$SAR >= 3 & Data$SAR < 4] = "3~4"
Data$Type[Data$SAR >= 2 & Data$SAR < 3] = "2~3"
Data$Type[Data$SAR >= 1 & Data$SAR < 2] = "1~2"
Data$Type[Data$SAR >= 0 & Data$SAR <  1] = "0~1"
Data$Type[Data$SAR >= -1 & Data$SAR < 0] = "-1~0"
Data$Type[Data$SAR >= -2 & Data$SAR < -1] = "-2~-1"
Data$Type[Data$SAR >= -3 & Data$SAR < -2] = "-3~-2"
Data$Type[Data$SAR >= -4 & Data$SAR < -3] = "-4~-3"
Data$Type[Data$SAR >= -5 & Data$SAR < -4] = "-5~-4"

Data$Type<-factor(Data$Type, levels=c("4~5","3~4", "2~3","1~2","0~1",
                                      "-1~0","-2~-1","-3~-2","-4~-3","-5~-4"))
r1 <- compare_means(
  ALS~Type, 
  Data, 
  method = "t.test", 
  p.adjust.method = "BH")
r2 <- r1[c("group1", "group2", "p.signif")]
library(igraph)
G <- graph.data.frame(r2,directed=FALSE)
A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="p.signif",type='lower')
A
write.csv(A,file=out_table)



#Do T-test on just on back scatter change for different bins.
file <- "2018-2008.csv"
dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result"
i <- file.path(dir,paste0(file))
Data = read.csv(i,header=T)
Data = Data[sample(nrow(Data), 3000), ]
Data$SAR <- Data$X2018.2008.HV
polar <- "HV"
Data$ALS <- Data$Volume
reg <- lm(SAR  ~ log2(ALS), data = Data)
Data$ALS <- predict(reg)
VC <- "Volume"
#ymin <- -0.5
#ymax <- 0.5
ymin <- -5000
ymax <- 5000

Data <- Data[Data$SAR <= 5 & Data$SAR >= -5,]
out <- file.path(dir,paste0(strsplit(basename(i),'.csv')[[1]],"_",polar,"_",VC,".jpg"))
out_table <- file.path(dir,paste0(strsplit(basename(i),'.csv')[[1]],"_",polar,"_",VC,".csv"))
Data$Type[Data$SAR >= 4 & Data$SAR < 5] = "4~5"
Data$Type[Data$SAR >= 3 & Data$SAR < 4] = "3~4"
Data$Type[Data$SAR >= 2 & Data$SAR < 3] = "2~3"
Data$Type[Data$SAR >= 1 & Data$SAR < 2] = "1~2"
Data$Type[Data$SAR >= 0 & Data$SAR <  1] = "0~1"
Data$Type[Data$SAR >= -1 & Data$SAR < 0] = "-1~0"
Data$Type[Data$SAR >= -2 & Data$SAR < -1] = "-2~-1"
Data$Type[Data$SAR >= -3 & Data$SAR < -2] = "-3~-2"
Data$Type[Data$SAR >= -4 & Data$SAR < -3] = "-4~-3"
Data$Type[Data$SAR >= -5 & Data$SAR < -4] = "-5~-4"

Data$Type<-factor(Data$Type, levels=c("4~5","3~4", "2~3","1~2","0~1",
                                      "-1~0","-2~-1","-3~-2","-4~-3","-5~-4"))
r1 <- compare_means(
  ALS~Type, 
  Data, 
  method = "t.test", 
  p.adjust.method = "BH")
r2 <- r1[c("group1", "group2", "p.signif")]
library(igraph)
G <- graph.data.frame(r2,directed=FALSE)
A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="p.signif",type='lower')
A
write.csv(A,file=out_table)



#Do T-test on just on back scatter change for different bins.
file <- "2018-2008.csv"
dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result"
i <- file.path(dir,paste0(file))
Data = read.csv(i,header=T)
Data = Data[sample(nrow(Data), 3000), ]
Data$SAR <- Data$X2018.2008.HV7
polar <- "HV7"
Data$ALS <- Data$Volume
reg <- lm(SAR  ~ log2(ALS), data = Data)
Data$ALS <- predict(reg)
VC <- "Volume"
#ymin <- -0.5
#ymax <- 0.5
ymin <- -5000
ymax <- 5000

Data <- Data[Data$SAR <= 5 & Data$SAR >= -5,]
out <- file.path(dir,paste0(strsplit(basename(i),'.csv')[[1]],"_",polar,"_",VC,".jpg"))
out_table <- file.path(dir,paste0(strsplit(basename(i),'.csv')[[1]],"_",polar,"_",VC,".csv"))
Data$Type[Data$SAR >= 4 & Data$SAR < 5] = "4~5"
Data$Type[Data$SAR >= 3 & Data$SAR < 4] = "3~4"
Data$Type[Data$SAR >= 2 & Data$SAR < 3] = "2~3"
Data$Type[Data$SAR >= 1 & Data$SAR < 2] = "1~2"
Data$Type[Data$SAR >= 0 & Data$SAR <  1] = "0~1"
Data$Type[Data$SAR >= -1 & Data$SAR < 0] = "-1~0"
Data$Type[Data$SAR >= -2 & Data$SAR < -1] = "-2~-1"
Data$Type[Data$SAR >= -3 & Data$SAR < -2] = "-3~-2"
Data$Type[Data$SAR >= -4 & Data$SAR < -3] = "-4~-3"
Data$Type[Data$SAR >= -5 & Data$SAR < -4] = "-5~-4"

Data$Type<-factor(Data$Type, levels=c("4~5","3~4", "2~3","1~2","0~1",
                                      "-1~0","-2~-1","-3~-2","-4~-3","-5~-4"))
r1 <- compare_means(
  ALS~Type, 
  Data, 
  method = "t.test", 
  p.adjust.method = "BH")
r2 <- r1[c("group1", "group2", "p.signif")]
library(igraph)
G <- graph.data.frame(r2,directed=FALSE)
A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="p.signif",type='lower')
A
write.csv(A,file=out_table)

#Do T-test on just on back scatter change for different bins.
file <- "2010-2008.csv"
dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result"
i <- file.path(dir,paste0(file))
Data = read.csv(i,header=T)
Data = Data[sample(nrow(Data), 3000), ]
Data$SAR <- Data$X2010.2008.HV
polar <- "HV"
Data$ALS <- Data$Volume
reg <- lm(SAR  ~ log2(ALS), data = Data)
Data$ALS <- predict(reg)
VC <- "Volume"
#ymin <- -0.5
#ymax <- 0.5
ymin <- -5000
ymax <- 5000

Data <- Data[Data$SAR <= 5 & Data$SAR >= -5,]
out <- file.path(dir,paste0(strsplit(basename(i),'.csv')[[1]],"_",polar,"_",VC,".jpg"))
out_table <- file.path(dir,paste0(strsplit(basename(i),'.csv')[[1]],"_",polar,"_",VC,".csv"))
Data$Type[Data$SAR >= 4 & Data$SAR < 5] = "4~5"
Data$Type[Data$SAR >= 3 & Data$SAR < 4] = "3~4"
Data$Type[Data$SAR >= 2 & Data$SAR < 3] = "2~3"
Data$Type[Data$SAR >= 1 & Data$SAR < 2] = "1~2"
Data$Type[Data$SAR >= 0 & Data$SAR <  1] = "0~1"
Data$Type[Data$SAR >= -1 & Data$SAR < 0] = "-1~0"
Data$Type[Data$SAR >= -2 & Data$SAR < -1] = "-2~-1"
Data$Type[Data$SAR >= -3 & Data$SAR < -2] = "-3~-2"
Data$Type[Data$SAR >= -4 & Data$SAR < -3] = "-4~-3"
Data$Type[Data$SAR >= -5 & Data$SAR < -4] = "-5~-4"

Data$Type<-factor(Data$Type, levels=c("4~5","3~4", "2~3","1~2","0~1",
                                      "-1~0","-2~-1","-3~-2","-4~-3","-5~-4"))
r1 <- compare_means(
  ALS~Type, 
  Data, 
  method = "t.test", 
  p.adjust.method = "BH")
r2 <- r1[c("group1", "group2", "p.signif")]
library(igraph)
G <- graph.data.frame(r2,directed=FALSE)
A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="p.signif",type='lower')
A
write.csv(A,file=out_table)













#Do T-test on just on back scatter change for different bins.
file <- "2018-2010.csv"
dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result"
i <- file.path(dir,paste0(file))
Data = read.csv(i,header=T)
Data = Data[sample(nrow(Data), 3000), ]
Data$SAR <- Data$X2018.2010.HV
polar <- "HV"
Data$ALS <- Data$Cover
reg <- lm(SAR  ~ log2(ALS), data = Data)
Data$ALS <- predict(reg)
VC <- "Cover"
ymin <- -0.5
ymax <- 0.5
#ymin <- -5000
#ymax <- 5000

Data <- Data[Data$SAR <= 5 & Data$SAR >= -5,]
out <- file.path(dir,paste0(strsplit(basename(i),'.csv')[[1]],"_",polar,"_",VC,".jpg"))
out_table <- file.path(dir,paste0(strsplit(basename(i),'.csv')[[1]],"_",polar,"_",VC,".csv"))
Data$Type[Data$SAR >= 4 & Data$SAR < 5] = "4~5"
Data$Type[Data$SAR >= 3 & Data$SAR < 4] = "3~4"
Data$Type[Data$SAR >= 2 & Data$SAR < 3] = "2~3"
Data$Type[Data$SAR >= 1 & Data$SAR < 2] = "1~2"
Data$Type[Data$SAR >= 0 & Data$SAR <  1] = "0~1"
Data$Type[Data$SAR >= -1 & Data$SAR < 0] = "-1~0"
Data$Type[Data$SAR >= -2 & Data$SAR < -1] = "-2~-1"
Data$Type[Data$SAR >= -3 & Data$SAR < -2] = "-3~-2"
Data$Type[Data$SAR >= -4 & Data$SAR < -3] = "-4~-3"
Data$Type[Data$SAR >= -5 & Data$SAR < -4] = "-5~-4"

Data$Type<-factor(Data$Type, levels=c("4~5","3~4", "2~3","1~2","0~1",
                                      "-1~0","-2~-1","-3~-2","-4~-3","-5~-4"))
r1 <- compare_means(
  ALS~Type, 
  Data, 
  method = "t.test", 
  p.adjust.method = "BH")
r2 <- r1[c("group1", "group2", "p.signif")]
library(igraph)
G <- graph.data.frame(r2,directed=FALSE)
A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="p.signif",type='lower')
A
write.csv(A,file=out_table)




#Do T-test on just on back scatter change for different bins.
file <- "2018-2008.csv"
dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result"
i <- file.path(dir,paste0(file))
Data = read.csv(i,header=T)
Data = Data[sample(nrow(Data), 3000), ]
Data$SAR <- Data$X2018.2008.HV
polar <- "HV"
Data$ALS <- Data$Cover
reg <- lm(SAR  ~ log2(ALS), data = Data)
Data$ALS <- predict(reg)
VC <- "Cover"
ymin <- -0.5
ymax <- 0.5
#ymin <- -5000
#ymax <- 5000

Data <- Data[Data$SAR <= 5 & Data$SAR >= -5,]
out <- file.path(dir,paste0(strsplit(basename(i),'.csv')[[1]],"_",polar,"_",VC,".jpg"))
out_table <- file.path(dir,paste0(strsplit(basename(i),'.csv')[[1]],"_",polar,"_",VC,".csv"))
Data$Type[Data$SAR >= 4 & Data$SAR < 5] = "4~5"
Data$Type[Data$SAR >= 3 & Data$SAR < 4] = "3~4"
Data$Type[Data$SAR >= 2 & Data$SAR < 3] = "2~3"
Data$Type[Data$SAR >= 1 & Data$SAR < 2] = "1~2"
Data$Type[Data$SAR >= 0 & Data$SAR <  1] = "0~1"
Data$Type[Data$SAR >= -1 & Data$SAR < 0] = "-1~0"
Data$Type[Data$SAR >= -2 & Data$SAR < -1] = "-2~-1"
Data$Type[Data$SAR >= -3 & Data$SAR < -2] = "-3~-2"
Data$Type[Data$SAR >= -4 & Data$SAR < -3] = "-4~-3"
Data$Type[Data$SAR >= -5 & Data$SAR < -4] = "-5~-4"

Data$Type<-factor(Data$Type, levels=c("4~5","3~4", "2~3","1~2","0~1",
                                      "-1~0","-2~-1","-3~-2","-4~-3","-5~-4"))
r1 <- compare_means(
  ALS~Type, 
  Data, 
  method = "t.test", 
  p.adjust.method = "BH")
r2 <- r1[c("group1", "group2", "p.signif")]
library(igraph)
G <- graph.data.frame(r2,directed=FALSE)
A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="p.signif",type='lower')
A
write.csv(A,file=out_table)



#Do T-test on just on back scatter change for different bins.
file <- "2018-2008.csv"
dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result"
i <- file.path(dir,paste0(file))
Data = read.csv(i,header=T)
Data = Data[sample(nrow(Data), 3000), ]
Data$SAR <- Data$X2018.2008.HV7
polar <- "HV7"
Data$ALS <- Data$Cover
reg <- lm(SAR  ~ log2(ALS), data = Data)
Data$ALS <- predict(reg)
VC <- "Cover"
ymin <- -0.5
ymax <- 0.5
#ymin <- -5000
#ymax <- 5000

Data <- Data[Data$SAR <= 5 & Data$SAR >= -5,]
out <- file.path(dir,paste0(strsplit(basename(i),'.csv')[[1]],"_",polar,"_",VC,".jpg"))
out_table <- file.path(dir,paste0(strsplit(basename(i),'.csv')[[1]],"_",polar,"_",VC,".csv"))
Data$Type[Data$SAR >= 4 & Data$SAR < 5] = "4~5"
Data$Type[Data$SAR >= 3 & Data$SAR < 4] = "3~4"
Data$Type[Data$SAR >= 2 & Data$SAR < 3] = "2~3"
Data$Type[Data$SAR >= 1 & Data$SAR < 2] = "1~2"
Data$Type[Data$SAR >= 0 & Data$SAR <  1] = "0~1"
Data$Type[Data$SAR >= -1 & Data$SAR < 0] = "-1~0"
Data$Type[Data$SAR >= -2 & Data$SAR < -1] = "-2~-1"
Data$Type[Data$SAR >= -3 & Data$SAR < -2] = "-3~-2"
Data$Type[Data$SAR >= -4 & Data$SAR < -3] = "-4~-3"
Data$Type[Data$SAR >= -5 & Data$SAR < -4] = "-5~-4"

Data$Type<-factor(Data$Type, levels=c("4~5","3~4", "2~3","1~2","0~1",
                                      "-1~0","-2~-1","-3~-2","-4~-3","-5~-4"))
r1 <- compare_means(
  ALS~Type, 
  Data, 
  method = "t.test", 
  p.adjust.method = "BH")
r2 <- r1[c("group1", "group2", "p.signif")]
library(igraph)
G <- graph.data.frame(r2,directed=FALSE)
A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="p.signif",type='lower')
A
write.csv(A,file=out_table)


#Do T-test on just on back scatter change for different bins.
file <- "2010-2008.csv"
dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result"
i <- file.path(dir,paste0(file))
Data = read.csv(i,header=T)
Data = Data[sample(nrow(Data), 3000), ]
Data$SAR <- Data$X2010.2008.HV
polar <- "HV"
Data$ALS <- Data$Cover
reg <- lm(SAR  ~ log2(ALS), data = Data)
Data$ALS <- predict(reg)
VC <- "Cover"
ymin <- -0.5
ymax <- 0.5
#ymin <- -5000
#ymax <- 5000

Data <- Data[Data$SAR <= 5 & Data$SAR >= -5,]
out <- file.path(dir,paste0(strsplit(basename(i),'.csv')[[1]],"_",polar,"_",VC,".jpg"))
out_table <- file.path(dir,paste0(strsplit(basename(i),'.csv')[[1]],"_",polar,"_",VC,".csv"))
Data$Type[Data$SAR >= 4 & Data$SAR < 5] = "4~5"
Data$Type[Data$SAR >= 3 & Data$SAR < 4] = "3~4"
Data$Type[Data$SAR >= 2 & Data$SAR < 3] = "2~3"
Data$Type[Data$SAR >= 1 & Data$SAR < 2] = "1~2"
Data$Type[Data$SAR >= 0 & Data$SAR <  1] = "0~1"
Data$Type[Data$SAR >= -1 & Data$SAR < 0] = "-1~0"
Data$Type[Data$SAR >= -2 & Data$SAR < -1] = "-2~-1"
Data$Type[Data$SAR >= -3 & Data$SAR < -2] = "-3~-2"
Data$Type[Data$SAR >= -4 & Data$SAR < -3] = "-4~-3"
Data$Type[Data$SAR >= -5 & Data$SAR < -4] = "-5~-4"

Data$Type<-factor(Data$Type, levels=c("4~5","3~4", "2~3","1~2","0~1",
                                      "-1~0","-2~-1","-3~-2","-4~-3","-5~-4"))
r1 <- compare_means(
  ALS~Type, 
  Data, 
  method = "t.test", 
  p.adjust.method = "BH")
r2 <- r1[c("group1", "group2", "p.signif")]
library(igraph)
G <- graph.data.frame(r2,directed=FALSE)
A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="p.signif",type='lower')
A
write.csv(A,file=out_table)











#Do T-test on just on back scatter change for different bins.

i <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\2010-2008.csv"
Data = read.csv(i,header=T)
Data <- Data[Data$C_2010 > 0 & Data$C_2008> 0,]


Data$SAR1 <- Data$X2010_HV
Data$SAR2 <- Data$X2008_HV

Data$ALS <- Data$C_2010 - Data$C_2008
  
reg1 <- lm(SAR1 ~ log2(C_2010), data = Data)
pred1 <- predict(reg1)
reg2 <- lm(SAR2 ~ log2(C_2008), data = Data)
pred2 <- predict(reg2)
Data$pred <- pred1 - pred2

Data$Type[Data$pred >= 0.3] = ">0.3"
Data$Type[Data$pred >= 0.2 & Data$pred < 0.3] = "0.2~0.3"
Data$Type[Data$pred >= 0.1 & Data$pred < 0.2] = "0.1~0.2"
Data$Type[Data$pred >= 0 & Data$pred < 0.1] = "0~0.1"
Data$Type[Data$pred >= -0.1 & Data$pred < 0] = "-0.1~0"
Data$Type[Data$pred >= -0.2 & Data$pred < -0.1] = "-0.2~-0.1"
Data$Type[Data$pred >= -0.3 & Data$pred < -0.2] = "-0.3~-0.2"
Data$Type[Data$pred <= -0.3] = "-0.3<"
Data$Type<-factor(Data$Type, levels=c("-0.3<","-0.3~-0.2", "-0.2~-0.1","-0.1~0",
                                      "0~0.1","0.1~0.2","0.2~0.3",">0.3"))
r1 <- compare_means(
  ALS~Type, 
  Data, 
  method = "t.test", 
  p.adjust.method = "BH")
r2 <- r1[c("group1", "group2", "p.signif")]
library(igraph)
G <- graph.data.frame(r2,directed=FALSE)
A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="p.signif",type='lower')
A
out_table <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\2010-2008_log_t.csv"
write.csv(A,file=out_table)







dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\figure"
i <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\2010-2008.csv"
Data = read.csv(i,header=T)
Data <- Data[Data$C_2010 > 0 & Data$C_2008 > 0,]

title = "2010-2008-HV-Cover-l"
Data$SAR1 <- Data$X2010_HV
Data$SAR2 <- Data$X2008_HV

Data$ALS <- Data$C_2010 - Data$C_2008

reg1 <- lm(C_2010 ~ SAR1, data = Data)
pred1 <- predict(reg1)
print(round(summary(reg1)$adj.r.squared,3))
reg2 <- lm(C_2008 ~ SAR2, data = Data)
pred2 <- predict(reg2)
Data$pred <- pred1 - pred2
Data <- Data[Data$pred > -0.4 & Data$pred < 0.4,]


Data$group_CC <- cut(Data$pred,breaks = seq(-0.4, 0.4, 0.1),dig.lab=1)

ggplot(Data, aes(x=pred, y=ALS, group = group_CC))+
  geom_boxplot(fill="grey70", fatten = 10,color = "black",outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single")) +
  theme_bw()+
  scale_x_continuous(breaks = seq(-0.4, 0.4, 0.1))+
  scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1))+
  theme(text=element_text(size=20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  labs(title = title,
       y = paste0("LiDAR cover change"),
       x = paste0("SAR cover change")) +
  theme(plot.title = element_text(hjust = 0.5))

out <- file.path(dir,paste0(title,".jpg"))
ggsave(out, height=15, width=15, dpi=300)









dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\figure"
i <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\2018-2010.csv"
Data = read.csv(i,header=T)
Data <- Data[Data$C_2018 > 0,]
Data = Data[sample(nrow(Data), 3000), ]
title = "2018-2010-HV-Cover"
Data$SAR1 <- Data$X2018_HV

reg1 <- lm(C_2018 ~ exp(SAR1), data = Data)
pred1 <- predict(reg1)
print(round(summary(reg1)$adj.r.squared,3))
round(cor(Data$C_2018,pred1)^2,3)

reg2 <- lm(C_2018 ~ SAR1, data = Data)
pred2 <- predict(reg2)
print(round(summary(reg2)$adj.r.squared,3))

p1<- ggplot(Data, aes(x=SAR1, y=C_2018))+ 
  geom_pointdensity()+
  scale_color_viridis(direction = 1)+
  annotate("text", x=-28, y=1, hjust = 0,color="blue",size = 10,
           label= paste(" Exp model R2: ",round(summary(reg1)$adj.r.squared,3))) + 
  geom_smooth(
    method="lm",
    formula = 'y ~ exp(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "blue")+
  labs(title="Inverted log",
       y = paste0("ALS cover"),
       x = paste0("SAR backscatter")) +
  theme(text=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5))


reg1 <- lm(SAR1 ~ exp(C_2018), data = Data)
pred1 <- predict(reg1)
print(round(summary(reg1)$adj.r.squared,3))

reg2 <- lm(SAR1 ~ C_2018, data = Data)
pred2 <- predict(reg2)
print(round(summary(reg2)$adj.r.squared,3))

p2<- ggplot(Data, aes(x=C_2018, y=SAR1))+ 
  geom_pointdensity()+
  scale_color_viridis(direction = 1)+
  annotate("text", x=0, y=-30, hjust = 0,color="blue",size = 10,
           label= paste(" Log model R2: ",round(summary(reg1)$adj.r.squared,3))) + 
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "blue")+
  labs(title="log",
       y = paste0("SAR backscatter"),
       x = paste0("ALS cover")) +
  theme(text=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5))



ggarrange(p1,p2)







#simple backscatter vs. cover change
name <- "2018-2008"
i <- file.path("E:\\ChangMap\\CHM\\DB_20210818\\DB_result",paste0(name,".csv"))
title <- paste0(name,"_HV_cover")
out_dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\figure"
out <- file.path(out_dir,paste0(title,".jpg"))
Data = read.csv(i,header=T)

Data <- Data[Data$C_2018 > 0 & Data$C_2008> 0,]
Data$ALS <- Data$C_2018 - Data$C_2008
Data$SAR <- Data$X2018_HV - Data$X2008_HV

Data <- Data[Data$ALS > -0.4 & Data$ALS < 0.4,]
Data <- Data[Data$SAR > -5 & Data$SAR < 5,]
Data$group_CC <- cut(Data$ALS,breaks = seq(-0.4, 0.4, 0.1),dig.lab=1)

ggplot(Data, aes(x=ALS, y=SAR, group = group_CC))+
  geom_boxplot(fill="grey70", fatten = 10,color = "black",outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single")) +
  theme_bw()+
  scale_x_continuous(breaks = seq(-0.4, 0.4, 0.1))+
  scale_y_continuous(breaks = seq(-5, 5, 1))+
  theme(text=element_text(size=20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  labs(title = title,
       x = paste0("LiDAR cover change"),
       y = paste0("SAR backscatter change")) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(out, height=15, width=15, dpi=300)



#simple backscatter vs. cover change
name <- "2018-2008"
i <- file.path("E:\\ChangMap\\CHM\\DB_20210818\\DB_result",paste0(name,".csv"))
title <- paste0(name,"_HV7_cover")
out_dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\figure"
out <- file.path(out_dir,paste0(title,".jpg"))
Data = read.csv(i,header=T)

Data <- Data[Data$C_2018 > 0 & Data$C_2008> 0,]
Data$ALS <- Data$C_2018 - Data$C_2008
Data$SAR <- Data$X2018_HV7 - Data$X2008_HV7

Data <- Data[Data$ALS > -0.4 & Data$ALS < 0.4,]
Data <- Data[Data$SAR > -5 & Data$SAR < 5,]
Data$group_CC <- cut(Data$ALS,breaks = seq(-0.4, 0.4, 0.1),dig.lab=1)

ggplot(Data, aes(x=ALS, y=SAR, group = group_CC))+
  geom_boxplot(fill="grey70", fatten = 10,color = "black",outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single")) +
  theme_bw()+
  scale_x_continuous(breaks = seq(-0.4, 0.4, 0.1))+
  scale_y_continuous(breaks = seq(-5, 5, 1))+
  theme(text=element_text(size=20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  labs(title = title,
       x = paste0("LiDAR cover change"),
       y = paste0("SAR backscatter change")) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(out, height=15, width=15, dpi=300)


#simple backscatter vs. cover change
name <- "2018-2010"
i <- file.path("E:\\ChangMap\\CHM\\DB_20210818\\DB_result",paste0(name,".csv"))
title <- paste0(name,"_HV_cover")
out_dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\figure"
out <- file.path(out_dir,paste0(title,".jpg"))
Data = read.csv(i,header=T)

Data <- Data[Data$C_2018 > 0 & Data$C_2010> 0,]
Data$ALS <- Data$C_2018 - Data$C_2010
Data$SAR <- Data$X2018_HV - Data$X2010_HV

Data <- Data[Data$ALS > -0.4 & Data$ALS < 0.4,]
Data <- Data[Data$SAR > -5 & Data$SAR < 5,]
Data$group_CC <- cut(Data$ALS,breaks = seq(-0.4, 0.4, 0.1),dig.lab=1)

ggplot(Data, aes(x=ALS, y=SAR, group = group_CC))+
  geom_boxplot(fill="grey70", fatten = 10,color = "black",outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single")) +
  theme_bw()+
  scale_x_continuous(breaks = seq(-0.4, 0.4, 0.1))+
  scale_y_continuous(breaks = seq(-5, 5, 1))+
  theme(text=element_text(size=20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  labs(title = title,
       x = paste0("LiDAR cover change"),
       y = paste0("SAR backscatter change")) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(out, height=15, width=15, dpi=300)


#simple backscatter vs. cover change
name <- "2010-2008"
i <- file.path("E:\\ChangMap\\CHM\\DB_20210818\\DB_result",paste0(name,".csv"))
title <- paste0(name,"_HV_cover")
out_dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\figure"
out <- file.path(out_dir,paste0(title,".jpg"))
Data = read.csv(i,header=T)

Data <- Data[Data$C_2010 > 0 & Data$C_2008> 0,]
Data$ALS <- Data$C_2010 - Data$C_2008
Data$SAR <- Data$X2010_HV - Data$X2008_HV

Data <- Data[Data$ALS > -0.4 & Data$ALS < 0.4,]
Data <- Data[Data$SAR > -5 & Data$SAR < 5,]
Data$group_CC <- cut(Data$ALS,breaks = seq(-0.4, 0.4, 0.1),dig.lab=1)

ggplot(Data, aes(x=ALS, y=SAR, group = group_CC))+
  geom_boxplot(fill="grey70", fatten = 10,color = "black",outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single")) +
  theme_bw()+
  scale_x_continuous(breaks = seq(-0.4, 0.4, 0.1))+
  scale_y_continuous(breaks = seq(-5, 5, 1))+
  theme(text=element_text(size=20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  labs(title = title,
       x = paste0("LiDAR cover change"),
       y = paste0("SAR backscatter change")) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(out, height=15, width=15, dpi=300)











#simple backscatter vs. volume change
name <- "2018-2008"
i <- file.path("E:\\ChangMap\\CHM\\DB_20210818\\DB_result",paste0(name,".csv"))
title <- paste0(name,"_HV_volume")
out_dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\figure"
out <- file.path(out_dir,paste0(title,".jpg"))
Data = read.csv(i,header=T)

Data <- Data[Data$V_2018 > 0 & Data$V_2008> 0,]
Data$ALS <- Data$V_2018 - Data$V_2008
Data$SAR <- Data$X2018_HV - Data$X2008_HV

Data <- Data[Data$ALS > -15000 & Data$ALS < 15000,]
Data <- Data[Data$SAR > -5 & Data$SAR < 5,]
Data$group_CC <- cut(Data$ALS,breaks = seq(-15000, 15000, 3000),dig.lab=1)

ggplot(Data, aes(x=ALS, y=SAR, group = group_CC))+
  geom_boxplot(fill="grey70", fatten = 10,color = "black",outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single")) +
  theme_bw()+
  scale_x_continuous(breaks = seq(-15000, 15000, 3000))+
  scale_y_continuous(breaks = seq(-5, 5, 1))+
  theme(text=element_text(size=20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  labs(title = title,
       x = paste0("LiDAR volume change"),
       y = paste0("SAR backscatter change")) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(out, height=15, width=15, dpi=300)


#simple backscatter vs. volume change
name <- "2018-2008"
i <- file.path("E:\\ChangMap\\CHM\\DB_20210818\\DB_result",paste0(name,".csv"))
title <- paste0(name,"_HV7_volume")
out_dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\figure"
out <- file.path(out_dir,paste0(title,".jpg"))
Data = read.csv(i,header=T)

Data <- Data[Data$V_2018 > 0 & Data$V_2008> 0,]
Data$ALS <- Data$V_2018 - Data$V_2008
Data$SAR <- Data$X2018_HV7 - Data$X2008_HV7

Data <- Data[Data$ALS > -15000 & Data$ALS < 15000,]
Data <- Data[Data$SAR > -5 & Data$SAR < 5,]
Data$group_CC <- cut(Data$ALS,breaks = seq(-15000, 15000, 3000),dig.lab=1)

ggplot(Data, aes(x=ALS, y=SAR, group = group_CC))+
  geom_boxplot(fill="grey70", fatten = 10,color = "black",outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single")) +
  theme_bw()+
  scale_x_continuous(breaks = seq(-15000, 15000, 3000))+
  scale_y_continuous(breaks = seq(-5, 5, 1))+
  theme(text=element_text(size=20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  labs(title = title,
       x = paste0("LiDAR volume change"),
       y = paste0("SAR backscatter change")) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(out, height=15, width=15, dpi=300)


#simple backscatter vs. volume change
name <- "2018-2010"
i <- file.path("E:\\ChangMap\\CHM\\DB_20210818\\DB_result",paste0(name,".csv"))
title <- paste0(name,"_HV_volume")
out_dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\figure"
out <- file.path(out_dir,paste0(title,".jpg"))
Data = read.csv(i,header=T)

Data <- Data[Data$V_2018 > 0 & Data$V_2010> 0,]
Data$ALS <- Data$V_2018 - Data$V_2010
Data$SAR <- Data$X2018_HV - Data$X2010_HV

Data <- Data[Data$ALS > -15000 & Data$ALS < 15000,]
Data <- Data[Data$SAR > -5 & Data$SAR < 5,]
Data$group_CC <- cut(Data$ALS,breaks = seq(-15000, 15000, 3000),dig.lab=1)

ggplot(Data, aes(x=ALS, y=SAR, group = group_CC))+
  geom_boxplot(fill="grey70", fatten = 10,color = "black",outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single")) +
  theme_bw()+
  scale_x_continuous(breaks = seq(-15000, 15000, 3000))+
  scale_y_continuous(breaks = seq(-5, 5, 1))+
  theme(text=element_text(size=20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  labs(title = title,
       x = paste0("LiDAR volume change"),
       y = paste0("SAR backscatter change")) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(out, height=15, width=15, dpi=300)


#simple backscatter vs. volume change
name <- "2010-2008"
i <- file.path("E:\\ChangMap\\CHM\\DB_20210818\\DB_result",paste0(name,".csv"))
title <- paste0(name,"_HV_volume")
out_dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\figure"
out <- file.path(out_dir,paste0(title,".jpg"))
Data = read.csv(i,header=T)

Data <- Data[Data$V_2010 > 0 & Data$V_2008> 0,]
Data$ALS <- Data$V_2010 - Data$V_2008
Data$SAR <- Data$X2010_HV - Data$X2008_HV

Data <- Data[Data$ALS > -15000 & Data$ALS < 15000,]
Data <- Data[Data$SAR > -5 & Data$SAR < 5,]
Data$group_CC <- cut(Data$ALS,breaks = seq(-15000, 15000, 3000),dig.lab=1)

ggplot(Data, aes(x=ALS, y=SAR, group = group_CC))+
  geom_boxplot(fill="grey70", fatten = 10,color = "black",outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single")) +
  theme_bw()+
  scale_x_continuous(breaks = seq(-15000, 15000, 3000))+
  scale_y_continuous(breaks = seq(-5, 5, 1))+
  theme(text=element_text(size=20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  labs(title = title,
       x = paste0("LiDAR volume change"),
       y = paste0("SAR backscatter change")) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(out, height=15, width=15, dpi=300)
