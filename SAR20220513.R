#Xiaoxuan Li #20220513
library(lidR)
library(tools) 
library(raster)
library(ggplot2)
library(ggpointdensity)
library(viridis)
library(ggcorrplot)
library(ggpmisc)
library(dplyr)
library(tidyverse) 
library(rsq)
library(readxl)
library(grid)
library(mltools)
library(Metrics)
library(devtools)
library(ggsignif)
library(igraph)
library(ff)
library(corrplot)
library(psych)
library(xlsx)
library(car)
library(MASS)
library(ggrepel)
library(rlist)
library(data.table)
library(lemon)
library(lmtest)
library(reshape2)
library(ggpubr)
library(tidyr)
library(stats)
library(igraph)
library(lattice)
library(latticeExtra)
library(caret)


#SAR change vs. ALS change table ---- Cover
# ------------------------------------------------------------------------------------------------ #
dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data <- na.omit(Data)
Data$pd <- Data$HH.x-Data$HH.y
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
i <- 1
rsqd <- 0
RMSE_ <- 0
for (i in 1:100){
  Data <- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
    arrange(as.numeric(gr))
  Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
  random_sample <- createDataPartition(Data$diff_C2_ALS_5, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  reg <- lm(diff_C2_ALS_5 ~ pd, data = training_dataset)
  training_dataset$preds <- predict(reg,newdata=training_dataset)
  rsqd[i] <- round(cor(training_dataset$diff_C2_ALS_5,training_dataset$preds)^2,3)
  RMSE_[i] <- sqrt(mean((training_dataset$preds - training_dataset$diff_C2_ALS_5)^2))}
R1_1<- mean(rsqd)
RMSE <- mean(RMSE_)


dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data <- na.omit(Data)
Data$pd <- Data$HH.x-Data$HH.y
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
i <- 1
rsqd <- 0
RMSE_ <- 0
for (i in 1:100){
  Data <- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
    arrange(as.numeric(gr))
  Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
  random_sample <- createDataPartition(Data$diff_C2_ALS_5, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  reg <- lm(diff_C2_ALS_5 ~ pd, data = training_dataset)
  training_dataset$preds <- predict(reg,newdata=training_dataset)
  rsqd[i] <- round(cor(training_dataset$diff_C2_ALS_5,training_dataset$preds)^2,3)
  RMSE_[i] <- sqrt(mean((training_dataset$preds - training_dataset$diff_C2_ALS_5)^2))}
R1_2<- mean(rsqd)
RMSE_2 <- mean(RMSE_)


dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data <- na.omit(Data)
Data$pd <- Data$HH.x-Data$HH.y
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
i <- 1
rsqd <- 0
RMSE_ <- 0
for (i in 1:100){
  Data <- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
    arrange(as.numeric(gr))
  Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
  random_sample <- createDataPartition(Data$diff_C2_ALS_5, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  reg <- lm(diff_C2_ALS_5 ~ pd, data = training_dataset)
  training_dataset$preds <- predict(reg,newdata=training_dataset)
  rsqd[i] <- round(cor(training_dataset$diff_C2_ALS_5,training_dataset$preds)^2,3)
  RMSE_[i] <- sqrt(mean((training_dataset$preds - training_dataset$diff_C2_ALS_5)^2))}
R1_3<- mean(rsqd)
RMSE_3 <- mean(RMSE_)



#SAR change vs. ALS change table ---- Volume
# ------------------------------------------------------------------------------------------------ #
dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$V2.x > 0 & Data$V2.y > 0,]
Data <- na.omit(Data)
Data$pd <- Data$HV.x-Data$HV.y
Data$diff_C2_ALS_5 = Data$V2.x - Data$V2.y
i <- 1
rsqd <- 0
RMSE_ <- 0
for (i in 1:100){
  Data <- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-24000, 24000, by=100))) %>% 
    arrange(as.numeric(gr))
  Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
  random_sample <- createDataPartition(Data$diff_C2_ALS_5, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  reg <- lm(diff_C2_ALS_5 ~ pd, data = training_dataset)
  training_dataset$preds <- predict(reg,newdata=training_dataset)
  rsqd[i] <- round(cor(training_dataset$diff_C2_ALS_5,training_dataset$preds)^2,3)
  RMSE_[i] <- sqrt(mean((training_dataset$preds - training_dataset$diff_C2_ALS_5)^2))}
R1_1<- mean(rsqd)
RMSE <- mean(RMSE_)


dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$V2.x > 0 & Data$V2.y > 0,]
Data <- na.omit(Data)
Data$pd <- Data$HV.x-Data$HV.y
Data$diff_C2_ALS_5 = Data$V2.x - Data$V2.y
i <- 1
rsqd <- 0
RMSE_ <- 0
for (i in 1:100){
  Data <- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-24000, 24000, by=100))) %>% 
    arrange(as.numeric(gr))
  Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
  random_sample <- createDataPartition(Data$diff_C2_ALS_5, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  reg <- lm(diff_C2_ALS_5 ~ pd, data = training_dataset)
  training_dataset$preds <- predict(reg,newdata=training_dataset)
  rsqd[i] <- round(cor(training_dataset$diff_C2_ALS_5,training_dataset$preds)^2,3)
  RMSE_[i] <- sqrt(mean((training_dataset$preds - training_dataset$diff_C2_ALS_5)^2))}
R1_2<- mean(rsqd)
RMSE_2 <- mean(RMSE_)


dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$V2.x > 0 & Data$V2.y > 0,]
Data <- na.omit(Data)
Data$pd <- Data$HV.x-Data$HV.y
Data$diff_C2_ALS_5 = Data$V2.x - Data$V2.y
i <- 1
rsqd <- 0
RMSE_ <- 0
for (i in 1:100){
  Data <- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-24000, 24000, by=100))) %>% 
    arrange(as.numeric(gr))
  Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
  random_sample <- createDataPartition(Data$diff_C2_ALS_5, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  reg <- lm(diff_C2_ALS_5 ~ pd, data = training_dataset)
  training_dataset$preds <- predict(reg,newdata=training_dataset)
  rsqd[i] <- round(cor(training_dataset$diff_C2_ALS_5,training_dataset$preds)^2,3)
  RMSE_[i] <- sqrt(mean((training_dataset$preds - training_dataset$diff_C2_ALS_5)^2))}
R1_3<- mean(rsqd)
RMSE_3 <- mean(RMSE_)



# ------------------------------------------------------------------------------------------------ #
#SAR/ALS cover change scatterplot
dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data$SAR <- Data$HV.x - Data$HV.y
Data$ALS <- Data$C2.x - Data$C2.y

random_sample <- createDataPartition(Data$ALS, p = 0.7, list = FALSE)
training_dataset  <- Data[random_sample, ]
testing_dataset <- Data[-random_sample, ]

df2 <- training_dataset[c("SAR","ALS")]
df3 <- df2 %>% 
  group_by(gr=cut(ALS, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
  arrange(as.numeric(gr))

i <- 1
rsqd <- 0
for (i in 1:100){
  new_df <- df3 %>% group_by(gr) %>% slice_sample(n=100)
  rsqd[i] <- cor(new_df$SAR,new_df$ALS)^2
}
R1_1<- mean(rsqd)


p2 <- ggplot(new_df, aes(x=ALS, y=SAR))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  labs(x = paste0("ALS cover change (2018-2008)"),
       y = paste0("SAR backscatter change (dB)(2018-2008)")) +
  theme(text=element_text(size=25)) +
  coord_cartesian(xlim = c(-0.8, 0.8),ylim = c(-7, 7))+
  scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  scale_y_continuous(minor_breaks = seq(-8, 8, 2),breaks = seq(-8, 8, 2))+
  geom_smooth(
    method="lm",
    formula = 'y ~ x', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "black")+
  annotate("text", x=-0.8, y=6, hjust = 0,color="black",size = 8,
           label= paste(expression(Linear~model~R^2),": ",round(R1_1,3)), parse=TRUE) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom",legend.key.width=unit(1.5,"cm"))


dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data$SAR <- Data$HV.x - Data$HV.y
Data$ALS <- Data$C2.x - Data$C2.y

random_sample <- createDataPartition(Data$ALS, p = 0.7, list = FALSE)
training_dataset  <- Data[random_sample, ]
testing_dataset <- Data[-random_sample, ]

df2 <- training_dataset[c("SAR","ALS")]
df3 <- df2 %>% 
  group_by(gr=cut(ALS, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
  arrange(as.numeric(gr))

i <- 1
rsqd <- 0
for (i in 1:100){
  new_df <- df3 %>% group_by(gr) %>% slice_sample(n=100)
  rsqd[i] <- cor(new_df$SAR,new_df$ALS)^2
}
R1_1<- mean(rsqd)

p3 <- ggplot(new_df, aes(x=ALS, y=SAR))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  labs(x = paste0("ALS cover change (2018-2010)"),
       y = paste0("SAR backscatter change (dB)(2018-2010)")) +
  theme(text=element_text(size=25)) +
  coord_cartesian(xlim = c(-0.8, 0.8),ylim = c(-7, 7))+
  scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  scale_y_continuous(minor_breaks = seq(-8, 8, 2),breaks = seq(-8, 8, 2))+
  geom_smooth(
    method="lm",
    formula = 'y ~ x', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "black")+
  annotate("text", x=-0.8, y=6, hjust = 0,color="black",size = 8,
           label= paste(expression(Linear~model~R^2),": ",round(R1_1,3)), parse=TRUE) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom",legend.key.width=unit(1.5,"cm"))




dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data$SAR <- Data$HV.x - Data$HV.y
Data$ALS <- Data$C2.x - Data$C2.y

random_sample <- createDataPartition(Data$ALS, p = 0.7, list = FALSE)
training_dataset  <- Data[random_sample, ]
testing_dataset <- Data[-random_sample, ]

df2 <- training_dataset[c("SAR","ALS")]
df3 <- df2 %>% 
  group_by(gr=cut(ALS, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
  arrange(as.numeric(gr))

i <- 1
rsqd <- 0
for (i in 1:100){
  new_df <- df3 %>% group_by(gr) %>% slice_sample(n=100)
  rsqd[i] <- cor(new_df$SAR,new_df$ALS)^2
}
R1_1<- mean(rsqd)

p1 <- ggplot(new_df, aes(x=ALS, y=SAR))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  labs(x = paste0("ALS cover change (2010-2008)"),
       y = paste0("SAR backscatter change (dB)(2010-2008)")) +
  theme(text=element_text(size=25)) +
  coord_cartesian(xlim = c(-0.8, 0.8),ylim = c(-7, 7))+
  scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  scale_y_continuous(minor_breaks = seq(-8, 8, 2),breaks = seq(-8, 8, 2))+
  geom_smooth(
    method="lm",
    formula = 'y ~ x', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "black")+
  annotate("text", x=-0.8, y=6, hjust = 0,color="black",size = 8,
           label= paste(expression(Linear~model~R^2),": ",round(R1_1,3)), parse=TRUE) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom",legend.key.width=unit(1.5,"cm"))



ggarrange(p1,p2,p3,ncol=3)
out = "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\composite thin 100 scatterplot.jpg"
ggsave(out,height=12, width=24, dpi=600)





#t-test boxplot + stats --100 subsample + 100 times
# ------------------------------------------------------------------------------------------------ #
dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data$SAR <- Data$HV.x - Data$HV.y
Data$ALS <- Data$C2.x - Data$C2.y

breakbin = c(-0.5,-0.25,-0.05,0.05,0.25,0.5)
Data$group <- cut(Data$ALS,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

Data$Type[Data$ALS >= 0.5] = ">0.5"
Data$Type[Data$ALS >= 0.25 & Data$ALS < 0.5] = "0.25~0.5"
Data$Type[Data$ALS >= 0.05 & Data$ALS < 0.25] = "0.05~0.25"
Data$Type[Data$ALS >= -0.05 & Data$ALS < 0.05] = "-0.05~0.05"
Data$Type[Data$ALS >= -0.25 & Data$ALS < -0.05] = "-0.25~-0.05"
Data$Type[Data$ALS >= -0.5 & Data$ALS < -0.25] = "-0.5~-0.25"
Data$Type[Data$ALS <= -0.5] = "-0.5<"
Data$Type<-factor(Data$Type, levels=c("-0.5<","-0.5~-0.25", "-0.25~-0.05","-0.05~0.05",
                                      "0.05~0.25","0.25~0.5",">0.5"))
aggregate(Data$SAR, by=list(Data$Type), FUN=length)

Data <- Data %>%
  group_by(Type) %>%
  do(sample_n(., 100, replace = FALSE))

mean_se <- Data %>%
  group_by(Type) %>%
  summarise(Means = mean(SAR), STD = sd(SAR))

r1 <- compare_means(
  SAR~Type, 
  Data, 
  method = "t.test", 
  p.adjust.method = "BH")
r2 <- r1[c("group1", "group2", "p.signif")]

G <- graph.data.frame(r2,directed=FALSE)
A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="p.signif",type='lower')
A

p2<- ggplot(Data, aes(x=group, y=SAR)) + 
  #geom_violin(trim=FALSE, fill="gray")+
  stat_summary(fun=mean, geom="point", size=4) + 
  stat_summary(fun.data=data_summary, geom = "errorbar", width=0.5)+
  labs(x = paste0("ALS cover change (2018-2008)"),
       y = paste0("SAR HV change (dB) (2018-2008)")) +
  theme_bw()+
  coord_cartesian(ylim = c(-5, 5))+
  scale_x_discrete(labels= c("(-0.5,-0.25)","(-0.25,-0.05)",
                             "(-0.05,0.05)","(0.05,0.25)","(0.25,0.5)"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle=15, vjust = 0.75, hjust=0.5))


dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data$SAR <- Data$HV.x - Data$HV.y
Data$ALS <- Data$C2.x - Data$C2.y

breakbin = c(-0.5,-0.25,-0.05,0.05,0.25,0.5)
Data$group <- cut(Data$ALS,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

Data$Type[Data$ALS >= 0.5] = ">0.5"
Data$Type[Data$ALS >= 0.25 & Data$ALS < 0.5] = "0.25~0.5"
Data$Type[Data$ALS >= 0.05 & Data$ALS < 0.25] = "0.05~0.25"
Data$Type[Data$ALS >= -0.05 & Data$ALS < 0.05] = "-0.05~0.05"
Data$Type[Data$ALS >= -0.25 & Data$ALS < -0.05] = "-0.25~-0.05"
Data$Type[Data$ALS >= -0.5 & Data$ALS < -0.25] = "-0.5~-0.25"
Data$Type[Data$ALS <= -0.5] = "-0.5<"
Data$Type<-factor(Data$Type, levels=c("-0.5<","-0.5~-0.25", "-0.25~-0.05","-0.05~0.05",
                                      "0.05~0.25","0.25~0.5",">0.5"))
aggregate(Data$SAR, by=list(Data$Type), FUN=length)
Data <- Data %>%
  group_by(Type) %>%
  do(sample_n(., 100, replace = FALSE))
mean_se <- Data %>%
  group_by(Type) %>%
  summarise(Means = mean(SAR), STD = sd(SAR))
mean_se
r1 <- compare_means(
  SAR~Type, 
  Data, 
  method = "t.test", 
  p.adjust.method = "BH")
r2 <- r1[c("group1", "group2", "p.signif")]

G <- graph.data.frame(r2,directed=FALSE)
A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="p.signif",type='lower')
A

p3<- ggplot(Data, aes(x=group, y=SAR)) + 
  #geom_violin(trim=FALSE, fill="gray")+
  stat_summary(fun=mean, geom="point", size=4) + 
  stat_summary(fun.data=data_summary, geom = "errorbar", width=0.5)+
  labs(x = paste0("ALS cover change (2018-2010)"),
       y = paste0("SAR HV change (dB) (2018-2010)")) +
  theme_bw()+
  coord_cartesian(ylim = c(-5, 5))+
  scale_x_discrete(labels= c("(-0.5,-0.25)","(-0.25,-0.05)",
                             "(-0.05,0.05)","(0.05,0.25)","(0.25,0.5)"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle=15, vjust = 0.75, hjust=0.5))



dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data$SAR <- Data$HV.x - Data$HV.y
Data$ALS <- Data$C2.x - Data$C2.y

breakbin = c(-0.5,-0.25,-0.05,0.05,0.25,0.5)
Data$group <- cut(Data$ALS,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

Data$Type[Data$ALS >= 0.5] = ">0.5"
Data$Type[Data$ALS >= 0.25 & Data$ALS < 0.5] = "0.25~0.5"
Data$Type[Data$ALS >= 0.05 & Data$ALS < 0.25] = "0.05~0.25"
Data$Type[Data$ALS >= -0.05 & Data$ALS < 0.05] = "-0.05~0.05"
Data$Type[Data$ALS >= -0.25 & Data$ALS < -0.05] = "-0.25~-0.05"
Data$Type[Data$ALS >= -0.5 & Data$ALS < -0.25] = "-0.5~-0.25"
Data$Type[Data$ALS <= -0.5] = "-0.5<"
Data$Type<-factor(Data$Type, levels=c("-0.5<","-0.5~-0.25", "-0.25~-0.05","-0.05~0.05",
                                      "0.05~0.25","0.25~0.5",">0.5"))
aggregate(Data$SAR, by=list(Data$Type), FUN=length)
Data <- Data %>%
  group_by(Type) %>%
  do(sample_n(., 100, replace = FALSE))
mean_se <- Data %>%
  group_by(Type) %>%
  summarise(Means = mean(SAR), STD = sd(SAR))
mean_se
r1 <- compare_means(
  SAR~Type, 
  Data, 
  method = "t.test", 
  p.adjust.method = "BH")
r2 <- r1[c("group1", "group2", "p.signif")]

G <- graph.data.frame(r2,directed=FALSE)
A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="p.signif",type='lower')
A

p1<- ggplot(Data, aes(x=group, y=SAR)) + 
  #geom_violin(trim=FALSE, fill="gray")+
  stat_summary(fun=mean, geom="point", size=4) + 
  stat_summary(fun.data=data_summary, geom = "errorbar", width=0.5)+
  labs(x = paste0("ALS cover change (2010-2008)"),
       y = paste0("SAR HV change (dB) (2010-2008)")) +
  theme_bw()+
  coord_cartesian(ylim = c(-5, 5))+
  scale_x_discrete(labels= c("(-0.5,-0.25)","(-0.25,-0.05)",
                             "(-0.05,0.05)","(0.05,0.25)","(0.25,0.5)"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle=15, vjust = 0.75, hjust=0.5))

ggarrange(p1,p2,p3,ncol=3)

out <- "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\t-test 100.jpg"
ggsave(out,height=9, width=18, dpi=600)









# ------------------------------------------------------------------------------------------------ #
#Cover-volume
dir <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")

Data = read.csv(dir,header=T)
Data <- Data[Data$C2 < 1 & Data$V2 > 0,]

R1_1 <- cor(Data$V2,Data$C2)^2

p1<- ggplot(Data, aes(x=V2, y=C2))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  labs(x = paste0("ALS Volume (2010)"),
       y = paste0("ALS Cover (2010)")) +
  theme(text=element_text(size=35)) +
  annotate("text", x=18000, y=0.25, hjust = 0,color="black",size = 15,
           label= paste(expression(Linear~model~R^2),": ",round(R1_1,3)), parse=TRUE)+
  theme(legend.position="none")

dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")

Data1 = read.csv(dir1,header=T)
Data1 <- Data1[Data1$C2 < 1 & Data1$V2 > 0,]

Data2 = read.csv(dir2,header=T)
Data2 <- Data2[Data2$C2 < 1 & Data2$V2 > 0,]

Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- na.omit(Data)

Data$diff_V2_ALS_5 <- Data$V2.x-Data$V2.y
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y

R1_1 <- cor(Data$diff_V2_ALS_5,Data$diff_C2_ALS_5)^2

p2<- ggplot(Data, aes(x=diff_V2_ALS_5, y=diff_C2_ALS_5))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  labs(x = paste0("ALS Volume Change (2018-2010)"),
       y = paste0("ALS Cover Change (2018-2010)")) +
  theme(text=element_text(size=35)) +
  annotate("text", x=0, y=-0.4, hjust = 0,color="black",size = 15,
           label= paste(expression(Linear~model~R^2),": ",round(R1_1,3)), parse=TRUE)+
  theme(legend.position="none")


ggarrange(p1,p2)
out = "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\scatterplot vol cov group 2018 2010.jpg"
ggsave(out,height=15, width=30, dpi=600)











# ------------------------------------------------------------------------------------------------ #
#composite plots
i_1 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
Data1 = read.csv(i_1,header=T)
Data1 <- Data1[Data1$V2 > 0,]
Data1 <- na.omit(Data1)
x1 <- Data1$V2
y1 <- Data1$HV

mult_nls_1 <- nls(y1 ~ a*exp(-c*x1)+b*(1-exp(-c*x1)), 
                  start = list(a=-25, b=-12, c=9e-5))
y_sim_1 <- coef(mult_nls_1)[1]*exp(-coef(mult_nls_1)[3]*x1)+
  coef(mult_nls_1)[2]*(1-exp(-coef(mult_nls_1)[3]*x1))
R2_1 <- round(cor(y1,y_sim_1)^2,3)

reg1_1 <- lm(y1 ~ x1 , data = Data1)
reg3_1 <- lm(y1 ~ log(x1), data = Data1)

p1<- ggplot(Data1, aes(x=x1, y=y1))+ 
  ylim(min(y1),max(y1))+
  geom_pointdensity()+
  scale_color_viridis(direction = 1)+
  geom_smooth(method = "nls", 
              method.args = list(formula = y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                                 start = list(a=-25, b=-12, c=9e-5)),
              size = 1.5, 
              linetype = "solid",
              colour = "black",se = F)+
  geom_smooth(
    method="lm",
    formula = 'y ~ x', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "purple")+
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "blue")+
  annotate("text", x=16000, y=-23, hjust = 0,color="black",size = 8,
           label= paste("\n",expression(WCM),": ",round(R2_1,3),"\n",
                        expression(Linear),": ",round(summary(reg1_1)$adj.r.squared,3),"\n",
                        expression(Log),": ",round(summary(reg3_1)$adj.r.squared,3)
           )) + 
  labs(x = paste0("LiDAR CHM volume (2008)"),
       y = paste0("SAR backscatter (2008)")) +
  theme_bw()+
  coord_cartesian(ylim = c(-27, -7))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=25)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")

i_2 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
Data2 = read.csv(i_2,header=T)
Data2 <- Data2[Data2$V2 > 0,]
Data2 <- na.omit(Data2)
x2 <- Data2$V2
y2 <- Data2$HV

mult_nls_2 <- nls(y2 ~ a*exp(-c*x2)+b*(1-exp(-c*x2)), 
                  start = list(a=-25, b=-12, c=9e-5))
y_sim_2 <- coef(mult_nls_2)[1]*exp(-coef(mult_nls_2)[3]*x2)+
  coef(mult_nls_2)[2]*(1-exp(-coef(mult_nls_2)[3]*x2))
R2_2 <- round(cor(y2,y_sim_2)^2,3)

reg1_2 <- lm(y2 ~ x2 , data = Data2)
reg3_2 <- lm(y2 ~ log(x2), data = Data2)

p2<- ggplot(Data2, aes(x=x2, y=y2))+ 
  ylim(min(y2),max(y2))+
  geom_pointdensity()+
  scale_color_viridis(direction = 1)+
  geom_smooth(method = "nls", 
              method.args = list(formula = y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                                 start = list(a=-25, b=-12, c=9e-5)),
              size = 1.5, 
              linetype = "solid",
              colour = "black",se = F)+
  geom_smooth(
    method="lm",
    formula = 'y ~ x', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "purple")+
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "blue")+
  annotate("text", x=16000, y=-23, hjust = 0,color="black",size = 8,
           label= paste("\n",expression(WCM),": ",round(R2_2,3),"\n",
                        expression(Linear),": ",round(summary(reg1_2)$adj.r.squared,3),"\n",
                        expression(Log),": ",round(summary(reg3_2)$adj.r.squared,3)
           )) + 
  labs(x = paste0("LiDAR CHM volume (2010)"),
       y = paste0("SAR backscatter (2010)")) +
  theme_bw()+
  coord_cartesian(ylim = c(-27, -7))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=25)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")


i_3 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
Data3 = read.csv(i_3,header=T)
Data3 <- Data3[Data3$V2 > 0,]
Data3 <- na.omit(Data3)
x3 <- Data3$V2
y3 <- Data3$HV

mult_nls_3 <- nls(y3 ~ a*exp(-c*x3)+b*(1-exp(-c*x3)), 
                  start = list(a=-25, b=-12, c=9e-5))
y_sim_3 <- coef(mult_nls_3)[1]*exp(-coef(mult_nls_3)[3]*x3)+
  coef(mult_nls_3)[2]*(1-exp(-coef(mult_nls_3)[3]*x3))
R2_3 <- round(cor(y3,y_sim_3)^2,3)

reg1_3 <- lm(y3 ~ x3 , data = Data3)
reg3_3 <- lm(y3 ~ log(x3), data = Data3)

p3<- ggplot(Data3, aes(x=x3, y=y3))+ 
  ylim(min(y3),max(y3))+
  geom_pointdensity()+
  scale_color_viridis(direction = 1)+
  geom_smooth(method = "nls", 
              method.args = list(formula = y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                                 start = list(a=-25, b=-12, c=9e-5)),
              size = 1.5, 
              linetype = "solid",
              colour = "black",se = F)+
  geom_smooth(
    method="lm",
    formula = 'y ~ x', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "purple")+
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "blue")+
  annotate("text", x=16000, y=-23, hjust = 0,color="black",size = 8,
           label= paste("\n",expression(WCM),": ",round(R2_3,3),"\n",
                        expression(Linear),": ",round(summary(reg1_3)$adj.r.squared,3),"\n",
                        expression(Log),": ",round(summary(reg3_3)$adj.r.squared,3)
           )) + 
  labs(x = paste0("LiDAR CHM volume (2018)"),
       y = paste0("SAR backscatter (2018)")) +
  theme_bw()+
  coord_cartesian(ylim = c(-27, -7))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=25)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")


ggarrange(p1,p2,p3,ncol=3,nrow=1) 
out = "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\figure scatterplot composite.jpg"
ggsave(out,height=9, width=18, dpi=600)





# ------------------------------------------------------------------------------------------------ #
#composite plots
i_1 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
Data1 = read.csv(i_1,header=T)
Data1 <- Data1[Data1$C2 > 0,]
Data1 <- na.omit(Data1)
x1 <- Data1$C2
y1 <- Data1$HV

mult_nls_1 <- nls(y1 ~ a*exp(-c*x1)+b*(1-exp(-c*x1)), 
                  start = list(a=-20, b=-7, c=2))
y_sim_1 <- coef(mult_nls_1)[1]*exp(-coef(mult_nls_1)[3]*x1)+
  coef(mult_nls_1)[2]*(1-exp(-coef(mult_nls_1)[3]*x1))
R2_1 <- round(cor(y1,y_sim_1)^2,3)

reg1_1 <- lm(y1 ~ x1 , data = Data1)
reg3_1 <- lm(y1 ~ log(x1), data = Data1)

p1<- ggplot(Data1, aes(x=x1, y=y1))+ 
  ylim(min(y1),max(y1))+
  geom_pointdensity()+
  scale_color_viridis(direction = 1)+
  geom_smooth(method = "nls", 
              method.args = list(formula = y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                                 start = list(a=-20, b=-7, c=2)),
              size = 1.5, 
              linetype = "solid",
              colour = "black",se = F)+
  geom_smooth(
    method="lm",
    formula = 'y ~ x', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "purple")+
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "blue")+
  annotate("text", x=0.5, y=-23, hjust = 0,color="black",size = 8,
           label= paste("\n",expression(WCM),": ",round(R2_1,3),"\n",
                        expression(Linear),": ",round(summary(reg1_1)$adj.r.squared,3),"\n",
                        expression(Log),": ",round(summary(reg3_1)$adj.r.squared,3)
           )) + 
  labs(x = paste0("LiDAR CHM cover (2008)"),
       y = paste0("SAR backscatter (2008)")) +
  theme_bw()+
  coord_cartesian(ylim = c(-27, -7))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=25)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")

i_2 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
Data2 = read.csv(i_2,header=T)
Data2 <- Data2[Data2$C2 > 0,]
Data2 <- na.omit(Data2)
x2 <- Data2$C2
y2 <- Data2$HV

mult_nls_2 <- nls(y2 ~ a*exp(-c*x2)+b*(1-exp(-c*x2)), 
                  start = list(a=-20, b=-7, c=2))
y_sim_2 <- coef(mult_nls_2)[1]*exp(-coef(mult_nls_2)[3]*x2)+
  coef(mult_nls_2)[2]*(1-exp(-coef(mult_nls_2)[3]*x2))
R2_2 <- round(cor(y2,y_sim_2)^2,3)

reg1_2 <- lm(y2 ~ x2 , data = Data2)
reg3_2 <- lm(y2 ~ log(x2), data = Data2)

p2<- ggplot(Data2, aes(x=x2, y=y2))+ 
  ylim(min(y2),max(y2))+
  geom_pointdensity()+
  scale_color_viridis(direction = 1)+
  geom_smooth(method = "nls", 
              method.args = list(formula = y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                                 start = list(a=-20, b=-7, c=2)),
              size = 1.5, 
              linetype = "solid",
              colour = "black",se = F)+
  geom_smooth(
    method="lm",
    formula = 'y ~ x', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "purple")+
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "blue")+
  annotate("text", x=0.5, y=-23, hjust = 0,color="black",size = 8,
           label= paste("\n",expression(WCM),": ",round(R2_2,3),"\n",
                        expression(Linear),": ",round(summary(reg1_2)$adj.r.squared,3),"\n",
                        expression(Log),": ",round(summary(reg3_2)$adj.r.squared,3)
           )) + 
  labs(x = paste0("LiDAR CHM cover (2010)"),
       y = paste0("SAR backscatter (2010)")) +
  theme_bw()+
  coord_cartesian(ylim = c(-27, -7))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=25)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")


i_3 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
Data3 = read.csv(i_3,header=T)
Data3 <- Data3[Data3$C2 > 0,]
Data3 <- na.omit(Data3)
x3 <- Data3$C2
y3 <- Data3$HV

mult_nls_3 <- nls(y3 ~ a*exp(-c*x3)+b*(1-exp(-c*x3)), 
                  start = list(a=-20, b=-7, c=2))
y_sim_3 <- coef(mult_nls_3)[1]*exp(-coef(mult_nls_3)[3]*x3)+
  coef(mult_nls_3)[2]*(1-exp(-coef(mult_nls_3)[3]*x3))
R2_3 <- round(cor(y3,y_sim_3)^2,3)

reg1_3 <- lm(y3 ~ x3 , data = Data3)
reg3_3 <- lm(y3 ~ log(x3), data = Data3)

p3<- ggplot(Data3, aes(x=x3, y=y3))+ 
  ylim(min(y3),max(y3))+
  geom_pointdensity()+
  scale_color_viridis(direction = 1)+
  geom_smooth(method = "nls", 
              method.args = list(formula = y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                                 start = list(a=-20, b=-7, c=2)),
              size = 1.5, 
              linetype = "solid",
              colour = "black",se = F)+
  geom_smooth(
    method="lm",
    formula = 'y ~ x', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "purple")+
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "blue")+
  annotate("text", x=0.5, y=-23, hjust = 0,color="black",size = 8,
           label= paste("\n",expression(WCM),": ",round(R2_3,3),"\n",
                        expression(Linear),": ",round(summary(reg1_3)$adj.r.squared,3),"\n",
                        expression(Log),": ",round(summary(reg3_3)$adj.r.squared,3)
           )) + 
  labs(x = paste0("LiDAR CHM cover (2018)"),
       y = paste0("SAR backscatter (2018)")) +
  theme_bw()+
  coord_cartesian(ylim = c(-27, -7))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=25)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")


ggarrange(p1,p2,p3,ncol=3,nrow=1) 
out = "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\figure cover composite.jpg"
ggsave(out,height=9, width=18, dpi=600)










# ------------------------------------------------------------------------------------------------ #
#predict cover vs. ALS cover -- direct linear model
dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data <- na.omit(Data)
Data$pd <- Data$HV.x-Data$HV.y
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y

Data <- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
  arrange(as.numeric(gr))


i <- 1
rsqd <- 0
bias_ <- 0
RMSE_ <- 0
for (i in 1:100){

  Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
  
  random_sample <- createDataPartition(Data$diff_C2_ALS_5, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  #70
  reg <- lm(diff_C2_ALS_5 ~ pd, data = training_dataset)
  testing_dataset$preds <- predict(reg,newdata=testing_dataset)
  #30
  reg <- lm(diff_C2_ALS_5 ~ pd, data = training_dataset)
  training_dataset$preds <- predict(reg,newdata=training_dataset)
  rsqd[i] <- round(cor(training_dataset$diff_C2_ALS_5,training_dataset$preds)^2,3)
  bias_[i] <- mean(training_dataset$preds - training_dataset$diff_C2_ALS_5)
  RMSE_[i] <- sqrt(mean((training_dataset$preds - training_dataset$diff_C2_ALS_5)^2))
}
R1_1<- mean(rsqd)
bias <- mean(bias_)
RMSE <- mean(RMSE_)
dc_2010_2008 <- paste0(R1_1," ",bias," ",RMSE)

p1<- ggplot(testing_dataset, aes(x=round(diff_C2_ALS_5,3), y=round(preds,3)))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(axis.title.x=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  coord_cartesian(xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8))+
  scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  scale_y_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(rr.label), sep = "*\", \"*")),
               formula = y ~ x, rr.digits = 3, coef.digits = 2, size = 10,
               parse = TRUE)+
  scale_color_viridis(direction = 1)+
  labs(x = paste0("ALS cover change (2010-2008)"),
       y = paste0("Pred. cover change (Direct SAR change Linear)")) +
  theme(text=element_text(size=25)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.15,0.7),legend.key.width=unit(1.5,"cm"))+
  theme(legend.text=element_text(size=25))+
  theme(legend.title=element_blank())

#boxplot -- direct model
testing_dataset$biases = testing_dataset$preds - testing_dataset$diff_C2_ALS_5
breakbin = round(seq(-0.4,0.4,0.2),2)
testing_dataset$group <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
testing_dataset <- testing_dataset[testing_dataset$biases > -0.6 & testing_dataset$biases < 0.6,]
testing_dataset <- testing_dataset[testing_dataset$diff_C2_ALS_5 > -0.6 & testing_dataset$diff_C2_ALS_5 < 0.6,]
d1<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=biases, group = group)) + 
  geom_boxplot(trim=FALSE)+
  labs(
    x = paste0("ALS cover change (2010-2008)"),
    y = paste0("Bias (2010-2008)")) +
  theme_bw()+
  coord_cartesian(xlim=c(-0.4, 0.4),ylim = c(-0.6, 0.6))+
  scale_y_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

testing_dataset$group_RH <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
a <- tapply(testing_dataset$biases, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(testing_dataset$diff_C2_ALS_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.4,0.2,0.2),digits = 1)
l <- cbind(x,RB,table(testing_dataset$group_RH))
df <- data.frame(l)
d4<- ggplot(df, aes(x=x+0.1, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(xlim=c(-0.4, 0.4),ylim = c(-100, 100))+
  scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
  scale_x_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  labs(x="ALS cover change (2010-2008)", 
       y="%Bias (%)  (2010-2008)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))



dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data <- na.omit(Data)
Data$pd <- Data$HV.x-Data$HV.y
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y

Data <- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
  arrange(as.numeric(gr))

i <- 1
rsqd <- 0
bias_ <- 0
RMSE_ <- 0
for (i in 1:100){

  Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
  
  random_sample <- createDataPartition(Data$diff_C2_ALS_5, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  #70
  reg <- lm(diff_C2_ALS_5 ~ pd, data = training_dataset)
  testing_dataset$preds <- predict(reg,newdata=testing_dataset)
  #30
  reg <- lm(diff_C2_ALS_5 ~ pd, data = training_dataset)
  training_dataset$preds <- predict(reg,newdata=training_dataset)
  rsqd[i] <- round(cor(training_dataset$diff_C2_ALS_5,training_dataset$preds)^2,3)
  bias_[i] <- mean(training_dataset$preds - training_dataset$diff_C2_ALS_5)
  RMSE_[i] <- sqrt(mean((training_dataset$preds - training_dataset$diff_C2_ALS_5)^2))
}
R1_1<- mean(rsqd)
bias <- mean(bias_)
RMSE <- mean(RMSE_)
dc_2018_2008 <- paste0(R1_1," ",bias," ",RMSE)

p2<- ggplot(testing_dataset, aes(x=round(diff_C2_ALS_5,3), y=round(preds,3)))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(axis.title.x=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  scale_y_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(rr.label), sep = "*\", \"*")),
               formula = y ~ x, rr.digits = 3, coef.digits = 2, size = 10,
               parse = TRUE)+
  scale_color_viridis(direction = 1)+
  labs(x = paste0("ALS cover change (2018-2008)"),
       y = paste0("Pred. cover change (Direct SAR change Linear)")) +
  theme(text=element_text(size=25)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.15,0.7),legend.key.width=unit(1.5,"cm"))+
  theme(legend.text=element_text(size=25))+
  theme(legend.title=element_blank())

#boxplot -- direct model
testing_dataset$biases = testing_dataset$preds - testing_dataset$diff_C2_ALS_5
breakbin = round(seq(-0.6,0.6,0.2),2)
testing_dataset$group <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
testing_dataset <- testing_dataset[testing_dataset$biases > -0.6 & testing_dataset$biases < 0.6,]
testing_dataset <- testing_dataset[testing_dataset$diff_C2_ALS_5 > -0.6 & testing_dataset$diff_C2_ALS_5 < 0.6,]
d2<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=biases, group = group)) + 
  geom_boxplot(trim=FALSE)+
  labs(
    x = paste0("ALS cover change (2018-2008)"),
    y = paste0("Bias (2018-2008)")) +
  theme_bw()+
  coord_cartesian(xlim=c(-0.6, 0.6),ylim = c(-0.6, 0.6))+
  scale_y_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))
  

testing_dataset$group_RH <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
a <- tapply(testing_dataset$biases, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(testing_dataset$diff_C2_ALS_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(testing_dataset$group_RH))
df <- data.frame(l)
d5<- ggplot(df, aes(x=x+0.1, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(xlim=c(-0.6, 0.6),ylim = c(-100, 100))+
  scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
  scale_x_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  labs(x="ALS cover change (2018-2008)", 
       y="%Bias (%)  (2018-2008)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))


dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data <- na.omit(Data)
Data$pd <- Data$HV.x-Data$HV.y
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y

Data <- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
  arrange(as.numeric(gr))

i <- 1
rsqd <- 0
bias_ <- 0
RMSE_ <- 0
for (i in 1:100){

  Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
  
  random_sample <- createDataPartition(Data$diff_C2_ALS_5, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  
  #70
  reg <- lm(diff_C2_ALS_5 ~ pd, data = training_dataset)
  testing_dataset$preds <- predict(reg,newdata=testing_dataset)
  #30
  reg <- lm(diff_C2_ALS_5 ~ pd, data = training_dataset)
  training_dataset$preds <- predict(reg,newdata=training_dataset)
  rsqd[i] <- round(cor(training_dataset$diff_C2_ALS_5,training_dataset$preds)^2,3)
  bias_[i] <- mean(training_dataset$preds - training_dataset$diff_C2_ALS_5)
  RMSE_[i] <- sqrt(mean((training_dataset$preds - training_dataset$diff_C2_ALS_5)^2))
}
R1_1<- mean(rsqd)
bias <- mean(bias_)
RMSE <- mean(RMSE_)
dc_2018_2010 <- paste0(R1_1," ",bias," ",RMSE)

p3<- ggplot(testing_dataset, aes(x=round(diff_C2_ALS_5,3), y=round(preds,3)))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  coord_cartesian(xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8))+
  scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  scale_y_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(rr.label), sep = "*\", \"*")),
               formula = y ~ x, rr.digits = 3, coef.digits = 2, size = 10,
               parse = TRUE)+
  scale_color_viridis(direction = 1)+
  labs(x = paste0("ALS cover change (2018-2010)"),
       y = paste0("Pred. cover change (Linear)")) +
  theme(text=element_text(size=30)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.15,0.7),legend.key.width=unit(1.5,"cm"))+
  theme(legend.text=element_text(size=25))+
  theme(legend.title=element_blank())


#boxplot -- direct model
testing_dataset$biases = testing_dataset$preds - testing_dataset$diff_C2_ALS_5
breakbin = round(seq(-0.6,0.6,0.2),2)
testing_dataset$group <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
testing_dataset <- testing_dataset[testing_dataset$biases > -0.6 & testing_dataset$biases < 0.6,]
testing_dataset <- testing_dataset[testing_dataset$diff_C2_ALS_5 > -0.6 & testing_dataset$diff_C2_ALS_5 < 0.6,]
d3<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=biases, group = group)) + 
  geom_boxplot(trim=FALSE)+
  labs(
    x = paste0("ALS cover change (2018-2010)"),
    y = paste0("Bias (2018-2010)")) +
  theme_bw()+
  coord_cartesian(xlim=c(-0.6, 0.6),ylim = c(-0.6, 0.6))+
  scale_y_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

testing_dataset$group_RH <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
a <- tapply(testing_dataset$biases, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(testing_dataset$diff_C2_ALS_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(testing_dataset$group_RH))
df <- data.frame(l)
d6<- ggplot(df, aes(x=x+0.1, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(xlim=c(-0.6, 0.6),ylim = c(-100, 100))+
  scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
  scale_x_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  labs(x="ALS cover change (2018-2010)", 
       y="%Bias (%)  (2018-2010)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))




#BWCM model
dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2010.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$preds = (Data$P_C_5.x - Data$P_C_5.y)

Data<- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
  arrange(as.numeric(gr))

i <- 1
rsqd <- 0
bias_ <- 0
RMSE_ <- 0
for (i in 1:100){

  Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
  
  random_sample <- createDataPartition(Data$diff_C2_ALS_5, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  rsqd[i] <- round(cor(training_dataset$diff_C2_ALS_5,training_dataset$preds)^2,3)
  bias_[i] <- mean(training_dataset$preds - training_dataset$diff_C2_ALS_5)
  RMSE_[i] <- sqrt(mean((training_dataset$preds - training_dataset$diff_C2_ALS_5)^2))
}
R1_2<- mean(rsqd)
bias <- mean(bias_)
RMSE <- mean(RMSE_)
bc_2010_2008 <- paste0(R1_2," ",bias," ",RMSE)

p7<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=preds))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(axis.title.x=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  coord_cartesian(xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8))+
  scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  scale_y_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(rr.label), sep = "*\", \"*")),
               formula = y ~ x, rr.digits = 3, coef.digits = 2, size = 10,
               parse = TRUE)+
  scale_color_viridis(direction = 1)+
  labs(x = paste0("ALS cover change (2010-2008)"),
       y = paste0("Pred. cover change (BWCM)")) +
  theme(text=element_text(size=30)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.15,0.7),legend.key.width=unit(1.5,"cm"))+
  theme(legend.text=element_text(size=25))+
  theme(legend.title=element_blank())



#boxplot -- BWCM model
testing_dataset$biases = testing_dataset$preds - testing_dataset$diff_C2_ALS_5
testing_dataset <- testing_dataset[testing_dataset$diff_C2_ALS_5 > -0.6 & testing_dataset$C2.x < 0.6,]
testing_dataset <- testing_dataset[testing_dataset$diff_C2_ALS_5 > -0.6 & testing_dataset$diff_C2_ALS_5 < 0.6,]
breakbin = round(seq(-0.4,0.4,0.2),2)
testing_dataset$group <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
testing_dataset <- testing_dataset[testing_dataset$biases > -0.6 & testing_dataset$biases < 0.6,]
b1<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=biases, group = group)) + 
  geom_boxplot(trim=FALSE)+
  labs(
    x = paste0("ALS cover change (2010-2008)"),
    y = paste0("Bias (2010-2008)")) +
  theme_bw()+
  coord_cartesian(xlim=c(-0.4, 0.4),ylim = c(-0.4, 0.4))+
  scale_y_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

testing_dataset$group_RH <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
a <- tapply(testing_dataset$biases, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(testing_dataset$diff_C2_ALS_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.4,0.2,0.2),digits = 1)
l <- cbind(x,RB,table(testing_dataset$group_RH))
df <- data.frame(l)
b4<- ggplot(df, aes(x=x+0.1, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(xlim=c(-0.4, 0.4),ylim = c(-100, 100))+
  scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
  scale_x_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  labs(x="ALS cover change (2010-2008)", 
       y="%Bias (%)  (2010-2008)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))



dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$preds = (Data$P_C_5.x - Data$P_C_5.y)

Data<- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
  arrange(as.numeric(gr))


i <- 1
rsqd <- 0
bias_ <- 0
RMSE_ <- 0
for (i in 1:100){
  Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
  
  random_sample <- createDataPartition(Data$diff_C2_ALS_5, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  rsqd[i] <- round(cor(training_dataset$diff_C2_ALS_5,training_dataset$preds)^2,3)
  bias_[i] <- mean(training_dataset$preds - training_dataset$diff_C2_ALS_5)
  RMSE_[i] <- sqrt(mean((training_dataset$preds - training_dataset$diff_C2_ALS_5)^2))
}
R1_2<- mean(rsqd)
bias <- mean(bias_)
RMSE <- mean(RMSE_)
bc_2018_2008 <- paste0(R1_2," ",bias," ",RMSE)

p8<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=preds))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  coord_cartesian(xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8))+
  scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  scale_y_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(rr.label), sep = "*\", \"*")),
               formula = y ~ x, rr.digits = 3, coef.digits = 2, size = 10,
               parse = TRUE)+
  scale_color_viridis(direction = 1)+
  labs(x = paste0("ALS cover change (2018-2008)"),
       y = paste0("Pred. cover change (BWCM)")) +
  theme(text=element_text(size=30)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.15,0.7),legend.key.width=unit(1.5,"cm"))+
  theme(legend.text=element_text(size=25))+
  theme(legend.title=element_blank())


#boxplot -- BWCM model
testing_dataset$biases = testing_dataset$preds - testing_dataset$diff_C2_ALS_5
testing_dataset <- testing_dataset[testing_dataset$diff_C2_ALS_5 > -0.6 & testing_dataset$C2.x < 0.6,]
testing_dataset <- testing_dataset[testing_dataset$diff_C2_ALS_5 > -0.6 & testing_dataset$diff_C2_ALS_5 < 0.6,]
breakbin = round(seq(-0.6,0.6,0.2),2)
testing_dataset$group <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
testing_dataset <- testing_dataset[testing_dataset$biases > -0.6 & testing_dataset$biases < 0.6,]
b2<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=biases, group = group)) + 
  geom_boxplot(trim=FALSE)+
  labs(
    x = paste0("ALS cover change (2018-2008)"),
    y = paste0("Bias (2018-2008)")) +
  theme_bw()+
  coord_cartesian(xlim=c(-0.6, 0.6),ylim = c(-0.6, 0.6))+
  scale_y_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

testing_dataset$group_RH <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
a <- tapply(testing_dataset$biases, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(testing_dataset$diff_C2_ALS_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(testing_dataset$group_RH))
df <- data.frame(l)
b5<- ggplot(df, aes(x=x+0.1, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(xlim=c(-0.6, 0.6),ylim = c(-100, 100))+
  scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
  scale_x_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  labs(x="ALS cover change (2018-2008)", 
       y="%Bias (%)  (2018-2008)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))




dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2010.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$preds = (Data$P_C_5.x - Data$P_C_5.y)

Data<- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
  arrange(as.numeric(gr))


i <- 1
rsqd <- 0
bias_ <- 0
RMSE_ <- 0
for (i in 1:100){
  Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
  
  random_sample <- createDataPartition(Data$diff_C2_ALS_5, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  rsqd[i] <- round(cor(training_dataset$diff_C2_ALS_5,training_dataset$preds)^2,3)
  bias_[i] <- mean(training_dataset$preds - training_dataset$diff_C2_ALS_5)
  RMSE_[i] <- sqrt(mean((training_dataset$preds - training_dataset$diff_C2_ALS_5)^2))
}
R1_2<- mean(rsqd)
bias <- mean(bias_)
RMSE <- mean(RMSE_)
bc_2018_2010 <- paste0(R1_2," ",bias," ",RMSE)

p9<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=preds))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  coord_cartesian(xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8))+
  scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  scale_y_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(rr.label), sep = "*\", \"*")),
               formula = y ~ x, rr.digits = 3, coef.digits = 1, size = 10,
               parse = TRUE)+
  scale_color_viridis(direction = 1)+
  labs(x = paste0("ALS cover change (2018-2010)"),
       y = paste0("Pred. cover change (BWCM)")) +
  theme(text=element_text(size=30)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.15,0.7),legend.key.width=unit(1.5,"cm"))+
  theme(legend.text=element_text(size=25))+
  theme(legend.title=element_blank())

#boxplot -- BWCM model
testing_dataset$biases = testing_dataset$preds - testing_dataset$diff_C2_ALS_5
testing_dataset <- testing_dataset[testing_dataset$diff_C2_ALS_5 > -0.6 & testing_dataset$C2.x < 0.6,]
testing_dataset <- testing_dataset[testing_dataset$diff_C2_ALS_5 > -0.6 & testing_dataset$diff_C2_ALS_5 < 0.6,]
breakbin = round(seq(-0.6,0.6,0.2),2)
testing_dataset$group <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
testing_dataset <- testing_dataset[testing_dataset$biases > -0.6 & testing_dataset$biases < 0.6,]
b3<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=biases, group = group)) + 
  geom_boxplot(trim=FALSE)+
  labs(
    x = paste0("ALS cover change (2018-2010)"),
    y = paste0("Bias (2018-2010)")) +
  theme_bw()+
  coord_cartesian(ylim = c(-0.6, 0.6),xlim = c(-0.6, 0.6))+
  scale_y_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

testing_dataset$group_RH <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
a <- tapply(testing_dataset$biases, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(testing_dataset$diff_C2_ALS_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(testing_dataset$group_RH))
df <- data.frame(l)
b6<- ggplot(df, aes(x=x+0.1, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-100, 100),xlim = c(-0.6, 0.6))+
  scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
  scale_x_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  labs(x="ALS cover change (2018-2010)", 
       y="%Bias (%)  (2018-2010)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))



#predict cover vs. ALS cover -- log model
dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data <- na.omit(Data)
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y

Data<- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
  arrange(as.numeric(gr))

i <- 1
rsqd <- 0
bias_ <- 0
RMSE_ <- 0
for (i in 1:100){
  Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
  
  random_sample <- createDataPartition(Data$diff_C2_ALS_5, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  
  #70
  x1 <- training_dataset$C2.x
  y1 <- training_dataset$HV.x
  mult_nls <- nls(x1~ a*exp(b*y1), start = list(a=20, b=0.2))
  pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*training_dataset$HV.x)
  x2 <- training_dataset$C2.y
  y2 <- training_dataset$HV.y
  mult_nls <- nls(x2~ a*exp(b*y2), start = list(a=20, b=0.2))
  pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*training_dataset$HV.y)
  training_dataset$preds <- pred.x - pred.y
  #30
  x1 <- training_dataset$C2.x
  y1 <- training_dataset$HV.x
  mult_nls <- nls(x1~ a*exp(b*y1), start = list(a=20, b=0.2))
  pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*testing_dataset$HV.x)
  x2 <- training_dataset$C2.y
  y2 <- training_dataset$HV.y
  mult_nls <- nls(x2~ a*exp(b*y2), start = list(a=20, b=0.2))
  pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*testing_dataset$HV.y)
  testing_dataset$preds <- pred.x - pred.y
  
  rsqd[i] <- round(cor(training_dataset$diff_C2_ALS_5,training_dataset$preds)^2,3)
  bias_[i] <- mean(training_dataset$preds - training_dataset$diff_C2_ALS_5)
  RMSE_[i] <- sqrt(mean((training_dataset$preds - training_dataset$diff_C2_ALS_5)^2))
}
R1_2<- mean(rsqd)
bias <- mean(bias_)
RMSE <- mean(RMSE_)
lc_2010_2008 <- paste0(R1_2," ",bias," ",RMSE)

p10<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=preds))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  coord_cartesian(xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8))+
  scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  scale_y_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(rr.label), sep = "*\", \"*")),
               formula = y ~ x, rr.digits = 3, coef.digits = 2, size = 10,
               parse = TRUE)+
  scale_color_viridis(direction = 1)+
  labs(x = paste0("ALS cover change (2010-2008)"),
       y = paste0("Pred. cover change (Log)")) +
  theme(text=element_text(size=30)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.15,0.7),legend.key.width=unit(1.5,"cm"))+
  theme(legend.text=element_text(size=25))+
  theme(legend.title=element_blank())


#boxplot -- log model
testing_dataset$biases = testing_dataset$preds - testing_dataset$diff_C2_ALS_5
breakbin = round(seq(-0.4,0.4,0.2),2)
testing_dataset$group <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
testing_dataset <- testing_dataset[testing_dataset$biases > -0.6 & testing_dataset$biases < 0.6,]
testing_dataset <- testing_dataset[testing_dataset$diff_C2_ALS_5 > -0.6 & testing_dataset$diff_C2_ALS_5 < 0.6,]
l1<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=biases, group = group)) + 
  geom_boxplot(trim=FALSE)+
  labs(
    x = paste0("ALS cover change (2010-2008)"),
    y = paste0("Bias (2010-2008)")) +
  theme_bw()+
  coord_cartesian(xlim=c(-0.4, 0.4),ylim = c(-0.6, 0.6))+
  scale_y_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

testing_dataset$group_RH <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
a <- tapply(testing_dataset$biases, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(testing_dataset$diff_C2_ALS_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.4,0.2,0.2),digits = 1)
l <- cbind(x,RB,table(testing_dataset$group_RH))
df <- data.frame(l)
l4<- ggplot(df, aes(x=x+0.1, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(xlim=c(-0.4, 0.4),ylim = c(-100, 100))+
  scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
  scale_x_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  labs(x="ALS cover change (2010-2008)", 
       y="%Bias (%)  (2010-2008)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(plot.title = element_text(hjust = 0.5))




dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data <- na.omit(Data)
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y

Data<- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
  arrange(as.numeric(gr))


i <- 1
rsqd <- 0
bias_ <- 0
RMSE_ <- 0
for (i in 1:100){
  Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
  
  random_sample <- createDataPartition(Data$diff_C2_ALS_5, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  #70
  x1 <- training_dataset$C2.x
  y1 <- training_dataset$HV.x
  mult_nls <- nls(x1~ a*exp(b*y1), start = list(a=20, b=0.2))
  pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*training_dataset$HV.x)
  x2 <- training_dataset$C2.y
  y2 <- training_dataset$HV.y
  mult_nls <- nls(x2~ a*exp(b*y2), start = list(a=20, b=0.2))
  pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*training_dataset$HV.y)
  training_dataset$preds <- pred.x - pred.y
  #30
  x1 <- training_dataset$C2.x
  y1 <- training_dataset$HV.x
  mult_nls <- nls(x1~ a*exp(b*y1), start = list(a=20, b=0.2))
  pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*testing_dataset$HV.x)
  x2 <- training_dataset$C2.y
  y2 <- training_dataset$HV.y
  mult_nls <- nls(x2~ a*exp(b*y2), start = list(a=20, b=0.2))
  pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*testing_dataset$HV.y)
  testing_dataset$preds <- pred.x - pred.y
  
  rsqd[i] <- round(cor(training_dataset$diff_C2_ALS_5,training_dataset$preds)^2,3)
  bias_[i] <- mean(training_dataset$preds - training_dataset$diff_C2_ALS_5)
  RMSE_[i] <- sqrt(mean((training_dataset$preds - training_dataset$diff_C2_ALS_5)^2))
}

R1_2<- mean(rsqd)
bias <- mean(bias_)
RMSE <- mean(RMSE_)
lc_2018_2008 <- paste0(R1_2," ",bias," ",RMSE)

p11<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=preds))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(axis.title.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  coord_cartesian(xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8))+
  scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  scale_y_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(rr.label), sep = "*\", \"*")),
               formula = y ~ x, rr.digits = 3, coef.digits = 2, size = 10,
               parse = TRUE)+
  scale_color_viridis(direction = 1)+
  labs(x = paste0("ALS cover change (2018-2008)"),
       y = paste0("Pred. cover change (Log)")) +
  theme(text=element_text(size=30)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.15,0.7),legend.key.width=unit(1.5,"cm"))+
  theme(legend.text=element_text(size=25))+
  theme(legend.title=element_blank())


#boxplot -- log model
testing_dataset$biases = testing_dataset$preds - testing_dataset$diff_C2_ALS_5
breakbin = round(seq(-0.6,0.6,0.2),2)
testing_dataset$group <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
testing_dataset <- testing_dataset[testing_dataset$biases > -0.6 & testing_dataset$biases < 0.6,]
testing_dataset <- testing_dataset[testing_dataset$diff_C2_ALS_5 > -0.6 & testing_dataset$diff_C2_ALS_5 < 0.6,]
l2<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=biases, group = group)) + 
  geom_boxplot(trim=FALSE)+
  labs(
    x = paste0("ALS cover change (2018-2008)"),
    y = paste0("Bias (2018-2008)")) +
  theme_bw()+
  coord_cartesian(xlim=c(-0.6, 0.6),ylim = c(-0.6, 0.6))+
  scale_y_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

testing_dataset$group_RH <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
a <- tapply(testing_dataset$biases, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(testing_dataset$diff_C2_ALS_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(testing_dataset$group_RH))
df <- data.frame(l)
l5<- ggplot(df, aes(x=x+0.1, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(xlim=c(-0.6, 0.6),ylim = c(-100, 100))+
  scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
  scale_x_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  labs(x="ALS cover change (2018-2008)", 
       y="%Bias (%)  (2018-2008)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(plot.title = element_text(hjust = 0.5))




dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data <- na.omit(Data)
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y

Data<- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
  arrange(as.numeric(gr))

i <- 1
rsqd <- 0
bias_ <- 0
RMSE_ <- 0
for (i in 1:100){
  Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
  
  random_sample <- createDataPartition(Data$diff_C2_ALS_5, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  
  #70
  x1 <- training_dataset$C2.x
  y1 <- training_dataset$HV.x
  mult_nls <- nls(x1~ a*exp(b*y1), start = list(a=20, b=0.2))
  pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*training_dataset$HV.x)
  x2 <- training_dataset$C2.y
  y2 <- training_dataset$HV.y
  mult_nls <- nls(x2~ a*exp(b*y2), start = list(a=20, b=0.2))
  pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*training_dataset$HV.y)
  training_dataset$preds <- pred.x - pred.y
  #30
  x1 <- training_dataset$C2.x
  y1 <- training_dataset$HV.x
  mult_nls <- nls(x1~ a*exp(b*y1), start = list(a=20, b=0.2))
  pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*testing_dataset$HV.x)
  x2 <- training_dataset$C2.y
  y2 <- training_dataset$HV.y
  mult_nls <- nls(x2~ a*exp(b*y2), start = list(a=20, b=0.2))
  pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*testing_dataset$HV.y)
  testing_dataset$preds <- pred.x - pred.y
  
  rsqd[i] <- round(cor(training_dataset$diff_C2_ALS_5,training_dataset$preds)^2,3)
  bias_[i] <- mean(training_dataset$preds - training_dataset$diff_C2_ALS_5)
  RMSE_[i] <- sqrt(mean((training_dataset$preds - training_dataset$diff_C2_ALS_5)^2))
}
R1_2<- mean(rsqd)
bias <- mean(bias_)
RMSE <- mean(RMSE_)
lc_2018_2010 <- paste0(R1_2," ",bias," ",RMSE)

p12<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=preds))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(axis.title.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  coord_cartesian(xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8))+
  scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  scale_y_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(rr.label), sep = "*\", \"*")),
               formula = y ~ x, rr.digits = 3, coef.digits = 2, size = 10,
               parse = TRUE)+
  scale_color_viridis(direction = 1)+
  labs(x = paste0("ALS cover change (2018-2010)"),
       y = paste0("Pred. cover change (Log)")) +
  theme(text=element_text(size=30)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position=c(0.15,0.7),legend.key.width=unit(1.5,"cm"))+
  theme(legend.text=element_text(size=25))+
  theme(legend.title=element_blank())

#boxplot -- log model
testing_dataset$biases = testing_dataset$preds - testing_dataset$diff_C2_ALS_5
breakbin = round(seq(-0.6,0.6,0.2),2)
testing_dataset$group <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
testing_dataset <- testing_dataset[testing_dataset$biases > -0.6 & testing_dataset$biases < 0.6,]
testing_dataset <- testing_dataset[testing_dataset$diff_C2_ALS_5 > -0.6 & testing_dataset$diff_C2_ALS_5 < 0.6,]
l3<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=biases, group = group)) + 
  geom_boxplot(trim=FALSE)+
  labs(
    x = paste0("ALS cover change (2018-2010)"),
    y = paste0("Bias (2018-2010)")) +
  theme_bw()+
  coord_cartesian(xlim=c(-0.6, 0.6),ylim = c(-0.6, 0.6))+
  scale_y_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

testing_dataset$group_RH <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
a <- tapply(testing_dataset$biases, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(testing_dataset$diff_C2_ALS_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(testing_dataset$group_RH))
df <- data.frame(l)
l6<- ggplot(df, aes(x=x+0.1, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(xlim=c(-0.6, 0.6),ylim = c(-100, 100))+
  scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
  scale_x_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  labs(x="ALS cover change (2018-2010)", 
       y="%Bias (%)  (2018-2010)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(plot.title = element_text(hjust = 0.5))





ggarrange(p1,p2,p3,p7,p8,p9,p10,p11,p12,nrow=3,ncol=3)
out = "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\inverted cover change 100 scatterplot.jpg"
ggsave(out,height=24, width=24, dpi=600)


q1<- ggarrange(d1,d2,d3,d4,d5,d6,ncol=3,nrow=2)

q2<- ggarrange(b1,b2,b3,b4,b5,b6,ncol=3,nrow=2)

q3<-ggarrange(l1,l2,l3,l4,l5,l6,ncol=3,nrow=2)

q1 <- annotate_figure(q1, top = text_grob("Direct Backscatter Change Model",size = 25))
q2 <- annotate_figure(q2, top = text_grob("Bayesian Water Cloud Model",size = 25))
q3 <- annotate_figure(q3, top = text_grob("Logarithm Model",size = 25))

ggarrange(q1,q2,q3,nrow=3)
out = "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\change cover composite boxplot.jpg"
ggsave(out,height=30, width=25, dpi=600)







# ------------------------------------------------------------------------------------------------ #
#SAR/ALS cover change -- db change scatterplot
dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data$SAR <- Data$HV.x - Data$HV.y
Data$ALS <- round(Data$C2.x - Data$C2.y,3)

random_sample <- createDataPartition(Data$ALS, p = 0.7, list = FALSE)
training_dataset  <- Data[random_sample, ]
testing_dataset <- Data[-random_sample, ]

df2 <- training_dataset[c("SAR","ALS")]
df3 <- df2 %>% 
  group_by(gr=cut(ALS, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
  arrange(as.numeric(gr))


new_df <- df3 %>% group_by(gr) %>% slice_sample(n=100)


mult_nls <- nls(ALS ~ a*SAR, start = list(a=0.08),data=new_df)
y_sim <- coef(mult_nls)[1]*new_df$SAR
R2_1 <- round(cor(new_df$ALS,y_sim)^2,3)

l <- lm(ALS~SAR,data=new_df)


p2 <- ggplot(new_df, aes(x=SAR, y=ALS))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  labs(y = paste0("ALS cover change (2018-2008)"),
       x = paste0("SAR backscatter change (dB)(2018-2008)")) +
  theme(text=element_text(size=35)) +
  geom_abline(intercept = 0, slope = coef(mult_nls)[1],color="black", linetype="solid", size=1.5)+
  geom_segment(aes(x = -7, y = 0, xend = 7, yend = 0),color="blue")+
  geom_segment(aes(x = 0, y = -0.8, xend = 0, yend = 0.8),color="blue")+
  coord_cartesian(ylim = c(-0.8, 0.8),xlim = c(-7, 7))+
  scale_y_continuous(minor_breaks = seq(-0.8,0.8,0.2),breaks = seq(-0.8,0.8,0.2),
                     labels = function(x) sprintf("%.1f", x))+
  scale_x_continuous(minor_breaks = seq(-7, 7, 2),breaks = seq(-7, 7, 2))+
  annotate("text", x=-7, y=0.4, hjust = 0,color="black",size = 9,
           label= paste(expression(R^2),": ",round(R2_1,3)), parse=TRUE) + 
  annotate("text", x=-7, y=0.3, hjust = 0,color="black",size = 9,
           label= paste(expression(Slope),": ",round(coef(mult_nls)[1],3)), parse=TRUE) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")


dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data$SAR <- Data$HV.x - Data$HV.y
Data$ALS <- Data$C2.x - Data$C2.y

random_sample <- createDataPartition(Data$ALS, p = 0.7, list = FALSE)
training_dataset  <- Data[random_sample, ]
testing_dataset <- Data[-random_sample, ]

df2 <- training_dataset[c("SAR","ALS")]
df3 <- df2 %>% 
  group_by(gr=cut(ALS, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
  arrange(as.numeric(gr))


new_df <- df3 %>% group_by(gr) %>% slice_sample(n=100)


mult_nls <- nls(ALS ~ a*SAR, start = list(a=0.08),data=new_df)
y_sim <- coef(mult_nls)[1]*new_df$SAR
R2_1 <- round(cor(new_df$ALS,y_sim)^2,3)

l <- lm(ALS~SAR,data=new_df)


p3 <- ggplot(new_df, aes(x=SAR, y=ALS))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  labs(y = paste0("ALS cover change (2018-2010)"),
       x = paste0("SAR backscatter change (dB)(2018-2010)")) +
  theme(text=element_text(size=35)) +
  geom_abline(intercept = 0, slope = coef(mult_nls)[1],color="black", linetype="solid", size=1.5)+
  geom_segment(aes(x = -7, y = 0, xend = 7, yend = 0),color="blue")+
  geom_segment(aes(x = 0, y = -0.8, xend = 0, yend = 0.8),color="blue")+
  coord_cartesian(ylim = c(-0.8, 0.8),xlim = c(-7, 7))+
  scale_y_continuous(minor_breaks = seq(-0.8,0.8,0.2),breaks = seq(-0.8,0.8,0.2),
                     labels = function(x) sprintf("%.1f", x))+
  scale_x_continuous(minor_breaks = seq(-7, 7, 2),breaks = seq(-7, 7, 2))+
  annotate("text", x=-7, y=0.4, hjust = 0,color="black",size = 9,
           label= paste(expression(R^2),": ",round(R2_1,3)), parse=TRUE) + 
  annotate("text", x=-7, y=0.3, hjust = 0,color="black",size = 9,
           label= paste(expression(Slope),": ",round(coef(mult_nls)[1],3)), parse=TRUE) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")



dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
Data$SAR <- Data$HV.x - Data$HV.y
Data$ALS <- Data$C2.x - Data$C2.y

random_sample <- createDataPartition(Data$ALS, p = 0.7, list = FALSE)
training_dataset  <- Data[random_sample, ]
testing_dataset <- Data[-random_sample, ]

df2 <- training_dataset[c("SAR","ALS")]
df3 <- df2 %>% 
  group_by(gr=cut(ALS, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
  arrange(as.numeric(gr))


new_df <- df3 %>% group_by(gr) %>% slice_sample(n=100)


mult_nls <- nls(ALS ~ a*SAR, start = list(a=0.08),data=new_df)
y_sim <- coef(mult_nls)[1]*new_df$SAR
R2_1 <- round(cor(new_df$ALS,y_sim)^2,3)

l <- lm(ALS~SAR,data=new_df)
new_df$preds <- predict(l,newdata=new_df)
bias <- mean(new_df$preds - new_df$ALS)
RMSE <- sqrt(mean((new_df$preds - new_df$ALS)^2))

p1 <-ggplot(new_df, aes(x=SAR, y=ALS))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  labs(y = paste0("ALS cover change (2010-2008)"),
       x = paste0("SAR backscatter change (dB)(2010-2008)")) +
  theme(text=element_text(size=35)) +
  geom_abline(intercept = 0, slope = coef(mult_nls)[1],color="black", linetype="solid", size=1.5)+
  geom_segment(aes(x = -7, y = 0, xend = 7, yend = 0),color="blue")+
  geom_segment(aes(x = 0, y = -0.8, xend = 0, yend = 0.8),color="blue")+
  coord_cartesian(ylim = c(-0.8, 0.8),xlim = c(-7, 7))+
  scale_y_continuous(minor_breaks = seq(-0.8,0.8,0.2),breaks = seq(-0.8,0.8,0.2),
                     labels = function(x) sprintf("%.1f", x))+
  scale_x_continuous(minor_breaks = seq(-7, 7, 2),breaks = seq(-7, 7, 2))+
  annotate("text", x=-7, y=0.4, hjust = 0,color="black",size = 9,
           label= paste(expression(R^2),": ",round(R2_1,3)), parse=TRUE) + 
  annotate("text", x=-7, y=0.3, hjust = 0,color="black",size = 9,
           label= paste(expression(Slope),": ",round(coef(mult_nls)[1],3)), parse=TRUE) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")



ggarrange(p1,p2,p3,ncol=3)
out = "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\composite thin 100 scatterplot dir slope constrained.jpg"
ggsave(out,height=12, width=36, dpi=600)

