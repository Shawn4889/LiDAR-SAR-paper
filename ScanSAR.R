#Xiaoxuan Li 
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
library(caret)
library(vegan)
library(rasterdiv)
library(snow)

# ------------------------------------------------------------------------------------------------ #
#Clean and merge standard SAR and GEDI data in 75m grid
dir <- "E:\\ScanSAR\\ScanSAR\\site_75m"
list_dir <- list.dirs("E:\\ScanSAR\\ScanSAR\\site_75m", recursive=FALSE)
for (l in list_dir){
  print(l)
  dir_index <- file.path(l,"index.csv")
  Data_index = read.csv(dir_index)
  colnames(Data_index)[1] <- "ID"
  
  files <- list.files(path = l, full.names = TRUE, pattern = "Scan")
  for (i in files){
    print(i)
    Data_sar = read_excel(i)
    Data_sar = subset(Data_sar, select=c(2,5))
    colnames(Data_sar)[2] <-  str_sub(basename(i), end=-6)
    write.csv(Data_sar, paste0(l,"\\",str_sub(basename(i), end=-6),".csv"), row.names = F)
    #Data_index <- merge(Data_index, Data_sar, by.x = "ID", by.y = "FID")
  }
}


# ------------------------------------------------------------------------------------------------ #
#Clean and merge standard Vol and cover data in 75m grid
dir <- "E:\\ScanSAR\\ScanSAR\\site_75m"
list_dir <- list.dirs("E:\\ScanSAR\\ScanSAR\\site_75m", recursive=FALSE)
for (l in list_dir){
  print(l)
  dir_index <- file.path(l,"index.csv")
  Data_index = read.csv(dir_index)
  colnames(Data_index)[1] <- "ID"
  files <- list.files(path = l, full.names = TRUE, pattern = "CHM_")
  for (i in files){
    print(i)
    Data_sar = read_excel(i)
    Data_sar$Volume <- Data_sar$COUNT*Data_sar$MEAN
    Data_sar = subset(Data_sar, select=c(2,6))
    colnames(Data_sar)[2] <-  str_sub(basename(i), end=-6)
    write.csv(Data_sar, paste0(l,"\\",str_sub(basename(i), end=-6),".csv"), row.names = F)
    #Data_index <- merge(Data_index, Data_sar, by.x = "ID", by.y = "FID")
  }
}


#AGBD
dir <- "E:\\ScanSAR\\ScanSAR\\site_75m"
list_dir <- list.dirs("E:\\ScanSAR\\ScanSAR\\site_75m", 
                      recursive=FALSE, full.names = FALSE)
for (l in list_dir){
  print(l)
  dir_index <- file.path(dir,l,"index.csv")
  Data_index = read.csv(dir_index)
  colnames(Data_index)[1] <- "ID"
  i <- file.path(dir,l,paste0("CHM_",l,"_75m.xlsx"))
  Data_sar = read_excel(i)
  Data_sar$AGBD <- 1.386052*Data_sar$MEAN^1.996775
  Data_sar = subset(Data_sar, select=c(2,6))
  colnames(Data_sar)[2] <-  str_sub(basename(i), end=-6)
  write.csv(Data_sar, file.path(dir,l,paste0("AGBD_2012",l,"_75m.csv")), row.names = F)
  #Data_index <- merge(Data_index, Data_sar, by.x = "ID", by.y = "FID")
  }


#cover
dir <- "E:\\ScanSAR\\ScanSAR\\site_75m"
list_dir <- list.dirs("E:\\ScanSAR\\ScanSAR\\site_75m", recursive=FALSE)
for (l in list_dir){
  print(l)
  dir_index <- file.path(l,"index.csv")
  Data_index = read.csv(dir_index)
  colnames(Data_index)[1] <- "ID"
  files <- list.files(path = l, full.names = TRUE, pattern = "Cover_")
  for (i in files){
    print(i)
    Data_sar = read_excel(i)
    Data_sar$Cover <- Data_sar$SUM/Data_sar$COUNT
    Data_sar = subset(Data_sar, select=c(2,6))
    colnames(Data_sar)[2] <-  str_sub(basename(i), end=-6)
    write.csv(Data_sar, paste0(l,"\\",str_sub(basename(i), end=-6),".csv"), row.names = F)
    #Data_index <- merge(Data_index, Data_sar, by.x = "ID", by.y = "FID")
  }
}




# ------------------------------------------------------------------------------------------------ #
#output ALS volume, cover, and SAR backscatter in one csv file based on SAR date 
#update version 20230725
#20140906
#20160903
#20170902
#20180929
#20190928
#20200926
#20210925
#20220924

list_ini_chm <- 0
list_ini_cover <- 0
list_ini_sar <- 0
list_ini_sar1 <- 0
list_ini_sar2 <- 0
list_ini_sar3 <- 0


dir <- "E:\\ScanSAR\\ScanSAR\\Result_0625\\site_75m"
sar_date <- "_20180929_75m.xlsx"
list_dir <- list.dirs(dir, recursive=FALSE,full.names = FALSE)
for (l in list_dir){
  print(l)
  dir_index <- file.path(dir,l,"index.csv")
  Data_index = read.csv(dir_index)
  colnames(Data_index)[1] <- "ID"
  
  dir_chm <- file.path(dir,l,paste0("CHM_",l,"_75m.xlsx"))
  Data = read_excel(dir_chm)
  Data_chm = subset(Data, select=c(2,5))
  Data_chm <- rbind(list_ini_chm, Data_chm)
  Data_index <- merge(Data_index, Data_chm, by.x = "ID", by.y = "FID")
  
  dir_cover <- file.path(dir,l,paste0("Cover_",l,"_75m.xlsx"))
  Data = read_excel(dir_cover)
  Data_cover = subset(Data, select=c(2,5))
  Data_cover <- rbind(list_ini_cover, Data_cover)
  Data_index <- merge(Data_index, Data_cover, by.x = "ID", by.y = "FID")
  
  dir_sar <- file.path(dir,l,paste0("ScanSAR_",l,sar_date))
  Data = read_excel(dir_sar)
  Data_sar = subset(Data, select=c(2,5))
  Data_sar <- rbind(list_ini_sar, Data_sar)
  Data_index <- merge(Data_index, Data_sar, by.x = "ID", by.y = "FID")
  
  #dir_sar1 <- file.path(dir,l,paste0("ScanSAR_",l,"_20140906_75m.xlsx"))
  #Data = read_excel(dir_sar1)
  #Data_sar1 = subset(Data, select=c(2,5))
  #Data_sar1 <- rbind(list_ini_sar1, Data_sar1)
  #Data_index <- merge(Data_index, Data_sar1, by.x = "ID", by.y = "FID")
  
  #dir_sar2 <- file.path(dir,l,paste0("ScanSAR_",l,"_20220924_75m.xlsx"))
  #Data = read_excel(dir_sar2)
  #Data_sar2 = subset(Data, select=c(2,5))
  #Data_sar2 <- rbind(list_ini_sar2, Data_sar2)
  #Data_index <- merge(Data_index, Data_sar2, by.x = "ID", by.y = "FID")
  
  #dir_sar3 <- file.path(dir,l,paste0("ScanSAR_",l,"_20180929_75m.xlsx"))
  #Data = read_excel(dir_sar3)
  #Data_sar3 = subset(Data, select=c(2,5))
  #Data_sar3 <- rbind(list_ini_sar3, Data_sar3)
  #Data_index <- merge(Data_index, Data_sar3, by.x = "ID", by.y = "FID")
  
}


Data <- na.omit(Data_index)
#colnames(Data) <- c("ID","CHM","Cover","SAR","SAR_2014","SAR_2022","SAR_2018")
colnames(Data) <- c("ID","CHM","Cover","SAR")
Data$Volume <- Data$CHM*5625
Data$Cover <- Data$Cover/5625
#Data$AGBD <- 10.35*Data$CHM*Data$Cover - 5.9236
Data$AGBD <- 9.0665*Data$CHM*Data$Cover

#1ha model test
Data$AGBD <- 18.839*Data$CHM*Data$Cover - 1.213
Data <- Data[Data$AGBD > 0 ,]

#Data$Diff1 <- Data$SAR_2018 - Data$SAR_2014
#Data$Diff2 <- Data$SAR_2018 - Data$SAR_2022
Data <- Data[is.finite(rowSums(Data)),]
Data <- Data[Data$CHM > 0,]
Data <- Data[Data$Cover > 0,]
Data <- Data[Data$SAR > -25 & Data$SAR < -10,]

#Data <- Data[Data$Diff1 > -10 & Data$Diff1 < 10,]
#Data <- Data[Data$Diff2 > -10 & Data$Diff2 < 10,]
#Data <- Data[Data$Diff1 > min(Data$Diff1)*0.8 & Data$Diff1 < max(Data$Diff1)*0.8,]
#Data <- Data[Data$Diff2 > min(Data$Diff2)*0.8 & Data$Diff2 < max(Data$Diff2)*0.8,]

#Volume
reg1 <- lm(log(Volume) ~ SAR, data = Data)
pred <- exp((coef(reg1)[1] + coef(reg1)[2]*Data$SAR))
coef(reg1)[2]
coef(reg1)[1]
round(summary(reg1)$adj.r.squared,2)
mean(pred - Data$Volume)
sqrt(mean((pred - Data$Volume)^2))
100*mean(pred - Data$Volume)/mean(Data$Volume)
100*sqrt(mean((pred - Data$Volume)^2))/mean(Data$Volume)

#Cover
reg2 <- lm(log(Cover) ~ SAR, data = Data)
pred <- exp((coef(reg2)[1] + coef(reg2)[2]*Data$SAR))
coef(reg2)[2]
coef(reg2)[1]
round(summary(reg2)$adj.r.squared,2)
mean(pred - Data$Cover)
sqrt(mean((pred - Data$Cover)^2))
100*mean(pred - Data$Cover)/mean(Data$Cover)
100*sqrt(mean((pred - Data$Cover)^2))/mean(Data$Cover)

#AGBD
Data$AGBD <- Data$AGBD 
reg3 <- lm(log(AGBD) ~ SAR, data = Data)
pred <- exp((coef(reg3)[1] + coef(reg3)[2]*Data$SAR)) 
coef(reg3)[2]
coef(reg3)[1]
round(summary(reg3)$adj.r.squared,2)
Data$AGBD <- Data$AGBD 
pred <- pred 
bias <- mean(pred - Data$AGBD)
RMSE <- sqrt(mean((pred - Data$AGBD)^2))
rbias <- 100*mean(pred - Data$AGBD)/mean(Data$AGBD)
rRMSE <- 100*sqrt(mean((pred - Data$AGBD)^2))/mean(Data$AGBD)
n<- nrow(Data)
cor(exp(predict(reg3, newdata = Data)),Data$AGBD)^2
#write.csv(Data, paste0(dir,"\\Result",sar_date), row.names = F)


ggplot(Data, aes(x=Volume, y=SAR))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  # coord_cartesian(xlim = c(0, 15),ylim = c(-30, -10))+
  scale_color_viridis(direction = 1)+
  #annotate("text",x=60,y=50,hjust = 0,size = 15,family= "A", label= "(c)") + 
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+
  annotate("text", x=10000, y=-25,hjust = 0,color="red",size = 7,
           label= paste(expression(Logarithmic~model~R^2),": ",
                        round(summary(reg1)$adj.r.squared,3)), parse=TRUE) + 
  labs(x="ALS Volume", 
       y="Mean of SAR backscatter")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=30))+
  #theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(Data, aes(x=Cover, y=SAR))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  # coord_cartesian(xlim = c(0, 15),ylim = c(-30, -10))+
  scale_color_viridis(direction = 1)+
  #annotate("text",x=60,y=50,hjust = 0,size = 15,family= "A", label= "(c)") + 
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+
  annotate("text", x=0.2, y=-25,hjust = 0,color="red",size = 7,
           label= paste(expression(Logarithmic~model~R^2),": ",
                        round(summary(reg2)$adj.r.squared,3)), parse=TRUE) + 
  labs(x="ALS Cover", 
       y="Mean of SAR backscatter")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=30))+
  #theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(Data, aes(x=AGBD, y=SAR))+ 
  geom_pointdensity()+
  theme_bw()+
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_cartesian(xlim = c(0, 50),ylim = c(-25, -10))+
  scale_color_viridis(direction = 1)+
  annotate("text", x=2.8, y=-10,hjust = 0,size = 7,
           label= paste(expression(Logarithmic~model~R^2),": ",
                        round(summary(reg3)$adj.r.squared,3)), parse=TRUE) + 
  annotate("text",x=2,y=-11.5,hjust = 0,size = 7,
           label= paste(
             "\n" , " RMSE: ", round(RMSE,3)," Mg/ha",
             "\n" , " rRMSE: ", round(rRMSE,3),"%",
             "\n" , " Bias: ", round(bias,3)," Mg/ha",
             "\n" , " %Bias: ", round(rbias,3)," %",
             "\n" , " Sample size: ", n)) + 
  #annotate("text",x=60,y=50,hjust = 0,size = 15,family= "A", label= "(c)") + 
  labs(x="Area-based (H*CC) ALS AGBD (Mg/ha)", 
       y="Mean of ScanSAR backscatter (2018 Sep) (dB)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=30))+
  #theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))



out = "E:\\ScanSAR\\ScanSAR\\Result_0625\\Scantterplot_20180929.jpg"
ggsave(out,height=12, width=12, dpi=600)


# ------------------------------------------------------------------------------------------------ #
#test 2019-09-28
dir = "C:\\Users\\Shawn\\Desktop\\test.xlsx"
Data = read_excel(dir, sheet = "Sheet1")
Data <- Data[Data$CHM > 0,]

reg3 <- lm(SAR ~ log(CHM), data = Data)

ggplot(Data, aes(x=CHM, y=SAR))+ 
  geom_pointdensity()+
  theme_bw()+
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+
  annotate("text", x=2, y=-25,hjust = 0,color="red",size = 7,
           label= paste(expression(Logarithmic~model~R^2),": ",
                        round(summary(reg3)$adj.r.squared,3)), parse=TRUE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  labs(x="Mean of CHM (m)", 
       y="Mean of SAR backscatter (dB)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=30))+
  #theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))



dir = "C:\\Users\\Shawn\\Desktop\\test.xlsx"
Data = read_excel(dir, sheet = "Sheet2")
Data <- Data[Data$RH98 > 0,]

reg3 <- lm(SAR ~ log(RH98), data = Data)
summary(reg3)

p1<- ggplot(Data, aes(x=RH98, y=SAR))+ 
  geom_pointdensity()+
  theme_bw()+
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+
  annotate("text", x=5, y=-25,hjust = 0,color="red",size = 7,
           label= paste(expression(Logarithmic~model~R^2),": ",
                        round(summary(reg3)$adj.r.squared,3)), parse=TRUE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  labs(x="Mean of GEDI RH98 (m)", 
       y="Mean of SAR backscatter (dB)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=30))+
  #theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


reg3 <- lm(SAR ~ log(AGBD), data = Data)
summary(reg3)
p2<- ggplot(Data, aes(x=AGBD, y=SAR))+ 
  geom_pointdensity()+
  theme_bw()+
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+
  annotate("text", x=5, y=-25,hjust = 0,color="red",size = 7,
           label= paste(expression(Logarithmic~model~R^2),": ",
                        round(summary(reg3)$adj.r.squared,3)), parse=TRUE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  labs(x="Mean of GEDI AGBD (Mg/ha)", 
       y="Mean of SAR backscatter (dB)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=30))+
  #theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p2)



# ------------------------------------------------------------------------------------------------ #
dir <- "C:\\Users\\Shawn\\Desktop\\Grid_100m_GEDI.csv"
Data_GEDI = read.csv(dir)
colnames(Data_GEDI)[1] <- "ID"

dir <- "C:\\Users\\Shawn\\Desktop\\Grid_100m_SAR.csv"
Data_SAR = read.csv(dir)
colnames(Data_SAR)[1] <- "ID"
df <- merge(Data_GEDI, Data_SAR, by.x = "ID", by.y = "ID")
df <- df[sample(nrow(df), 10000), ]

df <- df[df$SAR > -35,]
df <- df[df$RH98 < 50,]


reg3 <- lm(SAR ~ log(RH98), data = df)
summary(reg3)

ggplot(df, aes(x=RH98, y=SAR))+ 
  geom_pointdensity()+
  theme_bw()+
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+
  annotate("text", x=20, y=-25,hjust = 0,color="red",size = 7,
           label= paste(expression(Logarithmic~model~R^2),": ",
                        round(summary(reg3)$adj.r.squared,3)), parse=TRUE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  labs(x="Mean of CHM (m)", 
       y="Mean of SAR backscatter (dB)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=30))+
  #theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))



# ------------------------------------------------------------------------------------------------ #
#SAR vs. GEDI correlation table RH98
dir <- "E:\\ScanSAR\\ScanSAR\\table_100m\\GEDI.csv"
Data_GEDI = read.csv(dir)
colnames(Data_GEDI)[1] <- "ID"

Locdir <- "E:\\ScanSAR\\ScanSAR\\table_100m"
files <- list.files(path = Locdir, full.names = TRUE, pattern = "_100m.csv")
list_r2 <- list()

listt <- list("ScanSAR","R2")
df_listt <- data.frame(listt)
colnames(df_listt) <- df_listt

for (i in files){
  print(i)
  Data_SAR = read.csv(i)
  colnames(Data_SAR)[1] <- "ID"
  colnames(Data_SAR)[2] <- "SAR"
  Data <- merge(Data_GEDI, Data_SAR, by.x = "ID", by.y = "ID")
  Data <- Data[Data$SAR > -35,]
  Data <- Data[Data$RH98 < 50,]
  list_r2[[1]] <- file_path_sans_ext(basename(i))
  list_r2[[2]] <- round(cor(log(Data$RH98), Data$SAR,method = "pearson")^2,3)
  listt <- mapply(c, listt, list_r2, SIMPLIFY=FALSE)
  df1 <- data.frame(list_r2)
  colnames(df1) <- listt <- list("ScanSAR","R2")
  df_listt <- rbind(df_listt,df1)
  }
write.table(df_listt,file="E:\\ScanSAR\\ScanSAR\\R2_RH98_subsample100.csv", quote=F,sep=",",row.names=F)



# ------------------------------------------------------------------------------------------------ #
#SAR vs. GEDI correlation table AGBD
dir <- "E:\\ScanSAR\\ScanSAR\\table_100m\\GEDI.csv"
Data_GEDI = read.csv(dir)
colnames(Data_GEDI)[1] <- "ID"

Locdir <- "E:\\ScanSAR\\ScanSAR\\table_100m"
files <- list.files(path = Locdir, full.names = TRUE, pattern = "_100m.csv")
list_r2 <- list()

listt <- list("ScanSAR","R2")
df_listt <- data.frame(listt)
colnames(df_listt) <- df_listt

for (i in files){
  print(i)
  Data_SAR = read.csv(i)
  colnames(Data_SAR)[1] <- "ID"
  colnames(Data_SAR)[2] <- "SAR"
  Data <- merge(Data_GEDI, Data_SAR, by.x = "ID", by.y = "ID")
  Data <- Data[Data$SAR > -35,]
  Data <- Data[Data$AGBD < 100,]
  list_r2[[1]] <- file_path_sans_ext(basename(i))
  list_r2[[2]] <- round(cor(log(Data$AGBD), Data$SAR,method = "pearson")^2,3)
  listt <- mapply(c, listt, list_r2, SIMPLIFY=FALSE)
  df1 <- data.frame(list_r2)
  colnames(df1) <- listt <- list("ScanSAR","R2")
  df_listt <- rbind(df_listt,df1)
}
write.table(df_listt,file="E:\\ScanSAR\\ScanSAR\\R2_AGBD.csv", quote=F,sep=",",row.names=F)




# ------------------------------------------------------------------------------------------------ #
#test KNP SAR vs. GEDI
dir <- "E:\\ScanSAR\\ScanSAR\\table_100m\\GEDI.csv"
Data_GEDI = read.csv(dir)
colnames(Data_GEDI)[1] <- "ID"

Data_GEDI<-Data_GEDI[Data_GEDI$KNP==1,]
#Data_GEDI<-Data_GEDI[Data_GEDI$Site=="Justicia",]



dir <- "E:\\ScanSAR\\ScanSAR\\table_100m\\ScanSAR_20190928_100m.csv"
Data_SAR = read.csv(dir)
colnames(Data_SAR)[1] <- "ID"
colnames(Data_SAR)[2] <- "SAR"

Data <- merge(Data_GEDI, Data_SAR, by.x = "ID", by.y = "ID")
Data <- Data[Data$SAR > -35,]
Data <- Data[Data$AGBD < 80,]


#Data <- Data[sample(nrow(Data), 10000), ]

reg3 <- lm(SAR ~ log(AGBD), data = Data)
summary(reg3)
Data$pred <- predict(reg3)
RMSE <- sqrt(mean((Data$SAR - Data$pred)^2))


#subsampling process
#Data <- Data %>% group_by(gr=cut(RH98, breaks= seq(0, 100, by=1))) %>% arrange(as.numeric(gr))
#Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
#reg3 <- lm(SAR ~ log(RH98), data = Data)
#summary(reg3)



ggplot(Data, aes(x=AGBD, y=SAR))+ 
  geom_pointdensity()+
  theme_bw()+
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+
  annotate("text", x=10, y=-30,hjust = 0,color="red",size = 7,
           label= paste(expression(Logarithmic~model~R^2),": ",
                        round(summary(reg3)$adj.r.squared,3)), parse=TRUE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  labs(x="Mean of GEDI agbd (Mg/ha)", 
       y="Mean of SAR backscatter (dB)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=30))+
  #theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))






# ------------------------------------------------------------------------------------------------ #
dir <- "E:\\ScanSAR\\ScanSAR\\Result_0531\\Adjusted"
dir_index <- file.path(dir,"GEDI.csv")
Data_index = read.csv(dir_index)
colnames(Data_index)[1] <- "ID"

list_r2 <- list()
listt <- list("ScanSAR","R2")
df_listt <- data.frame(listt)
colnames(df_listt) <- df_listt

files <- list.files(path = dir, full.names = TRUE, pattern = "ScanSAR_KNP")
for (i in files){
  print(i)
  Data_sar = read.csv(i)
  Data_sar = subset(Data_sar, select=c(2,5))
  colnames(Data_sar)[1] <- "ID"
  colnames(Data_sar)[2] <- "SAR"
  Data <- merge(Data_sar,Data_index, by.x = "ID", by.y = "ID")
  Data <- Data[Data$SAR > -35,]
  Data <- Data[Data$agbd < 50,]
  list_r2[[1]] <- file_path_sans_ext(basename(i))
  list_r2[[2]] <- round(cor(log(Data$agbd), Data$SAR,method = "pearson")^2,3)
  listt <- mapply(c, listt, list_r2, SIMPLIFY=FALSE)
  df1 <- data.frame(list_r2)
  colnames(df1) <- listt <- list("ScanSAR","R2")
  df_listt <- rbind(df_listt,df1)
}


write.table(df_listt,file="E:\\ScanSAR\\ScanSAR\\Result_0531\\Adjusted\\R2_agbd.csv",
            quote=F, sep=",", row.names=F)

# ------------------------------------------------------------------------------------------------ #
#scatterplot AGBD vs. SAR
dir1 <- "E:\\ScanSAR\\ScanSAR\\Result_0531\\Adjusted\\GEDI_KNP.csv"
Data1 = read.csv(dir1,header=T)
Data1 <- na.omit(Data1)
colnames(Data1)[1] <- "ID"

dir2 <- "E:\\ScanSAR\\ScanSAR\\Result_0531\\Adjusted\\ScanSAR_KNP_20190928_25m.csv"
Data2 = read.csv(dir2,header=T)
Data2 <- na.omit(Data2)
colnames(Data2)[2] <- "ID"

m1 <- merge(Data2, Data1, by.x = "ID", by.y = "ID")
m1 <- m1[m1$MEAN > -35,]
m1 <- m1[m1$agbd < 80,]

reg <- lm(MEAN ~ log(agbd), data = m1)
summary(reg)

Data <- m1
Data <- Data[Data$MEAN > -35,]
Data <- Data[Data$agbd < 80,]


ggplot(Data, aes(x=agbd, y=MEAN))+ 
  geom_pointdensity()+
  theme_bw()+
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+
  annotate("text", x=10, y=-30,hjust = 0,color="red",size = 7,
           label= paste(expression(Logarithmic~model~R^2),": ",
                        round(summary(reg)$adj.r.squared,3)), parse=TRUE)+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  labs(x="Mean of GEDI agbd (Mg/ha)", 
       y="Mean of SAR backscatter (dB)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=30))+
  #theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))



# ------------------------------------------------------------------------------------------------ #
#scatterplot rh98 vs. SAR
dir1 <- "E:\\ScanSAR\\ScanSAR\\Result_0531\\Adjusted\\GEDI_KNP.csv"
Data1 = read.csv(dir1,header=T)
Data1 <- na.omit(Data1)
colnames(Data1)[1] <- "ID"

dir2 <- "E:\\ScanSAR\\ScanSAR\\Result_0531\\Adjusted\\ScanSAR_KNP_20190928_25m.csv"
Data2 = read.csv(dir2,header=T)
Data2 <- na.omit(Data2)
colnames(Data2)[2] <- "ID"

m1 <- merge(Data2, Data1, by.x = "ID", by.y = "ID")
m1 <- m1[m1$MEAN > -25,]
m1 <- m1[m1$rh98 < 30,]
Data<- m1

#subsample
#Data <- Data %>% group_by(gr=cut(rh98, breaks= seq(0, 30, by=1))) %>% arrange(as.numeric(gr))
#Data <- Data %>% group_by(gr) %>% slice_sample(n=50)

reg <- lm(MEAN ~ log(rh98), data = Data)
summary(reg)



ggplot(Data, aes(x=rh98, y=MEAN))+ 
  geom_pointdensity()+
  theme_bw()+
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+
  annotate("text", x=10, y=-30,hjust = 0,color="red",size = 7,
           label= paste(expression(Logarithmic~model~R^2),": ",
                        round(summary(reg)$adj.r.squared,3)), parse=TRUE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  labs(x="Mean of GEDI RH98 (m)", 
       y="Mean of SAR backscatter (dB)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=30))+
  #theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))




# ------------------------------------------------------------------------------------------------ #
#KNP AGBD vs. SAR
dir <- "E:\\ScanSAR\\ScanSAR\\Result_0531\\Adjusted"
dir_index <- file.path(dir, "GEDI_KNP.csv")
Data_index = read.csv(dir_index)
colnames(Data_index)[1] <- "ID"

list_r2 <- list()
listt <- list("ScanSAR","R2")
df_listt <- data.frame(listt)
colnames(df_listt) <- df_listt

files <- list.files(path = dir, full.names = TRUE, pattern = "ScanSAR_KNP_")
for (i in files){
  print(i)
  Data_sar = read.csv(i)
  Data_sar = subset(Data_sar, select=c(2,5))
  colnames(Data_sar)[1] <- "ID"
  colnames(Data_sar)[2] <- "SAR"
  Data <- merge(Data_sar,Data_index, by.x = "ID", by.y = "ID")
  Data <- Data[Data$SAR > -35,]
  Data <- Data[Data$agbd < 100,]
  list_r2[[1]] <- file_path_sans_ext(basename(i))
  list_r2[[2]] <- round(cor(log(Data$agbd), Data$SAR, method = "pearson")^2,3)
  listt <- mapply(c, listt, list_r2, SIMPLIFY=FALSE)
  df1 <- data.frame(list_r2)
  colnames(df1) <- listt <- list("ScanSAR", "R2")
  df_listt <- rbind(df_listt,df1)
}


write.table(df_listt, file="E:\\ScanSAR\\ScanSAR\\Result_0531\\Adjusted\\R2_agbd_KNP.csv", 
            quote=F, sep=",", row.names=F)




#boxplot 
dir <- "E:\\ScanSAR\\ScanSAR\\Result_0531\\Adjusted"
dir_index <- file.path(dir, "R2_agbd_KNP.csv")
Data_index = read.csv(dir_index)
Data_index$Month <- factor(Data_index$Month, levels=c("Jan", "Feb", "Mar", "Apr",
                                                 "May", "Jun", "Jul", "Aug",
                                                 "Sept", "Nov", "Dec"))


ggplot(Data_index, aes(x=Month, y=R2, group=Month)) + 
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  #theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))

  


# ------------------------------------------------------------------------------------------------ #
#KNP AGBD vs. SAR
dir <- "E:\\ScanSAR\\ScanSAR\\Result_0531\\Adjusted"
dir_index <- file.path(dir, "GEDI_KNP.csv")
Data_index = read.csv(dir_index)
colnames(Data_index)[1] <- "ID"

list_r2 <- list()
listt <- list("ScanSAR","R2")
df_listt <- data.frame(listt)
colnames(df_listt) <- df_listt

files <- list.files(path = dir, full.names = TRUE, pattern = "ScanSAR_KNP_")
for (i in files){
  print(i)
  Data_sar = read.csv(i)
  Data_sar = subset(Data_sar, select=c(2,5))
  colnames(Data_sar)[1] <- "ID"
  colnames(Data_sar)[2] <- "SAR"
  Data <- merge(Data_sar,Data_index, by.x = "ID", by.y = "ID")
  Data <- Data[Data$SAR > -35,]
  Data <- Data[Data$rh98 < 30,]
  Data <- Data %>% group_by(gr=cut(rh98, breaks= seq(0, 30, by=1))) %>% arrange(as.numeric(gr))
  Data <- Data %>% group_by(gr) %>% slice_sample(n=50)
  list_r2[[1]] <- file_path_sans_ext(basename(i))
  list_r2[[2]] <- round(cor(log(Data$rh98), Data$SAR, method = "pearson")^2,3)
  listt <- mapply(c, listt, list_r2, SIMPLIFY=FALSE)
  df1 <- data.frame(list_r2)
  colnames(df1) <- listt <- list("ScanSAR", "R2")
  df_listt <- rbind(df_listt,df1)
}


write.table(df_listt, file="E:\\ScanSAR\\ScanSAR\\Result_0531\\Adjusted\\R2_rh98_KNP_subsample.csv", 
            quote=F, sep=",", row.names=F)




#boxplot 
dir <- "E:\\ScanSAR\\ScanSAR\\Result_0531\\Adjusted"
dir_index <- file.path(dir, "R2_rh98_KNP_subsample.csv")
Data_index = read.csv(dir_index)


Data_index$Month <- factor(Data_index$Month, levels=seq(1, 12, by=1))

ggplot(Data_index, aes(x=Month, y=R2, group=Month)) + 
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  #theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))






# ------------------------------------------------------------------------------------------------ #
#test cover histogram per polygon 
folder_path <- "E:\\ScanSAR\\ScanSAR\\Result_0607\\Sandringham Private Nature Reserve"
files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
plot_list <- list()
c_list <- list()
for (i in 1:8){
  r <- raster(files[i])
  r[r > 1] <- NA
  year <- str_extract(basename(files[i]), "\\d{4}")       
  df <- data.frame(rasterToPoints(r))
  new_column_names <- c("x","y", "cover")
  colnames(df) <- new_column_names
  df$group_cover <- cut(df$cover,breaks = seq(0,1, 0.1),dig.lab = 5)
  c_list <- append(c_list, list(table(df$group_cover)))
  
  p <- ggplot(df, aes(x=group_cover)) +     
    theme_bw()+
    coord_cartesian(ylim =  c(0, 0.5))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    labs(x = paste0(year," canopy cover (%)"),
         y = "Frequency")+
    geom_histogram(aes(y = stat(count) / sum(count)),
                   color =  "blue",fill = "red",stat="count",alpha = 0.8) +
    scale_x_discrete(labels=c("[0,10]","[10,20]","[20,30]","[30,40]",
                              "[40,50]","[50,60]","[60,70]","[70,80]",
                              "[80,90]","[90,100]"))+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=20))
    
  #plot_list[[i]] = p
  
  #rao's Q
  out <- paRao(r, area=NULL, field=NULL, dist_m="euclidean",
               window=3, alpha=1, method="classic", rasterOut=TRUE, lambda=0,
               na.tolerance=1.0, rescale=FALSE, diag=TRUE, 
               simplify=1, np=4,cluster.type="SOCK", debugging=TRUE)
  
  writeRaster(brick(out[[1]]),
              file.path('E:\\ScanSAR\\ScanSAR\\Result_0607\\Rao\\Sandringham',
                        basename(files[i])),
              options=c('TFW=YES'))
}

c_df <- data.frame(do.call(rbind, c_list))
c_df$year <- c("Y2014","Y2016","Y2017","Y2018",
               "Y2019","Y2020","Y2021","Y2022")
colnames(c_df) <- c("[0-0.1]","[0.1-0.2]","[0.2-0.3]","[0.3-0.4]", 
                    "[0.4-0.5]","[0.5-0.6]","[0.6-0.7]","[0.7-0.8]",
                    "[0.8-0.9]","[0.9-1]","year")


#Shannon-Weiner index 
sw <- diversity(c_df[-11], index="shannon")
plot(sw)


ggarrange(plotlist=plot_list)
out <- "E:\\ScanSAR\\ScanSAR\\Result_0607\\Sandringham.jpg"
ggsave(out,height=13, width=26, dpi=600)







# ------------------------------------------------------------------------------------------------ #
#KNP AGBD vs. SAR subsample 50
#20140906
#20160903
#20170902
#20180929
#20190928
#20200926
#20210925
#20220924

dir <- "E:\\ScanSAR\\ScanSAR\\Result_0531\\Adjusted"
dir_index <- file.path(dir, "GEDI_KNP.csv")
Data_index = read.csv(dir_index)
colnames(Data_index)[1] <- "ID"

suf <- "20220924"
i <- file.path(dir,paste0("ScanSAR_KNP_",suf,"_25m.csv"))

print(i)
Data_sar = read.csv(i)
Data_sar = subset(Data_sar, select=c(2,5))
colnames(Data_sar)[1] <- "ID"
colnames(Data_sar)[2] <- "SAR"
Data <- merge(Data_sar,Data_index, by.x = "ID", by.y = "ID")
Data <- Data[Data$SAR > -35,]
Data <- Data[Data$agbd < 100,]
Data <- Data %>% group_by(gr=cut(agbd, breaks= seq(0, 100, by=1))) %>% arrange(as.numeric(gr))
Data <- Data %>% group_by(gr) %>% slice_sample(n=50)
reg <- lm(log(agbd) ~ SAR, data = Data)
summary(reg)

x<-Data$SAR
y<-Data$agbd
  
mult_nls <- nls(y ~ a*exp(b*x), start = list(a=1000, b=0.1))
pred <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*x)
r2 <- round(cor(y,pred)^2,3)
RMSE<- sqrt(mean((pred - y)^2))

ggplot(Data, aes(x=agbd, y=SAR))+ 
  geom_pointdensity()+
  theme_bw()+
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+
  coord_cartesian(xlim = c(0, 100),ylim = c(-10, -35))+
  annotate("text", x=10, y=-26,hjust = 0,color="red",size = 7,
           label= paste0("ScanSAR: ",suf), parse=TRUE) +  
  annotate("text", x=10, y=-28,hjust = 0,color="red",size = 7,
           label= paste(expression(Logarithmic~model~R^2),": ",
                        round(summary(reg)$adj.r.squared,3)), parse=TRUE) + 
  annotate("text",x=10,y=-30,hjust = 0,color="red",size = 7,
           label= paste("RMSE: ", round(RMSE,3), "Mg/ha")) +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  labs(x="Mean of GEDI AGBD (m)", 
       y="Mean of SAR backscatter (dB)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=30))+
  #theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))



# ------------------------------------------------------------------------------------------------ #
#KNP AGBD vs. SAR original
#20140906
#20160903
#20170902
#20180929
#20190928
#20200926
#20210925
#20220924

dir <- "E:\\ScanSAR\\ScanSAR\\Result_0531\\Adjusted"
dir_index <- file.path(dir, "GEDI_KNP.csv")
Data_index = read.csv(dir_index)
colnames(Data_index)[1] <- "ID"

suf <- "20180929"
i <- file.path(dir,paste0("ScanSAR_KNP_",suf,"_25m.csv"))

print(i)
Data_sar = read.csv(i)
Data_sar = subset(Data_sar, select=c(2,5))
colnames(Data_sar)[1] <- "ID"
colnames(Data_sar)[2] <- "SAR"
Data <- merge(Data_sar,Data_index, by.x = "ID", by.y = "ID")
Data <- Data[Data$SAR > -35,]
Data <- Data[Data$agbd < 100,]

#write.table(Data,file='E:\\ScanSAR\\ScanSAR\\Result_1004\\result\\gedi_sar_25m.csv',sep = ",",col.names=TRUE)

reg <- lm(log(agbd) ~ SAR, data = Data)
summary(reg)

x<-Data$SAR
y<-Data$agbd

mult_nls <- nls(y ~ a*exp(b*x), start = list(a=1000, b=0.1))
pred <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*x)
r2 <- round(cor(y,pred)^2,3)
RMSE<- sqrt(mean((pred - y)^2))

ggplot(Data, aes(x=agbd, y=SAR))+ 
  geom_pointdensity()+
  theme_bw()+
  geom_smooth(
    method="lm",
    formula = 'y ~ log(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+
  coord_cartesian(xlim = c(0, 100),ylim = c(-35, -10))+
  annotate("text", x=35, y=-28,hjust = 0,color="red",size = 7,
           label= paste0("ScanSAR: ",suf), parse=TRUE) +  
  annotate("text", x=35, y=-30,hjust = 0,color="red",size = 7,
           label= paste(expression(Logarithmic~model~R^2),": ",
                        round(summary(reg)$adj.r.squared,3)), parse=TRUE) + 
  annotate("text",x=35,y=-32,hjust = 0,color="red",size = 7,
           label= paste("RMSE: ", round(RMSE,3), "Mg/ha")) +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  labs(x="Mean of GEDI AGBD (m)", 
       y="Mean of SAR backscatter (dB)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=30))+
  #theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))







# ------------------------------------------------------------------------------------------------ #
#KNP AGBD vs. SAR
dir <- "E:\\ScanSAR\\ScanSAR\\Result_0531\\Adjusted"
dir_index <- file.path(dir, "GEDI_KNP.csv")
Data_index = read.csv(dir_index)
colnames(Data_index)[1] <- "ID"

list_r2 <- list()
listt <- list("ScanSAR", "R2", "RMSE")
df_listt <- data.frame(listt)
colnames(df_listt) <- df_listt

files <- list.files(path = dir, full.names = TRUE, pattern = "ScanSAR_KNP_")

for (i in files){
  print(i)
  Data_sar = read.csv(i)
  Data_sar = subset(Data_sar, select=c(2,5))
  colnames(Data_sar)[1] <- "ID"
  colnames(Data_sar)[2] <- "SAR"
  Data <- merge(Data_sar,Data_index, by.x = "ID", by.y = "ID")
  Data <- Data[Data$SAR > -35,]
  Data <- Data[Data$agbd < 100,]
  #Data <- Data %>% group_by(gr=cut(agbd, breaks= seq(0, 100, by=1))) %>% arrange(as.numeric(gr))
  #Data <- Data %>% group_by(gr) %>% slice_sample(n=50)
  
  reg <- lm(log(agbd) ~ SAR, data = Data)
  summary(reg)
  x<-Data$SAR
  y<-Data$agbd
  
  mult_nls <- nls(y ~ a*exp(b*x), start = list(a=1000, b=0.1))
  pred <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*x)
  RMSE<- sqrt(mean((pred - y)^2))
  
  list_r2[[1]] <- file_path_sans_ext(basename(i))
  list_r2[[2]] <- round(cor(log(Data$agbd), Data$SAR, method = "pearson")^2,3)
  list_r2[[3]] <- RMSE
  listt <- mapply(c, listt, list_r2, SIMPLIFY=FALSE)
  df1 <- data.frame(list_r2)
  colnames(df1) <- listt <- list("ScanSAR", "R2", "RMSE")
  df_listt <- rbind(df_listt,df1)
}













# ------------------------------------------------------------------------------------------------ #
#time series + power to db
dir <- "E:\\ScanSAR\\ScanSAR\\Result_0612"
dir_index <- file.path(dir,"Index.csv")
output <- file.path(dir,"Result.csv")
Data_index = read.csv(dir_index)
colnames(Data_index)[1] <- "ID"

files <- list.files(path = dir, full.names = TRUE, pattern = "Scan")
for (i in files){
  print(i)
  Data_sar = read.csv(i)
  Data_sar = subset(Data_sar, select=c(2,5))
  Data_sar$MEAN = 10*(log10(Data_sar$MEAN))
  colnames(Data_sar)[2] <-  str_sub(basename(i), end=-6)
  Data_index <- merge(Data_index, Data_sar, by.x = "ID", by.y = "FID")
}

write.csv(Data_index, output, row.names = F)






# ------------------------------------------------------------------------------------------------ #
#test rh98 histogram per polygon 50: Andover,  31: Sanderingham, 1: Thornybush, 3: Sabi Sand, 51: Mayeleti
plot_list <- list()
suf <- "51"
for (i in 2019:2022){
  dir <- file.path("E:\\ScanSAR\\ScanSAR\\Result_0614",i)
  dir_2 <- file.path(dir,paste0(suf,".csv"))
  if (file.exists(dir_2)){
    Data = read.csv(dir_2)
    Data <- Data[Data$rh98 < 16,]
    Data$group_rh98 <- cut(Data$rh98,breaks = seq(2,16, 2),dig.lab = 5)
    p <- ggplot(Data, aes(x=group_rh98)) +     
      theme_bw()+
      coord_cartesian(ylim =  c(0, 1))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      labs(x = paste0(i,", Polygon: ",suf,", RH98 (m),", " Total Count: ", nrow(Data)),
           y = "Frequency")+
      geom_histogram(aes(y = stat(count) / sum(count)),
                     color =  "blue",fill = "red",stat="count",alpha = 0.8) +
      theme(legend.title = element_blank())+
      theme(text=element_text(size=20))
    
    plot_list[[i-2018]] = p
  }
  
  
}
plot_list<- Filter(Negate(is.null), plot_list)
ggarrange(plotlist=plot_list)
out <- file.path("E:\\ScanSAR\\ScanSAR\\Result_0614",paste0(suf,".jpg"))
ggsave(out,height=13, width=26, dpi=600)





# ------------------------------------------------------------------------------------------------ #
#test cover histogram per ALS site
#['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
folder_path <- "E:\\ScanSAR\\ScanSAR\\Result_0614\\Cover_ALS_site\\Justicia"
files <- list.files(folder_path,pattern = "\\.tif$",  full.names = TRUE)
plot_list <- list()
for (i in 1:8){
  r <- raster(files[i])
  r[r > 1] <- NA
  year <- str_extract(basename(files[i]), "\\d{4}")       
  df <- data.frame(rasterToPoints(r))
  new_column_names <- c("x","y", "cover")
  colnames(df) <- new_column_names
  df$group_cover <- cut(df$cover,breaks = seq(0,1, 0.1),dig.lab = 5)
  
  p <- ggplot(df, aes(x=group_cover)) +     
    theme_bw()+
    coord_cartesian(ylim =  c(0, 0.6))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    labs(x = paste0(year," canopy cover (%)"),
         y = "Frequency")+
    geom_histogram(aes(y = stat(count) / sum(count)),
                   color =  "blue",fill = "red",stat="count",alpha = 0.8) +
    scale_x_discrete(labels=c("[0,10]","[10,20]","[20,30]","[30,40]",
                              "[40,50]","[50,60]","[60,70]","[70,80]",
                              "[80,90]","[90,100]"))+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=20))
  
  plot_list[[i]] = p
}

ggarrange(plotlist=plot_list)
out <- "E:\\ScanSAR\\ScanSAR\\Result_0614\\Cover_ALS_site\\Welverdiendt.jpg"
ggsave(out,height=13, width=26, dpi=600)




# ------------------------------------------------------------------------------------------------ #
#test cover histogram per ALS site
#mean annual cover value
#['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
folder_path <- "E:\\ScanSAR\\ScanSAR\\Result_0614\\Cover_ALS_site\\Limpopo3"
files <- list.files(folder_path,pattern = "\\.tif$",  full.names = TRUE)
list <- list()
for (i in 1:8){
  r <- raster(files[i])
  r[r > 1] <- NA
  year <- str_extract(basename(files[i]), "\\d{4}")       
  df <- data.frame(rasterToPoints(r))
  new_column_names <- c("x","y", "cover")
  colnames(df) <- new_column_names
  list[[i]] = mean(df$cover)
}

as.numeric(unlist(list))





# ------------------------------------------------------------------------------------------------ #
#KNP rh98 vs. SAR
#update 20230725
dir <- "E:\\ScanSAR\\ScanSAR\\Result_0531\\Adjusted"
dir_index <- file.path(dir, "GEDI_KNP.csv")
Data_index = read.csv(dir_index)
colnames(Data_index)[1] <- "ID"

list_r2 <- list()
listt <- list("ScanSAR", "R2", "RMSE")
df_listt <- data.frame(listt)
colnames(df_listt) <- df_listt

files <- list.files(path = dir, full.names = TRUE, pattern = "ScanSAR_KNP_")

for (i in files){
  print(i)
  Data_sar = read.csv(i)
  Data_sar = subset(Data_sar, select=c(2,5))
  colnames(Data_sar)[1] <- "ID"
  colnames(Data_sar)[2] <- "SAR"
  Data <- merge(Data_sar,Data_index, by.x = "ID", by.y = "ID")
  Data <- Data[Data$SAR > -25,]
  Data <- Data[Data$rh98 < 30,]
  Data <- Data %>% group_by(gr=cut(rh98, breaks= seq(0, 30, by=0.1))) %>% arrange(as.numeric(gr))
  Data <- Data %>% group_by(gr) %>% slice_sample(n=50)
  
  reg <- lm(log(rh98) ~ SAR, data = Data)
  summary(reg)
  x<-Data$SAR
  y<-Data$rh98
  
  mult_nls <- nls(y ~ a*exp(b*x), start = list(a=1000, b=0.1))
  pred <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*x)
  RMSE<- sqrt(mean((pred - y)^2))
  
  list_r2[[1]] <- file_path_sans_ext(basename(i))
  list_r2[[2]] <- round(cor(log(Data$rh98), Data$SAR, method = "pearson")^2,3)
  list_r2[[3]] <- RMSE
  listt <- mapply(c, listt, list_r2, SIMPLIFY=FALSE)
  df1 <- data.frame(list_r2)
  colnames(df1) <- listt <- list("ScanSAR", "R2", "RMSE")
  df_listt <- rbind(df_listt,df1)
}





# ------------------------------------------------------------------------------------------------ #
#KNP ALS MCH vs. SAR
#update 20230725
list_r2 <- list()
listt <- list("ScanSAR", "R2", "bias", "RMSE")
df_listt <- data.frame(listt)
colnames(df_listt) <- df_listt

dir <- "E:\\ScanSAR\\ScanSAR\\Result_0625\\site_75m"
dir_sar <- "E:\\ScanSAR\\ScanSAR\\Result_0625\\site_75m\\Agincourt"
list_sar_dir <- list.files(dir_sar, recursive=FALSE,full.names = FALSE,pattern = "ScanSAR_")
list_sar_dir <- substr(list_sar_dir, start = 18, stop = 40)
for (i in list_sar_dir){
  list_ini_chm <- 0
  list_ini_sar <- 0
  print(i)
  list_dir <- list.dirs(dir, recursive=FALSE,full.names = FALSE)
  for (l in list_dir){
    print(l)
    dir_index <- file.path(dir,l,"index.csv")
    Data_index = read.csv(dir_index)
    colnames(Data_index)[1] <- "ID"
    
    dir_chm <- file.path(dir,l,paste0("CHM_",l,"_75m.xlsx"))
    Data = read_excel(dir_chm)
    Data_chm = subset(Data, select=c(2,5))
    Data_chm <- rbind(list_ini_chm, Data_chm)
    Data_index <- merge(Data_index, Data_chm, by.x = "ID", by.y = "FID")
    
    dir_sar <- file.path(dir,l,paste0("ScanSAR_",l,i))
    Data = read_excel(dir_sar)
    Data_sar = subset(Data, select=c(2,5))
    Data_sar <- rbind(list_ini_sar, Data_sar)
    Data_index <- merge(Data_index, Data_sar, by.x = "ID", by.y = "FID")
  }
  Data <- na.omit(Data_index)
  colnames(Data) <- c("ID","CHM","SAR")
  Data <- Data[is.finite(rowSums(Data)),]
  Data <- Data[Data$CHM > 0,]
  
  reg1 <- lm(log(CHM) ~ SAR, data = Data)
  pred <- exp((coef(reg1)[1] + coef(reg1)[2]*Data$SAR))

  bias <- mean(pred - Data$CHM)
  RMSE <- sqrt(mean((pred - Data$CHM)^2))
  
  list_r2[[1]] <- file_path_sans_ext(basename(i))
  list_r2[[2]] <- round(round(summary(reg1)$adj.r.squared,2),3)
  list_r2[[3]] <- bias
  list_r2[[4]] <- RMSE

  listt <- mapply(c, listt, list_r2, SIMPLIFY=FALSE)
  df <- data.frame(list_r2)
  colnames(df) <- listt <- list("ScanSAR", "R2", "bias", "RMSE")
  df_listt <- rbind(df_listt,df)
}










# ------------------------------------------------------------------------------------------------ #
#output ALS volume, cover, and SAR backscatter in one csv file based on SAR date 
#update version 20230725
#20140906
#20160903
#20170902
#20180929
#20190928
#20200926
#20210925
#20220924

list_ini_chm <- 0
list_ini_cover <- 0
list_ini_sar <- 0
list_ini_sar1 <- 0
list_ini_sar2 <- 0
list_ini_sar3 <- 0


dir <- "E:\\ScanSAR\\ScanSAR\\Result_0625\\site_75m"
sar_date <- "_20180929_75m.xlsx"
list_dir <- list.dirs(dir, recursive=FALSE,full.names = FALSE)
for (l in list_dir){
  print(l)
  dir_index <- file.path(dir,l,"index.csv")
  Data_index = read.csv(dir_index)
  colnames(Data_index)[1] <- "ID"
  
  dir_chm <- file.path(dir,l,paste0("CHM_",l,"_75m.xlsx"))
  Data = read_excel(dir_chm)
  Data_chm = subset(Data, select=c(2,5))
  Data_chm <- rbind(list_ini_chm, Data_chm)
  Data_index <- merge(Data_index, Data_chm, by.x = "ID", by.y = "FID")
  
  dir_cover <- file.path(dir,l,paste0("Cover_",l,"_75m.xlsx"))
  Data = read_excel(dir_cover)
  Data_cover = subset(Data, select=c(2,5))
  Data_cover <- rbind(list_ini_cover, Data_cover)
  Data_index <- merge(Data_index, Data_cover, by.x = "ID", by.y = "FID")
  
  dir_sar <- file.path(dir,l,paste0("ScanSAR_",l,sar_date))
  Data = read_excel(dir_sar)
  Data_sar = subset(Data, select=c(2,5))
  Data_sar <- rbind(list_ini_sar, Data_sar)
  Data_index <- merge(Data_index, Data_sar, by.x = "ID", by.y = "FID")
  
}


Data <- na.omit(Data_index)

colnames(Data) <- c("ID","CHM","Cover","SAR")
Data$Volume <- Data$CHM*5625
Data$Cover <- Data$Cover/5625

Data$AGBD <- 9.0665*Data$CHM*Data$Cover

Data <- Data[is.finite(rowSums(Data)),]
Data <- Data[Data$CHM > 0,]
Data <- Data[Data$Cover > 0,]
Data <- Data[Data$SAR > -25 & Data$SAR < -10,]


#AGBD
random_sample <- createDataPartition(Data$AGBD, p = 0.7, list = FALSE)
training_dataset  <- Data[random_sample, ]
testing_dataset <- Data[-random_sample, ]

reg <- nls(AGBD~ a*10^(b*SAR), data = training_dataset, start = list(a=1800, b=0.2))

testing_dataset$pred <- coef(reg)[1]*10^(coef(reg)[2]*testing_dataset$SAR)

testing_dataset <- testing_dataset[testing_dataset$pred < 100,]

R2 <- round(cor(testing_dataset$pred,testing_dataset$AGBD)^2,3)
bias <- mean(testing_dataset$pred - testing_dataset$AGBD)
RMSE <- sqrt(mean((testing_dataset$pred - testing_dataset$AGBD)^2))
rbias <- 100*mean(testing_dataset$pred - testing_dataset$AGBD)/mean(testing_dataset$AGBD)
rRMSE <- 100*sqrt(mean((testing_dataset$pred - testing_dataset$AGBD)^2))/mean(testing_dataset$AGBD)
n <- nrow(testing_dataset)


ggplot(testing_dataset, aes(x=AGBD, y=pred))+ 
  geom_pointdensity()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(direction = 1)+
  annotate("text", x=1, y=57,hjust = 0,size = 8,
           label= paste(expression(Logarithmic~model~R^2),": ",
                        R2), parse=TRUE) + 
  annotate("text",x=0,y=50,hjust = 0,size = 8,
           label= paste(
             "\n" , " RMSE: ", round(RMSE,3)," Mg/ha",
             "\n" , " rRMSE: ", round(rRMSE,3),"%",
             "\n" , " Bias: ", round(bias,3)," Mg/ha",
             "\n" , " %Bias: ", round(rbias,3)," %",
             "\n" , " Sample size: ", n)) + 
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 60))+
  scale_x_continuous(minor_breaks = round(seq(0,60,10),digits = 1),
                     breaks = round(seq(0,60,10),digits = 1))+
  scale_y_continuous(minor_breaks = round(seq(0,60,10),digits = 1),
                     breaks = round(seq(0,60,10),digits = 1))+
  labs(x="Area-based (H*CC) ALS AGBD (Mg/ha)", 
       y="Predicted AGBD (Mg/ha)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=30))+
  #theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))



out = "E:\\ScanSAR\\ScanSAR\\Result_0625\\Scantterplot_20180929.jpg"
ggsave(out,height=12, width=12, dpi=600)








# ------------------------------------------------------------------------------------------------ #
#08172023

#visualization -- AGB mean
dir <- "E:\\ScanSAR\\ScanSAR\\Result_0630"
Data = read_excel(file.path(dir,"Result_agb.xlsx"), sheet = "Mean")
Data$ID <- Data$ID + 1
Data_subset <- Data[c(2,51,32,4,42,50,52,34,6), c(2,4:11)]
melt_data <- melt(Data_subset, id = c("Site")) 
melt_data$variable <- substring(melt_data$variable, 2)

ggplot(melt_data,aes(x=variable, y=value, group=Site, color=Site)) + 
  geom_line(size=1)+
  geom_point(size=4)+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(text=element_text(size=19))+
  labs(x = "Date of ScanSAR acquisition",
       y = "Mean AGBD (Mg/ha) predicted by ScanSAR backscatter")

out <- "E:\\ScanSAR\\ScanSAR\\Result_0630\\AGB_mean.jpg"
ggsave(out,height=10, width=20, dpi=600)


#visualization -- AGB sum
dir <- "E:\\ScanSAR\\ScanSAR\\Result_0630"
Data = read_excel(file.path(dir,"Result_agb.xlsx"), sheet = "Sum")
Data$ID <- Data$ID + 1
Data_subset <- Data[c(2,51,32,4,42,50,52,34,6), c(2,4:11)]
melt_data <- melt(Data_subset, id = c("Site")) 
melt_data$variable <- substring(melt_data$variable, 2)
melt_data$value <- melt_data$value/10000
ggplot(melt_data,aes(x=variable, y=value, group=Site, color=Site)) + 
  geom_line(size=1)+
  geom_point(size=4)+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(text=element_text(size=20))+
  labs(x = "Date of ScanSAR acquisition",
       y = "Summed AGBD (per 10,000 Mg) predicted by ScanSAR backscatter")

out <- "E:\\ScanSAR\\ScanSAR\\Result_0630\\AGB_sum.jpg"
ggsave(out,height=10, width=25, dpi=600)


#visualization -- AGB mean diff
dir <- "E:\\ScanSAR\\ScanSAR\\Result_0630"
Data = read_excel(file.path(dir,"Result_agb.xlsx"), sheet = "Mean")
Data$ID <- Data$ID + 1
Data_subset <- Data[c(2,51,32,4,42,50,52,34,6), c(2,12:18)]
melt_data <- melt(Data_subset, id = c("Site"))
melt_data$variable <- substring(melt_data$variable, 2)

ggplot(melt_data,aes(x=variable, y=value, group=Site, color=Site)) + 
  geom_line(size=1)+
  geom_point(size=4)+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(text=element_text(size=19))+
  labs(x = "Date of ScanSAR acquisition",
       y = "Difference between mean AGBD (Mg/ha)")

out <- "E:\\ScanSAR\\ScanSAR\\Result_0630\\AGB_mean_diff.jpg"
ggsave(out,height=10, width=20, dpi=600)


#visualization -- AGB mean diff
dir <- "E:\\ScanSAR\\ScanSAR\\Result_0630"
Data = read_excel(file.path(dir,"Result_agb.xlsx"), sheet = "Sum")
Data$ID <- Data$ID + 1
Data_subset <- Data[c(2,51,32,4,42,50,52,34,6), c(2,12:17)]
melt_data <- melt(Data_subset, id = c("Site"))
melt_data$variable <- substring(melt_data$variable, 2)
melt_data$value <- melt_data$value/10000

ggplot(melt_data,aes(x=variable, y=value, group=Site, color=Site)) + 
  geom_line(size=1)+
  geom_point(size=4)+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(text=element_text(size=19))+
  labs(x = "Date of ScanSAR acquisition",
       y = "Difference between summed AGBD (Mg/ha)")

out <- "E:\\ScanSAR\\ScanSAR\\Result_0630\\AGB_sum_diff.jpg"
ggsave(out,height=10, width=20, dpi=600)


#visualization -- AGB mean diff %
dir <- "E:\\ScanSAR\\ScanSAR\\Result_0630"
Data = read_excel(file.path(dir,"Result_agb.xlsx"), sheet = "Mean")
Data$ID <- Data$ID + 1
Data_subset <- Data[c(2,51,32,4,42,50,52,34,6), c(2,19:25)]
melt_data <- melt(Data_subset, id = c("Site"))
melt_data$variable <- substring(melt_data$variable, 2)
melt_data$value <- melt_data$value

ggplot(melt_data,aes(x=variable, y=value, group=Site, color=Site)) + 
  geom_line(size=1)+
  geom_point(size=4)+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(text=element_text(size=19))+
  labs(x = "Date of ScanSAR acquisition",
       y = " %difference between mean AGBD (%)")

out <- "E:\\ScanSAR\\ScanSAR\\Result_0630\\AGB_mean_diff_percent.jpg"
ggsave(out,height=10, width=20, dpi=600)


#visualization -- AGB sum diff %
dir <- "E:\\ScanSAR\\ScanSAR\\Result_0630"
Data = read_excel(file.path(dir,"Result_agb.xlsx"), sheet = "Sum")
Data$ID <- Data$ID + 1
Data_subset <- Data[c(2,51,32,4,42,50,52,34,6), c(2,19:25)]
melt_data <- melt(Data_subset, id = c("Site"))
melt_data$variable <- substring(melt_data$variable, 2)

ggplot(melt_data,aes(x=variable, y=value, group=Site, color=Site)) + 
  geom_line(size=1)+
  geom_point(size=4)+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(text=element_text(size=19))+
  labs(x = "Date of ScanSAR acquisition",
       y = " %difference between summed AGBD (%)")

out <- "E:\\ScanSAR\\ScanSAR\\Result_0630\\AGB_sum_diff_percent.jpg"
ggsave(out,height=10, width=20, dpi=600)



#Rao's Q plot
folder_path <- "E:\\ScanSAR\\ScanSAR\\Result_0607\\Rao\\Sandringham"
files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
for (i in 1:8){
  r <- raster(files[i])
  r_list <- getValues(r)
  print(i)
  print(mean(na.omit(r_list)))
  print(sd(na.omit(r_list)))
}


Data = read_excel(file.path(folder_path,"stats.xlsx"))
ggplot(Data,aes(x=Year, y=Mean,group = 1)) + 
  geom_point(size=4)+
  geom_errorbar(aes(ymin=Mean-STD, ymax=Mean+STD), colour="black", 
                width=.1, position= position_dodge(0.1)) +
  geom_line(size=1)+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(text=element_text(size=19))+
  labs(x = "Date of ScanSAR acquisition",
       y = "Mean Rao's Q index")



#merge all csv
dir_gedi_sar <- "E:\\ScanSAR\\ScanSAR\\Result_1004\\result\\gedi_sar_100m.csv"
data_index = read.csv(dir_gedi_sar)
data_index$rh50 <- data_index$rh50 + 100
list_dir <- list.files("E:\\ScanSAR\\ScanSAR\\Result_1231\\Table_zonal", recursive=FALSE)
for (l in list_dir){
  print(l)
  dir_file <- file.path("E:\\ScanSAR\\ScanSAR\\Result_1231\\Table_zonal",l)
  Data_sar = read.csv(dir_file)
  Data_sar = subset(Data_sar, select=c("Shots","RASTERVALU"))
  colnames(Data_sar)[2] <-  str_sub(l, start =-8)
  data_index <- merge(data_index, Data_sar, by.x = "Shots", by.y = "Shots")
}

write.csv(data_index, "E:\\ScanSAR\\ScanSAR\\Result_1231\\Ras_results\\Results_2014_2022.csv", row.names = F)



# ------------------------------------------------------------------------------------------------ #
#ALS agbd model to different SAR
filedir <- "E:\\ScanSAR\\ScanSAR\\Result_1004\\result\\CSIR_AGBD_1ha.csv"
Data = read.csv(filedir)

dir_hcc_gedi <- "E:\\ScanSAR\\ScanSAR\\Result_1004\\result\\hcc_gedi_1ha.csv"
data_hcc_gedi = read.csv(dir_hcc_gedi)
data_hcc_gedi$rh50 <- data_hcc_gedi$rh50 + 100
df_HCC <- data.frame(data_hcc_gedi[,"HCC"])
colnames(df_HCC) <- c("HCC")

lidar_model <- lm(formula = CSIR ~ HCC, Data)

lidar_est <- predict(lidar_model, newdata = df_HCC)
lidar_est_df_good <- data.frame(cbind(data_hcc_gedi,data.frame(lidar_est)))
lidar_est_df_good <- lidar_est_df_good[lidar_est_df_good$lidar_est > 0,]
cor(lidar_est_df_good$lidar_est,lidar_est_df_good$agbd)

#glm - different SAR year
dir_gedi_sar <- "E:\\ScanSAR\\ScanSAR\\Result_1231\\Ras_results\\Results_2014_2022.csv"
data_gedi_sar = read.csv(dir_gedi_sar)
data_gedi_sar_var_glm = data_gedi_sar[,c("rh50","rh98","fhd")]

formula <- as.formula("lidar_est ~ fhd + rh50 + rh98")
repeat_cv <- trainControl(method='cv', number=5)
gedi_model <- train(
  formula, 
  data=lidar_est_df_good, 
  method='glm', 
  family = Gamma(link = "identity"),
  start=c(-600,-2,6,2),
  trControl=repeat_cv)

gedi_est <- predict(gedi_model, newdata = data_gedi_sar_var_glm)
gedi_est_df_good <- data.frame(cbind(data_gedi_sar,data.frame(gedi_est)))
gedi_est_df_good <- na.omit(gedi_est_df_good)
cor(gedi_est_df_good$agbd,gedi_est_df_good$gedi_est)

raster_df_1m <- 
  data.frame(
    data.frame(
      rasterToPoints(
        raster("E:\\ScanSAR\\ScanSAR\\Result_1231\\Ras_100\\Ras_2022_100.tif"))[,3]))

raster_xy_df_1m <- 
  data.frame(
    data.frame(
      rasterToPoints(
        raster("E:\\ScanSAR\\ScanSAR\\Result_1231\\Ras_100\\Ras_2022_100.tif"))[,1:2]))

colnames(raster_df_1m) <- c("R2022")

suf <- "SAR_AGBD_2022"
sar_model <- lm(formula = log(gedi_est) ~ R2022, gedi_est_df_good)
summary(sar_model)
sar_est <- predict(sar_model, newdata = raster_df_1m)
sar_est_df <- exp(data.frame(sar_est))
result_ras <- cbind(raster_xy_df_1m,data.frame(sar_est_df))
file_path_ras <- file.path("E:\\ScanSAR\\ScanSAR\\Result_1231\\Ras_results_all",paste0(suf,".csv"))
write.csv(result_ras, file = file_path_ras)

dir <- "E:\\ScanSAR\\ScanSAR\\Result_1231\\Ras_results_all"
filedir1 <- file.path(dir,paste0(suf,".csv"))
Data = read.csv(filedir1)
colnames(Data) <- c("Index","X","Y","AGBD")
a <- rasterFromXYZ(Data[,c("X","Y","AGBD")])
output <- file.path(dir,paste0(suf,".tif"))
writeRaster(a, output, format = "GTiff",overwrite=TRUE)

dir <- "E:\\ScanSAR\\ScanSAR\\Result_1231\\Ras_results_all"
raster_files <- list.files(path = dir, pattern = "\\.tif$", full.names = TRUE)
raster_stack <- stack(raster_files)
writeRaster(raster_stack, filename=file.path(dir,"stack.nc"), format="CDF", overwrite=TRUE)   

