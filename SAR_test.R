#Xiaoxuan Li #20210830
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
library(plot3D)
library(lattice)
library(latticeExtra)
library(caret)




#individual site test --R2
dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))

Data_Agincourt <- Data[Data$Name == "Agincourt",]
Data_Agincourt <- na.omit(Data_Agincourt)
Data_Agincourt$diff_C2_SAR_5 = Data_Agincourt$HV.x - Data_Agincourt$HV.y
Data_Agincourt$diff_C2_ALS_5 = Data_Agincourt$C2.x - Data_Agincourt$C2.y
round(cor(Data_Agincourt$diff_C2_SAR_5,Data_Agincourt$diff_C2_ALS_5)^2,3)

Data_Ireagh <- Data[Data$Name == "Ireagh",]
Data_Ireagh <- na.omit(Data_Ireagh)
Data_Ireagh$diff_C2_SAR_5 = Data_Ireagh$HV.x - Data_Ireagh$HV.y
Data_Ireagh$diff_C2_ALS_5 = Data_Ireagh$C2.x - Data_Ireagh$C2.y
round(cor(Data_Ireagh$diff_C2_SAR_5,Data_Ireagh$diff_C2_ALS_5)^2,3)

Data_Justicia <- Data[Data$Name == "Justicia",]
Data_Justicia <- na.omit(Data_Justicia)
Data_Justicia$diff_C2_SAR_5 = Data_Justicia$HV.x - Data_Justicia$HV.y
Data_Justicia$diff_C2_ALS_5 = Data_Justicia$C2.x - Data_Justicia$C2.y
round(cor(Data_Justicia$diff_C2_SAR_5,Data_Justicia$diff_C2_ALS_5)^2,3)

Data_W <- Data[Data$Name == "W",]
Data_W <- na.omit(Data_W)
Data_W$diff_C2_SAR_5 = Data_W$HV.x - Data_W$HV.y
Data_W$diff_C2_ALS_5 = Data_W$C2.x - Data_W$C2.y
round(cor(Data_W$diff_C2_SAR_5,Data_W$diff_C2_ALS_5)^2,3)



# ------------------------------------------------------------------------------------------------ #
#individual site test -- SAR HV vs. Cover change
dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))

Data <- Data[Data$Name == "Agincourt",]


Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$HV.x - Data$HV.y)*2
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.5,0.5,0.25),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p1 <- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(
    x = paste0("ALS cover change (2018-2008)"),
    y = paste0("SAR HV change (dB) (2018-2008) (Agincourt)")) +
  theme_bw()+
  coord_cartesian(ylim = c(-20, 20))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
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

Data <- Data[Data$Name == "Ireagh",]


Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$HV.x - Data$HV.y)*2
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.5,0.5,0.25),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p2 <- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(
    x = paste0("ALS cover change (2018-2008)"),
    y = paste0("SAR HV change (dB) (2018-2008) (Ireagh)")) +
  theme_bw()+
  coord_cartesian(ylim = c(-20, 20))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
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

Data <- Data[Data$Name == "Justicia",]


Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$HV.x - Data$HV.y)*2
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.5,0.5,0.25),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p3 <- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(
    x = paste0("ALS cover change (2018-2008)"),
    y = paste0("SAR HV change (dB) (2018-2008)  (Justicia)")) +
  theme_bw()+
  coord_cartesian(ylim = c(-20, 20))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p2,p3,ncol=3)





dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))

Data <- Data[Data$Name == "Justicia",]

Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$HV.x - Data$HV.y)*2
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.5,0.5,0.25),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p1<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(
    x = paste0("ALS cover change (2018-2010)"),
    y = paste0("SAR HV change (dB) (2018-2010) (Justicia)")) +
  theme_bw()+
  coord_cartesian(ylim = c(-20, 20))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
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

Data <- Data[Data$Name == "W",]

Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$HV.x - Data$HV.y)*2
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.5,0.5,0.25),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p2<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(
    x = paste0("ALS cover change (2018-2010)"),
    y = paste0("SAR HV change (dB) (2018-2010) (And+Wel)")) +
  theme_bw()+
  coord_cartesian(ylim = c(-20, 20))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(plot.title = element_text(hjust = 0.5))


ggarrange(p1,p2,ncol=2)





dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))

Data <- Data[Data$Name == "Justicia",]

Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$HV.x - Data$HV.y)*2
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.5,0.5,0.25),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(
    x = paste0("ALS cover change (2010-2008)"),
    y = paste0("SAR HV change (dB) (2010-2008) (Justicia)")) +
  theme_bw()+
  coord_cartesian(ylim = c(-20, 20))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(plot.title = element_text(hjust = 0.5))



# ------------------------------------------------------------------------------------------------ #
#individual site test -- SAR predicted Cover change vs. Cover change
dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))

Data <- Data[Data$Name == "Agincourt",]

Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.5,0.5,0.25),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.5 & Data$diff_C2_SAR_5 < 0.5,]
p1<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(
    x = paste0("ALS cover change (2018-2008)"),
    y = paste0("Bayesian WCM cover change (2018-2008) (Agincourt)")) +
  theme_bw()+
  scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
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

Data <- Data[Data$Name == "Ireagh",]

Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.5,0.5,0.25),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.5 & Data$diff_C2_SAR_5 < 0.5,]
p2<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(
    x = paste0("ALS cover change (2018-2008)"),
    y = paste0("Bayesian WCM cover change (2018-2008) (Ireagh)")) +
  theme_bw()+
  scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
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

Data <- Data[Data$Name == "Justicia",]

Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.5,0.5,0.25),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.5 & Data$diff_C2_SAR_5 < 0.5,]
p3<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(
    x = paste0("ALS cover change (2018-2008)"),
    y = paste0("Bayesian WCM cover change (2018-2008) (Justicia)")) +
  theme_bw()+
  scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(plot.title = element_text(hjust = 0.5))


ggarrange(p1,p2,p3,ncol =3)




dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2010.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))

Data <- Data[Data$Name == "Justicia",]

Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.5,0.5,0.25),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.5 & Data$diff_C2_SAR_5 < 0.5,]
p1<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(
    x = paste0("ALS cover change (2018-2010)"),
    y = paste0("Bayesian WCM cover change (2018-2010) (Justicia)")) +
  theme_bw()+
  scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
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

Data <- Data[Data$Name == "W",]

Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.5,0.5,0.25),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.5 & Data$diff_C2_SAR_5 < 0.5,]
p2<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(
    x = paste0("ALS cover change (2018-2010)"),
    y = paste0("Bayesian WCM cover change (2018-2010) (And+Wel)")) +
  theme_bw()+
  scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p2,ncol =2)




dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2010.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))

Data <- Data[Data$Name == "Justicia",]

Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.5,0.5,0.25),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.5 & Data$diff_C2_SAR_5 < 0.5,]
ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(
    x = paste0("ALS cover change (2010-2008)"),
    y = paste0("Bayesian WCM cover change (2010-2008) (Justicia)")) +
  theme_bw()+
  scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(plot.title = element_text(hjust = 0.5))


# ------------------------------------------------------------------------------------------------ #
#3d: x: ALS cover change, y: ALS height change, z: frequency

dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2010.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2008.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))

Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_V2_ALS_5 = (Data$P_V_5.x - Data$P_V_5.y)/5250
Data <- na.omit(Data)
Data$Cover <- cut(Data$diff_C2_ALS_5,breaks = round(seq(-0.6,0.6,0.1),2),dig.lab=1)
Data$Height <- cut(Data$diff_V2_ALS_5,breaks = round(seq(-1,1,0.2),2),dig.lab=1)

a <- data.frame(rename(count(Data, Cover,Height), Freq = n))
a <- na.omit(a)

cloud(Freq~Cover+Height, a, panel.3d.cloud=panel.3dbars, col.facet='grey',   
      screen = list(z =-50, x = -60), xbase=0.8, ybase=0.8, 
      scales=list(arrows=FALSE, col=1))

b<-xtabs(Freq ~ Cover + Height, a)
write.csv(b,file="E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\3d_R_20102008.csv")





# ------------------------------------------------------------------------------------------------ #
#3d: x: ALS cover change, y: ALS height change, z: SAR

dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_Cover_5 = Data$C2.x - Data$C2.y
Data$diff_Volume_5 = (Data$V2.x - Data$V2.y)/5250
Data$SAR = Data$HV.x - Data$HV.y
Data$Cover <- cut(Data$diff_Cover_5,breaks = round(seq(-0.6,0.6,0.1),2),dig.lab=1)
Data$Height <- cut(Data$diff_Volume_5,breaks = round(seq(-1,1,0.2),2),dig.lab=1)


a<-aggregate(x=Data$SAR,by=list(Data$Cover,Data$Height),FUN=mean)


cloud(x~Group.1+Group.2, a, panel.3d.cloud=panel.3dbars, col.facet='grey',   
      screen = list(z =-50, x = -80), xbase=0.8, ybase=0.8, 
      scales=list(arrows=FALSE, col=1))

b<-xtabs(x ~ Group.1 + Group.2, a)
write.csv(b,file="E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\3d_R_SAR_20182008.csv")


round(cor(Data$diff_Cover_5,Data$diff_Volume_5)^2,3)

ggplot(Data, aes(x=diff_Cover_5, y=diff_Volume_5))+ 
  geom_pointdensity()+
  scale_color_viridis(direction = 1)+
  labs(x = paste0("LiDAR CHM cover change"),
       y = paste0("LiDAR CHM height change")) +
  theme(text=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5))



# ------------------------------------------------------------------------------------------------ #
#correlation
i <- "E:\\ChangMap\\CHM\\DB_20220221\\DB_csv\\Ind_2018.csv"
Data = read.csv(i,header=T)
x <- Data$H2
y<- Data$HV

reg1 <- lm(y ~ x, data = Data)
round(summary(reg1)$adj.r.squared,3)


reg2 <- lm(y ~ log(x), data = Data)
round(summary(reg2)$adj.r.squared,3)


mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-25, b=-12, c=9e-5))
y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
  coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
round(cor(y,y_sim)^2,3)



# ------------------------------------------------------------------------------------------------ #

#11142021
#composite plots
i_1 <- "E:\\ChangMap\\CHM\\DB_20220221\\DB_csv\\Ind_2008.csv"
Data1 = read.csv(i_1,header=T)
Data1 <- Data1[Data1$H2 > 0,]
#Data1 <- Data1[sample(nrow(Data1), 1000), ]
x1 <- Data1$H2
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

i_2 <- "E:\\ChangMap\\CHM\\DB_20220221\\DB_csv\\Ind_2010.csv"
Data2 = read.csv(i_2,header=T)
Data2 <- Data2[Data2$H2 > 0,]
#Data2 <- Data2[sample(nrow(Data2), 1000), ]
x2 <- Data2$H2
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

i_3 <- "E:\\ChangMap\\CHM\\DB_20220221\\DB_csv\\Ind_2018.csv"
Data3 = read.csv(i_3,header=T)
Data3 <- Data3[Data3$H2 > 0,]
#Data3 <- Data3[sample(nrow(Data3), 1000), ]
x3 <- Data3$H2
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
out = "E:\\ChangMap\\CHM\\DB_20220221\\figure scatterplot composite.jpg"
ggsave(out,height=9, width=18, dpi=600)




#20220306 SAR power-db test
ind_cover_linear <- function(){
  #2007 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2008.csv"
  Data = read.csv(i,header=T)
  x <- Data$C2
  y<- Data$HV7
  reg <- lm(y ~ x, data = Data)
  R2_HV_2007 <- round(summary(reg)$adj.r.squared,3)
  #2008 HV 
  x <- Data$C2
  y<- Data$HV
  reg <- lm(y ~ x, data = Data)
  R2_HV_2008 <- round(summary(reg)$adj.r.squared,3)
  #2010 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2010.csv"
  Data = read.csv(i,header=T)
  x <- Data$C2
  y<- Data$HV
  reg <- lm(y ~ x, data = Data)
  R2_HV_2010 <- round(summary(reg)$adj.r.squared,3)
  #2017 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2018.csv"
  Data = read.csv(i,header=T)
  x <- Data$C2
  y<- Data$HV7
  reg <- lm(y ~ x, data = Data)
  R2_HV_2017 <- round(summary(reg)$adj.r.squared,3)
  #2018 HV 
  x <- Data$C2
  y<- Data$HV
  reg <- lm(y ~ x, data = Data)
  R2_HV_2018 <- round(summary(reg)$adj.r.squared,3)
  
  #2007 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2008.csv"
  Data = read.csv(i,header=T)
  x <- Data$C2
  y<- Data$HH7
  reg <- lm(y ~ x, data = Data)
  R2_HH_2007 <- round(summary(reg)$adj.r.squared,3)
  #2008 HH 
  x <- Data$C2
  y<- Data$HH
  reg <- lm(y ~ x, data = Data)
  R2_HH_2008 <- round(summary(reg)$adj.r.squared,3)
  #2010 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2010.csv"
  Data = read.csv(i,header=T)
  x <- Data$C2
  y<- Data$HH
  reg <- lm(y ~ x, data = Data)
  R2_HH_2010 <- round(summary(reg)$adj.r.squared,3)
  #2017 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2018.csv"
  Data = read.csv(i,header=T)
  x <- Data$C2
  y<- Data$HH7
  reg <- lm(y ~ x, data = Data)
  R2_HH_2017 <- round(summary(reg)$adj.r.squared,3)
  #2018 HH 
  x <- Data$C2
  y<- Data$HH
  reg <- lm(y ~ x, data = Data)
  R2_HH_2018 <- round(summary(reg)$adj.r.squared,3)
  cat(R2_HH_2007,R2_HH_2008,R2_HH_2010,R2_HH_2017,R2_HH_2018,sep="\n")
  cat(R2_HV_2007,R2_HV_2008,R2_HV_2010,R2_HV_2017,R2_HV_2018,sep="\n")
}

ind_cover_log <- function(){
  #2007 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2008.csv"
  Data = read.csv(i,header=T)
  Data <- Data[Data$C2 > 0,]
  x <- Data$C2
  y<- Data$HV7
  reg <- lm(y ~ log(x), data = Data)
  R2_HV_2007 <- round(summary(reg)$adj.r.squared,3)
  #2008 HV 
  x <- Data$C2
  y<- Data$HV
  reg <- lm(y ~ log(x), data = Data)
  R2_HV_2008 <- round(summary(reg)$adj.r.squared,3)
  #2010 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2010.csv"
  Data = read.csv(i,header=T)
  Data <- Data[Data$C2 > 0,]
  x <- Data$C2
  y<- Data$HV
  reg <- lm(y ~ log(x), data = Data)
  R2_HV_2010 <- round(summary(reg)$adj.r.squared,3)
  #2017 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2018.csv"
  Data = read.csv(i,header=T)
  Data <- Data[Data$C2 > 0,]
  x <- Data$C2
  y<- Data$HV7
  reg <- lm(y ~ log(x), data = Data)
  R2_HV_2017 <- round(summary(reg)$adj.r.squared,3)
  #2018 HV 
  x <- Data$C2
  y<- Data$HV
  reg <- lm(y ~ log(x), data = Data)
  R2_HV_2018 <- round(summary(reg)$adj.r.squared,3)
  
  #2007 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2008.csv"
  Data = read.csv(i,header=T)
  Data <- Data[Data$C2 > 0,]
  x <- Data$C2
  y<- Data$HH7
  reg <- lm(y ~ log(x), data = Data)
  R2_HH_2007 <- round(summary(reg)$adj.r.squared,3)
  #2008 HH 
  x <- Data$C2
  y<- Data$HH
  reg <- lm(y ~ log(x), data = Data)
  R2_HH_2008 <- round(summary(reg)$adj.r.squared,3)
  #2010 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2010.csv"
  Data = read.csv(i,header=T)
  Data <- Data[Data$C2 > 0,]
  x <- Data$C2
  y<- Data$HH
  reg <- lm(y ~ log(x), data = Data)
  R2_HH_2010 <- round(summary(reg)$adj.r.squared,3)
  #2017 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2018.csv"
  Data = read.csv(i,header=T)
  Data <- Data[Data$C2 > 0,]
  x <- Data$C2
  y<- Data$HH7
  reg <- lm(y ~ log(x), data = Data)
  R2_HH_2017 <- round(summary(reg)$adj.r.squared,3)
  #2018 HH 
  x <- Data$C2
  y<- Data$HH
  reg <- lm(y ~ log(x), data = Data)
  R2_HH_2018 <- round(summary(reg)$adj.r.squared,3)
  cat(R2_HH_2007,R2_HH_2008,R2_HH_2010,R2_HH_2017,R2_HH_2018,sep="\n")
  cat(R2_HV_2007,R2_HV_2008,R2_HV_2010,R2_HV_2017,R2_HV_2018,sep="\n")
}

ind_cover_wcm <- function(){
  #2007 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2008.csv"
  Data = read.csv(i,header=T)
  x <- Data$C2
  y<- Data$HV7
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-20, b=-7, c=2))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HV_2007 <- round(cor(y,y_sim)^2,3)
  #2008 HV 
  x <- Data$C2
  y<- Data$HV
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-20, b=-7, c=2))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HV_2008 <- round(cor(y,y_sim)^2,3)
  #2010 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2010.csv"
  Data = read.csv(i,header=T)
  x <- Data$C2
  y<- Data$HV
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-20, b=-7, c=2))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HV_2010 <- round(cor(y,y_sim)^2,3)
  #2017 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2018.csv"
  Data = read.csv(i,header=T)
  x <- Data$C2
  y<- Data$HV7
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-20, b=-7, c=2))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HV_2017 <- round(cor(y,y_sim)^2,3)
  #2018 HV 
  x <- Data$C2
  y<- Data$HV
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-20, b=-7, c=2))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HV_2018 <- round(cor(y,y_sim)^2,3)
  
  #2007 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2008.csv"
  Data = read.csv(i,header=T)
  x <- Data$C2
  y<- Data$HH7
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-20, b=-7, c=2))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HH_2007 <- round(cor(y,y_sim)^2,3)
  #2008 HH 
  x <- Data$C2
  y<- Data$HH
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-20, b=-7, c=2))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HH_2008 <- round(cor(y,y_sim)^2,3)
  #2010 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2010.csv"
  Data = read.csv(i,header=T)
  x <- Data$C2
  y<- Data$HH
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-20, b=-7, c=2))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HH_2010 <- round(cor(y,y_sim)^2,3)
  #2017 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2018.csv"
  Data = read.csv(i,header=T)
  x <- Data$C2
  y<- Data$HH7
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-20, b=-7, c=2))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HH_2017 <- round(cor(y,y_sim)^2,3)
  #2018 HH 
  x <- Data$C2
  y<- Data$HH
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-20, b=-7, c=2))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HH_2018 <- round(cor(y,y_sim)^2,3)
  cat(R2_HH_2007,R2_HH_2008,R2_HH_2010,R2_HH_2017,R2_HH_2018,sep="\n")
  cat(R2_HV_2007,R2_HV_2008,R2_HV_2010,R2_HV_2017,R2_HV_2018,sep="\n")
}



ind_volume_linear <- function(){
  #2007 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2008.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HV7
  reg <- lm(y ~ x, data = Data)
  R2_HV_2007 <- round(summary(reg)$adj.r.squared,3)
  #2008 HV 
  x <- Data$V2
  y<- Data$HV
  reg <- lm(y ~ x, data = Data)
  R2_HV_2008 <- round(summary(reg)$adj.r.squared,3)
  #2010 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2010.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HV
  reg <- lm(y ~ x, data = Data)
  R2_HV_2010 <- round(summary(reg)$adj.r.squared,3)
  #2017 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2018.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HV7
  reg <- lm(y ~ x, data = Data)
  R2_HV_2017 <- round(summary(reg)$adj.r.squared,3)
  #2018 HV 
  x <- Data$V2
  y<- Data$HV
  reg <- lm(y ~ x, data = Data)
  R2_HV_2018 <- round(summary(reg)$adj.r.squared,3)
  
  #2007 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2008.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HH7
  reg <- lm(y ~ x, data = Data)
  R2_HH_2007 <- round(summary(reg)$adj.r.squared,3)
  #2008 HH 
  x <- Data$V2
  y<- Data$HH
  reg <- lm(y ~ x, data = Data)
  R2_HH_2008 <- round(summary(reg)$adj.r.squared,3)
  #2010 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2010.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HH
  reg <- lm(y ~ x, data = Data)
  R2_HH_2010 <- round(summary(reg)$adj.r.squared,3)
  #2017 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2018.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HH7
  reg <- lm(y ~ x, data = Data)
  R2_HH_2017 <- round(summary(reg)$adj.r.squared,3)
  #2018 HH 
  x <- Data$V2
  y<- Data$HH
  reg <- lm(y ~ x, data = Data)
  R2_HH_2018 <- round(summary(reg)$adj.r.squared,3)
  cat(R2_HH_2007,R2_HH_2008,R2_HH_2010,R2_HH_2017,R2_HH_2018,sep="\n")
  cat(R2_HV_2007,R2_HV_2008,R2_HV_2010,R2_HV_2017,R2_HV_2018,sep="\n")
}

ind_volume_log <- function(){
  #2007 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2008.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HV7
  reg <- lm(y ~ log(x), data = Data)
  R2_HV_2007 <- round(summary(reg)$adj.r.squared,3)
  #2008 HV 
  x <- Data$V2
  y<- Data$HV
  reg <- lm(y ~ log(x), data = Data)
  R2_HV_2008 <- round(summary(reg)$adj.r.squared,3)
  #2010 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2010.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HV
  reg <- lm(y ~ log(x), data = Data)
  R2_HV_2010 <- round(summary(reg)$adj.r.squared,3)
  #2017 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2018.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HV7
  reg <- lm(y ~ log(x), data = Data)
  R2_HV_2017 <- round(summary(reg)$adj.r.squared,3)
  #2018 HV 
  x <- Data$V2
  y<- Data$HV
  reg <- lm(y ~ log(x), data = Data)
  R2_HV_2018 <- round(summary(reg)$adj.r.squared,3)
  
  #2007 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2008.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HH7
  reg <- lm(y ~ log(x), data = Data)
  R2_HH_2007 <- round(summary(reg)$adj.r.squared,3)
  #2008 HH 
  x <- Data$V2
  y<- Data$HH
  reg <- lm(y ~ log(x), data = Data)
  R2_HH_2008 <- round(summary(reg)$adj.r.squared,3)
  #2010 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2010.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HH
  reg <- lm(y ~ log(x), data = Data)
  R2_HH_2010 <- round(summary(reg)$adj.r.squared,3)
  #2017 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2018.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HH7
  reg <- lm(y ~ log(x), data = Data)
  R2_HH_2017 <- round(summary(reg)$adj.r.squared,3)
  #2018 HH 
  x <- Data$V2
  y<- Data$HH
  reg <- lm(y ~ log(x), data = Data)
  R2_HH_2018 <- round(summary(reg)$adj.r.squared,3)
  cat(R2_HH_2007,R2_HH_2008,R2_HH_2010,R2_HH_2017,R2_HH_2018,sep="\n")
  cat(R2_HV_2007,R2_HV_2008,R2_HV_2010,R2_HV_2017,R2_HV_2018,sep="\n")
}

ind_volume_wcm <- function(){
  #2007 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2008.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HV7
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-25, b=-12, c=1e-3))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HV_2007 <- round(cor(y,y_sim)^2,3)
  #2008 HV 
  x <- Data$V2
  y<- Data$HV
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-25, b=-12, c=1e-3))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HV_2008 <- round(cor(y,y_sim)^2,3)
  #2010 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2010.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HV
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-25, b=-12, c=1e-3))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HV_2010 <- round(cor(y,y_sim)^2,3)
  #2017 HV 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2018.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HV7
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-25, b=-12, c=1e-3))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HV_2017 <- round(cor(y,y_sim)^2,3)
  #2018 HV 
  x <- Data$V2
  y<- Data$HV
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-25, b=-12, c=1e-3))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HV_2018 <- round(cor(y,y_sim)^2,3)
  
  #2007 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2008.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HH7
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-25, b=-12, c=1e-3))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HH_2007 <- round(cor(y,y_sim)^2,3)
  #2008 HH 
  x <- Data$V2
  y<- Data$HH
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-25, b=-12, c=1e-3))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HH_2008 <- round(cor(y,y_sim)^2,3)
  #2010 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2010.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HH
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-25, b=-12, c=1e-3))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HH_2010 <- round(cor(y,y_sim)^2,3)
  #2017 HH 
  i <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2018.csv"
  Data = read.csv(i,header=T)
  x <- Data$V2
  y<- Data$HH7
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-25, b=-12, c=1e-3))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HH_2017 <- round(cor(y,y_sim)^2,3)
  #2018 HH 
  x <- Data$V2
  y<- Data$HH
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-25, b=-12, c=1e-3))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2_HH_2018 <- round(cor(y,y_sim)^2,3)
  cat(R2_HH_2007,R2_HH_2008,R2_HH_2010,R2_HH_2017,R2_HH_2018,sep="\n")
  cat(R2_HV_2007,R2_HV_2008,R2_HV_2010,R2_HV_2017,R2_HV_2018,sep="\n")
}



change_linear <- function(){
  #2018-2008
  dir1 <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2018.csv"
  dir2 <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2008.csv"
  Data1 = read.csv(dir1,header=T)
  Data2 = read.csv(dir2,header=T)
  Data = merge(Data1,Data2,by=c("Merge_ID"))
  #2018-2008 volume
  x <- Data$V2.x
  y<- Data$HV.x
  reg <- lm(x ~ y, data = Data)
  pred.x <- predict(reg)
  x <- Data$V2.y
  y<- Data$HV.y
  reg <- lm(x ~ y, data = Data)
  pred.y <- predict(reg)
  Data$diff_V_ALS = Data$V2.x - Data$V2.y
  Data$diff_V_SAR = pred.x - pred.y
  V_2018_2008 <- round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
  #2017-2007 volume 
  x <- Data$V2.x
  y<- Data$HV7.x
  reg <- lm(x ~ y, data = Data)
  pred.x <- predict(reg)
  x <- Data$V2.y
  y<- Data$HV7.y
  reg <- lm(x ~ y, data = Data)
  pred.y <- predict(reg)
  Data$diff_V_ALS = Data$V2.x - Data$V2.y
  Data$diff_V_SAR = pred.x - pred.y
  V_2017_2007 <- round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
  
  #2018-2008 cover
  x <- Data$C2.x
  y<- Data$HV.x
  reg <- lm(x ~ y, data = Data)
  pred.x <- predict(reg)
  x <- Data$C2.y
  y<- Data$HV.y
  reg <- lm(x ~ y, data = Data)
  pred.y <- predict(reg)
  Data$diff_V_ALS = Data$C2.x - Data$C2.y
  Data$diff_V_SAR = pred.x - pred.y
  C_2018_2008 <- round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
  #2017-2007 cover 
  x <- Data$C2.x
  y<- Data$HV7.x
  reg <- lm(x ~ y, data = Data)
  pred.x <- predict(reg)
  x <- Data$C2.y
  y<- Data$HV7.y
  reg <- lm(x ~ y, data = Data)
  pred.y <- predict(reg)
  Data$diff_V_ALS = Data$C2.x - Data$C2.y
  Data$diff_V_SAR = pred.x - pred.y
  C_2017_2007 <- round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
  
  dir1 <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2018.csv"
  dir2 <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2010.csv"
  Data1 = read.csv(dir1,header=T)
  Data2 = read.csv(dir2,header=T)
  Data = merge(Data1,Data2,by=c("Merge_ID"))
  #2018-2010 volume
  x <- Data$V2.x
  y<- Data$HV.x
  reg <- lm(x ~ y, data = Data)
  pred.x <- predict(reg)
  x <- Data$V2.y
  y<- Data$HV.y
  reg <- lm(x ~ y, data = Data)
  pred.y <- predict(reg)
  Data$diff_V_ALS = Data$V2.x - Data$V2.y
  Data$diff_V_SAR = pred.x - pred.y
  V_2018_2010 <- round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
  #2018-2010 cover
  x <- Data$C2.x
  y<- Data$HV.x
  reg <- lm(x ~ y, data = Data)
  pred.x <- predict(reg)
  x <- Data$C2.y
  y<- Data$HV.y
  reg <- lm(x ~ y, data = Data)
  pred.y <- predict(reg)
  Data$diff_V_ALS = Data$C2.x - Data$C2.y
  Data$diff_V_SAR = pred.x - pred.y
  C_2018_2010 <- round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
  
  dir1 <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2010.csv"
  dir2 <- "E:\\ChangMap\\CHM\\DB_20220306\\DB_csv\\Merge_2008.csv"
  Data1 = read.csv(dir1,header=T)
  Data2 = read.csv(dir2,header=T)
  Data = merge(Data1,Data2,by=c("Merge_ID"))
  #2010-2008 volume
  x <- Data$V2.x
  y<- Data$HV.x
  reg <- lm(x ~ y, data = Data)
  pred.x <- predict(reg)
  x <- Data$V2.y
  y<- Data$HV.y
  reg <- lm(x ~ y, data = Data)
  pred.y <- predict(reg)
  Data$diff_V_ALS = Data$V2.x - Data$V2.y
  Data$diff_V_SAR = pred.x - pred.y
  V_2010_2008 <- round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
  
  #2010-2008 cover
  x <- Data$C2.x
  y<- Data$HV.x
  reg <- lm(x ~ y, data = Data)
  pred.x <- predict(reg)
  x <- Data$C2.y
  y<- Data$HV.y
  reg <- lm(x ~ y, data = Data)
  pred.y <- predict(reg)
  Data$diff_V_ALS = Data$C2.x - Data$C2.y
  Data$diff_V_SAR = pred.x - pred.y
  C_2010_2008 <- round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
  cat(V_2018_2008,C_2018_2008,V_2017_2007,C_2017_2007,
      V_2018_2010,C_2018_2010,V_2010_2008,C_2010_2008,sep="\n")
}








# ------------------------------------------------------------------------------------------------ #
#70 training/30 prediction 30 repeat, inverted linear
#2007 volume
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2008.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$V2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  model <- lm(V2 ~ HV7, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$V2)
  bias[l] <- mean(predictions - testing_dataset$V2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$V2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$V2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$V2)^2))/mean(testing_dataset$V2)
}
cat("2007 volume",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))
#2008 volume
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2008.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$V2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  model <- lm(V2 ~ HV, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$V2)
  bias[l] <- mean(predictions - testing_dataset$V2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$V2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$V2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$V2)^2))/mean(testing_dataset$V2)
}
cat("2008 volume",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))
#2010 volume
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2010.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$V2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  model <- lm(V2 ~ HV, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$V2)
  bias[l] <- mean(predictions - testing_dataset$V2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$V2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$V2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$V2)^2))/mean(testing_dataset$V2)
}
cat("2010 volume",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))
#2017 volume
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2018.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$V2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  model <- lm(V2 ~ HV7, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$V2)
  bias[l] <- mean(predictions - testing_dataset$V2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$V2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$V2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$V2)^2))/mean(testing_dataset$V2)
}
cat("2017 volume",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))
#2018 volume
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2018.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$V2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  model <- lm(V2 ~ HV, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$V2)
  bias[l] <- mean(predictions - testing_dataset$V2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$V2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$V2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$V2)^2))/mean(testing_dataset$V2)
}
cat("2018 volume",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))








# ------------------------------------------------------------------------------------------------ #
#70 training/30 prediction 30 repeat, inverted log
#2007 volume
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2008.csv"
Data = read.csv(i,header=T)
Data$V2 <- Data$V2 + 7000
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$V2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  #volume a=80000,b=0.1
  model <- nls(V2 ~ a*exp(b*HV7),data=training_dataset, start = list(a=80000, b=0.1))
  predictions <- predict(model, newdata = testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$V2)
  bias[l] <- mean(predictions - testing_dataset$V2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$V2-7000)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$V2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$V2)^2))/mean(testing_dataset$V2-7000)
}
cat("2007 volume",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))
#2008 volume
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2008.csv"
Data = read.csv(i,header=T)
Data$V2 <- Data$V2 + 7000
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$V2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  #volume a=80000,b=0.1
  model <- nls(V2 ~ a*exp(b*HV),data=training_dataset, start = list(a=80000, b=0.1))
  predictions <- predict(model, newdata = testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$V2)
  bias[l] <- mean(predictions - testing_dataset$V2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$V2-7000)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$V2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$V2)^2))/mean(testing_dataset$V2-7000)
}
cat("2008 volume",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))
#2010 volume
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2010.csv"
Data = read.csv(i,header=T)
Data$V2 <- Data$V2 + 7000
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$V2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  #volume a=80000,b=0.1
  model <- nls(V2 ~ a*exp(b*HV),data=training_dataset, start = list(a=80000, b=0.1))
  predictions <- predict(model, newdata = testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$V2)
  bias[l] <- mean(predictions - testing_dataset$V2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$V2-7000)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$V2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$V2)^2))/mean(testing_dataset$V2-7000)
}
cat("2010 volume",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))
#2017 volume
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2018.csv"
Data = read.csv(i,header=T)
Data$V2 <- Data$V2 + 7000
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$V2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  #volume a=80000,b=0.1
  model <- nls(V2 ~ a*exp(b*HV7),data=training_dataset, start = list(a=80000, b=0.1))
  predictions <- predict(model, newdata = testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$V2)
  bias[l] <- mean(predictions - testing_dataset$V2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$V2-7000)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$V2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$V2)^2))/mean(testing_dataset$V2-7000)
}
cat("2017 volume",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))
#2018 volume
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2018.csv"
Data = read.csv(i,header=T)
Data$V2 <- Data$V2 + 7000
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$V2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  #volume a=80000,b=0.1
  model <- nls(V2 ~ a*exp(b*HV),data=training_dataset, start = list(a=80000, b=0.1))
  predictions <- predict(model, newdata = testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$V2)
  bias[l] <- mean(predictions - testing_dataset$V2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$V2-7000)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$V2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$V2)^2))/mean(testing_dataset$V2-7000)
}
cat("2018 volume",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))



# ------------------------------------------------------------------------------------------------ #
#70 training/30 prediction 30 repeat, inverted wcm
#2007 volume
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Merge_2008.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$V2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ] 
  testing_dataset <- Data[-random_sample, ]
  model <- lm(V2 ~ P_7_V_5, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$V2)
  bias[l] <- mean(predictions - testing_dataset$V2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$V2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$V2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$V2)^2))/mean(testing_dataset$V2)
}
cat("2007 volume",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))

i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Merge_2008.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$V2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ] 
  testing_dataset <- Data[-random_sample, ]
  model <- lm(V2 ~ P_V_5, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$V2)
  bias[l] <- mean(predictions - testing_dataset$V2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$V2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$V2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$V2)^2))/mean(testing_dataset$V2)
}
cat("2008 volume",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))

i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Merge_2010.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$V2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ] 
  testing_dataset <- Data[-random_sample, ]
  model <- lm(V2 ~ P_V_5, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$V2)
  bias[l] <- mean(predictions - testing_dataset$V2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$V2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$V2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$V2)^2))/mean(testing_dataset$V2)
}
cat("2010 volume",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))

i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Merge_2018.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$V2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ] 
  testing_dataset <- Data[-random_sample, ]
  model <- lm(V2 ~ P_7_V_5, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$V2)
  bias[l] <- mean(predictions - testing_dataset$V2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$V2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$V2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$V2)^2))/mean(testing_dataset$V2)
}
cat("2017 volume",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))

i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Merge_2018.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$V2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ] 
  testing_dataset <- Data[-random_sample, ]
  model <- lm(V2 ~ P_V_5, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$V2)
  bias[l] <- mean(predictions - testing_dataset$V2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$V2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$V2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$V2)^2))/mean(testing_dataset$V2)
}
cat("2018 volume",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))









# ------------------------------------------------------------------------------------------------ #
#70 training/30 prediction 30 repeat, inverted linear
#2007 cover
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2008.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  model <- lm(C2 ~ HV7, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$C2)
  bias[l] <- mean(predictions - testing_dataset$C2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$C2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$C2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$C2)^2))/mean(testing_dataset$C2)
}
cat("2007 cover",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))
#2008 cover
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2008.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  model <- lm(C2 ~ HV, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$C2)
  bias[l] <- mean(predictions - testing_dataset$C2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$C2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$C2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$C2)^2))/mean(testing_dataset$C2)
}
cat("2008 cover",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))
#2010 cover
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2010.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  model <- lm(C2 ~ HV, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$C2)
  bias[l] <- mean(predictions - testing_dataset$C2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$C2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$C2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$C2)^2))/mean(testing_dataset$C2)
}
cat("2010 cover",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))
#2017 cover
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2018.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  model <- lm(C2 ~ HV7, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$C2)
  bias[l] <- mean(predictions - testing_dataset$C2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$C2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$C2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$C2)^2))/mean(testing_dataset$C2)
}
cat("2017 cover",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))
#2018 cover
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2018.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  model <- lm(C2 ~ HV, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$C2)
  bias[l] <- mean(predictions - testing_dataset$C2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$C2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$C2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$C2)^2))/mean(testing_dataset$C2)
}
cat("2018 cover",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))








# ------------------------------------------------------------------------------------------------ #
#70 training/30 prediction 30 repeat, inverted log
#2007 cover
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2008.csv"
Data = read.csv(i,header=T)
Data$C2 <- Data$C2
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  model <- nls(C2 ~ a*exp(b*HV7),data=training_dataset, start = list(a=200, b=0.4))
  predictions <- predict(model, newdata = testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$C2)
  bias[l] <- mean(predictions - testing_dataset$C2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$C2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$C2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$C2)^2))/mean(testing_dataset$C2)
}
cat("2007 cover",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))
#2008 cover
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2008.csv"
Data = read.csv(i,header=T)
Data$C2 <- Data$C2 
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  model <- nls(C2 ~ a*exp(b*HV),data=training_dataset, start = list(a=200, b=0.4))
  predictions <- predict(model, newdata = testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$C2)
  bias[l] <- mean(predictions - testing_dataset$C2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$C2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$C2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$C2)^2))/mean(testing_dataset$C2)
}
cat("2008 cover",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))
#2010 cover
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2010.csv"
Data = read.csv(i,header=T)
Data$C2 <- Data$C2 
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  model <- nls(C2 ~ a*exp(b*HV),data=training_dataset, start = list(a=200, b=0.4))
  predictions <- predict(model, newdata = testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$C2)
  bias[l] <- mean(predictions - testing_dataset$C2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$C2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$C2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$C2)^2))/mean(testing_dataset$C2)
}
cat("2010 cover",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))
#2017 cover
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2018.csv"
Data = read.csv(i,header=T)
Data$C2 <- Data$C2 
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  #volume a=80000,b=0.1
  model <- nls(C2 ~ a*exp(b*HV7),data=training_dataset, start = list(a=200, b=0.4))
  predictions <- predict(model, newdata = testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$C2)
  bias[l] <- mean(predictions - testing_dataset$C2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$C2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$C2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$C2)^2))/mean(testing_dataset$C2)
}
cat("2017 cover",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))
#2018 cover
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Ind_2018.csv"
Data = read.csv(i,header=T)
Data$C2 <- Data$C2 
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  model <- nls(C2 ~ a*exp(b*HV),data=training_dataset, start = list(a=200, b=0.4))
  predictions <- predict(model, newdata = testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$C2)
  bias[l] <- mean(predictions - testing_dataset$C2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$C2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$C2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$C2)^2))/mean(testing_dataset$C2)
}
cat("2018 cover",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))





# ------------------------------------------------------------------------------------------------ #
#70 training/30 prediction 30 repeat, inverted wcm
#2007 cover
i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Merge_2008.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ] 
  testing_dataset <- Data[-random_sample, ]
  model <- lm(C2 ~ P_7_V_5, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$C2)
  bias[l] <- mean(predictions - testing_dataset$C2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$C2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$C2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$C2)^2))/mean(testing_dataset$C2)
}
cat("2007 cover",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))

i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Merge_2008.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ] 
  testing_dataset <- Data[-random_sample, ]
  model <- lm(C2 ~ P_V_5, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$C2)
  bias[l] <- mean(predictions - testing_dataset$C2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$C2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$C2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$C2)^2))/mean(testing_dataset$C2)
}
cat("2008 cover",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))

i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Merge_2010.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ] 
  testing_dataset <- Data[-random_sample, ]
  model <- lm(C2 ~ P_V_5, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$C2)
  bias[l] <- mean(predictions - testing_dataset$C2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$C2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$C2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$C2)^2))/mean(testing_dataset$C2)
}
cat("2010 cover",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))

i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Merge_2018.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ] 
  testing_dataset <- Data[-random_sample, ]
  model <- lm(C2 ~ P_7_V_5, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$C2)
  bias[l] <- mean(predictions - testing_dataset$C2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$C2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$C2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$C2)^2))/mean(testing_dataset$C2)
}
cat("2017 cover",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))

i <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\Merge_2018.csv"
Data = read.csv(i,header=T)
l <- 1
r2 <- 0
bias <- 0
RB <- 0
RMSE <- 0
RRMSE <- 0
for (l in 1:30){
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ] 
  testing_dataset <- Data[-random_sample, ]
  model <- lm(C2 ~ P_V_5, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  r2[l] <- R2(predictions, testing_dataset$C2)
  bias[l] <- mean(predictions - testing_dataset$C2)
  RB[l] <- 100*bias[l]/mean(testing_dataset$C2)
  RMSE[l] <- sqrt(mean((predictions - testing_dataset$C2)^2))
  RRMSE[l] <- 100*sqrt(mean((predictions - testing_dataset$C2)^2))/mean(testing_dataset$C2)
}
cat("2018 cover",mean(r2),mean(bias),mean(RB),mean(RMSE),mean(RRMSE))





#--------------------------------------------------------------------------------------
#group bias distribution --- cover
boxplot_group_cover <- function(year){
  #linear
  dir <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_",year,".csv")
  Data = read.csv(dir,header=T)
  Data <- na.omit(Data)
  Data <- Data[Data$C2 > 0 & Data$C2 < 0.9,]
  
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ] 
  testing_dataset <- Data[-random_sample, ]
  reg <- lm(C2 ~ HV, data = training_dataset)
  y <- predict(reg, testing_dataset)
  
  
  testing_dataset$diff <- y-testing_dataset$C2
  breakbin = seq(0,1,0.1)
  testing_dataset$group_RH <- cut(testing_dataset$C2,breaks = breakbin
                                  ,dig.lab=1)
  table(testing_dataset$group_RH)
  
  p1<- ggplot(testing_dataset, aes(x=C2, y=diff, group=group_RH)) + 
    stat_boxplot(geom ='errorbar', width = 0.1) +
    geom_boxplot(width = 1)+
    geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
    coord_cartesian(ylim = c(-1, 0.6))+
    scale_y_continuous(minor_breaks = seq(-1, 0.6, 0.2),breaks = seq(-1, 0.6, 0.2))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme_bw()+
    labs(x="",y="Bias (m)")+
    theme(axis.title.x=element_blank())+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Inverted linear regression")
  
  
  a <- tapply(testing_dataset$diff, cut(testing_dataset$C2,breaks = breakbin, dig.lab=1), mean)
  b <- tapply(testing_dataset$C2, cut(testing_dataset$C2,breaks = breakbin, dig.lab=1), mean)
  RB <- a/b*100
  
  x <- seq(0,0.9,0.1)
  l <- cbind(x,RB,table(testing_dataset$group_RH))
  df <- data.frame(l)
  df <- head(df,-1)
  p4<- ggplot(df, aes(x=x+0.05, y=RB)) + 
    geom_bar(stat='identity')+
    geom_text(aes(label = V3,y=-40), size = 4)+
    theme_bw()+
    coord_cartesian(ylim = c(-50, 90))+
    scale_y_continuous(minor_breaks = seq(-50, 90, 10),breaks = seq(-50, 90, 10))+
    scale_x_continuous(minor_breaks = seq(0,1,0.1),breaks = seq(0,1,0.1))+
    labs(x="ALS Canopy cover (5*5)", 
         y="%Bias (%)")+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  #log
  i <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_",year,".csv")
  Data = read.csv(i,header=T)
  Data <- na.omit(Data)
  Data <- Data[Data$C2 > 0 & Data$C2 < 0.9,]
  
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ]
  testing_dataset <- Data[-random_sample, ]
  model <- nls(C2 ~ a*exp(b*HV),data=training_dataset, start = list(a=200, b=0.4))
  pred <- predict(model, newdata = testing_dataset)
  
  
  y <- pred
  x <- testing_dataset$C2
  testing_dataset$diff <- y-x
  breakbin = seq(0,1,0.1)
  testing_dataset$group_RH <- cut(x,breaks = breakbin
                                  ,dig.lab=1)
  table(testing_dataset$group_RH)
  
  p2<- ggplot(testing_dataset, aes(x=C2, y=diff, group=group_RH)) + 
    stat_boxplot(geom ='errorbar', width = 0.1) +
    geom_boxplot(width = 1)+
    geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
    coord_cartesian(ylim = c(-1, 0.6))+
    scale_y_continuous(minor_breaks = seq(-1, 0.6, 0.2),breaks = seq(-1, 0.6, 0.2))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme_bw()+
    labs(x="",y="Bias (m)")+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Exponential regression")
  
  a <- tapply(testing_dataset$diff, cut(testing_dataset$C2,breaks = breakbin, dig.lab=1), mean)
  b <- tapply(testing_dataset$C2, cut(testing_dataset$C2,breaks = breakbin, dig.lab=1), mean)
  RB <- a/b*100
  
  x <- seq(0,0.9,0.1)
  l <- cbind(x,RB,table(testing_dataset$group_RH))
  df <- data.frame(l)
  df <- head(df,-1)
  p5<- ggplot(df, aes(x=x+0.05, y=RB)) + 
    geom_bar(stat='identity')+
    geom_text(aes(label = V3,y=-40), size = 4)+
    theme_bw()+
    coord_cartesian(ylim = c(-50, 90))+
    scale_y_continuous(minor_breaks = seq(-50, 90, 10),breaks = seq(-50, 90, 10))+
    scale_x_continuous(minor_breaks = seq(0,1,0.1),breaks = seq(0,1,0.1))+
    labs(x="ALS Canopy cover (5*5)", 
         y="%Bias (%)")+
    theme(axis.title.y=element_blank())+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  
  #wcm
  dir <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_",year,".csv")
  Data = read.csv(dir,header=T)
  Data <- na.omit(Data)
  Data <- Data[Data$C2 > 0 & Data$C2 < 0.9,]
  
  random_sample <- createDataPartition(Data$C2, p = 0.7, list = FALSE)
  training_dataset  <- Data[random_sample, ] 
  testing_dataset <- Data[-random_sample, ]
  model <- lm(C2 ~ P_V_5, data = training_dataset)
  predictions <- predict(model, testing_dataset)
  
  testing_dataset$diff <- predictions-testing_dataset$C2
  breakbin = seq(0,1,0.1)
  testing_dataset$group_RH <- cut(testing_dataset$C2,breaks = breakbin
                                  ,dig.lab=1)
  table(Data$group_RH)
  
  p3<- ggplot(testing_dataset, aes(x=C2, y=diff, group=group_RH)) + 
    stat_boxplot(geom ='errorbar', width = 0.1) +
    geom_boxplot(width = 1)+
    geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
    coord_cartesian(ylim = c(-1, 0.6))+
    scale_y_continuous(minor_breaks = seq(-1, 0.6, 0.2),breaks = seq(-1, 0.6, 0.2))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme_bw()+
    labs(x="",y="Bias (m)")+
    theme(axis.title.x=element_blank())+
    theme(axis.title.y=element_blank())+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Bayesian WCM")
  
  a <- tapply(testing_dataset$diff, cut(testing_dataset$C2,breaks = breakbin, dig.lab=1), mean)
  b <- tapply(testing_dataset$C2, cut(testing_dataset$C2,breaks = breakbin, dig.lab=1), mean)
  RB <- a/b*100
  
  x <- seq(0,0.9,0.1)
  l <- cbind(x,RB,table(testing_dataset$group_RH))
  df <- data.frame(l)
  df <- head(df,-1)
  p6<- ggplot(df, aes(x=x+0.05, y=RB)) + 
    geom_bar(stat='identity')+
    geom_text(aes(label = V3,y=-40), size = 4)+
    theme_bw()+
    coord_cartesian(ylim = c(-50, 90))+
    scale_y_continuous(minor_breaks = seq(-50, 90, 10),breaks = seq(-50, 90, 10))+
    scale_x_continuous(minor_breaks = seq(0,1,0.1),breaks = seq(0,1,0.1))+
    labs(x="ALS Canopy cover (5*5)", 
         y="%Bias (%)")+
    theme(axis.title.y=element_blank())+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))
  
  ggarrange(p1,p2,p3,p4,p5,p6)
  out <- file.path("E:\\ChangMap\\CHM\\DB_20210926\\Figure",
                   paste0("figure bias cover ",year," subplot.jpg"))
  ggsave(out,height=12, width=24, dpi=600)
}

boxplot_group_cover(2010)

# ------------------------------------------------------------------------------------------------ #
#group bias distribution --- cover change  BWCM

dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2008.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]


random_sample <- createDataPartition(Data$C2.x, p = 0.7, list = FALSE)
training_dataset  <- Data[random_sample, ] 
testing_dataset <- Data[-random_sample, ]
model1 <- lm(C2.x ~ P_V_5.x, data = training_dataset)
predictions1 <- predict(model1, testing_dataset)
model2 <- lm(C2.y ~ P_V_5.y, data = training_dataset)
predictions2 <- predict(model2, testing_dataset)



testing_dataset$diff_C2_ALS_5 = testing_dataset$C2.x - testing_dataset$C2.y
testing_dataset$diff_pred = predictions1 - predictions2
testing_dataset$diff_C2_SAR_5 = testing_dataset$diff_pred - testing_dataset$diff_C2_ALS_5



breakbin = round(seq(-0.6,0.6,0.2),2)
testing_dataset$group <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
testing_dataset <- na.omit(testing_dataset)
testing_dataset <- testing_dataset[testing_dataset$diff_C2_SAR_5 > -0.6 & testing_dataset$diff_C2_SAR_5 < 0.6,]
p1<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_boxplot(trim=FALSE)+
  labs(
    x = paste0("ALS cover change (2018-2008)"),
    y = paste0("Bias (2018-2008)")) +
  theme_bw()+
  scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

testing_dataset$group_RH <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin
                                ,dig.lab=1)
a <- tapply(testing_dataset$diff_C2_SAR_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(testing_dataset$diff_C2_ALS_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(testing_dataset$group_RH))
df <- data.frame(l)
p4<- ggplot(df, aes(x=x+0.1, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-100, 100))+
  scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
  scale_x_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  labs(x="ALS cover change (2018-2008)", 
       y="%Bias (%)  (2018-2008)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(plot.title = element_text(hjust = 0.5))




dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2010.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]


random_sample <- createDataPartition(Data$C2.x, p = 0.7, list = FALSE)
training_dataset  <- Data[random_sample, ] 
testing_dataset <- Data[-random_sample, ]
model1 <- lm(C2.x ~ P_V_5.x, data = training_dataset)
predictions1 <- predict(model1, testing_dataset)
model2 <- lm(C2.y ~ P_V_5.y, data = training_dataset)
predictions2 <- predict(model2, testing_dataset)



testing_dataset$diff_C2_ALS_5 = testing_dataset$C2.x - testing_dataset$C2.y
testing_dataset$diff_pred = predictions1 - predictions2
testing_dataset$diff_C2_SAR_5 = testing_dataset$diff_pred - testing_dataset$diff_C2_ALS_5



breakbin = round(seq(-0.6,0.6,0.2),2)
testing_dataset$group <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
testing_dataset <- na.omit(testing_dataset)
testing_dataset <- testing_dataset[testing_dataset$diff_C2_SAR_5 > -0.6 & testing_dataset$diff_C2_SAR_5 < 0.6,]
p2<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_boxplot(trim=FALSE)+
  labs(
    x = paste0("ALS cover change (2018-2010)"),
    y = paste0("Bias (2018-2010)")) +
  theme_bw()+
  scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

testing_dataset$group_RH <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin
                                ,dig.lab=1)
a <- tapply(testing_dataset$diff_C2_SAR_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(testing_dataset$diff_C2_ALS_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(testing_dataset$group_RH))
df <- data.frame(l)
p5<- ggplot(df, aes(x=x+0.1, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-100, 100))+
  scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
  scale_x_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  labs(x="ALS cover change (2018-2010)", 
       y="%Bias (%)  (2018-2010)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(plot.title = element_text(hjust = 0.5))




dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2010.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2008.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]


random_sample <- createDataPartition(Data$C2.x, p = 0.7, list = FALSE)
training_dataset  <- Data[random_sample, ] 
testing_dataset <- Data[-random_sample, ]
model1 <- lm(C2.x ~ P_V_5.x, data = training_dataset)
predictions1 <- predict(model1, testing_dataset)
model2 <- lm(C2.y ~ P_V_5.y, data = training_dataset)
predictions2 <- predict(model2, testing_dataset)



testing_dataset$diff_C2_ALS_5 = testing_dataset$C2.x - testing_dataset$C2.y
testing_dataset$diff_pred = predictions1 - predictions2
testing_dataset$diff_C2_SAR_5 = testing_dataset$diff_pred - testing_dataset$diff_C2_ALS_5



breakbin = round(seq(-0.6,0.6,0.2),2)
testing_dataset$group <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
testing_dataset <- na.omit(testing_dataset)
testing_dataset <- testing_dataset[testing_dataset$diff_C2_SAR_5 > -0.6 & testing_dataset$diff_C2_SAR_5 < 0.6,]
p3<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_boxplot(trim=FALSE)+
  labs(
    x = paste0("ALS cover change (2010-2008)"),
    y = paste0("Bias (2010-2008)")) +
  theme_bw()+
  scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

testing_dataset$group_RH <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin
                                ,dig.lab=1)
a <- tapply(testing_dataset$diff_C2_SAR_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(testing_dataset$diff_C2_ALS_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(testing_dataset$group_RH))
df <- data.frame(l)
p6<- ggplot(df, aes(x=x+0.1, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-100, 100))+
  scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
  scale_x_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  labs(x="ALS cover change (2010-2008)", 
       y="%Bias (%)  (2010-2008)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(plot.title = element_text(hjust = 0.5))




ggarrange(p1,p2,p3,p4,p5,p6,ncol=3,nrow=2)
out <- "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\change cover composite boxplot wcm.jpg"
ggsave(out,height=9, width=18, dpi=600)


# ------------------------------------------------------------------------------------------------ #
#group bias distribution --- cover change --- log

dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]

random_sample <- createDataPartition(Data$C2.x, p = 0.7, list = FALSE)
training_dataset  <- Data[random_sample, ] 
testing_dataset <- Data[-random_sample, ]

model1 <- nls(C2.x ~ a*exp(b*HV.x),data=training_dataset, start = list(a=10, b=0.4))
predictions1 <- predict(model1, testing_dataset)
model2 <- nls(C2.y ~ a*exp(b*HV.y),data=training_dataset, start = list(a=10, b=0.4))
predictions2 <- predict(model2, testing_dataset)

testing_dataset$diff_C2_ALS_5 = testing_dataset$C2.x - testing_dataset$C2.y
testing_dataset$diff_pred = predictions1 - predictions2
testing_dataset$diff_C2_SAR_5 = testing_dataset$diff_pred - testing_dataset$diff_C2_ALS_5



breakbin = round(seq(-0.6,0.6,0.2),2)
testing_dataset$group <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
testing_dataset <- na.omit(testing_dataset)
testing_dataset <- testing_dataset[testing_dataset$diff_C2_SAR_5 > -0.6 & testing_dataset$diff_C2_SAR_5 < 0.6,]
p1<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_boxplot(trim=FALSE)+
  labs(
    x = paste0("ALS cover change (2018-2008)"),
    y = paste0("Bias (2018-2008)")) +
  theme_bw()+
  scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

testing_dataset$group_RH <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin
                                ,dig.lab=1)
a <- tapply(testing_dataset$diff_C2_SAR_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(testing_dataset$diff_C2_ALS_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(testing_dataset$group_RH))
df <- data.frame(l)
p4<- ggplot(df, aes(x=x+0.1, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-100, 100))+
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

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]

random_sample <- createDataPartition(Data$C2.x, p = 0.7, list = FALSE)
training_dataset  <- Data[random_sample, ] 
testing_dataset <- Data[-random_sample, ]

model1 <- nls(C2.x ~ a*exp(b*HV.x),data=training_dataset, start = list(a=10, b=0.4))
predictions1 <- predict(model1, testing_dataset)
model2 <- nls(C2.y ~ a*exp(b*HV.y),data=training_dataset, start = list(a=10, b=0.4))
predictions2 <- predict(model2, testing_dataset)

testing_dataset$diff_C2_ALS_5 = testing_dataset$C2.x - testing_dataset$C2.y
testing_dataset$diff_pred = predictions1 - predictions2
testing_dataset$diff_C2_SAR_5 = testing_dataset$diff_pred - testing_dataset$diff_C2_ALS_5



breakbin = round(seq(-0.6,0.6,0.2),2)
testing_dataset$group <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
testing_dataset <- na.omit(testing_dataset)
testing_dataset <- testing_dataset[testing_dataset$diff_C2_SAR_5 > -0.6 & testing_dataset$diff_C2_SAR_5 < 0.6,]
p2<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_boxplot(trim=FALSE)+
  labs(
    x = paste0("ALS cover change (2018-2010)"),
    y = paste0("Bias (2018-2010)")) +
  theme_bw()+
  scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

testing_dataset$group_RH <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin
                                ,dig.lab=1)
a <- tapply(testing_dataset$diff_C2_SAR_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(testing_dataset$diff_C2_ALS_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(testing_dataset$group_RH))
df <- data.frame(l)
p5<- ggplot(df, aes(x=x+0.1, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-100, 100))+
  scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
  scale_x_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  labs(x="ALS cover change (2018-2010)", 
       y="%Bias (%)  (2018-2010)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(plot.title = element_text(hjust = 0.5))



dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]

random_sample <- createDataPartition(Data$C2.x, p = 0.7, list = FALSE)
training_dataset  <- Data[random_sample, ] 
testing_dataset <- Data[-random_sample, ]

model1 <- nls(C2.x ~ a*exp(b*HV.x),data=training_dataset, start = list(a=10, b=0.4))
predictions1 <- predict(model1, testing_dataset)
model2 <- nls(C2.y ~ a*exp(b*HV.y),data=training_dataset, start = list(a=10, b=0.4))
predictions2 <- predict(model2, testing_dataset)

testing_dataset$diff_C2_ALS_5 = testing_dataset$C2.x - testing_dataset$C2.y
testing_dataset$diff_pred = predictions1 - predictions2
testing_dataset$diff_C2_SAR_5 = testing_dataset$diff_pred - testing_dataset$diff_C2_ALS_5



breakbin = round(seq(-0.6,0.6,0.2),2)
testing_dataset$group <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
testing_dataset <- na.omit(testing_dataset)
testing_dataset <- testing_dataset[testing_dataset$diff_C2_SAR_5 > -0.6 & testing_dataset$diff_C2_SAR_5 < 0.6,]
p3<- ggplot(testing_dataset, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
  geom_boxplot(trim=FALSE)+
  labs(
    x = paste0("ALS cover change (2010-2008)"),
    y = paste0("Bias (2010-2008)")) +
  theme_bw()+
  scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25)) +
  theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

testing_dataset$group_RH <- cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin
                                ,dig.lab=1)
a <- tapply(testing_dataset$diff_C2_SAR_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(testing_dataset$diff_C2_ALS_5, cut(testing_dataset$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(testing_dataset$group_RH))
df <- data.frame(l)
p6<- ggplot(df, aes(x=x+0.1, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-100, 100))+
  scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
  scale_x_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                     breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
  labs(x="ALS cover change (2010-2008)", 
       y="%Bias (%)  (2010-2008)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(plot.title = element_text(hjust = 0.5))




ggarrange(p1,p2,p3,p4,p5,p6,ncol=3,nrow=2)
out <- "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\change cover composite boxplot log.jpg"
ggsave(out,height=9, width=18, dpi=600)






#-------------------------------------------------------------------------------------------
#inverted SAR change vs. cover change 
dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_ALS = Data$C2.x - Data$C2.y
Data$diff_SAR = Data$HV.x - Data$HV.y


random_sample <- createDataPartition(Data$diff_ALS, p = 0.7, list = FALSE)
training_dataset  <- Data[random_sample, ]
testing_dataset <- Data[-random_sample, ]

model <- lm(diff_ALS ~ diff_SAR, data = training_dataset)
predictions <- predict(model, testing_dataset)


r2 <- R2(predictions, testing_dataset$diff_ALS)
bias <- mean(predictions - testing_dataset$diff_ALS)
RB <- 100*bias/mean(testing_dataset$diff_ALS)
RMSE <- sqrt(mean((predictions - testing_dataset$diff_ALS)^2))
RRMSE <- 100*sqrt(mean((predictions - testing_dataset$diff_ALS)^2))/mean(testing_dataset$diff_ALS)




#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#20200228
dir1 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
dir2 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$V2.x > 0,]
Data$height = (Data$V2.x - Data$V2.y)/5250
Data$cover = Data$C2.x - Data$C2.y
Data$SAR = Data$HV.x - Data$HV.y
Data <- na.omit(Data)
Data$group <- NA
Data$group[Data$cover >= -0.2 & Data$cover < -0.1]= "(-0.2,-0.1)"
Data$group[Data$cover >= -0.1 & Data$cover < 0]= "(-0.1,0)"
Data$group[Data$cover >= 0 & Data$cover < 0.1]= "(0,0.1)"
Data$group[Data$cover >= 0.1 & Data$cover < 0.2]= "(0.1,0.2)"
Data$group[Data$cover >= 0.2 & Data$cover < 0.3]= "(0.2,0.3)"
Data <- Data[!is.na(Data$group), ]

title <- "(-0.2,-0.1)"
Data3 <- Data[Data$group == title,]
p1 <- ggplot(Data3, aes(x=height, y=SAR))+ 
  geom_pointdensity()+
  scale_color_viridis(direction = 1)+
  labs(title = title,
       x = paste0("LiDAR CHM height change"),
       y = paste0("ALOS backscatter change")) +
  theme(text=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5))
R2(Data3$height,Data3$SAR)
title <- "(-0.1,0)"
Data3 <- Data[Data$group == title,]
p2 <- ggplot(Data3, aes(x=height, y=SAR))+ 
  geom_pointdensity()+
  scale_color_viridis(direction = 1)+
  labs(title = title,
       x = paste0("LiDAR CHM height change"),
       y = paste0("ALOS backscatter change")) +
  theme(text=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5))
R2(Data3$height,Data3$SAR)
title <- "(0,0.1)"
Data3 <- Data[Data$group == title,]
p3 <- ggplot(Data3, aes(x=height, y=SAR))+ 
  geom_pointdensity()+
  scale_color_viridis(direction = 1)+
  labs(title = title,
       x = paste0("LiDAR CHM height change"),
       y = paste0("ALOS backscatter change")) +
  theme(text=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5))
R2(Data3$height,Data3$SAR)
title <- "(0.1,0.2)"
Data3 <- Data[Data$group == title,]
p4 <- ggplot(Data3, aes(x=height, y=SAR))+ 
  geom_pointdensity()+
  scale_color_viridis(direction = 1)+
  labs(title = title,
       x = paste0("LiDAR CHM height change"),
       y = paste0("ALOS backscatter change")) +
  theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))
R2(Data3$height,Data3$SAR)
ggarrange(p1,p2,p3,p4,ncol=2,nrow=2)
out <- "E:\\temp\\LA\\SAR\\DB_20210926\\DB_csv\\group scatter 20182010.jpg"
ggsave(out, height=12, width=12, dpi=300)






