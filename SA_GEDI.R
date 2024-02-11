#Author: Xiaoxuan Li
library(corrplot)
library(tidyverse)
library(psych)
library(xlsx)
library(car)
library(MASS)
library(rsq)
library(ggpmisc)
library(ggrepel)
library(dplyr)
library(rlist)
library(data.table)
library(lemon)
library(lmtest)
library(reshape2)
library(ggpubr)
library(ggplot2)
library(mltools)
library(tidyr)
library(stats)
library(lidR)
library(tools) 
library(raster)
library(ggpointdensity)
library(viridis)
library(grid)
library(readxl)
library(ehaGoF)
library(Metrics)

windowsFonts(A = windowsFont("Palatino Linotype"))


#table for paper
Locdir <- "E:\\GEDI\\Result\\Result"
pattern = 'testcase'
files <- list.files(path = Locdir, full.names = TRUE, pattern = paste0(pattern,".csv"))
listt <- list("Studysite ","Orbit ","r2 ","Slope ",
              "Intercept ", "Mean ","SD ","RMSE ",
              "%RMSE ","Sample ","Power ","Coverage ","Power_R2 ","Coverage_R2 ","Solar ")
df <- data.frame(listt)
colnames(df) <- listt
for (i in files){
  print(i)
  Data = read.csv(i,header=T)
  Data1 <- Data[Data$Type == 1,]
  list1 <- list()
  Data = read.csv(i,header=T)
  Data <- Data %>% separate(Beam, c("Beam", "Type"))
  #Data <- Data[Data$new_quality_flag1 == 1 & Data$sensitivity_a1 > 0.95,]
  if(length(Data[,1])==0) next
  Data1 <- Data[Data$Type == 1,]
  Data2 <- Data[Data$Type == 0,]
  
  reg1 <- lm(Data$RH3_98 ~ Data$GEDI_sim_rh_98, data = Data)
  coeff <- round(reg1$coefficients , 2)
  
  if (nrow(Data1) > 1){
    reg2 <- lm(Data1$RH3_98 ~ Data1$GEDI_sim_rh_98, data = Data1)
    r2_p = round(summary(reg2)$adj.r.squared,3)
  }else{
    r2_p = 0
  }
  if (nrow(Data2) > 1){
    reg3 <- lm(Data2$RH3_98 ~ Data2$GEDI_sim_rh_98, data = Data2)
    r2_c = round(summary(reg3)$adj.r.squared,3)
  }else{
    r2_c = 0
  }
  
  Mean <- mean(reg1$fitted.values)
  RMSE <- sqrt(mean(reg1$residuals^2))
  rRMSE <- gofRRMSE(Data$RH3_98, predict(reg1))
  SD <- sd(reg1$fitted.values)
  
  Sub = strsplit(substring(strsplit(i, "Result/")[[1]][2], first = 1),"_")
  Studysite = Sub[[1]][1]
  Orbit = substring(Sub[[1]][2],first = 3)
  print(paste0("Studysite ",Studysite))
  print(paste0("Orbit: ",Orbit))
  print(paste0("r2: ",round(summary(reg1)$adj.r.squared,3)))
  print(paste0("Slope: ",coeff[2]))
  print(paste0("Intercept: ",coeff[1]))
  print(paste0("Mean ",round(Mean,3)))
  print(paste0("SD: ",round(SD,3)))
  print(paste0("RMSE: ",round(RMSE,3)))
  print(paste0("%RMSE: ",round(rRMSE,3)))
  print(paste0("Sample: ",nrow(Data)))
  print(paste0("Power: ",nrow(Data1)))
  print(paste0("Coverage: ",nrow(Data2)))
  print(paste0("r2_p: ",r2_p))
  print(paste0("r2_c: ",r2_c))
  print(paste0("Solar: ",mean(Data$solar_elevation)))
  list1[[1]] <- Studysite
  list1[[2]] <- Orbit
  list1[[3]] <- round(summary(reg1)$adj.r.squared,3)
  list1[[4]] <- coeff[2]
  list1[[5]] <- coeff[1]
  list1[[6]] <- round(Mean,3)
  list1[[7]] <- round(SD,3)
  list1[[8]] <- round(RMSE,3)
  list1[[9]] <- round(rRMSE,3)
  list1[[10]] <- nrow(Data)
  list1[[11]] <- nrow(Data1)
  list1[[12]] <- nrow(Data2)
  list1[[13]] <- r2_p
  list1[[14]] <- r2_c
  list1[[15]] <- mean(Data$solar_elevation)
  df1 <- data.frame(list1)
  colnames(df1) <- listt
  df <- rbind(df,df1)
}

write.table(df,file=file.path(Locdir,paste0(pattern,"_table98.csv")), quote=F,sep=" ",row.names=F)



#all test case summary scatterplot --------------------------------------

#all test case summary scatterplot
filedir <- "E:\\GEDI\\Result\\Result\\All_evergreen.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$site == "Duku",]

filedir <- "E:\\GEDI\\Result\\Result\\All_on.csv"
Data1 = read.csv(filedir,header=T)
filedir <- "E:\\GEDI\\Result\\Result\\All_evergreen.csv"
Data2 = read.csv(filedir,header=T)
Data = rbind(Data1,Data2)  
Data <- Data[Data$site == "Venetia",]

out_dir <- "C:\\Users\\Shawn\\Desktop\\paper\\graph"
filedir <- "E:\\GEDI\\Result\\Result\\All_off.csv"
Data = read.csv(filedir,header=T)

filedir <- "E:\\GEDI\\GEDI_archive\\GEDI01B\\SA\\VA\\metrics\\Result.csv"
Data = read.csv(filedir,header=T)


Data <- Data[Data$GEDI_sim_rh_98 <= 15,]
Data <- Data[Data$GEDI_sim_rh_98 >= 2,]

reg1 <- lm(Data$RH3_98 ~ Data$GEDI_sim_rh_98, data = Data)
Mean <- mean(reg1$fitted.values)
RMSE <- sqrt(mean(reg1$residuals^2))
#rRMSE <- gofRRMSE(Data$RH3_98, predict(reg1))
rRMSE <- sqrt(mean(reg1$residuals^2)/(max((Data$RH3_98))-min((Data$RH3_98))))*100
SD <- sd(reg1$fitted.values)
coeff <- round(reg1$coefficients , 2)

txt <- paste(" Linear regression: y = ",coeff[1] , " + " , coeff[2] , "*x",
             "\n" , "Adjusted R-squared: ",round(summary(reg1)$adj.r.squared,3),
             "\n" , "Mean: ", round(Mean,3), "m",
             "\n" , "RMSE: ", round(RMSE,3),"m",
             "\n" , "%RMSE: ", round(rRMSE,3),"%",
             "\n" , "Sample size: ", nrow(Data),
             "\n" , "Standard Deviation: ", round(SD,3)
)

grob <- grobTree(textGrob(txt, x=0.1,  y=0.77, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))

#scale_x_continuous(limits = c(0,30),minor_breaks = seq(0,30,2))+
#scale_y_continuous(limits = c(0,30),minor_breaks = seq(0,30,2))+
min1 <- min(Data$GEDI_sim_rh_98)
min2 <- min(Data$RH3_98)
ggplot(Data, aes(x=GEDI_sim_rh_98, y=RH3_98))+ 
  geom_pointdensity()+
  scale_color_viridis(direction = -1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=20)) + 
  scale_x_continuous(limits = c(min1,15),minor_breaks = seq(0,15,2))+
  scale_y_continuous(limits = c(min2,15),minor_breaks = seq(0,15,2))+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  geom_smooth(
    method="lm", 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red") + 
  annotation_custom(grob) +
  geom_abline(color="gray", linetype="dashed")+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y=expression("GEDI-RH98"["Orb"]~"(m)"))

out <- file.path(out_dir,"All_5.jpg")
ggsave(out, height=7, width=10, dpi=1200)

#for test case Welverdiendt 
out_dir <- "C:\\Users\\Shawn\\Desktop\\paper\\graph"
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$site == "Welverdiendt",]

reg1 <- lm(Data$RH3_98 ~ Data$GEDI_sim_rh_98, data = Data)

Mean <- mean(reg1$fitted.values)
RMSE <- sqrt(mean(reg1$residuals^2))
rRMSE <- gofRRMSE(Data$RH3_98, predict(reg1))

SD <- sd(reg1$fitted.values)
coeff <- round(reg1$coefficients , 2)

txt <- paste(" Linear regression: y = ",coeff[1] , " + " , coeff[2] , "*x",
             "\n" , "Adjusted R-squared: ",round(summary(reg1)$adj.r.squared,3),
             "\n" , "Mean: ", round(Mean,3), "m",
             "\n" , "RMSE: ", round(RMSE,3),"m",
             "\n" , "%RMSE: ", round(rRMSE,3),"%",
             "\n" , "Sample size: ", nrow(Data),
             "\n" , "Standard Deviation: ", round(SD,3)
)

ggplot(Data, aes(x=GEDI_sim_rh_98, y=RH3_98,color=orbit))+ 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  stat_poly_eq(
    formula = y ~ x, 
    label.x.npc = "left", label.y.npc = "top",vstep = 0.03,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~")), 
    size = 5,
    parse = TRUE)+
  theme(text=element_text(size=20)) + 
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  geom_abline(color="gray", linetype="dashed")+
  labs(x=expression("GEDI"["Sim"]~"(m)"), 
       y=expression("GEDI"["Rec"]~"(m)"))+
  scale_color_manual(values=c("#00ffff", "#ff4d4d", "#ff0000", "#b30000","#0000b3",
                              "#80ff00","#00b300","#00b300","#00b300","#00ffff",
                              "#008b8b","#ff0000","#ff0000","#ff0000","#b30000"),
                     labels = c("2019.04.28","2019.07.20",
                                "2019.08.02","2019.09.28",
                                "2019.11.15","2020.01.27",
                                "2020.02.01","2020.02.05",
                                "2020.02.13","2020.04.04",
                                "2020.05.12","2020.08.10",
                                "2020.08.21","2020.08.25",
                                "2020.09.02"))

out <- file.path(out_dir,"Wel_combined1.jpg")
ggsave(out)


# collocation result summary ----------------------------------------------

#collocation result summary --- r2~beam
out_dir <- "C:\\Users\\Shawn\\Desktop\\paper\\graph"
filedir <- "E:\\GEDI\\Result\\Result\\joined_table98.csv"
Data = read.csv(filedir,header=T)
Data$Solar_index = 0
Data$Solar_index[Data$Solar < 0]="Night"
Data$Solar_index[Data$Solar > 0]="Day"
Data <- Data[Data$Sample > 20 & 
               Data$correl > 0.85 & 
               Data$site != "Duku" ,]

give.n <- function(x){
  return(c(x = x, y = 1, label = length(x)))
}
ggplot(Data, aes(x=beam, y=r2, fill=Solar_index)) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(limits = c(0,1),minor_breaks = seq(0,1,0.2))+
  geom_boxplot() +
  theme(legend.title = element_blank())+
  stat_summary(fun.data = give.n, geom = "text",position = position_dodge(width = .75))+
  labs(x="Beam", 
       y=expression("R"^2))+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))
out <- file.path(out_dir,"collocate_boxplot_beam.jpg")
ggsave(out,height=10, width=15, dpi=1200)

#collocation result summary --- r2~site
out_dir <- "C:\\Users\\Shawn\\Desktop\\paper\\graph"
filedir <- "E:\\GEDI\\Result\\Result\\Correlation.csv"
Data = read.csv(filedir,header=T)

ggplot(Data, aes(x=site, y=r2, fill=beam)) + 
  scale_y_continuous(limits = c(0,1),minor_breaks = seq(0,1,0.2))+
  geom_boxplot()+
  theme(legend.title = element_blank())+

  labs(x="Study site", 
       y=expression("R"^2))+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))
out <- file.path(out_dir,"collocate_boxplot_site.jpg")
ggsave(out,height=10, width=15, dpi=1200)

#collocation result summary --- x y z offset histogram
out_dir <- "C:\\Users\\Shawn\\Desktop\\paper\\graph"
filedir <- "E:\\GEDI\\Result\\Result\\Correlation.csv"
Data = read.csv(filedir,header=T)
myvars <- c("dx","dy","site")
data <- Data[myvars]
Melt <- melt(data, id.var='site')

Data$offset <- sqrt((Data$dx)^2+(Data$dy)^2)
sd(Data$offset)
mean(Data$offset)
RMSE <- sqrt(mean((Data$RH3_98 - Data$GEDI_sim_rh_98)^2))
rRMSE <- 100*sqrt(mean((Data$RH3_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$RH3_98)
MD <- mean(Data$RH3_98 - Data$GEDI_sim_rh_98)
MAE <- mae(Data$GEDI_sim_rh_98, Data$RH3_98)

ggplot(Data, aes(offset)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(color =  "white",fill = "blue",bins = 30,alpha = 0.2) +
  xlab("Offset (m)") + ylab("Frequency")+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))

out <- file.path(out_dir,"collocate_xyzoffset_histogram.jpg")
ggsave(out, height=10, width=15, dpi=1200)

#collocation result summary --- x y z offset boxplot by site
ggplot(Melt, aes(x=site, y=value, fill=variable)) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_boxplot() +
  xlab("Study site") + ylab("Offset (m)")+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))

out <- file.path(out_dir,"collocate_xyzoffset_boxplot.jpg")
ggsave(out, height=10, width=15, dpi=1200)

#collocation result summary --- z offest boxplot by site
out_dir <- "C:\\Users\\Shawn\\Desktop\\paper\\graph"
filedir <- "E:\\GEDI\\Result\\Result\\table98_20201228.xlsx"
Data = read_excel(filedir, sheet = 2)
Data <- Data[Data$correl > 0.85 & Data$Sample > 20 & Data$dz > -100 & Data$site != "Duku",]
myvars <- c("site","dz")
data <- Data[myvars]
Melt <- melt(data, id.var='site')

ggplot(Melt, aes(x=site, y=value, fill=site)) + 
  geom_boxplot() +
  xlab("Study site") + ylab("Z offset (m)")+
  theme(legend.title = element_blank())+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))

out <- file.path(out_dir,"collocate_xyzoffset_boxplot_site.jpg")
ggsave(out, height=10, width=15, dpi=1200)


#overall R2 and RMSE histogram ------------------------------------------

#R2 histogram 
#RMSE histogram
out_dir <- "C:\\Users\\Shawn\\Desktop\\paper\\graph"
filedir <- "E:\\GEDI\\Result\\Result\\table98_20210129.csv"
Data = read.csv(filedir,header=T)
ggplot(Data, aes(x=r2))+
  xlab(expression("R"^2))+
  ylab("Number of test cases")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=20))+
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(n.breaks = 10)+
  theme(text=element_text(family="A"))+
  geom_histogram(data=Data,
                 aes(color = Phenology, fill = Phenology),
                 position = "identity",alpha = 0.2,binwidth  = 0.07)
out_r2 <- file.path(out_dir,"r2_hist.jpg.jpg")
ggsave(out_r2, height=10, width=15, dpi=1200)


ggplot(Data, aes(x=RMSE))+
  xlab(expression("RMSE"))+
  ylab("Number of test cases")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=20)) +
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(n.breaks = 10)+
  theme(text=element_text(family="A"))+
  geom_histogram(data=Data,
                 aes(color = Phenology, fill = Phenology),
                 position = "identity",alpha = 0.2,binwidth  = 0.2)

out_rmse <- file.path(out_dir,"rmse_hist.jpg")
ggsave(out_rmse, height=10, width=15, dpi=1200)


#R2~phenology/NDVI ------------------------------------------
out_dir <- "C:\\Users\\Shawn\\Desktop\\paper\\graph"
filedir <- "E:\\GEDI\\Result\\Result\\testcase_table98_new.csv"
Data = read.csv(filedir,header=T)
Data$Colour = 0
Data$Colour[Data$status == "Evergreen"]="#006400"
Data$Colour[Data$status == "Leaf-on"]="#90ee90"
Data$Colour[Data$status == "Leaf-off"]="red"
Data$Colour[Data$status == "Transition"]="blue"

reg1 <- lm(Data$r2 ~ Data$NDVI_diff, data = Data)
coeff <- round(reg1$coefficients , 2)

txt <- paste(" Linear regression: y = ",coeff[1] , " + " , coeff[2] , "*x",
             "\n" , "Adjusted R-squared: ",round(summary(reg1)$adj.r.squared,3)
)

grob <- grobTree(textGrob(txt, x=0.1,  y=0.82, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))
Data$SVI = Data$NDVI_diff/Data$SD_NDVI
ggplot(Data, aes(x=NDVI_diff, y=r2, color=status))+ 
  theme_bw()+
  ylim(0.1, 1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank(),legend.position=c(0.8,0.2))+
  scale_color_manual(labels = c("Evergreen", "Leaf-off","Leaf-on", "Transition"), 
                     values = c("#006400", "red","#90ee90", "blue"))+
  theme(text=element_text(size=20))+ 
  theme(text=element_text(family="A"))+
  geom_point(size = 3)+
  geom_smooth(
    method="lm", 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "black")+ 
  annotation_custom(grob) +
  labs(x="Relative Greenness", y=expression("R"^2))

out <- file.path(out_dir, "r2_phenology.jpg")
ggsave(out, height=10, width=15, dpi=1200)

#R2~phenology boxplot
give.n <- function(x){
  return(c(x = x, y = 1, label = length(x)))
}
ggplot(Data, aes(x=status, y=r2, fill=status)) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_boxplot() +
  theme(text=element_text(size=20)) +
  stat_summary(fun.data = give.n, geom = "text")+
  ylab(expression("R"^2))+xlab("Phenology")+
  theme(legend.title = element_blank())+
  theme(text=element_text(family="A"))
out_phen <- file.path(out_dir, "r2_phenology_boxplot.jpg")
ggsave(out_phen, height=10, width=15, dpi=1200)



#Grouped histogram for RH 50, 75, 90, 95, 98, 100 ---------------------------

filedir <- "E:\\GEDI\\Result\\Result\\All_on.csv"
Data1 = read.csv(filedir,header=T)
filedir <- "E:\\GEDI\\Result\\Result\\All_evergreen.csv"
Data2 = read.csv(filedir,header=T)
Data = rbind(Data1,Data2)  

#Data <- Data[Data$site == "Duku",]
#Data <- Data[Data$site == "Venetia",]


filedir = "E:\\GEDI\\Result\\Result\\All_off.csv"
Data = read.csv(filedir,header=T)

Data <- na.omit(Data)
myvars <- c("Unnamed..0","RH1_50","RH1_75","RH1_90","RH1_95","RH3_98","RH1_100",
            'GEDI_sim_rh_50','GEDI_sim_rh_75','GEDI_sim_rh_90',
            'GEDI_sim_rh_95','GEDI_sim_rh_98','GEDI_sim_rh_100')
data <- Data[myvars]
Melt <- melt(data, id.var='Unnamed..0')


p1<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(2, 3)+
  theme(legend.title = element_blank())+
  geom_histogram(data=filter(Melt, grepl('50',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=20,alpha = 0.2)+
  xlab("Relative Height 50 (m)") + ylab("Count")+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20)) 

p2<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(2, 6)+
  geom_histogram(data=filter(Melt, grepl('75',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=20,alpha = 0.2) +
  xlab("Relative Height 75 (m)") + ylab("Count")+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))

p3<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(2, 6)+
  geom_histogram(data=filter(Melt, grepl('90',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=20,alpha = 0.2) +
  xlab("Relative Height 90 (m)") + ylab("Count")+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))

p4<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(2, 15)+
  geom_histogram(data=filter(Melt, grepl('95',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=20,alpha = 0.2) +
  xlab("Relative Height 95 (m)") + ylab("Count")+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))

p5<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(2, 15)+
  geom_histogram(data=filter(Melt, grepl('98',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=20,alpha = 0.2) +
  xlab("Relative Height 98 (m)") + ylab("Count")+
  theme(legend.position = c(1.7,0.5))+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))

p6<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(2, 15)+
  geom_histogram(data=filter(Melt, grepl('100',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=20,alpha = 0.2) +
  xlab("Relative Height 100 (m)") + ylab("Count")+
  theme(legend.position = c(1.7,0.5))+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))

#leg <- get_legend(p1)
ggarrange(p1,p2,p3,p4,p5,p6,ncol=3,nrow=2) 
ggsave(out_dir, height=10, width=15, dpi=1200)
out = "E:\\GEDI\\Result\\Figure\\histogram_2up.jpg"
ggsave(out,height=12, width=18, dpi=600)

#special case, venetia and dukuduku
filedir <- "E:\\GEDI\\Result\\Result\\All_on.csv"
Data1 = read.csv(filedir,header=T)
filedir <- "E:\\GEDI\\Result\\Result\\All_evergreen.csv"
Data2 = read.csv(filedir,header=T)
Data = rbind(Data1,Data2)  

Data <- Data[Data$site == "Duku",]
Data <- Data[Data$site == "Venetia",]

myvars <- c("Unnamed..0","RH1_50","RH1_75","RH1_90","RH1_95","RH3_98","RH1_100",
            'GEDI_sim_rhMax_50','GEDI_sim_rhMax_75','GEDI_sim_rhMax_90',
            'GEDI_sim_rhMax_95','GEDI_sim_rh_98','GEDI_sim_rhMax_100')
data <- Data[myvars]
Melt <- melt(data, id.var='Unnamed..0')

p7<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(0, 10)+
  geom_histogram(data=filter(Melt, grepl('98',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins = 30,alpha = 0.2) +
  xlab("Relative Height 98 (m)") + ylab("Count")+
  theme(legend.position = c(1.7,0.5))+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))

p8<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(0, 10)+
  geom_histogram(data=filter(Melt, grepl('98',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins = 30,alpha = 0.2) +
  xlab("Relative Height 98 (m)") + ylab("Count")+
  theme(legend.position = c(1.7,0.5))+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))
leg <- get_legend(p7)
ggarrange(p7,p8,ncol=2,nrow=1,
          common.legend = TRUE,legend.grob = leg,legend="bottom") 
#Grouped histogram for Cover ---------------------------

filedir = "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
myvars <- c("X","GEDI_sim_Cover","GEDI_Cover")
data <- Data[myvars]
data <- data[data$GEDI_sim_Cover < 0.65,]
Melt <- melt(data, id.var='X')
ggplot(Melt, aes(value)) +
  geom_histogram(data=Melt,
                 aes(color = variable, fill = variable),
                 position = "identity", bins = 30,alpha = 0.2) +
  xlab("Relative Height 50") + ylab("Count")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))


#qqplot
filedir <- "E:\\GEDI\\Result\\Result\\All_on.csv"
Data1 = read.csv(filedir,header=T)
filedir <- "E:\\GEDI\\Result\\Result\\All_evergreen.csv"
Data2 = read.csv(filedir,header=T)
data = rbind(Data1,Data2) 

filedir = "E:\\GEDI\\Result\\Result\\All_off.csv"
data = read.csv(filedir,header=T)

data <- na.omit(data)
par(mfrow=c(2,3),mgp=c(2.5,1,0))
par(pty="s")
GEDI_sim_rh_50 <- data.matrix(as.vector(data["GEDI_sim_rh_50"]))
RH1_50 <- data.matrix(as.vector(data["RH1_50"]))
qqplot(GEDI_sim_rh_50,RH1_50,cex.axis=2,cex.lab=2,las=1,family="A",
       xlab=expression("GEDI-RH50"["Sim"]~"(m)"), 
       ylab=expression("GEDI-RH50"["Orb"]~"(m)"),xlim=c(2,3),ylim=c(0,3))
abline(c(0,1))

GEDI_sim_rh_75 <- data.matrix(as.vector(data["GEDI_sim_rh_75"]))
RH1_75 <- data.matrix(as.vector(data["RH1_75"]))
qqplot(GEDI_sim_rh_75,RH1_75,cex.axis=2,cex.lab=2,las=1,family="A",
       xlab=expression("GEDI-RH75"["Sim"]~"(m)"), 
       ylab=expression("GEDI-RH75"["Orb"]~"(m)"),xlim=c(2,6),ylim=c(2,6))
abline(c(0,1))

GEDI_sim_rh_90 <- data.matrix(as.vector(data["GEDI_sim_rh_90"]))
RH1_90 <- data.matrix(as.vector(data["RH1_90"]))
qqplot(GEDI_sim_rh_90,RH1_90,cex.axis=2,cex.lab=2,las=1,family="A",
       xlab=expression("GEDI-RH90"["Sim"]~"(m)"), 
       ylab=expression("GEDI-RH90"["Orb"]~"(m)"),xlim=c(2,6),ylim=c(2,6))
abline(c(0,1))

GEDI_sim_rh_95 <- data.matrix(as.vector(data["GEDI_sim_rh_95"]))
RH1_95 <- data.matrix(as.vector(data["RH1_95"]))
qqplot(GEDI_sim_rh_95,RH1_95,cex.axis=2,cex.lab=2,las=1,family="A",
       xlab=expression("GEDI-RH95"["Sim"]~"(m)"), 
       ylab=expression("GEDI-RH95"["Orb"]~"(m)"),xlim=c(2,15),ylim=c(2,15))
abline(c(0,1))

GEDI_sim_rh_98 <- data.matrix(as.vector(data["GEDI_sim_rh_98"]))
RH3_98 <- data.matrix(as.vector(data["RH3_98"]))
qqplot(GEDI_sim_rh_98,RH3_98,cex.axis=2,cex.lab=2,las=1,family="A",
       xlab=expression("GEDI-RH98"["Sim"]~"(m)"), 
       ylab=expression("GEDI-RH98"["Orb"]~"(m)"),xlim=c(2,15),ylim=c(2,15))
abline(c(0,1))

GEDI_sim_rh_100 <- data.matrix(as.vector(data["GEDI_sim_rh_100"]))
RH1_100 <- data.matrix(as.vector(data["RH1_100"]))
qqplot(GEDI_sim_rh_100,RH1_100,cex.axis=2,cex.lab=2,las=1,family="A",
       xlab=expression("GEDI-RH100"["Sim"]~"(m)"), 
       ylab=expression("GEDI-RH100"["Orb"]~"(m)"),
       xlim=c(2,15),ylim=c(2,15))
abline(c(0,1))


filedir = "E:\\GEDI\\Result\\Result\\All.csv"
data = read.csv(filedir,header=T)
GEDI_sim_rh_98 <- data.matrix(as.vector(data["GEDI_sim_rh_98"]))
RH3_98 <- data.matrix(as.vector(data["RH3_98"]))
ks.test(data1, data2)

filedir <- "E:\\GEDI\\Archive\\Result\\Result\\All_on.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$GEDI_sim_rh_98 <= 15,]
GEDI_sim_rh_98 <- data.matrix(as.vector(Data["GEDI_sim_rh_98"]))
RH3_98 <- data.matrix(as.vector(Data["RH3_98"]))
bks=c(1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,
      10,10.5,11,11.5,12,12.5,13)
xc=table(cut(RH3_98,breaks=bks))      # calc. individual counts in table for x
yc=table(cut(GEDI_sim_rh_98,breaks=bks))      # corresponding counts for y
rbind(xc,yc)                     # what the table looks like
chisq.test(rbind(xc,yc))         # testing the result


library(fpc)
round(bhattacharyya.dist(mean(GEDI_sim_rh_98),
                         mean(RH3_98),
                         cov(GEDI_sim_rh_98),
                         cov(RH3_98)),digits=2)




library(moments)
skewness(GEDI_sim_rh_98)
kurtosis(GEDI_sim_rh_98)
skewness(RH3_98)
kurtosis(RH3_98)

#cover
library(raster)
Locdir <- "E:\\GEDI\\ALS_archive\\LiDAR_CHM"
folders <- list.files(path = Locdir, full.names = TRUE, pattern = "Limpopo")
for (folder in folders){
  chms <- list.files(path = folder, full.names = TRUE, pattern = "tif")
  list1 <- 0
  list2 <- 0
  i = 1
  for (chm in chms){
    r <- values(raster(chm))
    r2 <- r[r > 1]
    list1 <- c(list1, length(r[!is.na(r)]))
    list2 <- c(list2, length(r2[!is.na(r2)]))
    i <- i + 1
    #print(length(r2[!is.na(r2)])/length(r2[!is.na(r)]))
  }
  c1 <- sum(list1)/(length(list1)-1)
  c2 <- sum(list2)/(length(list2)-1)
  cover <- c2/c1
  print(folder)
  print(i)
  print(cover)
}

#mean/std als height
library(raster)
Locdir <- "E:\\GEDI\\ALS_archive\\LiDAR_CHM"
folders <- list.files(path = Locdir, full.names = TRUE, pattern = "Limpopo")
for (folder in folders){
  chms <- list.files(path = folder, full.names = TRUE, pattern = "tif")
  list1 <- 0
  for (chm in chms){
    r <- getValues(raster(chm))
    r <- r[!is.na(r)]
    r <- r[r > 1]
    list1 <- c(list1, r)
  }
  c1 <- mean(list1)
  c2 <- sd(list1)
  print(folder)
  print(c1)
  print(c2)
}


#slope
library(raster)
Locdir <- "E:\\GEDI\\slope"
chms <- list.files(path = Locdir, full.names = TRUE, pattern = "Slope.tif")
for (chm in chms){
  r <- getValues(raster(chm))
  r <- r[!is.na(r)]
  r <- r[r > -100 & r < 100]
  print(chm)
  print(mean(r))
  print(sd(r))
}










#new analysis
#table for paper
Locdir <- "E:\\GEDI\\Result\\Result"
pattern = 'testcase'
files <- list.files(path = Locdir, full.names = TRUE, pattern = paste0(pattern,".csv"))
listt <- list("Studysite","Orbit","Sample","Power","Coverage",
              "r2","Power_R2","Coverage_R2",
              "Mean", "RMSE","%RMSE",
              "MD","%RB","MAE","Solar")
df <- data.frame(listt)
colnames(df) <- listt
for (i in files){
  print(i)
  Data = read.csv(i,header=T)
  Data1 <- Data[Data$Type == 1,]
  list1 <- list()
  Data = read.csv(i,header=T)
  Data <- Data %>% separate(Beam, c("Beam", "Type"))
  #Data <- Data[Data$new_quality_flag1 == 1 & Data$sensitivity_a1 > 0.95,]
  if(length(Data[,1])==0) next
  Data1 <- Data[Data$Type == 1,]
  Data2 <- Data[Data$Type == 0,]
  Data <- na.omit(Data)
  if (nrow(Data1) > 1){
    r2_p <- cor(Data1$RH3_98, Data1$GEDI_sim_rh_98,method = "pearson")^2
  }
  else{
    r2_p = 0
  }
  if (nrow(Data2) > 1){
    r2_c <- cor(Data2$RH3_98, Data2$GEDI_sim_rh_98,method = "pearson")^2
  }
  else{
    r2_c = 0
  }

  Mean <- mean(Data$RH3_98)       
  RMSE <- sqrt(mean((Data$RH3_98 - Data$GEDI_sim_rh_98)^2))
  rRMSE <- 100*sqrt(mean((Data$RH3_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)
  MD <- mean(Data$RH3_98 - Data$GEDI_sim_rh_98)
  RB <- 100*MD/mean(Data$GEDI_sim_rh_98)
  MAE <- mae(Data$GEDI_sim_rh_98, Data$RH3_98)
  Sub = strsplit(substring(strsplit(i, "Result")[[1]][3], first = 2),"_")
  Studysite = Sub[[1]][1]
  Orbit = substring(Sub[[1]][2],first = 3)
  print(paste0("Studysite ",Studysite))
  print(paste0("Orbit: ",Orbit))
  print(paste0("Sample: ",nrow(Data)))
  print(paste0("Power: ",nrow(Data1)))
  print(paste0("Coverage: ",nrow(Data2)))
  print(paste0("r2: ",round(cor(Data$RH3_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)))
  print(paste0("r2_p: ",r2_p))
  print(paste0("r2_c: ",r2_c))
  print(paste0("Mean ",round(Mean,3)))
  print(paste0("RMSE: ",round(RMSE,3)))
  print(paste0("%RB: ",round(RB,3))) 
  print(paste0("%RMSE: ",round(rRMSE,3)))
  print(paste0("Mean bias: ",round(MD,3)))
  print(paste0("MAE: ",round(MAE,3)))
  print(paste0("Solar: ",mean(Data$solar_elevation)))
  list1[[1]] <- Studysite
  list1[[2]] <- Orbit
  list1[[3]] <- nrow(Data)
  list1[[4]] <- nrow(Data1)
  list1[[5]] <- nrow(Data2)
  list1[[6]] <- round(cor(Data$RH3_98, Data$GEDI_sim_rh_98)^2,3)
  list1[[7]] <- r2_p
  list1[[8]] <- r2_c
  list1[[9]] <- round(Mean,3)
  list1[[10]] <- round(RMSE,3)
  list1[[11]] <- round(rRMSE,3)
  list1[[12]] <- round(MD,3)
  list1[[13]] <- round(RB,3)
  list1[[14]] <- round(MAE,3)
  list1[[15]] <- mean(Data$solar_elevation)
  listt <- mapply(c, listt, list1, SIMPLIFY=FALSE)
  df1 <- data.frame(list1)
  colnames(df1) <- listt <- list("Studysite","Orbit","Sample","Power","Coverage",
                                 "r2","Power_R2","Coverage_R2",
                                 "Mean", "RMSE","%RMSE",
                                 "MD","%RB","MAE","Solar")
  df <- rbind(df,df1)
}

write.table(df,file=file.path(Locdir,paste0(pattern,"_table98_new_67.csv")), quote=F,sep=",",row.names=F)


#all test case summary scatterplot
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)

filedir <- "E:\\GEDI\\Result\\Result\\All_on.csv"
Data1 = read.csv(filedir,header=T)
filedir <- "E:\\GEDI\\Result\\Result\\All_evergreen.csv"
Data2 = read.csv(filedir,header=T)
Data = rbind(Data1,Data2)  

filedir <- "E:\\GEDI\\Result\\Result\\All_off.csv"
Data = read.csv(filedir,header=T)

#filedir <- "E:\\GEDI\\Result\\Result\\All_evergreen.csv"
#Data = read.csv(filedir,header=T)
#Data <- Data[Data$site == "Duku",]
#filedir <- "E:\\GEDI\\Result\\Result\\All_off.csv"
#Data = read.csv(filedir,header=T)
#Data <- Data[Data$site == "Venetia",]

Data <- Data[Data$GEDI_sim_rh_98 <= 15,]
Data <- na.omit(Data)
r2 = round(cor(Data$RH2_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3)
Mean <- mean(Data$GEDI_sim_rh_98)       
RMSE <- sqrt(mean((Data$RH2_98 - Data$GEDI_sim_rh_98)^2))
rRMSE <- 100*sqrt(mean((Data$RH2_98 - Data$GEDI_sim_rh_98)^2))/mean(Data$GEDI_sim_rh_98)
MD <- mean(Data$RH2_98 - Data$GEDI_sim_rh_98)
RB <- 100*MD/mean(Data$GEDI_sim_rh_98)
#MAE <- mae(Data$GEDI_sim_rh_98, Data$RH3_98)
SD <- sd(Data$RH2_98 - Data$GEDI_sim_rh_98)
txt <- paste(" R square: ",r2,
             "\n" , "RMSE: ", round(RMSE,3),"m",
             "\n" , "%RMSE: ", round(rRMSE,3),"%",
             "\n" , "Bias: ", round(MD,3),"m",
             "\n" , "%Bias: ", round(RB,3),"%",
             "\n" , "Sample size: ", nrow(Data))

grob <- grobTree(textGrob(txt, x=0.1,  y=0.77, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))
min1 <- min(Data$GEDI_sim_rh_98)
min2 <- min(Data$RH2_98)




p2 <- ggplot(Data, aes(x=GEDI_sim_rh_98, y=RH2_98))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=20)) + 
  scale_x_continuous(limits = c(min1,15),minor_breaks = seq(0,15,2))+
  scale_y_continuous(limits = c(min2,15),minor_breaks = seq(0,15,2))+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  annotation_custom(grob) +
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  geom_abline(intercept = MD, slope = 1, color="red", 
              linetype="solid", size=1.5)+
  geom_abline(intercept = MD + SD, slope = 1, color="red", 
              linetype="dashed", size=1)+
  geom_abline(intercept = MD - SD, slope = 1, color="red", 
              linetype="dashed", size=1)+
  geom_abline(intercept = MD + 2*SD, slope = 1, color="blue", 
              linetype="dashed", size=1)+
  geom_abline(intercept = MD - 2*SD, slope = 1, color="blue", 
              linetype="dashed", size=1)+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y=expression("GEDI-RH98"["Orb"]~"(m)"))+
  theme(legend.position="bottom",legend.key.width=unit(3,"cm"))


p3 <- ggplot(Data, aes(x=GEDI_sim_rh_98, y=RH2_98))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(size=20)) + 
  scale_x_continuous(limits = c(min1,15),minor_breaks = seq(0,15,2))+
  scale_y_continuous(limits = c(min2,15),minor_breaks = seq(0,15,2))+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  annotation_custom(grob) +
  geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
  geom_abline(intercept = MD, slope = 1, color="red", 
              linetype="solid", size=1.5)+
  geom_abline(intercept = MD + SD, slope = 1, color="red", 
              linetype="dashed", size=1)+
  geom_abline(intercept = MD - SD, slope = 1, color="red", 
              linetype="dashed", size=1)+
  geom_abline(intercept = MD + 2*SD, slope = 1, color="blue", 
              linetype="dashed", size=1)+
  geom_abline(intercept = MD - 2*SD, slope = 1, color="blue", 
              linetype="dashed", size=1)+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y=expression("GEDI-RH98"["Orb"]~"(m)"))+
  theme(legend.position="bottom",legend.key.width=unit(3,"cm"))

ggarrange(p2,p3,ncol=2,nrow=1) 
out = "E:\\GEDI\\Result\\Figure\\scatterplot_3.jpg"
ggsave(out,height=12, width=18, dpi=600)



#error boxplot
filedir <- "E:\\GEDI\\Result\\Result\\All_on.csv"
Data1 = read.csv(filedir,header=T)
filedir <- "E:\\GEDI\\Result\\Result\\All_evergreen.csv"
Data2 = read.csv(filedir,header=T)
Data = rbind(Data1,Data2)  
Data <- na.omit(Data)

Data <- Data[Data$GEDI_sim_rh_98 <= 15,]
Data$diff <- Data$RH3_98 - Data$GEDI_sim_rh_98
Data$abs_diff <- abs(Data$RH3_98 - Data$GEDI_sim_rh_98)
Data$group_RH <- cut(Data$GEDI_sim_rh_98,breaks = c(1,2,3,4,5,6,
                                                    7,8,9,10,11,12,
                                                    13,14,15)
                     ,dig.lab=1)

p1 <- ggplot(Data, aes(x=GEDI_sim_rh_98, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(width = 1)+
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-12, 5))+
  scale_y_continuous(minor_breaks = seq(-12, 5, 1),breaks = seq(-12, 5, 2))+
  scale_x_continuous(breaks = seq(0, 15, 1))+
  theme_bw()+
  labs(y="Bias (m)")+
  theme(axis.title.x=element_blank())+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


filedir <- "E:\\GEDI\\Result\\Result\\All_off.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)

Data <- Data[Data$GEDI_sim_rh_98 <= 15,]
Data$diff <- Data$RH3_98 - Data$GEDI_sim_rh_98
Data$abs_diff <- abs(Data$RH3_98 - Data$GEDI_sim_rh_98)
Data$group_RH <- cut(Data$GEDI_sim_rh_98,breaks = c(1,2,3,4,5,6,
                                                    7,8,9,10,11,12,13,14,15),
                     dig.lab=1)



p2 <- ggplot(Data, aes(x=GEDI_sim_rh_98, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(width = 0.2)+
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-12, 5))+
  scale_y_continuous(minor_breaks = seq(-12, 5, 1),breaks = seq(-12, 5, 2))+
  scale_x_continuous(breaks = seq(0, 15, 1))+
  theme_bw()+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m) (leaf-on+evergreen)"), 
       y="Bias (m)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


x <- seq(1,14,1)
y1 <- c(66.08535,
        21.38219,
        -3.92559,
        -10.31038,
        -11.30874,
        -11.66541,
        -17.58376,
        -22.18148,
        -22.1587,
        -23.51561,
        -20.73811,
        -21.00722,
        -14.90351,
        -15.57523)


y2 <- c(58.76264,
        11.43277,
        -11.32974,
        -20.57196,
        -26.19084,
        -30.41251,
        -32.72303,
        -38.3856,
        -38.50615,
        -36.75932,
        -33.35146,
        -31.17157,
        -26.70735,
        -28.90161)


l <- cbind(x,y1)
df <- data.frame(l)
p3 <- ggplot(df, aes(x=x+0.5, y=y1)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-40, 70))+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m) (leaf-on+evergreen)"), 
       y="%Bias (%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))

l <- cbind(x,y2)
df <- data.frame(l)
p4 <- ggplot(df, aes(x=x+0.5, y=y2)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-40, 70))+
  theme(axis.title.y=element_blank())+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m) (leaf-off)"), 
       y="%Bias (%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p2,p3,p4,ncol=2,nrow=2) 

out = "E:\\GEDI\\Result\\Figure\\errordist.jpg"
#ggarrange(p1,p2,p3,p4,ncol=2,nrow=2,common.legend = TRUE, legend="bottom") 
ggsave(out,height=12, width=18, dpi=1200)




#%bias 6 methods
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)

Data$diff <- Data$RH6_98 - Data$GEDI_sim_rh_98

a <- tapply(Data$diff, cut(Data$GEDI_sim_rh_98,breaks = c(1,2,3,4,5,6,
                                                          7,8,9,10,11,12,13,14,15),
                           dig.lab=1), mean)
b <- tapply(Data$GEDI_sim_rh_98, cut(Data$GEDI_sim_rh_98,breaks = c(1,2,3,4,5,6,
                                                                    7,8,9,10,11,12,13,14,15),
                                     dig.lab=1), mean)
RB <- a/b*100
x <- seq(1,14,1)
l <- cbind(x,RB)
df <- data.frame(l)
p6 <- ggplot(df, aes(x=x+0.5, y=RB)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  coord_cartesian(ylim = c(-40, 70))+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y="Algo 6 %Bias (%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p2,p3,p4,p5,p6,ncol=2,nrow=3) 
out = "E:\\GEDI\\Result\\Figure\\bias6.jpg"

ggsave(out,height=12, width=18, dpi=600)




#%bias difference (%MAE)
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)

Data$diff <- Data$RH3_98 - Data$GEDI_sim_rh_98

a <- tapply(Data$diff, cut(Data$GEDI_sim_rh_98,breaks = c(1,2,3,4,5,6,
                                                          7,8,9,10,11,12,13,14,15),
                           dig.lab=1), mean)
b <- tapply(Data$GEDI_sim_rh_98, cut(Data$GEDI_sim_rh_98,breaks = c(1,2,3,4,5,6,
                                                                    7,8,9,10,11,12,13,14,15),
                                     dig.lab=1), mean)
RB <- abs(a/b*100)


Data$diff1 <- Data$RH3_98 - Data$GEDI_sim_rh_98
a1 <- tapply(Data$diff1, cut(Data$GEDI_sim_rh_98,breaks = c(1,2,3,4,5,6,
                                                           7,8,9,10,11,12,13,14,15),
                            dig.lab=1), mean)
RB1 <- abs(a1/b*100)

RB_diff <- RB1 - RB


x <- seq(1,14,1)
l <- cbind(x,RB_diff)
df <- data.frame(l)
p3 <- ggplot(df, aes(x=x+0.5, y=RB_diff)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  geom_text(aes(label=round(RB_diff,3),family="A",size = 20), 
            position=position_dodge(width=0.9), 
            vjust = ifelse(RB_diff >= 0, 0, 1))+
  coord_cartesian(ylim = c(-40, 40))+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y="%MAE difference (3-1) (%)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


x <- c(1,2,3,4,5,6)

y_r2 <- c(round(cor(Data$RH3_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3),
        round(cor(Data$RH2_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3),
        round(cor(Data$RH3_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3),
        round(cor(Data$RH4_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3),
        round(cor(Data$RH5_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3),
        round(cor(Data$RH6_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3))
l <- cbind(x,y_r2)
df <- data.frame(l)

p7 <- ggplot(df, aes(x=x, y=y_r2)) + 
  geom_bar(stat='identity')+
  geom_text(aes(label=y_r2,family="A",size = 20), 
            position=position_dodge(width=0.9), 
            vjust=-1)+
  theme_bw()+
  coord_cartesian(ylim = c(0, 0.8))+
  labs(x="Setting group number", 
       y=expression("R"^2))+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


ggarrange(p2,p3,p4,p5,p6,p7,ncol=2,nrow=3) 
out = "E:\\GEDI\\Result\\Figure\\bias6_diff.jpg"

ggsave(out,height=12, width=18, dpi=600)




#bias difference (MAE)
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)

Data$diff <- Data$RH3_98 - Data$GEDI_sim_rh_98
a <- tapply(Data$diff, cut(Data$GEDI_sim_rh_98,breaks = c(1,2,3,4,5,6,
                                                          7,8,9,10,11,12,13,14,15),
                           dig.lab=1), mean)

Data$diff1 <- Data$RH3_98 - Data$GEDI_sim_rh_98
a1 <- tapply(Data$diff1, cut(Data$GEDI_sim_rh_98,breaks = c(1,2,3,4,5,6,
                                                            7,8,9,10,11,12,13,14,15),
                             dig.lab=1), mean)

MAE <- abs(a1)-abs(a)


x <- seq(1,14,1)
l <- cbind(x,MAE)
df <- data.frame(l)
ggplot(df, aes(x=x+0.5, y=MAE)) + 
  geom_bar(stat='identity')+
  theme_bw()+
  geom_text(aes(label=round(MAE,3),family="A",size = 20), 
            position=position_dodge(width=0.9), 
            vjust = ifelse(MAE >= 0, 0, 1))+
  coord_cartesian(ylim = c(-1, 1))+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y="MAE difference (3-1) (m)")+
  scale_y_continuous(minor_breaks = seq(-200, 100, 10),breaks = seq(-200, 100, 10))+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


out = "E:\\GEDI\\Result\\Figure\\MAE3_diff.jpg"

ggsave(out,height=12, width=18, dpi=600)


x <- c(1,2,3,4,5,6)

y_r2 <- c(round(cor(Data$RH3_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3),
          round(cor(Data$RH2_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3),
          round(cor(Data$RH3_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3),
          round(cor(Data$RH4_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3),
          round(cor(Data$RH5_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3),
          round(cor(Data$RH6_98, Data$GEDI_sim_rh_98,method = "pearson")^2,3))
l <- cbind(x,y_r2)
df <- data.frame(l)

p7 <- ggplot(df, aes(x=x, y=y_r2)) + 
  geom_bar(stat='identity')+
  geom_text(aes(label=y_r2,family="A",size = 20), 
            position=position_dodge(width=0.9), 
            vjust=-1)+
  theme_bw()+
  coord_cartesian(ylim = c(0, 1))+
  labs(x="Setting group number", 
       y=expression("R"^2))+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))


ggarrange(p2,p3,p4,p5,p6,p7,ncol=2,nrow=3) 
out = "E:\\GEDI\\Result\\Figure\\bias6_diff.jpg"

ggsave(out,height=12, width=18, dpi=600)



#diff~green 
filedir <- "E:\\GEDI\\Result\\Result\\testcase_table98_new.csv"
Data = read.csv(filedir,header=T)
Data$Colour = 0
Data$Colour[Data$status == "Evergreen"]="#006400"
Data$Colour[Data$status == "Leaf-on"]="#90ee90"
Data$Colour[Data$status == "Leaf-off"]="red"
Data$Colour[Data$status == "Transition"]="blue"
Data$SVI = Data$NDVI_diff/Data$SD_NDVI
reg1 <- lm(Data$MD ~ Data$SVI, data = Data)
coeff <- round(reg1$coefficients , 2)

txt <- paste("R-squared: ",round(summary(reg1)$adj.r.squared,3)
)

grob <- grobTree(textGrob(txt, x=0.1,  y=0.8, hjust=0,
                          gp=gpar(fontfamily ="A",fontsize=20)))


p1 <- ggplot(Data, aes(x=SVI, y=RB, color=status))+ 
  theme_bw()+
  annotation_custom(grob) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank(),legend.position=c(0.8,0.8))+
  scale_color_manual(labels = c("Evergreen", "Leaf-off","Leaf-on", "Transition"), 
                     values = c("#006400", "red","#90ee90", "blue"))+
  theme(text=element_text(size=20))+ 
  theme(text=element_text(family="A"))+
  geom_point(size = 3)+
  geom_smooth(
    method="lm", 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "black")+ 
  labs(x="Relative Greenness", y="Bias (m)")

#diff~phenology boxplot
give.n <- function(x){
  return(c(x = x, y = 50, label = length(x)))
}
p2 <- ggplot(Data, aes(x=status, y=RB, fill=status)) + 
  theme_bw()+
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(outlier.shape = NA, width = 0.2)+
  theme(text=element_text(size=20)) +
  stat_summary(fun.data = give.n, geom = "text")+ 
  labs(x="Phenology")+
  theme(axis.title.y=element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank(),legend.position=c(0.8,0.8))+
  scale_fill_manual(values=c("#006400", "red","#90ee90", "blue"))

ggarrange(p1,p2,ncol=2,nrow=1) 

out = "E:\\GEDI\\Result\\Figure\\phenology_greenness.jpg"
ggsave(out,height=12, width=18, dpi=600)


#for test case Welverdiendt 
out_dir <- "C:\\Users\\Shawn\\Desktop\\paper\\graph"
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- Data[Data$site == "Welverdiendt",]


ggplot(Data, aes(x=GEDI_sim_rh_98, y=RH3_98,color=orbit))+ 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  stat_poly_eq(
    formula = y ~ x, 
    label.x.npc = "left", label.y.npc = "top",vstep = 0.03,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~")), 
    size = 5,
    parse = TRUE)+
  theme(text=element_text(size=20)) + 
  theme(text=element_text(family="A"))+
  theme(legend.title = element_blank())+
  geom_abline(color="gray", linetype="dashed")+
  labs(x=expression("GEDI"["Sim"]~"(m)"), 
       y=expression("GEDI"["Rec"]~"(m)"))+
  scale_color_manual(values=c("#00ffff", "#ff4d4d", "#ff0000", "#b30000","#0000b3",
                              "#80ff00","#00b300","#00b300","#00b300","#00ffff",
                              "#008b8b","#ff0000","#ff0000","#ff0000","#b30000"),
                     labels = c("2019.04.28","2019.07.20",
                                "2019.08.02","2019.09.28",
                                "2019.11.15","2020.01.27",
                                "2020.02.01","2020.02.05",
                                "2020.02.13","2020.04.04",
                                "2020.05.12","2020.08.10",
                                "2020.08.21","2020.08.25",
                                "2020.09.02"))

out <- file.path(out_dir,"Wel_combined1.jpg")


#outlier percent 2sigma
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)

filedir <- "E:\\GEDI\\Result\\Result\\All_on.csv"
Data1 = read.csv(filedir,header=T)
filedir <- "E:\\GEDI\\Result\\Result\\All_evergreen.csv"
Data2 = read.csv(filedir,header=T)
Data = rbind(Data1,Data2)  

filedir <- "E:\\GEDI\\Result\\Result\\All_off.csv"
Data = read.csv(filedir,header=T)



Data$diff <- Data$RH3_98 - Data$GEDI_sim_rh_98
Diff_mean <- mean(Data$diff)
Diff_sd <- sd(Data$diff)
Data_outliers <- Data[Data$diff < Diff_mean-2*Diff_sd |  
                        Data$diff > Diff_mean+2*Diff_sd,]



#other factor ~ bias
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)

Data <- Data[Data$GEDI_sim_rh_98 <= 20,]
Data$diff <- Data$RH3_98 - Data$GEDI_sim_rh_98

Data$abs_diff <- abs(Data$RH3_98 - Data$GEDI_sim_rh_98)
Data$group_RH <- cut(Data$sensitivity_a1,breaks = c(0.9,0.91,0.92,0.93,0.94,0.95,
                                                    0.96,0.97,0.98),dig.lab=1)
Data$Beam[Data$Beam == "BEAM0000_0"]="Coverage"
Data$Beam[Data$Beam == "BEAM0010_0"]="Coverage"
Data$Beam[Data$Beam == "BEAM0001_0"]="Coverage"
Data$Beam[Data$Beam == "BEAM0011_0"]="Coverage"
Data$Beam[Data$Beam == "BEAM1011_1"]="Power"
Data$Beam[Data$Beam == "BEAM0101_1"]="Power"
Data$Beam[Data$Beam == "BEAM0110_1"]="Power"
Data$Beam[Data$Beam == "BEAM1000_1"]="Power"
Data$Day[Data$orbit == "O02229"]="Day"
Data$Day[Data$orbit == "O06494"]="Day"
Data$Day[Data$orbit == "O07348"]="Day"
Data$Day[Data$orbit == "O06823"]="Day"
Data$Day[Data$orbit == "O06701"]="Day"
Data$Day[Data$orbit == "O02122"]="Day"
Data$Day[Data$orbit == "O05243"]="Day"
Data$Day[Data$orbit == "O06372"]="Day"
Data$Day[Data$orbit == "O09675"]="Day"
Data$Day[Data$orbit == "O08760"]="Day"
Data$Day[Data$orbit == "O02816"]="Day"
Data$Day[Data$orbit == "O09102"]="Day"
Data$Day[Data$orbit == "O03610"]="Day"
Data$Day[Data$orbit == "O09407"]="Day"
Data$Day[Data$orbit == "O02541"]="Day"
Data$Day[Data$orbit == "O03335"]="Day"
Data$Day[Data$orbit == "O02587"]="Day"
Data$Day[Data$orbit == "O04215"]="Day"
Data$Day[Data$orbit == "O06189"]="Day"
Data$Day[Data$orbit == "O09163"]="Day"
Data$Day[is.na(Data$Day)]="Night"
Data <- na.omit(Data)
Data$grp <- paste(Data[,"Beam"],Data[,"Day"])


give.n <- function(x){
  return(c(x = x, y = 4, label = length(x)))
}
#bias ~ beam day
p1<- ggplot(Data, aes(x=grp, y=diff, fill=grp)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(outlier.shape = NA, width = 0.2) +
  coord_cartesian(ylim = c(-4, 4))+
  scale_y_continuous(minor_breaks = seq(-4, 4, 0.5))+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x="Beams", 
       y="Bias (m)")+
  theme(legend.position = "none")+
  stat_summary(fun.data = give.n, geom = "text",size=7)+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))

#sensitivity ~ bias
p2<- ggplot(Data, aes(x=group_RH, y=diff, fill=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(outlier.shape = NA, width = 0.2)+
  coord_cartesian(ylim = c(-5, 5))+
  scale_y_continuous(minor_breaks = seq(-5, 5, 0.5))+
  theme_bw()+
  labs(x="Sensitivity",y="Bias (m)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c("#8b0000","#ff0000","#ffa500","#ffff00",
                             "#adff2f","#008000","#00ffff","#0000ff"))



ggarrange(p1,p2,ncol=1,nrow=2) 


out = "E:\\GEDI\\Result\\Figure\\Sensitivity.jpg"
ggsave(out,height=12, width=18, dpi=1200)


#sensitivity ~ beam day
give.n <- function(x){
  return(c(x = x, y = 1, label = length(x)))
}
ggplot(Data, aes(x=grp, y=sensitivity_a1, fill=grp)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(outlier.shape = NA, width = 0.2)+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x="Beams", 
       y="Sensitivity (%)")+
  theme(legend.position = "none")+
  stat_summary(fun.data = give.n, geom = "text",size=7)+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))


#error boxplot---rb
filedir <- "E:\\GEDI\\Result\\Result\\All_on.csv"
Data1 = read.csv(filedir,header=T)
filedir <- "E:\\GEDI\\Result\\Result\\All_evergreen.csv"
Data2 = read.csv(filedir,header=T)
Data = rbind(Data1,Data2)  

filedir <- "E:\\GEDI\\Result\\Result\\All_off.csv"
Data = read.csv(filedir,header=T)

Data <- na.omit(Data)

Data1 <- Data[Data$GEDI_sim_rh_98 <= 4,]

Data1 <- Data[Data$GEDI_sim_rh_98 >= 1 & Data$GEDI_sim_rh_98 <= 2,]


Data1 <- Data[Data$GEDI_sim_rh_98 >= 19 & Data$GEDI_sim_rh_98 <= 20,]

100*mean(Data1$RH3_98 - Data1$GEDI_sim_rh_98)/mean(Data1$GEDI_sim_rh_98)





#6 method
filedir <- "E:\\GEDI\\Result\\Result\\All.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)
Data <- Data[Data$GEDI_sim_rh_98 <= 15,]
Data$group_RH <- cut(Data$GEDI_sim_rh_98,breaks = c(1,2,3,4,5,6,
                                                    7,8,9,10,11,12,
                                                    13,14,15),dig.lab=1)

Data$diff <- Data$RH6_98 - Data$GEDI_sim_rh_98
y_label = "Algorithm 6 Bias (m)"
p6 <- ggplot(Data, aes(x=GEDI_sim_rh_98, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(width = 1)+
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-12, 5))+
  scale_y_continuous(minor_breaks = seq(-12, 5, 1),breaks = seq(-12, 5, 2))+
  scale_x_continuous(breaks = seq(0, 15, 1))+
  theme_bw()+
  labs(x=expression("GEDI-RH98"["Sim"]~"(m)"), 
       y=y_label)+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(text=element_text(family="A"))+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p2,p3,p4,p5,p6,ncol=2,nrow=3) 
out = "E:\\GEDI\\Result\\Figure\\6methods.jpg"
ggsave(out,height=12, width=18, dpi=600)



#method 3
filedir <- "E:\\GEDI\\Result\\Result\\All_on.csv"
Data1 = read.csv(filedir,header=T)
filedir <- "E:\\GEDI\\Result\\Result\\All_evergreen.csv"
Data2 = read.csv(filedir,header=T)
Data = rbind(Data1,Data2)  

Data <- na.omit(Data)
myvars <- c("Unnamed..0","RH3_50","RH3_75","RH3_90","RH3_95","RH3_98","RH3_100",
            'GEDI_sim_rh_50','GEDI_sim_rh_75','GEDI_sim_rh_90',
            'GEDI_sim_rh_95','GEDI_sim_rh_98','GEDI_sim_rh_100')
data <- Data[myvars]
Melt <- melt(data, id.var='Unnamed..0')


p1<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(0, 3)+
  theme(legend.title = element_blank())+
  geom_histogram(data=filter(Melt, grepl('50',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=20,alpha = 0.2)+
  xlab("Relative Height 50 (m)") + ylab("Count")+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20)) 

p2<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(0, 6)+
  geom_histogram(data=filter(Melt, grepl('75',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=20,alpha = 0.2) +
  xlab("Relative Height 75 (m)") + ylab("Count")+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))

p3<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(0, 6)+
  geom_histogram(data=filter(Melt, grepl('90',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=20,alpha = 0.2) +
  xlab("Relative Height 90 (m)") + ylab("Count")+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))

p4<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(0, 15)+
  geom_histogram(data=filter(Melt, grepl('95',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=20,alpha = 0.2) +
  xlab("Relative Height 95 (m)") + ylab("Count")+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))

p5<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(0, 15)+
  geom_histogram(data=filter(Melt, grepl('98',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=20,alpha = 0.2) +
  xlab("Relative Height 98 (m)") + ylab("Count")+
  theme(legend.position = c(1.7,0.5))+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))

p6<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(0, 15)+
  geom_histogram(data=filter(Melt, grepl('100',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=20,alpha = 0.2) +
  xlab("Relative Height 100 (m)") + ylab("Count")+
  theme(legend.position = c(1.7,0.5))+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=20))

#leg <- get_legend(p1)
ggarrange(p1,p2,p3,p4,p5,p6,ncol=3,nrow=2) 







# ------------------------------------------------------------------------------------------------ #
#figure histogram

filedir <- "E:\\GEDI\\Result\\Result\\All_on.csv"
Data1 = read.csv(filedir,header=T)
filedir <- "E:\\GEDI\\Result\\Result\\All_evergreen.csv"
Data2 = read.csv(filedir,header=T)
Data = rbind(Data1,Data2)  
out = "E:\\GEDI\\Result\\Figure\\figure histogram on.jpg"

filedir = "E:\\GEDI\\Result\\Result\\All_off.csv"
Data = read.csv(filedir,header=T)
out = "E:\\GEDI\\Result\\Figure\\figure histogram off.jpg"


Data <- na.omit(Data)
myvars <- c("Unnamed..0","RH3_50","RH3_75","RH3_90","RH3_95","RH3_98","RH3_100",
            'GEDI_sim_rh_50','GEDI_sim_rh_75','GEDI_sim_rh_90',
            'GEDI_sim_rh_95','GEDI_sim_rh_98','GEDI_sim_rh_100')
data <- Data[myvars]
Melt1 <- melt(data, id.var='Unnamed..0')

#png("E:\\GEDI\\Result\\Figure\\qqplot on.png", width = 18, height = 18, units = 'in', res = 600)

#scale_x_continuous(breaks=seq(0,3, by = 1))+

Melt <- Melt1[Melt1$value <= 3 & Melt1$value >= 0,]
p1<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=0:3)+
  theme(legend.title = element_blank())+
  geom_histogram(data=filter(Melt, grepl('50',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=3,binwidth = 1,alpha = 0.2)+
  xlab("Relative Height 50 (m)") + ylab("Count")+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=25)) 

Melt <- Melt1[Melt1$value <= 6,]
p2<- ggplot(Melt, aes(value)) +
  theme_bw()+
  scale_x_continuous(breaks=0:6)+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(data=filter(Melt, grepl('75',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=6,binwidth = 1,alpha = 0.2) +
  xlab("Relative Height 75 (m)") + ylab("Count")+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=25))

Melt <- Melt1[Melt1$value <= 6,]
p3<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=0:6)+
  geom_histogram(data=filter(Melt, grepl('90',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=6,binwidth = 1,alpha = 0.2) +
  xlab("Relative Height 90 (m)") + ylab("Count")+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=25))

Melt <- Melt1[Melt1$value <= 15,]
p4<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=0:15)+
  geom_histogram(data=filter(Melt, grepl('95',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=15,binwidth = 1,alpha = 0.2) +
  xlab("Relative Height 95 (m)") + ylab("Count")+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=25))

p5<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=0:15)+
  geom_histogram(data=filter(Melt, grepl('98',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=15,binwidth = 1,alpha = 0.2) +
  xlab("Relative Height 98 (m)") + ylab("Count")+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=25))


p6<- ggplot(Melt, aes(value)) +
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=0:15)+
  geom_histogram(data=filter(Melt, grepl('100',variable)),
                 aes(color = variable, fill = variable),
                 position = "identity", bins=15,binwidth = 1,alpha = 0.2) +
  xlab("Relative Height 100 (m)") + ylab("Count")+
  theme(legend.position = "none")+
  theme(text=element_text(family="A"))+
  theme(text=element_text(size=25))

ggarrange(p1,p2,p3,p4,p5,p6,ncol=3,nrow=2) 

ggsave(out,height=12, width=18, dpi=600)


#qqplot
filedir <- "E:\\GEDI\\Result\\Result\\All_on.csv"
Data1 = read.csv(filedir,header=T)
filedir <- "E:\\GEDI\\Result\\Result\\All_evergreen.csv"
Data2 = read.csv(filedir,header=T)
data = rbind(Data1,Data2) 
png("E:\\GEDI\\Result\\Figure\\figure qqplot on.png", width = 20, height = 14, units = 'in', res = 600)

filedir = "E:\\GEDI\\Result\\Result\\All_off.csv"
data = read.csv(filedir,header=T)
png("E:\\GEDI\\Result\\Figure\\figure qqplot off.png", width = 20, height = 14, units = 'in', res = 600)


par(pty="s",mfrow=c(2,3),mgp=c(4,1.5,0),mar=c(5,8,0,0), family = "A",oma=c(2,2,2,2),cex.lab=3, cex.axis=3, cex.main=3, cex.sub=3)

data <- na.omit(data)
GEDI_sim_rh_50 <- data.matrix(as.vector(data["GEDI_sim_rh_50"]))
RH1_50 <- data.matrix(as.vector(data["RH1_50"]))
qqplot(GEDI_sim_rh_50,RH1_50,cex.axis=3,cex.lab=3,las=1,family="A",
       xlab=expression("GEDI-RH50"["Sim"]~"(m)"), 
       ylab=expression("GEDI-RH50"["Orb"]~"(m)"),xlim=c(0,3),ylim=c(0,3))
abline(c(0,1))

GEDI_sim_rh_75 <- data.matrix(as.vector(data["GEDI_sim_rh_75"]))
RH1_75 <- data.matrix(as.vector(data["RH1_75"]))
qqplot(GEDI_sim_rh_75,RH1_75,cex.axis=3,cex.lab=3,las=1,family="A",
       xlab=expression("GEDI-RH75"["Sim"]~"(m)"), 
       ylab=expression("GEDI-RH75"["Orb"]~"(m)"),xlim=c(0,6),ylim=c(0,6))
abline(c(0,1))

GEDI_sim_rh_90 <- data.matrix(as.vector(data["GEDI_sim_rh_90"]))
RH1_90 <- data.matrix(as.vector(data["RH1_90"]))
qqplot(GEDI_sim_rh_90,RH1_90,cex.axis=3,cex.lab=3,las=1,family="A",
       xlab=expression("GEDI-RH90"["Sim"]~"(m)"), 
       ylab=expression("GEDI-RH90"["Orb"]~"(m)"),xlim=c(0,6),ylim=c(0,6))
abline(c(0,1))

GEDI_sim_rh_95 <- data.matrix(as.vector(data["GEDI_sim_rh_95"]))
RH1_95 <- data.matrix(as.vector(data["RH1_95"]))
qqplot(GEDI_sim_rh_95,RH1_95,cex.axis=3,cex.lab=3,las=1,family="A",
       xlab=expression("GEDI-RH95"["Sim"]~"(m)"), 
       ylab=expression("GEDI-RH95"["Orb"]~"(m)"),xlim=c(0,15),ylim=c(0,15))
abline(c(0,1))

GEDI_sim_rh_98 <- data.matrix(as.vector(data["GEDI_sim_rh_98"]))
RH3_98 <- data.matrix(as.vector(data["RH3_98"]))
qqplot(GEDI_sim_rh_98,RH3_98,cex.axis=3,cex.lab=3,las=1,family="A",
       xlab=expression("GEDI-RH98"["Sim"]~"(m)"), 
       ylab=expression("GEDI-RH98"["Orb"]~"(m)"),xlim=c(0,15),ylim=c(0,15))
abline(c(0,1))

GEDI_sim_rh_100 <- data.matrix(as.vector(data["GEDI_sim_rh_100"]))
RH1_100 <- data.matrix(as.vector(data["RH1_100"]))
qqplot(GEDI_sim_rh_100,RH1_100,cex.axis=3,cex.lab=3,las=1,family="A",
       xlab=expression("GEDI-RH100"["Sim"]~"(m)"), 
       ylab=expression("GEDI-RH100"["Orb"]~"(m)"),
       xlim=c(0,15),ylim=c(0,15))
abline(c(0,1))
dev.off()

