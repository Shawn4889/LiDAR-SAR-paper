#01082020
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
#changemaps 20201216
changemap <- function(date1,date2,studysite){
  out <- "E:\\ChangMap\\CHM\\CHM\\CHM_result"
  workspace <- "E:\\ChangMap\\CHM\\CHM\\CHM_processed\\subset"
  studysite1 <- paste0(studysite,date1,".tif")
  studysite2 <- paste0(studysite,date2,".tif")
  change_ext <- paste0("_Change_",date2,"_",date1,".tif")
  diff_ext <- paste0("_Diff_",date2,"_",date1,".tif")
  
  CHM1<- file.path(workspace,studysite1)
  CHM2<- file.path(workspace,studysite2)
  
  r1 <- raster(CHM1)
  r2 <- raster(CHM2)
  r3 <- raster(CHM1)
  #set NA values to NA
  values(r3)[!is.null(r3)] = NA
  #values(r1)[values(r1) <= -0.05] = NA
  values(r1)[values(r1) > 50] = NA
  #values(r2)[values(r2) <= -0.05] = NA
  values(r2)[values(r2) > 50] = NA
  
  #5.	Other loss (do not overwrite any of above) (apply before the above)
  #IF none of above, AND T4 - T1 < -1.5, Then pixel = T4 - T1, code 50
  code50 <- values(r2)-values(r1) < -1.5
  values(r3)[code50] <- 50
  
  #6.	Other Gain (do not overwrite any of above) (apply before the above)
  #IF none of above, THEN T4 - T1 > 1.5, THEN pixel =T4 -T1, code 60
  code60 <- values(r2)-values(r1) > 1.5
  values(r3)[code60] <- 60
  
  #7.	Persistence (do not overwrite any of above) (apply before the above)
  #IF T4-T1 between -1.5 and 1.5, code = 0 
  code0 <- values(r2)-values(r1) >= -1.5 & values(r2)-values(r1) <= 1.5
  values(r3)[code0] <- 0
  
  #1.	Big tree losses
  #T1 > 5m AND T4 < 0.5*T1, THEN pixel = T4-T1, code 10 (10 years of losses)
  code10 <- values(r1)>5 & values(r2)<0.5*values(r1)
  values(r3)[code10] <- 10
  
  #2.	Shrub encroachment
  #T1 < 3m, AND T4-T1 > +1.5m, And T4 <5m, THEN pixel = T4-T1, code 20
  code20 <- values(r1)<3 & values(r2)<5 & values(r2)-values(r1)>1.5
  values(r3)[code20] <- 20
  
  #4.	Shrub loss 
  #T1 >1.5m AND < 3m, AND T4-T1 <-1.5m, THEN T4-T1, code 40
  code40 <- values(r1)>1.5 & values(r1)<3 & values(r2)-values(r1)< -1.5
  values(r3)[code40] <- 40
  
  #Output
  writeRaster(r3,file.path(out,paste0(studysite,change_ext)),options=c('TFW=YES'),overwrite=TRUE)
  r4 <- r2-r1
  writeRaster(r4,file.path(out,paste0(studysite,diff_ext)),options=c('TFW=YES'),overwrite=TRUE)
}

changemap_stack <- function(date1,date2,studysite){
  out <- "E:\\ChangMap\\CHM\\CHM\\CHM_result"
  workspace <- "E:\\ChangMap\\CHM\\CHM\\CHM_processed\\subset"
  studysite1 <- paste0(studysite,date1,".tif")
  studysite2 <- paste0(studysite,date2,".tif")
  change_ext <- paste0("_Change_",date2,"_",date1,".tif")
  diff_ext <- paste0("_Diff_",date2,"_",date1,".tif")
  
  CHM1<- file.path(workspace,studysite1)
  CHM2<- file.path(workspace,studysite2)
  
  r1 <- raster(CHM1)
  r2 <- raster(CHM2)
  r3 <- raster(CHM1)
  #set NA values to NA
  values(r3)[!is.null(r3)] = NA
  #values(r1)[values(r1) <= -0.05] = NA
  values(r1)[values(r1) > 50] = NA
  #values(r2)[values(r2) <= -0.05] = NA
  values(r2)[values(r2) > 50] = NA
  
  #5.	Other loss (do not overwrite any of above) (apply before the above)
  #IF none of above, AND T4 - T1 < -1.5, Then pixel = T4 - T1, code 50
  code50 <- values(r2)-values(r1) < -1.5
  values(r3)[code50] <- 50
  
  #6.	Other Gain (do not overwrite any of above) (apply before the above)
  #IF none of above, THEN T4 - T1 > 1.5, THEN pixel =T4 -T1, code 60
  code60 <- values(r2)-values(r1) > 1.5
  values(r3)[code60] <- 60
  
  #7.	Persistence (do not overwrite any of above) (apply before the above)
  #IF T4-T1 between -1.5 and 1.5, code = 0 
  code0 <- values(r2)-values(r1) >= -1.5 & values(r2)-values(r1) <= 1.5
  values(r3)[code0] <- 0
  
  #1.	Big tree losses
  #T1 > 5m AND T4 < 0.5*T1, THEN pixel = T4-T1, code 10 (10 years of losses)
  code10 <- values(r1)>5 & values(r2)<0.5*values(r1)
  values(r3)[code10] <- 10
  
  #2.	Shrub encroachment
  #T1 < 3m, AND T4-T1 > +1.5m, And T4 <5m, THEN pixel = T4-T1, code 20
  code20 <- values(r1)<3 & values(r2)<5 & values(r2)-values(r1)>1.5
  values(r3)[code20] <- 20
  
  #4.	Shrub loss 
  #T1 >1.5m AND < 3m, AND T4-T1 <-1.5m, THEN T4-T1, code 40
  code40 <- values(r1)>1.5 & values(r1)<3 & values(r2)-values(r1)< -1.5
  values(r3)[code40] <- 40
  
  m1 <- r3
  m1[m1 != 0] <- NA
  s0 <- m1
  r4 <- r2-r1
  r4[is.na(s0[])] <- NA
  d0 <- r4
  
  m1 <- r3
  m1[m1 != 10] <- NA
  s10 <- m1
  r4 <- r2-r1
  r4[is.na(s10[])] <- NA
  d10 <- r4
  
  m1 <- r3
  m1[m1 != 20] <- NA
  s20 <- m1
  r4 <- r2-r1
  r4[is.na(s20[])] <- NA
  d20 <- r4
  
  m1 <- r3
  m1[m1 != 40] <- NA
  s40 <- m1
  r4 <- r2-r1
  r4[is.na(s40[])] <- NA
  d40 <- r4
  
  m1 <- r3
  m1[m1 != 50] <- NA
  s50 <- m1
  r4 <- r2-r1
  r4[is.na(s50[])] <- NA
  d50 <- r4
  
  m1 <- r3
  m1[m1 != 60] <- NA
  s60 <- m1
  r4 <- r2-r1
  r4[is.na(s60[])] <- NA
  d60 <- r4
  
  #Output
  change <- stack(s0,s10,s20,s40,s50,s60)
  diff <- stack(d0,d10,d20,d40,d50,d60)
  writeRaster(change,file.path(out,paste0(studysite,change_ext)),options=c('TFW=YES'),overwrite=TRUE)
  writeRaster(diff,file.path(out,paste0(studysite,diff_ext)),options=c('TFW=YES'),overwrite=TRUE)
  
}

changemap_stack(2008,2018, Justicia)


#individual images
filedir <- "E:\\ChangMap\\CHM\\DB_csv\\Welverdiendt_ALOS_2018_HV_w_result.csv"
Data = read.csv(filedir,header=T)
Data <- dplyr::select(Data,-c("OBJECTID","gridcode","length","area",
                              "C_c","C_c_mean2","C_c_mean3","C_c_max2",
                              "C_c_max3","C_c_sum2","C_c_sum3"))
Data <- dplyr::select(Data,-c("FID","gridcode","length","area",
                              "C_c","C_c_mean2","C_c_mean3","C_c_max2",
                              "C_c_max3","C_c_sum2","C_c_sum3"))
##Data <- na.omit(Data)
#r2_all
reg1 <- lm(CA_o ~ C_s, data = Data)
summary(reg1)


M <- cor(Data)
ggcorrplot(M, hc.order = TRUE, type = "lower",
           lab = TRUE)

#r2_max
reg1 <- lm(CA_o_mean3 ~ C_s_mean3, data = Data)
summary(reg1)

reg1 <- lm(CA_o ~ C_s_mean3, data = Data)
summary(reg1)

reg1 <- lm(CA_o_sum3 ~ C_s_sum3, data = Data)
summary(reg1)


#2nd polynomial regression
Data$CA_o_mean3 <- Data$CA_o_mean3/1000000
ggplot(Data, aes(x=CA_o_mean3, y=C_s_mean3))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme(text=element_text(size=20)) +
  stat_poly_eq(formula = y ~poly(x,2), 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  geom_smooth(
    method="lm",
    formula = 'y ~poly(x,2)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")





#difference map changes
filedir <- "E:\\ChangMap\\CHM\\DB_csv\\Welverdiendt_ALOS_2018_2010_HV_w_result.csv"
Data = read.csv(filedir,header=T)
##Data <- na.omit(Data)

Data <- select(Data,-c("FID","gridcode","length","area"))
Data <- Data[, -grep("c", colnames(Data))]
M <- Data %>% 
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1)
write.csv(M,file="C:\\Users\\Shawn\\Desktop\\table.csv", row.names = FALSE)

#r2_max
filedir <- "E:\\ChangMap\\CHM\\DB_csv\\Justicia_ALOS_2018_2008_HV7_w_result.csv"
Data = read.csv(filedir,header=T)
##Data <- na.omit(Data)
reg1 <- lm(CA_o_mean9 ~ C_s_sum9, data = Data)
reg1 <- lm(CA_o_mean5 ~ poly(C_s_sum5,2), data = Data)
reg1 <- glm(as.factor(Data$CA_o_mean9) ~ C_s_sum9,family=binomial(link='logit'),data=Data)
summary(reg1)

#geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  
#2nd polynomial regression
Data$CA_o_mean9 <- Data$CA_o_mean9/1000000
ggplot(Data, aes(x=C_s_sum9, y=as.factor(CA_o_mean9)))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme(text=element_text(size=20)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme(plot.title = element_text(hjust = 0.5))


#subset
filedir <- "E:\\ChangMap\\CHM\\DB_csv\\Agincourt_ALOS_2018_HV_w_result.csv"
Data = read.csv(filedir,header=T)
##Data <- na.omit(Data)
Data1 <-Data
Data1 <- Data[Data$C10_c_sum3 > 120 &
               Data$C20_c_sum3 <= 30 & 
               Data$C40_c_sum3 <= 30 & 
               Data$C50_c_sum3 <= 30 & 
               Data$C60_c_sum3 <= 30,]
Data1 <- Data1[, grep(paste(c("CA", "C_s"),collapse="|"), colnames(Data1))]
M <- cor(Data1)
ggcorrplot(M, hc.order = TRUE, type = "lower",
           lab = TRUE)
reg1 <- lm(CA_o_mean3 ~ C_s_sum3, data = Data)
summary(reg1)



#cover
library(raster)
out <- "E:/ChangMap/CHM/CHM/CHM_cover"

Locdir <- "E:/ChangMap/CHM/CHM/CHM_result/allin"
chms <- list.files(path = Locdir, full.names = TRUE, pattern = "^.*Diff*.*.tif$")

Locdir <- "E:/ChangMap/CHM/CHM/CHM_processed/subset"
chms <- list.files(path = Locdir, full.names = TRUE, pattern = "tif$")


for (chm in chms){
  print(chm)
  r <- raster(chm)
  r[r < 1.5] <- NA
  writeRaster(r,file.path(out,basename(chm)),options=c('TFW=YES'),overwrite=TRUE)
}




filedir <- "C:/Users/Shawn/Desktop/meeting.xlsx"
S <-"Agincourt2007"
Data = read_excel(filedir, sheet = S)
##Data <- na.omit(Data)
#Data <- Data[Data$ALOS > -25,]
Data$ALOS <- Data$ALOS/1000000
Data$Cover <- Data$Cover/max(Data$Cover)
reg1 <- lm(Cover ~ poly(ALOS,2), data = Data)
summary(reg1)

ggplot(Data, aes(x=ALOS, y=Cover))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme(text=element_text(size=20)) +
  stat_poly_eq(formula = y ~poly(x,2), 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  geom_smooth(
    method="lm",
    formula = 'y ~poly(x,2)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+
  labs(title=S,
       x ="Sum of ALOS pixel values (3*3)", 
       y = "Cover of LiDAR CHM (3*3)") +
  theme(plot.title = element_text(hjust = 0.5))
  


#merge
filedir1 <- "E:\\ChangMap\\CHM\\DB_csv\\Ireagh_ALOS_2018_HV_w_result.csv"
filedir2 <- "E:\\ChangMap\\CHM\\DB_csv\\Agincourt_ALOS_2018_HV_w_result.csv"
filedir3 <- "E:\\ChangMap\\CHM\\DB_csv\\Justicia_ALOS_2018_HV_w_result.csv"

filedir4 <- "E:\\ChangMap\\CHM\\DB_csv\\WelAndover_ALOS_2018_HV_w_result.csv"
filedir5 <- "E:\\ChangMap\\CHM\\DB_csv\\Welverdiendt_ALOS_2018_HV_w_result.csv"
filedir6 <- "E:\\ChangMap\\CHM\\DB_csv\\Justicia_ALOS_2018_HV_w_result.csv"

Data1 = read.csv(filedir1,header=T)
Data2 = read.csv(filedir2,header=T)
Data3 = read.csv(filedir3,header=T)

Data4 = read.csv(filedir4,header=T)
Data5 = read.csv(filedir5,header=T)
Data6 = read.csv(filedir6,header=T)

Data <- rbind(Data1,Data2,Data3)

Data <- rbind(Data4,Data5,Data6)

Data <- rbind(Data1,Data2,Data3,Data4,Data5,Data6)


#Data <- na.omit(Data)
Data$CA_o_mean3 <- Data$CA_o_mean3/1000000
reg1 <- lm(CA_o_mean3 ~ poly(C_s_sum3,2), data = Data)
summary(reg1)

reg1 <- lm(CA_o_mean3 ~ C_s_sum3, data = Data)
summary(reg1)

ggplot(Data, aes(x=CA_o_mean3, y=C_s_mean3))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme(text=element_text(size=20)) +
  stat_poly_eq(formula = y ~poly(x,2), 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  geom_smooth(
    method="lm",
    formula = 'y ~poly(x,2)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+ 
  labs(title="Agincourt+Ireagh+Justicia 2007",
       x ="Sum of ALOS pixel values (3*3)", 
       y = "LiDAR CHM change volume (3*3)") +
  theme(plot.title = element_text(hjust = 0.5))

#test 2010

filedir <- "C:/Users/Shawn/Desktop/2010.xlsx"
S <-"Justicia2017"
Data = read_excel(filedir, sheet = S)
#Data <- na.omit(Data)
#Data <- Data[Data$ALOS > -25,]
Data$ALOS <- Data$ALOS/1000000
reg1 <- lm(ALOS ~ poly(LIDAR,2), data = Data)
summary(reg1)



#poly for individuals 
filedir <- "E:/ChangMap/CHM/DB_csv/Justicia_ALOS_2018_HV7_w_result.csv"
Data = read.csv(filedir,header=T)
#Data <- na.omit(Data)
#Data <- Data[Data$ALOS > -25,]
Data$CA_o_mean3 <- Data$CA_o_mean3/1000000
reg1 <- lm(CA_o_mean3 ~ poly(C_s_sum3,2), data = Data)
summary(reg1)

ggplot(Data, aes(x=CA_o_mean3, y=C_s_sum3))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme(text=element_text(size=20)) +
  stat_poly_eq(formula = y ~poly(x,2), 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  geom_smooth(
    method="lm",
    formula = 'y ~poly(x,2)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+
  labs(title="Justicia2017",
       x ="Sum of ALOS pixel values (3*3)", 
       y = "Cover of LiDAR CHM (3*3)") +
  theme(plot.title = element_text(hjust = 0.5))




#02092021

filedir <- "E:\\ChangMap\\CHM\\DB_csv\\Ireagh_ALOS_2018_HV_w_result.csv"
Data = read.csv(filedir,header=T)
#Data <- na.omit(Data)
Data$CA_o_mean9 <- Data$CA_o_mean9/1000000

reg1 <- lm(CA_o_mean9 ~  C_s_sum9, data = Data)
reg1 <- lm(CA_o_mean9 ~ poly(C_s_sum9,2), data = Data)

summary(reg1)

#2nd polynomial regression
ggplot(Data, aes(x=C_s_sum9, y=CA_o_mean9))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme(text=element_text(size=20)) +
  stat_poly_eq(formula = y ~ x , 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +  
  geom_smooth(
    method="lm",
    formula = 'y ~x', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+
  theme(plot.title = element_text(hjust = 0.5))

#histogram
Data1 <- Data[Data$C10_c_sum3 > 120 &
                Data$C20_c_sum3 <= 30 & 
                Data$C40_c_sum3 <= 30 & 
                Data$C50_c_sum3 <= 30 & 
                Data$C60_c_sum3 <= 30,]





filedir <- "C:/Users/Shawn/Desktop/meeting.xlsx"
S1 <-"Justicia2007"
S2<-"Justicia2017"
Data1 = read_excel(filedir, sheet = S1)
Data2 = read_excel(filedir, sheet = S2)

#Data <- na.omit(Data)
#Data <- Data[Data$ALOS > -25,]
Data$ALOS <- Data$ALOS/1000000
Data$Cover <- Data$Cover/max(Data$Cover)
reg1 <- lm(Cover ~ poly(ALOS,2), data = Data)
summary(reg1)

ggplot(Data, aes(x=ALOS, y=Cover))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme(text=element_text(size=20)) +
  stat_poly_eq(formula = y ~poly(x,2), 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  geom_smooth(
    method="lm",
    formula = 'y ~poly(x,2)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+
  labs(title=S,
       x ="Sum of ALOS pixel values (3*3)", 
       y = "Cover of LiDAR CHM (3*3)") +
  theme(plot.title = element_text(hjust = 0.5))

#cover
library(raster)
out <- "E:/ChangMap/CHM/CHM/CHM_cover"
Locdir <- "E:/ChangMap/CHM/CHM/CHM_processed/subset"
chms <- list.files(path = Locdir, full.names = TRUE, pattern = "*.tif$")
for (chm in chms){
  print(chm)
  r <- raster(chm)
  r[r < 1.5] <- 0
  r[r >= 1.5] <- 1
  writeRaster(r,file.path(out,basename(chm)),options=c('TFW=YES'),overwrite=TRUE)
}




#02152021 
#stats table -- individual cases
Locdir <- "E:\\ChangMap\\CHM\\DB_csv"
pattern = '.csv'
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
listt <- list("testcase","linear3 ","linear5 ","linear9 ",
              "poly3","poly5", "poly9","log3","log5","log9")
df <- data.frame(listt)
colnames(df) <- listt
for (i in files){
  list1 <- list()
  Data = read.csv(i,header=T)
  #Data <- na.omit(Data)
  
  Data$CA_o_mean3 <- Data$CA_o_mean3/1000000
  Data$CA_o_mean5 <- Data$CA_o_mean5/1000000
  Data$CA_o_mean9 <- Data$CA_o_mean9/1000000
  if (grepl("cover", i, fixed=TRUE) == FALSE){
    print("Volume")
    print(i)
    Data <- Data[Data$C_s_sum3 > 0,]
    Data <- Data[Data$C_s_sum5 > 0,]
    Data <- Data[Data$C_s_sum9 > 0,]
    reg1 <- lm(CA_o_mean3  ~ C_s_sum3, data = Data)
    reg2 <- lm(CA_o_mean5 ~  C_s_sum5, data = Data)
    reg3 <- lm(CA_o_mean9 ~  C_s_sum9, data = Data)
    reg4 <- lm(CA_o_mean3 ~ poly(C_s_sum3,2), data = Data)
    reg5 <- lm(CA_o_mean5 ~ poly(C_s_sum5,2), data = Data)
    reg6 <- lm(CA_o_mean9 ~ poly(C_s_sum9,2), data = Data)
    reg7 <- lm(CA_o_mean3 ~ log(C_s_sum3), data = Data)
    reg8 <- lm(CA_o_mean5 ~ log(C_s_sum5), data = Data)
    reg9 <- lm(CA_o_mean9 ~ log(C_s_sum9), data = Data)
  }else{
    print("Cover")
    print(i)
    Data <- Data[Data$C_c_sum3 > 0,]
    Data <- Data[Data$C_c_sum5 > 0,]
    Data <- Data[Data$C_c_sum9 > 0,]
    reg1 <- lm(CA_o_mean3  ~ C_c_sum3, data = Data)
    reg2 <- lm(CA_o_mean5 ~  C_c_sum5, data = Data)
    reg3 <- lm(CA_o_mean9 ~  C_c_sum9, data = Data)
    reg4 <- lm(CA_o_mean3 ~ poly(C_c_sum3,2), data = Data)
    reg5 <- lm(CA_o_mean5 ~ poly(C_c_sum5,2), data = Data)
    reg6 <- lm(CA_o_mean9 ~ poly(C_c_sum9,2), data = Data)
    reg7 <- lm(CA_o_mean3 ~ log(C_c_sum3), data = Data)
    reg8 <- lm(CA_o_mean5 ~ log(C_c_sum5), data = Data)
    reg9 <- lm(CA_o_mean9 ~ log(C_c_sum9), data = Data)
  }

  list1[[1]] <- basename(i)
  list1[[2]] <- round(summary(reg1)$adj.r.squared,3)
  list1[[3]] <- round(summary(reg2)$adj.r.squared,3)
  list1[[4]] <- round(summary(reg3)$adj.r.squared,3)
  list1[[5]] <- round(summary(reg4)$adj.r.squared,3)
  list1[[6]] <- round(summary(reg5)$adj.r.squared,3)
  list1[[7]] <- round(summary(reg6)$adj.r.squared,3)
  list1[[8]] <- round(summary(reg7)$adj.r.squared,3)
  list1[[9]] <- round(summary(reg8)$adj.r.squared,3)
  list1[[10]] <- round(summary(reg9)$adj.r.squared,3)
  df1 <- data.frame(list1)
  colnames(df1) <- listt
  df <- rbind(df,df1)
}

write.table(df,file=file.path(Locdir,"Stats.csv"), quote=F,sep=" ",row.names=F)


#stats table -- change volume/cover
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\change"
pattern = '.csv'
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
listt <- list("testcase","linear3","linear5","linear9 ","poly3","poly5","poly9")
df <- data.frame(listt)
colnames(df) <- listt
for (i in files){
  list1 <- list()
  Data = read.csv(i,header=T)
  #Data <- na.omit(Data)
  
  Data$CA_o_mean3 <- Data$CA_o_mean3/1000000
  Data$CA_o_mean5 <- Data$CA_o_mean5/1000000
  Data$CA_o_mean9 <- Data$CA_o_mean9/1000000
  if (grepl("cover", i, fixed=TRUE) == FALSE){
    print("Volume")
    print(i)
    reg1 <- lm(CA_o_mean3  ~ C_s_sum3, data = Data)
    reg2 <- lm(CA_o_mean5 ~  C_s_sum5, data = Data)
    reg3 <- lm(CA_o_mean9 ~  C_s_sum9, data = Data)
    reg4 <- lm(CA_o_mean3 ~ poly(C_s_sum3,2), data = Data)
    reg5 <- lm(CA_o_mean5 ~ poly(C_s_sum5,2), data = Data)
    reg6 <- lm(CA_o_mean9 ~ poly(C_s_sum9,2), data = Data)
  }else{
    print("Cover")
    print(i)
    reg1 <- lm(CA_o_mean3  ~ I(cover3/2025), data = Data)
    reg2 <- lm(CA_o_mean5 ~  I(cover5/5625), data = Data)
    reg3 <- lm(CA_o_mean9 ~  I(cover9/18225), data = Data)
    reg4 <- lm(CA_o_mean3 ~ poly(I(cover3/2025),2), data = Data)
    reg5 <- lm(CA_o_mean5 ~ poly(I(cover5/5625),2), data = Data)
    reg6 <- lm(CA_o_mean9 ~ poly(I(cover9/18225),2), data = Data)
  }
  
  list1[[1]] <- basename(i)
  list1[[2]] <- round(summary(reg1)$adj.r.squared,3)
  list1[[3]] <- round(summary(reg2)$adj.r.squared,3)
  list1[[4]] <- round(summary(reg3)$adj.r.squared,3)
  list1[[5]] <- round(summary(reg4)$adj.r.squared,3)
  list1[[6]] <- round(summary(reg5)$adj.r.squared,3)
  list1[[7]] <- round(summary(reg6)$adj.r.squared,3)
  df1 <- data.frame(list1)
  colnames(df1) <- listt
  df <- rbind(df,df1)
}

write.table(df,file=file.path(Locdir,"Stats.csv"), quote=F,sep=" ",row.names=F)


#all site cases
filedir <- "E:\\ChangMap\\CHM\\DB_csv\\Justicia_ALOS_2018_HV7_w_result.csv"
Data = read.csv(filedir,header=T) 
#Data <- na.omit(Data)
Data$CA_o_mean9 <- Data$CA_o_mean9/1000000
Data <- Data[Data$C_s_sum3 > 0,]

reg1 <- lm(CA_o_mean9 ~ C_s_sum9 , data = Data)
reg2 <- lm(CA_o_mean9 ~ poly(C_s_sum9,2), data = Data)
reg3 <- lm(CA_o_mean9 ~ log(C_s_sum9), data = Data)
summary(reg1)
summary(reg2)
summary(reg3)


pattern = 'Welverdiendt_ALOS_2018_HV7_w_result.csv'
#scatterplot -- volume
Locdir <- "E:\\ChangMap\\CHM\\DB_csv"
Out <- "E:\\ChangMap\\CHM\\Image\\Individual_volume_scatterplot"
pattern = 'result.csv'
window <- "3"
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  print(i)
  out <- file.path(Out,paste0(file_path_sans_ext(basename(i)),window,".jpg"))
  if (!(file.exists(out))){
    Data = read.csv(i,header=T)
    if (window == "9"){
      Data$CA_o_mean9 <- Data$CA_o_mean9
      Data$C_s_sum9 <- Data$C_s_sum9
    }else if(window == "5"){
      Data$CA_o_mean9 <- Data$CA_o_mean5
      Data$C_s_sum9 <- Data$C_s_sum5
    }else if (window == "3"){
      Data$CA_o_mean9 <- Data$CA_o_mean3
      Data$C_s_sum9 <- Data$C_s_sum3
    }
    
    #Data <- na.omit(Data)
    Data$CA_o_mean9 <- Data$CA_o_mean9/1000000
    Data <- Data[Data$C_s_sum9 > 0,]
    
    reg1 <- lm(CA_o_mean9 ~ C_s_sum9 , data = Data)
    reg2 <- lm(CA_o_mean9 ~ poly(C_s_sum9,2), data = Data)
    reg3 <- lm(CA_o_mean9 ~ log(C_s_sum9), data = Data)
    
    ymax <- max(Data$CA_o_mean9) + 3
    year <- strsplit(basename(i),"_")[[1]][3]
    if (strsplit(basename(i),"_")[[1]][4] == "HV7"){
      year <- paste0(substr(strsplit(basename(i),"_")[[1]][3],1,3),
                     substr(strsplit(basename(i),"_")[[1]][4],3,3))
    }
    
    ggplot(Data, aes(x=C_s_sum9, y=CA_o_mean9))+ 
      ylim(-30, 0)+
      geom_pointdensity()+
      scale_color_viridis(direction = -1)+
      annotate("text", x=0, y=ymax, hjust = 0,color="purple",size = 10,
               label= paste(" Linear model R2: ",round(summary(reg1)$adj.r.squared,3))) + 
      annotate("text", x=0, y=ymax-1, hjust = 0,color="red",size = 10,
               label= paste(" Polynomial model R2: ",round(summary(reg2)$adj.r.squared,3))) + 
      annotate("text", x=0, y=ymax-2, hjust = 0,color="blue",size = 10,
               label= paste(" Logarithmic model R2: ",round(summary(reg3)$adj.r.squared,3))) + 
      geom_smooth(
        method="lm",
        formula = 'y ~ x', 
        se= F, 
        size = 1.5, 
        linetype = "solid",
        colour = "purple")+
      geom_smooth(
        method="lm",
        formula = 'y ~ poly(x,2)', 
        se= F, 
        size = 1.5, 
        linetype = "solid",
        colour = "red")+
      geom_smooth(
        method="lm",
        formula = 'y ~ log2(x)', 
        se= F, 
        size = 1.5, 
        linetype = "solid",
        colour = "blue")+
      labs(title=paste0(strsplit(basename(i),"_")[[1]][1]," ",year),
           x = paste0("LiDAR CHM height volume (sum, ",window,")"),
           y = paste0("ALOS backscatter (average, ",window,")")) +
      theme(text=element_text(size=20)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(out, height=15, width=15, dpi=600)
  }
  else{
    print(paste0("File exist...",out))
  }
}


pattern = 'merged_ALOS_2008_HV7_w_result_cover.csv'
#scatterplot -- cover
Locdir <- "E:\\ChangMap\\CHM\\DB_csv"
Out <- "E:\\ChangMap\\CHM\\Image\\Individual_cover_scatterplot"
pattern = 'result_cover.csv'
window <- "9"
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  print(i)
  out <- file.path(Out,paste0(file_path_sans_ext(basename(i)),window,".jpg"))
  if (!(file.exists(out))){
    Data = read.csv(i,header=T)
    if (window == "9"){
      Data$CA_o_mean9 <- Data$CA_o_mean9
      Data$C_c_sum9 <- Data$C_c_sum9
      Data$C_c_sum9 <- Data$C_c_sum9/18225
    }else if(window == "5"){
      Data$CA_o_mean9 <- Data$CA_o_mean5
      Data$C_c_sum9 <- Data$C_c_sum5
      Data$C_c_sum9 <- Data$C_c_sum9/5625
    }else if (window == "3"){
      Data$CA_o_mean9 <- Data$CA_o_mean3
      Data$C_c_sum9 <- Data$C_c_sum3
      Data$C_c_sum9 <- Data$C_c_sum9/2025
    }
    
    #Data <- na.omit(Data)
    Data$CA_o_mean9 <- Data$CA_o_mean9/1000000
    Data <- Data[Data$C_c_sum9 > 0,]
    
    reg1 <- lm(CA_o_mean9 ~ C_c_sum9 , data = Data)
    reg2 <- lm(CA_o_mean9 ~ poly(C_c_sum9,2), data = Data)
    reg3 <- lm(CA_o_mean9 ~ log(C_c_sum9), data = Data)
    
    ymax <- max(Data$CA_o_mean9) + 3
    year <- strsplit(basename(i),"_")[[1]][3]
    if (strsplit(basename(i),"_")[[1]][4] == "HV7"){
      year <- paste0(substr(strsplit(basename(i),"_")[[1]][3],1,3),
                     substr(strsplit(basename(i),"_")[[1]][4],3,3))
    }
    
    ggplot(Data, aes(x=C_c_sum9, y=CA_o_mean9))+ 
      ylim(-30, 0)+
      geom_pointdensity()+
      scale_color_viridis(direction = -1)+
      annotate("text", x=0, y=ymax, hjust = 0,color="purple",size = 10,
               label= paste(" Linear model R2: ",round(summary(reg1)$adj.r.squared,3))) + 
      annotate("text", x=0, y=ymax-1, hjust = 0,color="red",size = 10,
               label= paste(" Polynomial model R2: ",round(summary(reg2)$adj.r.squared,3))) + 
      annotate("text", x=0, y=ymax-2, hjust = 0,color="blue",size = 10,
               label= paste(" Logarithmic model R2: ",round(summary(reg3)$adj.r.squared,3))) + 
      geom_smooth(
        method="lm",
        formula = 'y ~ x', 
        se= F, 
        size = 1.5, 
        linetype = "solid",
        colour = "purple")+
      geom_smooth(
        method="lm",
        formula = 'y ~ poly(x,2)', 
        se= F, 
        size = 1.5, 
        linetype = "solid",
        colour = "red")+
      geom_smooth(
        method="lm",
        formula = 'y ~ log2(x)', 
        se= F, 
        size = 1.5, 
        linetype = "solid",
        colour = "blue")+
      labs(title=paste0(strsplit(basename(i),"_")[[1]][1]," ",year),
           x = paste0("LiDAR CHM cover (sum, ",window,")"),
           y = paste0("ALOS backscatter (average, ",window,")")) +
      theme(text=element_text(size=20)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(out, height=15, width=15, dpi=600)
  }
  else{
    print(paste0("File exist...",out))
  }
}


#scatterplot --  volume change
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\change"
Out <- "E:\\ChangMap\\CHM\\Image\\Change_volume_scatterplot"
pattern = 'result.csv'
window <- "9"
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  print(i)
  out <- file.path(Out,paste0(file_path_sans_ext(basename(i)),window,".jpg"))
  if (!(file.exists(out))){
    Data = read.csv(i,header=T)
    if (window == "9"){
      Data$CA_o_mean9 <- Data$CA_o_mean9/1000000
      Data$C_s_sum9 <- Data$C_s_sum9
    }else if(window == "5"){
      Data$CA_o_mean9 <- Data$CA_o_mean5/1000000
      Data$C_s_sum9 <- Data$C_s_sum5
    }else if (window == "3"){
      Data$CA_o_mean9 <- Data$CA_o_mean3/1000000
      Data$C_s_sum9 <- Data$C_s_sum3
    }
    
    #Data <- na.omit(Data)

    reg1 <- lm(CA_o_mean9 ~ C_s_sum9 , data = Data)
    reg2 <- lm(CA_o_mean9 ~ poly(C_s_sum9,2), data = Data)
    
    xmin <- min(Data$C_s_sum9)
    ymax <- max(Data$CA_o_mean9) + 3
    year <- paste0(strsplit(basename(i),"_")[[1]][3],"-",strsplit(basename(i),"_")[[1]][4])
    if (strsplit(basename(i),"_")[[1]][5] == "HV7"){
      year <- paste0(substr(strsplit(basename(i),"_")[[1]][3],1,3),
                     substr(strsplit(basename(i),"_")[[1]][5],3,3),"-",
                     substr(strsplit(basename(i),"_")[[1]][4],1,3),
                     substr(strsplit(basename(i),"_")[[1]][5],3,3))
    }
    
    ggplot(Data, aes(x=C_s_sum9, y=CA_o_mean9))+ 
      geom_pointdensity()+
      scale_color_viridis(direction = -1)+
      annotate("text", x=xmin, y=ymax, hjust = 0,color="purple",size = 10,
               label= paste(" Linear model R2: ",round(summary(reg1)$adj.r.squared,3))) + 
      annotate("text", x=xmin, y=ymax-1, hjust = 0,color="red",size = 10,
               label= paste(" Polynomial model R2: ",round(summary(reg2)$adj.r.squared,3))) + 
      geom_smooth(
        method="lm",
        formula = 'y ~ x', 
        se= F, 
        size = 1.5, 
        linetype = "solid",
        colour = "purple")+
      geom_smooth(
        method="lm",
        formula = 'y ~ poly(x,2)', 
        se= F, 
        size = 1.5, 
        linetype = "solid",
        colour = "red")+
      labs(title=paste0(strsplit(basename(i),"_")[[1]][1]," ",year),
           x = paste0("LiDAR CHM volume change(sum, ",window,")"),
           y = paste0("ALOS backscatter change (average, ",window,")")) +
      theme(text=element_text(size=20)) +
      theme(plot.title = element_text(hjust = 0.5))

    ggsave(out, height=15, width=15, dpi=600)
  }
  else{
    print(paste0("File exist...",out))
  }
}


#scatterplot -- cover change
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\change"
Out <- "E:\\ChangMap\\CHM\\Image\\Change_cover_scatterplot"
pattern = 'result_cover.csv'
window <- "9"
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  print(i)
  out <- file.path(Out,paste0(file_path_sans_ext(basename(i)),window,".jpg"))
  if (!(file.exists(out))){
    Data = read.csv(i,header=T)
    if (window == "9"){
      Data$CA_o_mean9 <- Data$CA_o_mean9/1000000
      Data$C_c_sum9 <- Data$cover9/18225
    }else if(window == "5"){
      Data$CA_o_mean9 <- Data$CA_o_mean5/1000000
      Data$C_c_sum9 <- Data$cover5/5625
    }else if (window == "3"){
      Data$CA_o_mean9 <- Data$CA_o_mean3/1000000
      Data$C_c_sum9 <- Data$cover3/2025

    }
    #Data <- na.omit(Data)

    reg1 <- lm(CA_o_mean9 ~ C_c_sum9 , data = Data)
    reg2 <- lm(CA_o_mean9 ~ poly(C_c_sum9,2), data = Data)

    

    ymax <- max(Data$CA_o_mean9) + 3
    year <- paste0(strsplit(basename(i),"_")[[1]][3],"-",strsplit(basename(i),"_")[[1]][4])
    if (strsplit(basename(i),"_")[[1]][5] == "HV7"){
      year <- paste0(substr(strsplit(basename(i),"_")[[1]][3],1,3),
                     substr(strsplit(basename(i),"_")[[1]][5],3,3),"-",
                     substr(strsplit(basename(i),"_")[[1]][4],1,3),
                     substr(strsplit(basename(i),"_")[[1]][5],3,3))
    }
    
    ggplot(Data, aes(x=C_c_sum9, y=CA_o_mean9))+ 
      geom_pointdensity()+
      scale_color_viridis(direction = -1)+
      annotate("text", x=0, y=ymax, hjust = 0,color="purple",size = 10,
               label= paste(" Linear model R2: ",round(summary(reg1)$adj.r.squared,3))) + 
      annotate("text", x=0, y=ymax-1, hjust = 0,color="red",size = 10,
               label= paste(" Polynomial model R2: ",round(summary(reg2)$adj.r.squared,3))) + 
      geom_smooth(
        method="lm",
        formula = 'y ~ x', 
        se= F, 
        size = 1.5, 
        linetype = "solid",
        colour = "purple")+
      geom_smooth(
        method="lm",
        formula = 'y ~ poly(x,2)', 
        se= F, 
        size = 1.5, 
        linetype = "solid",
        colour = "red")+
      labs(title=paste0(strsplit(basename(i),"_")[[1]][1]," ",year),
           x = paste0("LiDAR CHM cover change (sum, ",window,")"),
           y = paste0("ALOS backscatter change (average, ",window,")")) +
      theme(text=element_text(size=20)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(out, height=15, width=15, dpi=600)
  }
  else{
    print(paste0("File exist...",out))
  }
}



#boxplot -- volume
Locdir <- "E:\\ChangMap\\CHM\\DB_csv"
Out <- "E:\\ChangMap\\CHM\\Image\\Individual_volume_boxplot"
pattern = 'result.csv'
window <- "3"
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  print(i)
  out <- file.path(Out,paste0(file_path_sans_ext(basename(i)),window,".jpg"))
  if (!(file.exists(out))){
    Data = read.csv(i,header=T)
    if (window == "9"){
      Data$CA_o_mean9 <- Data$CA_o_mean9
      Data$C_s_sum9 <- Data$C_s_sum9
    }else if(window == "5"){
      Data$CA_o_mean9 <- Data$CA_o_mean5
      Data$C_s_sum9 <- Data$C_s_sum5
    }else if (window == "3"){
      Data$CA_o_mean9 <- Data$CA_o_mean3
      Data$C_s_sum9 <- Data$C_s_sum3
    }
    
    #Data <- na.omit(Data)
    Data$CA_o_mean9 <- Data$CA_o_mean9/1000000
    Data <- Data[Data$C_s_sum9 > 0,]
    year <- strsplit(basename(i),"_")[[1]][3]
    if (strsplit(basename(i),"_")[[1]][4] == "HV7"){
      year <- paste0(substr(strsplit(basename(i),"_")[[1]][3],1,3),
                     substr(strsplit(basename(i),"_")[[1]][4],3,3))
    }
    
    Data$group <- cut(Data$C_s_sum9, 15,dig.lab=6)
    ggplot(Data, aes(x=C_s_sum9, y=CA_o_mean9, fill=group)) + 
      geom_boxplot()+
      labs(title=paste0(strsplit(basename(i),"_")[[1]][1]," ",year),
           x = paste0("LiDAR CHM height volume (sum, ",window,")"),
           y = paste0("ALOS backscatter (average, ",window,")")) +
      theme(legend.title = element_blank())+
      theme(text=element_text(size=20))+
      theme(plot.title = element_text(hjust = 0.5))
    ggsave(out, height=15, width=15, dpi=600)
  }
    
}



#boxplot -- cover
Locdir <- "E:\\ChangMap\\CHM\\DB_csv"
Out <- "E:\\ChangMap\\CHM\\Image\\Individual_cover_boxplot"
pattern = 'result.csv'
window <- "3"
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  print(i)
  out <- file.path(Out,paste0(file_path_sans_ext(basename(i)),window,".jpg"))
  if (!(file.exists(out))){
    Data = read.csv(i,header=T)
    if (window == "9"){
      Data$CA_o_mean9 <- Data$CA_o_mean9/1000000
      Data$C_c_sum9 <- Data$C_c_sum9/18225
    }else if(window == "5"){
      Data$CA_o_mean9 <- Data$CA_o_mean5/1000000
      Data$C_c_sum9 <- Data$C_c_sum5/5625

    }else if (window == "3"){
      Data$CA_o_mean9 <- Data$CA_o_mean3/1000000
      Data$C_c_sum9 <- Data$C_c_sum3/2025
    }
    
    #Data <- na.omit(Data)
    Data <- Data[Data$C_c_sum9 > 0,]
    year <- strsplit(basename(i),"_")[[1]][3]
    if (strsplit(basename(i),"_")[[1]][4] == "HV7"){
      year <- paste0(substr(strsplit(basename(i),"_")[[1]][3],1,3),
                     substr(strsplit(basename(i),"_")[[1]][4],3,3))
    }
    
    Data$group <- cut(Data$C_c_sum9, 10,dig.lab=6)
    ggplot(Data, aes(x=C_c_sum9, y=CA_o_mean9, fill=group)) + 
      geom_boxplot()+
      labs(title=paste0(strsplit(basename(i),"_")[[1]][1]," ",year),
           x = paste0("LiDAR CHM cover (sum, ",window,")"),
           y = paste0("ALOS backscatter (average, ",window,")")) +
      theme(legend.title = element_blank())+
      theme(text=element_text(size=20))+
      theme(plot.title = element_text(hjust = 0.5))
    ggsave(out, height=15, width=15, dpi=600)
  }
  
}




#boxplot -- volume change 
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\change"
Out <- "E:\\ChangMap\\CHM\\Image\\Change_volume_boxplot"
pattern = 'result.csv'
window <- "9"
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  print(i)
  out <- file.path(Out,paste0(file_path_sans_ext(basename(i)),window,".jpg"))
  if (!(file.exists(out))){
    Data = read.csv(i,header=T)
    if (window == "9"){
      Data$CA_o_mean9 <- Data$CA_o_mean9/1000000
      Data$C_s_sum9 <- Data$C_s_sum9
    }else if(window == "5"){
      Data$CA_o_mean9 <- Data$CA_o_mean5/1000000
      Data$C_s_sum9 <- Data$C_s_sum5
    }else if (window == "3"){
      Data$CA_o_mean9 <- Data$CA_o_mean3/1000000
      Data$C_s_sum9 <- Data$C_s_sum3
    }
    
    #Data <- na.omit(Data)
    xmin <- min(Data$C_s_sum9)
    ymax <- max(Data$CA_o_mean9) + 3
    year <- paste0(strsplit(basename(i),"_")[[1]][3],"-",strsplit(basename(i),"_")[[1]][4])
    if (strsplit(basename(i),"_")[[1]][5] == "HV7"){
      year <- paste0(substr(strsplit(basename(i),"_")[[1]][3],1,3),
                     substr(strsplit(basename(i),"_")[[1]][5],3,3),"-",
                     substr(strsplit(basename(i),"_")[[1]][4],1,3),
                     substr(strsplit(basename(i),"_")[[1]][5],3,3))
    }
    
    
    Data$group <- cut(Data$C_s_sum9, 15,dig.lab=6)
    ggplot(Data, aes(x=C_s_sum9, y=CA_o_mean9, fill=group)) + 
      geom_boxplot()+
      labs(title=paste0(strsplit(basename(i),"_")[[1]][1]," ",year),
           x = paste0("LiDAR CHM volume change (sum, ",window,")"),
           y = paste0("ALOS backscatter change (average, ",window,")")) +
      theme(legend.title = element_blank())+
      theme(text=element_text(size=20))+
      theme(plot.title = element_text(hjust = 0.5))
    ggsave(out, height=15, width=15, dpi=600)
  }
  
}




#boxplot -- cover
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\change"
Out <- "E:\\ChangMap\\CHM\\Image\\Change_cover_boxplot"
pattern = 'result_cover.csv'
window <- "9"
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  print(i)
  out <- file.path(Out,paste0(file_path_sans_ext(basename(i)),window,".jpg"))
  if (!(file.exists(out))){
    Data = read.csv(i,header=T)
    if (window == "9"){
      Data$CA_o_mean9 <- Data$CA_o_mean9/1000000
      Data$C_c_sum9 <- Data$cover9/18225
    }else if(window == "5"){
      Data$CA_o_mean9 <- Data$CA_o_mean5/1000000
      Data$C_c_sum9 <- Data$cover5/5625
    }else if (window == "3"){
      Data$CA_o_mean9 <- Data$CA_o_mean3/1000000
      Data$C_c_sum9 <- Data$cover3/2025
      
    }
    #Data <- na.omit(Data)
    year <- paste0(strsplit(basename(i),"_")[[1]][3],"-",strsplit(basename(i),"_")[[1]][4])
    if (strsplit(basename(i),"_")[[1]][5] == "HV7"){
      year <- paste0(substr(strsplit(basename(i),"_")[[1]][3],1,3),
                     substr(strsplit(basename(i),"_")[[1]][5],3,3),"-",
                     substr(strsplit(basename(i),"_")[[1]][4],1,3),
                     substr(strsplit(basename(i),"_")[[1]][5],3,3))
    }
    Data$group <- cut(Data$C_c_sum9,breaks = c(-0.7,-0.6,-0.5,-0.4,-0.3,
                                               -0.2,-0.1,0,0.1,0.2,0.3,
                                               0.4,0.5,0.6,0.7)
                         ,dig.lab=1)
    ggplot(Data, aes(x=C_c_sum9, y=CA_o_mean9, fill=group)) + 
      geom_boxplot()+
      labs(title=paste0(strsplit(basename(i),"_")[[1]][1]," ",year),
           x = paste0("LiDAR CHM cover (sum, ",window,")"),
           y = paste0("ALOS backscatter (average, ",window,")")) +
      theme(legend.title = element_blank())+
      theme(text=element_text(size=20))+
      theme(plot.title = element_text(hjust = 0.5))
    ggsave(out, height=15, width=15, dpi=600)
  }
  
}



#t-test 
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\change"
Out <- "E:\\ChangMap\\CHM\\Image\\Change_cover_boxplot"
pattern = 'result_cover.csv'
window <- "3"
listt <- list("Year ","Aggregation","T-value","P-value")

df <- data.frame(listt)
colnames(df) <- listt
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  print(i)
  list1 <- list()
  Data = read.csv(i,header=T)
  if (window == "9"){
    Data$CA_o_mean9 <- Data$CA_o_mean9/1000000
    Data$C_c_sum9 <- Data$cover9/18225
  }else if(window == "5"){
    Data$CA_o_mean9 <- Data$CA_o_mean5/1000000
    Data$C_c_sum9 <- Data$cover5/5625
  }else if (window == "3"){
    Data$CA_o_mean9 <- Data$CA_o_mean3/1000000
    Data$C_c_sum9 <- Data$cover3/2025
    
  }
  
  #Data <- na.omit(Data)
  Data <- Data[Data$cover9 > 0.1 | Data$cover9 < -0.1,]
  Data$Type[Data$cover9 > 0.2]= "Growth"
  Data$Type[Data$cover9 < -0.2]= "Loss"
  
  year <- paste0(strsplit(basename(i),"_")[[1]][3],"-",strsplit(basename(i),"_")[[1]][4])
  if (strsplit(basename(i),"_")[[1]][5] == "HV7"){
    year <- paste0(substr(strsplit(basename(i),"_")[[1]][3],1,3),
                   substr(strsplit(basename(i),"_")[[1]][5],3,3),"-",
                   substr(strsplit(basename(i),"_")[[1]][4],1,3),
                   substr(strsplit(basename(i),"_")[[1]][5],3,3))
  }
  a <- t.test(CA_o_mean9 ~ Type, data = Data,
              var.equal = TRUE, alternative = "two.sided")    
  
  print(paste0("Year: ",year))
  print(paste0("Aggregation: ",window))
  print(paste0("T-value: ",a[[1]]))
  print(paste0("P-value: ",a[[3]]))
  list1[[1]] <- year
  list1[[2]] <- window
  list1[[3]] <- a[[1]]
  list1[[4]] <- a[[3]]
  
  df1 <- data.frame(list1)
  colnames(df1) <- listt
  df <- rbind(df,df1)
  
  print(ggplot(Data, aes(x=Type, y=CA_o_mean9)) + geom_boxplot())
}

write.table(df,file=file.path(Locdir,paste0(pattern,"_t5.csv")), quote=F,sep=" ",row.names=F)




#water cloud model
filedir <- "E:\\ChangMap\\CHM\\DB_archive\\Individual\\Cover\\Merged_ALOS_2018_HV_w_result_cover.csv"
filedir <- "E:\\ChangMap\\CHM\\DB_csv\\Individual\\Cover\\Merged_ALOS_2018_HV_w_result_cover.csv"
Data = read.csv(filedir,header=T)
#Data <- na.omit(Data)
Data$CA_o_mean5 <- Data$CA_o_mean5/1000000

#r2
x <- Data$C_c_sum5/5625
y<- Data$CA_o_mean5
mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-22, b=-14, c=3.9))
y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
  coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
R2 <- round(cor(y,y_sim)^2,3)



#loop r2
Locdir <- "E:\\ChangMap\\CHM\\DB_archive\\Individual\\Volume"
Locdir2 <- "E:\\ChangMap\\CHM\\DB_archive\\Individual\\Cover"
pattern = 'Merge'
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  try({
    print(i)
    Data = read.csv(i,header=T)
    #Data <- na.omit(Data)
    Data$CA_o_mean5 <- Data$CA_o_mean5/1000000
    #x <- Data$C_s_sum5
    
    #run cover
    sub <- paste0(strsplit(strsplit(i, "Volume/")[[1]][2],".csv"),"_cover.csv")
    i2 <- file.path(Locdir2,sub)
    Data2 = read.csv(i2,header=T)
    Data2 <- na.omit(Data2)
    x <- Data2$C_c_sum5/5625
    
    y<- 10^(Data$CA_o_mean5/10)
    #mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)),start = list(a=-20, b=-10, c=8e-5))
    mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)),start = list(a=0, b=49, c=0.0001))
    y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
      coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
    R2 <- round(cor(y,y_sim)^2,3)
    #Error
    RMSE <- sqrt(mean((y - y_sim)^2))
    rRMSE <- 100*sqrt(mean((y - y_sim)^2))/mean(y)
    MD <- mean(y - y_sim)
    MAE <- mae(y_sim, y)
    print(paste0("R2: ",R2))
    print(paste0("Mean bias: ",round(MD,3)))
    print(paste0("MAE: ",round(MAE,3)))
    print(paste0("RMSE: ",round(RMSE,3)))
    print(paste0("%RMSE: ",round(rRMSE,3)))
  })
}


#regression y~HH+HV
Locdir <- "E:\\ChangMap\\CHM\\DB_archive\\Individual\\Volume\\Match_ALOS_2008_HV_w_result.csv"
Data = read.csv(Locdir,header=T)
#Data <- na.omit(Data)
x1 <- Data$CA_o_mean5_x/1000000
x2 <- Data$CA_o_mean5_y/1000000
y <- Data$C_s_sum5
lm <- lm(y ~ x1+x2, data=Data)
summary(lm)

#loop regression y~HH+HV
Locdir <- "E:\\ChangMap\\CHM\\DB_archive\\Individual\\Cover"
pattern = 'Match'
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  try({
    print(i)
    Data = read.csv(i,header=T)
    #Data <- na.omit(Data)
    x1 <- Data$CA_o_mean5_x/1000000
    x2 <- Data$CA_o_mean5_y/1000000
    y <- Data$C_c_sum5
    lm <- lm(y ~ x1+x2, data=Data)
    y_sim <- coef(lm)[1]*x1+coef(lm)[2]*x2+coef(lm)[3]
    summary(lm)
    R2 <- round(summary(lm)$adj.r.squared,3)
    #Error
    RMSE <- sqrt(mean((y - y_sim)^2))
    rRMSE <- 100*sqrt(mean((y - y_sim)^2))/mean(y)
    MD <- mean(y - y_sim)
    MAE <- mae(y_sim, y)
    print(paste0("R2: ",R2))
    print(paste0("Mean bias: ",round(MD,3)))
    print(paste0("MAE: ",round(MAE,3)))
    print(paste0("RMSE: ",round(RMSE,3)))
    print(paste0("%RMSE: ",round(rRMSE,3)))
  })
}

#t-test, randomly sampled
Locdir <- "E:\\ChangMap\\CHM\\DB_archive\\Change\\Cover"
pattern =  "Merge"
listt <- list("Year","T-value","P-value")
resample <- c(300,1000,2000,3000,4000)
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
source_gist("https://gist.github.com/mrdwab/6424112")
output <- "E:\\ChangMap\\CHM\\Image\\Cover_Pairwise_Boxplot"
for (i in files){
  for (j in resample){
    try({
      print(i)
      list1 <- list()
      Data = read.csv(i,header=T)
      Data$CA_o_mean5 <- Data$CA_o_mean5/1000000
      Data$cover5 <- Data$cover5/5625
      #Data <- na.omit(Data)
      Data$Type[Data$cover5 >= 0.3] = ">0.3"
      Data$Type[Data$cover5 >= 0.2 & Data$cover5 < 0.3] = "0.2~0.3"
      Data$Type[Data$cover5 >= 0.1 & Data$cover5 < 0.2] = "0.1~0.2"
      Data$Type[Data$cover5 >= 0 & Data$cover5 < 0.1] = "0~0.1"
      Data$Type[Data$cover5 >= -0.1 & Data$cover5 < 0] = "-0.1~0"
      Data$Type[Data$cover5 >= -0.2 & Data$cover5 < -0.1] = "-0.2~-0.1"
      Data$Type[Data$cover5 >= -0.3 & Data$cover5 < -0.2] = "-0.3~-0.2"
      Data$Type[Data$cover5 <= -0.3] = "-0.3<"
      Data <- stratified(Data, "Type", j)
      Data$Type<-factor(Data$Type, levels=c("-0.3<","-0.3~-0.2", "-0.2~-0.1","-0.1~0",
                                            "0~0.1","0.1~0.2","0.2~0.3",">0.3"))
      
      year <- paste0(strsplit(basename(i),"_")[[1]][3],"-",strsplit(basename(i),"_")[[1]][4])
      if (strsplit(basename(i),"_")[[1]][5] == "HV7"){
        year <- paste0(substr(strsplit(basename(i),"_")[[1]][3],1,3),"7-",
                       substr(strsplit(basename(i),"_")[[1]][4],1,2),"07")
      }
      ggplot(Data, aes(x=Type, y=CA_o_mean5, group = Type))+
        geom_boxplot()+
        stat_compare_means(comparisons=
                             list(c("-0.3<","-0.3~-0.2"),
                                  c("-0.3~-0.2","-0.2~-0.1"),
                                  c("-0.2~-0.1","-0.1~0"),
                                  c("-0.1~0","0~0.1"),
                                  c("0~0.1","0.1~0.2"),
                                  c("0.1~0.2","0.2~0.3"),
                                  c("0.2~0.3",">0.3")),label.y = 5,
                           label = "p.signif")+
        theme_bw()+
        theme(text=element_text(size=20)) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(legend.title = element_blank())+
        labs(title=paste0(strsplit(basename(i),"_")[[1]][1]," ",year," Resampled ",j),
             x = paste0("LiDAR CHM cover change(%)"),
             y = paste0("ALOS backscatter change (m)")) +
        theme(plot.title = element_text(hjust = 0.5))
      out <- file.path(output,
                       paste0(strsplit(basename(i),"_")[[1]][1],
                              " ",year," Resampled ",j,".jpg"))
      ggsave(out, height=15, width=15, dpi=600)
    })
  }
  
}
  



#pairwise t-test metrix, randomly sampled 300 only
i <- "E:\\ChangMap\\CHM\\DB_archive\\Change\\Cover\\Merged_ALOS_2018_2010_HV_w_result_cover.csv"
j <- 300
source_gist("https://gist.github.com/mrdwab/6424112")
Data = read.csv(i,header=T)
Data$CA_o_mean5 <- Data$CA_o_mean5/1000000
Data$cover5 <- Data$C_c_sum5/5625
#Data <- na.omit(Data)
Data$Type[Data$cover5 >= 0.3] = ">0.3"
Data$Type[Data$cover5 >= 0.2 & Data$cover5 < 0.3] = "0.2~0.3"
Data$Type[Data$cover5 >= 0.1 & Data$cover5 < 0.2] = "0.1~0.2"
Data$Type[Data$cover5 >= 0 & Data$cover5 < 0.1] = "0~0.1"
Data$Type[Data$cover5 >= -0.1 & Data$cover5 < 0] = "-0.1~0"
Data$Type[Data$cover5 >= -0.2 & Data$cover5 < -0.1] = "-0.2~-0.1"
Data$Type[Data$cover5 >= -0.3 & Data$cover5 < -0.2] = "-0.3~-0.2"
Data$Type[Data$cover5 <= -0.3] = "-0.3<"
Data <- stratified(Data, "Type", j)
Data$Type<-factor(Data$Type, levels=c("-0.3<","-0.3~-0.2", "-0.2~-0.1","-0.1~0",
                                      "0~0.1","0.1~0.2","0.2~0.3",">0.3"))

year <- paste0(strsplit(basename(i),"_")[[1]][3],"-",strsplit(basename(i),"_")[[1]][4])
if (strsplit(basename(i),"_")[[1]][5] == "HV7"){
  year <- paste0(substr(strsplit(basename(i),"_")[[1]][3],1,3),"7-",
                 substr(strsplit(basename(i),"_")[[1]][4],1,2),"07")
}

ggplot(Data, aes(x=Type, y=CA_o_mean5, group = Type))+
  geom_boxplot()+
  stat_compare_means(comparisons=
                       list(c("-0.3<","-0.3~-0.2"),
                            c("-0.3~-0.2","-0.2~-0.1"),
                            c("-0.2~-0.1","-0.1~0"),
                            c("-0.1~0","0~0.1"),
                            c("0~0.1","0.1~0.2"),
                            c("0.1~0.2","0.2~0.3"),
                            c("0.2~0.3",">0.3")),
                     method = "t.test",
                     label.y = 5,
                     label = "p.signif")+
  theme_bw()+
  theme(text=element_text(size=20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  labs(title=paste0(strsplit(basename(i),"_")[[1]][1]," ",year," Resampled ",j),
       x = paste0("LiDAR CHM cover change(%)"),
       y = paste0("ALOS backscatter change (m)")) +
  theme(plot.title = element_text(hjust = 0.5))

r1 <- compare_means(
  CA_o_mean5~Type, 
  Data, 
  method = "t.test", 
  p.adjust.method = "BH")
r2 <- r1[c("group1", "group2", "p.signif")]
library(igraph)
G <- graph.data.frame(r2,directed=FALSE)
A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="p.signif",type='lower')
write.csv(A,file="C:\\Users\\Shawn\\Desktop\\table1.csv")



#pairwise t-test metrix, randomly sampled 300 only
i <- "E:\\ChangMap\\CHM\\DB_archive\\Change\\Volume\\Merged_ALOS_2010_2008_HV7_w_result.csv"
j <- 300
source_gist("https://gist.github.com/mrdwab/6424112")
Data = read.csv(i,header=T)
Data$CA_o_mean5 <- Data$CA_o_mean5/1000000
Data$Type[Data$C_s_sum5 >= -3000] = ">-3000"
Data$Type[Data$C_s_sum5 >= -3000 & Data$C_s_sum5 < -2000] = "-3000~-2000"
Data$Type[Data$C_s_sum5 >= -2000 & Data$C_s_sum5 < -1000] = "-2000~-1000"
Data$Type[Data$C_s_sum5 >= -1000 & Data$C_s_sum5 < 0] = "-1000~0"
Data$Type[Data$C_s_sum5 >= -0.1 & Data$C_s_sum5 < 1000] = "0~1000"
Data$Type[Data$C_s_sum5 >= 1000 & Data$C_s_sum5 < 2000] = "1000~2000"
Data$Type[Data$C_s_sum5 >= 2000 & Data$C_s_sum5 < 3000] = "2000~3000"
Data$Type[Data$C_s_sum5 >= 3000] = ">3000"
Data <- stratified(Data, "Type", j)
Data$Type<-factor(Data$Type, levels=c(">-3000","-3000~-2000", "-2000~-1000","-1000~0",
                                      "0~1000","1000~2000","2000~3000",">3000"))
#Data <- na.omit(Data)
year <- paste0(strsplit(basename(i),"_")[[1]][3],"-",strsplit(basename(i),"_")[[1]][4])
if (strsplit(basename(i),"_")[[1]][5] == "HV7"){
  year <- paste0(substr(strsplit(basename(i),"_")[[1]][3],1,3),"7-",
                 substr(strsplit(basename(i),"_")[[1]][4],1,2),"07")
}

ggplot(Data, aes(x=Type, y=CA_o_mean5, group = Type))+
  geom_boxplot()+
  stat_compare_means(comparisons=
                       list(c(">-3000","-3000~-2000"),
                            c("-3000~-2000","-2000~-1000"),
                            c("-2000~-1000","-1000~0"),
                            c("-1000~0","0~1000"),
                            c("0~1000","1000~2000"),
                            c("1000~2000","2000~3000"),
                            c("2000~3000",">3000")),
                     method = "t.test",
                     label.y = 5,
                     label = "p.signif")+
  theme_bw()+
  theme(text=element_text(size=20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  labs(title=paste0(strsplit(basename(i),"_")[[1]][1]," ",year," Resampled ",j),
       x = paste0("LiDAR CHM cover change(%)"),
       y = paste0("ALOS backscatter change (m)")) +
  theme(plot.title = element_text(hjust = 0.5))

r1 <- compare_means(
  CA_o_mean5~Type, 
  Data, 
  method = "t.test", 
  p.adjust.method = "BH")
r2 <- r1[c("group1", "group2", "p.signif")]

G <- graph.data.frame(r2,directed=FALSE)
A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="p.signif",type='lower')
A
#out = file.path("C:\\Users\\Shawn\\Desktop",paste0(strsplit(basename(i),"_")[[1]][1]," ",year," Resampled ",j,".csv"))
#write.csv(A,file=out)


#water cloud model
filedir <- "E:\\ChangMap\\CHM\\DB_archive\\Individual\\Volume\\Merged_ALOS_2018_HV_w_result.csv"
filedir <- "E:\\ChangMap\\CHM\\DB_csv\\merge_HH_2018_cover.csv"

Data = read.csv(filedir,header=T)
#Data <- na.omit(Data)

#r2 db scale
#x <- Data$cover
x <- Data$C_c_sum5
y<- Data$CA_o_mean5
mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-25, b=-12, c=2))
y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
  coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
R2 <- round(cor(y,y_sim)^2,3)

coef(mult_nls)[1]
coef(mult_nls)[2]
coef(mult_nls)[3]
round(cor(y,y_sim)^2,3)

#r2 linear scale
x <- Data$cover
y<- Data$linear
linear <- lm(y~x)
summary(linear)$adj.r.squared

#07092021 density 

filedir <- "E:\\ChangMap\\CHM\\DB_archive\\Individual\\Volume\\Merged_ALOS_2008_HV7_w_result.csv"
Data = read.csv(filedir,header=T)
#Data <- na.omit(Data)
#Data <- Data[sample(1:nrow(Data), 3000), ]

#water cloud model 
Data$CA_o_mean5 <- Data$CA_o_mean5/1000000
x <- Data$C_s_sum5
y<- Data$CA_o_mean5
mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-25, b=-12, c=9e-5))
y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
  coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
R2 <- round(cor(y,y_sim)^2,3)
round(cor(y,y_sim)^2,3)

reg1 <- lm(y ~ x , data = Data)
reg2 <- lm(y ~ poly(x,2), data = Data)
reg3 <- lm(y ~ log(x), data = Data)

#regression density plot
ggplot(Data, aes(x=C_s_sum5, y=CA_o_mean5))+ 
  geom_pointdensity()+
  scale_color_viridis()+
  theme_bw()+
  theme(text=element_text(size=20)) +
  #scale_y_continuous(limits = c(-30,-10))+
  geom_smooth(method = "nls", 
              method.args = list(formula = y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                                 start = list(a=-21.96345, b=-10.31212, c=6.09e-5)),
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
    formula = 'y ~ poly(x,2)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "red")+
  geom_smooth(
    method="lm",
    formula = 'y ~ log2(x)', 
    se= F, 
    size = 1.5, 
    linetype = "solid",
    colour = "blue")+
  annotate("text", x=20000, y=-20, hjust = 0,color="black",size = 10,
           label= paste(" Water cloud model R2: ",round(R2,3))) + 
  annotate("text", x=20000, y=-21, hjust = 0,color="purple",size = 10,
           label= paste(" Linear model R2: ",round(summary(reg1)$adj.r.squared,3))) + 
  annotate("text", x=20000, y=-22, hjust = 0,color="red",size = 10,
           label= paste(" 2nd Polynomial model R2: ",round(summary(reg2)$adj.r.squared,3))) + 
  annotate("text", x=20000, y=-23, hjust = 0,color="blue",size = 10,
           label= paste(" Logarithmic model R2: ",round(summary(reg3)$adj.r.squared,3))) + 
  labs(title="2018 ALOS PALSAR Backscatter ~ ALS CHM Volume",
       x = paste0("ALS CHM Volume (5*5)"),
       y = paste0("Mean ALOS PALSAR backscatter (5*5)")) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom",legend.key.width=unit(6,"cm"))


out = "E:\\ChangMap\\CHM\\Image\\Scatterplot.jpg"
ggsave(out,height=12, width=18, dpi=600)



#070902021 boxplot


#boxplot -- cover
filedir <- "E:\\ChangMap\\CHM\\DB_archive\\Change\\Cover\\Merged_ALOS_2018_2008_HV_w_result_cover.csv"
Data = read.csv(filedir,header=T)

Data$y <- Data$CA_o_mean5/1000000
Data$x <- Data$C_c_sum5/5625

Data$group <- cut(Data$x,breaks = c(-0.7,-0.6,-0.5,-0.4,-0.3,
                                           -0.2,-0.1,0,0.1,0.2,0.3,
                                           0.4,0.5,0.6,0.7)
                  ,dig.lab=6)
#Data <- na.omit(Data)


ggplot(Data, aes(x=x, y=y, fill=group)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  scale_x_continuous(breaks = seq(-1, 1, 0.1))+
  geom_boxplot(outline=FALSE)+
  theme_bw()+
  labs(title="2018-2008 ALOS PALSAR Backscatter ~ ALS CHM Cover change",
       x = "ALS CHM Cover change (5*5)",
       y = "ALOS PALSAR backscatter (5*5)") +
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))

out = "E:\\ChangMap\\CHM\\Image\\Boxplot.jpg"
ggsave(out,height=12, width=18, dpi=600)





#AGU
#volume R2 linear, log, wcm individual
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\Individual"
pattern = 'volume.csv'
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  print(i)
  
  Data = read.csv(i,header=T)
  Data <- Data[Data$C_s_sum5 > 0,]
  #Data <- na.omit(Data)
  
  x <- Data$C_s_sum5
  y <- Data$CA_o_mean5
  

  reg1 <- lm(y  ~ x, data = Data)
  reg2 <- lm(y ~ log(x), data = Data)
  
  reg_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-20, b=-10, c=6e-5))
  y_sim <- coef(reg_nls)[1]*exp(-coef(reg_nls)[3]*x)+
    coef(reg_nls)[2]*(1-exp(-coef(reg_nls)[3]*x))
  R2 <- round(cor(y,y_sim)^2,3)
  
  print(round(summary(reg1)$adj.r.squared,3))
  print(round(summary(reg2)$adj.r.squared,3))
  print(round(R2,3))
  print(coef(reg_nls)[1])
  print(coef(reg_nls)[2])
  print(coef(reg_nls)[3])
}


#cover R2 linear, log, wcm
#stats table -- individual cases
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\Individual"
pattern = 'cover.csv'
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  print(i)
  
  Data = read.csv(i,header=T)
  Data <- Data[Data$C_c_sum5>0 & Data$C_c_sum5<1,]
  #Data <- na.omit(Data)
  
  x <- Data$C_c_sum5
  y <- Data$CA_o_mean5
  
  
  reg1 <- lm(y  ~ x, data = Data)
  reg2 <- lm(y ~ log(x), data = Data)
  
  reg_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                 start = list(a=-20, b=-10, c=2.5))
  y_sim <- coef(reg_nls)[1]*exp(-coef(reg_nls)[3]*x)+
    coef(reg_nls)[2]*(1-exp(-coef(reg_nls)[3]*x))
  R2 <- round(cor(y,y_sim)^2,3)
  
  print(round(summary(reg1)$adj.r.squared,3))
  print(round(summary(reg2)$adj.r.squared,3))
  print(round(R2,3))
  print(coef(reg_nls)[1])
  print(coef(reg_nls)[2])
  print(coef(reg_nls)[3])
}

#multivariate
Locdir1 <- "E:\\ChangMap\\CHM\\DB_csv\\Individual\\merge_2018_HV_volume.csv"
Locdir2 <- "E:\\ChangMap\\CHM\\DB_csv\\Individual\\merge_2018_HH_volume.csv"
Data1 = read.csv(Locdir1,header=T)
Data2 = read.csv(Locdir2,header=T)
total <- merge(Data1,Data2,by=c("FID"))
reg3 <- lm(C_s_sum5.x  ~ CA_o_mean5.x+CA_o_mean5.y, data = total)
print(round(summary(reg3)$adj.r.squared,3))


#volume R2 linear change
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\Change"
pattern = 'volume.csv'
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  print(i)
  
  Data = read.csv(i,header=T)
  #Data <- na.omit(Data)
  
  x <- Data$C_s_sum5
  y <- Data$CA_o_mean5
  
  reg1 <- lm(y  ~ x, data = Data)

  print(round(summary(reg1)$adj.r.squared,3))
  
}


#cover R2 linear change
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\Change"
pattern = 'cover.csv'
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  print(i)
  
  Data = read.csv(i,header=T)
  #Data <- na.omit(Data)
  
  x <- Data$C_c_sum5
  y <- Data$CA_o_mean5
  
  reg1 <- lm(y  ~ x, data = Data)
  
  print(round(summary(reg1)$adj.r.squared,3))
  
}

#scatterplot -- volume individual
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\Individual"
files <- list.files(path = Locdir, full.names = TRUE, pattern = "volume.csv")
for (i in files){
  print(i)
  Data = read.csv(i,header=T)
  ##Data <- na.omit(Data)
  #Data <- Data[sample(nrow(Data), 300), ]
  Data <- Data[Data$C_s_sum5 > 0,]
  x <- Data$C_s_sum5
  y <- Data$CA_o_mean5
  
  
  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-25, b=-12, c=6e-5))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2 <- round(cor(y,y_sim)^2,3)
  round(cor(y,y_sim)^2,3)
  
  reg1 <- lm(y ~ x , data = Data)
  reg3 <- lm(y ~ log(x), data = Data)
  year <- paste0(strsplit(basename(i),"_")[[1]][2]," ",
                 substr(strsplit(basename(i),"_")[[1]][3],1,2))
  if (strsplit(basename(i),"_")[[1]][3] == "HV7"){
    year <- paste0(substr(strsplit(basename(i),"_")[[1]][2],1,3),"7"," ",
                   substr(strsplit(basename(i),"_")[[1]][3],1,2))
  }
  if (strsplit(basename(i),"_")[[1]][3] == "HH7"){
    year <- paste0(substr(strsplit(basename(i),"_")[[1]][2],1,3),"7"," ",
                   substr(strsplit(basename(i),"_")[[1]][3],1,2))
  }
  #title <- paste0("Volume individual ",year," Resampled ",j)
  title <- paste0("Volume individual ",year)
  out = file.path("E:\\ChangMap\\CHM\\DB_csv\\Figure",paste0(title,".jpg"))
  ggplot(Data, aes(x=x, y=y))+ 
    ylim(min(y), 0)+
    geom_pointdensity()+
    scale_color_viridis(direction = 1)+
    geom_smooth(method = "nls", 
                method.args = list(formula = y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                                   start = list(a=-21.96345, b=-10.31212, c=6e-5)),
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
      formula = 'y ~ log2(x)', 
      se= F, 
      size = 1.5, 
      linetype = "solid",
      colour = "blue")+
    annotate("text", x=min(x), y=0, hjust = 0,color="black",size = 10,
             label= paste(" Water cloud model R2: ",round(R2,3))) + 
    annotate("text", x=min(x), y=-2,hjust = 0,color="purple",size = 10,
             label= paste(" Linear model R2: ",round(summary(reg1)$adj.r.squared,3))) + 
    annotate("text", x=min(x), y=-1,hjust = 0,color="blue",size = 10,
             label= paste(" Logarithmic model R2: ",round(summary(reg3)$adj.r.squared,3))) + 
    labs(title=title,
         x = paste0("LiDAR CHM volume"),
         y = paste0("ALOS backscatter")) +
    theme(text=element_text(size=20)) +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(out, height=15, width=15, dpi=300)
  
}


#scatterplot -- CC individual
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\Individual"
files <- list.files(path = Locdir, full.names = TRUE, pattern = "cover.csv")
for (i in files){
  print(i)
  Data = read.csv(i,header=T)
  ##Data <- na.omit(Data)
  #Data <- Data[sample(nrow(Data), 300), ]
  Data <- Data[Data$C_c_sum5 > 0,]
  #Data <- Data[Data$CA_o_mean5 < -2,]
  x <- Data$C_c_sum5
  y <- Data$CA_o_mean5

  mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                  start = list(a=-25, b=-12, c=2))
  y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
  R2 <- round(cor(y,y_sim)^2,3)
  round(cor(y,y_sim)^2,3)
  
  reg1 <- lm(y ~ x , data = Data)
  reg3 <- lm(y ~ log(x), data = Data)
  year <- paste0(strsplit(basename(i),"_")[[1]][2]," ",
                 substr(strsplit(basename(i),"_")[[1]][3],1,2))
  if (strsplit(basename(i),"_")[[1]][3] == "HV7"){
    year <- paste0(substr(strsplit(basename(i),"_")[[1]][2],1,3),"7"," ",
                   substr(strsplit(basename(i),"_")[[1]][3],1,2))
  }
  if (strsplit(basename(i),"_")[[1]][3] == "HH7"){
    year <- paste0(substr(strsplit(basename(i),"_")[[1]][2],1,3),"7"," ",
                   substr(strsplit(basename(i),"_")[[1]][3],1,2))
  }
  #title <- paste0("Cover individual ",year," Resampled ",j)
  title <- paste0("Cover individual ",year)
  out = file.path("E:\\ChangMap\\CHM\\DB_csv\\Figure",paste0(title,".jpg"))
  ggplot(Data, aes(x=x, y=y))+
    ylim(min(y), 0)+
    geom_pointdensity()+
    scale_color_viridis(direction = 1)+
    geom_smooth(method = "nls", 
                method.args = list(formula = y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                                   start = list(a=-21.96345, b=-10.31212, c=2)),
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
      formula = 'y ~ log2(x)', 
      se= F, 
      size = 1.5, 
      linetype = "solid",
      colour = "blue")+
    annotate("text", x=0, y=0, hjust = 0,color="black",size = 10,
             label= paste(" Water cloud model R2: ",round(R2,3))) + 
    annotate("text", x=0, y=-2,hjust = 0,color="purple",size = 10,
             label= paste(" Linear model R2: ",round(summary(reg1)$adj.r.squared,3))) + 
    annotate("text", x=0, y=-1,hjust = 0,color="blue",size = 10,
             label= paste(" Logarithmic model R2: ",round(summary(reg3)$adj.r.squared,3))) + 
    labs(title=title,
         x = paste0("LiDAR CHM cover"),
         y = paste0("ALOS backscatter")) +
    theme(text=element_text(size=20)) +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(out, height=15, width=15, dpi=300)
  
}


#scatterplot -- volume change
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\Change"
files <- list.files(path = Locdir, full.names = TRUE, pattern = "volume.csv")

for (i in files){
  print(i)
  Data = read.csv(i,header=T)
  ##Data <- na.omit(Data)
  #Data <- Data[sample(nrow(Data), 300), ]
  x <- Data$C_s_sum5
  y <- Data$CA_o_mean5
  ggplot(Data, aes(x=x, y=y))+ 
    geom_pointdensity()+
    scale_color_viridis(direction = -1)+
    stat_poly_eq(formula = y ~x, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE, size = 10) +         
    geom_smooth(
      method="lm",
      formula = 'y ~ x', 
      se= F, 
      size = 1.5, 
      linetype = "solid",
      colour = "purple")+
    labs(title = basename(i),
      x = "LiDAR CHM height volume change",
         y = "SAR backscatter change")+
    theme(text=element_text(size=20)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  out = file.path("E:\\ChangMap\\CHM\\DB_csv\\Figure",paste0(basename(i),".jpg"))
  ggsave(out, height=15, width=15, dpi=300)
  
}
  

#scatterplot -- CC change
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\Change"
files <- list.files(path = Locdir, full.names = TRUE, pattern = "cover.csv")
for (i in files){
  print(i)
  Data = read.csv(i,header=T)
  ##Data <- na.omit(Data)
  #Data <- Data[sample(nrow(Data), 300), ]
  x <- Data$C_c_sum5
  y <- Data$CA_o_mean5
  ggplot(Data, aes(x=x, y=y))+ 
    geom_pointdensity()+
    scale_color_viridis(direction = -1)+
    stat_poly_eq(formula = y ~x, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE, size = 10) +         
    geom_smooth(
      method="lm",
      formula = 'y ~ x', 
      se= F, 
      size = 1.5, 
      linetype = "solid",
      colour = "purple")+
    labs(title = basename(i),
         x = "LiDAR CHM cover change",
         y = "SAR backscatter change")+
    theme(text=element_text(size=20)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  out = file.path("E:\\ChangMap\\CHM\\DB_csv\\Figure",paste0(basename(i),".jpg"))
  ggsave(out, height=15, width=15, dpi=300)
  
}


#volume pairwise t-test metrix, randomly sampled 300 only
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\Change"
files <- list.files(path = Locdir, full.names = TRUE, pattern = "volume.csv")
for (i in files){
  print(i)
  j <- 300
  #source_gist("https://gist.github.com/mrdwab/6424112")
  Data = read.csv(i,header=T)
  Data$CA_o_mean5 <- Data$CA_o_mean5
  Data$Type[Data$C_s_sum5 >= -3000] = ">-3000"
  Data$Type[Data$C_s_sum5 >= -3000 & Data$C_s_sum5 < -2000] = "-3000~-2000"
  Data$Type[Data$C_s_sum5 >= -2000 & Data$C_s_sum5 < -1000] = "-2000~-1000"
  Data$Type[Data$C_s_sum5 >= -1000 & Data$C_s_sum5 < 0] = "-1000~0"
  Data$Type[Data$C_s_sum5 >= -0.1 & Data$C_s_sum5 < 1000] = "0~1000"
  Data$Type[Data$C_s_sum5 >= 1000 & Data$C_s_sum5 < 2000] = "1000~2000"
  Data$Type[Data$C_s_sum5 >= 2000 & Data$C_s_sum5 < 3000] = "2000~3000"
  Data$Type[Data$C_s_sum5 >= 3000] = ">3000"
  Data <- stratified(Data, "Type", j)
  Data$Type<-factor(Data$Type, levels=c(">-3000","-3000~-2000", "-2000~-1000","-1000~0",
                                        "0~1000","1000~2000","2000~3000",">3000"))
  ##Data <- na.omit(Data)
  year <- paste0(strsplit(basename(i),"_")[[1]][2],"-",
                 strsplit(basename(i),"_")[[1]][3]," ",
                 substr(strsplit(basename(i),"_")[[1]][4],1,2))
  if (strsplit(basename(i),"_")[[1]][4] == "HV7"|| 
      strsplit(basename(i),"_")[[1]][4] == "HH7"){
    year <- gsub("8", "7", paste0(strsplit(basename(i),"_")[[1]][2],"-",
                                  strsplit(basename(i),"_")[[1]][3]," ",
                 substr(strsplit(basename(i),"_")[[1]][4],1,2)))
  }
  title <- paste0("Volume change ",year," Resampled ",j)
  out = file.path("E:\\ChangMap\\CHM\\DB_csv\\Figure",paste0(title,".jpg"))
  ggplot(Data, aes(x=Type, y=CA_o_mean5, group = Type))+
    geom_boxplot()+
    geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
    stat_compare_means(comparisons=
                         list(c(">-3000","-3000~-2000"),
                              c("-3000~-2000","-2000~-1000"),
                              c("-2000~-1000","-1000~0"),
                              c("-1000~0","0~1000"),
                              c("0~1000","1000~2000"),
                              c("1000~2000","2000~3000"),
                              c("2000~3000",">3000")),
                       method = "t.test",
                       label.y = 5,
                       label = "p.signif")+
    theme_bw()+
    theme(text=element_text(size=20)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    labs(title=title,
         x = paste0("LiDAR CHM volume change"),
         y = paste0("ALOS backscatter change")) +
    theme(plot.title = element_text(hjust = 0.5))
  print(out)
  ggsave(out, height=15, width=15, dpi=300)
}
r1 <- compare_means(
  CA_o_mean5~Type, 
  Data, 
  method = "t.test", 
  p.adjust.method = "BH")
r2 <- r1[c("group1", "group2", "p.signif")]

G <- graph.data.frame(r2,directed=FALSE)
A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="p.signif",type='lower')
A
#out = file.path("C:\\Users\\Shawn\\Desktop",paste0(strsplit(basename(i),"_")[[1]][1]," ",year," Resampled ",j,".csv"))
#write.csv(A,file=out)




#cover pairwise t-test metrix, randomly sampled 300 only
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\Change"
files <- list.files(path = Locdir, full.names = TRUE, pattern = "cover.csv")
for (i in files){
  print(i)
  j <- 300
  #source_gist("https://gist.github.com/mrdwab/6424112")
  Data = read.csv(i,header=T)
  Data$CA_o_mean5 <- Data$CA_o_mean5
  Data$cover5 <- Data$C_c_sum5
  ##Data <- na.omit(Data)
  Data$Type[Data$cover5 >= 0.3] = ">0.3"
  Data$Type[Data$cover5 >= 0.2 & Data$cover5 < 0.3] = "0.2~0.3"
  Data$Type[Data$cover5 >= 0.1 & Data$cover5 < 0.2] = "0.1~0.2"
  Data$Type[Data$cover5 >= 0 & Data$cover5 < 0.1] = "0~0.1"
  Data$Type[Data$cover5 >= -0.1 & Data$cover5 < 0] = "-0.1~0"
  Data$Type[Data$cover5 >= -0.2 & Data$cover5 < -0.1] = "-0.2~-0.1"
  Data$Type[Data$cover5 >= -0.3 & Data$cover5 < -0.2] = "-0.3~-0.2"
  Data$Type[Data$cover5 <= -0.3] = "-0.3<"
  Data <- stratified(Data, "Type", j)
  Data$Type<-factor(Data$Type, levels=c("-0.3<","-0.3~-0.2", "-0.2~-0.1","-0.1~0",
                                        "0~0.1","0.1~0.2","0.2~0.3",">0.3"))
  year <- paste0(strsplit(basename(i),"_")[[1]][2],"-",
                 strsplit(basename(i),"_")[[1]][3]," ",
                 substr(strsplit(basename(i),"_")[[1]][4],1,2))
  if (strsplit(basename(i),"_")[[1]][4] == "HV7"|| 
      strsplit(basename(i),"_")[[1]][4] == "HH7"){
    year <- gsub("8", "7", paste0(strsplit(basename(i),"_")[[1]][2],"-",
                                  strsplit(basename(i),"_")[[1]][3]," ",
                                  substr(strsplit(basename(i),"_")[[1]][4],1,2)))
  }
  title <- paste0("Cover change ",year," Resampled ",j)
  out = file.path("E:\\ChangMap\\CHM\\DB_csv\\Figure",paste0(title,".jpg"))
  ggplot(Data, aes(x=Type, y=CA_o_mean5, group = Type))+
    geom_boxplot()+
    geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
    stat_compare_means(comparisons=
                         list(c("-0.3<","-0.3~-0.2"),
                              c("-0.3~-0.2","-0.2~-0.1"),
                              c("-0.2~-0.1","-0.1~0"),
                              c("-0.1~0","0~0.1"),
                              c("0~0.1","0.1~0.2"),
                              c("0.1~0.2","0.2~0.3"),
                              c("0.2~0.3",">0.3")),
                       method = "t.test",
                       label.y = 5,
                       label = "p.signif")+
    theme_bw()+
    theme(text=element_text(size=20)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    labs(title=title,
         x = paste0("LiDAR CHM cover change"),
         y = paste0("ALOS backscatter change")) +
    theme(plot.title = element_text(hjust = 0.5))
  print(out)
  ggsave(out, height=15, width=15, dpi=300)
}
r1 <- compare_means(
  CA_o_mean5~Type, 
  Data, 
  method = "t.test", 
  p.adjust.method = "BH")
r2 <- r1[c("group1", "group2", "p.signif")]
library(igraph)
G <- graph.data.frame(r2,directed=FALSE)
A <- as_adjacency_matrix(G,names=TRUE,sparse=FALSE,attr="p.signif",type='lower')
A
#write.csv(A,file="C:\\Users\\Shawn\\Desktop\\table1.csv")



#correlation--inverted volume
#the NA should be removed to maintain the R2/stats
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\Individual"
pattern = 'volume.csv'
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  print(i)
  Data = read.csv(i,header=T)
  Data <- na.omit(Data)
  y <- Data$Vol_ALOS_w
  x <- Data$Vol_ALS
  reg1 <- lm(x  ~ y, data = Data)
  pre_linear <- predict(reg1)
  R2 <- round(cor(x,pre_linear)^2,3)
  
  MD <- mean(pre_linear - x)
  RB <- 100*MD/mean(x)
  RMSE <- sqrt(mean((pre_linear - x)^2))
  rRMSE <- 100*sqrt(mean((pre_linear - x)^2))/mean(x)
  print(R2)
  print(RMSE)
  print(rRMSE)
}


#correlation--inverted cover 
Locdir <- "E:\\ChangMap\\CHM\\DB_csv\\Individual"
pattern = 'cover.csv'
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  print(i)
  Data = read.csv(i,header=T)
  Data <- na.omit(Data)
  Data <- Data[Data$CC_ALOS_w <= 1 & Data$CC_ALOS_w >= 0,]
  y <- Data$CC_ALOS_w
  x <- Data$CC_ALS
  reg1 <- lm(x  ~ y, data = Data)
  pre_linear <- predict(reg1)
  R2 <- round(cor(x,pre_linear)^2,3)
  
  MD <- mean(pre_linear - x)
  RB <- 100*MD/mean(x)
  RMSE <- sqrt(mean((pre_linear - x)^2))
  rRMSE <- 100*sqrt(mean((pre_linear - x)^2))/mean(x)
  print(R2)
  print(MD)
  print(RB)
  print(RMSE)
  print(rRMSE)
}







#change -- volume change

change_volume_stats <- function(date1,date2){
  dir <- "E:\\ChangMap\\CHM\\DB_csv\\Individual"
  Locdir1 <- file.path(dir, paste0("merge_",date1,"_volume.csv"))
  Locdir2 <- file.path(dir, paste0("merge_",date2,"_volume.csv"))
  print(Locdir1)
  print(Locdir2)
  Data1 <-  read.csv(Locdir1,header=T)
  Data2 <-  read.csv(Locdir2,header=T)
  total <- merge(Data1,Data2,by=c("ORIG_FID"))
  total <- total[total$C_s_sum5.x > 0 & total$C_s_sum5.y > 0,]
  ALS_1 <- total$C_s_sum5.x
  ALS_2 <- total$C_s_sum5.y
  
  ALOS_ref_1 <- total$CA_o_mean5.x
  ALOS_ref_2 <- total$CA_o_mean5.y
  ALOS_ref_diff <- ALOS_ref_1 - ALOS_ref_2
  
  #linear
  reg1 <- lm(ALOS_ref_1 ~ ALS_1, data = total)
  ALOS_pred_1 <- predict(reg1)
  reg2 <- lm(ALOS_ref_2 ~ ALS_2, data = total)
  ALOS_pred_2 <- predict(reg2)
  ALOS_pred_diff <- ALOS_pred_1 - ALOS_pred_2
  
  reg <- lm(ALOS_ref_diff ~ ALOS_pred_diff)
  print(round(summary(reg)$adj.r.squared,3))
  
  #log
  reg1 <- lm(ALOS_ref_1 ~ log2(ALS_1), data = total)
  ALOS_pred_1 <- predict(reg1)
  reg2 <- lm(ALOS_ref_2 ~ log2(ALS_2), data = total)
  ALOS_pred_2 <- predict(reg2)
  ALOS_pred_diff <- ALOS_pred_1 - ALOS_pred_2
  
  reg <- lm(ALOS_ref_diff ~ ALOS_pred_diff)
  print(round(summary(reg)$adj.r.squared,3))
  
  
  #wcm
  mult_nls_1 <- nls(ALOS_ref_1 ~ a*exp(-c*ALS_1)+b*(1-exp(-c*ALS_1)), 
                    start = list(a=-25, b=-12, c=6e-5))
  ALOS_pred_1 <- coef(mult_nls_1)[1]*exp(-coef(mult_nls_1)[3]*ALS_1)+
    coef(mult_nls_1)[2]*(1-exp(-coef(mult_nls_1)[3]*ALS_1))
  
  mult_nls_2 <- nls(ALOS_ref_2 ~ a*exp(-c*ALS_2)+b*(1-exp(-c*ALS_2)), 
                    start = list(a=-25, b=-12, c=6e-5))
  ALOS_pred_2 <- coef(mult_nls_2)[1]*exp(-coef(mult_nls_2)[3]*ALS_2)+
    coef(mult_nls_2)[2]*(1-exp(-coef(mult_nls_2)[3]*ALS_2))
  
  ALOS_pred_diff <- ALOS_pred_1 - ALOS_pred_2
  
  reg <- lm(ALOS_ref_difred_diff)
  print(round(summary(reg)$adj.r.squared,3))
  
  
}

change_volume_stats("2010_HV","2008_HV7")

change_cover_stats <- function(date1,date2){
  dir <- "E:\\ChangMap\\CHM\\DB_csv\\Individual"
  Locdir1 <- file.path(dir, paste0("merge_",date1,"_cover.csv"))
  Locdir2 <- file.path(dir, paste0("merge_",date2,"_cover.csv"))
  print(Locdir1)
  print(Locdir2)
  Data1 <-  read.csv(Locdir1,header=T)
  Data2 <-  read.csv(Locdir2,header=T)
  total <- merge(Data1,Data2,by=c("ORIG_FID"))
  total <- total[total$C_c_sum5.x > 0 & total$C_c_sum5.y > 0,]
  ALS_1 <- total$C_c_sum5.x
  ALS_2 <- total$C_c_sum5.y
  
  ALOS_ref_1 <- total$CA_o_mean5.x
  ALOS_ref_2 <- total$CA_o_mean5.y
  ALOS_ref_diff <- ALOS_ref_1 - ALOS_ref_2
  
  #linear
  reg1 <- lm(ALOS_ref_1 ~ ALS_1, data = total)
  ALOS_pred_1 <- predict(reg1)
  reg2 <- lm(ALOS_ref_2 ~ ALS_2, data = total)
  ALOS_pred_2 <- predict(reg2)
  ALOS_pred_diff <- ALOS_pred_1 - ALOS_pred_2
  
  reg <- lm(ALOS_ref_diff ~ ALOS_pred_diff)
  print(round(summary(reg)$adj.r.squared,3))
  
  #log
  reg1 <- lm(ALOS_ref_1 ~ log2(ALS_1), data = total)
  ALOS_pred_1 <- predict(reg1)
  reg2 <- lm(ALOS_ref_2 ~ log2(ALS_2), data = total)
  ALOS_pred_2 <- predict(reg2)
  ALOS_pred_diff <- ALOS_pred_1 - ALOS_pred_2
  
  reg <- lm(ALOS_ref_diff ~ ALOS_pred_diff)
  print(round(summary(reg)$adj.r.squared,3))
  
  
  #wcm
  mult_nls_1 <- nls(ALOS_ref_1 ~ a*exp(-c*ALS_1)+b*(1-exp(-c*ALS_1)), 
                    start = list(a=-25, b=-12, c=2))
  ALOS_pred_1 <- coef(mult_nls_1)[1]*exp(-coef(mult_nls_1)[3]*ALS_1)+
    coef(mult_nls_1)[2]*(1-exp(-coef(mult_nls_1)[3]*ALS_1))
  
  mult_nls_2 <- nls(ALOS_ref_2 ~ a*exp(-c*ALS_2)+b*(1-exp(-c*ALS_2)), 
                    start = list(a=-25, b=-12, c=2))
  ALOS_pred_2 <- coef(mult_nls_2)[1]*exp(-coef(mult_nls_2)[3]*ALS_2)+
    coef(mult_nls_2)[2]*(1-exp(-coef(mult_nls_2)[3]*ALS_2))
  
  ALOS_pred_diff <- ALOS_pred_1 - ALOS_pred_2
  
  reg <- lm(ALOS_ref_diff ~ ALOS_pred_diff)
  print(round(summary(reg)$adj.r.squared,3))
  
  
}

change_cover_stats("2010_HV","2008_HV7")



#change multivariate

multi_volume_stats <- function(date1,date2,date3,date4){
  dir <- "E:\\ChangMap\\CHM\\DB_csv\\Individual"
  Locdir1 <- file.path(dir, paste0("merge_",date1,"_volume.csv"))
  Locdir2 <- file.path(dir, paste0("merge_",date2,"_volume.csv"))
  Locdir3 <- file.path(dir, paste0("merge_",date3,"_volume.csv"))
  Locdir4 <- file.path(dir, paste0("merge_",date4,"_volume.csv"))
  print(Locdir1)
  print(Locdir2)
  print(Locdir3)
  print(Locdir4)
  Data1 = read.csv(Locdir1,header=T)
  Data2 = read.csv(Locdir2,header=T)
  
  Data3 = read.csv(Locdir3,header=T)
  Data4 = read.csv(Locdir4,header=T)
  
  total1 <- merge(Data1,Data2,by=c("ORIG_FID"))
  reg3 <- lm(C_s_sum5.x  ~ CA_o_mean5.x+CA_o_mean5.y, data = total1)
  total1$ALS_ref_1 <- total1$C_s_sum5.x
  total1$ALS_pred_1 <- predict(reg3)
  
  total2 <- merge(Data3,Data4,by=c("ORIG_FID"))
  reg4 <- lm(C_s_sum5.x  ~ CA_o_mean5.x+CA_o_mean5.y, data = total2)
  total2$ALS_ref_2 <- total2$C_s_sum5.x
  total2$ALS_pred_2 <- predict(reg4)
  
  total <- merge(total1,total2,by=c("ORIG_FID"))
  total$ALS_ref_diff <- total$ALS_ref_1 - total$ALS_ref_2
  total$ALS_pred_diff <- total$ALS_pred_1 - total$ALS_pred_2
  
  reg <- lm(ALS_ref_diff ~ ALS_pred_diff, data = total)
  print(round(summary(reg)$adj.r.squared,3))
  
}

multi_volume_stats("2010_HV","2010_HH","2008_HV","2008_HH")


multi_cover_stats <- function(date1,date2,date3,date4){
  dir <- "E:\\ChangMap\\CHM\\DB_csv\\Individual"
  Locdir1 <- file.path(dir, paste0("merge_",date1,"_cover.csv"))
  Locdir2 <- file.path(dir, paste0("merge_",date2,"_cover.csv"))
  Locdir3 <- file.path(dir, paste0("merge_",date3,"_cover.csv"))
  Locdir4 <- file.path(dir, paste0("merge_",date4,"_cover.csv"))
  print(Locdir1)
  print(Locdir2)
  print(Locdir3)
  print(Locdir4)
  Data1 = read.csv(Locdir1,header=T)
  Data2 = read.csv(Locdir2,header=T)
  
  Data3 = read.csv(Locdir3,header=T)
  Data4 = read.csv(Locdir4,header=T)
  
  total1 <- merge(Data1,Data2,by=c("ORIG_FID"))
  reg3 <- lm(C_c_sum5.x  ~ CA_o_mean5.x+CA_o_mean5.y, data = total1)
  total1$ALS_ref_1 <- total1$C_c_sum5.x
  total1$ALS_pred_1 <- predict(reg3)
  
  total2 <- merge(Data3,Data4,by=c("ORIG_FID"))
  reg4 <- lm(C_c_sum5.x  ~ CA_o_mean5.x+CA_o_mean5.y, data = total2)
  total2$ALS_ref_2 <- total2$C_c_sum5.x
  total2$ALS_pred_2 <- predict(reg4)
  
  total <- merge(total1,total2,by=c("ORIG_FID"))
  total$ALS_ref_diff <- total$ALS_ref_1 - total$ALS_ref_2
  total$ALS_pred_diff <- total$ALS_pred_1 - total$ALS_pred_2
  
  reg <- lm(ALS_ref_diff ~ ALS_pred_diff, data = total)
  print(round(summary(reg)$adj.r.squared,3))
  
}

multi_cover_stats("2018_HV7","2018_HH7","2008_HV7","2008_HH7")



#Do basic linear model of backscatter change vs. cover change or Volume
dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\2018-2008.csv"
df <-  read.csv(dir,header=T)
reg <- lm(X2018.2008.HV7~Volume,data = df)
reg <- lm(X2018.2008.HV7~Cover,data = df)
print(round(summary(reg)$adj.r.squared,3))

dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\2018-2010.csv"
df <-  read.csv(dir,header=T)
reg <- lm(X2018.2010.HV~Volume,data = df)
reg <- lm(X2018.2010.HV~Cover,data = df)
print(round(summary(reg)$adj.r.squared,3))

dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\2010-2008.csv"
df <-  read.csv(dir,header=T)
reg <- lm(X2010.2008.HV~Volume,data = df)
reg <- lm(X2010.2008.HV~Cover,data = df)
print(round(summary(reg)$adj.r.squared,3))



#Do T-test on just on back scatter change for different bins.
file <- "2018-2010.csv"
dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result"
i <- file.path(dir,paste0(file))
Data = read.csv(i,header=T)

Data$SAR <- Data$X2018.2010.HV
polar <- "HV"
Data$ALS <- Data$Volume
VC <- "Volume"
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
Data$Type[Data$SAR >= -4 & Data$SAR < -5] = "-5~-4"

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




ggplot(Data, aes(x=SAR, y=ALS, group = Type))+
  geom_boxplot(fill="grey70", fatten = 10,color = "black",outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single")) +
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  theme_bw()+
  scale_x_continuous(breaks = seq(-5, 5, 1))+
  ylim(ymin, ymax)+
  theme(text=element_text(size=20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  labs(title = paste0(strsplit(basename(i),'.csv')[[1]],"_",polar,"_",VC),
       y = paste0("LiDAR CHM " ,VC," change"),
       x = paste0("ALOS backscatter change")) +
  theme(plot.title = element_text(hjust = 0.5))


ggsave(out, height=15, width=15, dpi=300)



#log model to map ---- 2018 2008
dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\2018_2008_formap.csv"
Data = read.csv(dir,header=T)

Data <- Data[Data$V_2018 > 0 & Data$V_2008 > 0,]

Data$SAR <- Data$X2018_HV
Data$ALS <- Data$V_2018
reg <- lm(ALS ~ exp(SAR), data = Data)
print(round(summary(reg)$adj.r.squared,3))
Data$SAR_2018 <- predict(reg)


Data$SAR <- Data$X2008_HV
Data$ALS <- Data$V_2008
reg <- lm(ALS ~ exp(SAR), data = Data)
print(round(summary(reg)$adj.r.squared,3))
Data$SAR_2008 <- predict(reg)

Data$SAR <- Data$X2018_HV7
Data$ALS <- Data$V_2018
reg <- lm(ALS ~ exp(SAR), data = Data)
print(round(summary(reg)$adj.r.squared,3))
Data$SAR_2017 <- predict(reg)

Data$SAR <- Data$X2008_HV7
Data$ALS <- Data$V_2008
reg <- lm(ALS ~ exp(SAR), data = Data)
print(round(summary(reg)$adj.r.squared,3))
Data$SAR_2007 <- predict(reg)

Data$Ref_VC_18_08 <- Data$V_2018 - Data$V_2008
Data$Pre_VC_18_08 <- Data$SAR_2018 - Data$SAR_2008
Data$Pre_VC_17_07 <- Data$SAR_2017 - Data$SAR_2007
Data <- subset(Data, select = c(ORIG_FID,Ref_VC_18_08,
                                Pre_VC_18_08,Pre_VC_17_07))

write.csv(Data, file="E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\change_2018-2008_pred_log.csv")

reg <- lm(Ref_VC_18_08 ~ Pre_VC_18_08, data = Data)
print(round(summary(reg)$adj.r.squared,3))
Data$SAR_2007 <- predict(reg)



#linear model to map -- 2018 2008
dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\2018_2008_formap.csv"
Data = read.csv(dir,header=T)

Data <- Data[Data$V_2018 > 0 & Data$V_2008 > 0,]

Data$SAR <- Data$X2018_HV
Data$ALS <- Data$V_2018
reg <- lm(ALS ~ SAR, data = Data)
print(round(summary(reg)$adj.r.squared,3))
print(paste0("2018 equation: Volume = SAR*",coef(reg)[2]," + ",coef(reg)[1]))
Data$SAR_2018 <- predict(reg)

Data$SAR <- Data$X2008_HV
Data$ALS <- Data$V_2008
reg <- lm(ALS ~ SAR, data = Data)
print(round(summary(reg)$adj.r.squared,3))
print(paste0("2008 equation: Volume = SAR*",coef(reg)[2]," + ",coef(reg)[1]))
Data$SAR_2008 <- predict(reg)

Data$SAR <- Data$X2018_HV7
Data$ALS <- Data$V_2018
reg <- lm(ALS ~ SAR, data = Data)
print(round(summary(reg)$adj.r.squared,3))
print(paste0("2017 equation: Volume = SAR*",coef(reg)[2]," + ",coef(reg)[1]))
Data$SAR_2017 <- predict(reg)

Data$SAR <- Data$X2008_HV7
Data$ALS <- Data$V_2008
reg <- lm(ALS ~ SAR, data = Data)
print(round(summary(reg)$adj.r.squared,3))
print(paste0("2007 equation: Volume = SAR*",coef(reg)[2]," + ",coef(reg)[1]))
Data$SAR_2007 <- predict(reg)

Data$Ref_VC_18_08 <- Data$V_2018 - Data$V_2008
Data$Pre_VC_18_08 <- Data$SAR_2018 - Data$SAR_2008
Data$Pre_VC_17_07 <- Data$SAR_2017 - Data$SAR_2007
Data <- subset(Data, select = c(ORIG_FID,Ref_VC_18_08,Pre_VC_18_08,Pre_VC_17_07))
write.csv(Data, file="E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\change_2018-2008_pred_l.csv")



#linear model to map -- 2018 2010
dir <- "E:\\ChangMap\\CHM\\DB_20210819\\DB_result\\2008.csv"
Data = read.csv(dir,header=T)
Data <- na.omit(Data)
Data <- Data[Data$V_2018 > 0 & Data$V_2008 > 0,]

Data$SAR <- Data$X2018_HV
Data$ALS <- Data$V_2018
reg <- lm(ALS ~ SAR, data = Data)
print(round(summary(reg)$adj.r.squared,3))
print(paste0("2018 equation: Volume = SAR*",coef(reg)[2]," + ",coef(reg)[1]))
Data$SAR_2018 <- predict(reg)

Data$SAR <- Data$X2008_HV
Data$ALS <- Data$V_2008
reg <- lm(ALS ~ SAR, data = Data)
print(round(summary(reg)$adj.r.squared,3))
print(paste0("2010 equation: Volume = SAR*",coef(reg)[2]," + ",coef(reg)[1]))
Data$SAR_2008 <- predict(reg)


Data$Ref_VC_18_08 <- Data$V_2018 - Data$V_2008
Data$Pre_VC_18_08 <- Data$SAR_2018 - Data$SAR_2008
Data <- subset(Data, select = c(ORIG_FID,Ref_VC_18_08,Pre_VC_18_08))

write.csv(Data, file="E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\change_2018-2010_pred_l.csv")








#linear model to map -- 2018 2008 -- cover
dir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\2018_2008_formap.csv"
Data = read.csv(dir,header=T)

Data <- Data[Data$C_2018 > 0 & Data$C_2008 > 0,]

Data$SAR <- Data$X2018_HV
Data$ALS <- Data$C_2018
reg <- lm(ALS ~ SAR, data = Data)
print(round(summary(reg)$adj.r.squared,3))
print(paste0("2018 equation: Cover = SAR*",coef(reg)[2]," + ",coef(reg)[1]))
Data$SAR_2018 <- predict(reg)

Data$SAR <- Data$X2008_HV
Data$ALS <- Data$C_2008
reg <- lm(ALS ~ SAR, data = Data)
print(round(summary(reg)$adj.r.squared,3))
print(paste0("2008 equation: Cover = SAR*",coef(reg)[2]," + ",coef(reg)[1]))
Data$SAR_2008 <- predict(reg)

Data$SAR <- Data$X2018_HV7
Data$ALS <- Data$C_2018
reg <- lm(ALS ~ SAR, data = Data)
print(round(summary(reg)$adj.r.squared,3))
print(paste0("2017 equation: Cover = SAR*",coef(reg)[2]," + ",coef(reg)[1]))
Data$SAR_2017 <- predict(reg)

Data$SAR <- Data$X2008_HV7
Data$ALS <- Data$C_2008
reg <- lm(ALS ~ SAR, data = Data)
print(round(summary(reg)$adj.r.squared,3))
print(paste0("2007 equation: Cover = SAR*",coef(reg)[2]," + ",coef(reg)[1]))
Data$SAR_2007 <- predict(reg)

Data$Ref_CC_18_08 <- Data$C_2018 - Data$C_2008
Data <- subset(Data, select = c(ORIG_FID,Ref_VC_18_08))
write.csv(Data, file="E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\changeC_2018-2008_pred_l.csv")





#thin process--R method
Locdir <- "E:\\ChangMap\\CHM\\thin_test\\tile_28920_-2718680.las"
las = readLAS(Locdir,select = "i,c,r")


las_sub = normalize_height(las, tin(),na.rm = TRUE)
chm <- grid_canopy(las_sub, 1, p2r())
out <- "E:\\ChangMap\\CHM\\thin_test\\CHM.tif"
writeRaster(chm,out,options=c('TFW=YES'),overwrite=TRUE)

las_1 = decimate_points(las, random(1))

las_sub_1 = normalize_height(las_1, tin(),na.rm = TRUE)
chm_1 <- grid_canopy(las_sub_1, 1, p2r())

las_pitfill <- filter_poi(las_sub_1, Z >= 3, Z <= 50)

mras <- mosaic(chm_1, chm_pitfill, fun = max)

out1 <- "E:\\ChangMap\\CHM\\thin_test\\CHM_1.tif"
writeRaster(mras,out1,options=c('TFW=YES'),overwrite=TRUE)

out2 <- "E:\\ChangMap\\CHM\\thin_test\\CHM_1_nofill.tif"
writeRaster(chm_1,out2,options=c('TFW=YES'),overwrite=TRUE)




#thin process--CC method
Locdir <- "E:\\ChangMap\\CHM\\thin_test\\CC.las"
las = readLAS(Locdir,select = "i,c,r")


las_sub = normalize_height(las, tin(),na.rm = TRUE)
chm <- grid_canopy(las_sub, 1, p2r())

las_pitfill <- filter_poi(las_sub_1, Z >= 3, Z <= 50)
chm_pitfill <- grid_canopy(las_pitfill, 1, p2r(subcircle = 1))

mras <- mosaic(chm, chm_pitfill, fun = max)

out1 <- "E:\\ChangMap\\CHM\\thin_test\\CHM_CC_1.tif"
writeRaster(mras,out1,options=c('TFW=YES'),overwrite=TRUE)


out2 <- "E:\\ChangMap\\CHM\\thin_test\\CHM_CC_1_nofill.tif"
writeRaster(chm,out2,options=c('TFW=YES'),overwrite=TRUE)

#cover
library(raster)
out <- "E:\\ChangMap\\CHM\\thin_test\\cover"
Locdir <- "E:\\ChangMap\\CHM\\thin_test"
chms <- list.files(path = Locdir, full.names = TRUE, pattern = "*.tif$")
for (chm in chms){
  print(chm)
  r <- raster(chm)
  r[r < 1.5] <- 0
  r[r >= 1.5] <- 1
  writeRaster(r,file.path(out,paste0("cover_",basename(chm))),options=c('TFW=YES'),overwrite=TRUE)
}




#histogram
raster_1 = "E:\\ChangMap\\CHM\\thin_test\\CHM.tif"
raster_2 = "E:\\ChangMap\\CHM\\thin_test\\CHM_1_nofill.tif"
raster_3 = "E:\\ChangMap\\CHM\\thin_test\\CHM_CC_1_nofill.tif"

raster1 <- raster(raster_1)
raster2 <- raster(raster_2)
raster3 <- raster(raster_3)


f <- hist(raster1, maxpixels=10000000,breaks=30,plot=F)
dat <- data.frame(counts= f$counts,breaks = f$mids)
p1<- ggplot(dat, aes(x = breaks, y = counts)) + 
  geom_bar(stat = "identity",color =  "white",fill = "blue",alpha = 0.2)+
  scale_x_continuous(limits = c(0, 7))+
  theme_bw()+
  labs(x="Original Height (m)", 
       y="Frequency")+
  theme(text=element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



f <- hist(raster3, maxpixels=10000000,breaks=30,plot=F)
dat <- data.frame(counts= f$counts,breaks = f$mids)
p2<- ggplot(dat, aes(x = breaks, y = counts)) + 
  geom_bar(stat = "identity",color =  "white",fill = "blue",alpha = 0.2)+
  scale_x_continuous(limits = c(0, 7))+
  theme_bw()+
  labs(x="CC Height (m)", 
       y="Frequency")+
  theme(text=element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())




f <- hist(raster2, maxpixels=10000000,breaks=30,plot=F)
dat <- data.frame(counts= f$counts,breaks = f$mids)
p3<- ggplot(dat, aes(x = breaks, y = counts)) + 
  geom_bar(stat = "identity",color =  "white",fill = "blue",alpha = 0.2)+
  scale_x_continuous(limits = c(0, 7))+
  theme_bw()+
  labs(x="R Height (m)", 
       y="Frequency")+
  theme(text=element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



filedir <- "E:\\ChangMap\\CHM\\thin_test\\Result.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)
Data <- Data[Data$VS_5 >= -99,]
Data$CS_5 <- Data$CS_5/5250
Data$C1S_5 <- Data$C1S_5/5250
Data$CCCS_5 <- Data$CCCS_5/5250
Data$C1SN_5 <- Data$C1SN_5/5250
Data$CCCSN_5 <- Data$CCCSN_5/5250


p4<- ggplot(Data, aes(VS_5)) +
  theme_bw()+
  coord_cartesian(ylim = c(0, 300))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(color =  "white",fill = "blue",bins = 30,alpha = 0.2) +
  labs(x="Original Volume", 
       y="Frequency")+
  scale_x_continuous(limits = c(0, 15000))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))



p5<- ggplot(Data, aes(VCCSN_5)) +
  theme_bw()+
  coord_cartesian(ylim = c(0, 300))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(color =  "white",fill = "blue",bins = 30,alpha = 0.2) +
  labs(x="CC Volume", 
       y="Frequency")+
  scale_x_continuous(limits = c(0, 15000))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))


p6<- ggplot(Data, aes(V1SN_5)) +
  theme_bw()+
  coord_cartesian(ylim = c(0, 300))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(color =  "white",fill = "blue",bins = 30,alpha = 0.2) +
  labs(x="R Volume", 
       y="Frequency")+
  scale_x_continuous(limits = c(0, 15000))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))

p7<- ggplot(Data, aes(CS_5)) +
  theme_bw()+
  coord_cartesian(ylim = c(0, 200))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(color =  "white",fill = "blue",bins = 30,alpha = 0.2) +
  labs(x="Original Canopy cover", 
       y="Frequency")+
  scale_x_continuous(breaks = seq(0, 0.8, 0.1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))


p8<- ggplot(Data, aes(CCCSN_5)) +
  theme_bw()+
  coord_cartesian(ylim = c(0, 200))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(color =  "white",fill = "blue",bins = 30,alpha = 0.2) +
  labs(x="CC Canopy cover", 
       y="Frequency")+
  scale_x_continuous(breaks = seq(0, 0.8, 0.1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))


p9<- ggplot(Data, aes(C1SN_5)) +
  theme_bw()+
  coord_cartesian(ylim = c(0, 200))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(color =  "white",fill = "blue",bins = 30,alpha = 0.2) +
  labs(x="R Canopy cover", 
       y="Frequency")+
  scale_x_continuous(breaks = seq(0, 0.8, 0.1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))


ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,ncol=3,nrow=3)
out <- "E:\\ChangMap\\CHM\\thin_test\\NO_pitfree.jpg"
ggsave(out,height=12, width=18, dpi=600)









par(mfrow=c(3,3),cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2) 

h<-hist(vector1[,3], plot=F)
h$counts <- h$counts/sum(h$counts)
plot(h, ylim = c(0,0.6), main = "Original",
     xlab = "Height (m)",ylab="%frequency",
     xlim = c(-1,6),col=scales::alpha('green',.99),freq=TRUE)

h<-hist(vector3[,3], plot=F)
h$counts <- h$counts/sum(h$counts)
plot(h,ylim = c(0,0.6), main = "CC method without pitfilling",
     xlab = "Height (m)",ylab="%frequency",
     xlim = c(-1,6),col=scales::alpha('orange',.70),freq=TRUE)

h<-hist(vector2[,3], plot=F)
h$counts <- h$counts/sum(h$counts)
plot(h,ylim = c(0,0.6), main = "R method without pitfilling",
     xlab = "Height (m)",ylab="%frequency",
     xlim = c(-1,6),col=scales::alpha('blue',.99),freq=TRUE)





h<-hist(Data$VS_5, plot=F)
h$counts <- h$counts/sum(h$counts)
plot(h, ylim = c(0,0.3), 
     xlab = "Volume",ylab="%frequency",main = "Original",
     xlim = c(0,20000),col=scales::alpha('green',.99),freq=TRUE)

h<-hist(Data$VCCSN_5, plot=F)
h$counts <- h$counts/sum(h$counts)
plot(h, ylim = c(0,0.3), 
     xlab = "Volume",ylab="%frequency",main = "CC method without pitfilling (>3m)",
     xlim = c(0,20000),col=scales::alpha('orange',.99),freq=TRUE)


h<-hist(Data$V1SN_5, plot=F)
h$counts <- h$counts/sum(h$counts)
plot(h, ylim = c(0,0.3),
     xlab = "Volume",ylab="%frequency",main = "R method without pitfilling (>3m)",
     xlim = c(0,20000),col=scales::alpha('blue',.99),freq=TRUE)



h<-hist(Data$CS_5, plot=F)
h$counts <- h$counts/sum(h$counts)
plot(h, ylim = c(0,0.2),
     xlab = "Canopy cover",ylab="%frequency",main = "Original",
     col=scales::alpha('green',.99),freq=TRUE)


h<-hist(Data$CCCSN_5, plot=F)
h$counts <- h$counts/sum(h$counts)
plot(h, ylim = c(0,0.2), 
     xlab = "Canopy cover",ylab="%frequency",main = "CC method without pitfilling (>3m)",
     xlim = c(0,0.8),col=scales::alpha('orange',.99),freq=TRUE)

h<-hist(Data$C1SN_5, plot=F)
h$counts <- h$counts/sum(h$counts)
plot(h, ylim = c(0,0.2), 
     xlab = "Canopy cover",ylab="%frequency",main = "R method without pitfilling (>3m)",
     xlim = c(0,0.8),col=scales::alpha('blue',.99),freq=TRUE)


#bulk thin process--CC method for 2018 data
Site <- "Agincourt2008"
Locdir <- paste0("E:\\ChangMap\\CHM\\LiDAR_archive\\",Site)
pattern = '.las'
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
for (i in files){
  print(i)
  las = readLAS(i,select = "i,c,r")
  las_sub = normalize_height(las, tin(),na.rm = TRUE)
  chm <- grid_canopy(las_sub, 1, p2r())
  
  out1 <- file.path("E:\\ChangMap\\CHM\\DB_20210819\\DB_chm_2018",Site,paste0(
                 strsplit(basename(i),'.las')[[1]],".tif"))
  writeRaster(chm,out1,options=c('TFW=YES'),overwrite=TRUE)
  
  #out2 <- "E:\\ChangMap\\CHM\\thin_test\\CHM_CC_1_nofill.tif"
  #writeRaster(chm,out2,options=c('TFW=YES'),overwrite=TRUE)
  
}


#cover generation for all CHMs
out <- "E:\\ChangMap\\CHM\\DB_20210819\\DB_cover"
Locdir <- "E:\\ChangMap\\CHM\\DB_20210819\\DB_chm"
chms <- list.files(path = Locdir, full.names = TRUE, pattern = "*.tif$")
for (chm in chms){
  print(chm)
  r <- raster(chm)
  r[r < 1.5] <- 0
  r[r >= 1.5] <- 1
  writeRaster(r,file.path(out,paste0("cover_",basename(chm))),options=c('TFW=YES'),overwrite=TRUE)
}



###concat 2008
filedir1 <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\Agincourt.csv"
filedir2 <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\Ireagh.csv"
filedir3 <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\Justicia.csv"
Data1 = read.csv(filedir1,header=T)
Data2 = read.csv(filedir2,header=T)
Data3 = read.csv(filedir3,header=T)
Data1 <- subset( Data1, select = c(ORIG_FID,V_2008,C_2008,X2008_HH,X2008_HV,X2008_HH7,X2008_HV7) )
Data2 <- subset( Data2, select = c(ORIG_FID,V_2008,C_2008,X2008_HH,X2008_HV,X2008_HH7,X2008_HV7) )
Data3 <- subset( Data3, select = c(ORIG_FID,V_2008,C_2008,X2008_HH,X2008_HV,X2008_HH7,X2008_HV7) )
new <- rbind(Data1,Data2,Data3)
write.csv(new, file="E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\merge2008.csv")

###concat 2010
filedir1 <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\WelAndover.csv"
filedir2 <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\Welverdiendt.csv"
filedir3 <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\Justicia.csv"
Data1 = read.csv(filedir1,header=T)
Data2 = read.csv(filedir2,header=T)
Data3 = read.csv(filedir3,header=T)
Data1 <- subset( Data1, select = c(ORIG_FID,V_2010,C_2010,X2010_HH,X2010_HV) )
Data2 <- subset( Data2, select = c(ORIG_FID,V_2010,C_2010,X2010_HH,X2010_HV) )
Data3 <- subset( Data3, select = c(ORIG_FID,V_2010,C_2010,X2010_HH,X2010_HV) )
new <- rbind(Data1,Data2,Data3)
write.csv(new, file="E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\merge2010.csv")

###concat 2018
filedir1 <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\Agincourt.csv"
filedir2 <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\Ireagh.csv"
filedir3 <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\Justicia.csv"
filedir4 <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\Agincourt.csv"
filedir5 <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\Ireagh.csv"
Data1 = read.csv(filedir1,header=T)
Data2 = read.csv(filedir2,header=T)
Data3 = read.csv(filedir3,header=T)
Data4 = read.csv(filedir4,header=T)
Data5 = read.csv(filedir5,header=T)
Data1 <- subset( Data1, select = c(ORIG_FID,V_2018,C_2018,X2018_HH,X2018_HV,X2018_HH7,X2018_HV7) )
Data2 <- subset( Data2, select = c(ORIG_FID,V_2018,C_2018,X2018_HH,X2018_HV,X2018_HH7,X2018_HV7) )
Data3 <- subset( Data3, select = c(ORIG_FID,V_2018,C_2018,X2018_HH,X2018_HV,X2018_HH7,X2018_HV7) )
Data4 <- subset( Data4, select = c(ORIG_FID,V_2018,C_2018,X2018_HH,X2018_HV,X2018_HH7,X2018_HV7) )
Data5 <- subset( Data5, select = c(ORIG_FID,V_2018,C_2018,X2018_HH,X2018_HV,X2018_HH7,X2018_HV7) )

new <- rbind(Data1,Data2,Data3,Data4,Data5)
write.csv(new, file="E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\merge2018.csv")


#VOLUME plots for new results---20210818
filedir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\Agincourt_result.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)
Data <- Data[Data$V_2018 >= -99,]


p1<- ggplot(Data, aes(V_2008)) +
  theme_bw()+
  ggtitle("Agincourt Volume 2008") +
  coord_cartesian(ylim = c(0,11000))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(color =  "white",fill = "blue",bins = 30,alpha = 0.2) +
  labs(x="Volume", 
       y="Frequency")+
  scale_x_continuous(limits = c(0, 40000))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))

p2<- ggplot(Data, aes(V_2010)) +
  theme_bw()+
  ggtitle("Welverdiendt Volume 2010") +
  coord_cartesian(ylim = c(0, 3000))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(color =  "white",fill = "blue",bins = 30,alpha = 0.2) +
  labs(x="Volume", 
       y="Frequency")+
  scale_x_continuous(limits = c(0, 15000))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))


p3<- ggplot(Data, aes(V_2018)) +
  theme_bw()+
  ggtitle("Agincourt Volume 2018") +
  coord_cartesian(ylim = c(0, 11000))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(color =  "white",fill = "blue",bins = 30,alpha = 0.2) +
  labs(x="Volume", 
       y="Frequency")+
  scale_x_continuous(limits = c(0, 40000))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))




ggarrange(p1,p3,ncol=3,nrow=1)

out <- "E:\\ChangMap\\CHM\\DB_20210818\\Figure\\Agincourt.jpg"
ggsave(out,height=12, width=18, dpi=600)



#COVER plots for new results---20210818
filedir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result\\Agincourt_result.csv"
Data = read.csv(filedir,header=T)
Data <- na.omit(Data)
Data <- Data[Data$V_2018 >= -99,]



p1<- ggplot(Data, aes(C_2008)) +
  theme_bw()+
  ggtitle("Agincourt Cover 2008") +
  coord_cartesian(ylim = c(0, 10000))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(color =  "white",fill = "blue",bins = 30,alpha = 0.2) +
  labs(x="Volume", 
       y="Frequency")+
  scale_x_continuous(limits = c(0, 1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))

p2<- ggplot(Data, aes(C_2010)) +
  theme_bw()+
  ggtitle("Agincourt Cover 2010") +
  coord_cartesian(ylim = c(0, 23000))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(color =  "white",fill = "blue",bins = 30,alpha = 0.2) +
  labs(x="Volume", 
       y="Frequency")+
  scale_x_continuous(limits = c(0, 1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))


p3<- ggplot(Data, aes(C_2018)) +
  theme_bw()+
  ggtitle("Agincourt Cover 2018") +
  coord_cartesian(ylim = c(0, 10000))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(color =  "white",fill = "blue",bins = 30,alpha = 0.2) +
  labs(x="Volume", 
       y="Frequency")+
  scale_x_continuous(limits = c(0, 1))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p3,ncol=3,nrow=1)

out <- "E:\\ChangMap\\CHM\\DB_20210818\\Figure\\Agincourt_cover.jpg"
ggsave(out,height=12, width=18, dpi=600)



#scatterplot -- volume individual
Locdir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result"

files <- list.files(path = Locdir, full.names = TRUE, pattern = "csv")
for (i in files){
  try({
    print(i)
    Data = read.csv(i,header=T)
    Data <- na.omit(Data)
    Data <- Data[Data$V_2018 > 0,]
    colnames(Data)
    x <- Data$V_2018
    y <- Data$X2018_HH7
    title <- paste0(sub('\\.csv$', '', basename(i)),"2018_HH7.jpg")
    out <- file.path("E:\\ChangMap\\CHM\\DB_20210818\\Figure\\scatterplot",title)
    mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                    start = list(a=-25, b=-12, c=6e-5))
    y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
      coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
    R2 <- round(cor(y,y_sim)^2,3)
    round(cor(y,y_sim)^2,3)
    
    reg1 <- lm(y ~ x , data = Data)
    reg3 <- lm(y ~ log(x), data = Data)
    
    ggplot(Data, aes(x=x, y=y))+ 
      ylim(min(y), 0)+
      geom_pointdensity()+
      scale_color_viridis(direction = 1)+
      geom_smooth(method = "nls", 
                  method.args = list(formula = y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                                     start = list(a=-21.96345, b=-10.31212, c=6e-5)),
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
        formula = 'y ~ log2(x)', 
        se= F, 
        size = 1.5, 
        linetype = "solid",
        colour = "blue")+
      annotate("text", x=min(x), y=0, hjust = 0,color="black",size = 10,
               label= paste(" Water cloud model R2: ",round(R2,3))) + 
      annotate("text", x=min(x), y=-2,hjust = 0,color="purple",size = 10,
               label= paste(" Linear model R2: ",round(summary(reg1)$adj.r.squared,3))) + 
      annotate("text", x=min(x), y=-1,hjust = 0,color="blue",size = 10,
               label= paste(" Logarithmic model R2: ",round(summary(reg3)$adj.r.squared,3))) + 
      labs(title=title,
           x = paste0("LiDAR CHM volume"),
           y = paste0("ALOS backscatter")) +
      theme(text=element_text(size=20)) +
      theme(plot.title = element_text(hjust = 0.5))
    ggsave(out, height=15, width=15, dpi=300)
    
  })
  
  
}




#scatterplot -- CC individual
Locdir <- "E:\\ChangMap\\CHM\\DB_20210818\\DB_result"

files <- list.files(path = Locdir, full.names = TRUE, pattern = "merge")
for (i in files){
  try({
    print(i)
    
    Data = read.csv(i,header=T)
    Data <- na.omit(Data)
    Data <- Data[Data$C_2010 > 0,]
    Data <- Data[Data$X2010_HH < -1,]
    colnames(Data)
    x <- Data$C_2010
    y <- Data$X2010_HH
    title <- paste0(sub('\\.csv$', '', basename(i)),"2010_HH.jpg")
    out <- file.path("E:\\ChangMap\\CHM\\DB_20210818\\Figure\\scatterplot_cc",title)
    mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                    start = list(a=-25, b=-12, c=2))
    y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
      coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
    R2 <- round(cor(y,y_sim)^2,3)
    round(cor(y,y_sim)^2,3)
    
    reg1 <- lm(y ~ x , data = Data)
    reg3 <- lm(y ~ log(x), data = Data)
    
    ggplot(Data, aes(x=x, y=y))+
      ylim(min(y), 0)+
      geom_pointdensity()+
      scale_color_viridis(direction = 1)+
      geom_smooth(method = "nls", 
                  method.args = list(formula = y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                                     start = list(a=-21.96345, b=-10.31212, c=2)),
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
        formula = 'y ~ log2(x)', 
        se= F, 
        size = 1.5, 
        linetype = "solid",
        colour = "blue")+
      annotate("text", x=0, y=0, hjust = 0,color="black",size = 10,
               label= paste(" Water cloud model R2: ",round(R2,3))) + 
      annotate("text", x=0, y=-2,hjust = 0,color="purple",size = 10,
               label= paste(" Linear model R2: ",round(summary(reg1)$adj.r.squared,3))) + 
      annotate("text", x=0, y=-1,hjust = 0,color="blue",size = 10,
               label= paste(" Logarithmic model R2: ",round(summary(reg3)$adj.r.squared,3))) + 
      labs(title=title,
           x = paste0("LiDAR CHM cover"),
           y = paste0("ALOS backscatter")) +
      theme(text=element_text(size=20)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(out, height=15, width=15, dpi=300)
    
        
  })
  
  
}






#correlation--inverted log model for volume 
Locdir <- "E:\\ChangMap\\CHM\\DB_20210818\DB_result"
pattern = 'cover.csv'
files <- list.files(path = Locdir, full.names = TRUE, pattern = pattern)
print(i)
Data = read.csv(i,header=T)
Data <- na.omit(Data)
Data <- Data[Data$CC_ALOS_w <= 1 & Data$CC_ALOS_w >= 0,]
y <- Data$CC_ALOS_w
x <- Data$CC_ALS
reg1 <- lm(x  ~ y, data = Data)
pre_linear <- predict(reg1)
R2 <- round(cor(x,pre_linear)^2,3)

MD <- mean(pre_linear - x)
RB <- 100*MD/mean(x)
RMSE <- sqrt(mean((pre_linear - x)^2))
rRMSE <- 100*sqrt(mean((pre_linear - x)^2))/mean(x)
print(R2)



#VOLUME estimate for bayesian WCM
i <- "E:\\ChangMap\\CHM\\DB_20210819\\DB_result\\2018.csv"
Data = read.csv(i,header=T)

x <- Data$V
y<- Data$HH7

mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-25, b=-12, c=9e-5))
coef(mult_nls)[1]
coef(mult_nls)[2]
coef(mult_nls)[3]
max(Data$V)

y<- Data$HH
mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-25, b=-12, c=9e-5))
coef(mult_nls)[1]
coef(mult_nls)[2]
coef(mult_nls)[3]
max(Data$V)


#COVER estimate for bayesian WCM
i <- "E:\\ChangMap\\CHM\\DB_20210819\\DB_result\\2018.csv"
Data = read.csv(i,header=T)

x <- Data$C
y<- Data$HH7

mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-20, b=-7, c=2))
coef(mult_nls)[1]
coef(mult_nls)[2]
coef(mult_nls)[3]
max(x)
min(y)
max(y)

x <- Data$C
y<- Data$HH
mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
               start = list(a=-20, b=-7, c=2))
coef(mult_nls)[1]
coef(mult_nls)[2]
coef(mult_nls)[3]
max(x)
min(y)
max(y)




#linear model to map -- 2018 2010 -- All map
dir <- "E:\\ChangMap\\CHM\\DB_20210819\\DB_result\\2008.csv"
Data = read.csv(dir,header=T)
Data <- na.omit(Data)
Data <- Data[Data$V > 0 & Data$C > 0,]

reg <- lm(C ~ HV7, data = Data)
print(round(summary(reg)$adj.r.squared,3))
print(paste0("Volume HV equation: Volume = SAR*",coef(reg)[2]," + ",coef(reg)[1]))

reg <- lm(C ~ HH7, data = Data)
print(round(summary(reg)$adj.r.squared,3))
print(paste0("Volume HH equation: Volume = SAR*",coef(reg)[2]," + ",coef(reg)[1]))



#match 2008,2010,2018
dir1 <- "E:\\ChangMap\\CHM\\DB_20210819\\DB_csv\\2018_all.csv"
dir2 <- "E:\\ChangMap\\CHM\\DB_20210819\\DB_csv\\2010_all.csv"
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))

Data_2018_2008 = Data$C.x - Data$C.y
Data_2018_2008_l = Data$HV_l_c.x - Data$HV_l_c.y
Data_2018_2008_w = Data$HV_w_c.x - Data$HV_w_c.y

reg1 <- lm(Data_2018_2008 ~ Data_2018_2008_l)
print(round(summary(reg1)$adj.r.squared,3))

reg2 <- lm(Data_2018_2008 ~ Data_2018_2008_w)
print(round(summary(reg2)$adj.r.squared,3))



#stats individual model
dir <- "E:\\ChangMap\\CHM\\DB_20210819\\DB_csv\\2008_all.csv"
Data = read.csv(dir,header=T)

x <- Data$C
y<- Data$HH7
reg <- lm(y~x, Data)
print(round(summary(reg)$adj.r.squared,3))
y<- Data$HH
reg <- lm(y~x, Data)
print(round(summary(reg)$adj.r.squared,3))
y<- Data$HV7
reg <- lm(y~x, Data)
print(round(summary(reg)$adj.r.squared,3))
y<- Data$HV
reg <- lm(y~x, Data)
print(round(summary(reg)$adj.r.squared,3))


y<- Data$HH7
mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-20, b=-7, c=2))
y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
  coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
round(cor(y,y_sim)^2,3)
y<- Data$HH
mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-20, b=-7, c=2))
y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
  coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
round(cor(y,y_sim)^2,3)
y<- Data$HV7
mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-20, b=-7, c=2))
y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
  coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
round(cor(y,y_sim)^2,3)
y<- Data$HV
mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-20, b=-7, c=2))
y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
  coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
round(cor(y,y_sim)^2,3)


#stats individual INVERTED model
#stats individual model
dir <- "E:\\ChangMap\\CHM\\DB_20210819\\DB_csv\\2018_all.csv"
Data = read.csv(dir,header=T)
x <- Data$C

y<- Data$HV7_l_c
reg <- lm(y~x, Data)
print(round(summary(reg)$adj.r.squared,3))
pred <- predict(reg)
mean(pred - x)
100*mean(pred - x)/mean(x)
sqrt(mean((pred - x)^2))
100*sqrt(mean((pred - x)^2))/mean(x)

y<- Data$HV_l_c
reg <- lm(y~x, Data)
print(round(summary(reg)$adj.r.squared,3))
pred <- predict(reg)
mean(pred - x)
100*mean(pred - x)/mean(x)
sqrt(mean((pred - x)^2))
100*sqrt(mean((pred - x)^2))/mean(x)

y<- Data$HV7_w_c
reg <- lm(y~x, Data)
print(round(summary(reg)$adj.r.squared,3))
pred <- predict(reg)
mean(pred - x)
100*mean(pred - x)/mean(x)
sqrt(mean((pred - x)^2))
100*sqrt(mean((pred - x)^2))/mean(x)

y<- Data$HV_w_c
reg <- lm(y~x, Data)
print(round(summary(reg)$adj.r.squared,3))
pred <- predict(reg)
mean(pred - x)
100*mean(pred - x)/mean(x)
sqrt(mean((pred - x)^2))
100*sqrt(mean((pred - x)^2))/mean(x)




#bias boxplot cover
dir <- "E:\\ChangMap\\CHM\\DB_20210819\\DB_csv\\2018_all.csv"
out = "E:\\ChangMap\\CHM\\DB_20210819\\Figure\\figure bias cover 2018.jpg"
Data = read.csv(dir,header=T)
Data <- Data[Data$V > 0 & Data$C > 0,]
Data <- Data[Data$C < 0.8,]

x <- Data$C
y<- Data$HV_w_c
round(cor(y, x,method = "pearson")^2,3)
Data$diff <- y-x
breakbin = seq(0,0.9,0.1)
Data$group_RH <- cut(x,breaks = breakbin
                     ,dig.lab=1)
table(Data$group_RH)
p1<- ggplot(Data, aes(x=C, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(width = 1)+
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-0.5, 0.5))+
  scale_y_continuous(minor_breaks = seq(-0.5, 0.5, 0.1),breaks = seq(-0.5, 0.5, 0.1))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme_bw()+
  labs(title = "Bayesian WCM canopy cover",
    x = "ALS Canopy cover",y="Bias (m)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(plot.title = element_text(hjust = 0.5))


a <- tapply(Data$diff, cut(Data$C,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$C, cut(Data$C,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- seq(0,0.8,0.1)
l <- cbind(x,RB,table(Data$group_RH))
df <- data.frame(l)
df <- head(df,-1)
p3<- ggplot(df, aes(x=x+0.05, y=RB)) + 
  geom_bar(stat='identity')+
  geom_text(aes(label = V3,y=70), size = 6)+
  theme_bw()+
  coord_cartesian(ylim = c(-50, 70))+
  scale_y_continuous(minor_breaks = seq(-50, 70, 10),breaks = seq(-50, 70, 10))+
  scale_x_continuous(minor_breaks = seq(0,1,0.1),breaks = seq(0,1,0.1))+
  labs(x="ALS Canopy cover", 
       y="%Bias (%)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(plot.title = element_text(hjust = 0.5))


x <- Data$C
y<- Data$HV_l_c
Data$diff <- y-x
breakbin = seq(0,0.9,0.1)
Data$group_RH <- cut(x,breaks = breakbin
                     ,dig.lab=1)
p2<- ggplot(Data, aes(x=C, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(width = 1)+
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-0.5, 0.5))+
  scale_y_continuous(minor_breaks = seq(-0.5, 0.5, 0.1),breaks = seq(-0.5, 0.5, 0.1))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme_bw()+
  labs(title = "Inverted linear canopy cover",
       x = "ALS Canopy cover",y="Bias (m)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(plot.title = element_text(hjust = 0.5))


a <- tapply(Data$diff, cut(Data$C,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$C, cut(Data$C,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100
x <- seq(0,0.8,0.1)
l <- cbind(x,RB,table(Data$group_RH))
df <- data.frame(l)
df <- head(df,-1)
p4<- ggplot(df, aes(x=x+0.05, y=RB)) + 
  geom_bar(stat='identity')+
  geom_text(aes(label = V3,y=70), size = 6)+
  theme_bw()+
  coord_cartesian(ylim = c(-50, 70))+
  scale_y_continuous(minor_breaks = seq(-50, 70, 10),breaks = seq(-50, 70, 10))+
  scale_x_continuous(minor_breaks = seq(0,1,0.1),breaks = seq(0,1,0.1))+
  labs(x="ALS Canopy cover", 
       y="%Bias (%)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p2,p3,p4)

ggsave(out,height=12, width=24, dpi=600)



#bias boxplot volume
dir <- "E:\\ChangMap\\CHM\\DB_20210819\\DB_csv\\2018_all.csv"
out = "E:\\ChangMap\\CHM\\DB_20210819\\Figure\\figure bias volume 2018.jpg"
Data = read.csv(dir,header=T)
Data <- Data[Data$V > 0 & Data$C > 0,]
Data <- Data[Data$V < 30000,]

x <- Data$V
y<- Data$HV_w_v
Data$diff <- y-x
breakbin = seq(0,30000,3000)
Data$group_RH <- cut(x,breaks = breakbin
                     ,dig.lab=1)
p1<- ggplot(Data, aes(x=V, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(width = 1)+
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-25000,15000))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  scale_y_continuous(minor_breaks = seq(-25000,15000,3000),
                     breaks = seq(-25000,15000,3000))+
  theme_bw()+
  labs(title = "Bayesian WCM volume",
    x = "ALS volume",y="Bias (m)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(plot.title = element_text(hjust = 0.5))


a <- tapply(Data$diff, cut(Data$V,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$V, cut(Data$V,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100
x <- seq(0,27000,3000)
l <- cbind(x,RB,table(Data$group_RH))
df <- data.frame(l)
p3<-ggplot(df, aes(x=x+1500, y=RB)) + 
  geom_bar(stat='identity')+
  geom_text(aes(label = V3,y=70), size = 6)+
  theme_bw()+
  coord_cartesian(ylim = c(-70, 70))+
  scale_y_continuous(minor_breaks = seq(-70, 70, 10),breaks = seq(-70, 70, 10))+
  scale_x_continuous(minor_breaks = seq(0,30000,3000),breaks = seq(0,30000,3000))+
  labs(x="ALS Volume", 
       y="%Bias (%)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(plot.title = element_text(hjust = 0.5))

x <- Data$V
y<- Data$HV_l_v
Data$diff <- y-x
breakbin = breakbin
Data$group_RH <- cut(x,breaks = breakbin
                     ,dig.lab=1)
p2<- ggplot(Data, aes(x=V, y=diff, group=group_RH)) + 
  stat_boxplot(geom ='errorbar', width = 0.1) +
  geom_boxplot(width = 1)+
  geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
  coord_cartesian(ylim = c(-25000,15000))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  scale_y_continuous(minor_breaks = seq(-25000,15000,3000),
                     breaks = seq(-25000,15000,3000))+
  theme_bw()+
  labs(title = "Inverted linear volume",
    x = "ALS volume",y="Bias (m)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(plot.title = element_text(hjust = 0.5))

Data$group_RH <- cut(x,breaks = breakbin
                     ,dig.lab=1)
a <- tapply(Data$diff, cut(Data$V,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$V, cut(Data$V,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100
x <- seq(0,27000,3000)
l <- cbind(x,RB,table(Data$group_RH))
df <- data.frame(l)
p4<-ggplot(df, aes(x=x+1500, y=RB)) + 
  geom_bar(stat='identity')+
  geom_text(aes(label = V3,y=70), size = 6)+
  theme_bw()+
  coord_cartesian(ylim = c(-70, 70))+
  scale_y_continuous(minor_breaks = seq(-70, 70, 10),breaks = seq(-70, 70, 10))+
  scale_x_continuous(minor_breaks = seq(0,30000,3000),breaks = seq(0,30000,3000))+
  labs(x="ALS Volume", 
       y="%Bias (%)")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=25))+
  theme(plot.title = element_text(hjust = 0.5))


ggarrange(p1,p2,p3,p4)
ggsave(out,height=12, width=24, dpi=600)




#change canopy cover SAR ~ LIDAR
dir1 <- "E:\\ChangMap\\CHM\\DB_20210819\\DB_csv\\2018_all.csv"
dir2 <- "E:\\ChangMap\\CHM\\DB_20210819\\DB_csv\\2010_all.csv"
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C.y > 0 & Data$C.x > 0,]

Data$Data_2018_2008 = Data$C.x - Data$C.y
Data$Data_2018_2008_l = Data$HV_l_c.x - Data$HV_l_c.y
Data$Data_2018_2008_w = Data$HV_w_c.x - Data$HV_w_c.y

Data <- Data[Data$Data_2018_2008 > -0.6 & Data$Data_2018_2008 < 0.6,]

breakbin = round(seq(-0.6,0.6,0.1),2)
Data$group <- cut(Data$Data_2018_2008,breaks = breakbin,dig.lab=1)

p1<- ggplot(Data, aes(x=Data_2018_2008, y=Data_2018_2008_w, group = group)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(
    x = paste0("LiDAR cover change"),
    y = paste0("SAR cover change (Bayesian WCM)")) +
  theme_bw()+
  ylim(-0.4, 0.4)+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5))

p2<- ggplot(Data, aes(x=Data_2018_2008, y=Data_2018_2008_l, group = group)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(
    x = paste0("LiDAR cover change"),
    y = paste0("SAR cover change (Inverted linear)")) +
  theme_bw()+
  ylim(-0.4, 0.4)+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p2)
out = "E:\\ChangMap\\CHM\\DB_20210819\\Figure\\figure violin cover 2018-2010.jpg"
ggsave(out,height=12, width=24, dpi=600)





#change volume SAR ~ LIDAR
dir1 <- "E:\\ChangMap\\CHM\\DB_20210819\\DB_csv\\2010_all.csv"
dir2 <- "E:\\ChangMap\\CHM\\DB_20210819\\DB_csv\\2008_all.csv"
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$V.y > 0 & Data$V.x > 0,]

Data$Data_2018_2008 = Data$V.x - Data$V.y
Data$Data_2018_2008_l = Data$HV_l_v.x - Data$HV_l_v.y
Data$Data_2018_2008_w = Data$HV_w_v.x - Data$HV_w_v.y


breakbin = seq(-25000,15000,3000)
Data$group <- cut(Data$Data_2018_2008,breaks = breakbin,dig.lab=1)

p1<- ggplot(Data, aes(x=Data_2018_2008, y=Data_2018_2008_w, group = group)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(
    x = paste0("LiDAR volume change"),
    y = paste0("SAR volume change (Bayesian WCM)")) +
  theme_bw()+
  scale_y_continuous(minor_breaks = seq(-25000,15000,2000),
                     breaks = seq(-25000,15000,2000))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5))

p2<- ggplot(Data, aes(x=Data_2018_2008, y=Data_2018_2008_l, group = group)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(
    x = paste0("LiDAR volume change"),
    y = paste0("SAR volume change (Inverted linear)")) +
  theme_bw()+
  scale_y_continuous(minor_breaks = seq(-25000,15000,2000),
                     breaks = seq(-25000,15000,2000))+
  scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  theme(text=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p2)
out = "E:\\ChangMap\\CHM\\DB_20210819\\Figure\\figure violin volume 2010-2008.jpg"
ggsave(out,height=12, width=24, dpi=600)







