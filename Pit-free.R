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
library(ggplot2)
library(raster)




path = "E:/temp/Konrad/CHM/20200616/lidar"
out_dir = "E:/temp/Konrad/CHM/20200616/chm"

list_folder <- list.dirs(path = path, full.names = TRUE, recursive = TRUE)

for (f in list_folder){
    if (grepl("2", f, fixed = TRUE)){
        print(f)
        part_f <- strsplit(f,"/")[[1]][7]
        list<-list.files(path = f, 
                         pattern = glob2rx('*.las'),full.names = T)
        for(i in list){
            part_i <- strsplit(i,"/")[[1]][8]
            out <- file.path(out_dir,part_f,paste0('CHM_',strsplit(part_i,'.las')[[1]], '.tif'))
            print(out)
            las = readLAS(i,select = "i,c,r")
            las = lasnormalize(las, tin())
            sub <- 1
            chm <- grid_canopy(las, 1, pitfree(thresholds = c(0,2,5,10,15), max_edge = c(0, 1), subcircle = sub))
            writeRaster(chm,out,options=c('TFW=YES'),overwrite=TRUE)
        }
    }
    
}

#test
path = "E:\\temp\\Konrad\\CHM\\20200616\\lidar\\Justicia2018"
out_path = "E:\\temp\\Konrad\\CHM\\20200917\\CHM_pitfree\\Ori\\Justicia2018"
list<-list.files(path = path, 
                 pattern = glob2rx('*.las'),full.names = T)
for(i in list){
    part_i <- basename(sub('\\.las$', '', i))
    out<- file.path(out_path,paste0(part_i,'.tif'))
    print(out)
    las = readLAS(i,select = "i,c,r")
    las = lasnormalize(las, tin(),na.rm = TRUE)
    chm <- grid_canopy(las, 1, p2r(subcircle = 0.5))
    writeRaster(chm,out,overwrite=TRUE)
}

#test case CHM  
path = "E:\\temp\\Konrad\\CHM\\20200616\\lidar\\Justicia2008\\Justicia2008_1.las"
las = readLAS(path,select = "i,c,r")
las = lasnormalize(las, tin())

chm <- grid_canopy(las, 1, p2r(subcircle = 0.3))
#plot(chm)
out = "E:\\temp\\Konrad\\CHM\\20200917\\CHM_pitfree\\Ori\\Justicia2008\\Justicia2008_0.3.tif"
writeRaster(chm,out,overwrite=TRUE)

#visualize ALS
path = "C:\\Users\\lxiao\\Desktop\\20200929\\tile_34920_-2718680_subset.las"
out = "C:\\Users\\lxiao\\Desktop\\20200929\\height.las"

path = "E:\\temp\\Konrad\\archive\\LiDAR_CHM_archive_20200926\\LiDAR\\Welverdiendt_2019118120814_O02122_T01471\\tile_30920_-2717680_subset.las"

path = "C:\\Users\\lxiao\\Desktop\\20200929\\subset.las"


out = "C:\\Users\\lxiao\\Desktop\\20200929\\height.las"
las = readLAS(path,select = "i,c,r")
las = lasnormalize(las, tin())
plot(las)

writeLAS(las, out, index = FALSE)


#test case ChangeMap
library(raster)
library(tiff)
library(rgdal)

date1 <- "2008"
date2 <- "2010"
studysite <- "Justicia"
pd <- ""

workspace <- "C:\\Users\\lxiao\\Desktop\\20200921"
studysite1 <- paste0(studysite,date1,pd,".tif")
studysite2 <- paste0(studysite,date2,pd,".tif")
change_ext <- paste0("_Change_",date2,"_",date1,pd,".tif")
diff_ext <- paste0("_Diff_",date2,"_",date1,pd,".tif")

CHM1<- file.path(workspace,studysite1)
CHM2<- file.path(workspace,studysite2)

r1 <- raster(CHM1)
r2 <- raster(CHM2)
r3 <- raster(CHM1)
#set NA values to NA
values(r3)[!is.null(r3)] = NA
values(r1)[values(r1) <= -0.05] = NA
values(r1)[values(r1) > 50] = NA
values(r2)[values(r2) <= -0.05] = NA
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
writeRaster(r3,file.path(workspace,paste0(studysite,change_ext)),options=c('TFW=YES'),overwrite=TRUE)

r4 <- r2-r1
writeRaster(r4,file.path(workspace,paste0(studysite,diff_ext)),options=c('TFW=YES'),overwrite=TRUE)



#histograms
require(ff)
library(lidR)
library(ggplot2)
raster_path1 = "C:\\Users\\lxiao\\Desktop\\20200916_CHM\\tile_45920_2752680\\CHMs\\Justicia2018_1.tif"
raster_path2 = "C:\\Users\\lxiao\\Desktop\\20200916_CHM\\tile_45920_2752680\\CHMs\\Justicia2018_2.tif"
raster_path3 = "C:\\Users\\lxiao\\Desktop\\20200916_CHM\\tile_45920_2752680\\CHMs\\Justicia2018_4.tif"
raster_path4 = "C:\\Users\\lxiao\\Desktop\\20200916_CHM\\tile_45920_2752680\\CHMs\\Justicia2018_7.tif"
raster_path5 = "C:\\Users\\lxiao\\Desktop\\20200916_CHM\\tile_45920_2752680\\CHMs\\Justicia2018_ori.tif"

raster1 <- raster(raster_path1)
raster2 <- raster(raster_path2)
raster3 <- raster(raster_path3)
raster4 <- raster(raster_path4)
raster5 <- raster(raster_path5)

vector1 <- ff(getValues(raster1))
vector2 <- ff(getValues(raster2))
vector3 <- ff(getValues(raster3))
vector4 <- ff(getValues(raster4))
vector5 <- ff(getValues(raster5))

#new raster hist
par(mfrow=c(1,1),cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,mgp=c(0.5,1,0)) 

p1<-hist(vector5[,3],xlab = "",ylab="",xlim = c(-1,15),ylim = c(0,600000),
         main="CHM histogram for original Justicia tile_45920_2752680",
         breaks = 50,
         col=scales::alpha('green',.99))

p2<-hist(vector4[,3],
         main="CHM histogram for Justicia tile_45920_2752680 at 7 point density level",
         breaks = 50,xlab = "",ylab="",xlim = c(-1,15),ylim = c(0,600000),
         col=scales::alpha('blue',.99),
         cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,border=F,add=TRUE)

p3<-hist(vector3[,3],xlab = "",ylab="",xlim = c(-1,15),ylim = c(0,600000),
         main="CHM histogram for Justicia tile_45920_2752680 at 4 point density level",
         breaks = 50,
         col=scales::alpha('orange',.70),
         cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,border=F,add=TRUE)

p4<-hist(vector2[,3],xlab = "",ylab="",xlim = c(-1,15),ylim = c(0,600000),
         main="CHM histogram for Justicia tile_45920_2752680 at 2 point density level",
         breaks = 50,
         col=scales::alpha('purple',.60),
         cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,border=F,add=TRUE)

p5<-hist(vector1[,3],xlab = "",ylab="",xlim = c(-1,15),ylim = c(0,600000),
         main="CHM histogram for Justicia tile_45920_2752680 at 1 point density level",
         breaks = 100,
         col=scales::alpha('red',.60),
         cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,border=F,add=TRUE)

legend(6, 500000, c("Justicia2018","Justicia2018_7","Justicia2018_4","Justicia2018_2","Justicia2018_1"),
       box.col = "white",cex = 2,lwd=5, col=c("green","blue","orange","purple","red"))





main="CHM histogram for Justicia tile_41920_-2754680 at 1 point density level"
plot(p5,main="", yaxs="i",xlab="Elevation",xlim=c(0,10),cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,ylim = c(0,50000),
     col=scales::alpha('red',.99),mgp=c(2.7,1,0),border=F,add=TRUE)  # ori


#2008 2010 2018 change map hist
raster_1 = "C:\\Users\\lxiao\\Desktop\\20200916_CHM\\tile_41920_2754680\\ChangeMaps_Ori\\Justicia_Change_2010_2008.tif"
raster_2 = "C:\\Users\\lxiao\\Desktop\\20200916_CHM\\tile_41920_2754680\\ChangeMaps_Ori\\Justicia_Change_2018_2008.tif"
raster_3 = "C:\\Users\\lxiao\\Desktop\\20200916_CHM\\tile_41920_2754680\\ChangeMaps_Ori\\Justicia_Change_2018_2010.tif"

raster1 <- raster(raster_1)
raster2 <- raster(raster_2)
raster3 <- raster(raster_3)

f1 <- as.data.frame(freq(raster1))
f2 <- as.data.frame(freq(raster2))
f3 <- as.data.frame(freq(raster3))

## Add text at top of bars
label <- c("Persistence", "Big tree losses", "Shrub encroachment", "Shrub losses", "Other losses", "Other gain", "NoData")
ggplot(f1, aes(x=label, y=count)) +
    geom_col() +
    geom_text(aes(y = 0, label = count), vjust = 1, size = 5,colour="Red") +
    theme(text=element_text(size=20)) +
    xlab("Class") + ylab("Frequency")+
    ylim(0,900000)+
    ggtitle("Justicia tile_41920_2754680 change map histogram (2010~2008)")+
    theme(plot.title = element_text(hjust = 0.5))

ggplot(f2, aes(x=label, y=count)) +
    geom_col() +
    geom_text(aes(y = 0, label = count), vjust = 1, size = 5,colour="Red") +
    theme(text=element_text(size=20)) +
    xlab("Class") + ylab("Frequency")+
    ylim(0,900000)+
    ggtitle("Justicia tile_41920_2754680 change map histogram (2018~2008)")+
    theme(plot.title = element_text(hjust = 0.5))

ggplot(f3, aes(x=label, y=count)) +
    geom_col() +
    geom_text(aes(y = 0, label = count), vjust = 1, size = 5,colour="Red") +
    theme(text=element_text(size=20)) +
    xlab("Class") + ylab("Frequency")+
    ylim(0,900000)+
    ggtitle("Justicia tile_41920_2754680 change map histogram (2018~2010)")+
    theme(plot.title = element_text(hjust = 0.5))





#subsample
library(lidR)

path <- "E:\\temp\\Drone\\May"
#path <- "C:\\Users\\lxiao\\Desktop\\20201126\\FlightGrid1_5_1.las"
list_folder <- list.files(path = path, full.names = TRUE, recursive = TRUE)
for (f in list_folder){
    if (grepl("loud", f, fixed = TRUE)){
        las <- readLAS(f,select = "i,c,r")
        print(las) 
    }
}
las <- readLAS(path,select = "i,c,r")
las
plot(las)
out <- "C:\\Users\\lxiao\\Desktop\\20200915\\Justicia2018_7.las"
las_sub <- lasfilterdecimate(las, homogenize(1,1))
las_sub
las_sub = lasnormalize(las, tin(),na.rm = TRUE)
writeLAS(las_sub, out, index = FALSE)


#loop, thin and CHM generation
path = "E:\\temp\\Konrad\\CHM\\20200616\\lidar"
out_dir = "E:\\temp\\Konrad\\CHM\\20200918\\Thin_CHM"

list_folder <- list.dirs(path = path, full.names = TRUE, recursive = TRUE)

for (f in list_folder){
    if (grepl("Justicia2018", f, fixed = TRUE)){
        print(f)
        part_f <- basename(f)
        print(part_f)
        lidar_list<-list.files(path = f, 
                         pattern = glob2rx('*.las'),full.names = T)
        for(i in lidar_list){
            #Subset lidars selection
            if (grepl("Justicia2018", i, fixed = TRUE)){
                
                pd_list <- c(1,2,4,7)
                part_i <- basename(sub('\\.las$', '', i))
                print(part_i)
                #ori
                las = readLAS(i,select = "i,c,r")
                for (pd in pd_list){
                    print(pd)
                    #thin 1
                    las_sub = lasfilterdecimate(las, homogenize(pd,1))
                    las_sub = lasnormalize(las_sub, tin(),na.rm = TRUE)
                    chm <- grid_canopy(las_sub, 1, p2r())
                    out <- file.path(out_dir,pd,paste0(part_i,"_",pd,".tif"))
                    print(out)
                    writeRaster(chm,out,options=c('TFW=YES'),overwrite=TRUE)
                    }
            

            }
            
            
        }
    }
    
}


#Drone individual
las <- "E:\\temp\\Drone\\May\\FlightGrid5_1_1.las"
out <- "E:\\temp\\Drone\\Results\\FlightGrid5_1_1.tif"
las <- readLAS(las,select = "i,c,r")
las = lasnormalize(las, tin(),na.rm = TRUE)
las <- filter_poi(las, Z >= 0, Z <= 50)
#Pitfree 0.6
chm <- grid_canopy(las, 0.6, p2r(subcircle = 0.6))
plot(chm)
writeRaster(chm,out,options=c('TFW=YES'),overwrite=TRUE)


#Neon
las <- "E:\\temp\\Drone\\NEON\\NEON2019_6_1.las"
out <- "E:\\temp\\Drone\\Results\\Neon.tif"
las <- readLAS(las,select = "i,c,r")
las = lasnormalize(las, tin(),na.rm = TRUE)
las <- filter_poi(las, Z >= 0, Z <= 50)
#Pitfree 0.6
chm <- grid_canopy(las, 0.3, p2r())
plot(chm)
writeRaster(chm,out,options=c('TFW=YES'),overwrite=TRUE)



#GGS662 loop pitfree chm
library(lidR)
lid_path <- "D:\\temp\\ggs662"
out_path <- "D:\\temp\\ggs662\\tif"
list<-list.files(path = lid_path, 
                 pattern = glob2rx('*.las'),full.names = T)
for (las in list){
    part <- basename(sub('\\.las$', '', las))
    print(part)
    las <- readLAS(las,select = "i,c,r")
    lasground(las, csf,last_returns = TRUE)
    las = lasnormalize(las, tin(),na.rm = TRUE)
    las <- filter_poi(las, Z >= 0, Z <= 50)
    #Pitfree 0.6
    chm <- grid_canopy(las, 0.6, p2r(subcircle = 0.6))
    out <- file.path(out_path,paste0(part,"_0.6.tif"))
    writeRaster(chm,out,options=c('TFW=YES'),overwrite=TRUE)
}

#merge 
c <- 1
x <- list()
tif_path <- "D:\\temp\\ggs662"
list<-(list.files(path = tif_path, 
                  pattern = "tif",full.names = T))
print(list)
for(i in list){
    x[[c]] <-stack(i)
    c <- c+1
}
x$fun <- max
x$na.rm <- TRUE
x$tolerance = 0.5
y <- do.call(mosaic, x)
plot(y)
out <- "E:\\temp\\Drone\\Results\\Drone_Merged.tif"
writeRaster(y,out,options=c('TFW=YES'),overwrite=TRUE)



library(raster)
library(ggplot2)
library(ggpointdensity)
library(viridis)
tif_drone <- "E:\\temp\\Drone\\Results\\Drone.tif"
tif_neon <- "E:\\temp\\Drone\\Results\\Neon.tif"
raster_drone <- raster(tif_drone)
raster_neon <- raster(tif_neon)
diff <- raster_drone - raster_neon
s <- stack(raster_drone, raster_neon, diff)
v <- data.frame(values(s))
names(v) <- c('Drone', 'Neon',"Diff")
v<-v[!(v$Drone < 0 | v$Neon < 0),]
#write.csv(v,"C:\\Users\\lxiao\\Desktop\\GGS662\\MyData.csv")
v <- v[sample(nrow(v), 100000), ]
reg1 <- lm(Drone ~ Neon, data=v)

ggplot(v, aes(x=Neon, y=Drone))+ 
    geom_pointdensity()+
    scale_color_viridis()+
    scale_x_continuous(limits = c(0,50))+
    scale_y_continuous(limits = c(0,50))+
    theme(text=element_text(size=20)) + 
    geom_smooth(
        method="lm", 
        se= F, 
        size = 1.5, 
        linetype = "solid",
        colour = "red") +
    stat_poly_eq(
        formula = y ~ x, 
        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
        size = 5,
        parse = TRUE)+
    geom_abline(color="gray", linetype="dashed")

Mean <- mean(reg1$fitted.values)
RMSE <- sqrt(mean(reg1$residuals^2))
rRMSE <- sqrt(mean(reg1$residuals^2))/sd(v$Drone)
SD <- sd(reg1$fitted.values)
coeff <- round(reg1$coefficients , 2)
print(paste0("r2 ",round(summary(reg1)$adj.r.squared,3)))
print(paste0("Slope ",coeff[2]))
print(paste0("Intercept ",coeff[1]))
print(paste0("Mean ",round(Mean,3)))
print(paste0("SD ",round(SD,3)))
print(paste0("RMSE ",round(RMSE,3)))
print(paste0("%RMSE ",round(rRMSE,3)*100))


ggplot(v, aes(Diff)) +
    geom_histogram(data=v,
                   aes(color = "blue", fill = Diff),
                   position = "identity", bins = 50,alpha = 0.5) +
    scale_x_continuous(limits = c(-30,30),breaks = scales::pretty_breaks(n = 10))+
    xlab("Height difference between UAV and ALS") + ylab("Frequency")+
    theme(legend.position = "none")+
    theme(text=element_text(size=20))

#20201215 updates individial chm generation
#loop, thin and CHM generation
path = "E:\\ChangMap\\CHM\\LiDAR"
out_dir1 = "E:\\ChangMap\\CHM\\CHM\\CHM_ori"
out_dir2 = "E:\\ChangMap\\CHM\\CHM\\CHM_pitfree"
list_folder <- list.dirs(path = path, full.names = TRUE)
pd = 0.5
for (f in list_folder){
    if (grepl("Justicia2008", f, fixed = TRUE)){
        print(f)
        part_f <- basename(f)
        print(part_f)
        lidar_list<-list.files(path = f, 
                               pattern = glob2rx('*.las'),full.names = T)
        for(i in lidar_list){
            #Subset lidars selection
            part_i <- basename(sub('\\.las$', '', i))
            print(part_i)
            #ori
            las = readLAS(i,select = "i,c,r")
            
            #las_sub = lasfilterdecimate(las, homogenize(pd,1))
            las_sub = lasnormalize(las, tin(),na.rm = TRUE)
            chm1 <- grid_canopy(las_sub, 1, p2r())
            out1 <- file.path(out_dir1,part_f,paste0(part_i,".tif"))
            print(out1)
            writeRaster(chm1,out1,options=c('TFW=YES'),overwrite=TRUE)
            
            chm1 <- grid_canopy(las_sub, 1, p2r(subcircle = pd))
            out1 <- file.path(out_dir2,part_f,paste0(part_i,"_",pd,".tif"))
            print(out1)
            writeRaster(chm1,out1,options=c('TFW=YES'),overwrite=TRUE)
        }
    }

}


library(lidR)
library(tools) 
library(ggplot2)
library(raster)

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

changemap_stack(2010,2018,"Welverdiendt")


#SA chm
Locdir <- "E:\\GEDI\\ALS_archive\\LiDAR_UTM"
Outdir <- "E:\\GEDI\\ALS_archive\\LiDAR_CHM"
folders <- list.files(path = Locdir, full.names = TRUE)
for (i in folders){
    dir.create(file.path(Outdir, basename(i)), showWarnings = FALSE)
    tiles <- list.files(path = i, full.names = TRUE)
    for (las_i in tiles){
        Output <- file.path(Outdir, basename(i),paste0(sub('\\.las$', '', basename(las_i)),".tif"))
        if (!(file.exists(Output))){
            print(Output)
            try({
                
                las <- readLAS(las_i,select = "i,c,r")
                las = lasnormalize(las, tin(),na.rm = TRUE)
                las <- filter_poi(las, Z >= 0, Z <= 50)
                chm <- se(las, 1, p2r())
                writeRaster(chm,Output,options=c('TFW=YES'),overwrite=TRUE)
            }
                
            )
        }
    }
}

#SA DTM DSM
Locdir <- "E:\\GEDI\\ALS_archive\\LiDAR_UTM"
Outdir1 <- "E:\\GEDI\\ALS_archive\\LiDAR_DTM"
Outdir2 <- "E:\\GEDI\\ALS_archive\\LiDAR_DSM"
folders <- list.files(path = Locdir, full.names = TRUE)
for (i in folders){
    dir.create(file.path(Outdir1, basename(i)), showWarnings = FALSE)
    dir.create(file.path(Outdir2, basename(i)), showWarnings = FALSE)
    tiles <- list.files(path = i, full.names = TRUE)
    for (las_i in tiles){
        outfiles1 <- file.path(Outdir1, basename(i),paste0(sub('\\.las$', '', basename(las_i)),".tif"))
        outfiles2 <- file.path(Outdir2, basename(i),paste0(sub('\\.las$', '', basename(las_i)),".tif"))
        if (!((file.exists(outfiles1))&(file.exists(outfiles2)))){
            print(outfiles1)
            print(outfiles2)
            try({
                las <- readLAS(las_i,select = "i,c,r")
                ground = lasfilter(las, Classification == 2)
                metrics_g = grid_metrics(ground, ~min(Z), 1)
                writeRaster(metrics_g,outfiles1,overwrite = T)
                canopy = lasfilter(las, Classification == c(3,5))
                metrics_c = try(grid_metrics(canopy, ~max(Z), 1), silent = TRUE)
                try(writeRaster(metrics_c,outfiles2,overwrite = T), silent = TRUE)
                
            }
            )
        }
    }
}







#SA limpopo chm
Locdir <- "E:\\GEDI\\ALS_archive\\LiDAR_UTM\\Limpopo3"
Outdir <- "E:\\GEDI\\ALS_archive\\LiDAR_CHM\\Limpopo3"
folders <- list.files(path = Locdir, full.names = TRUE,pattern = ".las")
for (las_i in folders){
    print(las_i)
    try({
    Output <- file.path(Outdir, paste0(sub('\\.las$', '', basename(las_i)),".tif"))
    las <- readLAS(las_i,select = "i,c,r")
    las = lasnormalize(las, tin(),na.rm = TRUE)
    las <- filter_poi(las, Z >= 0, Z <= 50)
    chm <- grid_canopy(las, 1, p2r())
    writeRaster(chm,Output,options=c('TFW=YES'),overwrite=TRUE)
    })
}

#SA limpopo DTM DSM
Locdir <- "E:\\GEDI\\ALS_archive\\LiDAR_UTM"
Outdir1 <- "E:\\GEDI\\ALS_archive\\LiDAR_DTM"
Outdir2 <- "E:\\GEDI\\ALS_archive\\LiDAR_DSM"
folders <- list.files(path = Locdir, full.names = TRUE)
for (i in folders){
    dir.create(file.path(Outdir1, basename(i)), showWarnings = FALSE)
    dir.create(file.path(Outdir2, basename(i)), showWarnings = FALSE)
    tiles <- list.files(path = i, full.names = TRUE)
    for (las_i in tiles){
        outfiles1 <- file.path(Outdir1, basename(i),paste0(sub('\\.las$', '', basename(las_i)),".tif"))
        outfiles2 <- file.path(Outdir2, basename(i),paste0(sub('\\.las$', '', basename(las_i)),".tif"))
        if (!((file.exists(outfiles1))&(file.exists(outfiles2)))){
            print(outfiles1)
            print(outfiles2)
            try({
                las <- readLAS(las_i,select = "i,c,r")
                ground = filter_poi(las, Classification == 2)
                metrics_g = grid_metrics(ground, ~min(Z), 1)
                writeRaster(metrics_g,outfiles1,overwrite = T)
                canopy = lasfilter(las, Classification == c(3,5))
                metrics_c = try(grid_metrics(canopy, ~max(Z), 1), silent = TRUE)
                try(writeRaster(metrics_c,outfiles2,overwrite = T), silent = TRUE)
                
            }
            )
        }
    }
}



#SA limpopo DTM DSM
Locdir <- "E:\\GEDI\\ALS_archive\\LiDAR_UTM"
Outdir1 <- "E:\\GEDI\\ALS_archive\\LiDAR_DTM"

folders <- list.files(path = Locdir, full.names = TRUE,pattern = "Limp")
for (i in folders){
    dir.create(file.path(Outdir1, basename(i)), showWarnings = FALSE)
    tiles <- list.files(path = i, full.names = TRUE)
    for (las_i in tiles){
        outfiles1 <- file.path(Outdir1, basename(i),paste0(sub('\\.las$', '', basename(las_i)),".tif"))
        if (!((file.exists(outfiles1)))){
            print(outfiles1)
            try({
                las <- readLAS(las_i,select = "i,c,r")
                ground = filter_poi(las, Classification == 2)
                metrics_g = grid_metrics(ground, ~min(Z), 1)
                writeRaster(metrics_g,outfiles1,overwrite = T)
            }
            )
        }
    }
}


dir = "E:\\Biomass\\CSIR\\Result_0622\\Wel.las"
output = "E:\\Biomass\\CSIR\\Result_0622\\Wel_normalized_height.las"
las = readLAS(dir,select = "i,c,r")
las = normalize_height(las, tin())
writeLAS(las, output, index = FALSE)




