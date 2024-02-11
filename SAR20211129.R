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
library(igraph)
###############################################################################
#thin data 
#bulk thin process--CC method for 2018 data -- test -- outdated
Locdir <- "E:\\ChangMap\\CHM\\DB_20210830\\DB_ALS\\Justicia_multipart\\thinned"
files <- list.files(path = Locdir, full.names = TRUE)
for (i in files){
    print(i)
    las = readLAS(i,select = "i,c,r")
    las_sub = normalize_height(las, tin(),na.rm = TRUE)
    chm <- grid_canopy(las_sub, 1, p2r())
    out <- file.path(Locdir,paste0(strsplit(basename(i),'.las')[[1]],".tif"))
    writeRaster(chm,out,options=c('TFW=YES'),overwrite=TRUE)
}


#bulk thin process--R method for 2018 data -- test -- good
Locdir <- "E:\\ChangMap\\CHM\\LiDAR_archive\\Ori_2018\\test"
files <- list.files(path = Locdir, full.names = TRUE)
for (i in files){
    print(i)
    pd = as.numeric(str_sub(strsplit(basename(i),'_')[[1]][4],1,1))
    las = readLAS(i,select = "i,c,r")
    las = decimate_points(las, homogenize(2,25))
    las_sub = normalize_height(las, tin(),na.rm = TRUE)
    chm <- grid_canopy(las_sub, 1, p2r())
    out <- file.path(Locdir,paste0(strsplit(basename(i),'.las')[[1]],".tif"))
    writeRaster(chm,out,options=c('TFW=YES'),overwrite=TRUE)
}


#thin process--R method-- test
out <- "E:\\ChangMap\\CHM\\LiDAR_archive\\Ori_2018\\test\\tile_21000_-2745000_2008.tif"
out_als <- "E:\\ChangMap\\CHM\\LiDAR_archive\\Ori_2018\\test\\tile_21000_-2745000_2008.las"
i<- "E:\\ChangMap\\CHM\\LiDAR_archive\\Ori_2018\\test\\original\\tile_21000_-2745000_2008.las"
las = readLAS(i,select = "i,c,r")
las = decimate_points(las, homogenize(2,25))
las_sub = normalize_height(las, tin(),na.rm = TRUE)
chm <- grid_canopy(las_sub, 1, p2r())
writeRaster(chm,out,options=c('TFW=YES'),overwrite=TRUE)
writeLAS(las_sub, out_als, index = FALSE)

#CHM generation with no thinning
out <- "E:\\ChangMap\\CHM\\LiDAR_archive\\Ori_2018\\test\\tile_21000_-2745000_2018_ori.tif"
i<- "E:\\ChangMap\\CHM\\LiDAR_archive\\Ori_2018\\test\\original\\tile_21000_-2745000_2018.las"
las = readLAS(i,select = "i,c,r")
las_sub = normalize_height(las, tin(),na.rm = TRUE)
chm <- grid_canopy(las_sub, 1, p2r())
writeRaster(chm,out,options=c('TFW=YES'),overwrite=TRUE)


#pitfill --test
#test case CHM  
path = "E:\\ChangMap\\CHM\\LiDAR_archive\\Original\\Ireagh2008\\Ireagh2008.las"
las = readLAS(path,select = "i,c,r")
las = normalize_height(las, tin(),na.rm = TRUE)
out_dir = "E:\\ChangMap\\CHM\\DB_20210905"

#original
out = file.path(out_dir,"Ireagh2008.tif")
chm <- grid_canopy(las, 1, p2r())
writeRaster(chm,out,overwrite=TRUE)
out = file.path(out_dir,"Ireagh2008.tif")
r <- raster(out)
r[r < 1.5] <- 0
r[r >= 1.5] <- 1
writeRaster(r,file.path(out_dir,paste0("cover_",basename(out))),options=c('TFW=YES'),overwrite=TRUE)

#pitfill 0.3~2
out = file.path(out_dir,"Ireagh2008_0.3.tif")
chm <- grid_canopy(las, 1, p2r(subcircle = 0.3))
writeRaster(chm,out,overwrite=TRUE)
out = file.path(out_dir,"Ireagh2008_0.3.tif")
r <- raster(out)
r[r < 1.5] <- 0
r[r >= 1.5] <- 1
writeRaster(r,file.path(out_dir,paste0("cover_",basename(out))),options=c('TFW=YES'),overwrite=TRUE)

out = file.path(out_dir,"Ireagh2008_0.5.tif")
chm <- grid_canopy(las, 1, p2r(subcircle = 0.5))
writeRaster(chm,out,overwrite=TRUE)
out = file.path(out_dir,"Ireagh2008_0.5.tif")
r <- raster(out)
r[r < 1.5] <- 0
r[r >= 1.5] <- 1
writeRaster(r,file.path(out_dir,paste0("cover_",basename(out))),options=c('TFW=YES'),overwrite=TRUE)

out = file.path(out_dir,"Ireagh2008_1.tif")
chm <- grid_canopy(las, 1, p2r(subcircle = 1))
writeRaster(chm,out,overwrite=TRUE)
out = file.path(out_dir,"Ireagh2008_1.tif")
r <- raster(out)
r[r < 1.5] <- 0
r[r >= 1.5] <- 1
writeRaster(r,file.path(out_dir,paste0("cover_",basename(out))),options=c('TFW=YES'),overwrite=TRUE)

out = file.path(out_dir,"Ireagh2008_2.tif")
chm <- grid_canopy(las, 1, p2r(subcircle = 2))
writeRaster(chm,out,overwrite=TRUE)
out = file.path(out_dir,"Ireagh2008_2.tif")
r <- raster(out)
r[r < 1.5] <- 0
r[r >= 1.5] <- 1
writeRaster(r,file.path(out_dir,paste0("cover_",basename(out))),options=c('TFW=YES'),overwrite=TRUE)


#histogram of cover 0.3~2
i <- "E:\\ChangMap\\CHM\\DB_20210905\\ZONAL.csv"
Data = read.csv(i,header=T)
p1<- ggplot(Data, aes(x=Cover)) + 
    theme_bw()+
    coord_cartesian(ylim = c(0, 25000))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_histogram(color =  "white",fill = "blue",alpha = 0.2,binwidth = 0.1) +
    scale_x_continuous(breaks = seq(0, 1, 0.1))+
    labs(x="Ireagh 2008 canopy cover, original", 
         y="Frequency")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=20))

p2<- ggplot(Data, aes(x=Cover_0.3)) + 
    theme_bw()+
    coord_cartesian(ylim = c(0, 25000))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_histogram(color =  "white",fill = "blue",bins = 10,alpha = 0.2) +
    scale_x_continuous(breaks = seq(0, 1, 0.1))+
    labs(x="Ireagh 2008 canopy cover, subcircle 0.3", 
         y="Frequency")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=20))

p3<- ggplot(Data, aes(x=Cover_0.5)) + 
    theme_bw()+
    coord_cartesian(ylim = c(0, 25000))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_histogram(color =  "white",fill = "blue",bins = 10,alpha = 0.2) +
    scale_x_continuous(breaks = seq(0, 1, 0.1))+
    labs(x="Ireagh 2008 canopy cover, subcircle 0.5", 
         y="Frequency")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=20))

p4<- ggplot(Data, aes(x=Cover_1)) + 
    theme_bw()+
    coord_cartesian(ylim = c(0, 25000))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_histogram(color =  "white",fill = "blue",bins = 10,alpha = 0.2) +
    scale_x_continuous(breaks = seq(0, 1, 0.1))+
    labs(x="Ireagh 2008 canopy cover, subcircle 1", 
         y="Frequency")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=20))

p5<- ggplot(Data, aes(x=Cover_2)) + 
    theme_bw()+
    coord_cartesian(ylim = c(0, 25000))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_histogram(color =  "white",fill = "blue",bins = 10,alpha = 0.2) +
    scale_x_continuous(breaks = seq(0, 1, 0.1))+
    labs(x="Ireagh 2008 canopy cover, subcircle 2", 
         y="Frequency")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=20))

ggarrange(p1,p2,p3,p4,p5)
###############################################################################
#check point density
#thinned 2018 las file
i <- "E:\\ChangMap\\CHM\\LiDAR_archive\\Original\\test\\tile_20920_-2719680.las"
las = readLAS(i,select = "i,c,r")
Data<- as.data.frame(grid_density(las, res = 1), res = 1, xy=FALSE)
p1 <- ggplot(Data, aes(x=point_density)) + 
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_histogram(color =  "white",fill = "blue",bins = 10,alpha = 0.2) +
    scale_x_continuous(breaks = seq(0, 15, 1), lim = c(0, 15))+
    labs(x="Original 2018 point density", 
         y="Frequency")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=20))

i <- "E:\\ChangMap\\CHM\\LiDAR_archive\\Original\\test\\tile_20920_-2719680_0.75.las"
las = readLAS(i,select = "i,c,r")
Data<- as.data.frame(grid_density(las, res = 1), res = 1, xy=FALSE)
p2 <- ggplot(Data, aes(x=point_density)) + 
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_histogram(color =  "white",fill = "blue",bins = 10,alpha = 0.2) +
    scale_x_continuous(breaks = seq(0, 15, 1), lim = c(0, 15))+
    labs(x="Thinned 2018 point density", 
         y="Frequency")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=20))


#check distribution between thinned and unthinned chm
raster_1 = "E:\\ChangMap\\CHM\\LiDAR_archive\\Original\\test\\tile_20920_-2719680.tif"
raster_2 <- "E:\\ChangMap\\CHM\\LiDAR_archive\\Original\\test\\tile_20920_-2719680_0.75.tif"

raster1 <- raster(raster_1)
raster2 <- raster(raster_2)

f <- hist(raster1, maxpixels=10000000,breaks=30,plot=F)
dat <- data.frame(counts= f$counts,breaks = f$mids)
p3 <- ggplot(dat, aes(x = breaks, y = counts)) + 
    geom_bar(stat = "identity",color =  "white",fill = "blue",alpha = 0.2)+
    scale_x_continuous(limits = c(0, 10))+
    scale_y_continuous(limits = c(0, 70000))+
    theme_bw()+
    labs(x="Original 2018 CHM (m)", 
         y="Frequency")+
    theme(text=element_text(size=20))+
    theme(legend.title = element_blank())+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

f <- hist(raster2, maxpixels=10000000,breaks=30,plot=F)
dat <- data.frame(counts= f$counts,breaks = f$mids)
p4 <- ggplot(dat, aes(x = breaks, y = counts)) + 
    geom_bar(stat = "identity",color =  "white",fill = "blue",alpha = 0.2)+
    scale_x_continuous(limits = c(0, 10))+
    scale_y_continuous(limits = c(0, 70000))+
    theme_bw()+
    labs(x="Thinned 2018 CHM (m) (1.78 pt/m2)", 
         y="Frequency")+
    theme(text=element_text(size=20))+
    theme(legend.title = element_blank())+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggarrange(p1,p2,p3,p4)



i <- "E:\\ChangMap\\CHM\\LiDAR_archive\\Original\\test\\tile_20920_-2719680_0.75.las"
las = readLAS(i,select = "i,c,r")
Data<- as.data.frame(grid_density(las, res = 1), res = 1, xy=FALSE)
p2 <- ggplot(Data, aes(x=point_density)) + 
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_histogram(color =  "white",fill = "blue",bins = 10,alpha = 0.2) +
    scale_x_continuous(breaks = seq(0, 10, 1), lim = c(0, 10))+
    labs(x="Thinned 2018 point density", 
         y="Frequency")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=20))

i <- "E:\\ChangMap\\CHM\\LiDAR_archive\\Original\\test\\WelAndover2010.las"
las = readLAS(i,select = "i,c,r")
Data<- as.data.frame(grid_density(las, res = 1), res = 1, xy=FALSE)
p1 <- ggplot(Data, aes(x=point_density)) + 
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_histogram(color =  "white",fill = "blue",bins = 10,alpha = 0.2) +
    scale_x_continuous(breaks = seq(0, 10, 1), lim = c(0, 10))+
    labs(x="Thinned 2010 point density", 
         y="Frequency")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=20))

ggarrange(p1,p2)

################################################################################

#CHM
site = "Justicia2018"
dir = "E:\\ChangMap\\CHM\\DB_20210830\\DB_ALS"
out <- file.path(dir,paste0(site,".tif"))
i<- file.path(dir,paste0(site,".las"))
las = readLAS(i,select = "i,c,r")
las = normalize_height(las, tin(),na.rm = TRUE)
chm <- grid_canopy(las, 1, p2r())
writeRaster(chm,out,options=c('TFW=YES'),overwrite=TRUE)

#cover
out <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_cover"
Locdir <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_chm\\Mosaic"
chms <- list.files(path = Locdir, full.names = TRUE, pattern = "*.tif$")
for (chm in chms){
    print(chm)
    r <- raster(chm)
    r[r < 1.5] <- 0
    r[r >= 1.5] <- 1
    writeRaster(r,file.path(out,paste0(file_path_sans_ext(basename(chm)),"_cover.tif")),options=c('TFW=YES'),overwrite=TRUE)
}

out <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_cover"
Locdir <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_chm\\Mosaic"
chms <- list.files(path = Locdir, full.names = TRUE, pattern = "*.tif$")
for (chm in chms){
    print(chm)
    r <- raster(chm)
    r[r < 1.5] <- 1
    r[r >= 1.5] <- 0
    writeRaster(r,file.path(out,paste0(file_path_sans_ext(basename(chm)),"_nocover.tif")),options=c('TFW=YES'),overwrite=TRUE)
}





#WCM parameter
#VOLUME estimate for bayesian WCM
i <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Merge_2018.csv"
Data = read.csv(i,header=T)

x <- Data$V2
y<- Data$HV

mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-25, b=-12, c=1e-3))
y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
R2 <- round(cor(y,y_sim)^2,3)
a1 <- coef(mult_nls)[1]
b1 <- coef(mult_nls)[2]
c1 <- coef(mult_nls)[3]
max_volume <- max(x)
min_HV_y <- min(y)
max_HV_y <- max(y)


y<- Data$HH
mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-25, b=-12, c=1e-3))
y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
R2 <- round(cor(y,y_sim)^2,3)
a2 <- coef(mult_nls)[1]
b2 <- coef(mult_nls)[2]
c2 <- coef(mult_nls)[3]
min_HH_y <- min(y)
max_HH_y <- max(y)

cat(paste0("Vol_max=",max_volume,";"),
    paste0("HV_list=",min_HV_y,":0.02:",max_HV_y,";"),
    paste0("HH_list=",min_HH_y,":0.02:",max_HH_y,";"),
    paste0("a_HV=",a1,";"),
    paste0("b_HV=",b1,";"),
    paste0("c_HV=",c1,";"),
    paste0("a_HH=",a2,";"),
    paste0("b_HH=",b2,";"),
    paste0("c_HH=",c2,";")
    ,sep="\n")






#COVER estimate for bayesian WCM
i <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Merge_2018.csv"
Data = read.csv(i,header=T)

x <- Data$C2
y<- Data$HV7

mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-20, b=-7, c=2))
y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
R2 <- round(cor(y,y_sim)^2,3)
a1 <- coef(mult_nls)[1]
b1 <- coef(mult_nls)[2]
c1 <- coef(mult_nls)[3]
max_volume <- max(x)
min_HV_y <- min(y)
max_HV_y <- max(y)


y<- Data$HH7

mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-20, b=-7, c=2))
y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
R2 <- round(cor(y,y_sim)^2,3)
a2 <- coef(mult_nls)[1]
b2 <- coef(mult_nls)[2]
c2 <- coef(mult_nls)[3]
min_HH_y <- min(y)
max_HH_y <- max(y)

cat(paste0("Vol_max=",max_volume,";"),
    paste0("HV_list=",min_HV_y,":0.02:",max_HV_y,";"),
    paste0("HH_list=",min_HH_y,":0.02:",max_HH_y,";"),
    paste0("a_HV=",a1,";"),
    paste0("b_HV=",b1,";"),
    paste0("c_HV=",c1,";"),
    paste0("a_HH=",a2,";"),
    paste0("b_HH=",b2,";"),
    paste0("c_HH=",c2,";")
    ,sep="\n")






#bias boxplot cover
dir <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Pred_2010.csv"

out = "E:\\ChangMap\\CHM\\DB_20210905\\Figure\\figure bias cover 2010.jpg"
Data = read.csv(dir,header=T)
Data <- Data[Data$C1 > 0 & Data$C1 < 1,]

x <- Data$C1
y<- Data$P_C
Data$diff <- y-x
breakbin = seq(0,1,0.1)
Data$group_RH <- cut(x,breaks = breakbin
                     ,dig.lab=1)
table(Data$group_RH)

p1 <- ggplot(Data, aes(x=C1, y=diff, group=group_RH)) + 
    stat_boxplot(geom ='errorbar', width = 0.1) +
    geom_boxplot(width = 1)+
    geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
    coord_cartesian(ylim = c(-1, 0.6))+
    scale_y_continuous(minor_breaks = seq(-1, 0.6, 0.2),breaks = seq(-1, 0.6, 0.2))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme_bw()+
    labs(title = "Bayesian WCM vs. ALS canopy cover",x='',y="Bias (m)")+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))

a <- tapply(Data$diff, cut(Data$C1,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$C1, cut(Data$C1,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- seq(0,0.9,0.1)
l <- cbind(x,RB,table(Data$group_RH))
df <- data.frame(l)
df <- head(df,-1)
p3 <- ggplot(df, aes(x=x+0.05, y=RB)) + 
    geom_bar(stat='identity')+
    geom_text(aes(label = V3,y=90), size = 6)+
    theme_bw()+
    coord_cartesian(ylim = c(-70, 90))+
    scale_y_continuous(minor_breaks = seq(-70, 90, 10),breaks = seq(-70, 90, 10))+
    scale_x_continuous(minor_breaks = seq(0,1,0.1),breaks = seq(0,1,0.1))+
    labs(x="ALS Canopy cover", 
         y="%Bias (%)")+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))

x <- Data$C2
y<- Data$P_C_5
Data$diff <- y-x
breakbin = seq(0,1,0.1)
Data$group_RH <- cut(x,breaks = breakbin
                     ,dig.lab=1)
table(Data$group_RH)

p2<- ggplot(Data, aes(x=C2, y=diff, group=group_RH)) + 
    stat_boxplot(geom ='errorbar', width = 0.1) +
    geom_boxplot(width = 1)+
    geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
    coord_cartesian(ylim = c(-1, 0.6))+
    scale_y_continuous(minor_breaks = seq(-1, 0.6, 0.2),breaks = seq(-1, 0.6, 0.2))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme_bw()+
    labs(title = "Bayesian WCM vs. ALS canopy cover (5*5)",x='',y="Bias (m)")+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))

a <- tapply(Data$diff, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$C2, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- seq(0,0.9,0.1)
l <- cbind(x,RB,table(Data$group_RH))
df <- data.frame(l)
df <- head(df,-1)
p4<- ggplot(df, aes(x=x+0.05, y=RB)) + 
    geom_bar(stat='identity')+
    geom_text(aes(label = V3,y=90), size = 6)+
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


ggarrange(p1,p2,p3,p4)

ggsave(out,height=12, width=24, dpi=600)





#bias boxplot volume
dir <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Pred_2010.csv"
out = "E:\\ChangMap\\CHM\\DB_20210905\\Figure\\figure bias volume 2010.jpg"
Data = read.csv(dir,header=T)
Data <- Data[Data$V1 > 0 & Data$V1 < 2400 & Data$V2 > 0 & Data$V2 < 24000,]

x <- Data$V1
y<- Data$P_V
Data$diff <- y-x
breakbin = seq(0,2400,400)
Data$group_RH <- cut(x,breaks = breakbin
                     ,dig.lab=1)
table(Data$group_RH)

p1 <- ggplot(Data, aes(x=V1, y=diff, group=group_RH)) + 
    stat_boxplot(geom ='errorbar', width = 0.1) +
    geom_boxplot(width = 1)+
    geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
    coord_cartesian(ylim = c(-2000,2000))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    scale_y_continuous(minor_breaks = seq(-2000,2000,500),
                       breaks = seq(-2000,2000,500))+
    theme_bw()+
    labs(title = "Bayesian WCM vs. ALS volume",
         x = "",y="Bias (m)")+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))


a <- tapply(Data$diff, cut(Data$V1,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$V1, cut(Data$V1,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- seq(0,2000,400)
l <- cbind(x,RB,table(Data$group_RH))
df <- data.frame(l)
df <- head(df,-1)
p3 <- ggplot(df, aes(x=x+200, y=RB)) + 
    geom_bar(stat='identity')+
    geom_text(aes(label = V3,y=120), size = 6)+
    theme_bw()+
    coord_cartesian(ylim = c(-70, 120))+
    scale_y_continuous(minor_breaks = seq(-70, 120, 10),breaks = seq(-70, 120, 10))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    labs(x="ALS Volume", 
         y="%Bias (%)")+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))


x <- Data$V2
y<- Data$P_V_5
Data$diff <- y-x
breakbin = seq(0,24000,3000)
Data$group_RH <- cut(x,breaks = breakbin
                     ,dig.lab=1)
table(Data$group_RH)

p2<- ggplot(Data, aes(x=V2, y=diff, group=group_RH)) + 
    stat_boxplot(geom ='errorbar', width = 0.1) +
    geom_boxplot(width = 1)+
    geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
    coord_cartesian(ylim = c(-15000,10000))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    scale_y_continuous(minor_breaks = seq(-15000,10000,5000),
                       breaks = seq(-15000,10000,5000))+
    theme_bw()+
    labs(title = "Bayesian WCM vs. ALS volume (5*5)",
         x = "",y="Bias (m)")+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))

a <- tapply(Data$diff, cut(Data$V2,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$V2, cut(Data$V2,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- seq(0,21000,3000)
l <- cbind(x,RB,table(Data$group_RH))
df <- data.frame(l)
df <- head(df,-1)
p4<- ggplot(df, aes(x=x+1500, y=RB)) + 
    geom_bar(stat='identity')+
    geom_text(aes(label = V3,y=120), size = 6)+
    theme_bw()+
    coord_cartesian(ylim = c(-70,120))+
    scale_y_continuous(minor_breaks = seq(-70, 120, 10),breaks = seq(-70, 120, 10))+
    scale_x_continuous(minor_breaks = seq(0,30000,3000),breaks = seq(0,30000,3000))+
    labs(x="ALS Volume (5*5)", 
         y="")+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))


ggarrange(p1,p2,p3,p4)

ggsave(out,height=12, width=24, dpi=600)




#stats for tables
#individual models  SAR  ~ ALS

################################################################
#2007 HV 
i <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Merge_2008.csv"
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
i <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Merge_2010.csv"
Data = read.csv(i,header=T)
x <- Data$V2
y<- Data$HV
mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-25, b=-12, c=1e-3))
y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
R2_HV_2010 <- round(cor(y,y_sim)^2,3)

#2017 HV 
i <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Merge_2018.csv"
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

cat(R2_HV_2007,R2_HV_2008,R2_HV_2010,R2_HV_2017,R2_HV_2018,sep="\n")


#2007 HH 
i <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Merge_2008.csv"
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
i <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Merge_2010.csv"
Data = read.csv(i,header=T)
x <- Data$V2
y<- Data$HH
mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-25, b=-12, c=1e-3))
y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
R2_HH_2010 <- round(cor(y,y_sim)^2,3)

#2017 HH 
i <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Merge_2018.csv"
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
################################################################

################################################################


#2007 HV 
i <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Merge_2008.csv"
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
i <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Merge_2010.csv"
Data = read.csv(i,header=T)
x <- Data$C2
y<- Data$HV
mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-20, b=-7, c=2))
y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
R2_HV_2010 <- round(cor(y,y_sim)^2,3)

#2017 HV 
i <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Merge_2018.csv"
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

cat(R2_HV_2007,R2_HV_2008,R2_HV_2010,R2_HV_2017,R2_HV_2018,sep="\n")


#2007 HH 
i <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Merge_2008.csv"
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
i <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Merge_2010.csv"
Data = read.csv(i,header=T)
x <- Data$C2
y<- Data$HH
mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                start = list(a=-20, b=-7, c=2))
y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
    coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
R2_HH_2010 <- round(cor(y,y_sim)^2,3)

#2017 HH 
i <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Merge_2018.csv"
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


#inverted individual models  ALS ~ SAR
dir <- "E:\\ChangMap\\CHM\\DB_20210921\\DB_csv_100\\Merge_2008.csv"
Data = read.csv(dir,header=T)
x <- Data$C2
y<- Data$P_C_5
reg <- lm(y~x)
r2<-round(summary(reg)$adj.r.squared,3)
pred <- predict(reg)
bias <- mean(pred - x)
RB<- 100*mean(pred - x)/mean(x)
RMSE<- sqrt(mean((pred - x)^2))
RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
cat(r2,bias,RB,RMSE,RRMSE,sep="\n")


#change maps diff ALS ~ diff SAR

dir1 <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Pred_2018.csv"
dir2 <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Pred_2008.csv"
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$V2.x > 0 & Data$C2.x > 0,]

Data$diff_V_ALS = Data$V2.x - Data$V2.y
Data$diff_V_SAR = Data$P_V_5.x - Data$P_V_5.y
round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)

Data$diff_V_ALS = Data$V2.x - Data$V2.y
Data$diff_V_SAR = Data$P_7_V_5.x - Data$P_7_V_5.y
round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)

Data$diff_C_ALS = Data$C2.x - Data$C2.y
Data$diff_C_SAR = Data$P_C_5.x - Data$P_C_5.y
round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)

Data$diff_C_ALS = Data$C2.x - Data$C2.y
Data$diff_C_SAR = Data$P_7_C_5.x - Data$P_7_C_5.y
round(summary(lm(Data$diff_C_SAR~Data$diff_C_ALS))$adj.r.squared,3)



dir1 <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Pred_2018.csv"
dir2 <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Pred_2010.csv"
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$V2.x > 0 & Data$C2.x > 0,]

Data$diff_V_ALS = Data$V2.x - Data$V2.y
Data$diff_V_SAR = Data$P_V_5.x - Data$P_V_5.y
round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)

Data$diff_C_ALS = Data$C2.x - Data$C2.y
Data$diff_C_SAR = Data$P_C_5.x - Data$P_C_5.y
round(summary(lm(Data$diff_C_SAR~Data$diff_C_ALS))$adj.r.squared,3)




dir1 <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Pred_2010.csv"
dir2 <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Pred_2008.csv"
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$V2.x > 0 & Data$C2.x > 0,]

Data$diff_V_ALS = Data$V2.x - Data$V2.y
Data$diff_V_SAR = Data$P_V_5.x - Data$P_V_5.y
round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)

Data$diff_C_ALS = Data$C2.x - Data$C2.y
Data$diff_C_SAR = Data$P_C_5.x - Data$P_C_5.y
round(summary(lm(Data$diff_C_SAR~Data$diff_C_ALS))$adj.r.squared,3)

################################################################

#geom_violin change volume SAR ~ LIDAR
out = "E:\\ChangMap\\CHM\\DB_20210905\\Figure\\change_volume_2018_2010.jpg"
dir1 <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Pred_2018.csv"
dir2 <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Pred_2010.csv"
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$V2.x > 0 & Data$C2.x > 0,]
Data$diff_V2_ALS_5 = Data$V2.x - Data$V2.y
Data$diff_V2_SAR_5 = Data$P_V_5.x - Data$P_V_5.y
breakbin = seq(-12000,12000,3000)
Data$group <- cut(Data$diff_V2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
ggplot(Data, aes(x=diff_V2_ALS_5, y=diff_V2_SAR_5, group = group)) + 
    geom_violin(trim=FALSE, fill="gray")+
    labs(
        x = paste0("ALS volume change (5*5)"),
        y = paste0("Bayesian WCM volume change (5*5)")) +
    theme_bw()+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=20)) +
    theme(plot.title = element_text(hjust = 0.5))
ggsave(out,height=12, width=24, dpi=600)

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$V2.x > 0 & Data$C2.x > 0,]
Data$diff_V_ALS = Data$V1.x - Data$V1.y
Data$diff_V_SAR = Data$P_V.x - Data$P_V.y
breakbin = seq(-1500,1500,300)
Data$group <- cut(Data$diff_V_ALS,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p1<- ggplot(Data, aes(x=diff_V_ALS, y=diff_V_SAR, group = group)) + 
    geom_violin(trim=FALSE, fill="gray")+
    labs(
        x = paste0("ALS volume change"),
        y = paste0("Bayesian WCM volume change")) +
    theme_bw()+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=20)) +
    theme(plot.title = element_text(hjust = 0.5))



ggarrange(p1,p2,nrow=2)

ggsave(out,height=12, width=24, dpi=600)


#geom_violin change cover SAR ~ LIDAR
out = "E:\\ChangMap\\CHM\\DB_20210921\\Figure_150\\change_cover_2018_2010.jpg"
dir1 <- "E:\\ChangMap\\CHM\\DB_20210921\\DB_csv_150\\Merge_2018.csv"
dir2 <- "E:\\ChangMap\\CHM\\DB_20210921\\DB_csv_150\\Merge_2010.csv"
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)*2
round(summary(lm(Data$diff_C2_ALS_5~Data$diff_C2_SAR_5))$adj.r.squared,3)
plot(Data$diff_C2_ALS_5~Data$diff_C2_SAR_5)
breakbin = round(seq(-0.6,0.6,0.1),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_violin(trim=FALSE, fill="gray")+
    labs(
        x = paste0("ALS cover change (5*5)"),
        y = paste0("Bayesian WCM cover change (5*5)")) +
    theme_bw()+
    ylim(-0.6, 0.6)+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=20)) +
    theme(plot.title = element_text(hjust = 0.5))
ggsave(out,height=12, width=24, dpi=600)


#scatterplot

#scatterplot -- individual
title <- "scatterplot_volume_2018"
i <- "E:\\ChangMap\\CHM\\DB_20210905\\DB_csv\\Pred_2018.csv"
print(i)
Data = read.csv(i,header=T)
Data <- Data[Data$P_C_5 > 0,]
x <- Data$V2
y <- Data$P_V_5
out = file.path("E:\\ChangMap\\CHM\\DB_20210905\\Figure",paste0(title,".jpg"))
ggplot(Data, aes(x=x, y=y))+ 
    ylim(0,max(y))+
    geom_pointdensity()+
    scale_color_viridis(direction = 1)+
    labs(title=title,
         x = paste0("LiDAR CHM cover change"),
         y = paste0("ALOS backscatter cover change")) +
    theme(text=element_text(size=20)) +
    theme(plot.title = element_text(hjust = 0.5))
#ggsave(out, height=15, width=15, dpi=300)





#bias boxplot cover
biomass <- "1.2"
dir <- "E:\\ChangMap\\CHM\\DB_20210923\\DB_csv_1_2\\Merge_2010.csv"
out <- file.path("E:\\ChangMap\\CHM\\DB_20210923\\Figure",
                 paste0("figure bias cover 2010 ",biomass,".jpg"))
Data = read.csv(dir,header=T)
Data <- Data[Data$C2 > 0 & Data$C2 < 1,]

x <- Data$C2
y<- Data$P_C_5
round(summary(lm(y~x))$adj.r.squared,3)

Data$diff <- y-x
breakbin = seq(0,1,0.1)
Data$group_RH <- cut(x,breaks = breakbin
                     ,dig.lab=1)
table(Data$group_RH)

p1<- ggplot(Data, aes(x=C2, y=diff, group=group_RH)) + 
    stat_boxplot(geom ='errorbar', width = 0.1) +
    geom_boxplot(width = 1)+
    geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
    coord_cartesian(ylim = c(-1, 0.6))+
    scale_y_continuous(minor_breaks = seq(-1, 0.6, 0.2),breaks = seq(-1, 0.6, 0.2))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme_bw()+
    labs(x="ALS Canopy cover (5*5)",y="Bias (m)")+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))

a <- tapply(Data$diff, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$C2, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- seq(0,0.9,0.1)
l <- cbind(x,RB,table(Data$group_RH))
df <- data.frame(l)
df <- head(df,-1)
p2<- ggplot(df, aes(x=x+0.05, y=RB)) + 
    geom_bar(stat='identity')+
    geom_text(aes(label = V3,y=90), size = 6)+
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


ggarrange(p1,p2)

ggsave(out,height=12, width=24, dpi=600)




#geom_violin change cover SAR ~ LIDAR
biomass <- "1.4"
out <- file.path("E:\\ChangMap\\CHM\\DB_20210923\\Figure",
                 paste0("change_cover_2010_2008 ",biomass,".jpg"))

dir1 <- "E:\\ChangMap\\CHM\\DB_20210923\\DB_csv_1_4\\Merge_2010.csv"
dir2 <- "E:\\ChangMap\\CHM\\DB_20210923\\DB_csv_1_4\\Merge_2008.csv"
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)*2
round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3)
breakbin = round(seq(-0.6,0.6,0.1),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_violin(trim=FALSE, fill="gray")+
    labs(
        x = paste0("ALS cover change (5*5)"),
        y = paste0("Bayesian WCM cover change (5*5)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-0.6, 0.6))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=20)) +
    theme(plot.title = element_text(hjust = 0.5))

ggsave(out,height=12, width=12, dpi=600)


#stats ----2021 09 26
#individual model volume
ind_volume_linear <- function(){
    #2007 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data = read.csv(i,header=T)
    x <- Data$V2
    y<- Data$HV
    reg <- lm(y ~ x, data = Data)
    R2_HV_2010 <- round(summary(reg)$adj.r.squared,3)
    #2017 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data = read.csv(i,header=T)
    x <- Data$V2
    y<- Data$HH
    reg <- lm(y ~ x, data = Data)
    R2_HH_2010 <- round(summary(reg)$adj.r.squared,3)
    #2017 HH 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data = read.csv(i,header=T)
    x <- Data$V2
    y<- Data$HV
    reg <- lm(y ~ log(x), data = Data)
    R2_HV_2010 <- round(summary(reg)$adj.r.squared,3)
    #2017 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data = read.csv(i,header=T)
    x <- Data$V2
    y<- Data$HH
    reg <- lm(y ~ log(x), data = Data)
    R2_HH_2010 <- round(summary(reg)$adj.r.squared,3)
    #2017 HH 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data = read.csv(i,header=T)
    x <- Data$V2
    y<- Data$HV
    mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                    start = list(a=-25, b=-12, c=1e-3))
    y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
        coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
    R2_HV_2010 <- round(cor(y,y_sim)^2,3)
    #2017 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data = read.csv(i,header=T)
    x <- Data$V2
    y<- Data$HH
    mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                    start = list(a=-25, b=-12, c=1e-3))
    y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
        coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
    R2_HH_2010 <- round(cor(y,y_sim)^2,3)
    #2017 HH 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
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

#individual model cover
ind_cover_linear <- function(){
    #2007 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data = read.csv(i,header=T)
    x <- Data$C2
    y<- Data$HV
    reg <- lm(y ~ x, data = Data)
    R2_HV_2010 <- round(summary(reg)$adj.r.squared,3)
    #2017 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data = read.csv(i,header=T)
    x <- Data$C2
    y<- Data$HH
    reg <- lm(y ~ x, data = Data)
    R2_HH_2010 <- round(summary(reg)$adj.r.squared,3)
    #2017 HH 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
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
ind_cover_linear()
ind_cover_log <- function(){
    #2007 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data = read.csv(i,header=T)
    Data <- Data[Data$C2 > 0,]
    x <- Data$C2
    y<- Data$HV
    reg <- lm(y ~ log(x), data = Data)
    R2_HV_2010 <- round(summary(reg)$adj.r.squared,3)
    #2017 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data = read.csv(i,header=T)
    Data <- Data[Data$C2 > 0,]
    x <- Data$C2
    y<- Data$HH
    reg <- lm(y ~ log(x), data = Data)
    R2_HH_2010 <- round(summary(reg)$adj.r.squared,3)
    #2017 HH 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data = read.csv(i,header=T)
    x <- Data$C2
    y<- Data$HV
    mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                    start = list(a=-20, b=-7, c=2))
    y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
        coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
    R2_HV_2010 <- round(cor(y,y_sim)^2,3)
    #2017 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
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
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data = read.csv(i,header=T)
    x <- Data$C2
    y<- Data$HH
    mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                    start = list(a=-20, b=-7, c=2))
    y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
        coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
    R2_HH_2010 <- round(cor(y,y_sim)^2,3)
    #2017 HH 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
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

#inverted individual model volume
inverted_volume_linear <- function(){
    #2007 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
    Data = read.csv(i,header=T)
    x <- Data$V2
    y<- Data$HV7
    reg <- lm(x ~ y, data = Data)
    r2 <- round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2008 HV 
    x <- Data$V2
    y<- Data$HV
    reg <- lm(x ~ y, data = Data)
    r2 <- round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2010 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data = read.csv(i,header=T)
    x <- Data$V2
    y<- Data$HV
    reg <- lm(x ~ y, data = Data)
    r2 <- round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2017 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
    Data = read.csv(i,header=T)
    x <- Data$V2
    y<- Data$HV7
    reg <- lm(x ~ y, data = Data)
    r2 <- round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2018 HV 
    x <- Data$V2
    y<- Data$HV
    reg <- lm(x ~ y, data = Data)
    r2 <- round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
}

inverted_volume_log <- function(){
    #2007 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
    Data = read.csv(i,header=T)
    Data <- na.omit(Data)
    y <- Data$V2 + 7000
    x <- Data$HV7
    #volume a=80000,b=0.1
    mult_nls <- nls(y ~ a*exp(b*x), start = list(a=80000, b=0.1))
    pred <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*x)
    r2 <- round(cor(y,pred)^2,3)
    bias <- mean(pred - y)
    RB<- 100*mean(pred - y)/mean(y-7000)
    RMSE<- sqrt(mean((pred - y)^2))
    RRMSE<- 100*sqrt(mean((pred - y)^2))/mean(y-7000)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2008 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
    Data = read.csv(i,header=T)
    Data <- na.omit(Data)
    y <- Data$V2 + 7000
    x <- Data$HV
    #volume a=80000,b=0.1
    mult_nls <- nls(y ~ a*exp(b*x), start = list(a=80000, b=0.1))
    pred <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*x)
    r2 <- round(cor(y,pred)^2,3)
    bias <- mean(pred - y)
    RB<- 100*mean(pred - y)/mean(y-7000)
    RMSE<- sqrt(mean((pred - y)^2))
    RRMSE<- 100*sqrt(mean((pred - y)^2))/mean(y-7000)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    
    #2010 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data = read.csv(i,header=T)
    Data <- na.omit(Data)
    y <- Data$V2 + 7000
    x <- Data$HV
    #volume a=80000,b=0.1
    mult_nls <- nls(y ~ a*exp(b*x), start = list(a=80000, b=0.1))
    pred <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*x)
    r2 <- round(cor(y,pred)^2,3)
    bias <- mean(pred - y)
    RB<- 100*mean(pred - y)/mean(y-7000)
    RMSE<- sqrt(mean((pred - y)^2))
    RRMSE<- 100*sqrt(mean((pred - y)^2))/mean(y-7000)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    
    #2017 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
    Data = read.csv(i,header=T)
    Data <- na.omit(Data)
    y <- Data$V2 + 7000
    x <- Data$HV7
    #volume a=80000,b=0.1
    mult_nls <- nls(y ~ a*exp(b*x), start = list(a=80000, b=0.1))
    pred <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*x)
    r2 <- round(cor(y,pred)^2,3)
    bias <- mean(pred - y)
    RB<- 100*mean(pred - y)/mean(y-7000)
    RMSE<- sqrt(mean((pred - y)^2))
    RRMSE<- 100*sqrt(mean((pred - y)^2))/mean(y-7000)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2018 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
    Data = read.csv(i,header=T)
    Data <- na.omit(Data)
    y <- Data$V2 + 7000
    x <- Data$HV
    #volume a=80000,b=0.1
    mult_nls <- nls(y ~ a*exp(b*x), start = list(a=80000, b=0.1))
    pred <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*x)
    r2 <- round(cor(y,pred)^2,3)
    bias <- mean(pred - y)
    RB<- 100*mean(pred - y)/mean(y-7000)
    RMSE<- sqrt(mean((pred - y)^2))
    RRMSE<- 100*sqrt(mean((pred - y)^2))/mean(y-7000)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
}

inverted_volume_wcm <- function(){
    #2007
    dir <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2008.csv"
    Data = read.csv(dir,header=T)
    x <- Data$V2
    y<- Data$P_7_V_5
    reg <- lm(y~x)
    r2<-round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2008
    x <- Data$V2
    y<- Data$P_V_5
    reg <- lm(y~x)
    r2<-round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2010
    dir <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2010.csv"
    Data = read.csv(dir,header=T)
    x <- Data$V2
    y<- Data$P_V_5
    reg <- lm(y~x)
    r2<-round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2017
    dir <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2018.csv"
    Data = read.csv(dir,header=T)
    x <- Data$V2
    y<- Data$P_7_V_5
    reg <- lm(y~x)
    r2<-round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2018
    x <- Data$V2
    y<- Data$P_V_5
    reg <- lm(y~x)
    r2<-round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
}

#inverted individual model cover
inverted_cover_linear <- function(){
    #2007 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
    Data = read.csv(i,header=T)
    x <- Data$C2
    y<- Data$HV7
    reg <- lm(x ~ y, data = Data)
    r2 <- round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2008 HV 
    x <- Data$C2
    y<- Data$HV
    reg <- lm(x ~ y, data = Data)
    r2 <- round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2010 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data = read.csv(i,header=T)
    x <- Data$C2
    y<- Data$HV
    reg <- lm(x ~ y, data = Data)
    r2 <- round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2017 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
    Data = read.csv(i,header=T)
    x <- Data$C2
    y<- Data$HV7
    reg <- lm(x ~ y, data = Data)
    r2 <- round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2018 HV 
    x <- Data$C2
    y<- Data$HV
    reg <- lm(x ~ y, data = Data)
    r2 <- round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
}

inverted_cover_log <- function(){
    #2007 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
    Data = read.csv(i,header=T)
    Data <- na.omit(Data)
    y <- Data$V2 + 7000
    x <- Data$HV7
    #cover a=200, b=0.4
    mult_nls <- nls(y ~ a*exp(b*x), start = list(a=200, b=0.4))
    pred <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*x)
    r2 <- round(cor(y,pred)^2,3)
    bias <- mean(pred - y)
    RB<- 100*mean(pred - y)/mean(y-7000)
    RMSE<- sqrt(mean((pred - y)^2))
    RRMSE<- 100*sqrt(mean((pred - y)^2))/mean(y-7000)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2008 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
    Data = read.csv(i,header=T)
    Data <- na.omit(Data)
    y <- Data$V2 + 7000
    x <- Data$HV
    #cover a=200, b=0.4
    mult_nls <- nls(y ~ a*exp(b*x), start = list(a=200, b=0.4))
    pred <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*x)
    r2 <- round(cor(y,pred)^2,3)
    bias <- mean(pred - y)
    RB<- 100*mean(pred - y)/mean(y-7000)
    RMSE<- sqrt(mean((pred - y)^2))
    RRMSE<- 100*sqrt(mean((pred - y)^2))/mean(y-7000)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    
    #2010 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data = read.csv(i,header=T)
    Data <- na.omit(Data)
    y <- Data$V2 + 7000
    x <- Data$HV
    #cover a=200, b=0.4
    mult_nls <- nls(y ~ a*exp(b*x), start = list(a=200, b=0.4))
    pred <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*x)
    r2 <- round(cor(y,pred)^2,3)
    bias <- mean(pred - y)
    RB<- 100*mean(pred - y)/mean(y-7000)
    RMSE<- sqrt(mean((pred - y)^2))
    RRMSE<- 100*sqrt(mean((pred - y)^2))/mean(y-7000)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    
    #2017 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
    Data = read.csv(i,header=T)
    Data <- na.omit(Data)
    y <- Data$V2 + 7000
    x <- Data$HV7
    #cover a=200, b=0.4
    mult_nls <- nls(y ~ a*exp(b*x), start = list(a=200, b=0.4))
    pred <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*x)
    r2 <- round(cor(y,pred)^2,3)
    bias <- mean(pred - y)
    RB<- 100*mean(pred - y)/mean(y-7000)
    RMSE<- sqrt(mean((pred - y)^2))
    RRMSE<- 100*sqrt(mean((pred - y)^2))/mean(y-7000)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2018 HV 
    i <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
    Data = read.csv(i,header=T)
    Data <- na.omit(Data)
    y <- Data$V2 + 7000
    x <- Data$HV
    #cover a=200, b=0.4
    mult_nls <- nls(y ~ a*exp(b*x), start = list(a=200, b=0.4))
    pred <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*x)
    r2 <- round(cor(y,pred)^2,3)
    bias <- mean(pred - y)
    RB<- 100*mean(pred - y)/mean(y-7000)
    RMSE<- sqrt(mean((pred - y)^2))
    RRMSE<- 100*sqrt(mean((pred - y)^2))/mean(y-7000)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
}

inverted_cover_wcm <- function(){
    #2007
    dir <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2008.csv"
    Data = read.csv(dir,header=T)
    x <- Data$C2
    y<- Data$P_7_C_5
    reg <- lm(y~x)
    r2<-round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2008
    x <- Data$C2
    y<- Data$P_C_5
    reg <- lm(y~x)
    r2<-round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2010
    dir <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2010.csv"
    Data = read.csv(dir,header=T)
    x <- Data$C2
    y<- Data$P_C_5
    reg <- lm(y~x)
    r2<-round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2017
    dir <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2018.csv"
    Data = read.csv(dir,header=T)
    x <- Data$C2
    y<- Data$P_7_C_5
    reg <- lm(y~x)
    r2<-round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
    #2018
    x <- Data$C2
    y<- Data$P_C_5
    reg <- lm(y~x)
    r2<-round(summary(reg)$adj.r.squared,3)
    pred <- predict(reg)
    bias <- mean(pred - x)
    RB<- 100*mean(pred - x)/mean(x)
    RMSE<- sqrt(mean((pred - x)^2))
    RRMSE<- 100*sqrt(mean((pred - x)^2))/mean(x)
    cat(r2,bias,RB,RMSE,RRMSE,sep="\n")
}

#change maps diff ALS ~ diff SAR - volume

change_linear <- function(){
    #2018-2008
    dir1 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
    dir2 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
    Data1 = read.csv(dir1,header=T)
    Data2 = read.csv(dir2,header=T)
    Data = merge(Data1,Data2,by=c("Merge_ID"))
    #2018-2008 volume
    x <- Data$V2.x
    y<- Data$HH.x
    reg <- lm(x ~ y, data = Data)
    pred.x <- predict(reg)
    x <- Data$V2.y
    y<- Data$HH.y
    reg <- lm(x ~ y, data = Data)
    pred.y <- predict(reg)
    Data$diff_V_ALS = Data$V2.x - Data$V2.y
    Data$diff_V_SAR = pred.x - pred.y
    V_2018_2008<-round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    RV_2018_2008<-sqrt(mean((Data$diff_V_SAR - Data$diff_V_ALS)^2))
    #2018-2008 cover
    x <- Data$C2.x
    y<- Data$HH.x
    reg <- lm(x ~ y, data = Data)
    pred.x <- predict(reg)
    x <- Data$C2.y
    y<- Data$HH.y
    reg <- lm(x ~ y, data = Data)
    pred.y <- predict(reg)
    Data$diff_V_ALS = Data$C2.x - Data$C2.y
    Data$diff_V_SAR = pred.x - pred.y
    C_2018_2008<-round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    RC_2018_2008<-sqrt(mean((Data$diff_V_SAR - Data$diff_V_ALS)^2))
    dir1 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
    dir2 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data1 = read.csv(dir1,header=T)
    Data2 = read.csv(dir2,header=T)
    Data = merge(Data1,Data2,by=c("Merge_ID"))
    #2018-2010 volume
    x <- Data$V2.x
    y<- Data$HH.x
    reg <- lm(x ~ y, data = Data)
    pred.x <- predict(reg)
    x <- Data$V2.y
    y<- Data$HH.y
    reg <- lm(x ~ y, data = Data)
    pred.y <- predict(reg)
    Data$diff_V_ALS = Data$V2.x - Data$V2.y
    Data$diff_V_SAR = pred.x - pred.y
    V_2018_2010<-round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    RV_2018_2010<-sqrt(mean((Data$diff_V_SAR - Data$diff_V_ALS)^2))
    #2018-2010 cover
    x <- Data$C2.x
    y<- Data$HH.x
    reg <- lm(x ~ y, data = Data)
    pred.x <- predict(reg)
    x <- Data$C2.y
    y<- Data$HH.y
    reg <- lm(x ~ y, data = Data)
    pred.y <- predict(reg)
    Data$diff_V_ALS = Data$C2.x - Data$C2.y
    Data$diff_V_SAR = pred.x - pred.y
    C_2018_2010<-round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    RC_2018_2010<-sqrt(mean((Data$diff_V_SAR - Data$diff_V_ALS)^2))
    dir1 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    dir2 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
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
    V_2010_2008<-round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    RV_2010_2008<-sqrt(mean((Data$diff_V_SAR - Data$diff_V_ALS)^2))
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
    C_2010_2008<-round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    RC_2010_2008<-sqrt(mean((Data$diff_V_SAR - Data$diff_V_ALS)^2))
    cat(V_2018_2008,C_2018_2008,V_2018_2010,C_2018_2010,V_2010_2008,C_2010_2008,sep="\n")
    cat(RV_2018_2008,RC_2018_2008,RV_2018_2010,RC_2018_2010,RV_2010_2008,RC_2010_2008,sep="\n")
}

change_log <- function(){
    #2018-2008
    dir1 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
    dir2 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
    Data1 = read.csv(dir1,header=T)
    Data2 = read.csv(dir2,header=T)
    Data = merge(Data1,Data2,by=c("Merge_ID"))
    Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
    #2018-2008 volume
    x <- Data$C2.x
    y <- Data$HV.x
    mult_nls <- nls(x~ a*exp(b*y), start = list(a=80000, b=0.1))
    pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
    x <- Data$C2.y
    y <- Data$HV.y
    mult_nls <- nls(x~ a*exp(b*y), start = list(a=80000, b=0.1))
    pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
    Data$diff_V_ALS = Data$V2.x - Data$V2.y
    Data$diff_V_SAR = pred.x - pred.y
    V_2018_2008 <- round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    #2017-2007 volume 
    x <- Data$C2.x
    y <- Data$HV7.x
    mult_nls <- nls(x~ a*exp(b*y), start = list(a=80000, b=0.1))
    pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
    x <- Data$C2.y
    y <- Data$HV7.y
    mult_nls <- nls(x~ a*exp(b*y), start = list(a=80000, b=0.1))
    pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
    Data$diff_V_ALS = Data$V2.x - Data$V2.y
    Data$diff_V_SAR = pred.x - pred.y
    V_2017_2007 <- round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    
    #2018-2008 cover
    x <- Data$C2.x
    y <- Data$HV.x
    mult_nls <- nls(x~ a*exp(b*y), start = list(a=20, b=0.2))
    pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
    x <- Data$C2.y
    y <- Data$HV.y
    mult_nls <- nls(x~ a*exp(b*y), start = list(a=20, b=0.2))
    pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
    Data$diff_V_ALS = Data$C2.x - Data$C2.y
    Data$diff_V_SAR = pred.x - pred.y
    C_2018_2008 <- round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    #2017-2007 cover 
    x <- Data$C2.x
    y <- Data$HV7.x
    mult_nls <- nls(x~ a*exp(b*y), start = list(a=100, b=0.3))
    pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
    x <- Data$C2.y
    y <- Data$HV7.y
    mult_nls <- nls(x~ a*exp(b*y), start = list(a=200, b=0.4))
    pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
    Data$diff_V_ALS = Data$C2.x - Data$C2.y
    Data$diff_V_SAR = pred.x - pred.y
    C_2017_2007 <- round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    
    dir1 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
    dir2 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    Data1 = read.csv(dir1,header=T)
    Data2 = read.csv(dir2,header=T)
    Data = merge(Data1,Data2,by=c("Merge_ID"))
    #2018-2010 volume
    x <- Data$C2.x
    y <- Data$HV.x
    mult_nls <- nls(x~ a*exp(b*y), start = list(a=80000, b=0.1))
    pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
    x <- Data$C2.y
    y <- Data$HV.y
    mult_nls <- nls(x~ a*exp(b*y), start = list(a=80000, b=0.1))
    pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
    Data$diff_V_ALS = Data$V2.x - Data$V2.y
    Data$diff_V_SAR = pred.x - pred.y
    V_2018_2010 <- round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    #2018-2010 cover
    x <- Data$C2.x
    y <- Data$HV.x
    mult_nls <- nls(x~ a*exp(b*y), start = list(a=200, b=0.4))
    pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
    x <- Data$C2.y
    y <- Data$HV.y
    mult_nls <- nls(x~ a*exp(b*y), start = list(a=200, b=0.4))
    pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
    Data$diff_V_ALS = Data$C2.x - Data$C2.y
    Data$diff_V_SAR = pred.x - pred.y
    C_2018_2010 <- round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    
    dir1 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
    dir2 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
    Data1 = read.csv(dir1,header=T)
    Data2 = read.csv(dir2,header=T)
    Data = merge(Data1,Data2,by=c("Merge_ID"))
    #2010-2008 volume
    x <- Data$C2.x
    y <- Data$HV.x
    mult_nls <- nls(x~ a*exp(b*y), start = list(a=80000, b=0.1))
    pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
    x <- Data$C2.y
    y <- Data$HV.y
    mult_nls <- nls(x~ a*exp(b*y), start = list(a=80000, b=0.1))
    pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
    Data$diff_V_ALS = Data$V2.x - Data$V2.y
    Data$diff_V_SAR = pred.x - pred.y
    V_2010_2008 <- round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    
    #2010-2008 cover
    x <- Data$C2.x
    y <- Data$HV.x
    mult_nls <- nls(x~ a*exp(b*y), start = list(a=200, b=0.4))
    pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
    x <- Data$C2.y
    y <- Data$HV.y
    mult_nls <- nls(x~ a*exp(b*y), start = list(a=200, b=0.4))
    pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
    Data$diff_V_ALS = Data$C2.x - Data$C2.y
    Data$diff_V_SAR = pred.x - pred.y
    C_2010_2008 <- round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    cat(V_2018_2008,C_2018_2008,V_2017_2007,C_2017_2007,
        V_2018_2010,C_2018_2010,V_2010_2008,C_2010_2008,sep="\n")
}

change_wcm <- function(){
    #2018-2008
    dir1 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2018.csv"
    dir2 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2008.csv"
    Data1 = read.csv(dir1,header=T)
    Data2 = read.csv(dir2,header=T)
    Data = merge(Data1,Data2,by=c("Merge_ID"))
    Data$diff_V_ALS = Data$V2.x - Data$V2.y
    Data$diff_V_SAR = Data$P_V_5.x - Data$P_V_5.y
    round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    Data$diff_V_ALS = Data$V2.x - Data$V2.y
    Data$diff_V_SAR = Data$P_7_V_5.x - Data$P_7_V_5.y
    round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    Data$diff_C_ALS = Data$C2.x - Data$C2.y
    Data$diff_C_SAR = Data$P_C_5.x - Data$P_C_5.y
    round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    
    Data$diff_C_ALS = Data$C2.x - Data$C2.y
    Data$diff_C_SAR = Data$P_7_C_5.x - Data$P_7_C_5.y
    round(summary(lm(Data$diff_C_SAR~Data$diff_C_ALS))$adj.r.squared,3)
    
    dir1 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2018.csv"
    dir2 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2010.csv"
    Data1 = read.csv(dir1,header=T)
    Data2 = read.csv(dir2,header=T)
    Data = merge(Data1,Data2,by=c("Merge_ID"))
    Data$diff_V_ALS = Data$V2.x - Data$V2.y
    Data$diff_V_SAR = Data$P_V_5.x - Data$P_V_5.y
    round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    Data$diff_C_ALS = Data$C2.x - Data$C2.y
    Data$diff_C_SAR = Data$P_C_5.x - Data$P_C_5.y
    round(summary(lm(Data$diff_C_SAR~Data$diff_C_ALS))$adj.r.squared,3)
    
    dir1 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2010.csv"
    dir2 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2008.csv"
    Data1 = read.csv(dir1,header=T)
    Data2 = read.csv(dir2,header=T)
    Data = merge(Data1,Data2,by=c("Merge_ID"))
    Data$diff_V_ALS = Data$V2.x - Data$V2.y
    Data$diff_V_SAR = Data$P_V_5.x - Data$P_V_5.y
    round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
    Data$diff_C_ALS = Data$C2.x - Data$C2.y
    Data$diff_C_SAR = Data$P_C_5.x - Data$P_C_5.y
    round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3)
}

#bias boxplot cover
boxplot_cover <- function(year){
    dir <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_",year,".csv")
    out <- file.path("E:\\ChangMap\\CHM\\DB_20210926\\Figure",
                     paste0("figure bias cover ",year,".jpg"))
    Data = read.csv(dir,header=T)
    Data <- Data[Data$C2 > 0 & Data$C2 < 1,]
    
    x <- Data$C2
    y<- Data$P_C_5
    round(summary(lm(y~x))$adj.r.squared,3)
    
    Data$diff <- y-x
    breakbin = seq(0,1,0.1)
    Data$group_RH <- cut(x,breaks = breakbin
                         ,dig.lab=1)
    table(Data$group_RH)
    
    p1<- ggplot(Data, aes(x=C2, y=diff, group=group_RH)) + 
        stat_boxplot(geom ='errorbar', width = 0.1) +
        geom_boxplot(width = 1)+
        geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
        coord_cartesian(ylim = c(-1, 0.6))+
        scale_y_continuous(minor_breaks = seq(-1, 0.6, 0.2),breaks = seq(-1, 0.6, 0.2))+
        scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
        theme_bw()+
        labs(x="ALS Canopy cover (5*5)",y="Bias (m)")+
        theme(legend.position = "none")+
        theme(legend.title = element_blank())+
        theme(text=element_text(size=25))+
        theme(plot.title = element_text(hjust = 0.5))
    
    a <- tapply(Data$diff, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
    b <- tapply(Data$C2, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
    RB <- a/b*100
    
    x <- seq(0,0.9,0.1)
    l <- cbind(x,RB,table(Data$group_RH))
    df <- data.frame(l)
    df <- head(df,-1)
    p2<- ggplot(df, aes(x=x+0.05, y=RB)) + 
        geom_bar(stat='identity')+
        geom_text(aes(label = V3,y=90), size = 6)+
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
    
    
    ggarrange(p1,p2)
    
    ggsave(out,height=12, width=24, dpi=600)
    
    
}
boxplot_volume <- function(year){
    dir <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_",year,".csv")
    out <- file.path("E:\\ChangMap\\CHM\\DB_20210926\\Figure",
                     paste0("figure bias volume ",year,".jpg"))
    Data = read.csv(dir,header=T)
    Data <- Data[Data$V2 > 0 & Data$V2 < 24000,]
    x <- Data$V2
    y<- Data$P_V_5
    Data$diff <- y-x
    breakbin = seq(0,24000,3000)
    Data$group_RH <- cut(x,breaks = breakbin
                         ,dig.lab=1)
    table(Data$group_RH)
    
    p1<- ggplot(Data, aes(x=V2, y=diff, group=group_RH)) + 
        stat_boxplot(geom ='errorbar', width = 0.1) +
        geom_boxplot(width = 1)+
        geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
        coord_cartesian(ylim = c(-15000,10000))+
        scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
        scale_y_continuous(minor_breaks = seq(-15000,10000,5000),
                           breaks = seq(-15000,10000,5000))+
        theme_bw()+
        labs(title = "Bayesian WCM vs. ALS volume (5*5)",
             x = "",y="Bias (m)")+
        theme(legend.position = "none")+
        theme(legend.title = element_blank())+
        theme(text=element_text(size=25))+
        theme(plot.title = element_text(hjust = 0.5))
    
    a <- tapply(Data$diff, cut(Data$V2,breaks = breakbin, dig.lab=1), mean)
    b <- tapply(Data$V2, cut(Data$V2,breaks = breakbin, dig.lab=1), mean)
    RB <- a/b*100
    
    x <- seq(0,24000,3000)
    l <- cbind(x,RB,table(Data$group_RH))
    df <- data.frame(l)
    df <- head(df,-1)
    p2<- ggplot(df, aes(x=x+1500, y=RB)) + 
        geom_bar(stat='identity')+
        geom_text(aes(label = V3,y=120), size = 6)+
        theme_bw()+
        coord_cartesian(ylim = c(-70,120))+
        scale_y_continuous(minor_breaks = seq(-70, 120, 10),breaks = seq(-70, 120, 10))+
        scale_x_continuous(minor_breaks = seq(0,30000,3000),breaks = seq(0,30000,3000))+
        labs(x="ALS Volume (5*5)", 
             y="")+
        theme(legend.position = "none")+
        theme(legend.title = element_blank())+
        theme(text=element_text(size=25))+
        theme(plot.title = element_text(hjust = 0.5))
    
    ggarrange(p1,p2)
    
    ggsave(out,height=12, width=24, dpi=600)
    
    
}

violin_cover <- function(year1,year2){
    out <- file.path("E:\\ChangMap\\CHM\\DB_20210926\\Figure",
                     paste0("change_cover_",year1,"_",year2,".jpg"))
    
    dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_",year1,".csv")
    dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_",year2,".csv")
    Data1 = read.csv(dir1,header=T)
    Data2 = read.csv(dir2,header=T)
    Data = merge(Data1,Data2,by=c("Merge_ID"))
    Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
    Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
    Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)
    print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
    breakbin = round(seq(-0.6,0.6,0.1),2)
    Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
    Data <- na.omit(Data)
    ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
        geom_violin(trim=FALSE, fill="gray")+
        labs(
            x = paste0("ALS cover change (5*5)"),
            y = paste0("Bayesian WCM cover change (5*5)")) +
        theme_bw()+
        coord_cartesian(ylim = c(-0.5, 0.5))+
        scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(legend.title = element_blank())+
        theme(text=element_text(size=20)) +
        theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(out,height=12, width=12, dpi=600)
    
}
violin_volume <- function(year1,year2){
    out <- file.path("E:\\ChangMap\\CHM\\DB_20210926\\Figure",
                     paste0("change_volume_",year1,"_",year2,".jpg"))
    
    dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_",year1,".csv")
    dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_",year2,".csv")
    Data1 = read.csv(dir1,header=T)
    Data2 = read.csv(dir2,header=T)
    Data = merge(Data1,Data2,by=c("Merge_ID"))
    Data <- Data[Data$V2.x > 0,]
    Data$diff_V2_ALS_5 = Data$V2.x - Data$V2.y
    Data$diff_V2_SAR_5 = Data$P_V_5.x - Data$P_V_5.y
    breakbin = seq(-12000,12000,3000)
    Data$group <- cut(Data$diff_V2_ALS_5,breaks = breakbin,dig.lab=1)
    Data <- na.omit(Data)
    ggplot(Data, aes(x=diff_V2_ALS_5, y=diff_V2_SAR_5, group = group)) + 
        geom_violin(trim=FALSE, fill="gray")+
        labs(
            x = paste0("ALS volume change (5*5)"),
            y = paste0("Bayesian WCM volume change (5*5)")) +
        theme_bw()+
        coord_cartesian(ylim = c(-8000, 8000))+
        scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(legend.title = element_blank())+
        theme(text=element_text(size=20)) +
        theme(plot.title = element_text(hjust = 0.5))
    ggsave(out,height=12, width=24, dpi=600)
}

scatterplot_volume <- function(year){
    #scatterplot -- individual
    title <- paste0("scatterplot_volume_",year)
    out <- file.path("E:\\ChangMap\\CHM\\DB_20210926\\Figure",
                     paste0("scatterplot_volume_",year,".jpg"))
    i <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_",year,".csv")
    print(i)
    Data = read.csv(i,header=T)
    Data <- Data[Data$V2 > 0,]
    x <- Data$V2
    y <- Data$HV
    
    mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                    start = list(a=-25, b=-12, c=9e-5))
    y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
        coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
    R2 <- round(cor(y,y_sim)^2,3)
    round(cor(y,y_sim)^2,3)
    
    reg1 <- lm(y ~ x , data = Data)
    reg3 <- lm(y ~ log(x), data = Data)
    
    ggplot(Data, aes(x=x, y=y))+ 
        ylim(min(y),max(y))+
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
        annotate("text", x=12000, y=-23, hjust = 0,color="black",size = 10,
                 label= paste(expression(Water~cloud~model~R^2),": ",round(R2,3)), parse=TRUE) + 
        annotate("text", x=12000, y=-24,hjust = 0,color="purple",size = 10,
                 label= paste(expression(Linear~model~R^2),": ",round(summary(reg1)$adj.r.squared,3)), parse=TRUE) + 
        annotate("text", x=12000, y=-25,hjust = 0,color="blue",size = 10,
                 label= paste(expression(Logarithmic~model~R^2),": ",round(summary(reg3)$adj.r.squared,3)), parse=TRUE) + 
        labs(title=title,
             x = paste0("LiDAR CHM volume"),
             y = paste0("ALOS backscatter")) +
        theme(text=element_text(size=20)) +
        theme(plot.title = element_text(hjust = 0.5))
    ggsave(out, height=15, width=15, dpi=300)
}
scatterplot_cover <- function(year){
    #scatterplot -- individual
    title <- paste0("scatterplot_cover_",year)
    out <- file.path("E:\\ChangMap\\CHM\\DB_20210926\\Figure",
                     paste0("scatterplot_cover_",year,".jpg"))
    i <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_",year,".csv")
    print(i)
    Data = read.csv(i,header=T)
    Data <- Data[Data$C2 > 0,]
    
    x <- Data$C2
    y <- Data$HV
    
    mult_nls <- nls(y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                    start = list(a=-25, b=-12, c=2))
    y_sim <- coef(mult_nls)[1]*exp(-coef(mult_nls)[3]*x)+
        coef(mult_nls)[2]*(1-exp(-coef(mult_nls)[3]*x))
    R2 <- round(cor(y,y_sim)^2,3)
    round(cor(y,y_sim)^2,3)
    
    reg1 <- lm(y ~ x , data = Data)
    reg3 <- lm(y ~ log(x), data = Data)
    
    ggplot(Data, aes(x=x, y=y))+ 
        ylim(min(y),max(y))+
        geom_pointdensity()+
        scale_color_viridis(direction = 1)+
        geom_smooth(method = "nls", 
                    method.args = list(formula = y ~ a*exp(-c*x)+b*(1-exp(-c*x)), 
                                       start = list(a=-25, b=-12, c=2)),
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
        annotate("text", x=0.3, y=-24, hjust = 0,color="black",size = 10,
                 label= paste(expression(Water~cloud~model~R^2),": ",round(R2,3)), parse=TRUE) + 
        annotate("text", x=0.3, y=-25,hjust = 0,color="purple",size = 10,
                 label= paste(expression(Linear~model~R^2),": ",round(summary(reg1)$adj.r.squared,3)), parse=TRUE) + 
        annotate("text", x=0.3, y=-26,hjust = 0,color="blue",size = 10,
                 label= paste(expression(Logarithmic~model~R^2),": ",round(summary(reg3)$adj.r.squared,3)), parse=TRUE) + 
        
        labs(title=title,
             x = paste0("LiDAR CHM cover"),
             y = paste0("ALOS backscatter")) +
        theme(text=element_text(size=20)) +
        theme(plot.title = element_text(hjust = 0.5))
    ggsave(out, height=15, width=15, dpi=300)
}

# linear bias boxplot
boxplot_cover_linear <- function(year){
    dir <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_",year,".csv")
    out <- file.path("E:\\ChangMap\\CHM\\DB_20210926\\Figure",
                     paste0("figure bias cover ",year,"_linear.jpg"))
    Data = read.csv(dir,header=T)
    Data <- Data[Data$C2 > 0 & Data$C2 < 1,]
    
    x <- Data$C2
    y<- Data$HV
    
    reg <- lm(x ~ y, data = Data)
    r2 <- round(summary(reg)$adj.r.squared,3)
    y <- predict(reg)
    
    Data$diff <- y-x
    breakbin = seq(0,1,0.1)
    Data$group_RH <- cut(x,breaks = breakbin
                         ,dig.lab=1)
    table(Data$group_RH)
    
    p1<- ggplot(Data, aes(x=C2, y=diff, group=group_RH)) + 
        stat_boxplot(geom ='errorbar', width = 0.1) +
        geom_boxplot(width = 1)+
        geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
        coord_cartesian(ylim = c(-1, 0.6))+
        scale_y_continuous(minor_breaks = seq(-1, 0.6, 0.2),breaks = seq(-1, 0.6, 0.2))+
        scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
        theme_bw()+
        labs(x="ALS Canopy cover (5*5)",y="Bias (m)")+
        theme(legend.position = "none")+
        theme(legend.title = element_blank())+
        theme(text=element_text(size=25))+
        theme(plot.title = element_text(hjust = 0.5))
    
    a <- tapply(Data$diff, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
    b <- tapply(Data$C2, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
    RB <- a/b*100
    
    x <- seq(0,0.9,0.1)
    l <- cbind(x,RB,table(Data$group_RH))
    df <- data.frame(l)
    df <- head(df,-1)
    p2<- ggplot(df, aes(x=x+0.05, y=RB)) + 
        geom_bar(stat='identity')+
        geom_text(aes(label = V3,y=90), size = 6)+
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
    
    
    ggarrange(p1,p2)
    
    ggsave(out,height=12, width=24, dpi=600)
    
    
}

# log bias boxplot
boxplot_cover_log <- function(year){
    i <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_",year,".csv")
    out <- file.path("E:\\ChangMap\\CHM\\DB_20210926\\Figure",
                     paste0("figure bias cover ",year,"_log.jpg"))
    Data = read.csv(i,header=T)
    Data <- na.omit(Data)
    Data <- Data[Data$C2 > 0 & Data$C2 < 1,]
    y <- Data$C2
    x <- Data$HV
    
    mult_nls <- nls(y ~ a*exp(b*x), start = list(a=200, b=0.4))
    pred <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*x)
    
    
    y <- pred
    x <- Data$C2
    Data$diff <- y-x
    breakbin = seq(0,1,0.1)
    Data$group_RH <- cut(x,breaks = breakbin
                         ,dig.lab=1)
    table(Data$group_RH)
    
    p1<- ggplot(Data, aes(x=C2, y=diff, group=group_RH)) + 
        stat_boxplot(geom ='errorbar', width = 0.1) +
        geom_boxplot(width = 1)+
        geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
        coord_cartesian(ylim = c(-1, 0.6))+
        scale_y_continuous(minor_breaks = seq(-1, 0.6, 0.2),breaks = seq(-1, 0.6, 0.2))+
        scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
        theme_bw()+
        labs(x="ALS Canopy cover (5*5)",y="Bias (m)")+
        theme(legend.position = "none")+
        theme(legend.title = element_blank())+
        theme(text=element_text(size=25))+
        theme(plot.title = element_text(hjust = 0.5))
    
    a <- tapply(Data$diff, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
    b <- tapply(Data$C2, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
    RB <- a/b*100
    
    x <- seq(0,0.9,0.1)
    l <- cbind(x,RB,table(Data$group_RH))
    df <- data.frame(l)
    df <- head(df,-1)
    p2<- ggplot(df, aes(x=x+0.05, y=RB)) + 
        geom_bar(stat='identity')+
        geom_text(aes(label = V3,y=90), size = 6)+
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
    
    
    ggarrange(p1,p2)
    
    ggsave(out,height=12, width=24, dpi=600)
}

#subplots
boxplot_group_cover <- function(year){
    #linear
    dir <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_",year,".csv")
    Data = read.csv(dir,header=T)
    Data <- na.omit(Data)
    Data <- Data[Data$C2 > 0 & Data$C2 < 1,]
    
    x <- Data$C2
    y<- Data$HV
    
    reg <- lm(x ~ y, data = Data)
    r2 <- round(summary(reg)$adj.r.squared,3)
    y <- predict(reg)
    
    Data$diff <- y-x
    breakbin = seq(0,1,0.1)
    Data$group_RH <- cut(x,breaks = breakbin
                         ,dig.lab=1)
    table(Data$group_RH)
    
    p1<- ggplot(Data, aes(x=C2, y=diff, group=group_RH)) + 
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
    
    
    a <- tapply(Data$diff, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
    b <- tapply(Data$C2, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
    RB <- a/b*100
    
    x <- seq(0,0.9,0.1)
    l <- cbind(x,RB,table(Data$group_RH))
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
    Data <- Data[Data$C2 > 0 & Data$C2 < 1,]
    y <- Data$C2
    x <- Data$HV
    
    mult_nls <- nls(y ~ a*exp(b*x), start = list(a=200, b=0.4))
    pred <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*x)
    
    
    y <- pred
    x <- Data$C2
    Data$diff <- y-x
    breakbin = seq(0,1,0.1)
    Data$group_RH <- cut(x,breaks = breakbin
                         ,dig.lab=1)
    table(Data$group_RH)
    
    p2<- ggplot(Data, aes(x=C2, y=diff, group=group_RH)) + 
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
    
    a <- tapply(Data$diff, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
    b <- tapply(Data$C2, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
    RB <- a/b*100
    
    x <- seq(0,0.9,0.1)
    l <- cbind(x,RB,table(Data$group_RH))
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
    Data <- Data[Data$C2 > 0 & Data$C2 < 1,]
    
    x <- Data$C2
    y<- Data$P_C_5
    round(summary(lm(y~x))$adj.r.squared,3)
    
    Data$diff <- y-x
    breakbin = seq(0,1,0.1)
    Data$group_RH <- cut(x,breaks = breakbin
                         ,dig.lab=1)
    table(Data$group_RH)
    
    p3<- ggplot(Data, aes(x=C2, y=diff, group=group_RH)) + 
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
    
    a <- tapply(Data$diff, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
    b <- tapply(Data$C2, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
    RB <- a/b*100
    
    x <- seq(0,0.9,0.1)
    l <- cbind(x,RB,table(Data$group_RH))
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


#linear equations for mapping linear-fitted cover
dir <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
Data = read.csv(dir,header=T)
Data <- Data[Data$C2 > 0 & Data$C2 < 1,]

x <- Data$C2
y<- Data$HV

reg <- lm(x ~ y, data = Data)
reg

#2018
PRED <- Data$HV*0.04549 + 1.12358
#2010
PRED <- Data$HV*0.06313 + 1.42622 
#2008
PRED <- Data$HV*0.05129 + 1.15105

#log equations for mapping log-fitted cover
dir <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
Data = read.csv(dir,header=T)
Data <- Data[Data$C2 > 0 & Data$C2 < 1,]

y <- Data$C2
x <- Data$HV

#cover a=200, b=0.4
mult_nls <- nls(y ~ a*exp(b*x), start = list(a=200, b=0.4))
pred <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*x)
mult_nls

#2018
PRED <- 3.5065*exp(0.1406*Data$HV)
#2010
PRED <- 19.2171*exp(0.2443*Data$HV)
#2008
PRED <- 6.2034*exp(0.1917*Data$HV)










# ------------------------------------------------------------------------------------------------ #

#11142021
#composite plots
i_1 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
Data1 = read.csv(i_1,header=T)
Data1 <- Data1[Data1$V2 > 0,]
#Data1 <- Data1[sample(nrow(Data1), 1000), ]
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
    annotate("text", x=18000, y=-19, hjust = 0,color="black",size = 8,
             label= paste("\n",expression(WCM),": ",round(R2_1,3),"\n",
                          expression(Linear),": ",round(summary(reg1_1)$adj.r.squared,3),"\n",
                          expression(Log),": ",round(summary(reg3_1)$adj.r.squared,3)
             )) + 
    labs(x = paste0("LiDAR CHM volume (2008)"),
         y = paste0("SAR backscatter (2008)")) +
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(text=element_text(size=25)) + 
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position=c(0.7,0.15),legend.key.width=unit(3,"cm"))


i_2 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
Data2 = read.csv(i_2,header=T)
Data2 <- Data2[Data2$V2 > 0,]
#Data2 <- Data2[sample(nrow(Data2), 1000), ]
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
    annotate("text", x=20000, y=-19, hjust = 0,color="black",size = 8,
             label= paste("\n",expression(WCM),": ",round(R2_2,3),"\n",
                          expression(Linear),": ",round(summary(reg1_2)$adj.r.squared,3),"\n",
                          expression(Log),": ",round(summary(reg3_2)$adj.r.squared,3)
             )) + 
    labs(x = paste0("LiDAR CHM volume (2010)"),
         y = paste0("SAR backscatter (2010)")) +
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(text=element_text(size=25)) + 
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position=c(0.7,0.15),legend.key.width=unit(3,"cm"))


i_3 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
Data3 = read.csv(i_3,header=T)
Data3 <- Data3[Data3$V2 > 0,]
#Data3 <- Data3[sample(nrow(Data3), 1000), ]
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
    annotate("text", x=20000, y=-19, hjust = 0,color="black",size = 8,
             label= paste("\n",expression(WCM),": ",round(R2_3,3),"\n",
                          expression(Linear),": ",round(summary(reg1_3)$adj.r.squared,3),"\n",
                          expression(Log),": ",round(summary(reg3_3)$adj.r.squared,3)
             )) + 
    labs(x = paste0("LiDAR CHM volume (2018)"),
         y = paste0("SAR backscatter (2018)")) +
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(text=element_text(size=25)) + 
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position=c(0.7,0.15),legend.key.width=unit(3,"cm"))



ggarrange(p1,p2,p3,ncol=3,nrow=1) 
out = "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\figure scatterplot composite.jpg"
ggsave(out,height=9, width=18, dpi=600)





# ------------------------------------------------------------------------------------------------ #
#composite plots cover
i_1 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv"
Data1 = read.csv(i_1,header=T)

Data1 <- Data1[Data1$C2 > 0,]
x1 <- Data1$C2
y1 <- Data1$HV

mult_nls_1 <- nls(y1 ~ a*exp(-c*x1)+b*(1-exp(-c*x1)), 
                  start = list(a=-25, b=-12, c=2))
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
                                   start = list(a=-25, b=-12, c=2)),
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
    annotate("text", x=0.55, y=-19.5, hjust = 0,color="black",size = 8,
             label= paste("\n",expression(WCM),": ",round(R2_1,3),"\n",
                          expression(Linear),": ",round(summary(reg1_1)$adj.r.squared,3),"\n",
                          expression(Log),": ",round(summary(reg3_1)$adj.r.squared,3)
             )) + 
    labs(x = paste0("LiDAR CHM cover (2008)"),
         y = paste0("SAR backscatter (2008)")) +
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(text=element_text(size=25)) + 
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position=c(0.7,0.15),legend.key.width=unit(3,"cm"))


i_2 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv"
Data2 = read.csv(i_2,header=T)
Data2 <- Data2[Data2$C2 > 0,]

x2 <- Data2$C2
y2 <- Data2$HV

mult_nls_2 <- nls(y2 ~ a*exp(-c*x2)+b*(1-exp(-c*x2)), 
                  start = list(a=-25, b=-12, c=2))
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
                                   start = list(a=-25, b=-12, c=2)),
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
    annotate("text", x=0.5, y=-20.5, hjust = 0,color="black",size = 8,
             label= paste("\n",expression(WCM),": ",round(R2_2,3),"\n",
                          expression(Linear),": ",round(summary(reg1_2)$adj.r.squared,3),"\n",
                          expression(Log),": ",round(summary(reg3_2)$adj.r.squared,3)
             )) + 
    labs(x = paste0("LiDAR CHM cover (2010)"),
         y = paste0("SAR backscatter (2010)")) +
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(text=element_text(size=25)) + 
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position=c(0.7,0.15),legend.key.width=unit(3,"cm"))


i_3 <- "E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv"
Data3 = read.csv(i_3,header=T)
Data3 <- Data3[Data3$C2 > 0,]

x3 <- Data3$C2
y3 <- Data3$HV

mult_nls_3 <- nls(y3 ~ a*exp(-c*x3)+b*(1-exp(-c*x3)), 
                  start = list(a=-25, b=-12, c=2))
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
                                   start = list(a=-25, b=-12, c=2)),
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
    annotate("text", x=0.55, y=-20.5, hjust = 0,color="black",size = 8,
             label= paste("\n",expression(WCM),": ",round(R2_3,3),"\n",
                          expression(Linear),": ",round(summary(reg1_3)$adj.r.squared,3),"\n",
                          expression(Log),": ",round(summary(reg3_3)$adj.r.squared,3)
             )) + 
    labs(x = paste0("LiDAR CHM cover (2018)"),
         y = paste0("SAR backscatter (2018)")) +
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(text=element_text(size=25)) + 
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position=c(0.7,0.15),legend.key.width=unit(3,"cm"))



ggarrange(p1,p2,p3,ncol=3,nrow=1) 
out = "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\figure cover composite.jpg"
ggsave(out,height=9, width=18, dpi=600)






# ------------------------------------------------------------------------------------------------ #
#violin plot cover

dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2008.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)*2
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.6,0.6,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.6 & Data$diff_C2_SAR_5 < 0.6,]
p1<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_violin(trim=FALSE, fill="gray")+
    labs(
        x = paste0("ALS cover change (2018-2008)"),
        y = paste0("Bayesian WCM cover change (2018-2008)")) +
    theme_bw()+
    scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))

dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2010.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)*2
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.6,0.6,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.6 & Data$diff_C2_SAR_5 < 0.6,]
p2<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_violin(trim=FALSE, fill="gray")+
    labs(
        x = paste0("ALS cover change (2018-2010)"),
        y = paste0("Bayesian WCM cover change (2018-2010)")) +
    theme_bw()+
    scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))

dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2010.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2008.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)*2
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.6,0.6,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
#Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.6 & Data$diff_C2_SAR_5 < 0.6,]
p3<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_violin(trim=FALSE, fill="gray")+
    labs(
        x = paste0("ALS cover change (2010-2008)"),
        y = paste0("Bayesian WCM cover change (2010-2008)")) +
    theme_bw()+
    scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p2,p3,ncol =3)
out <- "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\change cover composite.jpg"
ggsave(out,height=9, width=18, dpi=600)




# ------------------------------------------------------------------------------------------------ #

dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2008.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$V2.x > 0,]
Data$diff_V2_ALS_5 = Data$V2.x - Data$V2.y
Data$diff_V2_SAR_5 = Data$P_V_5.x - Data$P_V_5.y
breakbin = seq(-12000,12000,4000)
Data$group <- cut(Data$diff_V2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p1<- ggplot(Data, aes(x=diff_V2_ALS_5, y=diff_V2_SAR_5, group = group)) + 
    geom_violin(trim=FALSE, fill="gray")+
    labs(
        x = paste0("ALS volume change (2018-2008)"),
        y = paste0("Bayesian WCM volume change (2018-2008)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-8000, 8000))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(axis.text.x = element_text(angle=90, hjust=1))

dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2010.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$V2.x > 0,]
Data$diff_V2_ALS_5 = Data$V2.x - Data$V2.y
Data$diff_V2_SAR_5 = Data$P_V_5.x - Data$P_V_5.y
breakbin = seq(-12000,12000,4000)
Data$group <- cut(Data$diff_V2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p2<- ggplot(Data, aes(x=diff_V2_ALS_5, y=diff_V2_SAR_5, group = group)) + 
    geom_violin(trim=FALSE, fill="gray")+
    labs(
        x = paste0("ALS volume change (2018-2010)"),
        y = paste0("Bayesian WCM volume change (2018-2010)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-8000, 8000))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(axis.text.x = element_text(angle=90, hjust=1))

dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2010.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2008.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$V2.x > 0,]
Data$diff_V2_ALS_5 = Data$V2.x - Data$V2.y
Data$diff_V2_SAR_5 = Data$P_V_5.x - Data$P_V_5.y
breakbin = seq(-12000,12000,4000)
Data$group <- cut(Data$diff_V2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p3<- ggplot(Data, aes(x=diff_V2_ALS_5, y=diff_V2_SAR_5, group = group)) + 
    geom_violin(trim=FALSE, fill="gray")+
    labs(
        x = paste0("ALS volume change (2010-2008)"),
        y = paste0("Bayesian WCM volume change (2010-2008)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-8000, 8000))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(axis.text.x = element_text(angle=90, hjust=1))

ggarrange(p1,p2,p3,ncol =3)
out <- "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\change volume composite.jpg"
ggsave(out,height=9, width=20, dpi=600)





# ------------------------------------------------------------------------------------------------ #
#bias boxplot BWCM

dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2008.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = Data$P_C_5.x - Data$P_C_5.y
Data$diff_C2_SAR_5 = Data$diff_C2_SAR_5 - Data$diff_C2_ALS_5
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.6,0.6,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.6 & Data$diff_C2_SAR_5 < 0.6,]
p1<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
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

Data$group_RH <- cut(Data$diff_C2_ALS_5,breaks = breakbin
                     ,dig.lab=1)
a <- tapply(Data$diff_C2_SAR_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$diff_C2_ALS_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(Data$group_RH))
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
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = Data$P_C_5.x - Data$P_C_5.y
Data$diff_C2_SAR_5 = Data$diff_C2_SAR_5 - Data$diff_C2_ALS_5
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.6,0.6,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.6 & Data$diff_C2_SAR_5 < 0.6,]
p2<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_boxplot(trim=FALSE)+
    labs(
        x = paste0("ALS cover change (2018-2010)"),
        y = paste0("Bias (2018-2010)")) +
    theme_bw()+
    scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(axis.title.x=element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))

Data$group_RH <- cut(Data$diff_C2_ALS_5,breaks = breakbin
                     ,dig.lab=1)
a <- tapply(Data$diff_C2_SAR_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$diff_C2_ALS_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(Data$group_RH))
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
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = Data$P_C_5.x - Data$P_C_5.y
Data$diff_C2_SAR_5 = Data$diff_C2_SAR_5 - Data$diff_C2_ALS_5
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.2,0.4,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.6 & Data$diff_C2_SAR_5 < 0.6,]
p3<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_boxplot(trim=FALSE)+
    labs(
        x = paste0("ALS cover change (2010-2008)"),
        y = paste0("Bias (2010-2008)")) +
    theme_bw()+
    scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(axis.title.x=element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))


Data$group_RH <- cut(Data$diff_C2_ALS_5,breaks = breakbin
                     ,dig.lab=1)
a <- tapply(Data$diff_C2_SAR_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$diff_C2_ALS_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.2,0.2,0.2),digits = 1)
l <- cbind(x,RB,table(Data$group_RH))
df <- data.frame(l)
p6<- ggplot(df, aes(x=x+0.1, y=RB)) + 
    geom_bar(stat='identity')+
    theme_bw()+
    coord_cartesian(ylim = c(-100, 100))+
    scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
    scale_x_continuous(minor_breaks = round(seq(-0.2,0.4,0.2),digits = 1),
                       breaks = round(seq(-0.2,0.4,0.2),digits = 1))+
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
#violin plot cover ----- SAR HV~ALS

dir1 <- paste0("F:\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("F:\\DB_csv\\Ind_2008.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$HV.x - Data$HV.y)*2
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.6,0.6,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p1<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_violin(trim=FALSE, fill="gray")+
    labs(
        x = paste0("ALS cover change (2018-2008)"),
        y = paste0("SAR HV change (dB) (2018-2008)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-20, 20))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))



dir1 <- paste0("F:\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("F:\\DB_csv\\Ind_2010.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$HV.x - Data$HV.y)*2
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.6,0.6,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p2<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_violin(trim=FALSE, fill="gray")+
    labs(
        x = paste0("ALS cover change (2018-2010)"),
        y = paste0("SAR HV change (dB) (2018-2010)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-20, 20))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))



dir1 <- paste0("F:\\DB_csv\\Ind_2010.csv")
dir2 <- paste0("F:\\DB_csv\\Ind_2008.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = (Data$HV.x - Data$HV.y)*2
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.6,0.6,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p3 <- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_violin(trim=FALSE, fill="gray")+
    labs(
        x = paste0("ALS cover change (2010-2008)"),
        y = paste0("SAR HV change (dB) (2010-2008)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-20, 20))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))


ggarrange(p1,p2,p3,ncol =3)
out <- "F:\\change cover composite.jpg"
ggsave(out,height=9, width=18, dpi=600)




# ------------------------------------------------------------------------------------------------ #

dir1 <- paste0("F:\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("F:\\DB_csv\\Ind_2008.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$V2.x > 0,]
Data$diff_V2_ALS_5 = Data$V2.x - Data$V2.y
Data$diff_V2_SAR_5 = (Data$HV.x - Data$HV.y)*2
breakbin = seq(-12000,12000,4000)
Data$group <- cut(Data$diff_V2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p1<- ggplot(Data, aes(x=diff_V2_ALS_5, y=diff_V2_SAR_5, group = group)) + 
    geom_violin(trim=FALSE, fill="gray")+
    labs(
        x = paste0("ALS volume change (2018-2008)"),
        y = paste0("SAR HV change (dB) (2018-2008)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-20, 20))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(axis.text.x = element_text(angle=90, hjust=1))


dir1 <- paste0("F:\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("F:\\DB_csv\\Ind_2010.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$V2.x > 0,]
Data$diff_V2_ALS_5 = Data$V2.x - Data$V2.y
Data$diff_V2_SAR_5 = (Data$HV.x - Data$HV.y)*2
breakbin = seq(-12000,12000,4000)
Data$group <- cut(Data$diff_V2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p2<- ggplot(Data, aes(x=diff_V2_ALS_5, y=diff_V2_SAR_5, group = group)) + 
    geom_violin(trim=FALSE, fill="gray")+
    labs(
        x = paste0("ALS volume change (2018-2010)"),
        y = paste0("SAR HV change (dB) (2018-2010)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-20, 20))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(axis.text.x = element_text(angle=90, hjust=1))


dir1 <- paste0("F:\\DB_csv\\Ind_2010.csv")
dir2 <- paste0("F:\\DB_csv\\Ind_2008.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$V2.x > 0,]
Data$diff_V2_ALS_5 = Data$V2.x - Data$V2.y
Data$diff_V2_SAR_5 = (Data$HV.x - Data$HV.y)*2
breakbin = seq(-12000,12000,4000)
Data$group <- cut(Data$diff_V2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p3<- ggplot(Data, aes(x=diff_V2_ALS_5, y=diff_V2_SAR_5, group = group)) + 
    geom_violin(trim=FALSE, fill="gray")+
    labs(
        x = paste0("ALS volume change (2010-2008)"),
        y = paste0("SAR HV change (dB) (2010-2008)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-20, 20))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(axis.text.x = element_text(angle=90, hjust=1))


ggarrange(p1,p2,p3,ncol =3)
out <- "F:\\change volume composite.jpg"
ggsave(out,height=9, width=20, dpi=600)









# ------------------------------------------------------------------------------------------------ #
#bias boxplot -- log model

#2018-2008
dir1 <- paste0("F:\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("F:\\DB_csv\\Ind_2008.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
#2018-2008 cover
x <- Data$C2.x
y <- Data$HV.x
mult_nls <- nls(x~ a*exp(b*y), start = list(a=20, b=0.2))
pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
x <- Data$C2.y
y <- Data$HV.y
mult_nls <- nls(x~ a*exp(b*y), start = list(a=20, b=0.2))
pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = pred.x - pred.y

Data$diff_C2_SAR_5 = Data$diff_C2_SAR_5 - Data$diff_C2_ALS_5
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.6,0.6,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.6 & Data$diff_C2_SAR_5 < 0.6,]
p1<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_boxplot(trim=FALSE)+
    labs(
        x = paste0("ALS cover change (2018-2008)"),
        y = paste0("Bias (2018-2008)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-0.8, 0.8))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(axis.title.x=element_blank())+
    theme(plot.title = element_text(hjust = 0.5))

Data$group_RH <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
a <- tapply(Data$diff_C2_SAR_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$diff_C2_ALS_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(Data$group_RH))
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



#2018-2010
dir1 <- paste0("F:\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("F:\\DB_csv\\Ind_2010.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
x <- Data$C2.x
y <- Data$HV.x
mult_nls <- nls(x~ a*exp(b*y), start = list(a=20, b=0.2))
pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
x <- Data$C2.y
y <- Data$HV.y
mult_nls <- nls(x~ a*exp(b*y), start = list(a=20, b=0.2))
pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = pred.x - pred.y

Data$diff_C2_SAR_5 = Data$diff_C2_SAR_5 - Data$diff_C2_ALS_5
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.6,0.6,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.6 & Data$diff_C2_SAR_5 < 0.6,]
p2<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_boxplot(trim=FALSE)+
    labs(
        x = paste0("ALS cover change (2018-2010)"),
        y = paste0("Bias (2018-2010)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-0.8, 0.8))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(axis.title.x=element_blank())+
    theme(plot.title = element_text(hjust = 0.5))

Data$group_RH <- cut(Data$diff_C2_ALS_5,breaks = breakbin
                     ,dig.lab=1)
a <- tapply(Data$diff_C2_SAR_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$diff_C2_ALS_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(Data$group_RH))
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


#2010-2008
dir1 <- paste0("F:\\DB_csv\\Ind_2010.csv")
dir2 <- paste0("F:\\DB_csv\\Ind_2008.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.y > 0,]
x <- Data$C2.x
y <- Data$HV.x
mult_nls <- nls(x~ a*exp(b*y), start = list(a=20, b=0.2))
pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
x <- Data$C2.y
y <- Data$HV.y
mult_nls <- nls(x~ a*exp(b*y), start = list(a=20, b=0.2))
pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = pred.x - pred.y

Data$diff_C2_SAR_5 = Data$diff_C2_SAR_5 - Data$diff_C2_ALS_5
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.6,0.6,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.6 & Data$diff_C2_SAR_5 < 0.6,]
p3<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_boxplot(trim=FALSE)+
    labs(
        x = paste0("ALS cover change (2010-2008)"),
        y = paste0("Bias (2010-2008)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-0.8, 0.8))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(axis.title.x=element_blank())+
    theme(plot.title = element_text(hjust = 0.5))

Data$group_RH <- cut(Data$diff_C2_ALS_5,breaks = breakbin
                     ,dig.lab=1)
a <- tapply(Data$diff_C2_SAR_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$diff_C2_ALS_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(Data$group_RH))
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


ggarrange(p1,p2,p3,p4,p5,p6)
out <- "F:\\change cover composite boxplot log.jpg"
ggsave(out,height=9, width=20, dpi=600)




# ------------------------------------------------------------------------------------------------ #
#bias boxplot -- BWCM
dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Merge_2008.csv")

Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = Data$P_C_5.x - Data$P_C_5.y
Data$diff_C2_SAR_5 = Data$diff_C2_SAR_5 - Data$diff_C2_ALS_5
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.6,0.6,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.6 & Data$diff_C2_SAR_5 < 0.6,]
p1<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_boxplot(trim=FALSE)+
    labs(
        x = paste0("ALS cover change (2018-2008)"),
        y = paste0("Bias (2018-2008)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-0.8, 0.8))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(axis.title.x=element_blank())+
    theme(plot.title = element_text(hjust = 0.5))

Data$group_RH <- cut(Data$diff_C2_ALS_5,breaks = breakbin
                     ,dig.lab=1)
a <- tapply(Data$diff_C2_SAR_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$diff_C2_ALS_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(Data$group_RH))
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
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = Data$P_C_5.x - Data$P_C_5.y
Data$diff_C2_SAR_5 = Data$diff_C2_SAR_5 - Data$diff_C2_ALS_5
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.6,0.6,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.6 & Data$diff_C2_SAR_5 < 0.6,]
p2<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_boxplot(trim=FALSE)+
    labs(
        x = paste0("ALS cover change (2018-2010)"),
        y = paste0("Bias (2018-2010)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-0.8, 0.8))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(axis.title.x=element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))

Data$group_RH <- cut(Data$diff_C2_ALS_5,breaks = breakbin
                     ,dig.lab=1)
a <- tapply(Data$diff_C2_SAR_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$diff_C2_ALS_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(Data$group_RH))
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
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = Data$P_C_5.x - Data$P_C_5.y
Data$diff_C2_SAR_5 = Data$diff_C2_SAR_5 - Data$diff_C2_ALS_5
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(seq(-0.2,0.4,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.6 & Data$diff_C2_SAR_5 < 0.6,]
p3<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_boxplot(trim=FALSE)+
    labs(
        x = paste0("ALS cover change (2010-2008)"),
        y = paste0("Bias (2010-2008)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-0.8, 0.8))+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(axis.title.x=element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))


Data$group_RH <- cut(Data$diff_C2_ALS_5,breaks = breakbin
                     ,dig.lab=1)
a <- tapply(Data$diff_C2_SAR_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$diff_C2_ALS_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.2,0.2,0.2),digits = 1)
l <- cbind(x,RB,table(Data$group_RH))
df <- data.frame(l)
p6<- ggplot(df, aes(x=x+0.1, y=RB)) + 
    geom_bar(stat='identity')+
    theme_bw()+
    coord_cartesian(ylim = c(-100, 100))+
    scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
    scale_x_continuous(minor_breaks = round(seq(-0.2,0.4,0.2),digits = 1),
                       breaks = round(seq(-0.2,0.4,0.2),digits = 1))+
    labs(x="ALS cover change (2010-2008)", 
         y="%Bias (%)  (2010-2008)")+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))



ggarrange(p1,p2,p3,p4,p5,p6)
out <- "F:\\change cover composite boxplot wcm.jpg"
ggsave(out,height=9, width=20, dpi=600)





#HV HH linear
change_linear <- function(){
    #2018-2008
    dir1 <- "F:\\DB_csv\\Ind_2018.csv"
    dir2 <- "F:\\DB_csv\\Ind_2008.csv"
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
    print(round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3))
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
    print(round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3))
    
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
    print(round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3))
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
    print(round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3))
    
    dir1 <- "F:\\DB_csv\\Ind_2018.csv"
    dir2 <- "F:\\DB_csv\\Ind_2010.csv"
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
    print(round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3))
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
    print(round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3))
    
    dir1 <- "F:\\DB_csv\\Ind_2010.csv"
    dir2 <- "F:\\DB_csv\\Ind_2008.csv"
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
    print(round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3))
    
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
    print(round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3))
    
}
change_linear_HH <- function(){
    #2018-2008
    dir1 <- "F:\\DB_csv\\Ind_2018.csv"
    dir2 <- "F:\\DB_csv\\Ind_2008.csv"
    Data1 = read.csv(dir1,header=T)
    Data2 = read.csv(dir2,header=T)
    Data = merge(Data1,Data2,by=c("Merge_ID"))
    #2018-2008 volume
    x <- Data$V2.x
    y<- Data$HH.x
    reg <- lm(x ~ y, data = Data)
    pred.x <- predict(reg)
    x <- Data$V2.y
    y<- Data$HH.y
    reg <- lm(x ~ y, data = Data)
    pred.y <- predict(reg)
    Data$diff_V_ALS = Data$V2.x - Data$V2.y
    Data$diff_V_SAR = pred.x - pred.y
    print(round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3))
    #2017-2007 volume 
    x <- Data$V2.x
    y<- Data$HH7.x
    reg <- lm(x ~ y, data = Data)
    pred.x <- predict(reg)
    x <- Data$V2.y
    y<- Data$HH7.y
    reg <- lm(x ~ y, data = Data)
    pred.y <- predict(reg)
    Data$diff_V_ALS = Data$V2.x - Data$V2.y
    Data$diff_V_SAR = pred.x - pred.y
    print(round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3))
    
    #2018-2008 cover
    x <- Data$C2.x
    y<- Data$HH.x
    reg <- lm(x ~ y, data = Data)
    pred.x <- predict(reg)
    x <- Data$C2.y
    y<- Data$HH.y
    reg <- lm(x ~ y, data = Data)
    pred.y <- predict(reg)
    Data$diff_V_ALS = Data$C2.x - Data$C2.y
    Data$diff_V_SAR = pred.x - pred.y
    print(round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3))
    #2017-2007 cover 
    x <- Data$C2.x
    y<- Data$HH7.x
    reg <- lm(x ~ y, data = Data)
    pred.x <- predict(reg)
    x <- Data$C2.y
    y<- Data$HH7.y
    reg <- lm(x ~ y, data = Data)
    pred.y <- predict(reg)
    Data$diff_V_ALS = Data$C2.x - Data$C2.y
    Data$diff_V_SAR = pred.x - pred.y
    print(round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3))
    
    dir1 <- "F:\\DB_csv\\Ind_2018.csv"
    dir2 <- "F:\\DB_csv\\Ind_2010.csv"
    Data1 = read.csv(dir1,header=T)
    Data2 = read.csv(dir2,header=T)
    Data = merge(Data1,Data2,by=c("Merge_ID"))
    #2018-2010 volume
    x <- Data$V2.x
    y<- Data$HH.x
    reg <- lm(x ~ y, data = Data)
    pred.x <- predict(reg)
    x <- Data$V2.y
    y<- Data$HH.y
    reg <- lm(x ~ y, data = Data)
    pred.y <- predict(reg)
    Data$diff_V_ALS = Data$V2.x - Data$V2.y
    Data$diff_V_SAR = pred.x - pred.y
    print(round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3))
    #2018-2010 cover
    x <- Data$C2.x
    y<- Data$HH.x
    reg <- lm(x ~ y, data = Data)
    pred.x <- predict(reg)
    x <- Data$C2.y
    y<- Data$HH.y
    reg <- lm(x ~ y, data = Data)
    pred.y <- predict(reg)
    Data$diff_V_ALS = Data$C2.x - Data$C2.y
    Data$diff_V_SAR = pred.x - pred.y
    print(round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3))
    
    dir1 <- "F:\\DB_csv\\Ind_2010.csv"
    dir2 <- "F:\\DB_csv\\Ind_2008.csv"
    Data1 = read.csv(dir1,header=T)
    Data2 = read.csv(dir2,header=T)
    Data = merge(Data1,Data2,by=c("Merge_ID"))
    #2010-2008 volume
    x <- Data$V2.x
    y<- Data$HH.x
    reg <- lm(x ~ y, data = Data)
    pred.x <- predict(reg)
    x <- Data$V2.y
    y<- Data$HH.y
    reg <- lm(x ~ y, data = Data)
    pred.y <- predict(reg)
    Data$diff_V_ALS = Data$V2.x - Data$V2.y
    Data$diff_V_SAR = pred.x - pred.y
    print(round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3))
    
    #2010-2008 cover
    x <- Data$C2.x
    y<- Data$HH.x
    reg <- lm(x ~ y, data = Data)
    pred.x <- predict(reg)
    x <- Data$C2.y
    y<- Data$HH.y
    reg <- lm(x ~ y, data = Data)
    pred.y <- predict(reg)
    Data$diff_V_ALS = Data$C2.x - Data$C2.y
    Data$diff_V_SAR = pred.x - pred.y
    print(round(summary(lm(Data$diff_V_SAR~Data$diff_V_ALS))$adj.r.squared,3))
}



# ------------------------------------------------------------------------------------------------ #
#violin plot cover sar vs. cover change
#-0.05~0.05 one bin test
dir1 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
dir2 <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
index <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\index.csv")
Data1 = read.csv(dir1,header=T)
Data2 = read.csv(dir2,header=T)
Data = merge(Data1,Data2,by=c("Merge_ID"))
Data_index = read.csv(index,header=T)
Data12 = merge(Data1,Data2,by=c("Merge_ID"))
Data = merge(Data12,Data_index,by=c("Merge_ID"))

Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = Data$HV.x - Data$HV.y
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = c(-0.5,-0.25,-0.05,0.05,0.25,0.5)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
data_summary <- function(x) {
    m <- mean(x)
    ymin <- m-sd(x)
    ymax <- m+sd(x)
    return(c(y=m,ymin=ymin,ymax=ymax))
}

p1<- ggplot(Data, aes(x=group, y=diff_C2_SAR_5)) + 
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

Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = Data$HV.x - Data$HV.y
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = c(-0.5,-0.25,-0.05,0.05,0.25,0.5)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p2<- ggplot(Data, aes(x=group, y=diff_C2_SAR_5)) + 
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

Data <- Data[Data$C2.x > 0 & Data$C2.x < 1 & Data$C2.y > 0 & Data$C2.y < 1,]
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = Data$HV.x - Data$HV.y
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = c(-0.5,-0.25,-0.05,0.05,0.25,0.5)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p3<- ggplot(Data, aes(x=group, y=diff_C2_SAR_5)) + 
    #geom_violin(trim=FALSE, fill="gray")+
    stat_summary(fun=mean, geom="point", size=4) + 
    stat_summary(aes(label=round(..y..,2)), fun.y=data_summary, geom="text", size=6,
                 vjust = -0.5)
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

ggarrange(p1,p2,p3,ncol=3,nrow=1) 
out = "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\change sar cover composite.jpg"
ggsave(out,height=10, width=22, dpi=600)





# ------------------------------------------------------------------------------------------------ #
#violin plot cover vs. cover
#-0.05~0.05 one bin test
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
Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(c(-0.5,-0.25,-0.05,0.05,0.25,0.5),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.5 & Data$diff_C2_SAR_5 < 0.5,]

data_summary <- function(x) {
    m <- mean(x)
    ymin <- m-sd(x)
    ymax <- m+sd(x)
    return(c(y=m,ymin=ymin,ymax=ymax))
}
p1<- ggplot(Data, aes(x=group, y=diff_C2_SAR_5)) + 
    geom_violin(trim=FALSE, fill="gray")+
    stat_summary(fun.data=data_summary)+
    labs(
        x = paste0("ALS cover change (2018-2008)"),
        y = paste0("Bayesian WCM cover change (2018-2008)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-0.3, 0.3))+
    scale_x_discrete(labels= c("(-0.5,-0.25)","(-0.25,-0.05)",
                               "(-0.05,0.05)","(0.05,0.25)","(0.25,0.5)"))+
    scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text.x = element_text(angle=15, vjust = 0.75, hjust=0.5))


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
Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(c(-0.5,-0.25,-0.05,0.05,0.25,0.5),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.5 & Data$diff_C2_SAR_5 < 0.5,]
p2<- ggplot(Data, aes(x=group, y=diff_C2_SAR_5)) + 
    geom_violin(trim=FALSE, fill="gray")+
    stat_summary(fun.data=data_summary)+
    labs(
        x = paste0("ALS cover change (2018-2010)"),
        y = paste0("Bayesian WCM cover change (2018-2010)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-0.3, 0.3))+
    scale_x_discrete(labels= c("(-0.5,-0.25)","(-0.25,-0.05)",
                               "(-0.05,0.05)","(0.05,0.25)","(0.25,0.5)"))+
    scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text.x = element_text(angle=15, vjust = 0.75, hjust=0.5))


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
Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = round(c(-0.5,-0.25,-0.05,0.05,0.25,0.5),2)
Data <- Data[Data$diff_C2_ALS_5 > -0.5 & Data$diff_C2_ALS_5 < 0.5,]
Data <- Data[Data$diff_C2_SAR_5 > -0.5 & Data$diff_C2_SAR_5 < 0.5,]
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
#Data <- na.omit(Data)

p3<- ggplot(Data, aes(x=group, y=diff_C2_SAR_5)) + 
    geom_violin(trim=FALSE, fill="gray")+
    stat_summary(fun.data=data_summary)+
    labs(
        x = paste0("ALS cover change (2010-2008)"),
        y = paste0("Bayesian WCM cover change (2010-2008)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-0.3, 0.3))+
    scale_x_discrete(labels= c("(-0.5,-0.25)","(-0.25,-0.05)",
                               "(-0.05,0.05)","(0.05,0.25)","(0.25,0.5)"))+
    scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text.x = element_text(angle=15, vjust = 0.75, hjust=0.5))


ggarrange(p1,p2,p3,ncol =3)
out <- "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\change cover composite.jpg"
ggsave(out,height=10, width=22, dpi=600)




# ------------------------------------------------------------------------------------------------ #
#violin boxplot -- log model
#2018-2008
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
#2018-2008 cover
x <- Data$C2.x
y <- Data$HV.x
mult_nls <- nls(x~ a*exp(b*y), start = list(a=20, b=0.2))
pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
x <- Data$C2.y
y <- Data$HV.y
mult_nls <- nls(x~ a*exp(b*y), start = list(a=20, b=0.2))
pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = pred.x - pred.y
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = c(-0.5,-0.25,-0.05,0.05,0.25,0.5)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)

data_summary <- function(x) {
    m <- mean(x)
    ymin <- m-sd(x)
    ymax <- m+sd(x)
    return(c(y=m,ymin=ymin,ymax=ymax))
}

p1<- ggplot(Data, aes(x=group, y=diff_C2_SAR_5)) + 
    geom_violin(trim=TRUE, fill="gray")+
    stat_summary(fun.data=data_summary)+
    labs(
        x = paste0("ALS cover change (2018-2008)"),
        y = paste0("Inverted log cover change (2018-2008)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-0.3, 0.3))+
    scale_x_discrete(labels= c("(-0.5,-0.25)","(-0.25,-0.05)",
                               "(-0.05,0.05)","(0.05,0.25)","(0.25,0.5)"))+
    scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text.x = element_text(angle=15, vjust = 0.75, hjust=0.5))



#2018-2010
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
#2018-2008 cover
x <- Data$C2.x
y <- Data$HV.x
mult_nls <- nls(x~ a*exp(b*y), start = list(a=20, b=0.2))
pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
x <- Data$C2.y
y <- Data$HV.y
mult_nls <- nls(x~ a*exp(b*y), start = list(a=20, b=0.2))
pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = pred.x - pred.y
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = c(-0.5,-0.25,-0.05,0.05,0.25,0.5)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p2<- ggplot(Data, aes(x=group, y=diff_C2_SAR_5)) + 
    geom_violin(trim=TRUE, fill="gray")+
    stat_summary(fun.data=data_summary)+
    labs(
        x = paste0("ALS cover change (2018-2010)"),
        y = paste0("Inverted log cover change (2018-2010)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-0.3, 0.3))+
    scale_x_discrete(labels= c("(-0.5,-0.25)","(-0.25,-0.05)",
                               "(-0.05,0.05)","(0.05,0.25)","(0.25,0.5)"))+
    scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text.x = element_text(angle=15, vjust = 0.75, hjust=0.5))



#2010-2008
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
#2018-2008 cover
x <- Data$C2.x
y <- Data$HV.x
mult_nls <- nls(x~ a*exp(b*y), start = list(a=20, b=0.2))
pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
x <- Data$C2.y
y <- Data$HV.y
mult_nls <- nls(x~ a*exp(b*y), start = list(a=20, b=0.2))
pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y)
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = pred.x - pred.y
Data <- Data[Data$diff_C2_ALS_5 > -0.5 & Data$diff_C2_ALS_5 < 0.5,]
Data <- Data[Data$diff_C2_SAR_5 > -0.5 & Data$diff_C2_SAR_5 < 0.5,]
print(round(summary(lm(Data$diff_C2_SAR_5~Data$diff_C2_ALS_5))$adj.r.squared,3))
breakbin = c(-0.5,-0.25,-0.05,0.05,0.25,0.5)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p3<- ggplot(Data, aes(x=group, y=diff_C2_SAR_5)) + 
    geom_violin(trim=TRUE, fill="gray")+
    stat_summary(fun.data=data_summary)+
    labs(
        x = paste0("ALS cover change (2010-2008)"),
        y = paste0("Inverted log cover change (2010-2008)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-0.3, 0.3))+
    scale_x_discrete(labels= c("(-0.5,-0.25)","(-0.25,-0.05)",
                               "(-0.05,0.05)","(0.05,0.25)","(0.25,0.5)"))+
    scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text.x = element_text(angle=15, vjust = 0.75, hjust=0.5))


ggarrange(p1,p2,p3,nrow=1)
out <- "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\change cover composite log.jpg"
ggsave(out,height=9, width=20, dpi=600)




# ------------------------------------------------------------------------------------------------ #
#predict ALS cover change from SAR change
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
Data$SAR_diff <- Data$HV.x - Data$HV.y
Data$ALS_diff <- Data$C2.x - Data$C2.y
reg <- lm(Data$ALS_diff~Data$SAR_diff)
Data$pred <- predict(reg)
summary(reg)
breakbin = c(-0.5,-0.25,-0.05,0.05,0.25,0.5)
Data$group <- cut(Data$ALS_diff,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
data_summary <- function(x) {
    m <- mean(x)
    ymin <- m-sd(x)
    ymax <- m+sd(x)
    return(c(y=m,ymin=ymin,ymax=ymax))
}
p1<- ggplot(Data, aes(x=group, y=pred)) + 
    geom_violin(trim=TRUE, fill="gray")+
    stat_summary(fun.data=data_summary)+
    labs(
        x = paste0("ALS cover change (2018-2008)"),
        y = paste0("Linear cover change (2018-2008)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-0.3, 0.3))+
    scale_x_discrete(labels= c("(-0.5,-0.25)","(-0.25,-0.05)",
                               "(-0.05,0.05)","(0.05,0.25)","(0.25,0.5)"))+
    scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
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
Data$SAR_diff <- Data$HV.x - Data$HV.y
Data$ALS_diff <- Data$C2.x - Data$C2.y
reg <- lm(Data$ALS_diff~Data$SAR_diff)
Data$pred <- predict(reg)
summary(reg)
breakbin = c(-0.5,-0.25,-0.05,0.05,0.25,0.5)
Data$group <- cut(Data$ALS_diff,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p2<- ggplot(Data, aes(x=group, y=pred)) + 
    geom_violin(trim=TRUE, fill="gray")+
    stat_summary(fun.data=data_summary)+
    labs(
        x = paste0("ALS cover change (2018-2010)"),
        y = paste0("Linear cover change (2018-2010)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-0.3, 0.3))+
    scale_x_discrete(labels= c("(-0.5,-0.25)","(-0.25,-0.05)",
                               "(-0.05,0.05)","(0.05,0.25)","(0.25,0.5)"))+
    scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
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
Data$SAR_diff <- Data$HV.x - Data$HV.y
Data$ALS_diff <- Data$C2.x - Data$C2.y
reg <- lm(Data$ALS_diff~Data$SAR_diff)
Data$pred <- predict(reg)
summary(reg)
breakbin = c(-0.5,-0.25,-0.05,0.05,0.25,0.5)
Data$group <- cut(Data$ALS_diff,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
p3<- ggplot(Data, aes(x=group, y=pred)) + 
    geom_violin(trim=TRUE, fill="gray")+
    stat_summary(fun.data=data_summary)+
    labs(
        x = paste0("ALS cover change (2010-2008)"),
        y = paste0("Linear cover change (2010-2008)")) +
    theme_bw()+
    coord_cartesian(ylim = c(-0.3, 0.3))+
    scale_x_discrete(labels= c("(-0.5,-0.25)","(-0.25,-0.05)",
                               "(-0.05,0.05)","(0.05,0.25)","(0.25,0.5)"))+
    scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text.x = element_text(angle=15, vjust = 0.75, hjust=0.5))

ggarrange(p1,p2,p3,nrow=1)
#new_df <- Data[,c("Merge_ID","pred")]
#write.table(new_df,file="E:\\ChangMap\\CHM\\DB_20220317\\DB_csv\\DB_2010_2008.csv",quote=F,sep=",",row.names=F)




# ------------------------------------------------------------------------------------------------ #
#bias boxplot -- direct SAR
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
Data$diff_C2_SAR_5 <- Data$HV.x - Data$HV.y
Data$diff_C2_ALS_5 <- Data$C2.x - Data$C2.y

reg <- lm(Data$diff_C2_ALS_5~Data$diff_C2_SAR_5)
Data$pred <- predict(reg)
summary(reg)
Data$diff_C2_SAR_5 = Data$pred - Data$diff_C2_ALS_5
breakbin = round(seq(-0.6,0.6,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.6 & Data$diff_C2_SAR_5 < 0.6,]

p1<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_boxplot(trim=FALSE)+
    labs(x = paste0("ALS cover change (2018-2008)"),
         y = paste0("Bias (2018-2008)")) +
    theme_bw()+
    scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25)) +
    theme(axis.title.x=element_blank())+
    theme(plot.title = element_text(hjust = 0.5))

Data$group_RH <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
a <- tapply(Data$diff_C2_SAR_5, cut(Data$diff_C2_ALS_5,
                                    breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$diff_C2_ALS_5, cut(Data$diff_C2_ALS_5,
                                    breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(Data$group_RH))
df <- data.frame(l)
p4<- ggplot(df, aes(x=x+0.1, y=RB)) + 
    geom_bar(stat='identity')+
    theme_bw()+
    coord_cartesian(ylim = c(-100, 100))+
    scale_x_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                       breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
    scale_y_continuous(minor_breaks = seq(-100, 100, 20),
                       breaks = seq(-100, 100, 20))+
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
Data$diff_C2_SAR_5 <- Data$HV.x - Data$HV.y
Data$diff_C2_ALS_5 <- Data$C2.x - Data$C2.y

reg <- lm(Data$diff_C2_ALS_5~Data$diff_C2_SAR_5)
Data$pred <- predict(reg)
summary(reg)
Data$diff_C2_SAR_5 = Data$pred - Data$diff_C2_ALS_5
breakbin = round(seq(-0.6,0.6,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.5 & Data$diff_C2_SAR_5 < 0.5,]
p2<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_boxplot(trim=FALSE)+
    labs(
        x = paste0("ALS cover change (2018-2010)"),
        y = paste0("Bias (2018-2010)")) +
    theme_bw()+
    scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(axis.title.x=element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))

Data$group_RH <- cut(Data$diff_C2_ALS_5,breaks = breakbin
                     ,dig.lab=1)
a <- tapply(Data$diff_C2_SAR_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$diff_C2_ALS_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(Data$group_RH))
df <- data.frame(l)
p5<- ggplot(df, aes(x=x+0.1, y=RB)) + 
    geom_bar(stat='identity')+
    theme_bw()+
    coord_cartesian(ylim = c(-100, 100))+
    scale_x_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                       breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
    scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
    labs(x="ALS cover change (2018-2010)", 
         y="%Bias (%)  (2018-2010)")+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))




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
Data$diff_C2_SAR_5 <- Data$HV.x - Data$HV.y
Data$diff_C2_ALS_5 <- Data$C2.x - Data$C2.y

reg <- lm(Data$diff_C2_ALS_5~Data$diff_C2_SAR_5)
Data$pred <- predict(reg)
summary(reg)
Data$diff_C2_SAR_5 = Data$pred - Data$diff_C2_ALS_5
breakbin = round(seq(-0.6,0.6,0.2),2)
Data$group <- cut(Data$diff_C2_ALS_5,breaks = breakbin,dig.lab=1)
Data <- na.omit(Data)
Data <- Data[Data$diff_C2_SAR_5 > -0.5 & Data$diff_C2_SAR_5 < 0.5,]
p3<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5, group = group)) + 
    geom_boxplot(trim=FALSE)+
    labs(
        x = paste0("ALS cover change (2010-2008)"),
        y = paste0("Bias (2010-2008)")) +
    theme_bw()+
    scale_x_continuous(minor_breaks = breakbin,breaks = breakbin)+
    scale_y_continuous(minor_breaks = breakbin,breaks = breakbin)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.title = element_blank())+
    theme(axis.title.x=element_blank())+
    theme(text=element_text(size=25)) +
    theme(plot.title = element_text(hjust = 0.5))


Data$group_RH <- cut(Data$diff_C2_ALS_5,breaks = breakbin
                     ,dig.lab=1)
a <- tapply(Data$diff_C2_SAR_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$diff_C2_ALS_5, cut(Data$diff_C2_ALS_5,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- round(seq(-0.6,0.4,0.2),digits = 1)
l <- cbind(x,RB,table(Data$group_RH))
df <- data.frame(l)
p6<- ggplot(df, aes(x=x+0.1, y=RB)) + 
    geom_bar(stat='identity')+
    theme_bw()+
    coord_cartesian(ylim = c(-100, 100))+
    scale_x_continuous(minor_breaks = round(seq(-0.6,0.6,0.2),digits = 1),
                       breaks = round(seq(-0.6,0.6,0.2),digits = 1))+
    scale_y_continuous(minor_breaks = seq(-100, 100, 20),breaks = seq(-100, 100, 20))+
    labs(x="ALS cover change (2010-2008)", 
         y="%Bias (%)  (2010-2008)")+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))



ggarrange(p1,p2,p3,p4,p5,p6,ncol=3,nrow=2)
out <- "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\change cover composite boxplot direct.jpg"
ggsave(out,height=9, width=18, dpi=600)





# ------------------------------------------------------------------------------------------------ #

#slope mean/std
file <- "E:\\ChangMap\\CHM\\DB_20220317\\DB_slope\\Slope\\Welverdiendt_slope.tif"
ras <- raster(file)
val <- getValues(ras) #get raster values
m <- mean(val,na.rm=T) #remove NAs and compute mean
std <- sd(val,na.rm=T) 






# ------------------------------------------------------------------------------------------------ #
#t test, subsample, bins select


# ------------------------------------------------------------------------------------------------ #
#Do T-test on just on back scatter change vs. ALS cover change for different bins.

# ------------------------------------------------------------------------------------------------ #
#maps bins
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

Data$diff_C2_SAR_5 = Data$HV.x - Data$HV.y
Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y

#group
Data$Type[Data$diff_C2_ALS_5 >= 0.5] = ">0.5"
Data$Type[Data$diff_C2_ALS_5 >= 0.25 & Data$diff_C2_ALS_5 < 0.5] = "0.25~0.5"
Data$Type[Data$diff_C2_ALS_5 >= 0.05 & Data$diff_C2_ALS_5 < 0.25] = "0.05~0.25"
Data$Type[Data$diff_C2_ALS_5 >= -0.05 & Data$diff_C2_ALS_5 < 0.05] = "-0.05~0.05"
Data$Type[Data$diff_C2_ALS_5 >= -0.25 & Data$diff_C2_ALS_5 < -0.05] = "-0.25~-0.05"
Data$Type[Data$diff_C2_ALS_5 >= -0.5 & Data$diff_C2_ALS_5 < -0.25] = "-0.5~-0.25"
Data$Type[Data$diff_C2_ALS_5 <= -0.5] = "-0.5<"
Data$Type<-factor(Data$Type, levels=c("-0.5<","-0.5~-0.25", "-0.25~-0.05","-0.05~0.05",
                                      "0.05~0.25","0.25~0.5",">0.5"))


#count
aggregate(Data$diff_C2_SAR_5, by=list(Data$Type), FUN=length)

Data<-Data[!Data$Type==">0.5" ,]
Data<-Data[!Data$Type=="-0.5<" ,]
#Data<-Data[!Data$Type=="-0.5~-0.25" ,]
#Data<-Data[!Data$Type==">0.5",]
#Data<-Data[!Data$Type=="0.25~0.5",]

aggregate(Data$diff_C2_SAR_5, by=list(Data$Type), FUN=length)

Data <- Data %>%
    group_by(Type) %>%
    do(sample_n(., 100, replace = FALSE))

mean_se <- Data %>%
    group_by(Type) %>%
    summarise(Means = mean(diff_C2_SAR_5), STD = sd(diff_C2_SAR_5))

mean_ini <- data.frame(mean_se[c("Means")])
STD_ini <- data.frame(mean_se[c("STD")])

r1 <- compare_means(
    diff_C2_SAR_5~Type, 
    Data, 
    method = "t.test", 
    p.adjust.method = "BH")
p_ini <- data.frame(r1[c("p")])

for (i in 1:99){
    Data <- Data %>%
        group_by(Type) %>%
        do(sample_n(., 100, replace = FALSE))

    mean_se <- Data %>%
        group_by(Type) %>%
        summarise(Means = mean(diff_C2_SAR_5), STD = sd(diff_C2_SAR_5))
    
    mean2 <- data.frame(mean_se[c("Means")])
    STD2 <- data.frame(mean_se[c("STD")])
    mean_final <- cbind(mean_ini, mean2)
    STD_final <- cbind(STD_ini, STD2)
    
    r1 <- compare_means(
        diff_C2_SAR_5~Type, 
        Data, 
        method = "t.test", 
        p.adjust.method = "BH")
    p2 <- data.frame(r1[c("p")])
    p_final <- cbind(p_ini, p2)
}

rowMeans(mean_final)
rowMeans(STD_final)
rowMeans(p_final)

# ------------------------------------------------------------------------------------------------ #
#sigmoid curve fit
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
Data <- na.omit(Data)
model <- nls(ALS ~ a/(1+exp(-b*(SAR-c)))+d,data=Data, start = list(a=5, b=9,c=0.1,d=-2))
predictions <- predict(model, Data)
rv_sigmoid <- round(cor(Data$ALS,predictions)^2,3)

#new_df <- Data[,c("SAR","ALS")]
#write.table(new_df,file="C:\\Users\\Shawn\\Desktop\\Sigmoid_test.csv",quote=F,sep=",",row.names=F)

reg1_1 <- lm(SAR ~ ALS , data = Data)
R1_1 <- summary(reg1_1)$adj.r.squared

reg2_1 <- lm(SAR ~ poly(ALS,2), data = Data)
R2_1 <- summary(reg2_1)$adj.r.squared

mlog <- lm(SAR ~ log(1+ALS),data=Data)
predlog <- predict(mlog, newdata = Data)
R3_1 <- round(cor(Data$SAR,predlog)^2,3)



#new_df <- Data[,c("SAR","ALS")]
#write.table(new_df,file="C:\\Users\\Shawn\\Desktop\\Sigmoid_test.csv",quote=F,sep=",",row.names=F)


#scatterplot --- sigmoid curve
ggplot(Data, aes(x=ALS, y=SAR))+ 
    geom_pointdensity()+
    geom_smooth(method = "nls", 
                method.args = list(formula = y ~ a/(1+exp(-b*(x-c)))+d, 
                                   start = list(a=5, b=9,c=0.1,d=-2)),
                size = 1.5, 
                linetype = "solid",
                colour = "black",se = F)+
    geom_smooth(
        method="lm",
        formula = 'y ~ log(x+1)', 
        se= F, 
        size = 1.5, 
        linetype = "solid",
        colour = "blue")+
    geom_smooth(
        method="lm",
        formula = 'y ~ x', 
        se= F, 
        size = 1.5, 
        linetype = "solid",
        colour = "purple")+
    geom_smooth(
        method="lm",
        formula = 'y ~poly(x,2)', 
        se= F, 
        size = 1.5, 
        linetype = "solid",
        colour = "red")+
    annotate("text", x=0.1, y=-5, hjust = 0,color="black",size = 8,
             label= paste("\n",expression(Linear),": ",round(R1_1,3),"\n",
                          expression(Poly),": ",round(R2_1,3),"\n",
                          expression(Log),": ",round(R3_1,3),"\n",
                          expression(Sigmoid),": ",rv_sigmoid)) + 
    scale_color_viridis(direction = 1)+
    labs(x = paste0("LiDAR CHM cover change"),
         y = paste0("ALOS backscatter change")) +
    theme(text=element_text(size=20)) +
    theme(plot.title = element_text(hjust = 0.5))




#t-test boxplot + stats
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

df2<- Data[c("SAR","ALS")]
df3<- df2 %>% 
    group_by(gr=cut(ALS, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
    arrange(as.numeric(gr))


i <- 1
rsqd <- 0
slope <- 0
intercept <- 0 
for (i in 1:100){
    new_df <- df3 %>% group_by(gr) %>% slice_sample(n=100)
    rsqd[i] <- cor(new_df$SAR,new_df$ALS)^2
    linear <- lm(new_df$ALS~new_df$SAR)
    slope[i] <- coef(linear)[2]
    intercept[i] <- coef(linear)[1]
}
R1_1<- mean(rsqd)
Slope_1 <- mean(slope)
Intercept_1 <- mean(intercept)


#scatterplot
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

df2<- Data[c("SAR","ALS")]
df3<- df2 %>% 
    group_by(gr=cut(ALS, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
    arrange(as.numeric(gr))


i <- 1
rsqd <- 0
slope <- 0
intercept <- 0 
for (i in 1:100){
    new_df <- df3 %>% group_by(gr) %>% slice_sample(n=100)
    rsqd[i] <- cor(new_df$SAR,new_df$ALS)^2
    linear <- lm(new_df$ALS~new_df$SAR)
    slope[i] <- coef(linear)[2]
    intercept[i] <- coef(linear)[1]
}
R1_1<- mean(rsqd)
Slope_1 <- mean(slope)
Intercept_1 <- mean(intercept)



#scatterplot
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

df3<- Data %>% 
    group_by(gr=cut(ALS, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
    arrange(as.numeric(gr))


i <- 1
rsqd <- 0
slope <- 0
intercept <- 0 
for (i in 1:100){
    new_df <- df3 %>% group_by(gr) %>% slice_sample(n=100)
    rsqd[i] <- cor(new_df$SAR,new_df$ALS)^2
    linear <- lm(new_df$ALS~new_df$SAR)
    slope[i] <- coef(linear)[2]
    intercept[i] <- coef(linear)[1]
}
R1_1<- mean(rsqd)
Slope_1 <- mean(slope)
Intercept_1 <- mean(intercept)



#scatterplot
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







# ------------------------------------------------------------------------------------------------ #
#SAR/ALS cover change boxplot
dir <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2018.csv")
Data = read.csv(dir,header=T)
Data <- na.omit(Data)
Data <- Data[Data$C2 > 0 & Data$C2 < 0.9,]

x <- Data$C2
y<- Data$HV

reg <- lm(x ~ y, data = Data)
r2 <- round(summary(reg)$adj.r.squared,3)
y <- predict(reg)

Data$diff <- y-x
breakbin = seq(0,1,0.1)
Data$group_RH <- cut(x,breaks = breakbin, dig.lab=1)
table(Data$group_RH)


Data<- Data %>% 
    group_by(gr=cut(C2, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
    arrange(as.numeric(gr))
#Data <- Data %>% group_by(gr) %>% slice_sample(n=100)

p1<- ggplot(Data, aes(x=C2, y=diff, group=group_RH)) + 
    stat_boxplot(geom ='errorbar', width = 0.1) +
    geom_boxplot(width = 1)+
    geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
    coord_cartesian(ylim = c(-1, 0.6))+
    scale_y_continuous(minor_breaks = seq(-1, 0.6, 0.2),breaks = seq(-1, 0.6, 0.2))+
    scale_x_continuous(minor_breaks = seq(0, 0.9, 0.1),breaks = seq(0, 0.9, 0.1))+
    theme_bw()+
    labs(x="",y="Bias (m)")+
    theme(axis.title.x=element_blank())+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("2018 Inverted linear regression")


a <- tapply(Data$diff, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$C2, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- seq(0,0.9,0.1)
l <- cbind(x,RB,table(Data$group_RH))
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



dir <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2010.csv")
Data = read.csv(dir,header=T)
Data <- na.omit(Data)
Data <- Data[Data$C2 > 0 & Data$C2 < 0.9,]

x <- Data$C2
y<- Data$HV

reg <- lm(x ~ y, data = Data)
r2 <- round(summary(reg)$adj.r.squared,3)
y <- predict(reg)

Data$diff <- y-x
breakbin = seq(0,1,0.1)
Data$group_RH <- cut(x,breaks = breakbin, dig.lab=1)
table(Data$group_RH)


Data<- Data %>% 
    group_by(gr=cut(C2, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
    arrange(as.numeric(gr))
#Data <- Data %>% group_by(gr) %>% slice_sample(n=100)

p2<- ggplot(Data, aes(x=C2, y=diff, group=group_RH)) + 
    stat_boxplot(geom ='errorbar', width = 0.1) +
    geom_boxplot(width = 1)+
    geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
    coord_cartesian(ylim = c(-1, 0.6))+
    scale_y_continuous(minor_breaks = seq(-1, 0.6, 0.2),breaks = seq(-1, 0.6, 0.2))+
    scale_x_continuous(minor_breaks = seq(0, 0.9, 0.1),breaks = seq(0, 0.9, 0.1))+
    theme_bw()+
    labs(x="",y="Bias (m)")+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("2010 Inverted linear regression")


a <- tapply(Data$diff, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$C2, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- seq(0,0.9,0.1)
l <- cbind(x,RB,table(Data$group_RH))
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
    theme(legend.position = "none")+
    theme(axis.title.y=element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))



dir <- paste0("E:\\ChangMap\\CHM\\DB_20210926\\DB_csv\\Ind_2008.csv")
Data = read.csv(dir,header=T)
Data <- na.omit(Data)
Data <- Data[Data$C2 > 0 & Data$C2 < 0.9,]

x <- Data$C2
y<- Data$HV

reg <- lm(x ~ y, data = Data)
r2 <- round(summary(reg)$adj.r.squared,3)
y <- predict(reg)

Data$diff <- y-x
breakbin = seq(0,1,0.1)
Data$group_RH <- cut(x,breaks = breakbin, dig.lab=1)
table(Data$group_RH)


Data<- Data %>% 
    group_by(gr=cut(C2, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
    arrange(as.numeric(gr))
#Data <- Data %>% group_by(gr) %>% slice_sample(n=100)

p3<- ggplot(Data, aes(x=C2, y=diff, group=group_RH)) + 
    stat_boxplot(geom ='errorbar', width = 0.1) +
    geom_boxplot(width = 1)+
    geom_abline(intercept = 0, slope = 0, color="red", linetype="solid", size=1.5)+
    coord_cartesian(ylim = c(-1, 0.6))+
    scale_y_continuous(minor_breaks = seq(-1, 0.6, 0.2),breaks = seq(-1, 0.6, 0.2))+
    scale_x_continuous(minor_breaks = seq(0, 0.9, 0.1),breaks = seq(0, 0.9, 0.1))+
    theme_bw()+
    labs(x="",y="Bias (m)")+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
    theme(legend.position = "none")+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("2008 Inverted linear regression")


a <- tapply(Data$diff, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
b <- tapply(Data$C2, cut(Data$C2,breaks = breakbin, dig.lab=1), mean)
RB <- a/b*100

x <- seq(0,0.9,0.1)
l <- cbind(x,RB,table(Data$group_RH))
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
    theme(legend.position = "none")+
    theme(axis.title.y=element_blank())+
    theme(legend.title = element_blank())+
    theme(text=element_text(size=25))+
    theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1,p2,p3,p4,p5,p6,nrow=2,ncol=3)
out = "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\bias boxplot original.jpg"
ggsave(out,height=12, width=24, dpi=600)










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
Data <- Data %>% group_by(gr) %>% slice_sample(n=100)

reg <- lm(diff_C2_ALS_5 ~ pd, data = Data)
Data$preds <- predict(reg)
R1_1 <- round(cor(Data$diff_C2_ALS_5,Data$preds)^2,3)

a <- Data$diff_C2_ALS_5
pred <- Data$preds
bias <- mean(pred - a)
RMSE<- sqrt(mean((pred - a)^2))
cat(R1_1,bias,RMSE,sep="\n")

p1<- ggplot(Data, aes(x=round(diff_C2_ALS_5,3), y=round(preds,3)))+ 
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
Data <- Data %>% group_by(gr) %>% slice_sample(n=100)

reg <- lm(diff_C2_ALS_5 ~ pd, data = Data)
Data$preds <- predict(reg)
R1_1 <- round(cor(Data$diff_C2_ALS_5,Data$preds)^2,3)

a <- Data$diff_C2_ALS_5
pred <- Data$preds
bias <- mean(pred - a)
RMSE<- sqrt(mean((pred - a)^2))
cat(R1_1,bias,RMSE,sep="\n")

p2<- ggplot(Data, aes(x=diff_C2_ALS_5, y=preds))+ 
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
         y = paste0("Pred. cover change (Linear)")) +
    theme(text=element_text(size=30)) + 
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position=c(0.15,0.7),legend.key.width=unit(1.5,"cm"))+
    theme(legend.text=element_text(size=25))+
    theme(legend.title=element_blank())


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
Data <- Data %>% group_by(gr) %>% slice_sample(n=100)

reg <- lm(diff_C2_ALS_5 ~ pd, data = Data)
Data$preds <- predict(reg)
R1_1 <- round(cor(Data$diff_C2_ALS_5,Data$preds)^2,3)

a <- Data$diff_C2_ALS_5
pred <- Data$preds
bias <- mean(pred - a)
RMSE<- sqrt(mean((pred - a)^2))
cat(R1_1,bias,RMSE,sep="\n")

p3<- ggplot(Data, aes(x=diff_C2_ALS_5, y=preds))+ 
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

#predict cover vs. ALS cover -- linear model
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

x <- Data$C2.x
y <- Data$HV.x
reg <- lm(x ~ y, data = Data)
pred.x <- predict(reg)
x <- Data$C2.y
y <- Data$HV.y
reg <- lm(x ~ y, data = Data)
pred.y <- predict(reg)

Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$preds = pred.x - pred.y

Data <- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
    arrange(as.numeric(gr))
Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
R1_1 <- round(cor(Data$diff_C2_ALS_5,Data$preds)^2,3)
p4<- ggplot(Data, aes(x=diff_C2_ALS_5, y=preds))+ 
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
         y = paste0("Pred. cover change (Linear)")) +
    theme(text=element_text(size=30)) + 
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position=c(0.15,0.7),legend.key.width=unit(1.5,"cm"))+
    theme(legend.text=element_text(size=25))+
    theme(legend.title=element_blank())




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
Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)

Data<- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
    arrange(as.numeric(gr))
Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
R1_2 <- round(cor(Data$diff_C2_ALS_5,Data$diff_C2_SAR_5)^2,3)

a <- Data$diff_C2_ALS_5
pred <- Data$diff_C2_SAR_5
bias <- mean(pred - a)
RMSE<- sqrt(mean((pred - a)^2))
cat(R1_2,bias,RMSE,sep="\n")


p7<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5))+ 
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
Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)

Data<- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
    arrange(as.numeric(gr))
Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
R1_2 <- round(cor(Data$diff_C2_ALS_5,Data$diff_C2_SAR_5)^2,3)

a <- Data$diff_C2_ALS_5
pred <- Data$diff_C2_SAR_5
bias <- mean(pred - a)
RMSE<- sqrt(mean((pred - a)^2))
cat(R1_2,bias,RMSE,sep="\n")


p8<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5))+ 
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
Data$diff_C2_SAR_5 = (Data$P_C_5.x - Data$P_C_5.y)

Data<- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
    arrange(as.numeric(gr))
Data <- Data %>% group_by(gr) %>% slice_sample(n=100)
R1_2 <- round(cor(Data$diff_C2_ALS_5,Data$diff_C2_SAR_5)^2,3)

a <- Data$diff_C2_ALS_5
pred <- Data$diff_C2_SAR_5
bias <- mean(pred - a)
RMSE<- sqrt(mean((pred - a)^2))
cat(R1_2,bias,RMSE,sep="\n")


p9<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5))+ 
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
Data <- Data %>% group_by(gr) %>% slice_sample(n=100)

x1 <- Data$C2.x
y1 <- Data$HV.x
mult_nls <- nls(x1~ a*exp(b*y1), start = list(a=20, b=0.2))
pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y1)
x2 <- Data$C2.y
y2 <- Data$HV.y
mult_nls <- nls(x2~ a*exp(b*y2), start = list(a=20, b=0.2))
pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y2)
Data$preds = pred.x - pred.y

R1_3 <- round(cor(Data$diff_C2_ALS_5,Data$preds)^2,3)
a <- Data$diff_C2_ALS_5
pred <- Data$preds
bias <- mean(pred - a)
RMSE<- sqrt(mean((pred - a)^2))
cat(R1_3,bias,RMSE,sep="\n")

p10<- ggplot(Data, aes(x=diff_C2_ALS_5, y=preds))+ 
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
Data <- Data %>% group_by(gr) %>% slice_sample(n=100)

x1 <- Data$C2.x
y1 <- Data$HV.x
mult_nls <- nls(x1~ a*exp(b*y1), start = list(a=20, b=0.2))
pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y1)
x2 <- Data$C2.y
y2 <- Data$HV.y
mult_nls <- nls(x2~ a*exp(b*y2), start = list(a=20, b=0.2))
pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y2)
Data$preds = pred.x - pred.y

R1_3 <- round(cor(Data$diff_C2_ALS_5,Data$preds)^2,3)
a <- Data$diff_C2_ALS_5
pred <- Data$preds
bias <- mean(pred - a)
RMSE<- sqrt(mean((pred - a)^2))
cat(R1_3,bias,RMSE,sep="\n")

p11<- ggplot(Data, aes(x=diff_C2_ALS_5, y=preds))+ 
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
Data <- Data %>% group_by(gr) %>% slice_sample(n=100)

x1 <- Data$C2.x
y1 <- Data$HV.x
mult_nls <- nls(x1~ a*exp(b*y1), start = list(a=20, b=0.2))
pred.x <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y1)
x2 <- Data$C2.y
y2 <- Data$HV.y
mult_nls <- nls(x2~ a*exp(b*y2), start = list(a=20, b=0.2))
pred.y <- coef(mult_nls)[1]*exp(coef(mult_nls)[2]*y2)
Data$preds = pred.x - pred.y

R1_3 <- round(cor(Data$diff_C2_ALS_5,Data$preds)^2,3)
a <- Data$diff_C2_ALS_5
pred <- Data$preds
bias <- mean(pred - a)
RMSE<- sqrt(mean((pred - a)^2))
cat(R1_3,bias,RMSE,sep="\n")


p12<- ggplot(Data, aes(x=diff_C2_ALS_5, y=preds))+ 
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



ggarrange(p1,p2,p3,p7,p8,p9,p10,p11,p12,nrow=3,ncol=3)
out = "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\inverted cover change 100 scatterplot.jpg"
ggsave(out,height=24, width=24, dpi=600)


# ------------------------------------------------------------------------------------------------ #
#scatterplot all vs. subsample -- linear

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

x <- Data$C2.x
y <- Data$HV.x
reg <- lm(x ~ y, data = Data)
pred.x <- predict(reg)
x <- Data$C2.y
y <- Data$HV.y
reg <- lm(x ~ y, data = Data)
pred.y <- predict(reg)

Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = pred.x - pred.y

p1<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5))+ 
    geom_pointdensity()+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
    coord_cartesian(xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8))+
    scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
    scale_y_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
    stat_poly_eq(formula = y ~ x, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE, size = 10) +      
    scale_color_viridis(direction = 1)+
    labs(x = paste0("ALS cover change (2010-2008)"),
         y = paste0("Cover change (Linear)")) +
    theme(text=element_text(size=25)) + 
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position=c(0.15,0.7),legend.key.width=unit(1.5,"cm"))+
    theme(legend.text=element_text(size=15))+
    theme(legend.title=element_blank())


Data <- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
    arrange(as.numeric(gr))
Data_s <- Data %>% group_by(gr) %>% slice_sample(n=100)

p2<- ggplot(Data_s, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5))+ 
    geom_pointdensity()+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
    coord_cartesian(xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8))+
    scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
    scale_y_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
    stat_poly_eq(formula = y ~ x, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE, size = 10) +      
    scale_color_viridis(direction = 1)+
    labs(x = paste0("ALS cover change (2010-2008)"),
         y = paste0("Cover change (Linear)")) +
    theme(text=element_text(size=25)) + 
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position=c(0.15,0.7),legend.key.width=unit(1.5,"cm"))+
    theme(legend.text=element_text(size=15))+
    theme(legend.title=element_blank())

ggarrange(p1,p2,ncol=2)
out = "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\scatterplot all_100 20102008.jpg"
ggsave(out,height=12, width=24, dpi=600)



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

x <- Data$C2.x
y <- Data$HV.x
reg <- lm(x ~ y, data = Data)
pred.x <- predict(reg)
x <- Data$C2.y
y <- Data$HV.y
reg <- lm(x ~ y, data = Data)
pred.y <- predict(reg)

Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = pred.x - pred.y

p1<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5))+ 
    geom_pointdensity()+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
    coord_cartesian(xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8))+
    scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
    scale_y_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
    stat_poly_eq(formula = y ~ x, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE, size = 10) +      
    scale_color_viridis(direction = 1)+
    labs(x = paste0("ALS cover change (2018-2008)"),
         y = paste0("Cover change (Linear)")) +
    theme(text=element_text(size=25)) + 
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position=c(0.15,0.7),legend.key.width=unit(1.5,"cm"))+
    theme(legend.text=element_text(size=15))+
    theme(legend.title=element_blank())


Data <- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
    arrange(as.numeric(gr))
Data_s <- Data %>% group_by(gr) %>% slice_sample(n=100)

p2<- ggplot(Data_s, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5))+ 
    geom_pointdensity()+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
    coord_cartesian(xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8))+
    scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
    scale_y_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
    stat_poly_eq(formula = y ~ x, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE, size = 10) +      
    scale_color_viridis(direction = 1)+
    labs(x = paste0("ALS cover change (2018-2008)"),
         y = paste0("Cover change (Linear)")) +
    theme(text=element_text(size=25)) + 
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position=c(0.15,0.7),legend.key.width=unit(1.5,"cm"))+
    theme(legend.text=element_text(size=15))+
    theme(legend.title=element_blank())

ggarrange(p1,p2,ncol=2)
out = "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\scatterplot all_100 20182008.jpg"
ggsave(out,height=12, width=24, dpi=600)



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

x <- Data$C2.x
y <- Data$HV.x
reg <- lm(x ~ y, data = Data)
pred.x <- predict(reg)
x <- Data$C2.y
y <- Data$HV.y
reg <- lm(x ~ y, data = Data)
pred.y <- predict(reg)

Data$diff_C2_ALS_5 = Data$C2.x - Data$C2.y
Data$diff_C2_SAR_5 = pred.x - pred.y

p1<- ggplot(Data, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5))+ 
    geom_pointdensity()+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
    coord_cartesian(xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8))+
    scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
    scale_y_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
    stat_poly_eq(formula = y ~ x, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE, size = 10) +      
    scale_color_viridis(direction = 1)+
    labs(x = paste0("ALS cover change (2018-2010)"),
         y = paste0("Cover change (Linear)")) +
    theme(text=element_text(size=25)) + 
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position=c(0.15,0.7),legend.key.width=unit(1.5,"cm"))+
    theme(legend.text=element_text(size=15))+
    theme(legend.title=element_blank())


Data <- Data %>% group_by(gr=cut(diff_C2_ALS_5, breaks= seq(-0.7, 0.7, by=0.01))) %>% 
    arrange(as.numeric(gr))
Data_s <- Data %>% group_by(gr) %>% slice_sample(n=100)
round(cor(Data_s$diff_C2_ALS_5,Data_s$diff_C2_SAR_5)^2,3)
p2<- ggplot(Data_s, aes(x=diff_C2_ALS_5, y=diff_C2_SAR_5))+ 
    geom_pointdensity()+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_abline(intercept = 0, slope = 1,color="black", linetype="solid", size=1.5)+
    coord_cartesian(xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8))+
    scale_x_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
    scale_y_continuous(minor_breaks = c(-0.8,0.8),breaks = seq(-0.8,0.8,0.2))+
    stat_poly_eq(formula = y ~ x, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE, size = 10) +      
    scale_color_viridis(direction = 1)+
    labs(x = paste0("ALS cover change (2018-2010)"),
         y = paste0("Cover change (Linear)")) +
    theme(text=element_text(size=25)) + 
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position=c(0.15,0.7),legend.key.width=unit(1.5,"cm"))+
    theme(legend.text=element_text(size=15))+
    theme(legend.title=element_blank())

ggarrange(p1,p2,ncol=2)
out = "E:\\ChangMap\\CHM\\DB_20210926\\Figure\\scatterplot all_100 20182010.jpg"
ggsave(out,height=12, width=24, dpi=600)



