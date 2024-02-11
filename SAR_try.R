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
Data <- Data[Data$SAR > -5 & Data$SAR < 5,]

Data <- Data[Data$ALS > -1 & Data$ALS < 1,]
Data$group_CC <- cut(Data$ALS,breaks = seq(-1, 1, 0.1),dig.lab=1)




ggplot(Data, aes(x=ALS, y=SAR, group = group_CC))+
  geom_boxplot(fill="grey70", fatten = 10,color = "black",outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single")) +
  theme_bw()+
  scale_x_continuous(breaks = seq(-1, 1, 0.1))+
  scale_y_continuous(breaks = seq(-5, 5, 1))+
  theme(text=element_text(size=20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  labs(title = title,
       x = paste0("LiDAR cover change"),
       y = paste0("SAR backscatter change")) +
  theme(plot.title = element_text(hjust = 0.5))



Data$SAR1 <- Data$X2018_HV
Data$SAR2 <- Data$X2008_HV


reg1 <- lm(C_2018 ~ SAR1, data = Data)
pred1 <- predict(reg1)
print(round(summary(reg1)$adj.r.squared,3))
reg2 <- lm(C_2008 ~ SAR2, data = Data)
pred2 <- predict(reg2)
Data$pred <- pred1 - pred2
Data <- Data[Data$pred > -1 & Data$pred < 1,]


ggplot(Data, aes(x=ALS, y=pred, group = group_CC))+
  geom_boxplot(fill="grey70", fatten = 10,color = "black",outlier.shape = NA, lwd = 0.1, 
               position = position_dodge2(preserve = "single")) +
  theme_bw()+
  scale_x_continuous(breaks = seq(-1, 1, 0.1))+
  scale_y_continuous(breaks = seq(-1, 1, 0.1))+
  theme(text=element_text(size=20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())+
  labs(title = title,
       x = paste0("LiDAR cover change"),
       y = paste0("SAR cover change")) +
  theme(plot.title = element_text(hjust = 0.5))








ggsave(out, height=15, width=15, dpi=300)
