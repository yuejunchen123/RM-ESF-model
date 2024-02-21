library(foreign)
library(ggplot2)
library(caret)
library(writexl)
library(dplyr)
library(viridis) # 使用viridis提供的翠绿色标度：scale_fill_viridis()
library(ggpointdensity) # 绘制密度散点图
library(cowplot) # 图形组合，可以自动对其坐标轴
library(Hmisc)
library(corrplot)
install.packages('extrafont')
library(extrafont)
###############################
# 1 验证结果散点图
###############################
lb<-paste('R = ',round(criteria1['esf','cor'],2),'\n','RMSE = ',round(criteria1['esf','RMSE'],2),' mm\n','MAE = ',round(criteria1['esf','MAE'],2),' mm',sep = '')
p1<-ggplot(vdata_y1,aes(SWE,esf))+
  ylab('RE-ESF (mm)')+xlab(paste('Reference SWE (mm)   January','\n','(d)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria1['esf','cor']), colour="#C82423",size=0.8)+
  geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p1<-p1+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p1

lb<-paste('R = ',round(criteria2['esf','cor'],2),'\n','RMSE = ',round(criteria2['esf','RMSE'],2),' mm\n','MAE = ',round(criteria2['esf','MAE'],2),' mm',sep = '')
p2<-ggplot(vdata_y2,aes(SWE,esf))+
  ylab('RE-ESF (mm)')+xlab(paste('Reference SWE (mm)   February','\n','(f)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria2['esf','cor']), colour="#C82423",size=0.8)+   geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p2<-p2+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p2


lb<-paste('R = ',round(criteria4['esf','cor'],2),'\n','RMSE = ',round(criteria4['esf','RMSE'],2),' mm\n','MAE = ',round(criteria4['esf','MAE'],2),' mm',sep = '')
p3<-ggplot(vdata_y4,aes(SWE,esf))+
  ylab('RE-ESF (mm)')+xlab(paste('Reference SWE (mm)   December','\n','(b)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria4['esf','cor']), colour="#C82423",size=0.8)+   geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p3<-p3+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p3

lb<-paste('R = ',round(criteria1['ERA5','cor'],2),'\n','RMSE = ',round(criteria1['ERA5','RMSE'],2),' mm\n','MAE = ',round(criteria1['ERA5','MAE'],2),' mm',sep = '')
p4<-ggplot(vdata_y1,aes(SWE,ERA5))+
  ylab('ERA5 (mm)')+xlab(paste('Reference SWE (mm)   January','\n','(c)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria1['ERA5','cor']), colour="#C82423",size=0.8)+   geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p4<-p4+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p4

lb<-paste('R = ',round(criteria2['ERA5','cor'],2),'\n','RMSE = ',round(criteria2['ERA5','RMSE'],2),' mm\n','MAE = ',round(criteria2['ERA5','MAE'],2),' mm',sep = '')
p5<-ggplot(vdata_y2,aes(SWE,ERA5))+
  ylab('ERA5 (mm)')+xlab(paste('Reference SWE (mm)   February','\n','(e)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria2['ERA5','cor']), colour="#C82423",size=0.8)+   geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p5<-p5+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p5


lb<-paste('R = ',round(criteria4['ERA5','cor'],2),'\n','RMSE = ',round(criteria4['ERA5','RMSE'],2),' mm\n','MAE = ',round(criteria4['ERA5','MAE'],2),' mm',sep = '')
p6<-ggplot(vdata_y4,aes(SWE,ERA5))+
  ylab('ERA5 (mm)')+xlab(paste('Reference SWE (mm)   December','\n','(a)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria4['ERA5','cor']), colour="#C82423",size=0.8)+   geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p6<-p6+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p6

p<-plot_grid(p6,p3,p4,p1,p5,p2,cols=2)
p
# ggsave('D:/chenyuejun/SWE/RS/paper/plots/cor.test.jpg', width =7,height = 8 ,p, device = "jpg", dpi = 800)
ggsave('D:/chenyuejun/cor.test.jpg', width =7,height = 8 ,p, device = "jpg", dpi = 800)


#####################
# add plot (by month)
#####################

lb<-paste('R = ',round(criteria1['Globsnow3','cor'],2),'\n','RMSE = ',round(criteria1['Globsnow3','RMSE'],2),' mm\n','MAE = ',round(criteria1['Globsnow3','MAE'],2),' mm',sep = '')
p1<-ggplot(vdata_y1[which(vdata_y1$Globsnow3>0),],aes(SWE,Globsnow3))+
  ylab('GlobSnow3 (mm)')+xlab(paste('Reference SWE (mm)   January','\n','(f)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria1['Globsnow3','cor']), colour="#C82423",size=0.8)+
  geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p1<-p1+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p1

lb<-paste('R = ',round(criteria2['Globsnow3','cor'],2),'\n','RMSE = ',round(criteria2['Globsnow3','RMSE'],2),' mm\n','MAE = ',round(criteria2['Globsnow3','MAE'],2),' mm',sep = '')
p2<-ggplot(vdata_y2[which(vdata_y2$Globsnow3>0),],aes(SWE,Globsnow3))+
  ylab('GlobSnow3 (mm)')+xlab(paste('Reference SWE (mm)   February','\n','(j)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria2['Globsnow3','cor']), colour="#C82423",size=0.8)+   geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p2<-p2+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p2


lb<-paste('R = ',round(criteria4['Globsnow3','cor'],2),'\n','RMSE = ',round(criteria4['Globsnow3','RMSE'],2),' mm\n','MAE = ',round(criteria4['Globsnow3','MAE'],2),' mm',sep = '')
p3<-ggplot(vdata_y4[which(vdata_y4$Globsnow3>0),],aes(SWE,Globsnow3))+
  ylab('GlobSnow3 (mm)')+xlab(paste('Reference SWE (mm)   December','\n','(b)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria4['Globsnow3','cor']), colour="#C82423",size=0.8)+   geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p3<-p3+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p3

lb<-paste('R = ',round(criteria1['AMSR2','cor'],2),'\n','RMSE = ',round(criteria1['AMSR2','RMSE'],2),' mm\n','MAE = ',round(criteria1['AMSR2','MAE'],2),' mm',sep = '')
p4<-ggplot(vdata_y1[which(vdata_y1$AMSR2>0),],aes(SWE,AMSR2))+
  ylab('AMSR2 (mm)')+xlab(paste('Reference SWE (mm)   January','\n','(e)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria1['AMSR2','cor']), colour="#C82423",size=0.8)+   geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p4<-p4+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p4

lb<-paste('R = ',round(criteria2['AMSR2','cor'],2),'\n','RMSE = ',round(criteria2['AMSR2','RMSE'],2),' mm\n','MAE = ',round(criteria2['AMSR2','MAE'],2),' mm',sep = '')
p5<-ggplot(vdata_y2[which(vdata_y2$AMSR2>0),],aes(SWE,AMSR2))+
  ylab('AMSR2 (mm)')+xlab(paste('Reference SWE (mm)   February','\n','(i)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria2['AMSR2','cor']), colour="#C82423",size=0.8)+   geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p5<-p5+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p5


lb<-paste('R = ',round(criteria4['AMSR2','cor'],2),'\n','RMSE = ',round(criteria4['AMSR2','RMSE'],2),' mm\n','MAE = ',round(criteria4['AMSR2','MAE'],2),' mm',sep = '')
p6<-ggplot(vdata_y4[which(vdata_y4$AMSR2>0),],aes(SWE,AMSR2))+
  ylab('AMSR2 (mm)')+xlab(paste('Reference SWE (mm)   December','\n','(a)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria4['AMSR2','cor']), colour="#C82423",size=0.8)+   geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p6<-p6+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p6


lb<-paste('R = ',round(criteria1['NCAswe','cor'],2),'\n','RMSE = ',round(criteria1['NCAswe','RMSE'],2),' mm\n','MAE = ',round(criteria1['NCAswe','MAE'],2),' mm',sep = '')
p7<-ggplot(vdata_y1[which(vdata_y1$NCAswe>0),],aes(SWE,NCAswe))+
  ylab('NCA (mm)')+xlab(paste('Reference SWE (mm)   January','\n','(g)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria1['NCAswe','cor']), colour="#C82423",size=0.8)+   geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p7<-p7+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p7

lb<-paste('R = ',round(criteria2['NCAswe','cor'],2),'\n','RMSE = ',round(criteria2['NCAswe','RMSE'],2),' mm\n','MAE = ',round(criteria2['NCAswe','MAE'],2),' mm',sep = '')
p8<-ggplot(vdata_y2[which(vdata_y2$NCAswe>0),],aes(SWE,NCAswe))+
  ylab('NCA (mm)')+xlab(paste('Reference SWE (mm)   February','\n','(k)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria2['NCAswe','cor']), colour="#C82423",size=0.8)+   geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p8<-p8+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p8


lb<-paste('R = ',round(criteria4['NCAswe','cor'],2),'\n','RMSE = ',round(criteria4['NCAswe','RMSE'],2),' mm\n','MAE = ',round(criteria4['NCAswe','MAE'],2),' mm',sep = '')
p9<-ggplot(vdata_y4[which(vdata_y4$NCAswe>0),],aes(SWE,NCAswe))+
  ylab('NCA (mm)')+xlab(paste('Reference SWE (mm)   December','\n','(c)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria4['NCAswe','cor']), colour="#C82423",size=0.8)+   geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p9<-p9+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p9

lb<-paste('R = ',round(criteria1['ols','cor'],2),'\n','RMSE = ',round(criteria1['ols','RMSE'],2),' mm\n','MAE = ',round(criteria1['ols','MAE'],2),' mm',sep = '')
p10<-ggplot(vdata_y1[which(vdata_y1$ols>0),],aes(SWE,ols))+
  ylab('RM (mm)')+xlab(paste('Reference SWE (mm)   January','\n','(h)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria1['ols','cor']), colour="#C82423",size=0.8)+   geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p10<-p10+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p10

lb<-paste('R = ',round(criteria2['ols','cor'],2),'\n','RMSE = ',round(criteria2['ols','RMSE'],2),' mm\n','MAE = ',round(criteria2['ols','MAE'],2),' mm',sep = '')
p11<-ggplot(vdata_y2[which(vdata_y2$ols>0),],aes(SWE,ols))+
  ylab('RM (mm)')+xlab(paste('Reference SWE (mm)   February','\n','(l)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria2['ols','cor']), colour="#C82423",size=0.8)+   geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p11<-p11+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p11


lb<-paste('R = ',round(criteria4['ols','cor'],2),'\n','RMSE = ',round(criteria4['ols','RMSE'],2),' mm\n','MAE = ',round(criteria4['ols','MAE'],2),' mm',sep = '')
p12<-ggplot(vdata_y4[which(vdata_y4$ols>0),],aes(SWE,ols))+
  ylab('RM (mm)')+xlab(paste('Reference SWE (mm)   December','\n','(d)',sep = ''))+
  geom_pointdensity(size=1) +      scale_color_viridis()+   # geom_point(size=0.3,colour="#A5B6C5")+
  geom_abline(aes(intercept=0,slope=criteria4['ols','cor']), colour="#C82423",size=0.8)+   geom_abline(aes(intercept=0,slope=1), colour="#C82423",size=0.8,lty=2)+
  lims(x= c(0,350), y = c(0,350))+
  annotate("text",x=0,y=300,label=lb,size=3.2,hjust = 0,family="serif")+
  theme_classic()+   theme(legend.title = element_text(family = "serif",size = 10))+   labs(col = "Count",family = "serif",size = 10)+ theme_bw()
p12<-p12+theme(axis.title.x = element_text(family = "serif",face = "bold",size = 10))+theme(axis.title.y = element_text(family = "serif",face = "bold",size =10))
p12



p<-plot_grid(p6,p3,p9,p12,p4,p1,p7,p10,p5,p2,p8,p11,cols=4)
p
# ggsave('D:/chenyuejun/SWE/RS/paper/plots/cor.test.jpg', width =7,height = 8 ,p, device = "jpg", dpi = 800)
ggsave('D:/chenyuejun/cor.test.jpg', width =14,height = 8 ,p, device = "jpg", dpi = 800)


###############################
# 2 探索分析箱型图
###############################
df = read.csv('D:/chenyuejun/SWE/RS/paper/figure5/databox1.csv') 
df <- na.omit(df)
df$ym[df$ym=='Dec., 2010'] <- '2010年12月'
df$ym[df$ym=='Dec., 2011'] <- '2011年12月'
df$ym[df$ym=='Dec., 2012'] <- '2012年12月'
df$ym[df$ym=='Dec., 2013'] <- '2013年12月'
df$ym[df$ym=='Dec., 2014'] <- '2014年12月'
df$ym[df$ym=='Dec., 2015'] <- '2015年12月'
df$ym[df$ym=='Jan., 2010'] <- '2010年1月'
df$ym[df$ym=='Jan., 2011'] <- '2011年1月'
df$ym[df$ym=='Jan., 2012'] <- '2012年1月'
df$ym[df$ym=='Jan., 2013'] <- '2013年1月'
df$ym[df$ym=='Jan., 2014'] <- '2014年1月'
df$ym[df$ym=='Jan., 2015'] <- '2015年1月'
df$ym[df$ym=='Feb., 2010'] <- '2010年2月'
df$ym[df$ym=='Feb., 2011'] <- '2011年2月'
df$ym[df$ym=='Feb., 2012'] <- '2012年2月'
df$ym[df$ym=='Feb., 2013'] <- '2013年2月'
df$ym[df$ym=='Feb., 2014'] <- '2014年2月'
df$ym[df$ym=='Feb., 2015'] <- '2015年2月'


p1 = ggplot(df[df$ym%in%c('2010年12月','2011年1月','2011年2月'),], aes(x=factor(ym,levels =c('2010年12月','2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月')), y=SNWD))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                              outlier.color = "black" # 外点颜色
  )+lims(y = c(150,650))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#36B77B','#FAE620'), color = "black")+ylab('SD (mm)')
p1

p2 = ggplot(df[df$ym%in%c('2011年12月','2012年1月','2012年2月'),], aes(x=factor(ym,levels =c('2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月')), y=SNWD))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(150,650))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#3F0758','#36B77B','#FAE620'), color = "black")+ylab('SD (mm)')
p2

p3 = ggplot(df[df$ym%in%c('2012年12月','2013年1月','2013年2月'),], aes(x=factor(ym,levels =c('2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月')), y=SNWD))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(150,650))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#3F0758','#36B77B','#FAE620'), color = "black")+ylab('SD (mm)')
p3

p4 = ggplot(df[df$ym%in%c('2013年12月','2014年1月','2014年2月'),], aes(x=factor(ym,levels =c('2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月')), y=SNWD))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(150,650))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#3F0758','#36B77B','#FAE620'), color = "black")+ylab('SD (mm)')
p4

p5 = ggplot(df[df$ym%in%c('2014年12月','2015年1月','2015年2月'),], aes(x=factor(ym,levels =c('2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月','2015年1月','2015年2月')), y=SNWD))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(150,650))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#3F0758'), color = "black")+ylab('SD (mm)')
p5

p<-plot_grid(p1,p2,p3,p4,p5,cols=5)
p
# ggsave('D:/chenyuejun/SWE/RS/paper/proof/SNWD.jpg', width =7,height = 3 ,p, device = "jpg", dpi = 800)
ggsave('D:/chenyuejun/论文/大论文/图/雪深.png', width =7,height = 3 ,p, device = "png", dpi = 800)



df = read.csv('D:/chenyuejun/SWE/RS/paper/figure5/databox2.csv') 
df <- na.omit(df)
df$WESD <- df$WESD/10
df$DENST <- df$WESD/df$SNWD
df$ym[df$ym=='Dec., 2010'] <- '2010年12月'
df$ym[df$ym=='Dec., 2011'] <- '2011年12月'
df$ym[df$ym=='Dec., 2012'] <- '2012年12月'
df$ym[df$ym=='Dec., 2013'] <- '2013年12月'
df$ym[df$ym=='Dec., 2014'] <- '2014年12月'
df$ym[df$ym=='Dec., 2015'] <- '2015年12月'
df$ym[df$ym=='Jan., 2010'] <- '2010年1月'
df$ym[df$ym=='Jan., 2011'] <- '2011年1月'
df$ym[df$ym=='Jan., 2012'] <- '2012年1月'
df$ym[df$ym=='Jan., 2013'] <- '2013年1月'
df$ym[df$ym=='Jan., 2014'] <- '2014年1月'
df$ym[df$ym=='Jan., 2015'] <- '2015年1月'
df$ym[df$ym=='Feb., 2010'] <- '2010年2月'
df$ym[df$ym=='Feb., 2011'] <- '2011年2月'
df$ym[df$ym=='Feb., 2012'] <- '2012年2月'
df$ym[df$ym=='Feb., 2013'] <- '2013年2月'
df$ym[df$ym=='Feb., 2014'] <- '2014年2月'
df$ym[df$ym=='Feb., 2015'] <- '2015年2月'



p1 = ggplot(df, aes(x=factor(ym,levels =c('2010年12月','2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月')), y=WESD))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+
  theme_bw()+lims(y = c(0,170))+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ylab('SWE (mm)')
p1



p1 = ggplot(df[df$ym%in%c('2010年12月','2011年1月','2011年2月'),], aes(x=factor(ym,levels =c('2010年12月','2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月')), y=WESD))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(0,170))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#36B77B','#FAE620'), color = "black")+ylab('SWE (mm)')
p1

p2 = ggplot(df[df$ym%in%c('2011年12月','2012年1月','2012年2月'),], aes(x=factor(ym,levels =c('2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月')), y=WESD))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(0,170))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#3F0758','#36B77B','#FAE620'), color = "black")+ylab('SWE (mm)')
p2

p3 = ggplot(df[df$ym%in%c('2012年12月','2013年1月','2013年2月'),], aes(x=factor(ym,levels =c('2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月')), y=WESD))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(0,170))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#3F0758','#36B77B','#FAE620'), color = "black")+ylab('SWE (mm)')
p3

p4 = ggplot(df[df$ym%in%c('2013年12月','2014年1月','2014年2月'),], aes(x=factor(ym,levels =c('2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月')), y=WESD))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(0,170))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#3F0758','#36B77B','#FAE620'), color = "black")+ylab('SWE (mm)')
p4

p5 = ggplot(df[df$ym%in%c('2014年12月','2015年1月','2015年2月'),], aes(x=factor(ym,levels =c('2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月','2015年1月','2015年2月')), y=WESD))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(0,170))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#3F0758'), color = "black")+ylab('SWE (mm)')
p5

p<-plot_grid(p1,p2,p3,p4,p5,cols=5)
p
# ggsave('D:/chenyuejun/SWE/RS/paper/proof/WESD.jpg', width =7,height = 3 ,p, device = "jpg", dpi = 800)
ggsave('D:/chenyuejun/论文/大论文/图/雪水当量.png', width =7,height = 3 ,p, device = "png", dpi = 800)

p1 = ggplot(df[df$ym%in%c('2010年12月','2011年1月','2011年2月'),], aes(x=factor(ym,levels =c('2010年12月','2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月')), y=DENST))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(0.175,0.3))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#36B77B','#FAE620'), color = "black")+ylab('Density (mm)')
p1

p2 = ggplot(df[df$ym%in%c('2011年12月','2012年1月','2012年2月'),], aes(x=factor(ym,levels =c('2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月')), y=DENST))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(0.175,0.3))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#3F0758','#36B77B','#FAE620'), color = "black")+ylab('Density (mm)')
p2

p3 = ggplot(df[df$ym%in%c('2012年12月','2013年1月','2013年2月'),], aes(x=factor(ym,levels =c('2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月')), y=DENST))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(0.175,0.3))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#3F0758','#36B77B','#FAE620'), color = "black")+ylab('Density (mm)')
p3

p4 = ggplot(df[df$ym%in%c('2013年12月','2014年1月','2014年2月'),], aes(x=factor(ym,levels =c('2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月')), y=DENST))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(0.175,0.3))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#3F0758','#36B77B','#FAE620'), color = "black")+ylab('Density (mm)')
p4

p5 = ggplot(df[df$ym%in%c('2014年12月','2015年1月','2015年2月'),], aes(x=factor(ym,levels =c('2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月','2015年1月','2015年2月')), y=DENST))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(0.175,0.3))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#3F0758'), color = "black")+ylab('Density (mm)')
p5

p<-plot_grid(p1,p2,p3,p4,p5,cols=5)
p
# ggsave('D:/chenyuejun/SWE/RS/paper/proof/DENST.jpg', width =7,height = 3 ,p, device = "jpg", dpi = 800)
ggsave('D:/chenyuejun/论文/大论文/图/雪密度.png', width =7,height = 3 ,p, device = "png", dpi = 800)


tmp0 <- modeldataunlist[,c('DATE','NCASFrac')]
tmp <- aggregate(tmp0$NCASFrac,by=list(type=tmp0$DATE),mean)
tmp[,c('y')] <- substr(tmp[,c('type')],1,4)
tmp[,c('m')] <- NA
tmp[which(substr(tmp$type,5,6)=='01'),c('m')] <- 'January'
tmp[which(substr(tmp$type,5,6)=='02'),c('m')] <- 'February'
tmp[which(substr(tmp$type,5,6)=='12'),c('m')] <- 'December'
tmp[,c('ym')] <- paste(tmp[,c('m')],as.character(tmp[,c('y')]),sep=' ')
df <- na.omit(tmp)
colnames(df) <- c("type" ,"SCF",    "y"  ,  "m"  ,  "ym" )
df <- rbind(df,c(9999,9999.111,9999,9999,'2010年12月'))
df <- rbind(df,c(9999,9999.111,9999,9999,'2015年1月'))
df <- rbind(df,c(9999,9999.111,9999,9999,'2015年2月'))
df$SCF <- as.numeric(df$SCF)

p1 = ggplot(df[df$ym%in%c('2010年12月','2011年1月','2011年2月'),], aes(x=factor(ym,levels =c('2010年12月','2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月')), y=SCF))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(0.2,0.8))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#36B77B','#FAE620'), color = "black")+ylab('SCF')
p1

p2 = ggplot(df[df$ym%in%c('2011年12月','2012年1月','2012年2月'),], aes(x=factor(ym,levels =c('2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月')), y=SCF))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(0.2,0.8))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#3F0758','#36B77B','#FAE620'), color = "black")+ylab('SCF')
p2

p3 = ggplot(df[df$ym%in%c('2012年12月','2013年1月','2013年2月'),], aes(x=factor(ym,levels =c('2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月')), y=SCF))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(0.2,0.8))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#3F0758','#36B77B','#FAE620'), color = "black")+ylab('SCF')
p3

p4 = ggplot(df[df$ym%in%c('2013年12月','2014年1月','2014年2月'),], aes(x=factor(ym,levels =c('2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月')), y=SCF))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(0.2,0.8))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#3F0758','#36B77B','#FAE620'), color = "black")+ylab('SCF')
p4

p5 = ggplot(df[df$ym%in%c('2014年12月','2015年1月','2015年2月'),], aes(x=factor(ym,levels =c('2011年1月','2011年2月','2011年12月','2012年1月','2012年2月','2012年12月','2013年1月','2013年2月','2013年12月','2014年1月','2014年2月','2014年12月','2015年1月','2015年2月')), y=SCF))+
  geom_boxplot()+xlab('')+geom_boxplot(alpha = 1,              # 透明度
                                       outlier.color = "black" # 外点颜色
  )+lims(y = c(0.2,0.8))+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 60,
                               vjust = 0.5,family="serif"
    ),axis.title = element_text(family="serif"),     # x轴刻度改为倾斜90度，防止名称重叠
  )+ geom_boxplot(fill = c('#3F0758'), color = "black")+ylab('SCF')
p5

p<-plot_grid(p1,p2,p3,p4,p5,cols=5)
p
# ggsave('D:/chenyuejun/SWE/RS/paper/plots/SCF.jpg', width =7,height = 3 ,p, device = "jpg", dpi = 800)
ggsave('D:/chenyuejun/论文/大论文/图/积雪覆盖比例.png', width =7,height = 3 ,p, device = "png", dpi = 800)


round(aggregate(modeldataunlist$NCASFrac,by=list(type=substr(modeldataunlist$DATE,5,6)),mean)$x,2)




###############################
# 3 moranI 散点图
###############################
moranSD <-  ggplot(data = SNWDmoran) + 
  geom_point(mapping = aes(x = i, y = mc))+lims(y = c(0,1))
moranSD
ggsave('D:/chenyuejun/SWE/RS/paper/plots/moranSD.jpg', width =7.5,height = 5 ,moranSD, device = "jpg", dpi = 800)

###############################
# 4 探索分析因子筛选相关系数图
###############################
# correlation 
install.packages('Cairo')
library(Cairo)
tmp.TB.names<-c("TB37H37V","TB37H91H","TB37H91V","TB37H19H","TB37H19V","TB37H22V","TB37V91H","TB37V91V","TB37V19H","TB37V19V","TB37V22V","TB91H91V","TB91H19H","TB91H19V","TB91H22V","TB91V19H","TB91V19V","TB91V22V","TB19H19V","TB19H22V","TB19V22V")
tmp.factors.names<-c("SNWD","LAT","LON","NCAQg","NCAQsm","NCASnowf","NCASoilT","NCATair","NCASFrac","NCAWind","TC","TPelev","TPslope")
cor.1 <- cor(modeldataEV[[1]][,c(tmp.factors.names,tmp.TB.names)])
colnames(cor.1) <- c("SD","LAT","LON","QG","QSM","SPR","ST","AT","SCF","WS","TC","ELV","SLP","TBD37h37v","TBD37h91h","TBD37h91v","TBD37h19h","TBD37h19v","TBD37h22v","TBD37v91h","TBD37v91v","TBD37v19h","TBD37v19v","TBD37v22v","TBD91h91v","TBD91h19h","TBD91h19v","TBD91h22v","TBD91v19h","TBD91v19v","TBD91v22v","TBD19h19v","TBD19h22v","TBD19v22v")
rownames(cor.1) <- c("SD","LAT","LON","QG","QSM","SPR","ST","AT","SCF","WS","TC","ELV","SLP","TBD37h37v","TBD37h91h","TBD37h91v","TBD37h19h","TBD37h19v","TBD37h22v","TBD37v91h","TBD37v91v","TBD37v19h","TBD37v19v","TBD37v22v","TBD91h91v","TBD91h19h","TBD91h19v","TBD91h22v","TBD91v19h","TBD91v19v","TBD91v22v","TBD19h19v","TBD19h22v","TBD19v22v")
tbname <- c("SD","TBD37h37v","TBD37h91h","TBD37h91v","TBD37h19h","TBD37h19v","TBD37h22v","TBD37v91h","TBD37v91v","TBD37v19h","TBD37v19v","TBD37v22v","TBD91h91v","TBD91h19h","TBD91h19v","TBD91h22v","TBD91v19h","TBD91v19v","TBD91v22v","TBD19h19v","TBD19h22v","TBD19v22v")
fcname <- c("SD","LAT","LON","ELV","SLP","QSM","SPR","SCF","AT","ST","QG","WS","TC")

Cairo::CairoJPEG( 
  filename = "D:/chenyuejun/SWE/RS/paper/plots/cor.tb.1.jpg", # 文件名称
  width = 7000,           # 宽
  height = 7000,          # 高
  units = "px",        # 单位
  dpi = 500)           # 分辨率
# 2. 绘图
cor.plot<-corrplot.mixed(cor.1[tbname,tbname],tl.pos = 'lt',tl.cex=1.8,tl.col='black',number.font = NULL,cl.cex=2,number.cex = 1.3,family= "Times New Roman")
# 3. 关闭画布
dev.off() 

Cairo::CairoJPEG( 
  filename = "D:/chenyuejun/SWE/RS/paper/plots/cor.fc.1.jpg", # 文件名称
  width = 5000,           # 宽
  height = 5000,          # 高
  units = "px",        # 单位
  dpi = 500)           # 分辨率
# 2. 绘图
cor.plot<-corrplot.mixed(cor.1[fcname,fcname],tl.pos = 'lt',tl.cex=1.8,tl.col='black',number.font = NULL,cl.cex=2,number.cex = 1.3,family= "Times New Roman")
# 3. 关闭画布
dev.off() 


cor.2 <- cor(modeldataEV[[2]][,c(tmp.factors.names,tmp.TB.names)])
colnames(cor.2) <- c("SD","LAT","LON","QG","QSM","SPR","ST","AT","SCF","WS","TC","ELV","SLP","TBD37h37v","TBD37h91h","TBD37h91v","TBD37h19h","TBD37h19v","TBD37h22v","TBD37v91h","TBD37v91v","TBD37v19h","TBD37v19v","TBD37v22v","TBD91h91v","TBD91h19h","TBD91h19v","TBD91h22v","TBD91v19h","TBD91v19v","TBD91v22v","TBD19h19v","TBD19h22v","TBD19v22v")
rownames(cor.2) <- c("SD","LAT","LON","QG","QSM","SPR","ST","AT","SCF","WS","TC","ELV","SLP","TBD37h37v","TBD37h91h","TBD37h91v","TBD37h19h","TBD37h19v","TBD37h22v","TBD37v91h","TBD37v91v","TBD37v19h","TBD37v19v","TBD37v22v","TBD91h91v","TBD91h19h","TBD91h19v","TBD91h22v","TBD91v19h","TBD91v19v","TBD91v22v","TBD19h19v","TBD19h22v","TBD19v22v")
tbname <- c("SD","TBD37h37v","TBD37h91h","TBD37h91v","TBD37h19h","TBD37h19v","TBD37h22v","TBD37v91h","TBD37v91v","TBD37v19h","TBD37v19v","TBD37v22v","TBD91h91v","TBD91h19h","TBD91h19v","TBD91h22v","TBD91v19h","TBD91v19v","TBD91v22v","TBD19h19v","TBD19h22v","TBD19v22v")
fcname <- c("SD","LAT","LON","ELV","SLP","QSM","SPR","SCF","AT","ST","QG","WS","TC")
Cairo::CairoJPEG( 
  filename = "D:/chenyuejun/SWE/RS/paper/plots/cor.tb.2.jpg", # 文件名称
  width = 7000,           # 宽
  height = 7000,          # 高
  units = "px",        # 单位
  dpi = 500)           # 分辨率
# 2. 绘图
cor.plot<-corrplot.mixed(cor.2[tbname,tbname],tl.pos = 'lt',tl.cex=1.8,tl.col='black',number.font = NULL,cl.cex=2,number.cex = 1.3,family= "Times New Roman")
# 3. 关闭画布
dev.off() 

Cairo::CairoJPEG( 
  filename = "D:/chenyuejun/SWE/RS/paper/plots/cor.fc.2.jpg", # 文件名称
  width = 5000,           # 宽
  height = 5000,          # 高
  units = "px",        # 单位
  dpi = 500)           # 分辨率
# 2. 绘图
cor.plot<-corrplot.mixed(cor.2[fcname,fcname],tl.pos = 'lt',tl.cex=1.8,tl.col='black',number.font = NULL,cl.cex=2,number.cex = 1.3,family= "Times New Roman")
# 3. 关闭画布
dev.off() 


cor.4 <- cor(modeldataEV[[4]][,c(tmp.factors.names,tmp.TB.names)])
colnames(cor.4) <- c("SD","LAT","LON","QG","QSM","SPR","ST","AT","SCF","WS","TC","ELV","SLP","TBD37h37v","TBD37h91h","TBD37h91v","TBD37h19h","TBD37h19v","TBD37h22v","TBD37v91h","TBD37v91v","TBD37v19h","TBD37v19v","TBD37v22v","TBD91h91v","TBD91h19h","TBD91h19v","TBD91h22v","TBD91v19h","TBD91v19v","TBD91v22v","TBD19h19v","TBD19h22v","TBD19v22v")
rownames(cor.4) <- c("SD","LAT","LON","QG","QSM","SPR","ST","AT","SCF","WS","TC","ELV","SLP","TBD37h37v","TBD37h91h","TBD37h91v","TBD37h19h","TBD37h19v","TBD37h22v","TBD37v91h","TBD37v91v","TBD37v19h","TBD37v19v","TBD37v22v","TBD91h91v","TBD91h19h","TBD91h19v","TBD91h22v","TBD91v19h","TBD91v19v","TBD91v22v","TBD19h19v","TBD19h22v","TBD19v22v")
tbname <- c("SD","TBD37h37v","TBD37h91h","TBD37h91v","TBD37h19h","TBD37h19v","TBD37h22v","TBD37v91h","TBD37v91v","TBD37v19h","TBD37v19v","TBD37v22v","TBD91h91v","TBD91h19h","TBD91h19v","TBD91h22v","TBD91v19h","TBD91v19v","TBD91v22v","TBD19h19v","TBD19h22v","TBD19v22v")
fcname <- c("SD","LAT","LON","ELV","SLP","QSM","SPR","SCF","AT","ST","QG","WS","TC")
Cairo::CairoJPEG( 
  filename = "D:/chenyuejun/SWE/RS/paper/plots/cor.tb.4.jpg", # 文件名称
  width = 7000,           # 宽
  height = 7000,          # 高
  units = "px",        # 单位
  dpi = 500)           # 分辨率
# 2. 绘图
cor.plot<-corrplot.mixed(cor.4[tbname,tbname],tl.pos = 'lt',tl.cex=1.8,tl.col='black',number.font = NULL,cl.cex=2,number.cex = 1.3,family= "Times New Roman")
# 3. 关闭画布
dev.off() 

Cairo::CairoJPEG( 
  filename = "D:/chenyuejun/SWE/RS/paper/plots/cor.fc.4.jpg", # 文件名称
  width = 5000,           # 宽
  height = 5000,          # 高
  units = "px",        # 单位
  dpi = 500)           # 分辨率
# 2. 绘图
cor.plot<-corrplot.mixed(cor.4[fcname,fcname],tl.pos = 'lt',tl.cex=1.8,tl.col='black',number.font = NULL,cl.cex=2,number.cex = 1.3,family= "Times New Roman")
# 3. 关闭画布
dev.off()

