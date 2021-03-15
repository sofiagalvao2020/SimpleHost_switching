setwd("C:/Users/sofia/Documents/MEGAsync/MEGAsync/modelos/pergunta/4/5/testes_em_u_b_Y/Figura 3")
##script gera o plot de frequencia relativa em u=10-2
#####Video no R
ymax=max(fenotipos[,3])

geraco1=fenotipos [fenotipos$V1==1, ]
plot(geracao1[,2],geracao1[,3],col="black",pch=19,xlim=c(35,65),ylim=c(0,ymax),main= "gene=0")
for (i in 1:70){
  geracaon=fenotipos[fenotipos$V1==i, ]
  plot(geracaon[,2],geracaon[,3],col="black",pch=19,,xlim=c(35,65),ylim=c(0,ymax), main =paste0(main="gene=",i) )
  Sys.sleep(0.5)
}

#Gif
#require (animation)
ymax=max(fenotipos[,3])
geraco1=fenotipos [fenotipos$V1==1, ]
saveGIF({
  plot(geracao1[,2],geracao1[,3],col="grey",pch=19,xlim=c(35,65),ylim=c(0,ymax),main="u=10-2,y=10, gene=0")
  for (i in 1:70){
    geracaon=fenotipos[fenotipos$V1==i, ]
    plot(geracaon[,2],geracaon[,3],col="black",pch=19,,xlim=c(35,65),ylim=c(0,ymax), main =paste0(main="u=10-2,y=10, gene=",i) )
    Sys.sleep(1.0)
  }
},interval = 0.1)
###################################################################
#ggplot
library("ggplot2")
library("RColorBrewer")
library("gridExtra")
#u=10-2,no=1,b=1.7####
fenotipos1=read.table("numero_de_ind_por_soma02_pbaixa.txt")
fenotipos1=fenotipos1[fenotipos1$V3!=0,]

#fazendo frequencia relativa pra tabela:

fenotipos1$Vf=fenotipos1$V3/1000

geracao10=fenotipos1[fenotipos1$V1==10, ]
geracao13=fenotipos1[fenotipos1$V1==13, ]
geracao25=fenotipos1[fenotipos1$V1==25, ]
geracao35=fenotipos1[fenotipos1$V1==35, ]
geracao500=fenotipos1[fenotipos1$V1==500, ]

gen1=rbind(geracao10,geracao13,geracao25,geracao35,geracao500)
gen1$generation=c(rep("10",7),rep("13",6),rep("25",18),rep("35",19),rep("500",21))

plot1 <- ggplot(gen1, aes(x=V5,y=Vf,group=gen1$V1,method="lm",color=generation)) + 
  geom_vline(xintercept = 0,size=0.5,color="black")+
  geom_vline(xintercept = 1.0,size=0.5,color="darkgrey")+
      geom_smooth(size=1.5,se=FALSE,span=0.5)+
  theme_bw()+
  labs(title="a",x=" ", y="% freq")+
  scale_colour_manual(values = c('gold','orange1','darkorange1','red','darkred'))+
  theme(axis.title.y=element_text(size=16),
        legend.position="none",
        legend.text=element_text(size=16),
      #axis.text.y = element_blank(),
      plot.title=element_text(size=16,hjust=0.1),
      axis.title.x = element_text(size=16),
      axis.text.x = element_text(size=16),
        legend.title = element_blank()) +
  coord_cartesian(xlim=c(1.5,-1.0),ylim=c(0,0.6))
####u=10-2,n0=200,b=1.7###
fenotipos2=read.table("numero_de_ind_por_soma02_palta.txt")
fenotipos2=fenotipos2[fenotipos2$V3!=0,]

#fazendo frequencia relativa pra tabela:

fenotipos2$Vf=fenotipos2$V3/1000

geracao10=fenotipos2[fenotipos2$V1==10, ]
geracao13=fenotipos2[fenotipos2$V1==13, ]
geracao25=fenotipos2[fenotipos2$V1==25, ]
geracao35=fenotipos2[fenotipos2$V1==35, ]
geracao500=fenotipos2[fenotipos2$V1==500, ]

gen2=rbind(geracao10,geracao13,geracao25,geracao35,geracao500)
gen2$generation=c(rep("10",16),rep("13",17),rep("25",19),rep("35",16),rep("500",21))
cols <- c("10"="gold","13"="orange2","25"="darkorange2","35"="red","500"="darkred")
plot3 <- ggplot(gen2, aes(x=V5,y=Vf,group=V1,method="lm",color=generation)) + 
  # geom_point(color="black") + 
  geom_vline(xintercept = 0,size=0.5,color="black")+
  geom_vline(xintercept = 1.0,size=0.5,color="darkgrey")+
  geom_smooth(size=1.5,se=FALSE,span=0.5)+
  theme_bw()+
  labs(title="b",x=" ", y="% freq")+
  scale_colour_manual(values = cols)+
  scale_y_continuous(position = "left")+
  #xlim(NA,1.0)++
  theme(axis.title.y=element_text(size=14),
        legend.text=element_text(size=16),
        legend.position="none",
        plot.title=element_text(size=16,hjust=0.1),
        axis.text.y = element_text(size=14),
        axis.title.y.left = element_text(size=16),
        axis.title.x = element_text(size=16),
        axis.text.x=element_text(size=14),
        legend.title = element_blank()) +
  coord_cartesian(xlim=c(1.5,-1.0),ylim=c(0.00,0.6))
plot3

##u=10-4,no=1,b=1.7####
fenotipos3=read.table("num_ind_por_soma_u_10-4_pbaixa.txt")
ymax=max(fenotipos3[,3])
fenotipos3=fenotipos3[fenotipos3$V3!=0,]


#freq relativa

fenotipos3$Vf=fenotipos3$V3/1000

geracao30=fenotipos3[fenotipos3$V1==30, ]
geracao60=fenotipos3[fenotipos3$V1==60, ]
geracao120=fenotipos3[fenotipos3$V1==120, ]
geracao200=fenotipos3[fenotipos3$V1==200, ]
geracao500=fenotipos3[fenotipos3$V1==500, ]


gen3=rbind(geracao30,geracao60,geracao120,geracao200,geracao500)
gen3$generation=c(rep("30",5),rep("60",6),rep("120",6),rep("200",7),rep("500",7))

cols <- c("30"="gold","60"="orange2","120"="darkorange2","200"="red","500"="darkred")
plot2 <- ggplot(gen3, aes(x=V5,y=Vf,group=V1,method="lm",color=generation)) + 
  # geom_point(color="black") + 
  geom_vline(xintercept = 0,size=0.5,color="black")+
  geom_vline(xintercept = 1.0,size=0.5,color="darkgrey")+
    geom_smooth(size=1.5,se=FALSE,span=0.5)+
  theme_bw()+
  labs(title="d",x=" ", y=" ")+
  scale_colour_manual(values = cols)+
  scale_y_continuous(position = "left")+
  #xlim(NA,1.0)++
  theme(axis.title.y=element_text(size=14),
        legend.text=element_text(size=16),
        legend.position="none",
        plot.title=element_text(size=16,hjust=0.1),
        axis.text.y = element_text(size=14),
        axis.title.y.left = element_text(size=16),
        axis.title.x = element_text(size=16),
        axis.text.x=element_text(size=14),
        legend.title = element_blank()) +
  coord_cartesian(xlim=c(1.5,-1.0))
plot2
###u=10-4,no=200,b=1.7###
fenotipos4=read.table("numero_de_ind_por_soma04_palta.txt")
ymax=max(fenotipos4[,3])
fenotipos4=fenotipos4[fenotipos4$V3!=0,]


#freq relativa

fenotipos4$Vf=fenotipos4$V3/1000

geracao30=fenotipos4[fenotipos4$V1==30, ]
geracao60=fenotipos4[fenotipos4$V1==60, ]
geracao120=fenotipos4[fenotipos4$V1==120, ]
geracao200=fenotipos4[fenotipos4$V1==200, ]
geracao500=fenotipos4[fenotipos4$V1==500, ]

gen4=rbind(geracao30,geracao60,geracao120,geracao200,geracao500)
gen4$generation=c(rep("30",4),rep("60",6),rep("120",4),rep("200",6),rep("500",9))

cols <- c("30"="gold","60"="orange2","120"="darkorange2","200"="red","500"="darkred")
plot4 <- ggplot(gen4, aes(x=V5,y=Vf,group=V1,method="lm",color=generation)) + 
  # geom_point(color="black") + 
  geom_vline(xintercept = 0,size=0.5,color="black")+
  geom_vline(xintercept = 1.0,size=0.5,color="darkgrey")+
  geom_smooth(size=1.5,se=FALSE,span=0.5)+
  theme_bw()+
  labs(title="e",x=" ", y=" ")+
  scale_colour_manual(values = cols)+
  scale_y_continuous(position = "left")+
  #xlim(NA,1.0)++
  theme(axis.title.y=element_text(size=14),
        legend.text=element_text(size=16),
        legend.position="none",
        plot.title=element_text(size=16,hjust=0.1),
        axis.text.y = element_text(size=14),
        axis.title.y.left = element_text(size=16),
        axis.title.x = element_text(size=16),
        axis.text.x=element_text(size=14),
        legend.title = element_blank()) +
  coord_cartesian(xlim=c(1.5,-1.0))
plot4
###u=10-4,n0=1,b=7.5###
fenotipos5=read.table("numero_de_ind_por_soma04_balta.txt")
ymax=max(fenotipos5[,3])
fenotipos5=fenotipos5[fenotipos5$V3!=0,]


#freq relativa

fenotipos5$Vf=fenotipos5$V3/1000

geracao30=fenotipos5[fenotipos5$V1==30, ]
geracao60=fenotipos5[fenotipos5$V1==60, ]
geracao120=fenotipos5[fenotipos5$V1==120, ]
geracao200=fenotipos5[fenotipos5$V1==200, ]
geracao500=fenotipos5[fenotipos5$V1==500, ]

gen5=rbind(geracao30,geracao60,geracao120,geracao200,geracao500)
gen5$generation=c(rep("30",5),rep("60",6),rep("120",6),rep("200",7),rep("600",6))

cols <- c("30"="gold","60"="orange2","120"="darkorange2","200"="red","600"="darkred")
plot6 <- ggplot(gen5, aes(x=V5,y=Vf,group=V1,method="lm",color=generation)) + 
  # geom_point(color="black") + 
  geom_vline(xintercept = 0,size=0.5,color="black")+
  geom_vline(xintercept = 1.0,size=0.5,color="darkgrey")+
  geom_smooth(size=1.5,se=FALSE,span=0.5)+
  theme_bw()+
  labs(title="f",x=" ", y=" ")+
  scale_colour_manual(values = cols)+
  scale_y_continuous(position = "left")+
  #xlim(NA,1.0)++
  theme(axis.title.y=element_text(size=14),
        legend.text=element_text(size=16),
        legend.position="none",
        plot.title=element_text(size=16,hjust=0.1),
        axis.text.y = element_text(size=14),
        axis.title.y.left = element_text(size=16),
        axis.title.x = element_text(size=16),
        axis.text.x=element_text(size=14),
        legend.title = element_blank()) +
  coord_cartesian(xlim=c(1.5,-1.0),ylim=c(0.00,0.6))

plot6
####u=10-4,n0=1,b=1.7###

#fenotipos6=read.table("numero_de_ind_por_soma04_bbaixa.txt")
#ymax=max(fenotipos6[,3])
#fenotipos6=fenotipos6[fenotipos6$V3!=0,]


#freq relativa

#fenotipos6$Vf=fenotipos6$V3/1000

#geracao30=fenotipos6[fenotipos6$V1==30, ]
#geracao40=fenotipos6[fenotipos6$V1==40, ]
#geracao60=fenotipos6[fenotipos6$V1==60, ]
#geracao120=fenotipos6[fenotipos6$V1==120, ]
#geracao200=fenotipos6[fenotipos6$V1==200, ]
#geracao500=fenotipos6[fenotipos6$V1==500, ]


#gen6=rbind(geracao40,geracao60,geracao120,geracao200,geracao500)
#gen6$generation=c(rep("40",4),rep("60",5),rep("120",6),rep("200",6),rep("500",6))

#cols <- c("40"="gold","60"="orange2","120"="darkorange2","200"="red","500"="darkred")
#plot8 <- ggplot(gen6, aes(x=V5,y=Vf,group=V1,method="lm",color=generation)) + 
  # geom_point(color="black") + 
#  geom_vline(xintercept = 0,size=0.5,color="black")+
#  geom_vline(xintercept = 1.0,size=0.5,color="darkgrey")+
#  geom_smooth(size=2,se=FALSE,span=0.5)+
 # geom_point(geracao10, aes(x=V5,y=Vf))+
#  theme_bw()+
#  labs(title="u=10-4,n0=1,b=1.7   v500=6",x="Phenotype distance", y=" ")+
#  scale_colour_manual(values = cols)+
#  scale_y_continuous(position = "left")+
  #xlim(NA,1.0)++
#  theme(axis.title.y=element_text(size=14),
#        legend.text=element_text(size=16),
#        legend.position="none",
#        plot.title=element_text(size=16,hjust=0.1),
#        axis.text.y = element_text(size=14),
#        axis.title.y.left = element_text(size=16),
#        axis.title.x = element_text(size=16),
#        axis.text.x=element_text(size=14),
#        legend.title = element_blank()) +
 # coord_cartesian(xlim=c(1.5,-1.0))
#plot8
###u=10-2,no=1,b=7.5###

fenotipos7=read.table("numero_de_ind_por_soma02_balta.txt")
ymax=max(fenotipos7[,3])
fenotipos7=fenotipos7[fenotipos7$V3!=0,]


#freq relativa

fenotipos7$Vf=fenotipos7$V3/1000

geracao10=fenotipos7[fenotipos7$V1==10, ]
geracao13=fenotipos7[fenotipos7$V1==13, ]
geracao25=fenotipos7[fenotipos7$V1==25, ]
geracao35=fenotipos7[fenotipos7$V1==35, ]
geracao500=fenotipos7[fenotipos7$V1==500, ]

gen7=rbind(geracao10,geracao13,geracao25,geracao35,geracao500)
gen7$generation=c(rep("10",16),rep("13",18),rep("25",17),rep("35",20),rep("500",19))

cols <- c("10"="gold","13"="orange1","25"="darkorange2","35"="red","500"="darkred")
plot5 <- ggplot(gen7, aes(x=V5,y=Vf,group=V1,method="lm",color=generation)) + 
  # geom_point(color="black") + 
  geom_vline(xintercept = 0,size=0.5,color="black")+
  geom_vline(xintercept = 1.0,size=0.5,color="darkgrey")+
  geom_smooth(size=1.5,se=FALSE,span=0.5)+
  theme_bw()+
  labs(title="c",x=" ", y="% freq")+
  scale_colour_manual(values = cols)+
  scale_y_continuous(position = "left")+
  #xlim(NA,1.0)++
  theme(axis.title.y=element_text(size=14),
        legend.text=element_text(size=16),
        legend.position="none",
        plot.title=element_text(size=16,hjust=0.1),
        axis.text.y = element_text(size=14),
        axis.title.y.left = element_text(size=16),
        axis.title.x = element_text(size=16),
        axis.text.x=element_text(size=14),
        legend.title = element_blank()) +
  coord_cartesian(xlim=c(1.5,-1.0),ylim=c(0.00,0.6))


####u=10-2,n0=1,b=1.7### b baixo esta no primeiro plot do u=10-2 e 10-4

#fenotipos8=read.table("numero_de_ind_por_soma02_bbaixa.txt")
#ymax=max(fenotipos8[,3])
#fenotipos8=fenotipos8[fenotipos8$V3!=0,]


#freq relativa

#fenotipos8$Vf=fenotipos8$V3/1000

#geracao10=fenotipos8[fenotipos8$V1==10, ]
#geracao13=fenotipos8[fenotipos8$V1==13, ]
#geracao25=fenotipos8[fenotipos8$V1==25, ]
#geracao35=fenotipos8[fenotipos8$V1==35, ]
#geracao500=fenotipos8[fenotipos8$V1==500, ]

#gen8=rbind(geracao10,geracao13,geracao25,geracao35,geracao500)
#gen8$generation=c(rep("10",7),rep("13",6),rep("25",18),rep("35",19),rep("500",21))

#cols <- c("10"="gold","13"="orange1","25"="darkorange2","35"="red","500"="darkred")
#plot7 <- ggplot(gen8, aes(x=V5,y=Vf,group=V1,method="lm",color=generation)) + 
  # geom_point(color="black") + 
#  geom_vline(xintercept = 0,size=0.5,color="black")+
#  geom_vline(xintercept = 1.0,size=0.5,color="darkgrey")+
#  geom_smooth(size=2,se=FALSE,span=0.5)+
#  theme_bw()+
#  labs(title="u=10-2,n0=1,b=1.7   v500=21",x="Phenotype distance", y="% freq")+
#  scale_colour_manual(values = cols)+
#  scale_y_continuous(position = "left")+
  #xlim(NA,1.0)++
 # theme(axis.title.y=element_text(size=14),
#        legend.text=element_text(size=16),
#        legend.position="none",
#        plot.title=element_text(size=16,hjust=0.1),
#        axis.text.y = element_text(size=14),
#axis.title.y.left = element_text(size=16),
#        axis.title.x = element_text(size=16),
 #       axis.text.x=element_text(size=14),
 #       legend.title = element_blank()) +
#  coord_cartesian(xlim=c(1.5,-1.0),ylim=c(0.00,0.6))


#Maximo ####
#fenotiposmax=read.table("numero_de_ind_por_somamax.txt")
#ymax=max(fenotiposmax[,3])
#fenotiposmax=fenotiposmax[fenotiposmax$V3!=0,]

#freq relativa

#fenotiposmax$Vf=fenotiposmax$V3/1000


#geracao10=fenotiposmax[fenotiposmax$V1==10, ]
#geracao13=fenotiposmax[fenotiposmax$V1==13, ]
#geracao25=fenotiposmax[fenotiposmax$V1==25, ]
#geracao35=fenotiposmax[fenotiposmax$V1==35, ]
#geracao600=fenotiposmax[fenotiposmax$V1==600, ]

#genmax=rbind(geracao10,geracao13,geracao25,geracao35,geracao600)
#genmax$generation=c(rep("10",17),rep("13",17),rep("25",20),rep("35",19),rep("600",20))


#plotmax <- ggplot(genmax, aes(x=V5,y=Vf,group=V1,method="lm",color=generation)) + 
  geom_vline(xintercept=1.0,size=0.5,col="darkgrey") + 
  geom_vline(xintercept = 0,size=0.5,color="black")+
  geom_smooth(size=1.5,se=FALSE,span=0.5)+
  theme_bw()+
  labs(title="c",x="Phenotype distance" , y=" ")+
  scale_colour_manual(values = c('gold','orange1','darkorange1','red','darkred'))+
  scale_y_continuous(position = "left")+
  #xlim(NA,1.0)++
  theme(axis.title.y=element_text(size=14),
        legend.text=element_text(size=16),
        legend.position="none",
        plot.title=element_text(size=20,hjust=0.1),
        axis.text.y = element_blank(),
        axis.title.y.left = element_blank(),
        axis.title.x = element_text(size=16),
        axis.text.x=element_text(size=14),
        legend.title = element_blank()) +
        
  coord_cartesian(xlim=c(1.5,-1.0),ylim=c(0.00,0.6))


grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6,nrow =3)
