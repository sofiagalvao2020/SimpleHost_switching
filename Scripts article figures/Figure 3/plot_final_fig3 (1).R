 setwd("C:/Users/raque/Downloads/coisas da sof/Scripts figuras artigo/Figure 3")
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
#u=10-2####
fenotipos3=read.table("numero_de_ind_por_soma02.txt")
fenotipos3=fenotipos3[fenotipos3$V3!=0,]

#fazendo frequencia relativa pra tabela:

fenotipos3$Vf=fenotipos3$V3/1000

geracao10=fenotipos3[fenotipos3$V1==10, ]
geracao13=fenotipos3[fenotipos3$V1==13, ]
geracao25=fenotipos3[fenotipos3$V1==25, ]
geracao35=fenotipos3[fenotipos3$V1==35, ]
geracao500=fenotipos3[fenotipos3$V1==500, ]

gen3=rbind(geracao10,geracao13,geracao25,geracao35,geracao500)
gen3$generation=c(rep("10",7),rep("13",6),rep("25",18),rep("35",19),rep("500",21))

plot2 <- ggplot(gen3, aes(x=V5,y=Vf,group=gen3$V1,method="lm",color=generation)) + 
  geom_vline(xintercept = 0,size=0.5,color="black")+
  geom_vline(xintercept = 1.0,size=0.5,color="darkgrey")+
      geom_smooth(size=2,se=FALSE,span=0.5)+
  theme_bw()+
  labs(title="b",x="Phenotype distance ", y=" ")+
  scale_colour_manual(values = c('gold','orange1','darkorange1','red','darkred'))+
  theme(axis.title.y=element_blank(),
        legend.position="none",
        legend.text=element_text(size=16),
      axis.text.y = element_blank(),
      plot.title=element_text(size=20,hjust=0.1),
      axis.title.x = element_text(size=16),
      axis.text.x = element_text(size=14),
        legend.title = element_blank()) +
  coord_cartesian(xlim=c(1.5,-1.0),ylim=c(0,0.6))

##u=10-4####
fenotipos2=read.table("numero_de_ind_por_soma04.txt")
ymax=max(fenotipos2[,3])
fenotipos2=fenotipos2[fenotipos2$V3!=0,]


#freq relativa

fenotipos2$Vf=fenotipos2$V3/1000

geracao45=fenotipos2[fenotipos2$V1==45, ]
geracao80=fenotipos2[fenotipos2$V1==80, ]
geracao120=fenotipos2[fenotipos2$V1==120, ]
geracao200=fenotipos2[fenotipos2$V1==200, ]
geracao500=fenotipos2[fenotipos2$V1==500, ]

gen2=rbind(geracao45,geracao120,geracao200,geracao500)
gen2$generation=c(rep("45",4),rep("120",5),rep("200",7),rep("500",7))

cols <- c("45"="gold","120"="darkorange2","200"="red","500"="darkred")
plot1 <- ggplot(gen2, aes(x=V5,y=Vf,group=V1,method="lm",color=generation)) + 
  # geom_point(color="black") + 
  geom_vline(xintercept = 0,size=0.5,color="black")+
  geom_vline(xintercept = 1.0,size=0.5,color="darkgrey")+
    geom_smooth(size=2,se=FALSE,span=0.5)+
  theme_bw()+
  labs(title="a",x="Phenotype distance", y="Relative frequency")+
  scale_colour_manual(values = cols)+
  scale_y_continuous(position = "left")+
  #xlim(NA,1.0)++
  theme(axis.title.y=element_text(size=14),
        legend.text=element_text(size=16),
        legend.position="none",
        plot.title=element_text(size=20,hjust=0.1),
        axis.text.y = element_text(size=14),
        axis.title.y.left = element_text(size=16),
        axis.title.x = element_text(size=16),
        axis.text.x=element_text(size=14),
        legend.title = element_blank()) +
  coord_cartesian(xlim=c(1.5,-1.0),ylim=c(0.00,0.6))
plot1
#Maximo ####
fenotiposmax=read.table("numero_de_ind_por_somamax.txt")
ymax=max(fenotiposmax[,3])
fenotiposmax=fenotiposmax[fenotiposmax$V3!=0,]

#freq relativa

fenotiposmax$Vf=fenotiposmax$V3/1000


geracao10=fenotiposmax[fenotiposmax$V1==10, ]
geracao13=fenotiposmax[fenotiposmax$V1==13, ]
geracao25=fenotiposmax[fenotiposmax$V1==25, ]
geracao35=fenotiposmax[fenotiposmax$V1==35, ]
geracao500=fenotiposmax[fenotiposmax$V1==500, ]

genmax=rbind(geracao10,geracao13,geracao25,geracao35,geracao500)
genmax$generation=c(rep("10",17),rep("13",17),rep("25",20),rep("35",19),rep("500",20))


plotmax <- ggplot(genmax, aes(x=V5,y=Vf,group=V1,method="lm",color=generation)) + 
  geom_vline(xintercept=1.0,size=0.5,col="darkgrey") + 
  geom_vline(xintercept = 0,size=0.5,color="black")+
  geom_smooth(size=2,se=FALSE,span=0.5)+
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


grid.arrange(plot1, plot2,plotmax,nrow = 1)
