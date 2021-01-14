#plots b


library("ggplot2")
library("scales")
library("RColorBrewer")
library("gridExtra")


#Birth Rate ####
setwd("C:/Users/raque/Downloads/coisas da sof/Scripts figuras artigo/Figure 4/Birth effect")

Birthmax=read.table("contagem_probabilidade_max2.txt")

Birth1.5=read.table("contagem_probabilidade1.txt")
 
Birth3.5=read.table("contagem_probabilidade5.9.txt")
 
Birth5.5=read.table("contagem_probabilidade9.txt")
  
Birth7.5=read.table("contagem_probabilidade13 .txt")
 
Birth_prob=rbind(Birthmax,Birth1.5,Birth3.5,Birth5.5,Birth7.5)

Birth_prob$value=c(rep("max",50),rep("1.5",50),rep("3.5",50),rep("5.5",50),rep("7.5",50))
                   
#fun.1=function(x)  exp(-(x^2/(2*1^2))) 

eq.1 <- function (x) exp(-(x^2/(2*1^2)))
p1 <- ggplot(Birth_prob, aes(x=V2,y=V3,group=value,color=value)) + 
  theme_bw()+
  stat_function(fun=eq.1, geom="line", aes(colour="function"),size=1.0, color="black")+
  geom_line(aes(x =V2,y=V3), size=2.0 ) +  
  scale_x_continuous(limits = c( 0,3.5))+
  labs(title="a",x=expression("Propagule Phenotype Distance" (d[0])),y="Probability of Establishment")+
  scale_color_manual(name="Values",values=c("blue","red","green","gold","darkgrey"))+
  guides(guide_legend(ncol=2,nrow=2))+
  theme(legend.text=element_text(size=14),
        legend.title = element_blank(),
        legend.direction = "horizontal", 
        legend.position = "top",
        plot.title=element_text(size=20,hjust=0.1),
        legend.box = "horizontal",
        legend.margin=margin(),
        axis.title.x=element_text(face="italic",size=16),
        axis.title.y=element_text(size=16),
        axis.text = element_text(size=14))

  
#Novelty Rate ####

#x=expression(d~" "~["n,i"]~n=1)
setwd("C:/Users/raque/Downloads/coisas da sof/Scripts figuras artigo/Figure 4/Evolutionary Novelty effect")
      Mut03=read.table("contagem_probabilidade_28.txt") #10-3
      Mut06=read.table("contagem_probabilidade_16.txt")   #10-6
      Mut00=read.table("contagem_probabilidade_mut0.txt")  #0.0
      Mut02=read.table("contagem_probabilidade_mut1.txt") # 10-2
      Mut01=read.table("contagem_probabilidade_mut3.txt")  #10-1 dotted lines=unreal Valuessss for mutation= lty=x
      Mutmax=read.table("contagem_probabilidade_max2.txt")
      
      #A função expression() o "^" indica sobreescrito e o "_" subescrito.
      #library("ggplot2")
      #library("RColorBrewer")
      
      Mut_prob=rbind(Mut00,Mut06,Mut03,Mutmax,Mut02,Mut01)
      Mut_prob$Values=c(rep("0.0",50),rep("10-6",50),rep("10-3",50),rep("max",50),rep("dashed1",50),rep("dashed2",50))
      #Mut_prob$dashed1=Mut02[,3]
      #Mut_prob$dashed2=Mut01[,3]
      #Mut_prob$max=Mutmax[,3]
      Mut_prob$maior=Mut00[,3]
      
p2 <-  ggplot(Mut_prob, aes(x=V2,y=V3,group=Values,color=Values)) + 
        theme_bw()+
        geom_line(aes(x=V2,y=maior),size=3.5,color="green")+
        scale_x_continuous(limits = c( 0,3.5))+
        labs(title="b",x=expression("Propagule Phenotype Distance" (d[0])), y=" ")+
        stat_function(fun=function (x) exp(-(x^2/(2*1^2))), geom="line", aes(colour="function"),size=1.0,color="black")+
        geom_line(aes(x =V2,y=V3), size=2.0 )+ 
         scale_color_manual(values = c("green","red","blue","purple","gold","darkgrey"),
         labels = c("0.0",expression("10"^"-6"),expression("10"^"-3"),expression("10"^"-2"),expression("10"^"-1"),"max"))+
      guides(colour = guide_legend(nrow = 1))+
      theme(legend.direction = "horizontal", 
              legend.position = "top",
              legend.box = "horizontal",
              legend.text=element_text(size=13),
              legend.title = element_blank(),
              plot.title=element_text(size=20,hjust=0.1),
              legend.margin=margin(t = 0, unit='cm'),
              axis.title.x=element_text(size=16),
              axis.text = element_text(size=14))+
  scale_size_manual(values = c(1, 1, 1,2, 2, 1))
p2  
#Propagule Size ####

 setwd("C:/Users/raque/Downloads/coisas da sof/Scripts figuras artigo/Figure 4/Propagule Size effect")
      
      Sobre1=read.table("contagem_probabilidade_10.txt")
      Sobre2=read.table("contagem_probabilidade_50.txt")
      Sobre6=read.table("contagem_probabilidade_100.txt")
      Sobre13=read.table("contagem_probabilidade_200.txt")
      Sobret1=read.table("contagem_probabilidade1.txt")
      Sobremax=read.table("contagem_probabilidade_max2.txt")

Prop_prob=rbind(Sobret1, Sobre1,Sobre6,Sobre13,Sobremax)
Prop_prob$values=c(rep("1",50),rep("10",50),rep("100",50),rep("200",50),rep("max",50))
      
      
  
  p3 <- ggplot(Prop_prob, aes(x=V2,y=V3,group=values,color=values)) +
        theme_bw()+
        stat_function(fun=function (x) exp(-(x^2/(2*1^2))), geom="line", aes(colour="function"),size=1.0,color="black")+
        geom_line(aes(x =V2,y=V3), size=2.0 )+ 
        scale_x_continuous(limits = c( 0,3.5))+
        labs(title="c",x=expression("Propagule Phenotype Distance" (d[0])),y=" ")+
        scale_color_manual(values = c("green","blue","red","gold","darkgrey"))+
        guides(fill=guide_legend(ncol=3,nrow=2))+
        theme(legend.text=element_text(size=14),
              legend.title = element_blank(),
              plot.title=element_text(size=20,hjust=0.1),
              legend.direction = "horizontal", 
              legend.margin=margin(),
              legend.position = "top",
              legend.box = "horizontal",
              axis.title.x=element_text(face="italic",size=16),
              axis.text = element_text(size=14))
   # guides(guide_legend(ncol=3,nrow=2))
    p3
      
grid.arrange(p1, p2, p3, ncol=3,nrow=1)



