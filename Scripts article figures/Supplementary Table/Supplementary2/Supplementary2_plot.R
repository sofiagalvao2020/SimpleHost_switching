library(ggplot2)
library(gridExtra)
library(viridis)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
setwd("C:/Users/sofia/Documents/MEGAsync/MEGAsync/modelos/pergunta/4/5/testes_em_u_b_Y/Supplementary Table/Supplementary2")
obj1=read.table("contagem_probabilidade1.txt",header=FALSE)
obj2=read.table("contagem_probabilidade2.txt",header=FALSE)
obj3=read.table("contagem_probabilidade.txt",header=FALSE)
obj12=obj1
obj12$V5=(obj1$V5+obj2$V5)/2
obj=obj12
#obj=rbind(obj3,obj12)
obj$b=signif(obj$V3,3)
obj$V4=signif(obj$V4,4)
obj$u=signif(log(obj$V4,10),5)
unique(obj$u)
obj3$u=obj3$V4
obj3$u[obj3$V4==0]=-8
obj3$u[obj3$V4==0.5]=0
obj3$b=signif(obj3$V3,3)
obj=rbind(obj,obj3)
muta=unique(obj$u)
us=unique(signif(obj$V4,3))
us
yv=c(1e-7,1e-6,1e-5,1e-4,1e-3,1e-2,1e-1)
log(yv)
p=ggplot(obj, aes(b, u)) +
  geom_raster(aes(fill = V5)) +scale_fill_gradientn(colours = viridis(10))+
  #legendaI+facet_wrap(~tem)
   ggtitle(" ")+
  labs(x ="Birth rate (b)", y = expression("Novelty rate ("*mu*")"),
       fill=stringr::str_wrap("Prob. of Establishment",10))
p
p=p+scale_y_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,0)                   ,
                                 labels=c(0,0.0000001,0.000001,0.00001,0.0001,0.001,0.01,0.1,0.5))

p
####
#propagulo N=b#####
setwd("C:/Users/sofia/Documents/MEGAsync/MEGAsync/modelos/pergunta/4/5/testes_em_u_b_Y/Supplementary Table/Supplementary2/N0=b")
obj1=read.table("contagem_probabilidade1.txt",header=FALSE)
#obj2=read.table("contagem_probabilidade2.txt",header=FALSE)
obj3=read.table("contagem_probabilidade.txt",header=FALSE)

#obj12=obj1
#obj12$V5=(obj1$V5+obj2$V5)/2
#obj=obj12
#obj=rbind(obj3,obj12)
obj=obj1
obj$b=signif(log(obj$V3,2),3)
obj$V4=signif(obj$V4,4)
obj$u=signif(log(obj$V4,10),5)
unique(obj$u)
unique(obj$b)
obj3$u=obj3$V4
obj3$u[obj3$V4==0]=-8
obj3$u[obj3$V4==0.5]=0
obj3$b=signif(log(obj3$V3,2),3)
obj=rbind(obj,obj3)
pp=ggplot(obj, aes(b, u)) +
  geom_raster(aes(fill = V5)) +scale_fill_gradientn(colours = viridis(10))+
  #legendaI+facet_wrap(~tem)
  ggtitle(" ")+
  labs( #x =expression("Birth rate (b)=Propagule size ("*N[o]*")")
    x ="Reproduction rate (b)"    
    , y = expression("Novelty rate ("*mu*")"),
       fill=stringr::str_wrap("Prob. of Establishment",10))
bs=unique(obj$b)
bsr=2^bs
pp
pp=pp+scale_y_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,0)                   ,
                       labels=c(0,0.0000001,0.000001,0.00001,0.0001,0.001,0.01,0.1,0.5))
pp=pp+scale_x_continuous(breaks=bs,
                       labels=bsr)

pp

#propagulo N=1#####
setwd("C:/Users/sofia/Documents/MEGAsync/MEGAsync/modelos/pergunta/4/5/testes_em_u_b_Y/Supplementary Table/Supplementary2/N0=b/N=1")
obj1=read.table("contagem_probabilidade1.txt",header=FALSE)
#obj2=read.table("contagem_probabilidade2.txt",header=FALSE)
obj3=read.table("contagem_probabilidadev.txt",header=FALSE)

obj=obj1
obj$b=signif(log(obj$V3,2),3)
obj$V4=signif(obj$V4,4)
obj$u=signif(log(obj$V4,10),5)
unique(obj$u)
unique(obj$b)
obj3$u=obj3$V4
obj3$u[obj3$V4==0]=-8
obj3$u[obj3$V4==0.5]=0
obj3$b=signif(log(obj3$V3,2),3)
obj=rbind(obj,obj3)




bs=unique(obj$b)
bsr=2^bs
# obj3$u=obj3$V4
# obj3$u[obj3$V4==0]=-8
# obj3$u[obj3$V4==0.5]=0
# obj3$b=signif(obj3$V3,3)
# obj=rbind(obj,obj3)
p=ggplot(obj, aes(b, u)) +
  geom_raster(aes(fill = V5)) +scale_fill_gradientn(colours = viridis(10), limits=c(0,1))+
  #legendaI+facet_wrap(~tem)
  ggtitle(" ")+
  labs(x ="Reproduction rate (b)", y = expression("Novelty rate ("*mu*")"),
       fill=stringr::str_wrap("Prob. of Establishment",10))
p
  p=p+scale_y_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,0)      ,
                       labels=c(0,0.0000001,0.000001,0.00001,0.0001,0.001,0.01,0.1,0.5))
p=p+scale_x_continuous(breaks=bs, labels=bsr)
p
pp
## juntando os 2
legenda <- get_legend(pp)
p1 <- p + theme(legend.position="none")
pn= pp+ theme(legend.position="none")
p3=grid.arrange(p1,pn,legenda, ncol=3, nrow=1, widths=c(1,1, 0.5))

#nova tentativa
p1 <- p + theme(legend.position="none")+ggtitle(expression(N[0]==1))
pn= pp+ theme(legend.position="none")+ggtitle(expression(N[0]==b))
p3=grid.arrange(p1,pn,legenda, ncol=3, nrow=1, widths=c(1,1, 0.5))

grid.arrange(p1,pn,legenda, ncol=3, nrow=1, widths=c(1,1, 0.5))
pdf("Diagrama_fase.pdf",width = 9,height=3.5)
#grid.arrange(ppbI2,ppbT2,legend, ncol=3, nrow=1, widths=c(2.3,2.3, 0.8))
grid.arrange(p1,pn,legenda, ncol=3, nrow=1, widths=c(1,1, 0.5))
dev.off()
