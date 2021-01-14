#figura 2
#####graficos de fenot ancestral
library("ggplot2")
library("RColorBrewer")

#abline(v = y)

setwd("C:/Users/raque/Downloads/coisas da sof/Scripts figuras artigo/Figure 2")

#Plot Fig 2####
#jpeg("Fig2.jpeg",with=500,height=700)
par(mfcol=c(3, 1),mai = c(0.6, 0.1, 0.2, 0.1), oma = c(5, 5, 1, 5))

#setwd("~/modelos/pergunta/4/5/testes_teste3/teste_comparacao/teste1/teste1_extendido/o=0.5/y=20/Y=30/TESTE/GRAFICO/testes_em_u_b_Y/analise_tempo/0")
probabilidade4=read.table("numero_de_ind_por_soma0.txt")
distancia4=probabilidade4 [probabilidade4$V3!=0, ] #escreve no arquivo somente aqueles que possuem mais de um individuo no fenotipo
cor=colorRampPalette(c('gold','darkorange','darkorange1','darkorange2','red','firebrick1','darkred'))
cor2=cor(17)[as.numeric(cut(distancia4$V3, breaks = 17))] #quanto maior o valor, mais graduado ? a varia??o de cor 
plot1 <- plot(distancia4[ ,1], distancia4 [ ,5], col=cor2, pch=20,  xlab=" ", xlim=c(0,300),ylim=c(-1,1),ylab="",cex.lab=2,cex.axis=1.5,yaxt = "n",) #plot indica a dominancia do fenotipo na popula??o com o passar do tempo, inso sempre em direcao ao otimo  
axis(2, at = seq(-1.0, 1.0, by = 1),cex.axis=1.5)
par(new = TRUE)
plot(distancia4[,1],distancia4[,4],col="blue", type="l",lwd=3,xlim=c(0,300),ylim=c(0,1000), xaxt = "n", yaxt = "n",xlab=" ", ylab="",cex.axis=1.5)
axis(side = 4,cex.axis=1.5)
mtext("a",side=3,cex=1.4,line=1,adj=0.1)     


#migracao fenotipo 10-4 com pop size
#setwd("~/modelos/pergunta/4/5/testes_teste3/teste_comparacao/teste1/teste1_extendido/o=0.5/y=20/Y=30/TESTE/GRAFICO/testes_em_u_b_Y/analise_tempo/0.0001")
probabilidade4=read.table("numero_de_ind_por_soma04.txt")
distancia4=probabilidade4 [probabilidade4$V3!=0, ] #escreve no arquivo somente aqueles que possuem mais de um individuo no fenotipo
cor=colorRampPalette(c('gold','darkorange','darkorange1','darkorange2','red','firebrick1','darkred'))
cor2=cor(17)[as.numeric(cut(distancia4$V3, breaks = 17))] #quanto maior o valor, mais graduado ? a varia??o de cor 
plot1 <- plot(distancia4[ ,1], distancia4 [ ,5], col=cor2, pch=20, xlab=" ", xlim=c(0,300)
              ,ylim=c(-1,1),ylab=" ", cex.axis=1.5,yaxt = "n") #plot indica a dominancia do fenotipo na popula??o com o passar do tempo, inso sempre em direcao ao otimo  
axis(2, at = seq(-1.0, 1.0, by = 1),cex.axis=1.5)
abline(v=45, col="gold",lwd=2,lty=1)
abline(v=80,col="orange1",lwd=2,lty=1)
abline(v=120,col="darkorange1",lwd=2,lty=1)
abline(v=200,col="red",lwd=2,lty=1)
par(new = TRUE)
plot(distancia4[,1],distancia4[,4],col="blue", type="l",lwd=3,xlim=c(0,300),ylim=c(0,1000), xaxt = "n", yaxt = "n",xlab=" ", ylab="",cex.axis=1.5)
axis(side = 4,cex.axis=1.5)
mtext("Phenotype distance",side=2,cex=1.5,line=3)     
mtext("\u25ac",side=4,cex=1.5,line=3,srt=-180,col="blue",adj=-1)  
mtext("Population size",side=4,cex=1.5,line=3)
mtext("b",side=3,cex=1.4,line=1,adj=0.1)     


#setwd("~/modelos/pergunta/4/5/testes_teste3/teste_comparacao/teste1/teste1_extendido/o=0.5/y=20/Y=30/TESTE/GRAFICO/testes_em_u_b_Y/analise_tempo/0.01")
probabilidade4=read.table("numero_de_ind_por_soma02.txt")
distancia4=probabilidade4 [probabilidade4$V3!=0, ] #escreve no arquivo somente aqueles que possuem mais de um individuo no fenotipo
cor=colorRampPalette(c('gold','darkorange','darkorange1','darkorange2','red','firebrick1','darkred'))
cor2=cor(17)[as.numeric(cut(distancia4$V3, breaks = 17))] #quanto maior o valor, mais graduado ? a varia??o de cor 
plot1 <- plot(distancia4[ ,1], distancia4 [ ,5], col=cor2, pch=20, xlab="Generation",cex.lab=2, xlim=c(0,300),ylim=c(-1,1),ylab=" ",cex.axis=1.5,yaxt = "n") #plot indica a dominancia do fenotipo na popula??o com o passar do tempo, inso sempre em direcao ao otimo  
axis(2, at = seq(-1.0, 1.0, by = 1),cex.axis=1.5)
abline(v=10, col="gold",lwd=2,lty=1)
abline(v=15,col="orange1",lwd=2,lty=1)
abline(v=25,col="darkorange1",lwd=2,lty=1)
abline(v=35,col="red",lwd=2,lty=1)
abline(v=60,col="darkred",lwd=2,lty=1)
abline(v=500,col="red4",lwd=2,lty=1)
par(new = TRUE)
plot(distancia4[,1],distancia4[,4],col="blue", type="l",lwd=3,xlim=c(0,300),ylim=c(0,1000), xaxt = "n", yaxt = "n",xlab=" ", ylab="",cex.axis=1.5)
axis(side = 4,cex.axis=1.5)
mtext("c",side=3,cex=1.4,line=1,adj=0.1)     

