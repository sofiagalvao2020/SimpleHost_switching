Program MORT2
	IMPLICIT REAL*8 (A-H,O-Z) !as variávies iniciadas com "a,b,c,d,e,f,g,h" e "o,p,q,r,s,t,u,v,x,y,z" são reais e as demais são inteiras. 8 é a quantidade de casas decimais
	allocatable:: Mut(:,:), MutS(:,:), MutN(:,:)
	allocatable:: NSoma(:)
	OPEN(UNIT=5, FILE='teste_com_vetor.txt', STATUS='unknown')

	b=1.7			!birth rate
	k=1000			!capacity of host
	NG=100			!genome size
	u=0.0001 !mutation rate
!	o=0.5  		!mortality probability
	nt=500			!100 gerações
	Fenootimo=50    !colocando pressao do hospedeiro
	Y=10			!desvio padrão que define variedade fenotipica (=artigo), aqui, os fenotipos 15 e 45 tem chance de sobrevivencia reduzida pela metade qquando comparada ao do fenootimo
       ! nrepete=500
        nfenotipo_ini=40
	ninicial=200    !POpulacao inicial

        ALLOCATE (Mut(k, NG), MutS(k,NG), MutN(k, NG))
	Allocate (NSoma(NG+1))
IJ=0
ncont=0
DO WHILE(IJ.LT.NT)
							!TEMOS CINCO INDIVIDUOS IGUAIS (PRONTO)
		!OPEN(UNIT=7, FILE='formacao_genoma_sobrevivente2.txt', STATUS='unknown')
		!OPEN(UNIT=8, FILE='saida_descendentes.txt', STATUS='unknown')
		!OPEN(UNIT=9, FILE='valor_dos_fenotipos_soma.txt', STATUS='unknown')
				OPEN(UNIT=10, FILE='numero_de_ind_por_soma04_n0200.txt', STATUS='unknown')
              !  OPEN(UNIT=11, FILE='variedade_por_geracao4.txt', STATUS='unknown')
                !OPEN(UNIT=12, FILE='fenotipos_comparacao.txt', STATUS='unknown')
            !    OPEN (UNIT=13, FILE='contagem_probabilidade.txt', STATUS='unknown')
        !DO nsomain=1,nsomamax                                                  !funciona igual ao NSoma, com cada linha representando um fenotipo, ate o valo de 49
		!ncontsob=0
         ! DO nr=1,nrepete
        	Mut=0
       	 	ni=ninicial
            DO ip=1,ni
				nsomapar=0

                DO while (nsomapar.lt.nfenotipo_ini)  !� para criar 49 linhas
					call random_number (r)
					i=1+Int(r*NG)
					IF (Mut(ip,i).eq.0)then
						Mut(ip,i)=1
						nsomapar=nsomapar+1
         	        END IF
                ENDDO
				!PRINT*,'Mut1', 'Mut2', Mut(1,:), Mut(2,:)
                !PAUSE
			Enddo
		
		!Mortes (selecao)
            DO ij=1,nt  !Tempo
			!	Do while(ni.lt.k)
					! Salva variabilidade
				NSoma=0
				DO i=1,ni
					MS=sum(Mut(i,:))  !@SA
					NSoma(MS+1)=NSoma(MS+1)+1       !! Tinha um erro aqui @SA 
				ENDDO
				if(ni.eq.0) exit !sai do loop
				!Mortes(selecao)
				nsob=0
				MutS=0
				DO j=1,ni
					call random_number(r)
					dist=sum(Mut(j,:))-Fenootimo
					pm=exp(-dist**2/(2*Y**2))          !reconstru��o do pm, levandoa a uma nova presao do hspedeiro sobre a distribui��o dos fenotipos
					!Print*, "oi", pm
					!pause
					IF (r.lt.pm)then				!tem que ser .gt. nao .lt.porque o que nos interessa sao os sobreviventes
						nsob=nsob+1
						MutS(nsob,:)= Mut(j,:)		!na matriz MutS, a linha e o nsob, enquanto em Mut, a linha e k
					END IF
					!PRINT*, nsob+1, r, MutS(nsob,:)
					!Write(7, *) r, nsob+1, MutS(nsob,:)
				ENDDO   

					!PRONTO, TENHO 2 INDIVIDUOS IDENTICOS E COM GENOMA DE 50 LOCI
				!Reprodução
				MutN=0
				nquerem=NInt(nsob*b)
				IF (nquerem.le.k) then
					nasce=nquerem
				else
					nasce=k
				END IF
				DO i=1,nasce 				!do primeiro descendente ate o ultimo
					!sortear o pai
					call random_number(r)
					ipai=1+Int(r*nsob)				!a linha do pai é sorteada.
					DO j=1, NG						!locus de cada coluna
						call random_number (r)
						IF (r.lt.u) then 			!ocorre mutação
							MutN(i,j)=1-MutS(ipai,j)
						else						!não ocorre mutaçao
							MutN(i,j)=MutS(ipai,j)
						END IF
						!PRINT*, j, MutS(i,j), MutS(ipai,j)									!escreve a linha de sobrevivente, a mutação do filho e a do pai praquele j=locus
					!	Write(8,*) MutS(i,j), j, MutS(ipai,j)

					ENDDO 
		!enddo
				ENDDO 
				
                !Salvar a variabilidade e valores do fenotipo

				!nvari=0
				!DO ir=1, NG+1
				!	IF(NSoma(ir).ne.0) nvari=nvari+1
				!ENDDO
			!			Write(11,*) ij, nvari, ni
				DO i=1,NG+1
					Write(10,*) ij, i-1, NSoma(i), ni, (NInt(Fenootimo)-(i-1))/Y			!deve mostrar a frequencia de cada soma....i-1=numero do fenotipo
				ENDDO
				Mut=MutN
				ni=nasce


        ENDDO  !TEMPO
		
		CLOSE(10)
		ncont=ncont+1
		Print*, ncont, IJ
    ENDDO ! WHILE
		
	!	IF (ni.gt.0) ncontsob=ncontsob+1
	!Enddo
        !       	psob=Real(ncontsob)/Real(nrepete)
	!	Write(13,*) NInt(Fenootimo)-nsomapar,Real(NInt(Fenootimo)-nsomapar)/Y, psob

	!ENDDO 
   	end program MORT2
