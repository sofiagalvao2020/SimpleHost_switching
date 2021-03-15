Program MORT2
	IMPLICIT REAL*8 (A-H,O-Z) !as variávies iniciadas com "a,b,c,d,e,f,g,h" e "o,p,q,r,s,t,u,v,x,y,z" são reais e as demais são inteiras. 8 é a quantidade de casas decimais
	allocatable:: Mut(:,:), MutS(:,:), MutN(:,:)
	allocatable:: NSoma(:)
	!OPEN(UNIT=5, FILE='teste_com_vetor.txt', STATUS='unknown')

	bi=1		!birth rate
	bf=200
	deltab=2
	k=1000			!capacity of host
	NG=100			!genome size
	ui=0.0000001d0  		!mutation rate
	uf=0.5
	deltau=10
 	nt=1000			!100 gerações
	Fenootimo=50    !colocando pressao do hospedeiro
	Y=10			!desvio padrão que define variedade fenotipica (=artigo), aqui, os fenotipos 15 e 45 tem chance de sobrevivencia reduzida pela metade qquando comparada ao do fenootimo
    nrepete=700
   !     nsomamax=50
  nsomain=40
        ALLOCATE (Mut(k, NG), MutS(k,NG), MutN(k, NG))
	Allocate (NSoma(NG+1))

							!TEMOS CINCO INDIVIDUOS IGUAIS (PRONTO)
		!OPEN(UNIT=7, FILE='formacao_genoma_sobrevivente2.txt', STATUS='unknown')
		!OPEN(UNIT=8, FILE='saida_descendentes.txt', STATUS='unknown')
		!OPEN(UNIT=9, FILE='valor_dos_fenotipos_soma.txt', STATUS='unknown')
		!OPEN(UNIT=10, FILE='numero_de_ind_por_soma.txt', STATUS='unknown')
                !OPEN(UNIT=11, FILE='variedade_por_geracao4.txt', STATUS='unknown')
               ! OPEN(UNIT=12, FILE='fenotipos_comparacao.txt', STATUS='unknown')
                OPEN (UNIT=13, FILE='contagem_probabilidade.txt', STATUS='unknown')
                !OPEN (UNIT=15, FILE='ESTATISTICA.txt', STATUS='unknown')
        

		!Do1: DO nsomain=1,nsomamax                                                  !funciona igual ao NSoma, com cada linha representando um fenotipo, ate o valo de 49
		u=ui
		Dou: Do while(u.lt.uf)
		b=bi
		Dob: Do while (b.lt.bf)
		ncontsob=0
        	Dorepete: DO nr=1,nrepete
        	Mut=0
       	 	ni=Nint(b)
			
			do ind=1, ni
				nsomapar=0
				DO while (nsomapar.lt.nsomain)  !� para criar 49 linhas
				  call random_number (r)
				   i=1+Int(r*NG)
				   IF (Mut(ind,i).eq.0)then
					  Mut(ind,i)=1
					  nsomapar=nsomapar+1
				  END IF
				ENDDO
			enddo	
	!PRINT*,'nsomain, nsomapar, repete', nsomain, nsomapar, nrepete
        !PAUSE
		!Mortes (selecao)
	DO ij=1,nt
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
				IF (r.le.u) then 			!ocorre mutação
					MutN(i,j)=1-MutS(ipai,j)
				else						!não ocorre mutaçao
					MutN(i,j)=MutS(ipai,j)
				END IF
				!PRINT*, j, MutS(i,j), MutS(ipai,j)									!escreve a linha de sobrevivente, a mutação do filho e a do pai praquele j=locus
			!	Write(8,*) MutS(i,j), j, MutS(ipai,j)

			ENDDO
		ENDDO
		Mut=MutN
		ni=nasce

        ENDDO
                 !Write (15, *)  nr, REal(NInt(Fenootimo)-nsomapar)/Y , ncontsob

        IF (ni.gt.0) ncontsob=ncontsob+1
     !       Write (15, *)  REal(NInt(Fenootimo)-nsomapar)/Y ,nr, ncontsob,b
        ENDDO Dorepete
		psob=Real(ncontsob)/Real(nrepete)
		Write(13,*) NInt(Fenootimo)-nsomapar,REal(NInt(Fenootimo)-nsomapar)/Y,b, u, psob
	!	Write (14,*)  ncontsob, nrepete, psob,b*deltab
       ! ENDDO Do1
	   b=b*deltab
    Enddo Dob
	    u=u*deltau
		Enddo Dou
	
	Close(13)
	!Close(14)
	!Close(15)
   	end program MORT2
