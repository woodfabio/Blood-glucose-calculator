
## BIOQUIMICA II
## Pratica: Determinacao da Glicemia

## Segundo a lei de Lambert-Beer, quanto maior a concentracao de uma substancia em uma
## solucao, maior sera a quantidade de luz abosrvida por ela (absorbancia).
## Assim, podemos estimar a concentracao de uma substancia em uma amostra a partir da sua
## absorbancia.

## Para isso, precisamos primeiro estimar a aborbancia tipica da substancia analisada,
## verificando a sua absorbancia em varias concentracoes para podermos estimar uma
## CURVA PADRAO. Neste caso, a substancia analisada sera a glicose em plasma sanguineo de
## indivuos em estado alimentado e em jejum.

## funcoes:
# funcao qgc (calcular quantidade de glicose em 100ml):
qgc <- function(a,b,c,d) {
  # a = absorbancia
  # b = fcm
  # c = diluicao do tubo (em relacao a amostra)
  # d = vol. do tubo (mL)
  q <- a*b*c
  qc <- (q*100)/d
  return(qc)
}

## argumentos
# tabela com valores para calcular a curva padrao:
tcp <- matrix(NA,5,4)
colnames(tcp) <- c("Quant.(µL)","Abs1","Abs2","Média")
tcp[,1] <- c(0, 0.001, 0.002, 0.003, 0.004)

# absorbancia de cada tubo:
tcp[1,2] <- 0.01    # valor "branco"
tcp[1,3] <- 0.02    # valor "branco"  
tcp[2,2] <- 0.110
tcp[2,3] <- 0.105
tcp[3,2] <- 0.195
tcp[3,3] <- 0.2
tcp[4,2] <- 0.3
tcp[4,3] <- 0.31
tcp[5,2] <- 0.4
tcp[5,3] <- 0.39

# calcular fatores de calibracao (fc), medias (descontando media dos tubos "brancos")
# e fator de calibracao medio (fcm):
fc <- rep(NA, nrow(tcp)-1)

for (i in 1:nrow(tcp)) {
  if (i==1) {
    tcp[i,ncol(tcp)] <- mean(c(tcp[i,2:(ncol(tcp)-1)]))
  } else {
    tcp[i,ncol(tcp)] <- mean(c(sapply(tcp[i,2:(ncol(tcp)-1)], function(x) x - tcp[1,ncol(tcp)])))
    fc[i-1] <- tcp[i,1]/tcp[i,ncol(tcp)]
  }
}

fcm <- mean(fc)

## Agora que ja temos o valor de fcm, podemos utiliza-lo para calcular a quantidade de glicose
## nas amostras:

# tabela com amostras de individuo em estado alimentado e em jejum:
ta <- matrix(NA,2,2)
rownames(ta) <- c("Alim.","Jejum")
colnames(ta) <- c("Abs1","Abs2")

#absorbancia de cada tubo (0.02mL):
ta[1,1] <- 0.29
ta[1,2] <- 0.28
ta[2,1] <- 0.17
ta[2,2] <- 0.18

#absorbancia media de cada tubo:
alm <- mean(c(ta[1,]))-tcp[1,ncol(tcp)] # alimentado
jem <- mean(c((ta[2,]-tcp[1,ncol(tcp)]))) # jejum

## calcular quantidade de glicose em cada amostra (alimentado e jejum):
ga <- qgc(alm, fcm, 10, 0.02)
gj <- qgc(jem, fcm, 10, 0.02)
