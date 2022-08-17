# Verifica Bibliotecas
if (!(is.element("pacman", installed.packages()[,1]))) {
    install.packages("pacman")
}

#pacman::p_load(plotly, reshape2, shiny, shinydashboard, stats)
#packrat::.snapshotImpl(".", snapshot.sources = FALSE) # cria arquivo packrat com todas dependências 

library(shiny)
library(shinydashboard)
library(stats)
library(ggplot2)

# Bibliotecas
#library(plotly)
#library(reshape2)


# Tabuas de Vida
#dados <- read.table('data/tabuas_de_vida.txt', h=T)
tabuas <- read.csv2("~/TCC/aderencia-tabuas/data/tabuas.csv")

#expRPPS<-tabuas[,c('x', 'AT.2000.MALE')]
attach(tabuas)




#Função de seleção de tábua de vida, usa os inputs como parametros e retorna a tábua desejada
tabSelect <- function(tab){ # tab=input$tab e sex=input$sex
    operador<-tab==colnames(tabuas[,])
    return(tabuas[,which(operador)])
}

tabSelect2 <- function(tab){ # tab=input$tab e sex=input$sex
  operador<-tab==colnames(tabuas[,]) | 'x'==colnames(tabuas[,])
  return(tabuas[, which(operador)])
}


# Cria um conjunto de dados com o prêmio por idade
p_gra <- function(i, idade, b, qx){
    p <- c()
    id <- c()
    for(j in (0:length(dados$Idade))){
        id[j] <- j
        p[j] <- (SV_Vit(i, j, 0, b, qx))$Ax
    }
    return(list(premio=p, idade=id))
}


# Cria duas listas com VPA e VP financeiro para comparativo
fa_gra <- function(i, idade, fim, b, qx){ 
  vpa <- c()
  vp <- c()
  n <- c()
  for(t in (1:fim)){
    n[t] <- t
    vp[t] <- b*(1/(1+i)^t)
    vpa[t] <- (Dotal_Puro(i, idade, t, b, qx))$Ax
  }
  return(list(tempo=n, financeiro=vp, atuarial=vpa))
}


