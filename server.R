# assign(dados)

shinyServer(function(input, output, session) {
  
  
  #Saída caso a aba selecionada seja a de Seguros de Vida
  output$infoExp <- renderTable(width='90%', digits = 5,{
    inFile <- input$exp
    
    if (is.null(inFile))
      return(NULL)
    
    
    #read.csv(inFile$datapath, header = input$header)
    expRPPS_val <<- read.csv(inFile$datapath)[,2:4]
    expRPPS_val$exp<-expRPPS_val$mortes/expRPPS_val$expostos
    expRPPS <<- expRPPS_val[,c(1, 4)]
    
    #aux[,2] <- format(aux[,2], digits = 10)
    expRPPS_val
    
    
  })
  output$expTabuas <- renderTable(width='90%', digits = 5, {
    #inFile <- input$exp
    
    # if (is.null(expRPPS))
    #   return(NULL)
    if(!exists('expRPPS'))
       return(NULL)
    
    qx <- tabSelect2(input$tab)
    #read.csv(inFile$datapath, header = input$header)
    #expRPPS <<- read.csv2(inFile$datapath, header = T)
    
    aux <- subset(expRPPS, expRPPS$x>=input$rangeIdades[1] & expRPPS$x<=input$rangeIdades[2])
    
    aux <- merge(x = aux, y = qx, by = "x", all.x = TRUE)
    #aux[,2] <- format(aux[,2], digits = 10)
    print('3')
    aux
    
    
  })
  
  
  output$reportKS2 <- renderTable(width='90%', digits = 5, {
    #inFile <- input$exp
    # 
    # if (is.null(expRPPS))
    #   return(NULL)
    if(!exists('expRPPS'))
      return(NULL)
    
    n_tabua <- character()
    desvio <- numeric()
    p_value <- numeric()
    p_value_chisq <- numeric()
    mape <- numeric()
    
    for (n in names(tabuas)){
      if (n=='x')
        next
      aux <- merge(x = tabSelect2(n), y = expRPPS, by = "x", all = F)
      aux2<-ks.test(aux[,2], aux[,3])
      
      
      auxChisq<<-merge(x = tabSelect2(n), y = expRPPS_val[,c('x', 'mortes')], by = "x", all = F)
      
      #Linhas para teste
      #auxChisq<-merge(x = tabSelect2("AT.2000.MALE"), y = expRPPS_val[,c('x', 'mortes')], by = "x", all = F)
      #auxChisq<-merge(x = tabSelect2("GAM.71.MALE"), y = expRPPS_val[,c('x', 'mortes')], by = "x", all = F)
      
      
      auxChisq[is.na(auxChisq)] <- 0  
      #auxChisq<<-subset(auxChisq, !is.na(auxChisq[,2]))
      aux3<-chisq.test(x=auxChisq$mortes, p=auxChisq[,2], rescale.p=T)
      
      n_tabua<-c(n_tabua, n)
      desvio <- c(desvio, aux2$statistic)
      p_value <- c(p_value, aux2$p.value)
      p_value_chisq <- c(p_value_chisq, aux3$p.value)
      mape <- c(mape, MAPE(aux[,2], aux[,3]))
      
    }
    df <- data.frame(tabua=n_tabua, desvio=desvio, p_valor_KS=p_value, p_valor_Chisq=p_value_chisq, MAPE=mape)
    return(df[order(df$desvio),])
    
    
  })
  
  
  output$reportAderencia <- renderPrint({
    
    #Listar os hipóteses dos métodos utilizados
    #3 casos, 
    # -- KS Aceita, Chisq não aceita: Aponta a melhor tábua ks e diz que chisq não teve aceitação
    # -- KS e Chisq Aceitas: Aponta a melhor tábua pelo critério KS e aponta a melhor tábua pelo combinado
    # -- KS e Chisq Não aceitas: Aponta a tábua com menor desvio mas deixa claro que nenhuma foi aceita no teste
    
    
    n_tabua <- character()
    desvio <- numeric()
    p_value_ks <- numeric()
    p_value_chisq <- numeric()
    
    
    
    aux <- merge(x = tabSelect2(input$tab), y = expRPPS, by = "x", all = F)
    aux2<-ks.test(aux[,2], aux[,3])
      
    auxChisq<-merge(x = tabSelect2(input$tab), y = expRPPS_val[,c('x', 'mortes')], by = "x", all = F)
    auxChisq[is.na(auxChisq)] <- 0  
    aux3<-chisq.test(x=auxChisq$mortes, p=auxChisq[,2], rescale.p=T)
    dx_esperado <- sum(aux3$expected)
    dx_observado <- sum(expRPPS_val$mortes)
    #aux3<-chisq.test(aux[,2], aux[,3])
    
    desvio <- aux2$statistic
    p_value_ks <- aux2$p.value
    p_value_chisq <- aux3$p.value
    

    #, '\n'ifelse()
    
    
    #Incluir informaç~eos resumidas sobre o conjunto de dados. Como total de mortes na base x total de mortes segundo a tábua. Total de participantes
    
    cat('-Tábua:', input$tab, 
        # '\nDx Esperado: ', dx_esperado,
        # '\nDx Observado: ', dx_observado,
        '\n\n\nTeste Kolmorogov-Smirnov: \n -H0: P-valor >= α A tábua é aderente ao comportamento da população \n -H1: P-valor < α A tábua não é aderente ao comportamento da população \n',
        'P-valor observado: ', p_value_ks,
        '\nEstatística do Teste: ', desvio,
        ifelse(p_value_ks>input$alfa, paste0('A um nível ', input$alfa, ' de significância não se rejeita H0.'),
               paste0('A um nível ', input$alfa, ' de significância rejeita-se H0.')),
        '\n\n\nTeste Qui Quadrado: \n -H0: P-valor >= α Há Aderência \n -H1: P-valor < α  Não há aderência \n',
        'P-valor observado:', p_value_chisq,
        ifelse(p_value_ks<=input$alfa, paste0('\nA um nível ', input$alfa, ' de significância não se rejeita H0.'),
               paste0('A um nível ', input$alfa, ' de significância rejeita-se H0.')))
  })
  
  #return(data.frame(tabua=n_tabua, desvio=desvio, p_valor=p_value))
  
  output$plot1 <- renderPlot({
    qx <- tabSelect2(input$tab)
    expRPPS <- read.csv2(inFile$datapath, header = T)
    p_gra0 <- merge(x = expRPPS, y = qx, by = "x", all = F)
    p_gra0 <- p_gra0[!is.na(p_gra0[,2]) & !is.na(p_gra0[,3]),]
    
    
    ggplot(p_gra0) +
      geom_line(aes(x = x, y = tab)) +
      geom_line(aes(x = x, y = p_gra0[, 2]), color="red") +
      #theme(legend.position = "none") +
      labs(title="VPA x VP", x='Tempo', y='$')
    
  })
  
  
  
  output$plotTabuas <- renderPlot({
    if(!exists('expRPPS'))
      return(NULL)
    # inFile <- input$exp
    # if (is.null(inFile))
    #if (!exists("expRPPS"))
    #  return(NULL)
    qx <- tabSelect2(input$tab)
    # expRPPS <- read.csv2(inFile$datapath, header = T)
    p_gra0 <- merge(x = expRPPS, y = qx, by = "x", all = F)
    names(p_gra0)[c(2, 3)]<-c('Observada' ,'Tabua')
    
    # ggplot(p_gra0) +
    #   geom_line(aes(x = x, y = log(p_gra0[,2])),size=1) +
    #   geom_line(aes(x = x, y = log(p_gra0[,3])), color="red",size=1) +
    #   theme(legend.position = "top") +
    #   labs(title="qx observado x tábua (log)", x='Idade', y='qx')
    # 
    # 
    # 
    
    
    ggplot(p_gra0) +
      geom_line(data=p_gra0,aes(y=log(Observada),x= x,colour="Observada"),size=1 )+
      geom_line(data=p_gra0,aes(y=log(Tabua),x= x,colour="Tabua"),size=1) +
      scale_color_manual(name = "qx", values = c("Observada" = "darkblue", "Tabua" = "red"))+
      #theme(legend.position = "top") +
      labs(title="qx observado x tábua (log)", x='Idade', y='qx')
    
    
  })
  
  output$plotTabuasDif <- renderPlot({
    #inFile <- input$exp
    # 
    # if (is.null(inFile))
    #   return(NULL)
    if(!exists('expRPPS'))
      return(NULL)
    qx <- tabSelect2(input$tab)
    #read.csv(inFile$datapath, header = input$header)
    #expRPPS <- read.csv2(inFile$datapath, header = T)
    p_gra0 <- merge(x = expRPPS, y = qx, by = "x", all = F)
    
    
    ggplot(p_gra0) +
      geom_point(aes(x = x, y =( (p_gra0[, 2]/p_gra0[, 3])-1)),size=1) +
      theme(legend.position = "none") +
      labs(title="Diferença entre o qx observado e a tábua selecionada", x='Idade', y='dif')
  })
  
  # output$event <- renderPrint({
  #   d <- event_data("plotly_hover")
  #   if (is.null(d)) "Passe o mouse sobre um ponto!" else d
  # })
  
})
