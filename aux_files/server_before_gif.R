
server <- function(input, output, session) {
  
  #cria a tabela de início
  output$table <- render_gt(folha_generator(folha_aux), align = 'left')
# print(folha)

  #adicionando jogadores, só para isso
  observeEvent(c(input$addplayerbutton),{
    
    if(input$jogador!=""){

    #adicionando jogador
    players <<- c(players, input$jogador)
    # output$jogadores_table <- players
    
    #atualizando a tabela

    
    
    if(length(players) >= 2){

      folha_aux <<- folha_aux |>
        add_column(a = "")
      
      print(folha_aux)
      
      colnames(folha_aux) <<- c("Lines", players)
      
    }else{
      colnames(folha_aux) <<- c("Lines", players)
      }
  

    
    print(folha_aux)
    #plotando a tabela
    output$table <<- render_gt(folha_generator(folha_aux), align = 'left')
    
    # output$table <- renderTable(folha, rownames = T, bordered = T, align = 'c')
    
    updateTextInput(session, inputId = "jogador", 
                    value = c(""))
    # print(folha)
    }
    

    
  })
  
#removendo a coluna de adicionar jogares
  observeEvent(input$readytoplay,{
    
    # print(folha)
    
    if(is.null(players)){
      players <<- c("Jogador 1")
      # folha <<- matrix(nrow = 12, ncol = length(players), data = "0")
      # row.names(folha) <- c(1:6,"S","F","Q","G","G*")
      # colnames(folha) <<- players

    }
    # print(players)
    output$table <<- render_gt(folha_generator(folha_aux), align = 'left')
    
    # print(folha)
    
    removeUI(selector = "div:has(> #jogador)")
    removeUI(selector = "div:has(> #addplayerbutton)")
    removeUI(selector = "div:has(> #readytoplay)")
    
    #adicionando os jogadores que irão jogar com o checkbox
    # insertUI(selector = '#placeholder', where = "beforeBegin",
    #          ui =  textInput(inputId = "tittle_jogando", 
    #                          label= h4(paste0("Bora mexer o caneco"))
    #          )
    # )
    
      insertUI(selector = '#placeholder', where = "beforeEnd", multiple = T,
                      ui =  radioButtons("jogando", label = h4("Jogando:"),
                                               choices = players, inline = T)
      )

      #adicionando o botão de jogar dados
      insertUI(selector = '#placeholder', where = "beforeEnd", multiple = T,
                ui = fluidRow(
                  column(6,
                 inline(actionButton("jogar_dados", label = "Jogar dados!"))
                  )
                )
      )
    

    
  })
  ################################## Começo partida  ##################################
  
  #começo da partida será assim
  observeEvent(input$jogar_dados, priority = 2,{
    

    if(n_jogada_dado == 0){
      
      dados_rol <<- (fixar(0)) #%>% as.character()
      


      for(i in 1:5){
        if(dados_rol[i] == 1){
         lista_choicenames[[i]] <- icon("dice-one",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 2){
          lista_choicenames[[i]] <- icon("dice-two",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 3){
          lista_choicenames[[i]] <- icon("dice-three",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 4){
          lista_choicenames[[i]] <- icon("dice-four",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 5){
          lista_choicenames[[i]] <- icon("dice-five",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 6){
          lista_choicenames[[i]] <- icon("dice-six",lib = "font-awesome", "fa-3x")
        }

      }

    
   print(dados_rol)
      # a <- tags$div(HTML(img(src = "./www/Alea_1.png")))
      if(aux_inicio_server == 0){
      insertUI(selector = '#placeholder', where = "beforeEnd", multiple = T,
               ui = fluidRow( 
                 column(9,
                 checkboxGroupInput("ficamdados", label = "Selecione os que ficarão",
                                        choiceNames =lista_choicenames,

                                    choiceValues = 1:5,
                                        selected = NULL, inline = T))
                 

                )
      )
        
        hideElement(id = "fim_jogada")
        
      }
      if(aux_inicio_server != 0){
        updateCheckboxGroupInput(session, "ficamdados", label = "Selecione os que ficarão",
                                 choiceNames =lista_choicenames,
                                 
                                 choiceValues = 1:5,
                                 inline = T, selected = NULL)
        # updateTextInput(session, "valor", value = 0)
        
      }


      

    }
    if(n_jogada_dado == 1){
      aux_inicio_server <<- 1
      
      dados_rol <<- fixar(escolhas) #%>% as.character() 
      
      for(i in 1:5){
        if(dados_rol[i] == 1){
          lista_choicenames[[i]] <- icon("dice-one",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 2){
          lista_choicenames[[i]] <- icon("dice-two",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 3){
          lista_choicenames[[i]] <- icon("dice-three",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 4){
          lista_choicenames[[i]] <- icon("dice-four",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 5){
          lista_choicenames[[i]] <- icon("dice-five",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 6){
          lista_choicenames[[i]] <- icon("dice-six",lib = "font-awesome", "fa-3x")
        }
        
      }
      
      updateCheckboxGroupInput(session, "ficamdados", label = "Selecione os que ficarão",
                               choiceNames =lista_choicenames,
                               
                               choiceValues = 1:5,
                               inline = T, selected = escolhas)
      

      show(id = "fim_jogada")
      

      
      
    }
    if(n_jogada_dado >= 2){
      dados_rol <<- fixar(escolhas) #%>% as.character() 
      
      for(i in 1:5){
        if(dados_rol[i] == 1){
          lista_choicenames[[i]] <- icon("dice-one",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 2){
          lista_choicenames[[i]] <- icon("dice-two",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 3){
          lista_choicenames[[i]] <- icon("dice-three",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 4){
          lista_choicenames[[i]] <- icon("dice-four",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 5){
          lista_choicenames[[i]] <- icon("dice-five",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 6){
          lista_choicenames[[i]] <- icon("dice-six",lib = "font-awesome", "fa-3x")
        }
        
      }
      
      updateCheckboxGroupInput(session, "ficamdados", label = "Selecione os que ficarão",
                               choiceNames =lista_choicenames,
                               
                               choiceValues = 1:5,
                               inline = T, selected = escolhas)
      

      

    }

    # print(n_jogada_dado)
    n_jogada_dado <<- n_jogada_dado + 1
    # print(escolhas)
    
    ############################# Atualizando valor  ###############################33
    #atualizando o valor que vai pra tabela
    
    # f<<- input$linhas
    # print(input$linhas)
    
    
    #Numeros
    if(aux_inicio_jogo == 1){
      if(is.na(as.numeric(input$linhas)) == F){
        x <- length(which(dados_rol==input$linhas))
        updateTextInput(session, "valor", value = as.numeric(input$linhas)*x)
      }

    #uni é auxiliar de ula, quadra e general
    uni <<- unique(dados_rol)
    
    #general
    if((input$linhas == "G") | (input$linhas == "G*")){
      if(length(uni) == 1){
        updateTextInput(session, "valor", value = 50)
        
        if(n_jogada_dado == 0){
          updateTextInput(session, "valor", value = 1000)
        }
      }else(
      updateTextInput(session, "valor", value = 0)
    )
    }
    
    #Fula e quadra
    if(length(uni) == 2){
    n1 <<- length(dados_rol[dados_rol==uni[1]])
    n2 <<- length(dados_rol[dados_rol==uni[2]])

    if(input$linhas == "F"){
      if( ( (n1 == 3) & (n2 ==2) ) | ( (n1 == 2) & (n2 == 3) ) ) {
        updateTextInput(session, "valor", value = 30)
        
        #de mao
        if(n_jogada_dado == 0){
          updateTextInput(session, "valor", value = 35)
        }
      }else(
        updateTextInput(session, "valor", value = 0)
      )
    }
    
    if(input$linhas == "Q"){
      if( ( (n1 == 4) & (n2 ==1) ) | ( (n1 == 1) & (n2 == 4) ) ) {
        updateTextInput(session, "valor", value = 40)
        
        if(n_jogada_dado == 0){
          updateTextInput(session, "valor", value = 45)
        }
      }else(
        updateTextInput(session, "valor", value = 0)
      )
      }
    }
    
    #Seguida
    if(input$linhas == "S"){
      if( all.equal.character(sort(dados_rol),c(1,2,3,4,5)) == T| 
          all.equal.character(sort(dados_rol),c(2,3,4,5,6)) == T|
          all.equal.character(sort(dados_rol),c(1,3,4,5,6)) == T){
        
        updateTextInput(session, "valor", value = 20)
        
        if(n_jogada_dado == 0){
          updateTextInput(session, "valor", value = 26)
        }
      }else{
        updateTextInput(session, "valor", value = 0)
      }
    }
    
  }
    # print(sort(dados_rol))
      
    ####################################################
    

  })
  
  #atualizando o checkboxgroup dos dados que ficava selecionado
  
  observeEvent(c(input$ficamdados, input$jogar_dados),{
    escolhas <<- c(unique(input$ficamdados))

    
  })
  
  #selecionando nenhum dado a cada fim de jogada
  
  observeEvent(input$fim_jogada, priority = 2,{
    escolhas <<- c()
    
    
    #colocando os redo 
    
    for(i in 1:5){
      lista_repeat[[i]] <<- icon("play", lib = "font-awesome", "fa-3x")
    }
    
    
      updateCheckboxGroupInput(session, "ficamdados", label = "Selecione os que ficarão",
                               choiceNames = lista_repeat,
                               choiceValues = 1:5,
                               inline = T, selected = NULL)
      dados_rol <<- c()
      n_jogada_dado <<- 0
      aux_inicio_server <<- 1
  })

######################4 Botões pós jogada #######################
observeEvent(input$jogar_dados, priority = 1, {
  if(aux_inicio_server == 0){
    aux_inicio_jogo <<- 1
    
    insertUI(selector = '#daquiprabaixotabela', where = "afterEnd", multiple = T,
           ui = fluidRow(
             column(5,

  
             inline(radioButtons("linhas", label = "E aí, vai marcar no quê?",
                                   choices = nome_linhas,
                                   selected = NULL))
             ),
             column(1,
             inline(textInput("valor", label = "E quanto?",
                                 value = "")),
                   
                    inline(actionButton("adicionar_valor", label = "Adicionar a folha", width = "150px")),
             inline(actionButton("fim_jogada", label = "Finalizar jogada", width = "150px")),
             HTML(paste("<br></br>")),
             HTML(paste("<br></br>")),
                    actionButton("revanche", label = "Reiniciar partida", width = "150px")#,
                    # actionButton("restartapp","Começar partida com outros jogadores", width = "300px")
             
           )
           )
  )

  }


  })
  
  #LINHAS ATUALIZANDO VALOR
  observeEvent(input$linhas,{
    # print(input$linhas)
    
    if(is.na(as.numeric(input$linhas)) == F){
      x <<- length(which(dados_rol==input$linhas))
      updateTextInput(session, "valor", value = as.numeric(input$linhas)*x)
      # print(x)
      # print(as.numeric(input$linhas)*x)
    }
    
    # uni é o auxiliar  de unicos
    uni <<- unique(dados_rol)
    
    #general
    if((input$linhas == "G") | (input$linhas == "G*")){
      if(length(uni) == 1){
          updateTextInput(session, "valor", value = 50)
          if(is.null(escolhas)){
            updateTextInput(session, "valor", value = 1000)
          }
      }else(
          updateTextInput(session, "valor", value = 0)
        )
  }
    
    
    #Fula e quadra
    if(length(uni) == 2){
      n1 <<- length(dados_rol[dados_rol==uni[1]])
      n2 <<- length(dados_rol[dados_rol==uni[2]])
      
      if(input$linhas == "F"){
        if( ( (n1 == 3) & (n2 ==2) ) | ( (n1 == 2) & (n2 == 3) ) ) {
          updateTextInput(session, "valor", value = 30)
          if(is.null(escolhas)){
            updateTextInput(session, "valor", value = 35)
          }
        }else(
          updateTextInput(session, "valor", value = 0)
        )
      }
      
      if(input$linhas == "Q"){
        if( ( (n1 == 4) & (n2 ==1) ) | ( (n1 == 1) & (n2 == 4) ) ) {
          updateTextInput(session, "valor", value = 40)
          if(is.null(escolhas)){
            updateTextInput(session, "valor", value = 45)
          }
        }else(
          updateTextInput(session, "valor", value = 0)
        )
      }
    }
    
    
    #Seguida
    if(input$linhas == "S"){
      if( all.equal.character(sort(dados_rol),c(1,2,3,4,5)) == T| 
          all.equal.character(sort(dados_rol),c(2,3,4,5,6)) == T|
          all.equal.character(sort(dados_rol),c(1,3,4,5,6)) == T){
        
        updateTextInput(session, "valor", value = 20)
        
        if(is.null(escolhas)){
          updateTextInput(session, "valor", value = 26)
        }
      }else{
        updateTextInput(session, "valor", value = 0)
      }
    }
    

  })

  
  #aTUALIZAR TABELA COM O VALOR
observeEvent(input$adicionar_valor,{
  
  # aux_escolhas_linhas <<- c(aux_escolhas_linhas, input$linhas)
  # aux_adicionar_valor <<- c(aux_adicionar_valor, input$valor)
  
  # eval(parse(text = paste0("folha$","input$jogando","<<- input$valor")))
  folha_aux[(folha_aux$Lines == input$linhas), input$jogando] <<- input$valor 
 
  
  # if(length(aux_adicionar_valor) == 2){
# 
    # folha[aux_escolhas_linhas[1], input$jogando] <<- ""
    # folha[input$linhas, input$jogando] <<- input$valor
    # aux_adicionar_valor <<- c(input$valor)
    # aux_escolhas_linhas <<- c(input$linhas)

  # }
  
  # print(folha)
  # output$table <-  renderTable(folha, rownames = T, bordered = T, align = 'c')
  
  output$table <<- render_gt(folha_generator(folha_aux), align = 'left')
  
  
  #somando os valores
  aux_total_pts <<- folha_aux[1:11,input$jogando]
  tosum <<- aux_total_pts[! aux_total_pts %in% ""]
  
  folha_aux[(folha_aux$Lines == "Total"), input$jogando] <<- sum(as.numeric(tosum))
  
})


#Após fim da jogada, atualiza sozinho o player jogando
observeEvent(input$fim_jogada, priority = 1,{
  
  if(aux_players == length(players)-1){
    aux_players <<- 0
  }else{
  aux_players <<- aux_players + 1
  }
  
     
  updateRadioButtons(session, inputId = "jogando", 
                     selected = players[aux_players+1])

  updateTextInput(session, "valor", value = 0)
  
  # aux_adicionar_valor <<- c()
  # aux_escolhas_linhas <<- c()
  
})

#REVANCHE
observeEvent(input$revanche,{
  
  updateRadioButtons(session, inputId = "jogando", 
                     selected = players[1])
  
  # folha <<- matrix(nrow = 12, ncol = length(players), data = "") 
  # row.names(folha) <<- nome_linhas
  # colnames(folha) <<- players
  
  
  
  # output$table <- renderTable(folha, rownames = T, bordered = T, data = "")
  output$table <<- render_gt(folha_generator(folha_aux), align = 'left')
  
  
})

session$onSessionEnded(function(){
  stopApp()
})



}



