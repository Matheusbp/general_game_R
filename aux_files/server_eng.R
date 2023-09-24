
server <- function(input, output, session) {

  #create the initial table 
  output$table <- render_gt(folha_generator(folha_aux), align = 'left')

  #adding players
  observeEvent(c(input$addplayerbutton),{
    
    if(input$jogador!=""){

    #adding player
    players <<- c(players, input$jogador)

    #updating the table collumn player names

    if(length(players) >= 2){

      folha_aux <<- folha_aux |>
        add_column(a = "")
      
      colnames(folha_aux) <<- c("Lines", players)
      
    }else{ #enjambration to make it work
      colnames(folha_aux) <<- c("Lines", players)
      }
  

    
    #updating the table
    output$table <<- render_gt(folha_generator(folha_aux), align = 'left')
    
    
    #update the player's name textinput
    updateTextInput(session, inputId = "jogador", 
                    value = c(""))
    }
    

    
  })
  
  ###updating detail to be ready to play!
  observeEvent(input$readytoplay, priority = 2,{
    
    # print(folha)
    
    if(is.null(players)){
      players <<- c("Player")
    }
    # updating table again, to be sure Player will be there
    output$table <<- render_gt(folha_generator(folha_aux), align = 'left')
    

      insertUI(selector = '#placeholder', where = "beforeEnd", multiple = T,
                      ui =  radioButtons("jogando", label = h4("Player:"),
                                               choices = players, inline = T)
      )

      #adicionando o botão de jogar dados
      insertUI(selector = '#placeholder', where = "beforeEnd", multiple = F, 
               ui = fluidRow(
                  useShinyjs(),
                  #adding column to throw the dices
                   column(width = 8, offset = 0, style='padding:0px;',
                 inline(actionButton("jogar_dados", label = "Throw the Dices!")),
                 inline(actionButton("mexer_dados", label = "Shake the Dices!"))
                 )
               )
      )
    
    
    #removing some ui elements that won't be used
    removeUI(selector = "div:has(> #jogador)")
    removeUI(selector = "div:has(> #addplayerbutton)")
    removeUI(selector = "div:has(> #readytoplay)")
    

    
  })
 

  
  ### The game begins 
  ##começo da partida será assim
  observeEvent(input$jogar_dados, priority = 2,{

    if(n_jogada_dado == 0){
      
      dados_rol <<- (fixar(0)) #%>% as.character()
      


      #colocar os incones no lugar
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

    
   
      #adding ui element to select the dices
      if(aux_inicio_server == 0){
        
      insertUI(selector = '#placeholder', where = "beforeEnd", multiple = T,
               ui = fluidRow( 
                 column(9,
                 checkboxGroupInput("ficamdados", label = "Select the dices to hold",
                                        choiceNames =lista_choicenames,

                                    choiceValues = 1:5,
                                        selected = NULL, inline = T))
                 
                )
      )
        
        hideElement(id = "fim_jogada")

        
      }
      if(aux_inicio_server != 0){
        updateCheckboxGroupInput(session, "ficamdados", label = "Select the dices to hold",
                                 choiceNames =lista_choicenames,
                                 
                                 choiceValues = 1:5,
                                 inline = T, selected = NULL)
        # updateTextInput(session, "valor", value = 0)
        
      }
      
    }
    if(n_jogada_dado >= 1){
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

    
    ### updating the values shown in the right side of table
    
    
    #Numbers
    if(aux_inicio_jogo == 1){
      if(is.na(as.numeric(input$linhas)) == F){
        x <- length(which(dados_rol==input$linhas))
        updateTextInput(session, "valor", value = as.numeric(input$linhas)*x)
      }

    #uni is the auxiliar of fula, quadra e general
    uni <<- unique(dados_rol)
    
    #general
    if((input$linhas == "G") | (input$linhas == "G*")){
      if(length(uni) == 1){
        updateTextInput(session, "valor", value = 50)
        
        if(primeira_jogada == 1){
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
        if(primeira_jogada == 1){
          updateTextInput(session, "valor", value = 35)
        }
      }else(
        updateTextInput(session, "valor", value = 0)
      )
    }
    
    if(input$linhas == "Q"){
      if( ( (n1 == 4) & (n2 ==1) ) | ( (n1 == 1) & (n2 == 4) ) ) {
        updateTextInput(session, "valor", value = 40)
        
        if(primeira_jogada == 1){
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
        
        if(primeira_jogada == 1){
          updateTextInput(session, "valor", value = 26)
        }
      }else{
        updateTextInput(session, "valor", value = 0)
      }
    }
    
  }

})
  
  ###after update the values just after the dices are thrown, 
  ####it should appear the button to shake the dices and update some auxiliar objects
  observeEvent(input$jogar_dados, priority = 1,{
    
    #shake and throw are never together
    hideElement("jogar_dados")
    showElement("mexer_dados")
    showElement("linhas")

    #auxiliar
    n_jogada_dado <<- n_jogada_dado + 1
    
    #to update correcly the values of figures that are made in the first turn's throw
    if(length(escolhas) == 0){
      primeira_jogada <<- 1
    }else{
      primeira_jogada <<- 0
    }
    
    })
  
  #mexeu os dados, o jogar aparece
  observeEvent(input$mexer_dados, priority = 2,{
    
    #shake and throw are never together
    showElement("jogar_dados")
    hideElement("mexer_dados")
    
    hideElement("linhas")
    
    #auxiliar
    
    for(i in 1:5){
      lista_repeat[[i]] <<- icon("play", lib = "font-awesome", "fa-3x")
    }
    
    #using the play icon when the dices were not chosen to stay there
    if(is.null(escolhas) == F){
      
      lista_ate_mexer <- lista_repeat
      
      #mostrando os valores corretos nos icones e nao apenas os numeros
      for(i in as.numeric(escolhas)){
        
        if(dados_rol[i] == 1){
          lista_ate_mexer[[i]] <- icon("dice-one",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 2){
          lista_ate_mexer[[i]] <- icon("dice-two",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 3){
          lista_ate_mexer[[i]] <- icon("dice-three",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 4){
          lista_ate_mexer[[i]] <- icon("dice-four",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 5){
          lista_ate_mexer[[i]] <- icon("dice-five",lib = "font-awesome", "fa-3x")
        }
        if(dados_rol[i] == 6){
          lista_ate_mexer[[i]] <- icon("dice-six",lib = "font-awesome", "fa-3x")
        }
        
      }

    
    
    updateCheckboxGroupInput(session, "ficamdados", label = "Select the dices to hold",
                             choiceNames = lista_ate_mexer,
                             choiceValues = 1:5,
                             inline = T, selected = escolhas)
    }

    
                             
  })  
  
  ###updating the checkboxgroup
  
  observeEvent(c(input$ficamdados, input$jogar_dados),{
    escolhas <<- c(unique(input$ficamdados))

    
  })
  
  ###the turns finish, nothig is chosen and the dices turn into play icons 
  
  observeEvent(input$fim_jogada, priority = 2,{
    escolhas <<- c()
    
    
    #colocando os redo 
    
    for(i in 1:5){
      lista_repeat[[i]] <<- icon("play", lib = "font-awesome", "fa-3x")
    }
    
    
      updateCheckboxGroupInput(session, "ficamdados", label = "Select the dices to hold",
                               choiceNames = lista_repeat,
                               choiceValues = 1:5,
                               inline = T, selected = NULL)
      dados_rol <<- c()
      n_jogada_dado <<- 0
      aux_inicio_server <<- 1
      
  })

### after the turn is finished
observeEvent(input$jogar_dados, priority = 1, {
  if(aux_inicio_server == 0){
    aux_inicio_jogo <<- 1
    
    insertUI(selector = '#daquiprabaixotabela', where = "afterEnd", multiple = T,
           ui = fluidRow(
             column(5,

  
             inline(radioButtons("linhas", label = "Which line to add the values?",
                                   choices = nome_linhas,
                                   selected = NULL))
             ),
             column(1,
             inline(textInput("valor", label = "Value to add",
                                 value = "")),
                   
                    inline(actionButton("adicionar_valor", label = "Add to table", width = "150px")),
             inline(actionButton("fim_jogada", label = "Finish turn", width = "150px")),
             HTML(paste("<br></br>")),
             HTML(paste("<br></br>")),
                    actionButton("revanche", label = "Play again", width = "150px")#,

           )
           )
  )

  }


  })
  
  #updating the values to add in the the table on the textinput in the side after
  #changing the radiobuttons
  observeEvent(input$linhas,{

    if(is.na(as.numeric(input$linhas)) == F){
      x <<- length(which(dados_rol==input$linhas))
      updateTextInput(session, "valor", value = as.numeric(input$linhas)*x)

    }
    
    # uni is the auxiliar
    uni <<- unique(dados_rol)
    
    #general
    if((input$linhas == "G") | (input$linhas == "G*")){
      if(length(uni) == 1){
          updateTextInput(session, "valor", value = 50)
        
          if(primeira_jogada == 1){
            updateTextInput(session, "valor", value = 1000)
          }
      }else(
          updateTextInput(session, "valor", value = 0)
        )
  }
    
    
    #Fula and quadra
    if(length(uni) == 2){
      n1 <<- length(dados_rol[dados_rol==uni[1]])
      n2 <<- length(dados_rol[dados_rol==uni[2]])
      
      if(input$linhas == "F"){
        if( ( (n1 == 3) & (n2 ==2) ) | ( (n1 == 2) & (n2 == 3) ) ) {
          updateTextInput(session, "valor", value = 30)
          
          
          if(primeira_jogada == 1){
            updateTextInput(session, "valor", value = 35)
          }
        }else(
          updateTextInput(session, "valor", value = 0)
        )
      }
      
      if(input$linhas == "Q"){
        if( ( (n1 == 4) & (n2 ==1) ) | ( (n1 == 1) & (n2 == 4) ) ) {
          updateTextInput(session, "valor", value = 40)
          
          if(primeira_jogada == 1){
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
        
        if(primeira_jogada == 1){
          updateTextInput(session, "valor", value = 26)
        }
      }else{
        updateTextInput(session, "valor", value = 0)
      }
    }
    

  })

  
  #now update the table after the value is chosen
observeEvent(input$adicionar_valor,{
  

  if(folha_aux[(folha_aux$Lines == input$linhas), input$jogando] != ""){
    updateTextInput(session, "valor", value = "Selecione outra linha")
    
    if(input$valor == "Selecione outra linha"){
      updateTextInput(session, "valor", value = "Selecione outra linha") 
    }
    
  }else{
    
  folha_aux[(folha_aux$Lines == input$linhas), input$jogando] <<- input$valor 
  
}
    

  output$table <<- render_gt(folha_generator(folha_aux), align = 'left')
  
  
  #summing everything
  aux_total_pts <<- folha_aux[1:11,input$jogando]
  tosum <<- aux_total_pts[! aux_total_pts %in% ""]
  
  folha_aux[(folha_aux$Lines == "Total"), input$jogando] <<- sum(as.numeric(tosum))
  
})


#After the end of player's turn, the players' radiobutton updates automatically
observeEvent(input$fim_jogada, priority = 1,{
  
  if(aux_players == length(players)-1){
    aux_players <<- 0
  }else{
  aux_players <<- aux_players + 1
  }
  
     
  updateRadioButtons(session, inputId = "jogando", 
                     selected = players[aux_players+1])

  updateTextInput(session, "valor", value = 0)
  
})

#Play again, it initializes with the same players
observeEvent(input$revanche,{
  
  updateRadioButtons(session, inputId = "jogando", 
                     selected = players[1])
  
  
  
  folha_aux[,2:ncol(folha_aux)] <<- ""
  
  
  
  output$table <<- render_gt(folha_generator(folha_aux), align = 'left')
  
  #play icons
  for(i in 1:5){
    lista_repeat[[i]] <<- icon("play", lib = "font-awesome", "fa-3x")
  }
  
  
  updateCheckboxGroupInput(session, "ficamdados", label = "Select the dices to hold",
                           choiceNames = lista_repeat,
                           choiceValues = 1:5,
                           inline = T, selected = NULL)
  dados_rol <<- c()
  n_jogada_dado <<- 0
  aux_inicio_server <<- 1
  
  
})

session$onSessionEnded(function(){
  stopApp()
})


}



