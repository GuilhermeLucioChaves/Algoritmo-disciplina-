algoritmo "MedControl"
// ============================================================================
// MEDCONTROL - Sistema de Gerenciamento de Atendimentos de uma Clinica Medica
// Desafio 1 - Logica de Programacao - Tema 2
//
// OBSERVACOES DE PROJETO:
//
// 1) CONSTANTES: variaveis em MAIUSCULAS preenchidas uma unica vez em
//    inicializarSistema e nunca mais alteradas (o VisuAlg nao possui "const").
//
// 2) REGISTRO + VETOR DE REGISTROS: o tipo TPaciente (registro) agrupa todos
//    os dados de um paciente. O vetor "pacientes" guarda ate MAX_PACIENTES
//    registros. Ex.: pacientes[3].nome e pacientes[3].idade sao campos do
//    paciente 3. Na ordenacao, os registros sao trocados por inteiro
//    (procedimento trocarPacientes), preservando todas as informacoes.
//
// 3) MATRIZ matEstat[prioridade, coluna]:
//    linhas  = 4 niveis de prioridade (1 a 4)
//    colunas = 1-Cadastrados, 2-Aguardando, 3-Atendidos
//    Atualizada em cadastrarPaciente (cadastrados+1, aguardando+1) e em
//    realizarAtendimento (aguardando-1, atendidos+1).
//
// 4) GLOBAIS: apenas constantes, vetor de registros, matriz e contador de
//    pacientes. O vetor/matriz sao globais pois o VisuAlg nao aceita
//    vetor como parametro (por isso a busca recursiva recebe codigo, posicao
//    atual e quantidade por parametro e acessa o vetor global). A quantidade de
//    pacientes e passada por parametro sempre que possivel; o que e apenas
//    auxiliar (contadores, acumuladores, leituras) e LOCAL.
//
// 5) FILA: Bubble Sort sobre o vetor de registros. Criterios, em ordem:
//    status (aguardando antes de atendido), prioridade (menor numero primeiro),
//    maior idade, ordem de cadastro (campo ordem).
// ============================================================================
tipo
   // REGISTRO: agrupa logicamente todos os dados de UM paciente
   TPaciente = registro
      codigo: inteiro          // identificador unico
      nome: caractere
      idade: inteiro
      sexo: caractere          // "M" ou "F"
      prioridade: inteiro      // 1 a 4 (EMERGENCIA ... POUCO_URGENTE)
      sistolica: inteiro       // pressao arterial sistolica
      diastolica: inteiro      // pressao arterial diastolica
      temperatura: real
      status: inteiro          // AGUARDANDO ou ATENDIDO
      ordem: inteiro           // ordem de cadastro (criterio de desempate)
   fimregistro

var
   // ---------- "constantes" ----------
   MAX_PACIENTES: inteiro       // deve ser igual ao tamanho dos vetores (30)
   EMERGENCIA: inteiro
   MUITO_URGENTE: inteiro
   URGENTE: inteiro
   POUCO_URGENTE: inteiro
   AGUARDANDO: inteiro
   ATENDIDO: inteiro
   NAO_ENCONTRADO: inteiro      // retorno das buscas quando nada e encontrado
   IDADE_MAXIMA: inteiro
   PA_SIST_MIN: inteiro
   PA_SIST_MAX: inteiro
   PA_DIAST_MIN: inteiro
   PA_DIAST_MAX: inteiro
   TEMP_MIN: real
   TEMP_MAX: real
   COL_CADASTRADOS: inteiro     // colunas da matriz de estatisticas
   COL_AGUARDANDO: inteiro
   COL_ATENDIDOS: inteiro

   // ---------- vetor de registros ----------
   pacientes: vetor[1..30] de TPaciente      // 30 = MAX_PACIENTES

   // ---------- matriz de estatisticas ----------
   matEstat: vetor[1..4,1..3] de inteiro

   // ---------- estado geral ----------
   qtdPacientes: inteiro
   opcao: inteiro

// ============================================================================
// SUB-ROTINAS DE APOIO (entrada, validacao e formatacao)
// ============================================================================

procedimento inicializarSistema
var
   i, j: inteiro
inicio
   MAX_PACIENTES <- 30
   EMERGENCIA <- 1
   MUITO_URGENTE <- 2
   URGENTE <- 3
   POUCO_URGENTE <- 4
   AGUARDANDO <- 0
   ATENDIDO <- 1
   NAO_ENCONTRADO <- 0
   IDADE_MAXIMA <- 120
   PA_SIST_MIN <- 50
   PA_SIST_MAX <- 300
   PA_DIAST_MIN <- 30
   PA_DIAST_MAX <- 200
   TEMP_MIN <- 30
   TEMP_MAX <- 45
   COL_CADASTRADOS <- 1
   COL_AGUARDANDO <- 2
   COL_ATENDIDOS <- 3
   qtdPacientes <- 0
   // zera a matriz de estatisticas (repeticao aninhada)
   para i de 1 ate 4 faca
      para j de 1 ate 3 faca
         matEstat[i,j] <- 0
      fimpara
   fimpara
   // zera o vetor de pacientes
   para i de 1 ate MAX_PACIENTES faca
      pacientes[i].codigo <- 0
      pacientes[i].nome <- ""
      pacientes[i].idade <- 0
      pacientes[i].sexo <- ""
      pacientes[i].prioridade <- 0
      pacientes[i].sistolica <- 0
      pacientes[i].diastolica <- 0
      pacientes[i].temperatura <- 0
      pacientes[i].status <- AGUARDANDO
      pacientes[i].ordem <- 0
   fimpara
fimprocedimento

// Escreve o texto e completa com espacos ate a largura informada
procedimento escreverPad(texto: caractere; largura: inteiro)
var
   i: inteiro
inicio
   escreva(texto)
   para i de compr(texto) + 1 ate largura faca
      escreva(" ")
   fimpara
fimprocedimento

// Le um numero INTEIRO como texto e so aceita se for realmente numerico
// (aceita sinal "-"). Evita erro de execucao do VisuAlg com letras.
funcao lerInteiro(mensagem: caractere): inteiro
var
   texto: caractere
   i, inicioDigitos, valor: inteiro
   valido, negativo: logico
inicio
   repita
      escreva(mensagem)
      leia(texto)
      valido <- verdadeiro
      negativo <- falso
      inicioDigitos <- 1
      se compr(texto) = 0 entao
         valido <- falso
      senao
         se copia(texto, 1, 1) = "-" entao
            negativo <- verdadeiro
            inicioDigitos <- 2
         fimse
         // so o sinal, ou texto longo demais
         se (compr(texto) < inicioDigitos) ou (compr(texto) > 9) entao
            valido <- falso
         fimse
         i <- inicioDigitos
         enquanto (i <= compr(texto)) e valido faca
            se (asc(copia(texto, i, 1)) < 48) ou (asc(copia(texto, i, 1)) > 57) entao
               valido <- falso
            fimse
            i <- i + 1
         fimenquanto
      fimse
      se nao valido entao
         escreval("Entrada invalida! Digite apenas numeros inteiros.")
      fimse
   ate valido
   // converte o texto validado em numero (acumulador)
   valor <- 0
   para i de inicioDigitos ate compr(texto) faca
      valor <- valor * 10 + (asc(copia(texto, i, 1)) - 48)
   fimpara
   se negativo entao
      valor <- -valor
   fimse
   retorne valor
fimfuncao

// Le um numero REAL (aceita ponto ou virgula) validando os caracteres
funcao lerReal(mensagem: caractere): real
var
   texto, ch: caractere
   i, qtdSeparadores, qtdDigitos: inteiro
   valido, passouSeparador: logico
   resultado, divisor: real
inicio
   repita
      escreva(mensagem)
      leia(texto)
      valido <- verdadeiro
      qtdSeparadores <- 0
      qtdDigitos <- 0
      se compr(texto) = 0 entao
         valido <- falso
      senao
         para i de 1 ate compr(texto) faca
            ch <- copia(texto, i, 1)
            se (asc(ch) >= 48) e (asc(ch) <= 57) entao
               qtdDigitos <- qtdDigitos + 1
            senao
               se (ch = ".") ou (ch = ",") entao
                  qtdSeparadores <- qtdSeparadores + 1
               senao
                  valido <- falso
               fimse
            fimse
         fimpara
         se (qtdSeparadores > 1) ou (qtdDigitos = 0) ou (compr(texto) > 8) entao
            valido <- falso
         fimse
      fimse
      se nao valido entao
         escreval("Entrada invalida! Digite um numero (ex.: 36.5).")
      fimse
   ate valido
   // converte o texto validado em numero real
   resultado <- 0
   divisor <- 1
   passouSeparador <- falso
   para i de 1 ate compr(texto) faca
      ch <- copia(texto, i, 1)
      se (ch = ".") ou (ch = ",") entao
         passouSeparador <- verdadeiro
      senao
         se passouSeparador entao
            divisor <- divisor * 10
            resultado <- resultado + (asc(ch) - 48) / divisor
         senao
            resultado <- resultado * 10 + (asc(ch) - 48)
         fimse
      fimse
   fimpara
   retorne resultado
fimfuncao

// Le um inteiro dentro de [minimo, maximo], repetindo ate ser valido
funcao lerInteiroEntre(mensagem: caractere; minimo: inteiro; maximo: inteiro): inteiro
var
   valor: inteiro
inicio
   repita
      escreva(mensagem, " [", minimo, " a ", maximo, "]")
      valor <- lerInteiro(": ")
      se valor < 0 entao
         escreval("Valor negativo nao e permitido!")
      senao
         se (valor < minimo) ou (valor > maximo) entao
            escreval("Valor fora do intervalo permitido!")
         fimse
      fimse
   ate (valor >= minimo) e (valor <= maximo)
   retorne valor
fimfuncao

// Le um real dentro de [minimo, maximo], repetindo ate ser valido
funcao lerRealEntre(mensagem: caractere; minimo: real; maximo: real): real
var
   valor: real
inicio
   repita
      escreva(mensagem, " [", minimo:4:1, " a ", maximo:4:1, "]")
      valor <- lerReal(": ")
      se (valor < minimo) ou (valor > maximo) entao
         escreval("Valor fora do intervalo permitido!")
      fimse
   ate (valor >= minimo) e (valor <= maximo)
   retorne valor
fimfuncao

// Le o sexo: aceita apenas M ou F (maiuscula ou minuscula)
funcao lerSexo(mensagem: caractere): caractere
var
   entrada: caractere
inicio
   repita
      escreva(mensagem)
      leia(entrada)
      entrada <- maiusc(entrada)
      se (entrada <> "M") e (entrada <> "F") entao
         escreval("Sexo invalido! Digite M ou F.")
      fimse
   ate (entrada = "M") ou (entrada = "F")
   retorne entrada
fimfuncao

// Um texto e valido se tiver ao menos um caractere diferente de espaco
funcao nomeValido(texto: caractere): logico
var
   i: inteiro
   achouLetra: logico
inicio
   achouLetra <- falso
   para i de 1 ate compr(texto) faca
      se copia(texto, i, 1) <> " " entao
         achouLetra <- verdadeiro
      fimse
   fimpara
   retorne achouLetra
fimfuncao

// Converte o codigo de prioridade em texto (funcao: devolve um valor)
funcao nomePrioridade(prioridade: inteiro): caractere
var
   texto: caractere
inicio
   se prioridade = EMERGENCIA entao
      texto <- "Emergencia"
   senao
      se prioridade = MUITO_URGENTE entao
         texto <- "Muito urgente"
      senao
         se prioridade = URGENTE entao
            texto <- "Urgente"
         senao
            texto <- "Pouco urgente"
         fimse
      fimse
   fimse
   retorne texto
fimfuncao

funcao nomeStatus(situacao: inteiro): caractere
var
   texto: caractere
inicio
   se situacao = AGUARDANDO entao
      texto <- "Aguardando atendimento"
   senao
      texto <- "Atendido"
   fimse
   retorne texto
fimfuncao

// Funcao: media = soma / quantidade (protege contra divisao por zero)
funcao calcularMedia(soma: real; quantidade: inteiro): real
var
   media: real
inicio
   se quantidade > 0 entao
      media <- soma / quantidade
   senao
      media <- 0
   fimse
   retorne media
fimfuncao

// Funcao: percentual = parte * 100 / total
funcao calcularPercentual(parte: inteiro; total: inteiro): real
var
   percentual: real
inicio
   se total > 0 entao
      percentual <- parte * 100 / total
   senao
      percentual <- 0
   fimse
   retorne percentual
fimfuncao

// ============================================================================
// BUSCAS
// ============================================================================

// Busca ITERATIVA: devolve a posicao do paciente ou NAO_ENCONTRADO
funcao localizarPaciente(codigo: inteiro; total: inteiro): inteiro
var
   i, posicao: inteiro
inicio
   posicao <- NAO_ENCONTRADO
   i <- 1
   enquanto (i <= total) e (posicao = NAO_ENCONTRADO) faca
      se pacientes[i].codigo = codigo entao
         posicao <- i
      fimse
      i <- i + 1
   fimenquanto
   retorne posicao
fimfuncao

// Busca RECURSIVA: verifica UMA posicao e chama a si propria para a proxima.
// CASO-BASE 1: posicao > total -> todas as posicoes validas foram verificadas.
// CASO-BASE 2: pacientes[posicao].codigo = codigo -> paciente encontrado.
// PASSO RECURSIVO: buscaRecursiva(codigo, posicao + 1, total).
funcao buscaRecursiva(codigo: inteiro; posicao: inteiro; total: inteiro): inteiro
var
   resultado: inteiro
inicio
   se posicao > total entao
      resultado <- NAO_ENCONTRADO
   senao
      se pacientes[posicao].codigo = codigo entao
         resultado <- posicao
      senao
         resultado <- buscaRecursiva(codigo, posicao + 1, total)
      fimse
   fimse
   retorne resultado
fimfuncao

// Contagem RECURSIVA de pacientes aguardando:
// quantidadeAguardando(pos) = (1 se a posicao atual aguarda, senao 0)
//                             + quantidadeAguardando(pos + 1)
// CASO-BASE: posicao > total -> nao ha mais pacientes (retorna 0).
funcao quantidadeAguardando(posicao: inteiro; total: inteiro): inteiro
var
   parcial, resultado: inteiro
inicio
   se posicao > total entao
      resultado <- 0
   senao
      se pacientes[posicao].status = AGUARDANDO entao
         parcial <- 1
      senao
         parcial <- 0
      fimse
      resultado <- parcial + quantidadeAguardando(posicao + 1, total)
   fimse
   retorne resultado
fimfuncao

// Le um codigo NOVO: repete enquanto for invalido ou duplicado.
// Retorna 0 se o usuario cancelar.
funcao lerCodigoNovo(total: inteiro): inteiro
var
   codigo: inteiro
   aceito: logico
inicio
   aceito <- falso
   repita
      codigo <- lerInteiro("Codigo do paciente (inteiro positivo, 0 = cancelar): ")
      se codigo = 0 entao
         aceito <- verdadeiro
      senao
         se codigo < 0 entao
            escreval("Codigo invalido! O codigo deve ser positivo.")
         senao
            se localizarPaciente(codigo, total) <> NAO_ENCONTRADO entao
               escreval("Codigo duplicado! Ja existe paciente com o codigo ", codigo, ". Tente outro.")
            senao
               aceito <- verdadeiro
            fimse
         fimse
      fimse
   ate aceito
   retorne codigo
fimfuncao

// ============================================================================
// FUNCIONALIDADES DO MENU
// ============================================================================

// Cadastro. 'total' e passado POR REFERENCIA (var): o procedimento altera a
// quantidade de pacientes e a mudanca precisa refletir no programa principal.
procedimento cadastrarPaciente(var total: inteiro)
var
   codigo, idade, prioridade, sistolica, diastolica, novo: inteiro
   nome, sexo: caractere
   temperatura: real
inicio
   se total >= MAX_PACIENTES entao
      escreval("Capacidade maxima atingida (", MAX_PACIENTES, " pacientes). Cadastro nao permitido.")
   senao
      codigo <- lerCodigoNovo(total)
      se codigo = 0 entao
         escreval("Cadastro cancelado.")
      senao
         repita
            escreva("Nome do paciente: ")
            leia(nome)
            se nao nomeValido(nome) entao
               escreval("O nome nao pode ser vazio!")
            fimse
         ate nomeValido(nome)
         idade <- lerInteiroEntre("Idade", 0, IDADE_MAXIMA)
         sexo <- lerSexo("Sexo (M/F): ")
         escreval("Prioridade: 1-Emergencia 2-Muito urgente 3-Urgente 4-Pouco urgente")
         prioridade <- lerInteiroEntre("Prioridade", EMERGENCIA, POUCO_URGENTE)
         repita
            sistolica <- lerInteiroEntre("Pressao sistolica (mmHg)", PA_SIST_MIN, PA_SIST_MAX)
            diastolica <- lerInteiroEntre("Pressao diastolica (mmHg)", PA_DIAST_MIN, PA_DIAST_MAX)
            se diastolica >= sistolica entao
               escreval("Pressoes inconsistentes: a diastolica deve ser menor que a sistolica.")
            fimse
         ate diastolica < sistolica
         temperatura <- lerRealEntre("Temperatura corporal (graus C)", TEMP_MIN, TEMP_MAX)

         // todos os dados validos: grava o registro
         total <- total + 1
         novo <- total
         pacientes[novo].codigo <- codigo
         pacientes[novo].nome <- nome
         pacientes[novo].idade <- idade
         pacientes[novo].sexo <- sexo
         pacientes[novo].prioridade <- prioridade
         pacientes[novo].sistolica <- sistolica
         pacientes[novo].diastolica <- diastolica
         pacientes[novo].temperatura <- temperatura
         pacientes[novo].status <- AGUARDANDO
         pacientes[novo].ordem <- total
         // atualiza a matriz de estatisticas
         matEstat[prioridade, COL_CADASTRADOS] <- matEstat[prioridade, COL_CADASTRADOS] + 1
         matEstat[prioridade, COL_AGUARDANDO] <- matEstat[prioridade, COL_AGUARDANDO] + 1
         escreval("Paciente ", nome, " cadastrado com sucesso! Status: ", nomeStatus(AGUARDANDO))
      fimse
   fimse
fimprocedimento

// Exibe os dados completos de um paciente (idx = posicao no vetor)
procedimento exibirPaciente(idx: inteiro)
inicio
   escreval("Codigo.............: ", pacientes[idx].codigo)
   escreval("Nome...............: ", pacientes[idx].nome)
   escreval("Idade..............: ", pacientes[idx].idade)
   escreval("Sexo...............: ", pacientes[idx].sexo)
   escreval("Prioridade.........: ", pacientes[idx].prioridade, " - ", nomePrioridade(pacientes[idx].prioridade))
   escreval("Pressao arterial...: ", pacientes[idx].sistolica, "/", pacientes[idx].diastolica, " mmHg")
   escreval("Temperatura........: ", pacientes[idx].temperatura:4:1, " C")
   escreval("Status.............: ", nomeStatus(pacientes[idx].status))
   escreval("Ordem de cadastro..: ", pacientes[idx].ordem)
fimprocedimento

procedimento listarPacientes(total: inteiro)
var
   i: inteiro
inicio
   se total = 0 entao
      escreval("Nenhum paciente cadastrado.")
   senao
      escreval("Ord  Cod    Nome                  Idade Sx Prioridade       Status")
      escreval("-----------------------------------------------------------------------")
      para i de 1 ate total faca
         escreva(pacientes[i].ordem:3, "  ", pacientes[i].codigo:5, "  ")
         escreverPad(pacientes[i].nome, 20)
         escreva(pacientes[i].idade:6, "  ", pacientes[i].sexo, "  ")
         escreverPad(nomePrioridade(pacientes[i].prioridade), 15)
         escreval(" ", nomeStatus(pacientes[i].status))
      fimpara
      escreval("Total de pacientes: ", total)
   fimse
fimprocedimento

procedimento consultarPaciente(total: inteiro)
var
   codigo, posicao: inteiro
inicio
   se total = 0 entao
      escreval("Nenhum paciente cadastrado. Nao ha dados para consultar.")
   senao
      codigo <- lerInteiro("Codigo do paciente: ")
      posicao <- localizarPaciente(codigo, total)
      se posicao = NAO_ENCONTRADO entao
         escreval("Paciente nao encontrado (codigo ", codigo, " inexistente).")
      senao
         escreval("Paciente encontrado.")
         exibirPaciente(posicao)
      fimse
   fimse
fimprocedimento

// Funcao: decide se o paciente da posicao 'b' deve ficar ANTES do da posicao 'a'.
// Criterios em cascata: 1) aguardando antes de atendido
//                       2) menor numero de prioridade (1 = mais grave)
//                       3) maior idade
//                       4) menor ordem de cadastro
funcao deveTrocar(a: inteiro; b: inteiro): logico
var
   trocar: logico
inicio
   trocar <- falso
   se pacientes[b].status < pacientes[a].status entao
      trocar <- verdadeiro
   senao
      se pacientes[b].status = pacientes[a].status entao
         se pacientes[b].prioridade < pacientes[a].prioridade entao
            trocar <- verdadeiro
         senao
            se pacientes[b].prioridade = pacientes[a].prioridade entao
               se pacientes[b].idade > pacientes[a].idade entao
                  trocar <- verdadeiro
               senao
                  se pacientes[b].idade = pacientes[a].idade entao
                     se pacientes[b].ordem < pacientes[a].ordem entao
                        trocar <- verdadeiro
                     fimse
                  fimse
               fimse
            fimse
         fimse
      fimse
   fimse
   retorne trocar
fimfuncao

// Troca dois pacientes de posicao, campo a campo (registro completo preservado)
procedimento trocarPacientes(a: inteiro; b: inteiro)
var
   auxInt: inteiro
   auxTxt: caractere
   auxReal: real
inicio
   auxInt <- pacientes[a].codigo
   pacientes[a].codigo <- pacientes[b].codigo
   pacientes[b].codigo <- auxInt

   auxTxt <- pacientes[a].nome
   pacientes[a].nome <- pacientes[b].nome
   pacientes[b].nome <- auxTxt

   auxInt <- pacientes[a].idade
   pacientes[a].idade <- pacientes[b].idade
   pacientes[b].idade <- auxInt

   auxTxt <- pacientes[a].sexo
   pacientes[a].sexo <- pacientes[b].sexo
   pacientes[b].sexo <- auxTxt

   auxInt <- pacientes[a].prioridade
   pacientes[a].prioridade <- pacientes[b].prioridade
   pacientes[b].prioridade <- auxInt

   auxInt <- pacientes[a].sistolica
   pacientes[a].sistolica <- pacientes[b].sistolica
   pacientes[b].sistolica <- auxInt

   auxInt <- pacientes[a].diastolica
   pacientes[a].diastolica <- pacientes[b].diastolica
   pacientes[b].diastolica <- auxInt

   auxReal <- pacientes[a].temperatura
   pacientes[a].temperatura <- pacientes[b].temperatura
   pacientes[b].temperatura <- auxReal

   auxInt <- pacientes[a].status
   pacientes[a].status <- pacientes[b].status
   pacientes[b].status <- auxInt

   auxInt <- pacientes[a].ordem
   pacientes[a].ordem <- pacientes[b].ordem
   pacientes[b].ordem <- auxInt
fimprocedimento

// ORDENACAO: Bubble Sort. Compara vizinhos e troca quando deveTrocar = verdadeiro.
// Ao final, os pacientes AGUARDANDO ficam no inicio do vetor, em ordem de fila.
procedimento ordenarFila(total: inteiro)
var
   i, j: inteiro
inicio
   para i de 1 ate total - 1 faca
      para j de 1 ate total - i faca
         se deveTrocar(j, j + 1) entao
            trocarPacientes(j, j + 1)
         fimse
      fimpara
   fimpara
fimprocedimento

procedimento exibirFila(total: inteiro)
var
   i, posFila, qtdAguardando: inteiro
inicio
   se total = 0 entao
      escreval("Nenhum paciente cadastrado.")
   senao
      qtdAguardando <- quantidadeAguardando(1, total)
      se qtdAguardando = 0 entao
         escreval("Fila vazia: nenhum paciente aguardando atendimento.")
      senao
         ordenarFila(total)
         escreval("=========== FILA DE ATENDIMENTO ===========")
         escreval("Pos  Cod    Nome                  Idade Prioridade")
         escreval("-----------------------------------------------------")
         posFila <- 0
         para i de 1 ate total faca
            se pacientes[i].status = AGUARDANDO entao
               posFila <- posFila + 1
               escreva(posFila:3, "  ", pacientes[i].codigo:5, "  ")
               escreverPad(pacientes[i].nome, 20)
               escreva(pacientes[i].idade:6, "  ")
               escreverPad(nomePrioridade(pacientes[i].prioridade), 15)
               // destaque para prioridade 1 OU 2
               se (pacientes[i].prioridade = EMERGENCIA) ou (pacientes[i].prioridade = MUITO_URGENTE) entao
                  escreva(" (!)")
               fimse
               escreval("")
            fimse
         fimpara
         escreval("Aguardando: ", qtdAguardando)
      fimse
   fimse
fimprocedimento

procedimento realizarAtendimento(total: inteiro)
var
   qtdAguardando, codigo, alvo: inteiro
inicio
   se total = 0 entao
      escreval("Nenhum paciente cadastrado.")
   senao
      qtdAguardando <- quantidadeAguardando(1, total)
      se qtdAguardando = 0 entao
         escreval("Nao ha pacientes aguardando atendimento (fila vazia).")
      senao
         ordenarFila(total)
         // apos a ordenacao, a posicao 1 e o proximo da fila
         escreval("Proximo da fila: ", pacientes[1].nome, " (codigo ", pacientes[1].codigo, ", ", nomePrioridade(pacientes[1].prioridade), ")")
         se (pacientes[1].prioridade = EMERGENCIA) e (pacientes[1].status = AGUARDANDO) entao
            escreval("*** ATENCAO: paciente em EMERGENCIA! ***")
         fimse
         codigo <- lerInteiro("Codigo do paciente a atender (0 = proximo da fila): ")
         se codigo = 0 entao
            alvo <- 1
         senao
            alvo <- localizarPaciente(codigo, total)
         fimse
         se alvo = NAO_ENCONTRADO entao
            escreval("Atendimento negado: paciente inexistente.")
         senao
            se pacientes[alvo].status = ATENDIDO entao
               escreval("Atendimento negado: este paciente ja foi atendido.")
            senao
               se alvo <> 1 entao
                  escreval("Atendimento negado: fora da ordem da fila. O proximo e ", pacientes[1].nome, " (codigo ", pacientes[1].codigo, ").")
               senao
                  pacientes[alvo].status <- ATENDIDO
                  // atualiza a matriz: sai de "aguardando" e entra em "atendidos"
                  matEstat[pacientes[alvo].prioridade, COL_AGUARDANDO] <- matEstat[pacientes[alvo].prioridade, COL_AGUARDANDO] - 1
                  matEstat[pacientes[alvo].prioridade, COL_ATENDIDOS] <- matEstat[pacientes[alvo].prioridade, COL_ATENDIDOS] + 1
                  escreval("Atendimento realizado: ", pacientes[alvo].nome, " (codigo ", pacientes[alvo].codigo, ").")
               fimse
            fimse
         fimse
      fimse
   fimse
fimprocedimento

procedimento exibirAtendidos(total: inteiro)
var
   i, qtdAtendidos: inteiro
inicio
   se total = 0 entao
      escreval("Nenhum paciente cadastrado.")
   senao
      qtdAtendidos <- 0
      para i de 1 ate total faca
         se pacientes[i].status = ATENDIDO entao
            se qtdAtendidos = 0 entao
               escreval("=========== PACIENTES ATENDIDOS ===========")
               escreval("Cod    Nome                  Idade Prioridade")
               escreval("-----------------------------------------------")
            fimse
            qtdAtendidos <- qtdAtendidos + 1
            escreva(pacientes[i].codigo:5, "  ")
            escreverPad(pacientes[i].nome, 20)
            escreva(pacientes[i].idade:6, "  ")
            escreval(nomePrioridade(pacientes[i].prioridade))
         fimse
      fimpara
      se qtdAtendidos = 0 entao
         escreval("Nenhum paciente foi atendido ainda.")
      senao
         escreval("Total de atendidos: ", qtdAtendidos)
      fimse
   fimse
fimprocedimento

procedimento exibirEstatisticas(total: inteiro)
var
   i, j, qtdAguardando, qtdAtendidos, maiorIdade, menorIdade, qtdPrioAlta: inteiro
   somaIdade, somaTemperatura, mediaIdade, mediaTemperatura: real
inicio
   escreval("================ ESTATISTICAS ================")
   se total = 0 entao
      escreval("Nenhum paciente cadastrado. Nao ha estatisticas para exibir.")
   senao
      // acumuladores e contadores
      somaIdade <- 0
      somaTemperatura <- 0
      maiorIdade <- pacientes[1].idade
      menorIdade <- pacientes[1].idade
      qtdPrioAlta <- 0
      para i de 1 ate total faca
         somaIdade <- somaIdade + pacientes[i].idade
         somaTemperatura <- somaTemperatura + pacientes[i].temperatura
         se pacientes[i].idade > maiorIdade entao
            maiorIdade <- pacientes[i].idade
         fimse
         se pacientes[i].idade < menorIdade entao
            menorIdade <- pacientes[i].idade
         fimse
         se (pacientes[i].prioridade = EMERGENCIA) ou (pacientes[i].prioridade = MUITO_URGENTE) entao
            qtdPrioAlta <- qtdPrioAlta + 1
         fimse
      fimpara
      qtdAguardando <- quantidadeAguardando(1, total)   // calculo recursivo
      qtdAtendidos <- total - qtdAguardando
      mediaIdade <- calcularMedia(somaIdade, total)
      mediaTemperatura <- calcularMedia(somaTemperatura, total)

      escreval("Total de pacientes.............: ", total)
      escreval("Pacientes aguardando...........: ", qtdAguardando)
      escreval("Pacientes atendidos............: ", qtdAtendidos)
      escreval("Media de idade.................: ", mediaIdade:6:1, " anos")
      escreval("Media de temperatura...........: ", mediaTemperatura:5:1, " C")
      escreval("Maior idade cadastrada.........: ", maiorIdade)
      escreval("Menor idade cadastrada.........: ", menorIdade)
      escreval("Percentual de atendidos........: ", calcularPercentual(qtdAtendidos, total):6:1, " %")
      escreval("Percentual aguardando..........: ", calcularPercentual(qtdAguardando, total):6:1, " %")
      escreval("Pacientes com prioridade 1 ou 2: ", qtdPrioAlta)
      escreval("")
      escreval("Por prioridade (matriz de estatisticas):")
      escreval("Prioridade           Cadastrados  Aguardando   Atendidos")
      para i de EMERGENCIA ate POUCO_URGENTE faca
         escreverPad(nomePrioridade(i), 20)
         para j de COL_CADASTRADOS ate COL_ATENDIDOS faca
            escreva(matEstat[i,j]:12)
         fimpara
         escreval("")
      fimpara
   fimse
fimprocedimento

// Menu 8: consulta de paciente usando a busca RECURSIVA
procedimento consultaRecursiva(total: inteiro)
var
   codigo, posicao: inteiro
inicio
   se total = 0 entao
      escreval("Nenhum paciente cadastrado. Nao ha dados para consultar.")
   senao
      codigo <- lerInteiro("Codigo do paciente (busca recursiva): ")
      posicao <- buscaRecursiva(codigo, 1, total)
      se posicao = NAO_ENCONTRADO entao
         escreval("Busca recursiva: paciente nao encontrado (codigo ", codigo, ").")
      senao
         escreval("Busca recursiva: paciente encontrado na posicao ", posicao, " do vetor.")
         exibirPaciente(posicao)
      fimse
      escreval("Pacientes aguardando (contagem recursiva): ", quantidadeAguardando(1, total))
   fimse
fimprocedimento

procedimento exibirMenu
inicio
   escreval("")
   escreval("====== MEDCONTROL - GERENCIAMENTO DE ATENDIMENTOS ======")
   escreval("1. Cadastrar paciente")
   escreval("2. Listar pacientes")
   escreval("3. Consultar paciente")
   escreval("4. Exibir fila de atendimento")
   escreval("5. Realizar atendimento")
   escreval("6. Exibir pacientes atendidos")
   escreval("7. Exibir estatisticas")
   escreval("8. Executar consulta recursiva")
   escreval("9. Encerrar sistema")
   escreval("========================================================")
fimprocedimento

// ============================================================================
// ALGORITMO PRINCIPAL: inicializa e controla o menu
// ============================================================================
inicio
   inicializarSistema
   repita
      exibirMenu
      opcao <- lerInteiro("Escolha uma opcao: ")
      escreval("")
      escolha opcao
         caso 1
            cadastrarPaciente(qtdPacientes)
         caso 2
            listarPacientes(qtdPacientes)
         caso 3
            consultarPaciente(qtdPacientes)
         caso 4
            exibirFila(qtdPacientes)
         caso 5
            realizarAtendimento(qtdPacientes)
         caso 6
            exibirAtendidos(qtdPacientes)
         caso 7
            exibirEstatisticas(qtdPacientes)
         caso 8
            consultaRecursiva(qtdPacientes)
         caso 9
            escreval("Sistema encerrado.")
         outrocaso
            escreval("Opcao invalida! Escolha um numero de 1 a 9.")
      fimescolha
   ate opcao = 9
fimalgoritmo
