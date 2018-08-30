
build_education_attainment_1970 <- function(CensusData){
        
        library(data.table)
        library(tidyverse)
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        #rm(list=ls())
        gc()
        #options(scipen = 999)
        #library(data.table)
        #library(tidyverse)
        #library(questionr)
        #library(descr)
        
        #setwd("c:\\users\\rogerio\\desktop\\Censo 1970\\")
        
        # Censo de 1970 ==============================================================================================================
        # ============================================================================================================================
        
        # Abrindo dados
        #CensusData <- fread("censo1970.csv")
        
        #######################
        # Nenhum grau completo
        #######################
        
        # Se nunca frequentou a escola ou grau nao declarado
        # (todos os casos de serie sem declaracao sao dessas duas
        # categorias. Entao nao e preciso usar a variavel v037)
        CensusData[v038 %in% c(0, 5), education := 1]
        
        # Se cursou alfabetizacao de adultos
        CensusData[v037 == 9, education := 1]
        
        gc()
        ### Primario incompleto
        #######################
        
        # Se esta frequentando a primeira serie do primario
        CensusData[v037 == 1, education := 2]
        
        # Se concluiu a primeira serie do primario
        CensusData[v037 == 2 & v038 == 1, education := 2]
        
        # Se concluiu a segunda serie do primario
        CensusData[v037 == 3 & v038 == 1, education := 2]
        
        # Se concluiu a terceira serie do primario
        CensusData[v037 == 4 & v038 == 1, education := 2]
        
        gc()
        ### Primario completo
        #######################
        
        # Se concluiu a quarta serie do primario
        CensusData[v037 == 5 & v038 == 1, education := 3]
        
        # Se concluiu a quinta/sexta serie do primario
        CensusData[v037 == 6 & v038 == 1, education := 3]
        
        # Se concluiu a classe de admissao do primário para o médio
        CensusData[v037 == 7 & v038 == 1, education := 3]
        
        # Se cursa alguma modalidade de ensino à distancia ou esta
        # estudando para os exames de madureza para obtencao do 
        # medio 1o ciclo
        CensusData[v037 == 8 & v038 == 2, education := 3]
        
        gc()
        ### Fundamental (ginasial) incompleto
        #######################
        
        # Se concluiu a primeira serie do medio 1o ciclo
        CensusData[v037 == 2 & v038 == 2, education := 4]
        
        # Se concluiu a segunda serie do medio 1o ciclo
        CensusData[v037 == 3 & v038 == 2, education := 4]
        
        # Se concluiu a terceira serie do medio 1o ciclo
        CensusData[v037 == 4 & v038 == 2, education := 4]
        
        gc()
        ### Fundamental (ginasial) completo
        #######################
        
        # Se concluiu a quarta serie do medio 1o ciclo
        CensusData[v037 == 5 & v038 == 2, education := 5]
        
        # Se concluiu a quinta/sexta serie do medio 1o ciclo
        CensusData[v037 == 6 & v038 == 2, education := 5]
        
        
        # Se cursa alguma modalidade de ensino à distancia ou esta
        # estudando para os exames de madureza para obtencao do 
        # medio 2o ciclo
        CensusData[v037 == 8 & v038 == 3, education := 5]
        
        gc()
        ### Medio (secundario/colegial) incompleto
        #######################
        
        # Se concluiu a primeira serie do medio 2o ciclo
        CensusData[v037 == 2 & v038 == 3, education := 6]
        
        # Se concluiu a segunda serie do medio 2o ciclo
        CensusData[v037 == 3 & v038 == 3, education := 6]
        
        
        gc()
        ### Medio (secundario/colegial) completo
        #######################
        
        # Se concluiu a terceira serie do medio 2o ciclo
        CensusData[v037 == 4 & v038 == 3, education := 7]
        
        #nao ha casos de 4a  ou 5a/6a serie no medio 2o ciclo
        
        # Se cursa a admissao/vestibular
        CensusData[v037 == 7 & v038 == 3, education := 7]
        
        ### Superior incompleto
        #######################
        
        # Se concluiu da primeira até a sexta serie do superior (mas nao sabemos se concluiu o curso)
        CensusData[v038 == 4 & v037 %in% 2:6 , education := 8]
        
        gc()
        ### Superior completo
        #######################
        
        # Se cursou superior e informou algum curso concluido com códigos entre 70 e 97 na v039
        # (códigos abaixo de 70 informam cursos de primário e médio, os códigos 98 e 99 se referepe a cursos
        # de grau indeterminado ou nenhum grau)
        CensusData[v038 == 4 & v039 %in% 70:97, education := 9]
        
        
        #education
        # 1 - Nenhum
        # 2 - Primario incompleto
        # 3 - Primario completo
        # 4 - Fundamental incompleto
        # 5 - Fundamental completo
        # 6 - Medio incompleto
        # 7 - Medio completo
        # 8 - Superior incompleto
        # 9 - Superior completo
        
        
        gc()
        
        CensusData
}
