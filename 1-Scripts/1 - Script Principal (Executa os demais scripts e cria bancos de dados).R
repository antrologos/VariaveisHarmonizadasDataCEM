#rm(list=ls());gc()
library(tidyverse)
library(stringr)
library(readxl)
library(data.table)
library(questionr)
library(descr)

########################################################################################################################
# Opcoes globais
options(scipen = 999)

# Aponta para a pasta onde se localizam os Censos Demograficos em formato CSV, baixados do Centro de Estudos da Metropole
# Cada Censo deve estar em uma pasta separada com o nome "Censo XXXX"
censo_dir         <- "Censos/"

# Arquivos disponíveis no repositório do GitHUB
auxiliares_dir    <- "~/Variáveis Harmonizadas - CEM/1-Scripts/Arquivos_auxiliares/"
harmonization_dir <- "~/Variáveis Harmonizadas - CEM/1-Scripts/Funcoes_de_harmonizacao/"

# Diretório para os arquivos de saída
output            <- "~/"

########################################################################################################################
# Carregando funcoes

harmonization_functions <- list.files(harmonization_dir, pattern = "_FUNCTION")
for(f in harmonization_functions){
        source(paste0(harmonization_dir,f))
}


########################################################################################################################
# Carregando arquivos auxiliares


# Lista de variaveis originais em cada Censo -- para abrir apenas vari?veis selecionadas
variaveis      <- fread(paste0(auxiliares_dir,"variaveis_CENSOS.csv"))


# Crosswalk para os nomes de variaveis -- para levar todas as variaveis para o padrao de nomes do CEM
nomes_variaveis <- read_xlsx(paste0(auxiliares_dir,"cem_harmonizacao_varNames.xlsx")) %>%
        filter(!is.na(tema))


########################################################################################################################
# Procedimentos de Harmonizacao

#y=1960
anos_censo = c(1960,1970,1980,1991,2000,2010)
for(y in anos_censo){

        print(y)
        file_person    = variaveis[year == y]$file_person

        selected_vars = variaveis[year == y] %>%
                select(-file_person,-file_household) %>%
                unlist() %>%
                paste(collapse=";") %>%
                str_split(pattern=";") %>%
                unlist()

        find_NA <- grep("NA",selected_vars)
        if(length(find_NA)>0){
                selected_vars <- selected_vars[-find_NA]
        }

        selected_vars <- selected_vars[grep("[[:alpha:]]",selected_vars)]
        selected_vars <- selected_vars[nchar(selected_vars)>0]
        selected_vars = c(selected_vars, toupper(selected_vars), tolower(selected_vars)) %>%
                unique()

        nome_censo <- paste0("c_",y)


        # Abrindo os bancos de dados de PESSOAS
        assign(x = nome_censo,
               value = fread(paste0(censo_dir,"Censo ",y,"/",file_person), select = c(selected_vars),    nrows = 30000000) %>%
                       rename_all(tolower)
        )
        gc()


        #############################################
        # RECODING

        gc()
        var_occ          = variaveis[year == y, occ] %>% tolower()
        var_sector       = variaveis[year == y, sector] %>% tolower()
        var_class_worker = variaveis[year == y, class_worker] %>% tolower()

        if(y == 1980) {
                var_class_worker = str_split(var_class_worker, pattern = ";") %>% unlist()
                var_occ          = str_split(var_occ, pattern = ";") %>% unlist()
        }

        # Renomeando variaveis
        assign(x = nome_censo,
               value = get(nome_censo) %>%
                       rename_("occ"      = eval(var_occ[1]),
                               "sector"   = eval(var_sector) ,
                               "pos_ocup" = eval(var_class_worker[1])) %>% # em 1980 eh preciso cuidado: a ordem das variaveis importa
                       mutate(occ = as.numeric(occ))
        )
        gc()

        # Fixing "ocupacao" e posicao na ocupacao" in 1980. from habital to current work
        if(y == 1980){
                assign(x = nome_censo,
                       value = get(nome_censo) %>%
                               rename_("ocup2"     = eval(var_occ[2]),
                                       "pos_ocup2" = eval(var_class_worker[2])) %>%
                               mutate(occ      = ifelse(!is.na(ocup2) & ocup2 != 0, ocup2, occ),
                                      pos_ocup = ifelse(!is.na(pos_ocup2) & pos_ocup2 != 0, pos_ocup2, pos_ocup)) %>%
                               select(-pos_ocup2, -ocup2)
                )
                gc()
        }


        # HARMONIZING...
        gc();Sys.sleep(1);gc()
        assign(x = nome_censo,
               value = get(nome_censo) %>%
                       harmonize_identification(year = y, delete_originals = T)
        )
        gc();Sys.sleep(1);gc()

        assign(x = nome_censo,
               value = get(nome_censo) %>%
                       harmonize_demographics(year = y,  delete_originals = T)
        )
        gc();Sys.sleep(1);gc()

        assign(x = nome_censo,
               value = get(nome_censo) %>%
                       harmonize_education(year = y, delete_originals = T)
        )
        gc();Sys.sleep(1);gc()

        assign(x = nome_censo,
               value = get(nome_censo) %>%
                       harmonize_economicActivity(year = y, delete_originals = T)
        )
        gc();Sys.sleep(1);gc()

        assign(x = nome_censo,
               value = get(nome_censo)[ ,class_worker := harmonize_classWorker(var_posOcup = pos_ocup,
                                                                               var_setor = sector,
                                                                               year = y,
                                                                               type = "censo")]
        )
        gc();Sys.sleep(1);gc()

        assign(x = nome_censo,
               value = get(nome_censo) %>%
                       harmonize_income(year = y)
        )
        gc();Sys.sleep(1);gc()


        assign(x = nome_censo,
               value = get(nome_censo) %>%
                       dummy_at_least_15hours_work(year = y)
        )
        gc();Sys.sleep(1);gc()


        assign(x = nome_censo,
               value = get(nome_censo) %>%
                       correct_classWorker_econActivity(year = y)
        )
        gc();Sys.sleep(1);gc()


        assign(x = nome_censo,
               value = get(nome_censo) %>%
                       harmonize_geography(year = y,
                                           crosswalks_folder = auxiliares_dir,
                                           delete_originals = T
                                           )
        )
        gc();Sys.sleep(1);gc()


        assign(x = nome_censo,
               value = get(nome_censo) %>%
                       select(-one_of(selected_vars,"pos_ocup"))
        )
        gc();Sys.sleep(1);gc()

        write_csv(get(nome_censo), path = paste0(output,nome_censo,".csv"), na = "")
        rm(list=nome_censo);gc()
}


########################################################################################################################
# Aplicando o Crosswalk para os nomes de variaveis e salvando novamente os bancos com os nomes padronizados

censos = list.files(path = output, pattern = "csv")

for(censo in censos){
        print(censo)
        censo_tmp <- fread(paste0(output,censo))
        gc();Sys.sleep(1);gc()
        censo_tmp <- censo_tmp %>%
                select(nomes_variaveis$variaveis)

        names(censo_tmp) <- nomes_variaveis$new_name
        write_csv(censo_tmp, path = paste0(output,censo), na = "")
        rm(censo_tmp)
        gc();Sys.sleep(1);gc()
}



