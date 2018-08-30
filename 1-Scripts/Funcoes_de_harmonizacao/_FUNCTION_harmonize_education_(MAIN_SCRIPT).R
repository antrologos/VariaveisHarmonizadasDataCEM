
harmonize_education <- function(CensusData, 
                                year,
                                #scripts_folder = "E:/Dropbox-Ro/Dropbox/Rogerio/Artigos/1 - Em faccao/000 - Padronização de Ocupações e Setores/Rogerio/Harmonization Scripts - INCOMPLETE",
                                harmonize_demographics_first = F,
                                delete_originals = T){
        
        library(data.table)
        library(tidyverse)
        
        #old_wd <- getwd()
        #setwd(scripts_folder)
        gc()
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        gc(); Sys.sleep(1); gc()
        
        
        if(harmonize_demographics_first == T){
                #source(paste0(scripts_folder,"/_FUNCTION_harmonize_demographics.R"))
                CensusData <- CensusData %>%
                        harmonize_demographics(year = year) %>%
                        as.data.table()
                gc(); Sys.sleep(1); gc()
        }
        
        CensusData <- build_harmonized_literacy_attendance(CensusData = CensusData, year = year)
        gc(); Sys.sleep(1); gc()
        
        
        call <- paste0("build_education_attainment_",year,"(CensusData)")
        CensusData <- eval(parse(text = call))
        gc() 
        
        CensusData[ , education_tmp1 := NULL]
        CensusData[ , education_tmp2 := NULL]
        CensusData[ , education_tmp3 := NULL]
        
        CensusData[ , graus  := NULL]
        CensusData[ , series := NULL]
        
        
        if(delete_originals == T){
                
                if(year == 1960){
                        var_to_exclude <- c("v204","v211","v212","v213","v214")
                }
                
                if(year == 1970){
                        var_to_exclude <- c("v035","v036","v037","v038","v039")
                }
                
                if(year == 1980){
                        var_to_exclude <- c("v519", "v520", "v521", "v522", "v523", "v524", "v525")
                }
                
                if(year == 1991){
                        var_to_exclude <- c("v0323", "v0324","v0325","v0326","v0327","v0328","v0329")
                }
                
                if(year == 2000){
                        var_to_exclude <- c("v0428", "v0429", "v0430", "v0431", "v0432", "v0433", "v0434", "v4752")
                }
                
                if(year == 2010){
                        var_to_exclude <- c("v0627", "v0628", "v0629", "v0630", "v0631", "v0632", "v0633", "v0634", "v0636")
                }
                
                CensusData[, (var_to_exclude) := NULL]
                
                gc();Sys.sleep(1);gc()
                
                
        }
        
        
        # AJUSTE POR IDADE
        CensusData[age <= 4,  literacy1 := NA]
        CensusData[age <= 4,  schoolattnd := NA]
        CensusData[age <= 4,  levelattnd := NA]
        CensusData[age <= 4,  education := NA]

        gc()
        
        # education
        # 1 "None"  
        # 2 "Primary, incomplete" 
        # 3 "Primary, complete" 
        # 4 "Middle school, incomplete" 
        # 5 "Middle school, complete" 
        # 6 "High School, incomplete" 
        # 7 "High School, complete" 
        # 8 "Higher Education, incomplete" 
        # 9 "Higher Education, complete" 
        # 999 "Unknown", replace
        
        #levelattnd
        # 1 "Regular Primary/Middle school" 
        # 2 "Regular High chool"
        # 3 "Higher education" 
        # 9 "Other", replace
        
        
        #setwd(old_wd)
        
        return(CensusData)
        
}