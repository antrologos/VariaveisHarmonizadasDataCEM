harmonize_identification <- function(CensusData,
                                     year,
                                     delete_originals = T){
        
        
        library(data.table)
        library(tidyverse) 
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        gc(); Sys.sleep(1); gc()
        
        CensusData[ , year := year]
        
        if(year != 1960) {CensusData <- rename(CensusData, person_id = idpessoa)}
        
        if(year == 1960){
                
                CensusData <- CensusData %>%
                        mutate(wgt       = cem_wgt, 
                               person_id = cem_idindividuo,
                               hh_id     = cem_iddomicilio)
                
                CensusData = as.data.table(CensusData)
                
                if(delete_originals == T){
                        CensusData[ , cem_wgt         := NULL]
                        CensusData[ , cem_idindividuo := NULL]
                        CensusData[ , cem_iddomicilio := NULL]
                        gc()
                }
        }
        
        
        if(year == 1970){
                CensusData[ , wgt := v054] 
                CensusData[ , hh_id := iddomicilio]
                
                if(delete_originals == T){
                        CensusData[ , v054 := NULL]
                        CensusData[ , iddomicilio := NULL]
                        gc()
                }
        }
        
         
        if(year == 1980){
                CensusData[ , wgt := v604]
                CensusData[ , hh_id := v601]
                
                if(delete_originals == T){
                        CensusData[ , v604 := NULL]
                        CensusData[ , v601 := NULL]
                        gc()
                }
        }
        
        if(year == 1991){
                CensusData[ , wgt := v7301]
                CensusData[ , hh_id := v0102]
                
                if(delete_originals == T){
                        CensusData[ , v0102 := NULL]
                        CensusData[ , v7301 := NULL]
                        gc()
                }
        }
        
        
        if(year == 2000){
                CensusData[ , wgt := p001]
                CensusData[ , hh_id := v0300]
                
                if(delete_originals == T){
                        CensusData[ , p001 := NULL]
                        CensusData[ , v0300 := NULL]
                        gc()
                }
        }
        
        
        if(year == 2010){
                CensusData[ , wgt := v0010] 
                CensusData[ , hh_id := v0300]
                
                if(delete_originals == T){
                        CensusData[ , v0010 := NULL]
                        CensusData[ , v0300 := NULL]
                        gc()
                }
        }
        
        
        return(CensusData)
}
                             
