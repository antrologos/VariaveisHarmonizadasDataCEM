build_harmonized_literacy_attendance <- function(CensusData, 
                                                 year){
        
        
        #==============================================================================
        # 1960
        
        if(year == 1960){
                
                # Literacy
                CensusData[, literacy1 := as.numeric(NA)]
                CensusData[v211 %in% c(0, 1), literacy1 := 1]
                CensusData[v211 %in% c(2, 3), literacy1 := 0]
                
                # School Attendance
                CensusData[, schoolattnd := as.numeric(NA)]
                CensusData[v211 %in% c(1, 3), schoolattnd := 0]
                CensusData[v211 %in% c(0, 2), schoolattnd := 1]
                
                
                # Level of attendance
                CensusData[v213 %in% c(0, 2, 3 ), levelattnd := 1]
                CensusData[v213 %in% c(4), levelattnd := 2]
                CensusData[v213 %in% c(5), levelattnd := 3]
                CensusData[v213 %in% c(6), levelattnd := 9]
                CensusData[v213 %in% c(1), levelattnd := NA]
        }
        
        #==============================================================================
        # 1970
        
        if(year == 1970){
                
                # Literacy
                CensusData[, literacy1 := as.numeric(NA)]
                CensusData[v035 == 2, literacy1 := 0]
                CensusData[v035 == 1, literacy1 := 1]
                
                # School Attendance
                CensusData[, schoolattnd := as.numeric(NA)]
                CensusData[v036 == 2, schoolattnd := 0]
                CensusData[v036 == 1, schoolattnd := 1]
                
                
                # Level of attendance
                CensusData[v038 %in% c(1, 2), levelattnd := 1]
                CensusData[v038 %in% c(3), levelattnd := 2]
                CensusData[v038 %in% c(4), levelattnd := 3]
                CensusData[v038 %in% c(9), levelattnd := 9]
                CensusData[v038 %in% c(5), levelattnd := NA]
                CensusData[v037 %in% c(8 ,9), levelattnd := 9]
                
        }
        
        #==============================================================================
        # 1980
        
        if(year == 1980){
                
                # Literacy
                CensusData[, literacy1 := as.numeric(NA)]
                CensusData[v519 %in% 2, literacy1 := 1]
                CensusData[v519 %in% c(4, 6), literacy1 := 0]
                
                # School Attendance
                CensusData[, schoolattnd := 0]
                CensusData[v521 != 0, schoolattnd := 1]
                CensusData[v522 %in% c(2:4, 7, 8), schoolattnd := 1] # esta incluindo vestibular (opcao 7)
                
                # Level of attendance
                CensusData[v521 %in% c(1, 2, 3), levelattnd := 1]
                CensusData[v521 %in% c(4, 5), levelattnd := 2]
                CensusData[v521 %in% c(8), levelattnd := 3]
                CensusData[v521 %in% c(6, 7, 9), levelattnd := 9]
                
                CensusData[v522 %in% c(8), levelattnd := 3]
                CensusData[v522 %in% c(1:7 ,9), levelattnd := 9]
        }
        
        #==============================================================================
        # 1991
        
        if(year == 1991){
                
                # Literacy
                CensusData[, literacy1 := as.numeric(NA)]
                CensusData[v0323 == 2, literacy1 := 0]
                CensusData[v0323 == 1, literacy1 := 1]
                
                # School Attendance
                CensusData[, schoolattnd := 0]
                CensusData[v0325 != 0, schoolattnd := 1]
                CensusData[v0326 %in% c(2:4, 5, 6), schoolattnd := 1] # esta incluindo vestibular (opcao 5)
                
                # Level of attendance
                CensusData[v0325 %in% c(1),    levelattnd := 1]
                CensusData[v0325 %in% c(2),    levelattnd := 2]
                CensusData[v0325 %in% c(3),    levelattnd := 3]
                CensusData[v0325 %in% c(4, 5), levelattnd := 9]
                
                CensusData[v0326 %in% c(6),   levelattnd := 3]
                CensusData[v0326 %in% c(1:5), levelattnd := 9]
        }
        
        #==============================================================================
        # 2000
        
        if(year == 2000){
                
                # Literacy
                CensusData[, literacy1 := as.numeric(NA)]
                CensusData[v0428 == 2, literacy1 := 0]
                CensusData[v0428 == 1, literacy1 := 1]
                
                # School Attendance
                CensusData[, schoolattnd := as.numeric(NA)]
                CensusData[v0429 %in% c(3, 4), schoolattnd := 0]
                CensusData[v0430<=3 , schoolattnd := 0]
                #CensusData[v0430==11 , schoolattnd := 0]        #desativar essa linha faz incluir o vestibular
                CensusData[v0429 %in% c(1, 2), schoolattnd := 1] 
                
                
                # Level of attendance
                CensusData[v0430 %in% c(5),               levelattnd := 1]
                CensusData[v0430 %in% c(8),               levelattnd := 2]
                CensusData[v0430 %in% c(12, 13),          levelattnd := 3]
                CensusData[v0430 %in% c(1:4, 6, 7, 9:11), levelattnd := 9]
                
        }
        
        #==============================================================================
        # 2010
        
        if(year == 2010){
                 
                # Literacy
                CensusData[, literacy1 := as.numeric(NA)]
                CensusData[v0627 == 2, literacy1 := 0]
                CensusData[v0627 == 1, literacy1 := 1]
                
                # School Attendance
                CensusData[, schoolattnd := as.numeric(NA)]
                CensusData[v0628 %in% c(3, 4), schoolattnd := 0]
                CensusData[v0628 %in% c(1, 2), schoolattnd := 1]
                CensusData[v0629<=3 , schoolattnd := 0]
                
                
                # Level of attendance
                CensusData[v0629 %in% c(5),         levelattnd := 1]
                CensusData[v0629 %in% c(7),         levelattnd := 2]
                CensusData[v0629 %in% c(9:12),      levelattnd := 3]
                CensusData[v0629 %in% c(1:4, 6, 8), levelattnd := 9]
        }
        
        CensusData

}







