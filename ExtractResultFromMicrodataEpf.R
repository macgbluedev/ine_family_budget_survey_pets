#install.packages("haven")
#install.packages("dplyr")
#install.packages("plyr")
#install.packages("'xlsx") 

library(haven)
library(plyr)
library(dplyr)
library (xlsx)

rename_using_epf_labels <- function(hogar) {
    hogar <- (hogar %>% mutate(CCAA = replace(CCAA, CCAA == '01', "Andalucía"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '02', "Aragón"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '03', "Asturias, Principado de"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '04', "Balears, Illes"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '05', "Canarias"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '06',"Cantabria"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '07',"Castilla y León"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '08',"Castilla - La Mancha"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '09',"Cataluña"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '10',"Comunitat Valenciana"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '11',"Extremadura"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '12',"Galicia"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '13',"Madrid, Comunidad de"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '14',"Murcia, Región de"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '15',"Navarra, Comunidad Foral de"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '16',"País Vasco"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '17',"Rioja, La"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '18',"Ceuta"))
        %>% mutate(CCAA = replace(CCAA, CCAA == '19',"Melilla"))
    )

    hogar <- (hogar %>% mutate(CAPROV = replace(CAPROV, CAPROV == 1, "Sí")) 
        %>% mutate(CAPROV = replace(CAPROV, CAPROV == 6, "No"))
    )

    hogar <- (hogar %>% mutate(TAMAMU = replace(TAMAMU, TAMAMU == 1, "Municipio de 100.000 habitantes o más")) 
        %>% mutate(TAMAMU = replace(TAMAMU, TAMAMU == 2, "Municipio con 50.000 o más y menos 100.000 habitantes")) 
        %>% mutate(TAMAMU = replace(TAMAMU, TAMAMU == 3, "Municipio con 20.000 o más y menos de 50.000 habitantes")) 
        %>% mutate(TAMAMU = replace(TAMAMU, TAMAMU == 4, "Municipio con 10.000 o más y menos de 20.000 habitantes")) 
        %>% mutate(TAMAMU = replace(TAMAMU, TAMAMU == 5, "Municipio con menos de 10.000 habitantes"))
    )

    hogar <- (hogar %>% mutate(TAMANO = replace(TAMANO, TAMANO == 1, "Una persona")) 
        %>% mutate(TAMANO = replace(TAMANO, TAMANO == 2, "Dos personas")) 
        %>% mutate(TAMANO = replace(TAMANO, TAMANO == 3, "Tres personas")) 
        %>% mutate(TAMANO = replace(TAMANO, TAMANO == 4, "Cuatro personas")) 
        %>% mutate(TAMANO = replace(TAMANO, TAMANO == 5, "Cinco personas")) 
        %>% mutate(TAMANO = replace(TAMANO, TAMANO == 6, "Seis o más personas"))
    )

    hogar <- (hogar %>% mutate(TIPHOGAR1 = replace(TIPHOGAR1, TIPHOGAR1 == '01', "Una persona de 65 o más años")) 
        %>% mutate(TIPHOGAR1 = replace(TIPHOGAR1, TIPHOGAR1 == '02', "Una persona de 30 a 64 años")) 
        %>% mutate(TIPHOGAR1 = replace(TIPHOGAR1, TIPHOGAR1 == '03', "Una persona de menos de 30 años")) 
        %>% mutate(TIPHOGAR1 = replace(TIPHOGAR1, TIPHOGAR1 == '04', "Un adulto con niños menores de 16 años")) 
        %>% mutate(TIPHOGAR1 = replace(TIPHOGAR1, TIPHOGAR1 == '05', "Pareja sin hijos teniendo al menos uno de los miembros 65 años o más")) 
        %>% mutate(TIPHOGAR1 = replace(TIPHOGAR1, TIPHOGAR1 == '06', "Pareja sin hijos teniendo los dos miembros menos de 65 años")) 
        %>% mutate(TIPHOGAR1 = replace(TIPHOGAR1, TIPHOGAR1 == '07', "Pareja con un hijo menor de 16 años")) 
        %>% mutate(TIPHOGAR1 = replace(TIPHOGAR1, TIPHOGAR1 == '08', "Pareja con dos hijos menores de 16 años")) 
        %>% mutate(TIPHOGAR1 = replace(TIPHOGAR1, TIPHOGAR1 == '09', "Pareja con tres o más hijos menores de 16 años")) 
        %>% mutate(TIPHOGAR1 = replace(TIPHOGAR1, TIPHOGAR1 == '10', "Padre o madre solo, con al menos un hijo de 16 o más años")) 
        %>% mutate(TIPHOGAR1 = replace(TIPHOGAR1, TIPHOGAR1 == '11', "Pareja con al menos un hijo de 16 o más años")) 
        %>% mutate(TIPHOGAR1 = replace(TIPHOGAR1, TIPHOGAR1 == '12', "Otros Hogares"))
    )

    hogar <- (hogar %>% mutate(TIPHOGAR7 = replace(TIPHOGAR7, TIPHOGAR7 == '01', "Persona sola de menos de 65 años")) 
        %>% mutate(TIPHOGAR7 = replace(TIPHOGAR7, TIPHOGAR7 == '02', "Persona sola de 65 o más años")) 
        %>% mutate(TIPHOGAR7 = replace(TIPHOGAR7, TIPHOGAR7 == '03', "Pareja sin hijos")) 
        %>% mutate(TIPHOGAR7 = replace(TIPHOGAR7, TIPHOGAR7 == '04', "Pareja con un hijo")) 
        %>% mutate(TIPHOGAR7 = replace(TIPHOGAR7, TIPHOGAR7 == '05', "Pareja con dos hijos")) 
        %>% mutate(TIPHOGAR7 = replace(TIPHOGAR7, TIPHOGAR7 == '06', "Pareja con tres o mas hijos")) 
        %>% mutate(TIPHOGAR7 = replace(TIPHOGAR7, TIPHOGAR7 == '07', "Un adulto con hijos")) 
        %>% mutate(TIPHOGAR7 = replace(TIPHOGAR7, TIPHOGAR7 == '08', "Otros tipos de hogares")) 
    )

    hogar <- (hogar %>% mutate(INTERIN = replace(INTERIN, INTERIN == '01', "Menos de 500 €")) 
        %>% mutate(INTERIN = replace(INTERIN, INTERIN == '02', "De 500 a menos de 1000 €")) 
        %>% mutate(INTERIN = replace(INTERIN, INTERIN == '03', "De 1000 a menos de 1500 €")) 
        %>% mutate(INTERIN = replace(INTERIN, INTERIN == '04', "De 1500 a menos de 2000 €")) 
        %>% mutate(INTERIN = replace(INTERIN, INTERIN == '05', "De 2000 a menos de 2500 €")) 
        %>% mutate(INTERIN = replace(INTERIN, INTERIN == '06', "De 2500 a menos de 3000 €")) 
        %>% mutate(INTERIN = replace(INTERIN, INTERIN == '07', "De 3000 a menos de 5000 €")) 
        %>% mutate(INTERIN = replace(INTERIN, INTERIN == '08', "De 5000 a menos de 7000 €")) 
        %>% mutate(INTERIN = replace(INTERIN, INTERIN == '09', "De 7000 a menos de 9000 €")) 
        %>% mutate(INTERIN = replace(INTERIN, INTERIN == '10', "9000 o más €"))
    )
    
    return(hogar)
}

pathGastos <- "Inputs\\2024\\R\\EPFgastos_2024.RData"
pathHogar <- "Inputs\\2024\\R\\EPFhogar_2024.RData"

load(pathGastos)
Gastos <- Microdatos
MetadataGastos <- Metadatos

GastosOnlyPets <- subset(Microdatos, CODIGO %in% c('09321','09322','09450'))

load(pathHogar)
Hogar <- Microdatos
MetadataHogar <- Metadatos

HogarFiltered <- Hogar %>% select(NUMERO,CCAA,CAPROV,TAMAMU,TAMANO,TIPHOGAR1,TIPHOGAR7,INTERIN)
GastosOnlyPetsFiltered <- GastosOnlyPets %>% select(ANOENC,NUMERO,CODIGO,GASTO)

# Detail
GastoOnlyPetsHogar <- join(GastosOnlyPetsFiltered, HogarFiltered, by = "NUMERO","left","all")

GastoOnlyPetsHogar <- rename_using_epf_labels(GastoOnlyPetsHogar)

write.xlsx2 (GastoOnlyPetsHogar, file = "EPFOnlyPetsResult.xlsx", SheetName = "Result",col.names = TRUE, row.names = TRUE, append = FALSE)

# Proportion
GastosOnlyPetsFilterByPetAcquisition <- subset(GastosOnlyPets, CODIGO == "09321") %>% select(NUMERO,GASTO)
GastosOnlyPetsFilterByPetFoodAndProducts <- subset(GastosOnlyPets, CODIGO == "09322") %>% select(NUMERO,GASTO)
GastosOnlyPetsFilterByPetVeterinaryServices <- subset(GastosOnlyPets, CODIGO == "09450") %>% select(NUMERO,GASTO)

HogarFilteredByExpenseCode <- HogarFiltered

HogarFilteredByExpenseCode <- join(HogarFilteredByExpenseCode, GastosOnlyPetsFilterByPetAcquisition, by = "NUMERO","left","all")
HogarFilteredByExpenseCode <- join(HogarFilteredByExpenseCode, GastosOnlyPetsFilterByPetFoodAndProducts, by = "NUMERO","left","all")
HogarFilteredByExpenseCode <- join(HogarFilteredByExpenseCode, GastosOnlyPetsFilterByPetVeterinaryServices, by = "NUMERO","left","all")
HogarFilteredByExpenseCode[is.na(HogarFilteredByExpenseCode)] <- 0

names(HogarFilteredByExpenseCode)[9] = "PetAcquisition"
names(HogarFilteredByExpenseCode)[10] = "PetFoodAndProducts"
names(HogarFilteredByExpenseCode)[11] = "PetVeterinaryServices"

HogarFilteredByExpenseCode$Clasificacion <- "Hogares sin mascotas"

HogarFilteredByExpenseCode<- mutate(HogarFilteredByExpenseCode,Clasificacion = ifelse( (PetAcquisition == 0 | PetAcquisition > 0) 
                                                                                & PetFoodAndProducts > 0 
                                                                                & PetVeterinaryServices == 0,
                                                                                "Hogares con mascotas - Productos", 
                                                                                Clasificacion)
)

HogarFilteredByExpenseCode<- mutate(HogarFilteredByExpenseCode,Clasificacion = ifelse( (PetAcquisition == 0 | PetAcquisition > 0) 
                                                                                & PetFoodAndProducts == 0 
                                                                                & PetVeterinaryServices > 0,
                                                                                "Hogares con mascotas - Servicios", 
                                                                                Clasificacion)
)

HogarFilteredByExpenseCode<- mutate(HogarFilteredByExpenseCode,Clasificacion = ifelse( (PetAcquisition == 0 | PetAcquisition > 0) 
                                                                                & PetFoodAndProducts > 0 
                                                                                & PetVeterinaryServices > 0,
                                                                               "Hogares con mascotas - Productos y Servicios",
                                                                               Clasificacion)
)

HogarFilteredByExpenseCode <- rename_using_epf_labels(HogarFilteredByExpenseCode)

write.xlsx2 (HogarFilteredByExpenseCode, file = "EPFOnlyPetsProportionResult.xlsx", SheetName = "Result",col.names = TRUE, row.names = TRUE, append = FALSE)