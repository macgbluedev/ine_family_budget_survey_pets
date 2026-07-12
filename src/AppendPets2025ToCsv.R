# Añade los gastos de mascotas de 2025 a GastosSoloMascotas22a24.csv
# para que el dashboard (dashboard_mascotas.py) pueda visualizarlos.
#
# Reutiliza la misma lógica de selección/etiquetado que ExtractResultFromMicrodataEpf_2025.R,
# pero en lugar de generar los .xlsx, produce filas con el layout exacto del CSV combinado
# (ANOENC,NUMERO,CODIGO,CODIGO 4D,GASTO,CCAA,CAPROV,TAMAMU,TAMANO,TIPHOGAR1,TIPHOGAR7,INTERIN)
# y las añade al final del fichero.

suppressMessages({
  library(plyr)
  library(dplyr)
  library(readr)
})

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

pathGastos <- "./Inputs/2025/R/EPFgastos_2025.RData"
pathHogar <- "./Inputs/2025/R/EPFhogar_2025.RData"
pathCsv <- "./GastosSoloMascotas22a24.csv"

load(pathGastos)
GastosOnlyPets <- subset(df_micro, CODIGO %in% c('09321','09322','09450'))
GastosOnlyPets <- GastosOnlyPets %>% mutate(GastoEfectivo = GASTO * (1- PORCENDES/100) * (1- PORCENIMP/100))
GastosOnlyPets <- GastosOnlyPets %>% filter(GastoEfectivo > 0)

load(pathHogar)
HogarFiltered <- df_micro %>% select(NUMERO,CCAA,CAPROV,TAMAMU,TAMANO,TIPHOGAR1,TIPHOGAR7,INTERIN)
GastosOnlyPetsFiltered <- GastosOnlyPets %>% select(ANOENC,NUMERO,CODIGO,GASTO)

Pets2025 <- join(GastosOnlyPetsFiltered, HogarFiltered, by = "NUMERO", "left", "all")
Pets2025 <- rename_using_epf_labels(Pets2025)

# Layout exacto del CSV combinado: ANOENC,NUMERO,CODIGO,CODIGO 4D,GASTO,CCAA,CAPROV,TAMAMU,TAMANO,TIPHOGAR1,TIPHOGAR7,INTERIN
Pets2025 <- Pets2025 %>%
  mutate(
    ANOENC = as.integer(ANOENC),
    NUMERO = as.integer(NUMERO),
    CODIGO = as.integer(CODIGO),
    `CODIGO 4D` = as.integer(substr(as.character(CODIGO), 1, 3)),
    GASTO = as.numeric(GASTO)
  ) %>%
  select(ANOENC, NUMERO, CODIGO, `CODIGO 4D`, GASTO, CCAA, CAPROV, TAMAMU, TAMANO, TIPHOGAR1, TIPHOGAR7, INTERIN)

cat(sprintf("Filas 2025 a añadir: %d\n", nrow(Pets2025)))

# Append sin cabecera, con el mismo estilo de comillas (mínimo, solo si hace falta) que el resto del fichero
readr::write_csv(Pets2025, pathCsv, append = TRUE, col_names = FALSE)

cat("Listo. Nuevo total de filas en", pathCsv, ":\n")
cat(length(readLines(pathCsv)), "\n")
