file_path <- "path/to/your/fusugast.txt"
file_path_result <- "path/to/your/fusugast_2024.rds"

column_widths <- c(4, 5, 5, 15, 5, 5, 12, 15, 13, 13, 13, 13, 13, 11)
column_names <- c(
  "ANOENC", "NUMERO", "CODIGO", "GASTO", "PORCENDES", "PORCENIMP", "CANTIDAD",
  "GASTMON", "GASTNOM1", "GASTNOM2", "GASTNOM3", "GASTNOM4", "GASTNOM5", "FACTOR"
)

data <- read.fwf(
  file = file_path,
  widths = column_widths,
  col.names = column_names,
  strip.white = TRUE
)

fields_2dec <- c("GASTO", "PORCENDES", "PORCENIMP", "CANTIDAD", "GASTMON", 
                 "GASTNOM1", "GASTNOM2", "GASTNOM3", "GASTNOM4", "GASTNOM5")
for (field in fields_2dec) {
  data[[field]] <- as.numeric(gsub(" ", "", data[[field]])) / 100
}

data$FACTOR <- as.numeric(gsub(" ", "", data$FACTOR)) / 1e6

saveRDS(data, file = file_path_result)

unique_codes <- sort(unique(data$CODIGO))
print(unique_codes)

GastosOnlyPets <- subset(data, CODIGO %in% c('09321', '09322', '09450', '12199'))

# Summarize GASTO by CODIGO
summary_gasto <- aggregate(GASTO ~ CODIGO, data = GastosOnlyPets, sum)

# View result
print(summary_gasto)
