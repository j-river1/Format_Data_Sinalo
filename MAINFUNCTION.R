source("Format_Chiapas.R")

mainDir <- getwd()
dir.create(file.path(mainDir, "Data_Format"), showWarnings = FALSE)
dir.create(file.path(mainDir, "Data_Original"), showWarnings = FALSE)

source("Format_Chiapas.R")

format_chiapas(list.files(path="./Data_Original"), variables = c("HR", "Pp", "RadG", "Tmax","Tmin"))
