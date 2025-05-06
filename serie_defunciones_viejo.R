## Carga de paquetes
library(tidyverse)
library(readxl)
library(readr)

##### BASES DE FALLE TOTALES ################
##### Defino los nombres de los archivos ----
archivos_ARG <- c("ARG_2005_tot.xlsx","ARG_2006_tot.xlsx","ARG_2007_tot.xlsx","ARG_2008_tot.xlsx",
                      "ARG_2009_tot.xlsx","ARG_2010_tot.xlsx","ARG_2011_tot.xlsx","ARG_2012_tot.xlsx",
                      "ARG_2013_tot.xlsx","ARG_2014_tot.xlsx","ARG_2015_tot.xlsx","ARG_2016_tot.xlsx",
                      "ARG_2017_tot.xlsx","ARG_2018_tot.xlsx","ARG_2019_tot.xlsx","ARG_2020_tot.xlsx",
                      "ARG_2021_tot.xlsx","ARG_2022_tot.xlsx")
archivos_CABA <- c("CABA_2005_tot.xlsx","CABA_2006_tot.xlsx","CABA_2007_tot.xlsx","CABA_2008_tot.xlsx",
                  "CABA_2009_tot.xlsx","CABA_2010_tot.xlsx","CABA_2011_tot.xlsx","CABA_2012_tot.xlsx",
                  "CABA_2013_tot.xlsx","CABA_2014_tot.xlsx","CABA_2015_tot.xlsx","CABA_2016_tot.xlsx",
                  "CABA_2017_tot.xlsx","CABA_2018_tot.xlsx","CABA_2019_tot.xlsx","CABA_2020_tot.xlsx",
                  "CABA_2021_tot.xlsx","CABA_2022_tot.xlsx")
archivos_PBA <- c("PBA_2005_tot.xlsx","PBA_2006_tot.xlsx","PBA_2007_tot.xlsx","PBA_2008_tot.xlsx",
                  "PBA_2009_tot.xlsx","PBA_2010_tot.xlsx","PBA_2011_tot.xlsx","PBA_2012_tot.xlsx",
                  "PBA_2013_tot.xlsx","PBA_2014_tot.xlsx","PBA_2015_tot.xlsx","PBA_2016_tot.xlsx",
                  "PBA_2017_tot.xlsx","PBA_2018_tot.xlsx","PBA_2019_tot.xlsx","PBA_2020_tot.xlsx",
                  "PBA_2021_tot.xlsx","PBA_2022_tot.xlsx")
archivos_CBA <- c("CBA_2005_tot.xlsx","CBA_2006_tot.xlsx","CBA_2007_tot.xlsx","CBA_2008_tot.xlsx",
                  "CBA_2009_tot.xlsx","CBA_2010_tot.xlsx","CBA_2011_tot.xlsx","CBA_2012_tot.xlsx",
                  "CBA_2013_tot.xlsx","CBA_2014_tot.xlsx","CBA_2015_tot.xlsx","CBA_2016_tot.xlsx",
                  "CBA_2017_tot.xlsx","CBA_2018_tot.xlsx","CBA_2019_tot.xlsx","CBA_2020_tot.xlsx",
                  "CBA_2021_tot.xlsx","CBA_2022_tot.xlsx")
archivos_CAT <- c("CAT_2005_tot.xlsx","CAT_2006_tot.xlsx","CAT_2007_tot.xlsx","CAT_2008_tot.xlsx",
                  "CAT_2009_tot.xlsx","CAT_2010_tot.xlsx","CAT_2011_tot.xlsx","CAT_2012_tot.xlsx",
                  "CAT_2013_tot.xlsx","CAT_2014_tot.xlsx","CAT_2015_tot.xlsx","CAT_2016_tot.xlsx",
                  "CAT_2017_tot.xlsx","CAT_2018_tot.xlsx","CAT_2019_tot.xlsx","CAT_2020_tot.xlsx",
                  "CAT_2021_tot.xlsx","CAT_2022_tot.xlsx")
archivos_CHA <- c("CHA_2005_tot.xlsx","CHA_2006_tot.xlsx","CHA_2007_tot.xlsx","CHA_2008_tot.xlsx",
                  "CHA_2009_tot.xlsx","CHA_2010_tot.xlsx","CHA_2011_tot.xlsx","CHA_2012_tot.xlsx",
                  "CHA_2013_tot.xlsx","CHA_2014_tot.xlsx","CHA_2015_tot.xlsx","CHA_2016_tot.xlsx",
                  "CHA_2017_tot.xlsx","CHA_2018_tot.xlsx","CHA_2019_tot.xlsx","CHA_2020_tot.xlsx",
                  "CHA_2021_tot.xlsx","CHA_2022_tot.xlsx")
archivos_CHU <- c("CHU_2005_tot.xlsx","CHU_2006_tot.xlsx","CHU_2007_tot.xlsx","CHU_2008_tot.xlsx",
                  "CHU_2009_tot.xlsx","CHU_2010_tot.xlsx","CHU_2011_tot.xlsx","CHU_2012_tot.xlsx",
                  "CHU_2013_tot.xlsx","CHU_2014_tot.xlsx","CHU_2015_tot.xlsx","CHU_2016_tot.xlsx",
                  "CHU_2017_tot.xlsx","CHU_2018_tot.xlsx","CHU_2019_tot.xlsx","CHU_2020_tot.xlsx",
                  "CHU_2021_tot.xlsx","CHU_2022_tot.xlsx")
archivos_COR <- c("COR_2005_tot.xlsx","COR_2006_tot.xlsx","COR_2007_tot.xlsx","COR_2008_tot.xlsx",
                  "COR_2009_tot.xlsx","COR_2010_tot.xlsx","COR_2011_tot.xlsx","COR_2012_tot.xlsx",
                  "COR_2013_tot.xlsx","COR_2014_tot.xlsx","COR_2015_tot.xlsx","COR_2016_tot.xlsx",
                  "COR_2017_tot.xlsx","COR_2018_tot.xlsx","COR_2019_tot.xlsx","COR_2020_tot.xlsx",
                  "COR_2021_tot.xlsx","COR_2022_tot.xlsx")
archivos_ER <- c("ER_2005_tot.xlsx","ER_2006_tot.xlsx","ER_2007_tot.xlsx","ER_2008_tot.xlsx",
                  "ER_2009_tot.xlsx","ER_2010_tot.xlsx","ER_2011_tot.xlsx","ER_2012_tot.xlsx",
                  "ER_2013_tot.xlsx","ER_2014_tot.xlsx","ER_2015_tot.xlsx","ER_2016_tot.xlsx",
                  "ER_2017_tot.xlsx","ER_2018_tot.xlsx","ER_2019_tot.xlsx","ER_2020_tot.xlsx",
                  "ER_2021_tot.xlsx","ER_2022_tot.xlsx")
archivos_FOR <- c("FOR_2005_tot.xlsx","FOR_2006_tot.xlsx","FOR_2007_tot.xlsx","FOR_2008_tot.xlsx",
                  "FOR_2009_tot.xlsx","FOR_2010_tot.xlsx","FOR_2011_tot.xlsx","FOR_2012_tot.xlsx",
                  "FOR_2013_tot.xlsx","FOR_2014_tot.xlsx","FOR_2015_tot.xlsx","FOR_2016_tot.xlsx",
                  "FOR_2017_tot.xlsx","FOR_2018_tot.xlsx","FOR_2019_tot.xlsx","FOR_2020_tot.xlsx",
                  "FOR_2021_tot.xlsx","FOR_2022_tot.xlsx")
archivos_JUJ <- c("JUJ_2005_tot.xlsx","JUJ_2006_tot.xlsx","JUJ_2007_tot.xlsx","JUJ_2008_tot.xlsx",
                  "JUJ_2009_tot.xlsx","JUJ_2010_tot.xlsx","JUJ_2011_tot.xlsx","JUJ_2012_tot.xlsx",
                  "JUJ_2013_tot.xlsx","JUJ_2014_tot.xlsx","JUJ_2015_tot.xlsx","JUJ_2016_tot.xlsx",
                  "JUJ_2017_tot.xlsx","JUJ_2018_tot.xlsx","JUJ_2019_tot.xlsx","JUJ_2020_tot.xlsx",
                  "JUJ_2021_tot.xlsx","JUJ_2022_tot.xlsx")
archivos_LAP <- c("LAP_2005_tot.xlsx","LAP_2006_tot.xlsx","LAP_2007_tot.xlsx","LAP_2008_tot.xlsx",
                  "LAP_2009_tot.xlsx","LAP_2010_tot.xlsx","LAP_2011_tot.xlsx","LAP_2012_tot.xlsx",
                  "LAP_2013_tot.xlsx","LAP_2014_tot.xlsx","LAP_2015_tot.xlsx","LAP_2016_tot.xlsx",
                  "LAP_2017_tot.xlsx","LAP_2018_tot.xlsx","LAP_2019_tot.xlsx","LAP_2020_tot.xlsx",
                  "LAP_2021_tot.xlsx","LAP_2022_tot.xlsx")
archivos_LAR <- c("LAR_2005_tot.xlsx","LAR_2006_tot.xlsx","LAR_2007_tot.xlsx","LAR_2008_tot.xlsx",
                  "LAR_2009_tot.xlsx","LAR_2010_tot.xlsx","LAR_2011_tot.xlsx","LAR_2012_tot.xlsx",
                  "LAR_2013_tot.xlsx","LAR_2014_tot.xlsx","LAR_2015_tot.xlsx","LAR_2016_tot.xlsx",
                  "LAR_2017_tot.xlsx","LAR_2018_tot.xlsx","LAR_2019_tot.xlsx","LAR_2020_tot.xlsx",
                  "LAR_2021_tot.xlsx","LAR_2022_tot.xlsx")
archivos_MEN <- c("MEN_2005_tot.xlsx","MEN_2006_tot.xlsx","MEN_2007_tot.xlsx","MEN_2008_tot.xlsx",
                  "MEN_2009_tot.xlsx","MEN_2010_tot.xlsx","MEN_2011_tot.xlsx","MEN_2012_tot.xlsx",
                  "MEN_2013_tot.xlsx","MEN_2014_tot.xlsx","MEN_2015_tot.xlsx","MEN_2016_tot.xlsx",
                  "MEN_2017_tot.xlsx","MEN_2018_tot.xlsx","MEN_2019_tot.xlsx","MEN_2020_tot.xlsx",
                  "MEN_2021_tot.xlsx","MEN_2022_tot.xlsx")
archivos_MIS <- c("MIS_2005_tot.xlsx","MIS_2006_tot.xlsx","MIS_2007_tot.xlsx","MIS_2008_tot.xlsx",
                  "MIS_2009_tot.xlsx","MIS_2010_tot.xlsx","MIS_2011_tot.xlsx","MIS_2012_tot.xlsx",
                  "MIS_2013_tot.xlsx","MIS_2014_tot.xlsx","MIS_2015_tot.xlsx","MIS_2016_tot.xlsx",
                  "MIS_2017_tot.xlsx","MIS_2018_tot.xlsx","MIS_2019_tot.xlsx","MIS_2020_tot.xlsx",
                  "MIS_2021_tot.xlsx","MIS_2022_tot.xlsx")
archivos_NQN <- c("NQN_2005_tot.xlsx","NQN_2006_tot.xlsx","NQN_2007_tot.xlsx","NQN_2008_tot.xlsx",
                  "NQN_2009_tot.xlsx","NQN_2010_tot.xlsx","NQN_2011_tot.xlsx","NQN_2012_tot.xlsx",
                  "NQN_2013_tot.xlsx","NQN_2014_tot.xlsx","NQN_2015_tot.xlsx","NQN_2016_tot.xlsx",
                  "NQN_2017_tot.xlsx","NQN_2018_tot.xlsx","NQN_2019_tot.xlsx","NQN_2020_tot.xlsx",
                  "NQN_2021_tot.xlsx","NQN_2022_tot.xlsx")
archivos_RN <- c("RN_2005_tot.xlsx","RN_2006_tot.xlsx","RN_2007_tot.xlsx","RN_2008_tot.xlsx",
                  "RN_2009_tot.xlsx","RN_2010_tot.xlsx","RN_2011_tot.xlsx","RN_2012_tot.xlsx",
                  "RN_2013_tot.xlsx","RN_2014_tot.xlsx","RN_2015_tot.xlsx","RN_2016_tot.xlsx",
                  "RN_2017_tot.xlsx","RN_2018_tot.xlsx","RN_2019_tot.xlsx","RN_2020_tot.xlsx",
                  "RN_2021_tot.xlsx","RN_2022_tot.xlsx")
archivos_SAL <- c("SAL_2005_tot.xlsx","SAL_2006_tot.xlsx","SAL_2007_tot.xlsx","SAL_2008_tot.xlsx",
                  "SAL_2009_tot.xlsx","SAL_2010_tot.xlsx","SAL_2011_tot.xlsx","SAL_2012_tot.xlsx",
                  "SAL_2013_tot.xlsx","SAL_2014_tot.xlsx","SAL_2015_tot.xlsx","SAL_2016_tot.xlsx",
                  "SAL_2017_tot.xlsx","SAL_2018_tot.xlsx","SAL_2019_tot.xlsx","SAL_2020_tot.xlsx",
                  "SAL_2021_tot.xlsx","SAL_2022_tot.xlsx")
archivos_SCR <- c("SCR_2005_tot.xlsx","SCR_2006_tot.xlsx","SCR_2007_tot.xlsx","SCR_2008_tot.xlsx",
                  "SCR_2009_tot.xlsx","SCR_2010_tot.xlsx","SCR_2011_tot.xlsx","SCR_2012_tot.xlsx",
                  "SCR_2013_tot.xlsx","SCR_2014_tot.xlsx","SCR_2015_tot.xlsx","SCR_2016_tot.xlsx",
                  "SCR_2017_tot.xlsx","SCR_2018_tot.xlsx","SCR_2019_tot.xlsx","SCR_2020_tot.xlsx",
                  "SCR_2021_tot.xlsx","SCR_2022_tot.xlsx")
archivos_SFE <- c("SFE_2005_tot.xlsx","SFE_2006_tot.xlsx","SFE_2007_tot.xlsx","SFE_2008_tot.xlsx",
                  "SFE_2009_tot.xlsx","SFE_2010_tot.xlsx","SFE_2011_tot.xlsx","SFE_2012_tot.xlsx",
                  "SFE_2013_tot.xlsx","SFE_2014_tot.xlsx","SFE_2015_tot.xlsx","SFE_2016_tot.xlsx",
                  "SFE_2017_tot.xlsx","SFE_2018_tot.xlsx","SFE_2019_tot.xlsx","SFE_2020_tot.xlsx",
                  "SFE_2021_tot.xlsx","SFE_2022_tot.xlsx")
archivos_SGO <- c("SGO_2005_tot.xlsx","SGO_2006_tot.xlsx","SGO_2007_tot.xlsx","SGO_2008_tot.xlsx",
                  "SGO_2009_tot.xlsx","SGO_2010_tot.xlsx","SGO_2011_tot.xlsx","SGO_2012_tot.xlsx",
                  "SGO_2013_tot.xlsx","SGO_2014_tot.xlsx","SGO_2015_tot.xlsx","SGO_2016_tot.xlsx",
                  "SGO_2017_tot.xlsx","SGO_2018_tot.xlsx","SGO_2019_tot.xlsx","SGO_2020_tot.xlsx",
                  "SGO_2021_tot.xlsx","SGO_2022_tot.xlsx")
archivos_SJN <- c("SJN_2005_tot.xlsx","SJN_2006_tot.xlsx","SJN_2007_tot.xlsx","SJN_2008_tot.xlsx",
                  "SJN_2009_tot.xlsx","SJN_2010_tot.xlsx","SJN_2011_tot.xlsx","SJN_2012_tot.xlsx",
                  "SJN_2013_tot.xlsx","SJN_2014_tot.xlsx","SJN_2015_tot.xlsx","SJN_2016_tot.xlsx",
                  "SJN_2017_tot.xlsx","SJN_2018_tot.xlsx","SJN_2019_tot.xlsx","SJN_2020_tot.xlsx",
                  "SJN_2021_tot.xlsx","SJN_2022_tot.xlsx")
archivos_SLU <- c("SLU_2005_tot.xlsx","SLU_2006_tot.xlsx","SLU_2007_tot.xlsx","SLU_2008_tot.xlsx",
                  "SLU_2009_tot.xlsx","SLU_2010_tot.xlsx","SLU_2011_tot.xlsx","SLU_2012_tot.xlsx",
                  "SLU_2013_tot.xlsx","SLU_2014_tot.xlsx","SLU_2015_tot.xlsx","SLU_2016_tot.xlsx",
                  "SLU_2017_tot.xlsx","SLU_2018_tot.xlsx","SLU_2019_tot.xlsx","SLU_2020_tot.xlsx",
                  "SLU_2021_tot.xlsx","SLU_2022_tot.xlsx")
archivos_TDF <- c("TDF_2005_tot.xlsx","TDF_2006_tot.xlsx","TDF_2007_tot.xlsx","TDF_2008_tot.xlsx",
                  "TDF_2009_tot.xlsx","TDF_2010_tot.xlsx","TDF_2011_tot.xlsx","TDF_2012_tot.xlsx",
                  "TDF_2013_tot.xlsx","TDF_2014_tot.xlsx","TDF_2015_tot.xlsx","TDF_2016_tot.xlsx",
                  "TDF_2017_tot.xlsx","TDF_2018_tot.xlsx","TDF_2019_tot.xlsx","TDF_2020_tot.xlsx",
                  "TDF_2021_tot.xlsx","TDF_2022_tot.xlsx")
archivos_TUC <- c("TUC_2005_tot.xlsx","TUC_2006_tot.xlsx","TUC_2007_tot.xlsx","TUC_2008_tot.xlsx",
                  "TUC_2009_tot.xlsx","TUC_2010_tot.xlsx","TUC_2011_tot.xlsx","TUC_2012_tot.xlsx",
                  "TUC_2013_tot.xlsx","TUC_2014_tot.xlsx","TUC_2015_tot.xlsx","TUC_2016_tot.xlsx",
                  "TUC_2017_tot.xlsx","TUC_2018_tot.xlsx","TUC_2019_tot.xlsx","TUC_2020_tot.xlsx",
                  "TUC_2021_tot.xlsx","TUC_2022_tot.xlsx")


nombres_TOT <- c(archivos_ARG, archivos_CABA, archivos_PBA, archivos_CAT, archivos_CBA,
                 archivos_CHA, archivos_CHU,
                 archivos_COR, archivos_ER, archivos_FOR, archivos_JUJ, archivos_LAP, archivos_LAR,
                 archivos_MEN, archivos_MIS, archivos_NQN, archivos_RN, archivos_SAL, archivos_SCR,
                 archivos_SFE, archivos_SGO, archivos_SJN, archivos_SLU, archivos_TDF, archivos_TUC)

##### Leo los archivos y asignar a objetos en R ----

# Carpeta de destino
carpeta_destino <- "C:/Users/Mica Gauto/Desktop/INE/INVESTIGACIÓN/DIABETES/CARGa de enfermedad/Procesamiento R/Bases_mort/Falle_quinquenales/tot"

## Armo listas para almacenar datos, por jurisdicción y totales
datos_ARG <- list() #ARG

for (i in 1:length(archivos_ARG)) {
  ruta_completa <- file.path(carpeta_destino, archivos_ARG[i])
  extension <- tools::file_ext(archivos_ARG[i])
  
  if (extension == "csv") {
    datos_ARG[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_ARG[[i]] <- read_excel(ruta_completa)
  }
}

datos_CABA <- list() #CABA

for (i in 1:length(archivos_CABA)) {
  ruta_completa <- file.path(carpeta_destino, archivos_CABA[i])
  extension <- tools::file_ext(archivos_CABA[i])
  
  if (extension == "csv") {
    datos_CABA[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_CABA[[i]] <- read_excel(ruta_completa)
  }
}

datos_PBA <- list() #PBA

for (i in 1:length(archivos_PBA)) {
  ruta_completa <- file.path(carpeta_destino, archivos_PBA[i])
  extension <- tools::file_ext(archivos_PBA[i])
  
  if (extension == "csv") {
    datos_PBA[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_PBA[[i]] <- read_excel(ruta_completa)
  }
}

datos_CAT <- list() #CAT

for (i in 1:length(archivos_CAT)) {
  ruta_completa <- file.path(carpeta_destino, archivos_CAT[i])
  extension <- tools::file_ext(archivos_CAT[i])
  
  if (extension == "csv") {
    datos_CAT[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_CAT[[i]] <- read_excel(ruta_completa)
  }
}

datos_CBA <- list() #CBA

for (i in 1:length(archivos_CBA)) {
  ruta_completa <- file.path(carpeta_destino, archivos_CBA[i])
  extension <- tools::file_ext(archivos_CBA[i])
  
  if (extension == "csv") {
    datos_CBA[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_CBA[[i]] <- read_excel(ruta_completa)
  }
}

datos_COR <- list() #COR

for (i in 1:length(archivos_COR)) {
  ruta_completa <- file.path(carpeta_destino, archivos_COR[i])
  extension <- tools::file_ext(archivos_COR[i])
  
  if (extension == "csv") {
    datos_COR[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_COR[[i]] <- read_excel(ruta_completa)
  }
}

datos_CHA <- list() #CHA

for (i in 1:length(archivos_CHA)) {
  ruta_completa <- file.path(carpeta_destino, archivos_CHA[i])
  extension <- tools::file_ext(archivos_CHA[i])
  
  if (extension == "csv") {
    datos_CHA[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_CHA[[i]] <- read_excel(ruta_completa)
  }
}

datos_CHU <- list() #CHU

for (i in 1:length(archivos_CHU)) {
  ruta_completa <- file.path(carpeta_destino, archivos_CHU[i])
  extension <- tools::file_ext(archivos_CHU[i])
  
  if (extension == "csv") {
    datos_CHU[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_CHU[[i]] <- read_excel(ruta_completa)
  }
}

datos_ER <- list() #ER

for (i in 1:length(archivos_ER)) {
  ruta_completa <- file.path(carpeta_destino, archivos_ER[i])
  extension <- tools::file_ext(archivos_ER[i])
  
  if (extension == "csv") {
    datos_ER[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_ER[[i]] <- read_excel(ruta_completa)
  }
}

datos_FOR <- list() #FOR

for (i in 1:length(archivos_FOR)) {
  ruta_completa <- file.path(carpeta_destino, archivos_FOR[i])
  extension <- tools::file_ext(archivos_FOR[i])
  
  if (extension == "csv") {
    datos_FOR[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_FOR[[i]] <- read_excel(ruta_completa)
  }
}

datos_JUJ <- list() #JUJ

for (i in 1:length(archivos_JUJ)) {
  ruta_completa <- file.path(carpeta_destino, archivos_JUJ[i])
  extension <- tools::file_ext(archivos_JUJ[i])
  
  if (extension == "csv") {
    datos_JUJ[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_JUJ[[i]] <- read_excel(ruta_completa)
  }
}

datos_LAP <- list() #LAP

for (i in 1:length(archivos_LAP)) {
  ruta_completa <- file.path(carpeta_destino, archivos_LAP[i])
  extension <- tools::file_ext(archivos_LAP[i])
  
  if (extension == "csv") {
    datos_LAP[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_LAP[[i]] <- read_excel(ruta_completa)
  }
}

datos_LAR <- list() #LAR

for (i in 1:length(archivos_LAR)) {
  ruta_completa <- file.path(carpeta_destino, archivos_LAR[i])
  extension <- tools::file_ext(archivos_LAR[i])
  
  if (extension == "csv") {
    datos_LAR[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_LAR[[i]] <- read_excel(ruta_completa)
  }
}

datos_MEN <- list() #MEN

for (i in 1:length(archivos_MEN)) {
  ruta_completa <- file.path(carpeta_destino, archivos_MEN[i])
  extension <- tools::file_ext(archivos_MEN[i])
  
  if (extension == "csv") {
    datos_MEN[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_MEN[[i]] <- read_excel(ruta_completa)
  }
}

datos_MIS <- list() #MIS

for (i in 1:length(archivos_MIS)) {
  ruta_completa <- file.path(carpeta_destino, archivos_MIS[i])
  extension <- tools::file_ext(archivos_MIS[i])
  
  if (extension == "csv") {
    datos_MIS[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_MIS[[i]] <- read_excel(ruta_completa)
  }
}

datos_NQN <- list() #NQN

for (i in 1:length(archivos_NQN)) {
  ruta_completa <- file.path(carpeta_destino, archivos_NQN[i])
  extension <- tools::file_ext(archivos_NQN[i])
  
  if (extension == "csv") {
    datos_NQN[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_NQN[[i]] <- read_excel(ruta_completa)
  }
}

datos_RN <- list() #RN

for (i in 1:length(archivos_RN)) {
  ruta_completa <- file.path(carpeta_destino, archivos_RN[i])
  extension <- tools::file_ext(archivos_RN[i])
  
  if (extension == "csv") {
    datos_RN[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_RN[[i]] <- read_excel(ruta_completa)
  }
}

datos_SAL <- list() #SAL

for (i in 1:length(archivos_SAL)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SAL[i])
  extension <- tools::file_ext(archivos_SAL[i])
  
  if (extension == "csv") {
    datos_SAL[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SAL[[i]] <- read_excel(ruta_completa)
  }
}

datos_SCR <- list() #SCR

for (i in 1:length(archivos_SCR)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SCR[i])
  extension <- tools::file_ext(archivos_SCR[i])
  
  if (extension == "csv") {
    datos_SCR[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SCR[[i]] <- read_excel(ruta_completa)
  }
}

datos_SFE <- list() #SFE

for (i in 1:length(archivos_SFE)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SFE[i])
  extension <- tools::file_ext(archivos_SFE[i])
  
  if (extension == "csv") {
    datos_SFE[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SFE[[i]] <- read_excel(ruta_completa)
  }
}

datos_SGO <- list() #SGO

for (i in 1:length(archivos_SGO)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SGO[i])
  extension <- tools::file_ext(archivos_SGO[i])
  
  if (extension == "csv") {
    datos_SGO[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SGO[[i]] <- read_excel(ruta_completa)
  }
}

datos_SJN <- list() #SJN

for (i in 1:length(archivos_SJN)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SJN[i])
  extension <- tools::file_ext(archivos_SJN[i])
  
  if (extension == "csv") {
    datos_SJN[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SJN[[i]] <- read_excel(ruta_completa)
  }
}

datos_SLU <- list() #SLU

for (i in 1:length(archivos_SLU)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SLU[i])
  extension <- tools::file_ext(archivos_SLU[i])
  
  if (extension == "csv") {
    datos_SLU[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SLU[[i]] <- read_excel(ruta_completa)
  }
}

datos_TDF <- list() #TDF

for (i in 1:length(archivos_TDF)) {
  ruta_completa <- file.path(carpeta_destino, archivos_TDF[i])
  extension <- tools::file_ext(archivos_TDF[i])
  
  if (extension == "csv") {
    datos_TDF[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_TDF[[i]] <- read_excel(ruta_completa)
  }
}

datos_TUC <- list() #TUC

for (i in 1:length(archivos_TUC)) {
  ruta_completa <- file.path(carpeta_destino, archivos_TUC[i])
  extension <- tools::file_ext(archivos_TUC[i])
  
  if (extension == "csv") {
    datos_TUC[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_TUC[[i]] <- read_excel(ruta_completa)
  }
}

## Agrego año y jurisdicción

lista_año <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
               2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)

for (i in 1:length(datos_ARG)) {
  datos_ARG[[i]] <- datos_ARG[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "ARG")
}

for (i in 1:length(datos_CABA)) {
  datos_CABA[[i]] <- datos_CABA[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "CABA")
}

for (i in 1:length(datos_PBA)) {
  datos_PBA[[i]] <- datos_PBA[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "PBA")
}

for (i in 1:length(datos_CAT)) {
  datos_CAT[[i]] <- datos_CAT[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "CAT")
}

for (i in 1:length(datos_CBA)) {
  datos_CBA[[i]] <- datos_CBA[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "CBA")
}

for (i in 1:length(datos_COR)) {
  datos_COR[[i]] <- datos_COR[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "COR")
}

for (i in 1:length(datos_CHA)) {
  datos_CHA[[i]] <- datos_CHA[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "CHA")
}

for (i in 1:length(datos_CHU)) {
  datos_CHU[[i]] <- datos_CHU[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "CHU")
}

for (i in 1:length(datos_ER)) {
  datos_ER[[i]] <- datos_ER[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "ER")
}

for (i in 1:length(datos_FOR)) {
  datos_FOR[[i]] <- datos_FOR[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "FOR")
}

for (i in 1:length(datos_JUJ)) {
  datos_JUJ[[i]] <- datos_JUJ[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "JUJ")
}

for (i in 1:length(datos_LAP)) {
  datos_LAP[[i]] <- datos_LAP[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "LAP")
}

for (i in 1:length(datos_LAR)) {
  datos_LAR[[i]] <- datos_LAR[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "LAR")
}

for (i in 1:length(datos_MEN)) {
  datos_MEN[[i]] <- datos_MEN[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "MEN")
}

for (i in 1:length(datos_MIS)) {
  datos_MIS[[i]] <- datos_MIS[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "MIS")
}

for (i in 1:length(datos_NQN)) {
  datos_NQN[[i]] <- datos_NQN[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "NQN")
}

for (i in 1:length(datos_RN)) {
  datos_RN[[i]] <- datos_RN[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "RN")
}

for (i in 1:length(datos_SAL)) {
  datos_SAL[[i]] <- datos_SAL[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SAL")
}

for (i in 1:length(datos_SCR)) {
  datos_SCR[[i]] <- datos_SCR[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SCR")
}

for (i in 1:length(datos_SFE)) {
  datos_SFE[[i]] <- datos_SFE[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SFE")
}

for (i in 1:length(datos_SGO)) {
  datos_SGO[[i]] <- datos_SGO[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SGO")
}

for (i in 1:length(datos_SJN)) {
  datos_SJN[[i]] <- datos_SJN[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SJN")
}

for (i in 1:length(datos_SLU)) {
  datos_SLU[[i]] <- datos_SLU[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SLU")
}

for (i in 1:length(datos_TDF)) {
  datos_TDF[[i]] <- datos_TDF[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "TDF")
}

for (i in 1:length(datos_TUC)) {
  datos_TUC[[i]] <- datos_TUC[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "TUC")
}


## Filtro fallecimientos con causa DM --> CHEQUEAR ORDEN
datos_TOT <- c(datos_ARG, datos_CABA, datos_PBA, datos_CAT, datos_CBA,
               datos_CHA, datos_CHU, 
               datos_COR, datos_ER, datos_FOR, datos_JUJ, datos_LAP,
               datos_LAR, datos_MEN, datos_MIS, datos_NQN, datos_RN, datos_SAL,
               datos_SCR, datos_SFE, datos_SGO, datos_SJN, datos_SLU, datos_TDF,
               datos_TUC)

for (i in 1:length(datos_TOT)) {
  datos_TOT[[i]] <- datos_TOT[[i]] %>% 
    filter(`GRUPO DE CAUSAS DE MUERTE` == "3. Diabetes mellitus")
}

## Asigno a objetos con nombres específicos
#nombres_TOT
#view(datos_TOT[163])

for (i in 1:length(nombres_TOT)) {
  assign(paste0(gsub(".xlsx", "", nombres_TOT[[i]])), datos_TOT[[i]])
}

## Uno todas las bases
bases_tot <- bind_rows(datos_TOT)
#prueba <- bases_tot %>% filter(AÑO == 2011) %>% select(TOTAL, AÑO, JURI)

## Pivoteo para armar grupo de edad
bases_tot <- bases_tot %>% 
  pivot_longer(cols = 3:22, names_to = "GRUP_EDAD", values_to = "RECUENTO_FALLE_TOT") %>% 
  select(`GRUPO DE CAUSAS DE MUERTE`, JURI, AÑO, GRUP_EDAD, RECUENTO_FALLE_TOT) #reordeno columnas, saco la de falle total



##### BASES DE FALLE MUJERES ################
##### Defino los nombres de los archivos ----
archivos_ARG <- c("ARG_2005_fem.xlsx","ARG_2006_fem.xlsx","ARG_2007_fem.xlsx","ARG_2008_fem.xlsx",
                  "ARG_2009_fem.xlsx","ARG_2010_fem.xlsx","ARG_2011_fem.xlsx","ARG_2012_fem.xlsx",
                  "ARG_2013_fem.xlsx","ARG_2014_fem.xlsx","ARG_2015_fem.xlsx","ARG_2016_fem.xlsx",
                  "ARG_2017_fem.xlsx","ARG_2018_fem.xlsx","ARG_2019_fem.xlsx","ARG_2020_fem.xlsx",
                  "ARG_2021_fem.xlsx","ARG_2022_fem.xlsx")
archivos_CABA <- c("CABA_2005_fem.xlsx","CABA_2006_fem.xlsx","CABA_2007_fem.xlsx","CABA_2008_fem.xlsx",
                   "CABA_2009_fem.xlsx","CABA_2010_fem.xlsx","CABA_2011_fem.xlsx","CABA_2012_fem.xlsx",
                   "CABA_2013_fem.xlsx","CABA_2014_fem.xlsx","CABA_2015_fem.xlsx","CABA_2016_fem.xlsx",
                   "CABA_2017_fem.xlsx","CABA_2018_fem.xlsx","CABA_2019_fem.xlsx","CABA_2020_fem.xlsx",
                   "CABA_2021_fem.xlsx","CABA_2022_fem.xlsx")
archivos_PBA <- c("PBA_2005_fem.xlsx","PBA_2006_fem.xlsx","PBA_2007_fem.xlsx","PBA_2008_fem.xlsx",
                  "PBA_2009_fem.xlsx","PBA_2010_fem.xlsx","PBA_2011_fem.xlsx","PBA_2012_fem.xlsx",
                  "PBA_2013_fem.xlsx","PBA_2014_fem.xlsx","PBA_2015_fem.xlsx","PBA_2016_fem.xlsx",
                  "PBA_2017_fem.xlsx","PBA_2018_fem.xlsx","PBA_2019_fem.xlsx","PBA_2020_fem.xlsx",
                  "PBA_2021_fem.xlsx","PBA_2022_fem.xlsx")
archivos_CBA <- c("CBA_2005_fem.xlsx","CBA_2006_fem.xlsx","CBA_2007_fem.xlsx","CBA_2008_fem.xlsx",
                  "CBA_2009_fem.xlsx","CBA_2010_fem.xlsx","CBA_2011_fem.xlsx","CBA_2012_fem.xlsx",
                  "CBA_2013_fem.xlsx","CBA_2014_fem.xlsx","CBA_2015_fem.xlsx","CBA_2016_fem.xlsx",
                  "CBA_2017_fem.xlsx","CBA_2018_fem.xlsx","CBA_2019_fem.xlsx","CBA_2020_fem.xlsx",
                  "CBA_2021_fem.xlsx","CBA_2022_fem.xlsx")
archivos_CAT <- c("CAT_2005_fem.xlsx","CAT_2006_fem.xlsx","CAT_2007_fem.xlsx","CAT_2008_fem.xlsx",
                  "CAT_2009_fem.xlsx","CAT_2010_fem.xlsx","CAT_2011_fem.xlsx","CAT_2012_fem.xlsx",
                  "CAT_2013_fem.xlsx","CAT_2014_fem.xlsx","CAT_2015_fem.xlsx","CAT_2016_fem.xlsx",
                  "CAT_2017_fem.xlsx","CAT_2018_fem.xlsx","CAT_2019_fem.xlsx","CAT_2020_fem.xlsx",
                  "CAT_2021_fem.xlsx","CAT_2022_fem.xlsx")
archivos_CHA <- c("CHA_2005_fem.xlsx","CHA_2006_fem.xlsx","CHA_2007_fem.xlsx","CHA_2008_fem.xlsx",
                  "CHA_2009_fem.xlsx","CHA_2010_fem.xlsx","CHA_2011_fem.xlsx","CHA_2012_fem.xlsx",
                  "CHA_2013_fem.xlsx","CHA_2014_fem.xlsx","CHA_2015_fem.xlsx","CHA_2016_fem.xlsx",
                  "CHA_2017_fem.xlsx","CHA_2018_fem.xlsx","CHA_2019_fem.xlsx","CHA_2020_fem.xlsx",
                  "CHA_2021_fem.xlsx","CHA_2022_fem.xlsx")
archivos_CHU <- c("CHU_2005_fem.xlsx","CHU_2006_fem.xlsx","CHU_2007_fem.xlsx","CHU_2008_fem.xlsx",
                  "CHU_2009_fem.xlsx","CHU_2010_fem.xlsx","CHU_2011_fem.xlsx","CHU_2012_fem.xlsx",
                  "CHU_2013_fem.xlsx","CHU_2014_fem.xlsx","CHU_2015_fem.xlsx","CHU_2016_fem.xlsx",
                  "CHU_2017_fem.xlsx","CHU_2018_fem.xlsx","CHU_2019_fem.xlsx","CHU_2020_fem.xlsx",
                  "CHU_2021_fem.xlsx","CHU_2022_fem.xlsx")
archivos_COR <- c("COR_2005_fem.xlsx","COR_2006_fem.xlsx","COR_2007_fem.xlsx","COR_2008_fem.xlsx",
                  "COR_2009_fem.xlsx","COR_2010_fem.xlsx","COR_2011_fem.xlsx","COR_2012_fem.xlsx",
                  "COR_2013_fem.xlsx","COR_2014_fem.xlsx","COR_2015_fem.xlsx","COR_2016_fem.xlsx",
                  "COR_2017_fem.xlsx","COR_2018_fem.xlsx","COR_2019_fem.xlsx","COR_2020_fem.xlsx",
                  "COR_2021_fem.xlsx","COR_2022_fem.xlsx")
archivos_ER <- c("ER_2005_fem.xlsx","ER_2006_fem.xlsx","ER_2007_fem.xlsx","ER_2008_fem.xlsx",
                 "ER_2009_fem.xlsx","ER_2010_fem.xlsx","ER_2011_fem.xlsx","ER_2012_fem.xlsx",
                 "ER_2013_fem.xlsx","ER_2014_fem.xlsx","ER_2015_fem.xlsx","ER_2016_fem.xlsx",
                 "ER_2017_fem.xlsx","ER_2018_fem.xlsx","ER_2019_fem.xlsx","ER_2020_fem.xlsx",
                 "ER_2021_fem.xlsx","ER_2022_fem.xlsx")
archivos_FOR <- c("FOR_2005_fem.xlsx","FOR_2006_fem.xlsx","FOR_2007_fem.xlsx","FOR_2008_fem.xlsx",
                  "FOR_2009_fem.xlsx","FOR_2010_fem.xlsx","FOR_2011_fem.xlsx","FOR_2012_fem.xlsx",
                  "FOR_2013_fem.xlsx","FOR_2014_fem.xlsx","FOR_2015_fem.xlsx","FOR_2016_fem.xlsx",
                  "FOR_2017_fem.xlsx","FOR_2018_fem.xlsx","FOR_2019_fem.xlsx","FOR_2020_fem.xlsx",
                  "FOR_2021_fem.xlsx","FOR_2022_fem.xlsx")
archivos_JUJ <- c("JUJ_2005_fem.xlsx","JUJ_2006_fem.xlsx","JUJ_2007_fem.xlsx","JUJ_2008_fem.xlsx",
                  "JUJ_2009_fem.xlsx","JUJ_2010_fem.xlsx","JUJ_2011_fem.xlsx","JUJ_2012_fem.xlsx",
                  "JUJ_2013_fem.xlsx","JUJ_2014_fem.xlsx","JUJ_2015_fem.xlsx","JUJ_2016_fem.xlsx",
                  "JUJ_2017_fem.xlsx","JUJ_2018_fem.xlsx","JUJ_2019_fem.xlsx","JUJ_2020_fem.xlsx",
                  "JUJ_2021_fem.xlsx","JUJ_2022_fem.xlsx")
archivos_LAP <- c("LAP_2005_fem.xlsx","LAP_2006_fem.xlsx","LAP_2007_fem.xlsx","LAP_2008_fem.xlsx",
                  "LAP_2009_fem.xlsx","LAP_2010_fem.xlsx","LAP_2011_fem.xlsx","LAP_2012_fem.xlsx",
                  "LAP_2013_fem.xlsx","LAP_2014_fem.xlsx","LAP_2015_fem.xlsx","LAP_2016_fem.xlsx",
                  "LAP_2017_fem.xlsx","LAP_2018_fem.xlsx","LAP_2019_fem.xlsx","LAP_2020_fem.xlsx",
                  "LAP_2021_fem.xlsx","LAP_2022_fem.xlsx")
archivos_LAR <- c("LAR_2005_fem.xlsx","LAR_2006_fem.xlsx","LAR_2007_fem.xlsx","LAR_2008_fem.xlsx",
                  "LAR_2009_fem.xlsx","LAR_2010_fem.xlsx","LAR_2011_fem.xlsx","LAR_2012_fem.xlsx",
                  "LAR_2013_fem.xlsx","LAR_2014_fem.xlsx","LAR_2015_fem.xlsx","LAR_2016_fem.xlsx",
                  "LAR_2017_fem.xlsx","LAR_2018_fem.xlsx","LAR_2019_fem.xlsx","LAR_2020_fem.xlsx",
                  "LAR_2021_fem.xlsx","LAR_2022_fem.xlsx")
archivos_MEN <- c("MEN_2005_fem.xlsx","MEN_2006_fem.xlsx","MEN_2007_fem.xlsx","MEN_2008_fem.xlsx",
                  "MEN_2009_fem.xlsx","MEN_2010_fem.xlsx","MEN_2011_fem.xlsx","MEN_2012_fem.xlsx",
                  "MEN_2013_fem.xlsx","MEN_2014_fem.xlsx","MEN_2015_fem.xlsx","MEN_2016_fem.xlsx",
                  "MEN_2017_fem.xlsx","MEN_2018_fem.xlsx","MEN_2019_fem.xlsx","MEN_2020_fem.xlsx",
                  "MEN_2021_fem.xlsx","MEN_2022_fem.xlsx")
archivos_MIS <- c("MIS_2005_fem.xlsx","MIS_2006_fem.xlsx","MIS_2007_fem.xlsx","MIS_2008_fem.xlsx",
                  "MIS_2009_fem.xlsx","MIS_2010_fem.xlsx","MIS_2011_fem.xlsx","MIS_2012_fem.xlsx",
                  "MIS_2013_fem.xlsx","MIS_2014_fem.xlsx","MIS_2015_fem.xlsx","MIS_2016_fem.xlsx",
                  "MIS_2017_fem.xlsx","MIS_2018_fem.xlsx","MIS_2019_fem.xlsx","MIS_2020_fem.xlsx",
                  "MIS_2021_fem.xlsx","MIS_2022_fem.xlsx")
archivos_NQN <- c("NQN_2005_fem.xlsx","NQN_2006_fem.xlsx","NQN_2007_fem.xlsx","NQN_2008_fem.xlsx",
                  "NQN_2009_fem.xlsx","NQN_2010_fem.xlsx","NQN_2011_fem.xlsx","NQN_2012_fem.xlsx",
                  "NQN_2013_fem.xlsx","NQN_2014_fem.xlsx","NQN_2015_fem.xlsx","NQN_2016_fem.xlsx",
                  "NQN_2017_fem.xlsx","NQN_2018_fem.xlsx","NQN_2019_fem.xlsx","NQN_2020_fem.xlsx",
                  "NQN_2021_fem.xlsx","NQN_2022_fem.xlsx")
archivos_RN <- c("RN_2005_fem.xlsx","RN_2006_fem.xlsx","RN_2007_fem.xlsx","RN_2008_fem.xlsx",
                 "RN_2009_fem.xlsx","RN_2010_fem.xlsx","RN_2011_fem.xlsx","RN_2012_fem.xlsx",
                 "RN_2013_fem.xlsx","RN_2014_fem.xlsx","RN_2015_fem.xlsx","RN_2016_fem.xlsx",
                 "RN_2017_fem.xlsx","RN_2018_fem.xlsx","RN_2019_fem.xlsx","RN_2020_fem.xlsx",
                 "RN_2021_fem.xlsx","RN_2022_fem.xlsx")
archivos_SAL <- c("SAL_2005_fem.xlsx","SAL_2006_fem.xlsx","SAL_2007_fem.xlsx","SAL_2008_fem.xlsx",
                  "SAL_2009_fem.xlsx","SAL_2010_fem.xlsx","SAL_2011_fem.xlsx","SAL_2012_fem.xlsx",
                  "SAL_2013_fem.xlsx","SAL_2014_fem.xlsx","SAL_2015_fem.xlsx","SAL_2016_fem.xlsx",
                  "SAL_2017_fem.xlsx","SAL_2018_fem.xlsx","SAL_2019_fem.xlsx","SAL_2020_fem.xlsx",
                  "SAL_2021_fem.xlsx","SAL_2022_fem.xlsx")
archivos_SCR <- c("SCR_2005_fem.xlsx","SCR_2006_fem.xlsx","SCR_2007_fem.xlsx","SCR_2008_fem.xlsx",
                  "SCR_2009_fem.xlsx","SCR_2010_fem.xlsx","SCR_2011_fem.xlsx","SCR_2012_fem.xlsx",
                  "SCR_2013_fem.xlsx","SCR_2014_fem.xlsx","SCR_2015_fem.xlsx","SCR_2016_fem.xlsx",
                  "SCR_2017_fem.xlsx","SCR_2018_fem.xlsx","SCR_2019_fem.xlsx","SCR_2020_fem.xlsx",
                  "SCR_2021_fem.xlsx","SCR_2022_fem.xlsx")
archivos_SFE <- c("SFE_2005_fem.xlsx","SFE_2006_fem.xlsx","SFE_2007_fem.xlsx","SFE_2008_fem.xlsx",
                  "SFE_2009_fem.xlsx","SFE_2010_fem.xlsx","SFE_2011_fem.xlsx","SFE_2012_fem.xlsx",
                  "SFE_2013_fem.xlsx","SFE_2014_fem.xlsx","SFE_2015_fem.xlsx","SFE_2016_fem.xlsx",
                  "SFE_2017_fem.xlsx","SFE_2018_fem.xlsx","SFE_2019_fem.xlsx","SFE_2020_fem.xlsx",
                  "SFE_2021_fem.xlsx","SFE_2022_fem.xlsx")
archivos_SGO <- c("SGO_2005_fem.xlsx","SGO_2006_fem.xlsx","SGO_2007_fem.xlsx","SGO_2008_fem.xlsx",
                  "SGO_2009_fem.xlsx","SGO_2010_fem.xlsx","SGO_2011_fem.xlsx","SGO_2012_fem.xlsx",
                  "SGO_2013_fem.xlsx","SGO_2014_fem.xlsx","SGO_2015_fem.xlsx","SGO_2016_fem.xlsx",
                  "SGO_2017_fem.xlsx","SGO_2018_fem.xlsx","SGO_2019_fem.xlsx","SGO_2020_fem.xlsx",
                  "SGO_2021_fem.xlsx","SGO_2022_fem.xlsx")
archivos_SJN <- c("SJN_2005_fem.xlsx","SJN_2006_fem.xlsx","SJN_2007_fem.xlsx","SJN_2008_fem.xlsx",
                  "SJN_2009_fem.xlsx","SJN_2010_fem.xlsx","SJN_2011_fem.xlsx","SJN_2012_fem.xlsx",
                  "SJN_2013_fem.xlsx","SJN_2014_fem.xlsx","SJN_2015_fem.xlsx","SJN_2016_fem.xlsx",
                  "SJN_2017_fem.xlsx","SJN_2018_fem.xlsx","SJN_2019_fem.xlsx","SJN_2020_fem.xlsx",
                  "SJN_2021_fem.xlsx","SJN_2022_fem.xlsx")
archivos_SLU <- c("SLU_2005_fem.xlsx","SLU_2006_fem.xlsx","SLU_2007_fem.xlsx","SLU_2008_fem.xlsx",
                  "SLU_2009_fem.xlsx","SLU_2010_fem.xlsx","SLU_2011_fem.xlsx","SLU_2012_fem.xlsx",
                  "SLU_2013_fem.xlsx","SLU_2014_fem.xlsx","SLU_2015_fem.xlsx","SLU_2016_fem.xlsx",
                  "SLU_2017_fem.xlsx","SLU_2018_fem.xlsx","SLU_2019_fem.xlsx","SLU_2020_fem.xlsx",
                  "SLU_2021_fem.xlsx","SLU_2022_fem.xlsx")
archivos_TDF <- c("TDF_2005_fem.xlsx","TDF_2006_fem.xlsx","TDF_2007_fem.xlsx","TDF_2008_fem.xlsx",
                  "TDF_2009_fem.xlsx","TDF_2010_fem.xlsx","TDF_2011_fem.xlsx","TDF_2012_fem.xlsx",
                  "TDF_2013_fem.xlsx","TDF_2014_fem.xlsx","TDF_2015_fem.xlsx","TDF_2016_fem.xlsx",
                  "TDF_2017_fem.xlsx","TDF_2018_fem.xlsx","TDF_2019_fem.xlsx","TDF_2020_fem.xlsx",
                  "TDF_2021_fem.xlsx","TDF_2022_fem.xlsx")
archivos_TUC <- c("TUC_2005_fem.xlsx","TUC_2006_fem.xlsx","TUC_2007_fem.xlsx","TUC_2008_fem.xlsx",
                  "TUC_2009_fem.xlsx","TUC_2010_fem.xlsx","TUC_2011_fem.xlsx","TUC_2012_fem.xlsx",
                  "TUC_2013_fem.xlsx","TUC_2014_fem.xlsx","TUC_2015_fem.xlsx","TUC_2016_fem.xlsx",
                  "TUC_2017_fem.xlsx","TUC_2018_fem.xlsx","TUC_2019_fem.xlsx","TUC_2020_fem.xlsx",
                  "TUC_2021_fem.xlsx","TUC_2022_fem.xlsx")


nombres_fem <- c(archivos_ARG, archivos_CABA, archivos_PBA, archivos_CAT, archivos_CBA,
                 archivos_CHA, archivos_CHU,
                 archivos_COR, archivos_ER, archivos_FOR, archivos_JUJ, archivos_LAP, archivos_LAR,
                 archivos_MEN, archivos_MIS, archivos_NQN, archivos_RN, archivos_SAL, archivos_SCR,
                 archivos_SFE, archivos_SGO, archivos_SJN, archivos_SLU, archivos_TDF, archivos_TUC)

##### Leo los archivos y asignar a objetos en R ----

# Carpeta de destino
carpeta_destino <- "C:/Users/Mica Gauto/Desktop/INE/INVESTIGACIÓN/DIABETES/CARGa de enfermedad/Procesamiento R/Bases_mort/Falle_quinquenales/fem"

## Armo listas para almacenar datos, por jurisdicción y females
datos_ARG <- list() #ARG

for (i in 1:length(archivos_ARG)) {
  ruta_completa <- file.path(carpeta_destino, archivos_ARG[i])
  extension <- tools::file_ext(archivos_ARG[i])
  
  if (extension == "csv") {
    datos_ARG[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_ARG[[i]] <- read_excel(ruta_completa)
  }
}

datos_CABA <- list() #CABA

for (i in 1:length(archivos_CABA)) {
  ruta_completa <- file.path(carpeta_destino, archivos_CABA[i])
  extension <- tools::file_ext(archivos_CABA[i])
  
  if (extension == "csv") {
    datos_CABA[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_CABA[[i]] <- read_excel(ruta_completa)
  }
}

datos_PBA <- list() #PBA

for (i in 1:length(archivos_PBA)) {
  ruta_completa <- file.path(carpeta_destino, archivos_PBA[i])
  extension <- tools::file_ext(archivos_PBA[i])
  
  if (extension == "csv") {
    datos_PBA[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_PBA[[i]] <- read_excel(ruta_completa)
  }
}

datos_CAT <- list() #CAT

for (i in 1:length(archivos_CAT)) {
  ruta_completa <- file.path(carpeta_destino, archivos_CAT[i])
  extension <- tools::file_ext(archivos_CAT[i])
  
  if (extension == "csv") {
    datos_CAT[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_CAT[[i]] <- read_excel(ruta_completa)
  }
}

datos_CBA <- list() #CBA

for (i in 1:length(archivos_CBA)) {
  ruta_completa <- file.path(carpeta_destino, archivos_CBA[i])
  extension <- tools::file_ext(archivos_CBA[i])
  
  if (extension == "csv") {
    datos_CBA[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_CBA[[i]] <- read_excel(ruta_completa)
  }
}

datos_COR <- list() #COR

for (i in 1:length(archivos_COR)) {
  ruta_completa <- file.path(carpeta_destino, archivos_COR[i])
  extension <- tools::file_ext(archivos_COR[i])
  
  if (extension == "csv") {
    datos_COR[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_COR[[i]] <- read_excel(ruta_completa)
  }
}

datos_CHA <- list() #CHA

for (i in 1:length(archivos_CHA)) {
  ruta_completa <- file.path(carpeta_destino, archivos_CHA[i])
  extension <- tools::file_ext(archivos_CHA[i])
  
  if (extension == "csv") {
    datos_CHA[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_CHA[[i]] <- read_excel(ruta_completa)
  }
}

datos_CHU <- list() #CHU

for (i in 1:length(archivos_CHU)) {
  ruta_completa <- file.path(carpeta_destino, archivos_CHU[i])
  extension <- tools::file_ext(archivos_CHU[i])
  
  if (extension == "csv") {
    datos_CHU[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_CHU[[i]] <- read_excel(ruta_completa)
  }
}

datos_ER <- list() #ER

for (i in 1:length(archivos_ER)) {
  ruta_completa <- file.path(carpeta_destino, archivos_ER[i])
  extension <- tools::file_ext(archivos_ER[i])
  
  if (extension == "csv") {
    datos_ER[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_ER[[i]] <- read_excel(ruta_completa)
  }
}

datos_FOR <- list() #FOR

for (i in 1:length(archivos_FOR)) {
  ruta_completa <- file.path(carpeta_destino, archivos_FOR[i])
  extension <- tools::file_ext(archivos_FOR[i])
  
  if (extension == "csv") {
    datos_FOR[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_FOR[[i]] <- read_excel(ruta_completa)
  }
}

datos_JUJ <- list() #JUJ

for (i in 1:length(archivos_JUJ)) {
  ruta_completa <- file.path(carpeta_destino, archivos_JUJ[i])
  extension <- tools::file_ext(archivos_JUJ[i])
  
  if (extension == "csv") {
    datos_JUJ[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_JUJ[[i]] <- read_excel(ruta_completa)
  }
}

datos_LAP <- list() #LAP

for (i in 1:length(archivos_LAP)) {
  ruta_completa <- file.path(carpeta_destino, archivos_LAP[i])
  extension <- tools::file_ext(archivos_LAP[i])
  
  if (extension == "csv") {
    datos_LAP[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_LAP[[i]] <- read_excel(ruta_completa)
  }
}

datos_LAR <- list() #LAR

for (i in 1:length(archivos_LAR)) {
  ruta_completa <- file.path(carpeta_destino, archivos_LAR[i])
  extension <- tools::file_ext(archivos_LAR[i])
  
  if (extension == "csv") {
    datos_LAR[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_LAR[[i]] <- read_excel(ruta_completa)
  }
}

datos_MEN <- list() #MEN

for (i in 1:length(archivos_MEN)) {
  ruta_completa <- file.path(carpeta_destino, archivos_MEN[i])
  extension <- tools::file_ext(archivos_MEN[i])
  
  if (extension == "csv") {
    datos_MEN[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_MEN[[i]] <- read_excel(ruta_completa)
  }
}

datos_MIS <- list() #MIS

for (i in 1:length(archivos_MIS)) {
  ruta_completa <- file.path(carpeta_destino, archivos_MIS[i])
  extension <- tools::file_ext(archivos_MIS[i])
  
  if (extension == "csv") {
    datos_MIS[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_MIS[[i]] <- read_excel(ruta_completa)
  }
}

datos_NQN <- list() #NQN

for (i in 1:length(archivos_NQN)) {
  ruta_completa <- file.path(carpeta_destino, archivos_NQN[i])
  extension <- tools::file_ext(archivos_NQN[i])
  
  if (extension == "csv") {
    datos_NQN[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_NQN[[i]] <- read_excel(ruta_completa)
  }
}

datos_RN <- list() #RN

for (i in 1:length(archivos_RN)) {
  ruta_completa <- file.path(carpeta_destino, archivos_RN[i])
  extension <- tools::file_ext(archivos_RN[i])
  
  if (extension == "csv") {
    datos_RN[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_RN[[i]] <- read_excel(ruta_completa)
  }
}

datos_SAL <- list() #SAL

for (i in 1:length(archivos_SAL)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SAL[i])
  extension <- tools::file_ext(archivos_SAL[i])
  
  if (extension == "csv") {
    datos_SAL[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SAL[[i]] <- read_excel(ruta_completa)
  }
}

datos_SCR <- list() #SCR

for (i in 1:length(archivos_SCR)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SCR[i])
  extension <- tools::file_ext(archivos_SCR[i])
  
  if (extension == "csv") {
    datos_SCR[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SCR[[i]] <- read_excel(ruta_completa)
  }
}

datos_SFE <- list() #SFE

for (i in 1:length(archivos_SFE)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SFE[i])
  extension <- tools::file_ext(archivos_SFE[i])
  
  if (extension == "csv") {
    datos_SFE[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SFE[[i]] <- read_excel(ruta_completa)
  }
}

datos_SGO <- list() #SGO

for (i in 1:length(archivos_SGO)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SGO[i])
  extension <- tools::file_ext(archivos_SGO[i])
  
  if (extension == "csv") {
    datos_SGO[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SGO[[i]] <- read_excel(ruta_completa)
  }
}

datos_SJN <- list() #SJN

for (i in 1:length(archivos_SJN)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SJN[i])
  extension <- tools::file_ext(archivos_SJN[i])
  
  if (extension == "csv") {
    datos_SJN[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SJN[[i]] <- read_excel(ruta_completa)
  }
}

datos_SLU <- list() #SLU

for (i in 1:length(archivos_SLU)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SLU[i])
  extension <- tools::file_ext(archivos_SLU[i])
  
  if (extension == "csv") {
    datos_SLU[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SLU[[i]] <- read_excel(ruta_completa)
  }
}

datos_TDF <- list() #TDF

for (i in 1:length(archivos_TDF)) {
  ruta_completa <- file.path(carpeta_destino, archivos_TDF[i])
  extension <- tools::file_ext(archivos_TDF[i])
  
  if (extension == "csv") {
    datos_TDF[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_TDF[[i]] <- read_excel(ruta_completa)
  }
}

datos_TUC <- list() #TUC

for (i in 1:length(archivos_TUC)) {
  ruta_completa <- file.path(carpeta_destino, archivos_TUC[i])
  extension <- tools::file_ext(archivos_TUC[i])
  
  if (extension == "csv") {
    datos_TUC[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_TUC[[i]] <- read_excel(ruta_completa)
  }
}

## Agrego año y jurisdicción

lista_año <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
               2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)

for (i in 1:length(datos_ARG)) {
  datos_ARG[[i]] <- datos_ARG[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "ARG")
}

for (i in 1:length(datos_CABA)) {
  datos_CABA[[i]] <- datos_CABA[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "CABA")
}

for (i in 1:length(datos_PBA)) {
  datos_PBA[[i]] <- datos_PBA[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "PBA")
}

for (i in 1:length(datos_CAT)) {
  datos_CAT[[i]] <- datos_CAT[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "CAT")
}

for (i in 1:length(datos_CBA)) {
  datos_CBA[[i]] <- datos_CBA[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "CBA")
}

for (i in 1:length(datos_COR)) {
  datos_COR[[i]] <- datos_COR[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "COR")
}

for (i in 1:length(datos_CHA)) {
  datos_CHA[[i]] <- datos_CHA[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "CHA")
}

for (i in 1:length(datos_CHU)) {
  datos_CHU[[i]] <- datos_CHU[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "CHU")
}

for (i in 1:length(datos_ER)) {
  datos_ER[[i]] <- datos_ER[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "ER")
}

for (i in 1:length(datos_FOR)) {
  datos_FOR[[i]] <- datos_FOR[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "FOR")
}

for (i in 1:length(datos_JUJ)) {
  datos_JUJ[[i]] <- datos_JUJ[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "JUJ")
}

for (i in 1:length(datos_LAP)) {
  datos_LAP[[i]] <- datos_LAP[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "LAP")
}

for (i in 1:length(datos_LAR)) {
  datos_LAR[[i]] <- datos_LAR[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "LAR")
}

for (i in 1:length(datos_MEN)) {
  datos_MEN[[i]] <- datos_MEN[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "MEN")
}

for (i in 1:length(datos_MIS)) {
  datos_MIS[[i]] <- datos_MIS[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "MIS")
}

for (i in 1:length(datos_NQN)) {
  datos_NQN[[i]] <- datos_NQN[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "NQN")
}

for (i in 1:length(datos_RN)) {
  datos_RN[[i]] <- datos_RN[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "RN")
}

for (i in 1:length(datos_SAL)) {
  datos_SAL[[i]] <- datos_SAL[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SAL")
}

for (i in 1:length(datos_SCR)) {
  datos_SCR[[i]] <- datos_SCR[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SCR")
}

for (i in 1:length(datos_SFE)) {
  datos_SFE[[i]] <- datos_SFE[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SFE")
}

for (i in 1:length(datos_SGO)) {
  datos_SGO[[i]] <- datos_SGO[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SGO")
}

for (i in 1:length(datos_SJN)) {
  datos_SJN[[i]] <- datos_SJN[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SJN")
}

for (i in 1:length(datos_SLU)) {
  datos_SLU[[i]] <- datos_SLU[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SLU")
}

for (i in 1:length(datos_TDF)) {
  datos_TDF[[i]] <- datos_TDF[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "TDF")
}

for (i in 1:length(datos_TUC)) {
  datos_TUC[[i]] <- datos_TUC[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "TUC")
}


## Filtro fallecimientos con causa DM --> CHEQUEAR ORDEN
datos_fem <- c(datos_ARG, datos_CABA, datos_PBA, datos_CAT, datos_CBA,
               datos_CHA, datos_CHU, 
               datos_COR, datos_ER, datos_FOR, datos_JUJ, datos_LAP,
               datos_LAR, datos_MEN, datos_MIS, datos_NQN, datos_RN, datos_SAL,
               datos_SCR, datos_SFE, datos_SGO, datos_SJN, datos_SLU, datos_TDF,
               datos_TUC)

for (i in 1:length(datos_fem)) {
  datos_fem[[i]] <- datos_fem[[i]] %>% 
    filter(`GRUPO DE CAUSAS DE MUERTE` == "3. Diabetes mellitus")
}

## Asigno a objetos con nombres específicos
#nombres_fem
#view(datos_fem[163])

for (i in 1:length(nombres_fem)) {
  assign(paste0(gsub(".xlsx", "", nombres_fem[[i]])), datos_fem[[i]])
}

## Uno todas las bases
bases_fem <- bind_rows(datos_fem)
#prueba <- bases_fem %>% filter(AÑO == 2017) %>% select(TOTAL, AÑO, JURI)

## Pivoteo para armar grupo de edad
bases_fem <- bases_fem %>% 
  pivot_longer(cols = 3:22, names_to = "GRUP_EDAD", values_to = "RECUENTO_FALLE_fem") %>% 
  select(`GRUPO DE CAUSAS DE MUERTE`, JURI, AÑO, GRUP_EDAD, RECUENTO_FALLE_fem) #reordeno columnas, saco la de falle femal



##### BASES DE FALLE VARONES ################
##### Defino los nombres de los archivos ----
archivos_ARG <- c("ARG_2005_masc.xlsx","ARG_2006_masc.xlsx","ARG_2007_masc.xlsx","ARG_2008_masc.xlsx",
                  "ARG_2009_masc.xlsx","ARG_2010_masc.xlsx","ARG_2011_masc.xlsx","ARG_2012_masc.xlsx",
                  "ARG_2013_masc.xlsx","ARG_2014_masc.xlsx","ARG_2015_masc.xlsx","ARG_2016_masc.xlsx",
                  "ARG_2017_masc.xlsx","ARG_2018_masc.xlsx","ARG_2019_masc.xlsx","ARG_2020_masc.xlsx",
                  "ARG_2021_masc.xlsx","ARG_2022_masc.xlsx")
archivos_CABA <- c("CABA_2005_masc.xlsx","CABA_2006_masc.xlsx","CABA_2007_masc.xlsx","CABA_2008_masc.xlsx",
                   "CABA_2009_masc.xlsx","CABA_2010_masc.xlsx","CABA_2011_masc.xlsx","CABA_2012_masc.xlsx",
                   "CABA_2013_masc.xlsx","CABA_2014_masc.xlsx","CABA_2015_masc.xlsx","CABA_2016_masc.xlsx",
                   "CABA_2017_masc.xlsx","CABA_2018_masc.xlsx","CABA_2019_masc.xlsx","CABA_2020_masc.xlsx",
                   "CABA_2021_masc.xlsx","CABA_2022_masc.xlsx")
archivos_PBA <- c("PBA_2005_masc.xlsx","PBA_2006_masc.xlsx","PBA_2007_masc.xlsx","PBA_2008_masc.xlsx",
                  "PBA_2009_masc.xlsx","PBA_2010_masc.xlsx","PBA_2011_masc.xlsx","PBA_2012_masc.xlsx",
                  "PBA_2013_masc.xlsx","PBA_2014_masc.xlsx","PBA_2015_masc.xlsx","PBA_2016_masc.xlsx",
                  "PBA_2017_masc.xlsx","PBA_2018_masc.xlsx","PBA_2019_masc.xlsx","PBA_2020_masc.xlsx",
                  "PBA_2021_masc.xlsx","PBA_2022_masc.xlsx")
archivos_CBA <- c("CBA_2005_masc.xlsx","CBA_2006_masc.xlsx","CBA_2007_masc.xlsx","CBA_2008_masc.xlsx",
                  "CBA_2009_masc.xlsx","CBA_2010_masc.xlsx","CBA_2011_masc.xlsx","CBA_2012_masc.xlsx",
                  "CBA_2013_masc.xlsx","CBA_2014_masc.xlsx","CBA_2015_masc.xlsx","CBA_2016_masc.xlsx",
                  "CBA_2017_masc.xlsx","CBA_2018_masc.xlsx","CBA_2019_masc.xlsx","CBA_2020_masc.xlsx",
                  "CBA_2021_masc.xlsx","CBA_2022_masc.xlsx")
archivos_CAT <- c("CAT_2005_masc.xlsx","CAT_2006_masc.xlsx","CAT_2007_masc.xlsx","CAT_2008_masc.xlsx",
                  "CAT_2009_masc.xlsx","CAT_2010_masc.xlsx","CAT_2011_masc.xlsx","CAT_2012_masc.xlsx",
                  "CAT_2013_masc.xlsx","CAT_2014_masc.xlsx","CAT_2015_masc.xlsx","CAT_2016_masc.xlsx",
                  "CAT_2017_masc.xlsx","CAT_2018_masc.xlsx","CAT_2019_masc.xlsx","CAT_2020_masc.xlsx",
                  "CAT_2021_masc.xlsx","CAT_2022_masc.xlsx")
archivos_CHA <- c("CHA_2005_masc.xlsx","CHA_2006_masc.xlsx","CHA_2007_masc.xlsx","CHA_2008_masc.xlsx",
                  "CHA_2009_masc.xlsx","CHA_2010_masc.xlsx","CHA_2011_masc.xlsx","CHA_2012_masc.xlsx",
                  "CHA_2013_masc.xlsx","CHA_2014_masc.xlsx","CHA_2015_masc.xlsx","CHA_2016_masc.xlsx",
                  "CHA_2017_masc.xlsx","CHA_2018_masc.xlsx","CHA_2019_masc.xlsx","CHA_2020_masc.xlsx",
                  "CHA_2021_masc.xlsx","CHA_2022_masc.xlsx")
archivos_CHU <- c("CHU_2005_masc.xlsx","CHU_2006_masc.xlsx","CHU_2007_masc.xlsx","CHU_2008_masc.xlsx",
                  "CHU_2009_masc.xlsx","CHU_2010_masc.xlsx","CHU_2011_masc.xlsx","CHU_2012_masc.xlsx",
                  "CHU_2013_masc.xlsx","CHU_2014_masc.xlsx","CHU_2015_masc.xlsx","CHU_2016_masc.xlsx",
                  "CHU_2017_masc.xlsx","CHU_2018_masc.xlsx","CHU_2019_masc.xlsx","CHU_2020_masc.xlsx",
                  "CHU_2021_masc.xlsx","CHU_2022_masc.xlsx")
archivos_COR <- c("COR_2005_masc.xlsx","COR_2006_masc.xlsx","COR_2007_masc.xlsx","COR_2008_masc.xlsx",
                  "COR_2009_masc.xlsx","COR_2010_masc.xlsx","COR_2011_masc.xlsx","COR_2012_masc.xlsx",
                  "COR_2013_masc.xlsx","COR_2014_masc.xlsx","COR_2015_masc.xlsx","COR_2016_masc.xlsx",
                  "COR_2017_masc.xlsx","COR_2018_masc.xlsx","COR_2019_masc.xlsx","COR_2020_masc.xlsx",
                  "COR_2021_masc.xlsx","COR_2022_masc.xlsx")
archivos_ER <- c("ER_2005_masc.xlsx","ER_2006_masc.xlsx","ER_2007_masc.xlsx","ER_2008_masc.xlsx",
                 "ER_2009_masc.xlsx","ER_2010_masc.xlsx","ER_2011_masc.xlsx","ER_2012_masc.xlsx",
                 "ER_2013_masc.xlsx","ER_2014_masc.xlsx","ER_2015_masc.xlsx","ER_2016_masc.xlsx",
                 "ER_2017_masc.xlsx","ER_2018_masc.xlsx","ER_2019_masc.xlsx","ER_2020_masc.xlsx",
                 "ER_2021_masc.xlsx","ER_2022_masc.xlsx")
archivos_FOR <- c("FOR_2005_masc.xlsx","FOR_2006_masc.xlsx","FOR_2007_masc.xlsx","FOR_2008_masc.xlsx",
                  "FOR_2009_masc.xlsx","FOR_2010_masc.xlsx","FOR_2011_masc.xlsx","FOR_2012_masc.xlsx",
                  "FOR_2013_masc.xlsx","FOR_2014_masc.xlsx","FOR_2015_masc.xlsx","FOR_2016_masc.xlsx",
                  "FOR_2017_masc.xlsx","FOR_2018_masc.xlsx","FOR_2019_masc.xlsx","FOR_2020_masc.xlsx",
                  "FOR_2021_masc.xlsx","FOR_2022_masc.xlsx")
archivos_JUJ <- c("JUJ_2005_masc.xlsx","JUJ_2006_masc.xlsx","JUJ_2007_masc.xlsx","JUJ_2008_masc.xlsx",
                  "JUJ_2009_masc.xlsx","JUJ_2010_masc.xlsx","JUJ_2011_masc.xlsx","JUJ_2012_masc.xlsx",
                  "JUJ_2013_masc.xlsx","JUJ_2014_masc.xlsx","JUJ_2015_masc.xlsx","JUJ_2016_masc.xlsx",
                  "JUJ_2017_masc.xlsx","JUJ_2018_masc.xlsx","JUJ_2019_masc.xlsx","JUJ_2020_masc.xlsx",
                  "JUJ_2021_masc.xlsx","JUJ_2022_masc.xlsx")
archivos_LAP <- c("LAP_2005_masc.xlsx","LAP_2006_masc.xlsx","LAP_2007_masc.xlsx","LAP_2008_masc.xlsx",
                  "LAP_2009_masc.xlsx","LAP_2010_masc.xlsx","LAP_2011_masc.xlsx","LAP_2012_masc.xlsx",
                  "LAP_2013_masc.xlsx","LAP_2014_masc.xlsx","LAP_2015_masc.xlsx","LAP_2016_masc.xlsx",
                  "LAP_2017_masc.xlsx","LAP_2018_masc.xlsx","LAP_2019_masc.xlsx","LAP_2020_masc.xlsx",
                  "LAP_2021_masc.xlsx","LAP_2022_masc.xlsx")
archivos_LAR <- c("LAR_2005_masc.xlsx","LAR_2006_masc.xlsx","LAR_2007_masc.xlsx","LAR_2008_masc.xlsx",
                  "LAR_2009_masc.xlsx","LAR_2010_masc.xlsx","LAR_2011_masc.xlsx","LAR_2012_masc.xlsx",
                  "LAR_2013_masc.xlsx","LAR_2014_masc.xlsx","LAR_2015_masc.xlsx","LAR_2016_masc.xlsx",
                  "LAR_2017_masc.xlsx","LAR_2018_masc.xlsx","LAR_2019_masc.xlsx","LAR_2020_masc.xlsx",
                  "LAR_2021_masc.xlsx","LAR_2022_masc.xlsx")
archivos_MEN <- c("MEN_2005_masc.xlsx","MEN_2006_masc.xlsx","MEN_2007_masc.xlsx","MEN_2008_masc.xlsx",
                  "MEN_2009_masc.xlsx","MEN_2010_masc.xlsx","MEN_2011_masc.xlsx","MEN_2012_masc.xlsx",
                  "MEN_2013_masc.xlsx","MEN_2014_masc.xlsx","MEN_2015_masc.xlsx","MEN_2016_masc.xlsx",
                  "MEN_2017_masc.xlsx","MEN_2018_masc.xlsx","MEN_2019_masc.xlsx","MEN_2020_masc.xlsx",
                  "MEN_2021_masc.xlsx","MEN_2022_masc.xlsx")
archivos_MIS <- c("MIS_2005_masc.xlsx","MIS_2006_masc.xlsx","MIS_2007_masc.xlsx","MIS_2008_masc.xlsx",
                  "MIS_2009_masc.xlsx","MIS_2010_masc.xlsx","MIS_2011_masc.xlsx","MIS_2012_masc.xlsx",
                  "MIS_2013_masc.xlsx","MIS_2014_masc.xlsx","MIS_2015_masc.xlsx","MIS_2016_masc.xlsx",
                  "MIS_2017_masc.xlsx","MIS_2018_masc.xlsx","MIS_2019_masc.xlsx","MIS_2020_masc.xlsx",
                  "MIS_2021_masc.xlsx","MIS_2022_masc.xlsx")
archivos_NQN <- c("NQN_2005_masc.xlsx","NQN_2006_masc.xlsx","NQN_2007_masc.xlsx","NQN_2008_masc.xlsx",
                  "NQN_2009_masc.xlsx","NQN_2010_masc.xlsx","NQN_2011_masc.xlsx","NQN_2012_masc.xlsx",
                  "NQN_2013_masc.xlsx","NQN_2014_masc.xlsx","NQN_2015_masc.xlsx","NQN_2016_masc.xlsx",
                  "NQN_2017_masc.xlsx","NQN_2018_masc.xlsx","NQN_2019_masc.xlsx","NQN_2020_masc.xlsx",
                  "NQN_2021_masc.xlsx","NQN_2022_masc.xlsx")
archivos_RN <- c("RN_2005_masc.xlsx","RN_2006_masc.xlsx","RN_2007_masc.xlsx","RN_2008_masc.xlsx",
                 "RN_2009_masc.xlsx","RN_2010_masc.xlsx","RN_2011_masc.xlsx","RN_2012_masc.xlsx",
                 "RN_2013_masc.xlsx","RN_2014_masc.xlsx","RN_2015_masc.xlsx","RN_2016_masc.xlsx",
                 "RN_2017_masc.xlsx","RN_2018_masc.xlsx","RN_2019_masc.xlsx","RN_2020_masc.xlsx",
                 "RN_2021_masc.xlsx","RN_2022_masc.xlsx")
archivos_SAL <- c("SAL_2005_masc.xlsx","SAL_2006_masc.xlsx","SAL_2007_masc.xlsx","SAL_2008_masc.xlsx",
                  "SAL_2009_masc.xlsx","SAL_2010_masc.xlsx","SAL_2011_masc.xlsx","SAL_2012_masc.xlsx",
                  "SAL_2013_masc.xlsx","SAL_2014_masc.xlsx","SAL_2015_masc.xlsx","SAL_2016_masc.xlsx",
                  "SAL_2017_masc.xlsx","SAL_2018_masc.xlsx","SAL_2019_masc.xlsx","SAL_2020_masc.xlsx",
                  "SAL_2021_masc.xlsx","SAL_2022_masc.xlsx")
archivos_SCR <- c("SCR_2005_masc.xlsx","SCR_2006_masc.xlsx","SCR_2007_masc.xlsx","SCR_2008_masc.xlsx",
                  "SCR_2009_masc.xlsx","SCR_2010_masc.xlsx","SCR_2011_masc.xlsx","SCR_2012_masc.xlsx",
                  "SCR_2013_masc.xlsx","SCR_2014_masc.xlsx","SCR_2015_masc.xlsx","SCR_2016_masc.xlsx",
                  "SCR_2017_masc.xlsx","SCR_2018_masc.xlsx","SCR_2019_masc.xlsx","SCR_2020_masc.xlsx",
                  "SCR_2021_masc.xlsx","SCR_2022_masc.xlsx")
archivos_SFE <- c("SFE_2005_masc.xlsx","SFE_2006_masc.xlsx","SFE_2007_masc.xlsx","SFE_2008_masc.xlsx",
                  "SFE_2009_masc.xlsx","SFE_2010_masc.xlsx","SFE_2011_masc.xlsx","SFE_2012_masc.xlsx",
                  "SFE_2013_masc.xlsx","SFE_2014_masc.xlsx","SFE_2015_masc.xlsx","SFE_2016_masc.xlsx",
                  "SFE_2017_masc.xlsx","SFE_2018_masc.xlsx","SFE_2019_masc.xlsx","SFE_2020_masc.xlsx",
                  "SFE_2021_masc.xlsx","SFE_2022_masc.xlsx")
archivos_SGO <- c("SGO_2005_masc.xlsx","SGO_2006_masc.xlsx","SGO_2007_masc.xlsx","SGO_2008_masc.xlsx",
                  "SGO_2009_masc.xlsx","SGO_2010_masc.xlsx","SGO_2011_masc.xlsx","SGO_2012_masc.xlsx",
                  "SGO_2013_masc.xlsx","SGO_2014_masc.xlsx","SGO_2015_masc.xlsx","SGO_2016_masc.xlsx",
                  "SGO_2017_masc.xlsx","SGO_2018_masc.xlsx","SGO_2019_masc.xlsx","SGO_2020_masc.xlsx",
                  "SGO_2021_masc.xlsx","SGO_2022_masc.xlsx")
archivos_SJN <- c("SJN_2005_masc.xlsx","SJN_2006_masc.xlsx","SJN_2007_masc.xlsx","SJN_2008_masc.xlsx",
                  "SJN_2009_masc.xlsx","SJN_2010_masc.xlsx","SJN_2011_masc.xlsx","SJN_2012_masc.xlsx",
                  "SJN_2013_masc.xlsx","SJN_2014_masc.xlsx","SJN_2015_masc.xlsx","SJN_2016_masc.xlsx",
                  "SJN_2017_masc.xlsx","SJN_2018_masc.xlsx","SJN_2019_masc.xlsx","SJN_2020_masc.xlsx",
                  "SJN_2021_masc.xlsx","SJN_2022_masc.xlsx")
archivos_SLU <- c("SLU_2005_masc.xlsx","SLU_2006_masc.xlsx","SLU_2007_masc.xlsx","SLU_2008_masc.xlsx",
                  "SLU_2009_masc.xlsx","SLU_2010_masc.xlsx","SLU_2011_masc.xlsx","SLU_2012_masc.xlsx",
                  "SLU_2013_masc.xlsx","SLU_2014_masc.xlsx","SLU_2015_masc.xlsx","SLU_2016_masc.xlsx",
                  "SLU_2017_masc.xlsx","SLU_2018_masc.xlsx","SLU_2019_masc.xlsx","SLU_2020_masc.xlsx",
                  "SLU_2021_masc.xlsx","SLU_2022_masc.xlsx")
archivos_TDF <- c("TDF_2005_masc.xlsx","TDF_2006_masc.xlsx","TDF_2007_masc.xlsx","TDF_2008_masc.xlsx",
                  "TDF_2009_masc.xlsx","TDF_2010_masc.xlsx","TDF_2011_masc.xlsx","TDF_2012_masc.xlsx",
                  "TDF_2013_masc.xlsx","TDF_2014_masc.xlsx","TDF_2015_masc.xlsx","TDF_2016_masc.xlsx",
                  "TDF_2017_masc.xlsx","TDF_2018_masc.xlsx","TDF_2019_masc.xlsx","TDF_2020_masc.xlsx",
                  "TDF_2021_masc.xlsx","TDF_2022_masc.xlsx")
archivos_TUC <- c("TUC_2005_masc.xlsx","TUC_2006_masc.xlsx","TUC_2007_masc.xlsx","TUC_2008_masc.xlsx",
                  "TUC_2009_masc.xlsx","TUC_2010_masc.xlsx","TUC_2011_masc.xlsx","TUC_2012_masc.xlsx",
                  "TUC_2013_masc.xlsx","TUC_2014_masc.xlsx","TUC_2015_masc.xlsx","TUC_2016_masc.xlsx",
                  "TUC_2017_masc.xlsx","TUC_2018_masc.xlsx","TUC_2019_masc.xlsx","TUC_2020_masc.xlsx",
                  "TUC_2021_masc.xlsx","TUC_2022_masc.xlsx")


nombres_masc <- c(archivos_ARG, archivos_CABA, archivos_PBA, archivos_CAT, archivos_CBA,
                 archivos_CHA, archivos_CHU,
                 archivos_COR, archivos_ER, archivos_FOR, archivos_JUJ, archivos_LAP, archivos_LAR,
                 archivos_MEN, archivos_MIS, archivos_NQN, archivos_RN, archivos_SAL, archivos_SCR,
                 archivos_SFE, archivos_SGO, archivos_SJN, archivos_SLU, archivos_TDF, archivos_TUC)

##### Leo los archivos y asignar a objetos en R ----

# Carpeta de destino
carpeta_destino <- "C:/Users/Mica Gauto/Desktop/INE/INVESTIGACIÓN/DIABETES/CARGa de enfermedad/Procesamiento R/Bases_mort/Falle_quinquenales/masc"

## Armo listas para almacenar datos, por jurisdicción y mascales
datos_ARG <- list() #ARG

for (i in 1:length(archivos_ARG)) {
  ruta_completa <- file.path(carpeta_destino, archivos_ARG[i])
  extension <- tools::file_ext(archivos_ARG[i])
  
  if (extension == "csv") {
    datos_ARG[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_ARG[[i]] <- read_excel(ruta_completa)
  }
}

datos_CABA <- list() #CABA

for (i in 1:length(archivos_CABA)) {
  ruta_completa <- file.path(carpeta_destino, archivos_CABA[i])
  extension <- tools::file_ext(archivos_CABA[i])
  
  if (extension == "csv") {
    datos_CABA[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_CABA[[i]] <- read_excel(ruta_completa)
  }
}

datos_PBA <- list() #PBA

for (i in 1:length(archivos_PBA)) {
  ruta_completa <- file.path(carpeta_destino, archivos_PBA[i])
  extension <- tools::file_ext(archivos_PBA[i])
  
  if (extension == "csv") {
    datos_PBA[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_PBA[[i]] <- read_excel(ruta_completa)
  }
}

datos_CAT <- list() #CAT

for (i in 1:length(archivos_CAT)) {
  ruta_completa <- file.path(carpeta_destino, archivos_CAT[i])
  extension <- tools::file_ext(archivos_CAT[i])
  
  if (extension == "csv") {
    datos_CAT[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_CAT[[i]] <- read_excel(ruta_completa)
  }
}

datos_CBA <- list() #CBA

for (i in 1:length(archivos_CBA)) {
  ruta_completa <- file.path(carpeta_destino, archivos_CBA[i])
  extension <- tools::file_ext(archivos_CBA[i])
  
  if (extension == "csv") {
    datos_CBA[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_CBA[[i]] <- read_excel(ruta_completa)
  }
}

datos_COR <- list() #COR

for (i in 1:length(archivos_COR)) {
  ruta_completa <- file.path(carpeta_destino, archivos_COR[i])
  extension <- tools::file_ext(archivos_COR[i])
  
  if (extension == "csv") {
    datos_COR[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_COR[[i]] <- read_excel(ruta_completa)
  }
}

datos_CHA <- list() #CHA

for (i in 1:length(archivos_CHA)) {
  ruta_completa <- file.path(carpeta_destino, archivos_CHA[i])
  extension <- tools::file_ext(archivos_CHA[i])
  
  if (extension == "csv") {
    datos_CHA[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_CHA[[i]] <- read_excel(ruta_completa)
  }
}

datos_CHU <- list() #CHU

for (i in 1:length(archivos_CHU)) {
  ruta_completa <- file.path(carpeta_destino, archivos_CHU[i])
  extension <- tools::file_ext(archivos_CHU[i])
  
  if (extension == "csv") {
    datos_CHU[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_CHU[[i]] <- read_excel(ruta_completa)
  }
}

datos_ER <- list() #ER

for (i in 1:length(archivos_ER)) {
  ruta_completa <- file.path(carpeta_destino, archivos_ER[i])
  extension <- tools::file_ext(archivos_ER[i])
  
  if (extension == "csv") {
    datos_ER[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_ER[[i]] <- read_excel(ruta_completa)
  }
}

datos_FOR <- list() #FOR

for (i in 1:length(archivos_FOR)) {
  ruta_completa <- file.path(carpeta_destino, archivos_FOR[i])
  extension <- tools::file_ext(archivos_FOR[i])
  
  if (extension == "csv") {
    datos_FOR[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_FOR[[i]] <- read_excel(ruta_completa)
  }
}

datos_JUJ <- list() #JUJ

for (i in 1:length(archivos_JUJ)) {
  ruta_completa <- file.path(carpeta_destino, archivos_JUJ[i])
  extension <- tools::file_ext(archivos_JUJ[i])
  
  if (extension == "csv") {
    datos_JUJ[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_JUJ[[i]] <- read_excel(ruta_completa)
  }
}

datos_LAP <- list() #LAP

for (i in 1:length(archivos_LAP)) {
  ruta_completa <- file.path(carpeta_destino, archivos_LAP[i])
  extension <- tools::file_ext(archivos_LAP[i])
  
  if (extension == "csv") {
    datos_LAP[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_LAP[[i]] <- read_excel(ruta_completa)
  }
}

datos_LAR <- list() #LAR

for (i in 1:length(archivos_LAR)) {
  ruta_completa <- file.path(carpeta_destino, archivos_LAR[i])
  extension <- tools::file_ext(archivos_LAR[i])
  
  if (extension == "csv") {
    datos_LAR[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_LAR[[i]] <- read_excel(ruta_completa)
  }
}

datos_MEN <- list() #MEN

for (i in 1:length(archivos_MEN)) {
  ruta_completa <- file.path(carpeta_destino, archivos_MEN[i])
  extension <- tools::file_ext(archivos_MEN[i])
  
  if (extension == "csv") {
    datos_MEN[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_MEN[[i]] <- read_excel(ruta_completa)
  }
}

datos_MIS <- list() #MIS

for (i in 1:length(archivos_MIS)) {
  ruta_completa <- file.path(carpeta_destino, archivos_MIS[i])
  extension <- tools::file_ext(archivos_MIS[i])
  
  if (extension == "csv") {
    datos_MIS[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_MIS[[i]] <- read_excel(ruta_completa)
  }
}

datos_NQN <- list() #NQN

for (i in 1:length(archivos_NQN)) {
  ruta_completa <- file.path(carpeta_destino, archivos_NQN[i])
  extension <- tools::file_ext(archivos_NQN[i])
  
  if (extension == "csv") {
    datos_NQN[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_NQN[[i]] <- read_excel(ruta_completa)
  }
}

datos_RN <- list() #RN

for (i in 1:length(archivos_RN)) {
  ruta_completa <- file.path(carpeta_destino, archivos_RN[i])
  extension <- tools::file_ext(archivos_RN[i])
  
  if (extension == "csv") {
    datos_RN[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_RN[[i]] <- read_excel(ruta_completa)
  }
}

datos_SAL <- list() #SAL

for (i in 1:length(archivos_SAL)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SAL[i])
  extension <- tools::file_ext(archivos_SAL[i])
  
  if (extension == "csv") {
    datos_SAL[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SAL[[i]] <- read_excel(ruta_completa)
  }
}

datos_SCR <- list() #SCR

for (i in 1:length(archivos_SCR)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SCR[i])
  extension <- tools::file_ext(archivos_SCR[i])
  
  if (extension == "csv") {
    datos_SCR[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SCR[[i]] <- read_excel(ruta_completa)
  }
}

datos_SFE <- list() #SFE

for (i in 1:length(archivos_SFE)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SFE[i])
  extension <- tools::file_ext(archivos_SFE[i])
  
  if (extension == "csv") {
    datos_SFE[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SFE[[i]] <- read_excel(ruta_completa)
  }
}

datos_SGO <- list() #SGO

for (i in 1:length(archivos_SGO)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SGO[i])
  extension <- tools::file_ext(archivos_SGO[i])
  
  if (extension == "csv") {
    datos_SGO[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SGO[[i]] <- read_excel(ruta_completa)
  }
}

datos_SJN <- list() #SJN

for (i in 1:length(archivos_SJN)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SJN[i])
  extension <- tools::file_ext(archivos_SJN[i])
  
  if (extension == "csv") {
    datos_SJN[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SJN[[i]] <- read_excel(ruta_completa)
  }
}

datos_SLU <- list() #SLU

for (i in 1:length(archivos_SLU)) {
  ruta_completa <- file.path(carpeta_destino, archivos_SLU[i])
  extension <- tools::file_ext(archivos_SLU[i])
  
  if (extension == "csv") {
    datos_SLU[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_SLU[[i]] <- read_excel(ruta_completa)
  }
}

datos_TDF <- list() #TDF

for (i in 1:length(archivos_TDF)) {
  ruta_completa <- file.path(carpeta_destino, archivos_TDF[i])
  extension <- tools::file_ext(archivos_TDF[i])
  
  if (extension == "csv") {
    datos_TDF[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_TDF[[i]] <- read_excel(ruta_completa)
  }
}

datos_TUC <- list() #TUC

for (i in 1:length(archivos_TUC)) {
  ruta_completa <- file.path(carpeta_destino, archivos_TUC[i])
  extension <- tools::file_ext(archivos_TUC[i])
  
  if (extension == "csv") {
    datos_TUC[[i]] <- read.csv(ruta_completa, encoding = "UTF-8")
  } else if (extension == "xlsx"|extension == "xls") {
    datos_TUC[[i]] <- read_excel(ruta_completa)
  }
}

## Agrego año y jurisdicción

lista_año <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
               2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)

for (i in 1:length(datos_ARG)) {
  datos_ARG[[i]] <- datos_ARG[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "ARG")
}

for (i in 1:length(datos_CABA)) {
  datos_CABA[[i]] <- datos_CABA[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "CABA")
}

for (i in 1:length(datos_PBA)) {
  datos_PBA[[i]] <- datos_PBA[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "PBA")
}

for (i in 1:length(datos_CAT)) {
  datos_CAT[[i]] <- datos_CAT[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "CAT")
}

for (i in 1:length(datos_CBA)) {
  datos_CBA[[i]] <- datos_CBA[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "CBA")
}

for (i in 1:length(datos_COR)) {
  datos_COR[[i]] <- datos_COR[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "COR")
}

for (i in 1:length(datos_CHA)) {
  datos_CHA[[i]] <- datos_CHA[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "CHA")
}

for (i in 1:length(datos_CHU)) {
  datos_CHU[[i]] <- datos_CHU[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "CHU")
}

for (i in 1:length(datos_ER)) {
  datos_ER[[i]] <- datos_ER[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "ER")
}

for (i in 1:length(datos_FOR)) {
  datos_FOR[[i]] <- datos_FOR[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "FOR")
}

for (i in 1:length(datos_JUJ)) {
  datos_JUJ[[i]] <- datos_JUJ[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "JUJ")
}

for (i in 1:length(datos_LAP)) {
  datos_LAP[[i]] <- datos_LAP[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "LAP")
}

for (i in 1:length(datos_LAR)) {
  datos_LAR[[i]] <- datos_LAR[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "LAR")
}

for (i in 1:length(datos_MEN)) {
  datos_MEN[[i]] <- datos_MEN[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "MEN")
}

for (i in 1:length(datos_MIS)) {
  datos_MIS[[i]] <- datos_MIS[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "MIS")
}

for (i in 1:length(datos_NQN)) {
  datos_NQN[[i]] <- datos_NQN[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "NQN")
}

for (i in 1:length(datos_RN)) {
  datos_RN[[i]] <- datos_RN[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "RN")
}

for (i in 1:length(datos_SAL)) {
  datos_SAL[[i]] <- datos_SAL[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SAL")
}

for (i in 1:length(datos_SCR)) {
  datos_SCR[[i]] <- datos_SCR[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SCR")
}

for (i in 1:length(datos_SFE)) {
  datos_SFE[[i]] <- datos_SFE[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SFE")
}

for (i in 1:length(datos_SGO)) {
  datos_SGO[[i]] <- datos_SGO[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SGO")
}

for (i in 1:length(datos_SJN)) {
  datos_SJN[[i]] <- datos_SJN[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SJN")
}

for (i in 1:length(datos_SLU)) {
  datos_SLU[[i]] <- datos_SLU[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "SLU")
}

for (i in 1:length(datos_TDF)) {
  datos_TDF[[i]] <- datos_TDF[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "TDF")
}

for (i in 1:length(datos_TUC)) {
  datos_TUC[[i]] <- datos_TUC[[i]] %>%
    mutate(AÑO = lista_año[[i]],
           JURI = "TUC")
}


## Filtro fallecimientos con causa DM --> CHEQUEAR ORDEN
datos_masc <- c(datos_ARG, datos_CABA, datos_PBA, datos_CAT, datos_CBA,
               datos_CHA, datos_CHU, 
               datos_COR, datos_ER, datos_FOR, datos_JUJ, datos_LAP,
               datos_LAR, datos_MEN, datos_MIS, datos_NQN, datos_RN, datos_SAL,
               datos_SCR, datos_SFE, datos_SGO, datos_SJN, datos_SLU, datos_TDF,
               datos_TUC)

for (i in 1:length(datos_masc)) {
  datos_masc[[i]] <- datos_masc[[i]] %>% 
    filter(`GRUPO DE CAUSAS DE MUERTE` == "3. Diabetes mellitus")
}

## Asigno a objetos con nombres específicos
#nombres_masc
#view(datos_masc[163])

for (i in 1:length(nombres_masc)) {
  assign(paste0(gsub(".xlsx", "", nombres_masc[[i]])), datos_masc[[i]])
}

## Uno todas las bases
bases_masc <- bind_rows(datos_masc)
prueba <- bases_masc %>% filter(AÑO == 2014) %>% select(TOTAL, AÑO, JURI)

## Pivoteo para armar grupo de edad
bases_masc <- bases_masc %>% 
  pivot_longer(cols = 3:22, names_to = "GRUP_EDAD", values_to = "RECUENTO_FALLE_masc") %>% 
  select(`GRUPO DE CAUSAS DE MUERTE`, JURI, AÑO, GRUP_EDAD, RECUENTO_FALLE_masc) #reordeno columnas, saco la de falle mascal


### Unión de bases final ----
rm(list=setdiff(ls(), c("bases_tot", "bases_fem", "bases_masc")))

serie_falle <- bases_tot %>% 
  left_join(bases_fem, by = join_by("GRUPO DE CAUSAS DE MUERTE" == "GRUPO DE CAUSAS DE MUERTE",
                                    "JURI" == "JURI", "AÑO" == "AÑO", "GRUP_EDAD" == "GRUP_EDAD")) %>% 
  left_join(bases_masc, by = join_by("GRUPO DE CAUSAS DE MUERTE" == "GRUPO DE CAUSAS DE MUERTE",
                                    "JURI" == "JURI", "AÑO" == "AÑO", "GRUP_EDAD" == "GRUP_EDAD"))

# Guardar el dataset en .RData, archivo binario liviano, sólo se abre en R 
save(serie_falle,
     file = "serie_falle.RData")

#load("serie_falle.RData")
