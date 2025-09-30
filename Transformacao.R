library(readxl)
library(sf)
library(dplyr)

#importações dos dados:
library(readr)
dados <- read_csv("Global_Education.csv")

shapefile <- st_read("shapefile_world")

#limpando banco completo:
banco_completo <- dados %>%
  filter(if_all(everything(), ~ !is.na(.)))

#países que nao foi possível juntar com o shapefile
paises_nao_agrupados <- setdiff(
  banco_completo$`Countries and areas`,
  shapefile$name
)

paises_nao_agrupados


banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Antigua and Barbuda"
] <- "Antigua & Barbuda"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "The Bahamas"
] <- "Bahamas"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Bosnia and Herzegovina"
] <- "Bosnia & Herzegovina"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Brunei"
] <- "Brunei Darussalam"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Republic of the Congo"
] <- "Democratic Republic of the Congo"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Ivory Coast"
] <- "Côte d'Ivoire"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "North Korea"
] <- "Democratic People's Republic of Korea"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Eswatini"
] <- "Swaziland"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "The Gambia"
] <- "Gambia"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Guinea0Bissau"
] <- "Guinea-Bissau"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Vatican City"
] <- "Holy See"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Iran"
] <- "Iran (Islamic Republic of)"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Republic of Ireland"
] <- "Ireland"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Laos"
] <- "Lao People's Democratic Republic"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Libya"
] <- "Libyan Arab Jamahiriya"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Federated States of Micronesia"
] <- "Micronesia (Federated States of)"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "North Macedonia"
] <- "The former Yugoslav Republic of Macedonia"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "South Korea"
] <- "Republic of Korea"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Moldova"
] <- "Moldova, Republic of"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Russia"
] <- "Russian Federation"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Syria"
] <- "Syrian Arab Republic"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "East Timor"
] <- "Timor-Leste"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "United Kingdom"
] <- "U.K. of Great Britain and Northern Ireland"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Tanzania"
] <- "United Republic of Tanzania"

banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "United States"
] <- "United States of America"

# Conferir
banco_completo$`Countries and areas`[
  banco_completo$`Countries and areas` == "Palestinian National Authority"
] <- "West Bank"

banco_completo$`Countries and areas`[
  banco_completo$Latitude == 0.186360 & banco_completo$Longitude == 6.613081
] <- "Sao Tome and Principe"

#países que nao foi possível juntar com o shapefile
paises_nao_agrupados <- setdiff(
  banco_completo$`Countries and areas`,
  shapefile$name
)

paises_nao_agrupados

# Renomeando colunas
colnames(banco_completo)
library(dplyr)

banco_completo <- banco_completo %>%
  rename(
    `Países e áreas` = `Countries and areas`,
    `Latitude` = `Latitude`,
    `Longitude` = `Longitude`,
    `Taxa fora da escola - pré-escola masculino` = `OOSR_Pre0Primary_Age_Male`,
    `Taxa fora da escola - pré-escola feminino` = `OOSR_Pre0Primary_Age_Female`,
    `Taxa fora da escola - ensino primário masculino` = `OOSR_Primary_Age_Male`,
    `Taxa fora da escola - ensino primário feminino` = `OOSR_Primary_Age_Female`,
    `Taxa fora da escola - ensino fundamental 1 masculino` = `OOSR_Lower_Secondary_Age_Male`,
    `Taxa fora da escola - ensino fundamental 1 feminino` = `OOSR_Lower_Secondary_Age_Female`,
    `Taxa fora da escola - ensino fundamental 2 masculino` = `OOSR_Upper_Secondary_Age_Male`,
    `Taxa fora da escola - ensino fundamental 2 feminino` = `OOSR_Upper_Secondary_Age_Female`,
    `Taxa de conclusão - primário masculino` = `Completion_Rate_Primary_Male`,
    `Taxa de conclusão - primário feminino` = `Completion_Rate_Primary_Female`,
    `Taxa de conclusão - fundamental 1 masculino` = `Completion_Rate_Lower_Secondary_Male`,
    `Taxa de conclusão - fundamental 1 feminino` = `Completion_Rate_Lower_Secondary_Female`,
    `Taxa de conclusão - fundamental 2 masculino` = `Completion_Rate_Upper_Secondary_Male`,
    `Taxa de conclusão - fundamental 2 feminino` = `Completion_Rate_Upper_Secondary_Female`,
    `Proficiência em leitura 2º-3º ano` = `Grade_2_3_Proficiency_Reading`,
    `Proficiência em matemática 2º-3º ano` = `Grade_2_3_Proficiency_Math`,
    `Proficiência em leitura final do primário` = `Primary_End_Proficiency_Reading`,
    `Proficiência em matemática final do primário` = `Primary_End_Proficiency_Math`,
    `Proficiência em leitura final do fundamental 1` = `Lower_Secondary_End_Proficiency_Reading`,
    `Proficiência em matemática final do fundamental 1` = `Lower_Secondary_End_Proficiency_Math`,
    `Taxa de alfabetização 15-24 anos masculino` = `Youth_15_24_Literacy_Rate_Male`,
    `Taxa de alfabetização 15-24 anos feminino` = `Youth_15_24_Literacy_Rate_Female`,
    `Taxa de natalidade` = `Birth_Rate`,
    `Matrícula bruta no primário` = `Gross_Primary_Education_Enrollment`,
    `Matrícula bruta no ensino superior` = `Gross_Tertiary_Education_Enrollment`,
    `Taxa de desemprego` = `Unemployment_Rate`
  )


library(dplyr)
library(sf)
library(leaflet)

#juntando com o shapefile
banco_sf <- banco_completo %>%
  left_join(shapefile, by = c("Países e áreas" = "name"))

banco_sf = st_as_sf(banco_sf)
