library(tidyverse)
library(ofce)
library(openxlsx)
library(ggh4x)
library(plotly)
library(ggiraph)

deficits_data <- read.xlsx("decompo_deficits.xlsx", sheet = "Deficits %PIB") |> 
  pivot_longer(-c(geo,mesure), names_to = "annee", values_to = "value")