library(tidyverse)
library(ofce)
library(openxlsx)
library(ggh4x)
library(plotly)
library(ggiraph)

deficits_data <- read.xlsx("decompo_deficits.xlsx", sheet = "Deficits %PIB") |> 
  pivot_longer(-c(geo,mesure), names_to = "annee", values_to = "value")

deficits_data_pb <- deficits_data |> 
  filter(mesure == "total" & annee %in% c(2019,2023,2025)) |> 
  group_by(geo) |> 
  pivot_wider(names_prefix = "d_", names_from = "annee", values_from = "value") |> 
  mutate(consolid = d_2025-d_2023,
         lollipop_start = d_2023,
         d_2023 = case_when(d_2019 <0 ~  d_2023-d_2019,
                            TRUE ~ d_2023),
         across(where(is.numeric), ~ round(., digits = 1))
         ) |> 
  pivot_longer(c(d_2019,d_2023), names_to = "annee", values_to= "value" ) |> 
  mutate(annee = factor(str_remove(annee,"d_"), levels = c("2019", "2023")),
         tooltip_d_2019_2023 = case_when(annee == "2023" ~ str_c("Variation du déficit 2019-2023 : <br>", lollipop_start-value[annee=="2019"]),
                                         annee == "2019" ~ str_c("Déficit 2019 :<br>", value[annee=="2019"])))
  

graph_deficits_pbinter <-
  (ggplot(deficits_data_pb |> drop_na(),
         aes(x=geo, y = value, fill = fct_rev(annee),  data_id=annee, tooltip=tooltip_d_2019_2023))+
    geom_bar_interactive(stat="identity", position = "stack")+
    theme_ofce()+
    scale_fill_manual(values = c("firebrick3", "dodgerblue4"), name = "")+
    geom_segment_interactive(aes(x=geo, xend=geo, y=lollipop_start, yend=d_2025, data_id="consolid", tooltip =str_c("Variation du déficit 2023-2025 : <br>",consolid)),size=1.2)+
    geom_point_interactive(aes(y=d_2025, data_id=d_2025, tooltip= str_c("Déficit 2025 (prévision) : <br>", d_2025), color = "2025"), size = 3)+
    xlab("")+
    ylab("% du PIB")+
    scale_colour_manual(values = "black", name="")+
    guides(fill = guide_legend(override.aes = list(shape = NA)),
           colour = guide_legend(order=1))+
    labs(caption = "Sources: FRED, Eurostat, prévisions OFCE. <br><i>Note de lecture : le solde public allemand était de 1.5% du PIB en 2019 et de -2.5% en <br> 2023. Il atteindrait -1.4% en 2025 selon les prévisions OFCE.</i>")
    
  )

print(graph_deficits_pbinter)
graph_deficits_pbinter<- girafe(ggobj= graph_deficits_pbinter,
                                options = list(opts_sizing(rescale = TRUE, width = .5)),
       )

graph_deficits_pbinter

save(list = ls(pattern="^graph_.+$"), 
     file = "graphs_pbinter_fipu.rda")


save(list = ls(pattern="^graph_.+$"), 
     file = "docs/graphs_pbinter_fipu.rda")
