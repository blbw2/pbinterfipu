library(tidyverse)
library(openxlsx)
library(zoo)
library(ggiraph)
library(ofce)
library(patchwork)


data_salaires <- read.xlsx("data_salaires.xlsx", sheet = "Sheet1") |> 
  mutate(smpt_reel = smpt/hicp,
         obsTime = as.yearqtr(obsTime)) |> 
  group_by(geo) |> 
  mutate(across(hicp:smpt_reel, ~ (100*./(.[obsTime ==2019.75]))-100))




data_salaires_reshaped <- data_salaires|> 
  filter(obsTime ==2024.25 | obsTime ==2025.75) |> 
  select(-hicp, smpt,smpt_reel) |> 
  pivot_longer(-c(obsTime,geo), names_to = "variable", values_to = "value") |> 
  group_by(geo,variable) |> 
  summarise(historique = value[obsTime==2024.25],
            prev = value[obsTime == 2025.75]-value[obsTime==2024.25],
            endpoint = value[obsTime == 2025.75]) |> 
  pivot_longer(historique:endpoint, names_to="periode", values_to = "value" ) |> 
  ungroup() |> 
  mutate(periode = fct_rev(factor(periode, levels = c("historique","prev","endpoint"))),
         geo=fct_reorder(.f= factor(geo), .x = value))

str(data_salaires_reshaped$periode)
graph_Salairesnom <- ggplot(data_salaires_reshaped |> filter(variable == "smpt" & (!periode == "endpoint")),
                           aes(y=geo) )+
  geom_bar(position = "stack", stat = "identity",
           aes (x=value, y=geo, fill = periode),
           alpha =1,
           width= 0.4)+
  geom_vline(aes(xintercept =0))+
  geom_point(data=data_salaires_reshaped |> filter(variable == "smpt" & (periode == "endpoint")),
             aes(x = value, y = geo),  size = 2, colour = "firebrick3")+
  scale_fill_manual(values = c("darkgoldenrod3","royalblue4"), 
                     breaks = c("prev", "historique"),
                     labels = c("2025 T4- 2024 T2", "2024 T2- 2019 T4"  ),
                     # name = "Contributions à\nl'évolution totale\n2024 T4 - 2019 T4",
                     name = "")+
  expand_limits(x = c(0,40))+
  scale_x_continuous(limits = c(-10,35), breaks = seq(-10,35, by = 5))+
  guides(fill = guide_legend(byrow= TRUE))+
  labs( x = "Evolution totale (% du niveau 2019 T4)",
        colour = "", fill = "Croissance des salaires",
        title = "Salaires Nominaux")+
  theme_ofce()

graph_Salairesnom

graph_Salairesreels <- ggplot(data_salaires_reshaped |> filter(variable == "smpt_reel" & (!periode == "endpoint")),
                            aes(y=geo) )+
  geom_bar(position = "stack", stat = "identity",
           aes (x=value, y=geo, fill = periode),
           alpha =1,
           width= 0.4)+
  geom_vline(aes(xintercept =0))+
  geom_point(data=data_salaires_reshaped |> filter(variable == "smpt_reel" & (periode == "endpoint")),
             aes(x = value, y = geo),  size = 2, colour = "firebrick3")+
  scale_fill_manual(values = c("darkgoldenrod3","royalblue4"), 
                    breaks = c("prev", "historique"),
                    labels = c("2025 T4- 2024 T2", "2024 T2- 2019 T4"  ),
                    # name = "Contributions à\nl'évolution totale\n2024 T4 - 2019 T4",
                    name = "")+
  expand_limits(x = c(0,10))+
  scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10, by = 5))+
  guides(fill = guide_legend(byrow= TRUE))+
  labs( x = "Evolution totale (% du niveau 2019 T4)",
        colour = "", fill = "Croissance des salaires",
        title = "Salaires Réels")+
  theme_ofce()

graph_Salairesreels

data_salaires_reshaped2 <- data_salaires_reshaped |>
  mutate(variable =case_when(variable == "smpt" ~ "Salaires nominaux",
                             variable == "smpt_reel" ~ "Salaires réels"))


graph_Salaires <- ggplot(data_salaires_reshaped2 |> filter((!periode == "endpoint")),
                              aes(y=geo) )+
  facet_wrap(~variable)+
  geom_bar(position = "stack", stat = "identity",
           aes (x=value, y=geo, fill = periode),
           alpha =1,
           width= 0.4)+
  geom_vline(aes(xintercept =0))+
  geom_point(data=data_salaires_reshaped2 |> filter ((periode == "endpoint")),
             aes(x = value, y = geo),  size = 2, colour = "firebrick3")+
  scale_fill_manual(values = c("darkgoldenrod3","royalblue4"), 
                    breaks = c("prev", "historique"),
                    labels = c("2025 T4- 2024 T2", "2024 T2- 2019 T4"  ),
                    # name = "Contributions à\nl'évolution totale\n2024 T4 - 2019 T4",
                    name = "")+
  expand_limits(x = c(0,10))+
  scale_x_continuous(limits = c(-10,35), breaks = seq(-10,35, by = 5))+
  guides(fill = guide_legend(byrow= TRUE))+
  labs( x = "Evolution totale (% du niveau 2019 T4)",
        colour = "", fill = "Croissance des salaires",
        title = "")+
  theme_ofce()

graph_Salaires


##### Version avec 3 périodes #####

ggplot(data_salaires,
       aes(x=obsTime, y= smpt_reel, colour = geo))+
  geom_line()+
  geom_vline(xintercept = 2023)+
  ofce::theme_ofce()

data_salaires_reshaped3p <- data_salaires|> 
  select(-hicp, smpt,smpt_reel) |> 
  pivot_longer(-c(obsTime,geo), names_to = "variable", values_to = "value") |> 
  group_by(geo,variable) |> 
  summarise(historique1 = value[obsTime==2022.75],
            historique2 = value[obsTime == 2024.25]-value[obsTime==2022.75],
            prev = value[obsTime == 2025.75]-value[obsTime==2024.25],
            endpoint = value[obsTime == 2025.75]) |> 
  pivot_longer(historique1:endpoint, names_to="periode", values_to = "value" ) |> 
  ungroup() |> 
  mutate(periode = fct_rev(factor(periode, levels = c("historique1","historique2","prev","endpoint"))),
         geo=factor(geo, levels = c("ITA", "FRA", "DEU", "GBR", "ESP", "USA")),
         variable =case_when(variable == "smpt" ~ "Salaires nominaux",
                             variable == "smpt_reel" ~ "Salaires réels"))


graph_Salaires <- ggplot(data_salaires_reshaped3p |> filter((!periode == "endpoint")),
                         aes(y=geo) )+
  facet_wrap(~variable)+
  geom_bar(position = "stack", stat = "identity",
           aes (x=value, y=geo, fill = periode),
           alpha =1,
           width= 0.4)+
  geom_vline(aes(xintercept =0))+
  geom_point(data=data_salaires_reshaped3p |> filter ((periode == "endpoint")),
             aes(x = value, y = geo),  size = 2, colour = "firebrick3")+
  scale_fill_manual(values = c("darkgoldenrod3","royalblue4", "coral3"), 
                    breaks = c("prev", "historique2", "historique1"),
                    labels = c("2025 T4- 2024 T2", "2024 T2- 2022 T4", "2022 T4 - 2019 T4"),
                    # name = "Contributions à\nl'évolution totale\n2024 T4 - 2019 T4",
                    name = "")+
  expand_limits(x = c(0,10))+
  scale_x_continuous(limits = c(-10,35), breaks = seq(-10,35, by = 5))+
  guides(fill = guide_legend(byrow= TRUE))+
  labs( x = "Evolution totale (% du niveau 2019 T4)",
        colour = "", fill = "Croissance des salaires",
        title = "")+
  theme_ofce()


graph_Salaires

graph_Salaires <- ggplot(data_salaires_reshaped3p |> filter((!periode == "endpoint")),
                         aes(y=geo) )+
  facet_wrap(~variable)+
  geom_bar_interactive(position = "stack", stat = "identity",
           aes (x=value, y=geo, fill = periode, tooltip = str_c("Contribution à la variation depuis 2019 T4 : <br>",
                                                                round(value,digits=2),
                                                                "%")),
           alpha =1,
           width= 0.4)+
  geom_vline(aes(xintercept =0))+
  geom_point_interactive(data=data_salaires_reshaped3p |> filter ((periode == "endpoint")),
             aes(x = value, y = geo, tooltip = str_c("Variation depuis 2019 T4 :<br>", round(value,digits=2), "%")),
                 size = 1, colour = "firebrick3", shape =19
             )+
  scale_fill_manual(values = c("darkgoldenrod3","royalblue4", "coral3"), 
                    breaks = c("prev", "historique2", "historique1"),
                    labels = c("2025 T4- 2024 T2", "2024 T2- 2022 T4", "2022 T4 - 2019 T4"),
                    # name = "Contributions à\nl'évolution totale\n2024 T4 - 2019 T4",
                    name = "")+
  expand_limits(x = c(0,10))+
  scale_x_continuous(limits = c(-10,35), breaks = seq(-10,35, by = 5))+
  guides(fill = guide_legend(byrow= TRUE),
         colour = FALSE,
         size = FALSE)+
  labs( x = "Evolution totale (% du niveau 2019 T4)",
        y = "",
        colour = "", fill = "Croissance des salaires",
        title = "")+
  theme_ofce()

girafe(ggobj = graph_Salaires,
       options = list(opts_sizing(rescale = TRUE, width = .5)))


graph_Salairesnom <- ggplot(data_salaires_reshaped3p |> filter((!periode == "endpoint"), variable == "Salaires nominaux"),
                         aes(y=geo) )+
  geom_bar_interactive(position = "stack", stat = "identity",
                       aes (x=value, y=geo, fill = periode, tooltip = str_c("Contribution à la variation depuis 2019 T4 : <br>",
                                                                            round(value,digits=2),
                                                                            "%")),
                       alpha =1,
                       width= 0.4)+
  geom_vline(aes(xintercept =0))+
  geom_point_interactive(data=data_salaires_reshaped3p |> filter(periode == "endpoint", variable == "Salaires nominaux"),
                         aes(x = value, y = geo, tooltip = str_c("Variation depuis 2019 T4 :<br>", round(value,digits=2), "%")),
                         size = 1, colour = "firebrick3", shape =19
  )+
  scale_fill_manual(values = c("darkgoldenrod3","royalblue4", "coral3"), 
                    breaks = c("prev", "historique2", "historique1"),
                    labels = c("2025 T4- 2024 T2", "2024 T2- 2022 T4", "2022 T4 - 2019 T4"),
                    # name = "Contributions à\nl'évolution totale\n2024 T4 - 2019 T4",
                    name = "")+
  expand_limits(x = c(0,10))+
  scale_x_continuous(limits = c(-10,35), breaks = seq(-10,35, by = 5))+
  guides(fill = guide_legend(byrow= TRUE),
         colour = FALSE,
         size = FALSE)+
  labs( x = "Evolution totale (% du niveau 2019 T4)",
        y= "",
        colour = "", fill = "Croissance des salaires",
        title = "Salaires nominaux")+
  theme_ofce()

graph_Salairesreels <- ggplot(data_salaires_reshaped3p |> filter((!periode == "endpoint"), variable == "Salaires réels"),
                            aes(y=geo) )+
  geom_bar_interactive(position = "stack", stat = "identity",
                       aes (x=value, y=geo, fill = periode, tooltip = str_c("Contribution à la variation depuis 2019 T4 : <br>",
                                                                            round(value,digits=2),
                                                                            "%")),
                       alpha =1,
                       width= 0.4)+
  geom_vline(aes(xintercept =0))+
  geom_point_interactive(data=data_salaires_reshaped3p |> filter ((periode == "endpoint"), variable == "Salaires réels"),
                         aes(x = value, y = geo, tooltip = str_c("Variation depuis 2019 T4 :<br>", round(value,digits=2), "%")),
                         size = 1, colour = "firebrick3", shape =19
  )+
  scale_fill_manual(values = c("darkgoldenrod3","royalblue4", "coral3"), 
                    breaks = c("prev", "historique2", "historique1"),
                    labels = c("2025 T4- 2024 T2", "2024 T2- 2022 T4", "2022 T4 - 2019 T4"),
                    # name = "Contributions à\nl'évolution totale\n2024 T4 - 2019 T4",
                    name = "")+
  expand_limits(x = c(0,10))+
  scale_x_continuous(limits = c(-10,35), breaks = seq(-10,35, by = 5))+
  guides(fill = guide_legend(byrow= TRUE),
         colour = FALSE,
         size = FALSE)+
  labs( x = "Evolution totale (% du niveau 2019 T4)",
        y= "",
        colour = "", fill = "Croissance des salaires",
        title = "Salaires réels")+
  theme_ofce()

graph_Salairespatchwork <- (graph_Salairesreels+graph_Salairesnom)+
  plot_layout(guides = "collect")

graph_Salairespatchwork

save(list =ls(pattern = "^graph_.+$"),
     file ="graph_pbinter_salaires.rda")
