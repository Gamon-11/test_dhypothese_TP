################################################################################
# TEST D'HYPOTHESE
# TP2 Cadre paramétrique
# Auteur : GAMONDELE Maxime
# 30/09/24
################################################################################



#' ---
#' title : |
#'         |
#'         | \textcolor{purple}{\Huge TP Test d'hypothèses}
#'         | \textcolor{blue}{\Large Comparaison de moyennes - Cas de données appariées}
#' subtitle : | 
#'            |
#'            | IUT Grand Ouest Normandie
#'            | Département - Sciences des Données
#'            | Campus - Lisieux
#' author : "Maxime Gamondele"
#' date : 07/09/2024
#' fontsize: 11pt
#' ---
#' =========================================================================== #
#' \begin{center} \bf{chargement des librairies} \end{center}
#' =========================================================================== #


library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

library(ggpubr)
library(purrr)
library(qqplotr)

library(rlang)
library(ggstatsplot)


library(rstatix) #t_test
library(ggstatsplot) # ggbetweenstats()

library(gginference)

################################################################################

readLines("../Data/Experiment.csv",n=5)

read.table(file= "../Data/Experiment.csv", header = TRUE) -> dataframe
str(dataframe)
#' =========================================================================== #
#' \begin{center} \bf{préparation des données} \end{center}
#' =========================================================================== #
dataframe |>
  pivot_longer(cols= c(weight_before,weight_after),
               names_to = "Treatment",
               values_to = "weight",
               names_pattern = "_(.*)") |>
  mutate(Treatment = factor(Treatment,
                            ordered = TRUE,
                            levels = c("before","after")))-> data
str(data)

#' =========================================================================== #
#' \begin{center} \bf{Analyse exploratoire des données} \end{center}
#' =========================================================================== #

data |>
group_by(Treatment) |>
summarize(count=n(),
          Mean = mean(weight, na.rm = TRUE),
          sd = sd(weight,na.rm = TRUE))

data |>
  ggplot(mapping = aes(x = Treatment,
                       y = weight))+
  geom_violin(mapping = aes(fill=Treatment),
              alpha = 0.3)+
  geom_point(mapping = aes(fill = Treatment),
             shape = 21,
             size = 2,
             colour = "black",
             show.legend = FALSE)+
  scale_x_discrete(labels=c("Avant","Aprés"))+
  scale_fill_manual(name = "Traitement",
                    values = c("purple","yellow3"),
                    labels = c("Avant","Aprés"))+
  geom_segment(data = data |>
                 pivot_wider(names_from = Treatment,
                             values_from = weight,
                             values_fn = list)|>
                 unnest(cols = c("before","after")),
               mapping = aes(x = 1,
                             xend = 2,
                             y = before,
                             yend = after))+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 15),
        plot.subtitle = element_text(hjust = 0.5,
                                     colour = "red4",
                                     margin = margin(b=10)),
        plot.caption = element_text(hjust = c(0,1),
                                    face = "bold"))+
  labs(title = "Évaluation du poids avant et aprés traitement",
       subtitle = "Source : Simulation",
       x = "Traitement",
       y = "Poids",
       caption = c("BUT Science des Données", "Auteur : Maxime Gamondele"))

#' =========================================================================== #
#' \begin{center} \bf{Test d'hypothèse} \end{center}
#' =========================================================================== #

data |>
  pivot_wider(names_from = Treatment,
              values_from = weight,
              values_fn = list)|>
  unnest(cols = c("before","after"))|>
  mutate(diff = after - before) -> global.data

str(global.data)

global.data|>
  ggplot(mapping = aes(x = diff))+
  geom_density(fill = "pink",
               alpha = 0.4,
               bw = 8)+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 15),
        plot.subtitle = element_text(hjust = 0.5,
                                     colour = "red4",
                                     margin = margin(b=10)),
        plot.caption = element_text(hjust = c(0,1),
                                    face = "bold"))+
  labs(title = "Distribution de la variable 'diff = after - before'",
       subtitle = "Source : Simulation",
       x = "Différence de poids entre avant et après traitement",
       y = "Densité",
       caption = c("BUT Science des Données", "Auteur : Maxime Gamondele"))





