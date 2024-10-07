################################################################################
# TEST D'HYPOTHESE
# TP1 Cadre paramétrique
# Auteur
# 30/09/24
################################################################################

# Chargement des librairies

library(dplyr)
library(ggplot2)

library(LearningStats)
library(report)
library(ggpubr)
library(rstatix)

# Etape 1 

#1. Ensmeble des patients dont le poid est à 100 kg
#2. Poids quantittive continue
#3. u0 moyenne théorique de l'ensemble des patients de 100 kg


############################# ETAPE 3 #############################
# chargement fichier

readLines("../Data/Etude_fibre.csv",n=5)
data = read.table("../Data/Etude_fibre.csv", sep = ";", header = TRUE)
head(data)
str(data)

############################# ETAPE 4 #############################
# Analyse exploratoire des données

data %>%
  summary()

data %>% 
  ggplot(mapping = aes(x = Poids))+
  geom_density(fill = "darkblue",
               alpha = 0.4)+
  scale_x_continuous(limits = c(80,120))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold"))+
  labs(title = "Représentation de la densité lissée de la variable Poids\n",
       xlab = "Poids en kg",
       ylab = "Densité")


data %>% 
  ggdensity(x = "Poids",
            fill = "pink",
            alpha = 0.4)+
  scale_x_continuous(limits = c(80,120))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold"))+
  labs(title = "Représentation de la densité lissée de la variable Poids\n",
       xlab = "Poids en kg",
       ylab = "Densité")

data %>% 
  ggqqplot(data = .,
           x = "Poids",
           color = "blue",
           conf.int = TRUE,
           conf.int.level = 0.95,
           title = "Q-Q plot pour la variable poids\n",
           ylab = "Poids des patients",
           ggtheme = theme_minimal())+
  geom_qq(color = "black")+ #couleur des points 
  geom_qq_line(color = "red",
               linewidth = 1.2)+
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold"))
# Selon vous peut-on, au vu de cette courbe, rejeter l'hypothèse d'une distribution gausienne?

# Non car il y a une coresspondance entre les quantiles de notres variable et les quantiles de la loi normal

# Test normalité Shapiro-Wilk

data %>% 
  pull(Poids) %>% 
  shapiro.test(x=.)
# Loi normal p value = 0.192 donc pas d'évidence à l'encontre de la conservation de h0 p>0,05


############################# ETAPE 5 #############################

# réalisation test d'ihypothese

data %>% 
  pull(Poids) %>% 
  t.test(x=.,
         alternative = "less",
         mu = 100)

# Peut-on rejeter l’hypoth`ese nulle avec un niveau de signification de 5%? Justifier votre r ́eponse. En d ́eduire une conclusion contextuelle sur le r ́egime `a base de fibre pour la perte de poids chez les personnes en situation d’ob ́esit ́e.


data %>% 
  pull(Poids) %>% 
  Mean.test(x=.,
            mu0 = 100,
            sigma = NULL,
            alternative = "less",
            alpha = 0.05,
            plot = TRUE) #greater pour sup à u
# Que repr ́esente le segment RR repr ́esent ́e en rouge sur le graphique ?


data %>% 
  t_test(data = .,
         formula = Poids ~ 1,
         alternative = "less",
         mu = 100,
         conf.level = 0.95) -> result3

print(result3)

data %>%  ggplot(mapping = aes(y = Poids,
                               x = NA))+
  geom_boxplot(width = 0.5,
               fill = "lightblue")+
  geom_jitter(width = 0.12,
              colour = "red")+
  stat_summary(fun = "mean",
               geom = "point",
               color = "black",
               fill = "purple",
               shape = 21,
               size = 2)+
  stat_summary(fun = "mean",
               colour = "black",
               geom = "text",
               vjust = +0.5,
               hjust = -0.7,
               mapping = aes(label = round(after_stat(y),
                                           digits = 1)))+
  scale_y_continuous(limits = c(87,105))+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+
  labs(title = "Distribution de la variable sous la forme d'un box plot")
  




data %>% 
  mutate(y = NA) %>% 
  ggboxplot(x = "y",
            y = "Poids",
            width = 0.5,
            fill = "lightblue",
            add = c("jitter"),
            add.params = list(color = c("red")),
            ylab = "poids exprimé en kilogramme")+
  stat_mean(geom = "point",
            color = "black",
            fill = "purple",
            shape = 21,
            size = 2)+
  stat_mean(colour = "black",
            geom = "text",
            vjust = +0.5,
            hjust = -0.7,
            mapping = aes(label = round(after_stat(y),
                                        digits = 1)))+
  scale_y_continuous(limits = c(87,105))+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+
  labs(title = "Distribution de la variable sous la forme d'un boxplot\n",
       subtitle = get_test_label(stat.test = result3,
                                 detailed = TRUE),
       x= NULL)
