################################################################################
# TEST D'HYPOTHESE
# TP1 Cadre non-paramétrique
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

############################# ETAPE 1 #############################

#1. Ensmeble des 500 individus supposé etre la population d'interet
#2. Poids quantittive continue
#3. u0 moyenne théorique de l'ensemble des patients de 100 kg

############################# ETAPE 3 #############################

# chargement fichier

readLines("../Data/data1.csv",n=5)
data = read.table("../Data/data1.csv", sep = ",", header = TRUE)
head(data)
str(data)

############################# ETAPE 4 #############################

# Préparation des données

data %>% 
  mutate(Gender = factor(Gender)) %>% str()

# Statistique Exploratoire

data %>% 
  get_summary_stats()

# Observe-t-on la pr ́esence de donn ́ees manquantes ? Quelle est le poids moyen th ́eorique pour la variable Weight dans le cas pr ́esent ?
# Non = 500 partout, le poids moyen théorique est de 106 kg.


# Séléction d'un échantillon de taille n = 40

set.seed(seed = 125)
data %>% 
  sample_n(size = 40,
           replace = FALSE) -> sample
print(sample)

############################# ETAPE 5 #############################

sample %>% 
  get_summary_stats()


sample %>% 
  ggdensity(x = "Weight",
            fill = "pink",
            alpha = 0.4)+
  scale_x_continuous(limits = c(20,180))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold"))+
  labs(title = "Représentation de la densité lissée de la variable Poids\n",
       xlab = "Poids en kg",
       ylab = "Densité")

sample %>% 
  ggqqplot(data = .,
           x = "Weight",
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

# Test normalité Shapiro-Wilk

sample %>% 
  pull(Weight) %>% 
  shapiro.test(x=.)      
# Interpretation ...


############################# ETAPE 6 #############################

# Statistique inférentielle : test d'hypothèses

mu_0 = 100
alpha = 0.05

sample %>% 
  summarize(W = sqrt(length(Weight))*(mean(Weight)-mu_0)/sd(Weight)) %>% 
  mutate(Critical_Value = qnorm(p=1-alpha),
         p_value = 1-pnorm(W))

sample %>% 
  pull(Weight) %>% 
  t.test(x=.,
         alternative = "less",
         mu = 100)

print(result3)    

