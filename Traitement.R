#################### CHARGEMENT DES BIBLIOTHEQUES ########################

library(readr) 
library(dplyr)
library(readxl) 
library(ggplot2) 
library(lubridate)
library(plotly)
library(ggiraph)
library(haven)
library(leaflet)
library(sf)

  
# ------------------- CHARGEMENT DES DONNÉES -------------------

achats = read.csv2("Achats_csv.csv")
corres <- read.csv2("Correspondance_sites.csv")
clients <- read_sas("clients.sas7bdat")



# Prise en compte des consignes sur la base achat

achats <- achats %>%
  mutate(Date.Achat = dmy(Date.Achat),
         Mois = month(Date.Achat),   # 1 à 12
         Annee = year(Date.Achat)) %>%
  filter(Num.Site != 7)

glimpse(achats)


#Prise en compte des consignes sur la base clients


clients <- clients %>%
  mutate(
    DATE_NAIS = ymd(DATE_NAIS),
    Age = year(Sys.Date()) - year(DATE_NAIS),#Calcul de l'âge des clients
    tranche_age = case_when(
      Age < 30 ~ "Moins de 30",
      Age >= 30 & Age <= 45 ~ "30-45",
      Age > 45 ~ "Plus de 45"
    ),
    Sexe = if_else(COD_SEXE == 1, "Homme", "Femme")#Recodage de l'âge
  )

# Jointure des 3 tables
data_joint <- achats %>%
  left_join(corres, by = c("Num.Site" = "NUM_SITE")) %>%
  left_join(clients, by = c("Id.Client" = "ID_CLIENT"))



#Regroupement par mois du nombre d'achat et du montant d'achat
# Nombre d’achats par mois et site

nb_achats <- data_joint %>%
  group_by(Mois, NOM_SITE) %>%
  summarise(Nb_Achats = sum(Nb.Achat), .groups = "drop")


# Montant total par mois et site

mnt_achats <- data_joint %>%
  group_by(Mois, NOM_SITE) %>%
  summarise(Montant = sum(Mnt.Achat), .groups = "drop") %>% 
  arrange(desc(Montant))


# Comparaisons des indicateurs par sexe

achats_sexe <- data_joint %>%
  group_by(Mois, Sexe) %>%
  summarise(
    Nb_Achats = sum(Nb.Achat),
    Montant = sum(Mnt.Achat),
    .groups = "drop"
  )


#Comparaison des indicateurs par âge

achats_age <- data_joint %>%
  group_by(Mois, tranche_age) %>%
  summarise(
    Nb_Achats = sum(Nb.Achat),
    Montant = sum(Mnt.Achat),
    .groups = "drop"
  )





# Fusionner les tables achats, correspondances et clients
data_joint <- achats %>%
  left_join(corres, by = c("Num.Site" = "NUM_SITE")) %>%
  left_join(clients, by = c("Id.Client" = "ID_CLIENT"))


# Chargement des départements (GeoJSON) depuis un URL
fr_dept <- st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson", quiet = TRUE)

#Vente par département
ventes_dept <- data_joint %>%
  mutate(Dept = substr(COD_POSTAL, 1, 2)) %>%
  group_by(Dept) %>%
  summarise(Montant = sum(Mnt.Achat, na.rm = TRUE), .groups = "drop")

