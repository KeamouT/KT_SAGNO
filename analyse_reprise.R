#   Analyse des determinants de l'abandont des TARV  ----

##  Chargement des données et des packages  ----

library(tidyverse)
library(labelled)
library(questionr)
library(gtsummary)
library(GGally)

theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")

cta <- read_excel("data_abTARV.xlsx")

## Etiquettes de variables ----

cta <- cta %>%
  set_variable_labels(
    Centre_traitement = "Centre de traitemet",
    sexe_patient = "Sexe du patient",
    age_an = "Age (ans)",
    sit_matrim = "Situation matrimoniale",
    niveau_inst = "Niveau d'instruction",
    profession = "Profession",
    revenus_mensuel = "Revenu mensuel",
    distance_CTA = "Distance du CTA",
    duree_trajet = "Durée du trajet pour le CTA",
    mode_transport = "Mode de transport pour le CTA",
    dd_TARV = "Date de debut de traitemet ARV",
    TARV_interrompu = "Interruption du traitement dans le temps",
    nbre_de_fois = "Nombre de fois d'interruption de TARV",
    derniere_interruption_duree = "Durée de la dernière interruption de TARV",
    raison_effets_secondaires = "Interruption pour les effets secondaires",
    raisons_oubli = "Interruption par oubli",
    raisons_distance = "interruption à cause de la distance",
    raisons_financiers = "interruption pour des raisons financières",
    raisons_stigmatisation = "interruption à cause de la stigmatisation",
    raisons_rupture_ARV = "interruption à cause de la rupture des ARV",
    raisons_deplacement = "interruption à cause d'un déplacement",
    sous_traitement = "Actuellement sous traitement",
    abandon_ARV = "Abandon du traitement ARV",
    formation_sensibilisation_VIH_ARV = "Avez-vous reçu une formation ou sensibilisation sur le VIH/ARV ?",
    effets_second_actuel = "Ressentez-vous des effets secondaires actuellement ?",
    soutien_famille_entourage = "Vous sentez-vous soutenu(e) par votre famille/entourage ?",
    victime_stigmatisation = "Avez-vous été victime de stigmatisation ?",
    freq_visites_CTA = "Fréquence des visites au centre ARV",
    on_vie_longtemps_avec_VIH = "Selon vous, peut-on vivre longtemps avec le VIH ?",
    arret_TARV_nocif = "L’arrêt du traitement ARV peut-il nuire à votre santé ?",
    jugement_de_statut = "Pensez-vous que les autres vous jugeraient si vous disiez votre statut ?"

  )

##  Analyse univarié -----

description1<- cta %>%
  select(Centre_traitement, sexe_patient, age_an, sit_matrim, niveau_inst, profession) %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({min}, {max})", all_categorical() ~
                       "{n} ({p}%)")
  )

ggplot(cta)+
  aes(x = Centre_traitement) +
  geom_bar()

