# ============================================================
# Dépistage nutritionnel - Busuma
# Analyse OMS : global semaine + focus dernière semaine
# ============================================================

library(readxl)
library(dplyr)
library(janitor)
library(stringr)

# ------------------------------------------------------------
# 1) Import
# ------------------------------------------------------------
nut_comm_busuma <- read_excel(
  "Dépistage Nutritionnel_Base de Données.xlsx",
  skip = 1
) %>%
  clean_names()

names(nut_comm_busuma) <- c("date_encodage", "nom_encodeur", "semaine", "nom_sensibilisateur", 
                            "quartier", "x6_23_mois_number_vert", "x6_23_mois_number_jaune", 
                            "x6_23_mois_number_rouge", "x6_23_mois_number_oedeme", "x24_59_mois_number_vert", 
                            "x24_59_mois_number_jaune", "x24_59_mois_number_rouge", "x24_59_mois_number_oedeme", 
                            "x6_23_mois_percent_vert", "x6_23_mois_percent_jaune", "x6_23_mois_percent_rouge", 
                            "x6_23_mois_percent_oedeme", "x24_59_mois_percent_vert", "x24_59_mois_percent_jaune", 
                            "x24_59_mois_percent_rouge", "x24_59_mois_percent_oedeme")
# ------------------------------------------------------------
# 2) Nettoyage des lignes invalides
# ------------------------------------------------------------
nut_comm_busuma <- nut_comm_busuma %>%
  mutate(semaine = str_squish(as.character(semaine))) %>%
  filter(!is.na(semaine), semaine != "", !str_detect(semaine, "TOTAL"))

# ------------------------------------------------------------
# 3) Retirer colonnes %
# ------------------------------------------------------------
base_nut_analyse <- nut_comm_busuma %>%
  select(-matches("percent"))

# ------------------------------------------------------------
# 4) Semaine en facteur
# ------------------------------------------------------------
base_nut_analyse <- base_nut_analyse %>%
  mutate(semaine = as.integer(semaine))

# ============================================================
# 🔵 PARTIE 1 : ANALYSE GLOBALE PAR SEMAINE (tout le camp)
# ============================================================

resume_semaine <- base_nut_analyse %>%
  group_by(semaine) %>%
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    # Dépistage total
    depistage_6_23 = rowSums(across(matches("x6_23_mois_number_vert|x6_23_mois_number_jaune|x6_23_mois_number_rouge|x6_23_mois_number_oedeme")), na.rm = TRUE),
    depistage_24_59 = rowSums(across(matches("x24_59_mois_number_vert|x24_59_mois_number_jaune|x24_59_mois_number_rouge|x24_59_mois_number_oedeme")), na.rm = TRUE),
    
    depistage = depistage_6_23 + depistage_24_59,
    
    # Cas
    cas_MAS = rowSums(across(matches("rouge|oedeme")), na.rm = TRUE),
    cas_MAM = rowSums(across(matches("jaune")), na.rm = TRUE),
    MAG = cas_MAS + cas_MAM,
    
    # Prévalences
    prev_MAS = round((cas_MAS / depistage)*100, 2),
    prev_MAM = round((cas_MAM / depistage)*100, 2),
    prev_MAG = round((MAG / depistage)*100, 2)
  )

# ============================================================
# 🔴 PARTIE 2 : ANALYSE DE LA DERNIÈRE SEMAINE (PAR QUARTIER)
# ============================================================

# Identifier dernière semaine
derniere_semaine <- max(base_nut_analyse$semaine, na.rm = TRUE)

# Filtrer uniquement cette semaine
data_last_week <- base_nut_analyse %>%
  filter(semaine == derniere_semaine)

# Résumé par quartier (dernière semaine)
resume_last_week_quartier <- data_last_week %>%
  group_by(quartier) %>%
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    # Dépistage
    depistage_6_23 = rowSums(across(matches("x6_23_mois_number_vert|x6_23_mois_number_jaune|x6_23_mois_number_rouge|x6_23_mois_number_oedeme")), na.rm = TRUE),
    depistage_24_59 = rowSums(across(matches("x24_59_mois_number_vert|x24_59_mois_number_jaune|x24_59_mois_number_rouge|x24_59_mois_number_oedeme")), na.rm = TRUE),
    
    depistage = depistage_6_23 + depistage_24_59,
    
    # Cas
    cas_MAS = rowSums(across(matches("rouge|oedeme")), na.rm = TRUE),
    cas_MAM = rowSums(across(matches("jaune")), na.rm = TRUE),
    MAG = cas_MAS + cas_MAM,
    
    # Prévalence
    prev_MAS = round((cas_MAS / depistage)*100, 2),
    prev_MAM = round((cas_MAM / depistage)*100, 2),
    prev_MAG = round((MAG / depistage)*100, 2)
  )

# ============================================================
# Résultats
# ============================================================

resume_semaine                 # ✅ Situation globale par semaine
resume_last_week_quartier     # ✅ Situation spatiale dernière semaine
derniere_semaine              # ✅ Numéro de la dernière semaine

# ============================================================
# RESUME
# ============================================================
top6_MAG <- resume_last_week_quartier %>%
  arrange(desc(MAG)) %>%
  slice_head(n = 6)

top6_MAS <- resume_last_week_quartier %>%
  arrange(desc(cas_MAS)) %>%
  slice_head(n = 6)
