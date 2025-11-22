# Script amélioré pour l'analyse logistique (OR)
# Auteur : (ton nom)
# Pré-conditions : place data_abTARV.xlsx dans le dossier "data" (ici using here::here)

### - Clean l'environnement global. 
rm(list = ls(all=TRUE))

# Packages nécessaires (charge uniquement ceux utilisés)
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  here, rio, janitor, data.table, dplyr, stringr, tibble,readxl, dplyr, 
  lubridate, zoo, gtsummary, broom, lmtest, parameters, webshot2, tidyr, ISOweek, 
  car, pROC, flextable, sjPlot, forcats, readxl, tidyr, labelled, gt, ggplot2,
  forcats, rnaturalearth, rnaturalearthdata, viridis, ggrepel, readr, GGally
)



# Répertoire de travail et date
if (!dir.exists("outputs")) dir.create("outputs")
old_locale <- Sys.getlocale("LC_TIME")
# Attention : change de locale peut échouer sur certains systèmes CI
try(Sys.setlocale("LC_TIME", "French"), silent = TRUE)
today_date_fr <- format(Sys.Date(), "%d %B %Y")

# Import
cta <- read_excel(here::here("data", "data_abTARV.xlsx"))
cta <- janitor::clean_names(as.data.table(cta))  # noms propres

# Aperçu
glimpse(cta)
skimr::skim(cta) # si tu veux un résumé rapide


## Etiquettes de variables ----

names(cta)[33] <- "autre_raison_dinterup"

# Age -> classes
# corrige la faute right (anciennement 'rigth')
cta <- cta %>%
  mutate(
    classe_age = cut(age_an, breaks = c(15, 25, 35, 45, 80),
                     right = TRUE, include.lowest = TRUE,
                     labels = c("15-24", "25-34", "35-44","45 et plus"))
  )

cta <- cta %>%
  set_variable_labels(
    centre_traitement = "Centre de traitement",
    sexe_patient = "Sexe du patient",
    age_an = "Age (ans)",
    sit_matrim = "Situation matrimoniale",
    niveau_inst = "Niveau d'instruction",
    profession = "Profession",
    revenus_mensuel = "Revenu mensuel",
    distance_cta = "Distance du CTA",
    duree_trajet = "Durée du trajet pour le CTA",
    mode_transport = "Mode de transport pour le CTA",
    dd_tarv = "Date de debut de traitemet ARV",
    tarv_interrompu = "Interruption du traitement dans le temps",
    nbre_de_fois = "Nombre de fois d'interruption de TARV",
    derniere_interruption_duree = "Durée de la dernière interruption de TARV",
    raison_effets_secondaires = "Interruption pour les effets secondaires",
    raisons_distance = "interruption à cause de la distance",
    raisons_financiers = "interruption pour des raisons financières",
    raisons_stigmatisation = "interruption à cause de la stigmatisation",
    raisons_rupture_arv = "interruption à cause de la rupture des ARV",
    autre_raison_dinterup = "Autre raison de l'interruption",
    raisons_deplacement = "interruption à cause d'un déplacement",
    sous_traitement = "Actuellement sous traitement",
    abandon_arv = "Abandon du traitement ARV",
    formation_sensibilisation_vih_arv = "Avez-vous reçu une formation ou sensibilisation sur le VIH/ARV ?",
    effets_second_actuel = "Ressentez-vous des effets secondaires actuellement ?",
    soutien_famille_entourage = "Vous sentez-vous soutenu(e) par votre famille/entourage ?",
    victime_stigmatisation = "Avez-vous été victime de stigmatisation ?",
    freq_visites_cta = "Fréquence des visites au centre ARV",
    on_vie_longtemps_avec_vih = "Selon vous, peut-on vivre longtemps avec le VIH ?",
    arret_tarv_nocif = "L’arrêt du traitement ARV peut-il nuire à votre santé ?",
    jugement_de_statut = "Pensez-vous que les autres vous jugeraient si vous disiez votre statut ?",
    classe_age = "Catégorie d'âge"
  )


# Dates
# Assure-toi du format ; si non-standard, utilise dmy(), ymd() etc.
cta <- cta %>%
  mutate(
    date_intw = as.Date(date_intw),
    dd_tarv = as.Date(dd_tarv),
    initiation = zoo::as.yearmon(dd_tarv),
    annee_inti = format(zoo::as.yearmon(dd_tarv), "%Y")
  )


# Recodages binaires : utiliser case_when / %in%
cta <- cta %>%
  mutate(
    on_vie_longtemps_avec_vih = if_else(on_vie_longtemps_avec_vih %in% c("Non","NSP"), 0L, 1L),
    arret_tarv_nocif = if_else(arret_tarv_nocif %in% c("Non","NSP"), 0L, 1L),
    jugement_de_statut = if_else(jugement_de_statut %in% c("Non","NSP"), 0L, 1L)
  )

# Harmonisation de profession (regroupement et factor)
cta <- cta %>%
  mutate(
    profession = as.character(profession),
    profession = fct_recode(as_factor(profession),
      "Fonctionnaire" = "Agent de santé",
      "Coiffeuse/Couture" = "Coiffeuse",
      "Coiffeuse/Couture" = "Couture",
      "Cultivateur/Pêcheur" = "Cultivateur",
      "Cultivateur/Pêcheur" = "Pêcheur",
      "Sans emploi/Etudiant" = "Etudiant",
      "Sans emploi/Etudiant" = "Sans emploi"
    )
  )

# creation de la variable autre raison d'interuption du TARV en fonction 
#  des variables TARV interrompu et la liste des raisons d'interuption
cta <- cta %>%
  mutate(autre_raisons_itarv = case_when(
    tarv_interrompu == "Oui" & !is.na(autre_raison_dinterup) ~ 1,
    tarv_interrompu == "Oui" &  is.na(autre_raison_dinterup) ~ 0,
    TRUE ~ NA_real_
  ))

# Individualiser certaines professions

cta <- cta %>%
  mutate(
    coif_couture   = ifelse(profession == "Coiffeuse/Couture", 1, 0),
    fonctionnaire    = ifelse(profession == "Fonctionnaire", 1, 0),
    commerce    = ifelse(profession == "Commerçant", 1, 0),
    chauffeur      = ifelse(profession == "chauffeur", 1, 0),
    mineur   = ifelse(profession == "Mineur", 1, 0),
    menagere    = ifelse(profession == "Ménagère", 1, 0),
    h_uniforme  = ifelse(profession == "Homme en uniforme", 1, 0),
    cut_peche  = ifelse(profession == "Cultivateur/Pêcheur", 1, 0),
    sans_emploi = ifelse(profession == "Sans emploi", 1, 0),
    ouvrier     = ifelse(profession == "Ouvrier", 1, 0)
  )



cta <- cta %>%
  set_variable_labels(
    sexe_patient = "Sexe du patient",
    h_uniforme = "Homme en uniforme",
    sans_emploi= "Sans emploi/Etudiant",
    chauffeur = "Chauffeur",
    menagere = "Ménagère",
    mineur = "Mineur",
    cut_peche= "Cutlitvateur/Pêcheur",
    commerce = "Commerçant",
    coif_couture = "Coiffeuse/couture",
    arret_tarv_nocif = "L’arrêt du traitement ARV peut-il nuire à votre santé ?",
    on_vie_longtemps_avec_vih = "Selon vous, peut-on vivre longtemps avec le VIH ?",
    jugement_de_statut = "Pensez-vous que les autres vous jugeraient si vous disiez votre statut ?")



# Exemple d'obtention de dummies si souhaité (fastDummies ou model.matrix)
# Ici on montre une option avec modelr : créer dummies uniquement au moment du modèle
# Préparer le jeu d'analyse en sélectionnant les variables d'intérêt
explicative <- c("sexe_patient", "classe_age", "niveau_inst", "sit_matrim", "coif_couture", "fonctionnaire", 
                 "chauffeur", "mineur", "menagere", "commerce", "h_uniforme", "cut_peche", 
                 "ouvrier", "revenus_mensuel", "distance_cta", "duree_trajet", "mode_transport", 
                 "raison_effets_secondaires", "raisons_distance", "raisons_financiers", 
                 "raisons_stigmatisation", "raisons_rupture_arv", "formation_sensibilisation_vih_arv",
                 "arret_tarv_nocif", "victime_stigmatisation", "freq_visites_cta", "on_vie_longtemps_avec_vih", 
                 "jugement_de_statut")



# Assure-toi que la variable outcome est codée 0/1
cta <- cta %>%
  mutate(abandon_arv = if_else(abandon_arv == "OUI", 1L, 0L))

# Vérifier les NA avant drop_na
n_before <- nrow(cta)
cta_clean <- cta %>% drop_na(any_of(c("abandon_arv", explicative)))
n_after <- nrow(cta_clean)
message("Observations before drop_na: ", n_before, " after: ", n_after,
        " (removed ", n_before - n_after, " rows)")

# Relevel les facteurs pour référence pertinente
if ("classe_age" %in% names(cta)) {
  cta <- cta %>% mutate(classe_age = fct_relevel(as.factor(classe_age), "15-24"))
}
if ("sexe_patient" %in% names(cta)) {
  cta <- cta %>% mutate(sexe_patient = fct_relevel(as.factor(sexe_patient), "Femme"))
}

# Table descriptive
library(ggplot2)

 CTA<-ggplot(cta)+
   aes(x = centre_traitement) +
   geom_bar(  fill ="blue4")+
   coord_flip() +
   geom_text(aes(x = centre_traitement), stat = "prop", 
             position = position_dodge(.9), vjust = "top", colour = "black") +
   ylab("Pourcentage (%)")


descr_demo <- cta %>%
  select(sexe_patient, classe_age, age_an, sit_matrim, niveau_inst, profession) %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({min}, {max})", all_categorical() ~
                                 "{n} ({p}%)",
    missing = "no"
  )) %>%
  bold_labels()

save_as_docx("description demograpgique" = descr_demo, path = "demograpgique.docx")


descr_quintil <- cta %>%
  select(revenus_mensuel, distance_cta, duree_trajet, mode_transport) %>%
  tbl_summary(missing = "no")%>%
  bold_labels()

descr_interrution<- cta %>%
  select(tarv_interrompu, nbre_de_fois, derniere_interruption_duree) %>%
  tbl_summary(missing = "no") %>%
  bold_labels()

descr_raison <- cta %>%
  select(raison_effets_secondaires, raisons_distance,raisons_financiers, 
         raisons_stigmatisation, raisons_rupture_arv,autre_raisons_itarv) %>%
  tbl_summary(missing = "no") %>%
  bold_labels()


descr_facteurs<- cta %>%
  select(sous_traitement, abandon_arv, formation_sensibilisation_vih_arv, effets_second_actuel, 
         soutien_famille_entourage, victime_stigmatisation, 
         on_vie_longtemps_avec_vih, arret_tarv_nocif, jugement_de_statut ) %>%
  tbl_summary(missing = "no") %>%
  bold_labels()

# Age group and Sex
age_sexe<-cta %>% 
  select(classe_age, sexe_patient) %>%
  tbl_summary(by = sexe_patient,
              digits = all_categorical() ~ 1) %>%
  modify_header(label  ~ "**Variable**") %>%
  bold_labels() %>%
  add_overall() %>%
  modify_spanning_header(starts_with("stat_") ~ "**sexe_patient**")

# moyen par sexe
moyen_age_sexe<- cta %>% 
  select(age_an, sexe_patient) %>%                     
  tbl_summary(by = sexe_patient,                                    
              type = all_continuous() ~ "continuous2",      
              statistic = all_continuous() ~ c(
                "{mean} ({sd})",                             
                "{median} ({p25}, {p75})",                   
                "{min}, {max}"),
              label = age_an ~ "Age du patient",
              digits = list(all_categorical() ~ 1,
                            where(is.numeric) ~1)) %>% 
  add_overall() %>%  
  add_stat_label(label = age_an ~ c("Moyenne (sd)", 
                                    "Médiane (IQR)", 
                                    "Min - Max")) %>% add_p()



# Sex Ratio (H/F)
sratio<-cta %>% count(sexe_patient, na.rm=TRUE) %>% spread(sexe_patient,n) %>% mutate(ratio = round(Homme/Femme, digits = 2))

## Analyse bi
## definir les variables d'interet 

descr_demobi <- cta %>%
  select(abandon_arv, sexe_patient, classe_age, age_an,sit_matrim, niveau_inst, coif_couture, 
         fonctionnaire, mineur, menagere, commerce, h_uniforme, cut_peche, ouvrier) %>%
  tbl_summary(
    by = "abandon_arv",
    percent = "row",
    missing = "no") %>%
  add_overall(last = TRUE) %>% add_p() %>%
  bold_labels()



descr_quintilbi <- cta %>%
  select(abandon_arv, revenus_mensuel, distance_cta, duree_trajet, mode_transport) %>%
  tbl_summary(
    by = "abandon_arv",
    percent = "row",
    missing = "no") %>%
  add_overall(last = TRUE) %>% add_p() %>%
  bold_labels()



descr_interrutionbi <- cta %>%
  select(abandon_arv, tarv_interrompu, nbre_de_fois, derniere_interruption_duree) %>%
  tbl_summary(
    by = "abandon_arv",
    percent = "row",
    missing = "no") %>%
  add_overall(last = TRUE) %>% add_p() %>%
  bold_labels()



descr_raisonbi <- cta %>%
  select(abandon_arv, raison_effets_secondaires, raisons_distance,raisons_financiers, 
         raisons_stigmatisation, raisons_rupture_arv) %>%
  tbl_summary(
    by = "abandon_arv",
    percent = "row",
    missing = "no") %>%
  add_overall(last = TRUE) %>% add_p() %>%
  bold_labels()




descr_facteursbi<- cta %>%
  select(sous_traitement, abandon_arv, formation_sensibilisation_vih_arv, effets_second_actuel, 
         soutien_famille_entourage, victime_stigmatisation, 
         on_vie_longtemps_avec_vih, arret_tarv_nocif, jugement_de_statut ) %>%
  tbl_summary(
    by = "abandon_arv",
    percent = "row",
    missing = "no") %>%
  add_overall(last = TRUE) %>% add_p() %>%
  bold_labels()



# Analyse univariée (tbl_uvregression)
univ_tab <- cta %>%
  dplyr::select(all_of(explicative), abandon_arv) %>%
  tbl_uvregression(
    method = glm,
    y = abandon_arv,
    method.args = list(family = binomial),
    exponentiate = TRUE)

# Modèle multivariable : choisis la stratégie de sélection (ici on met toutes les variables)
# ATTENTION : vérifier colinéarité avant d'inclure tout
# Construire la formule
formula_multiv <- as.formula(paste("abandon_arv ~", paste(explicative, collapse = " + ")))

mv_full <- glm(formula_multiv, family = binomial(link = "logit"), data = cta)
summary(mv_full)

# Diagnostics de colinéarité
 if (requireNamespace("car", quietly = TRUE)) {
   vif_vals <- car::vif(mv_full)
   print(vif_vals)
 }

# Vérifier séparation quasi-totale (si coefficients très grands / warnings)
# Si séparation suspectée, envisager logistf::logistf()
# Vérifier influence
influential <- cooks.distance(mv_full)
which(influential > (4/ nrow(cta))) -> infl_idx
if (length(infl_idx) > 0) message("Observations influentes détectées: ", paste(infl_idx, collapse = ", "))

# AUC
# if (requireNamespace("pROC", quietly = TRUE)) {
#  roc_obj <- pROC::roc(cta$abandon_arv, predict(mv_full, type = "response"))
#  message("AUC: ", round(pROC::auc(roc_obj), 3))
#}


# Extraire les données utilisées par le modèle
data_used <- model.frame(mv_full)

# Vérifier la longueur
length(data_used$abandon_arv) == length(predict(mv_full, type = "response"))

# Calcul ROC
library(pROC)
roc_obj <- roc(data_used$abandon_arv, predict(mv_full, type = "response"))
auc(roc_obj)

    # modèle a AUC ≈ 0,976, donc il distingue très bien les deux classes (abandon ARV vs non-abandon).



# Résumé final avec OR et IC
mv_tab <- tbl_regression(mv_full, exponentiate = TRUE, tidy_fun = broom::tidy)

# Fusionner tableaux univarie et multivarie si souhaité
tbl_merge(list(univ_tab, mv_tab), tab_spanner = c("Univariate", "Multivariable"))

# Sauvegarde des résultats
library(gt)
# saveRDS(mv_full, file = here::here("outputs", "mv_full_model.rds"))
# gtsave(as_gt(mv_tab), filename = here::here("outputs", "mv_tab.png"))

CTA
descr_demo
descr_quintil
descr_interrution
descr_raison
descr_facteurs
age_sexe
moyen_age_sexe
sratio

descr_demobi
descr_quintilbi
descr_interrutionbi
descr_raisonbi
descr_facteursbi


univ_tab

auc(roc_obj)

mv_tab

# Restauration de la locale
try(Sys.setlocale("LC_TIME", old_locale), silent = TRUE)
