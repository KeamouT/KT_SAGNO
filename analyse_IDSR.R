###SC - J'ai rajouté la ligne ci-dessous. 
rm(list = ls(all=TRUE))

## Load libraries ---- 
library(sf)	
library(MAP)	
library(terra)	
library(rio)           	# importer/exporter
library(here)          	# chemin vers les fichiers
library(skimr)       	  # obtenir un aperçu des données
library(rstatix)     	  # sommaire des statistiques et tests statistique
library(janitor)      	# ajouter des totaux et des pourcentages à des tableaux
library(scales)		      # convertir facilement les proportions en pourcentages
library(gtsummary)    	# sommaire des statistiques et tests
library(flextable)      # creer des tables HTML
library(officer)        # fonctions d'aide pour les tables
library(tidyverse)      # data management_resume et visualisation
library(readxl)        	# lire les fichiers excel
library(haven)          # lire les fichiers format stata
library(naniar)       	# bilan des données manquantes
library(lubridate)  	  # paquet général pour la manipulation et la conversion des dates 
library(parsedate)    	# a une fonction pour "deviner" les dates désordonnées
library(aweek)        	# une autre option pour convertir les dates en semaines et les semaines en dates
library(zoo)          	# fonctions supplémentaires de date et d'heure
library(mice)           # imputation
library(kableExtra)	
library(tidyverse)	
library(data.table)	
library(rstatix)      	# sommaire des statistiques et tests statistiques
library(janitor)      	# ajouter des totaux et des pourcentages à des tableaux
library(scales)       	# convertir facilement les proportions en pourcentages  
library(flextable)    	# convertir les tableaux en belles images
library(labelled)	
library(questionr)	
library(dplyr)	
library(tidyr)	
library(stringr)	
library(gt)	
library(tools)
library(lwgeom)
library(tmap)


### SC - I suggest you work with projects. This way you don't have to always indicate WD. Otherwise use here:here(), it makes it more shareable than setwd()
# ============================================================
# Data preparation
# ============================================================

# Import des bases de données----
base_idsr<- read_excel("analyse_weekly-IDSR_.xlsx",
                       sheet = 1)

idsr<- base_idsr %>% filter(!is.na(base_idsr$Semaine))

malariaepidemic <- as.data.table(read_excel("Weekly__Malariaepidemic.xls", 
                                     sheet = "Seuil National", skip = 3, n_max = 7))

ds_bui<- st_read("DISTRICTS/DS BDI 1.4.2026 Vf.shp")

tmap_mode("plot")
tm_shape(ds_bui) +
  tm_polygons()


# Analyse comparée entre 2025 et 2026 incluant le seui d'alerte----
names(malariaepidemic)<- c("sit_an", "annees", "S1", "S2", "S3", "S4", "S5", "S6", "S7", 
                           "S8", "S9", "S10", "S11", "S12", "S13", "S14", "S15", "S16", 
                           "S17", "S18", "S19", "S20", "S21", "S22", "S23", "S24", "S25", 
                           "S26", "S27", "S28", "S29", "S30", "S31", "S32", "S33", "S34", 
                           "S35", "S36", "S37", "S38", "S39", "S40", "S41", "S42", "S43", 
                           "S44", "S45", "S46", "S47", "S48", "S49", "S50", "S51", "S52", 
                           "S53", "min", "max")

malariaepidemic$annees[7]<- "seuil_alerte"
malariaepidemic$sit_an[1]<- "annnee_ant6"
malariaepidemic$sit_an[2]<- "annnee_ant5"
malariaepidemic$sit_an[3]<- "annnee_ant4"
malariaepidemic$sit_an[4]<- "annnee_ant3"
malariaepidemic$sit_an[5]<- "annnee_ant2"

malariaepidemic <- malariaepidemic |> 
  select(annees:S17)

malariaepidemic <- malariaepidemic |> filter(annees =="2025" | annees =="2026" | annees =="seuil_alerte") 

malaria_epidemic <- malariaepidemic

# Nettoyage du libellé "seui alerte" 
malariaepidemic <- malariaepidemic %>%
  mutate(
    annees = str_squish(as.character(annees)),
    annees = ifelse(str_detect(tolower(annees), "seui"), "Seuil alerte", annees)
  )

# Colonnes semaines (S1, S2, ...): détection automatique
week_cols <- names(malariaepidemic)[str_detect(names(malariaepidemic), "^S\\d+$")]

# Passage en format long : Année | semaine | valeur
long <- malariaepidemic %>%
  pivot_longer(cols = all_of(week_cols),
               names_to = "semaine",
               values_to = "valeur") %>%
  mutate(
    week_num = as.integer(str_remove(semaine, "^S")),
    semaine  = factor(semaine, levels = paste0("S", sort(unique(week_num))))
  ) %>%
  arrange(week_num)

# Séparer séries
d2025 <- long %>% filter(annees == "2025")
d2026 <- long %>% filter(annees == "2026")
dthr  <- long %>% filter(annees == "Seuil alerte")


# Graphique superposé
p <- ggplot() +
  # --- 2025 : barres en contour (vide) ---
  geom_col(
    data = d2025,
    aes(x = semaine, y = valeur, color = "2025"),
    fill = NA,
    linewidth = 0.9,
    width = 0.78
  ) +
  # --- 2026 : barres remplies (par-dessus) ---
  geom_col(
    data = d2026,
    aes(x = semaine, y = valeur, fill = "2026"),
    color = "midnightblue",
    alpha = 0.65,
    width = 0.62
  ) +
  # --- Seuil d’alerte : ligne + points ---
  geom_line(
    data = dthr,
    aes(x = semaine, y = valeur, color = "Seuil alerte", group = 1),
    linewidth = 1.2
  ) +
  geom_point(
    data = dthr,
    aes(x = semaine, y = valeur, color = "Seuil alerte"),
    size = 2
  ) +
  # --- Axes / format ---
  scale_y_continuous(labels = label_comma(big.mark = " ", decimal.mark = ",")) +
  scale_fill_manual(values = c("2026" = "#2C7FB8")) +
  scale_color_manual(values = c("2025" = "grey30", "Seuil alerte" = "#08306B")) +
  guides(
    fill  = guide_legend(title = NULL, order = 1),
    color = guide_legend(title = NULL, order = 2)
  ) +
  labs(
    title = "Cas de paludisme : comparaison 2025 vs 2026 et seuil d’alerte",
    x = "Semaine épidémiologique",
    y = "Nombre de cas"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 11)
  )

# Affichage
p









library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gt)             # création de jolis tableaux
library(janitor)      	# ajouter des totaux et des pourcentages à des tableaux
library(scales)       	# convertir facilement les proportions en pourcentages 

# 1) Import
df <- read_excel("malaria_epic.xlsx", sheet = "Feuil2")

# 2) Harmoniser le libellé "seui alerte" -> "Seuil d’alerte"
df <- df %>%
  mutate(
    Année = str_squish(as.character(Année)),
    Année = ifelse(str_detect(tolower(Année), "seui"), "Seuil d’alerte", Année)
  )

# 3) Colonnes semaines S1, S2, ... (détection auto)
week_cols <- names(df)[str_detect(names(df), "^S\\d+$")]

# 4) Format long
long <- df %>%
  pivot_longer(all_of(week_cols), names_to = "semaine", values_to = "valeur") %>%
  mutate(
    week_num = as.integer(str_remove(semaine, "^S")),
    semaine  = factor(semaine, levels = paste0("S", sort(unique(week_num))))
  ) %>%
  arrange(week_num)

# 5) Séries
d2025 <- long %>% filter(Année == "2025")
d2026 <- long %>% filter(Année == "2026")
dthr  <- long %>% filter(Année == "Seuil d’alerte")

# 6) Graphique (superposition)
p <- ggplot() +
  
  # --- 2025 : barre contour (fill transparent, bord visible) ---
  geom_col(
    data = d2025,
    aes(x = semaine, y = valeur, fill = "2025"),
    color = "grey30",
    linewidth = 0.9,
    width = 0.78
  ) +
  
  # --- 2026 : barre remplie (par-dessus) ---
  geom_col(
    data = d2026,
    aes(x = semaine, y = valeur, fill = "2026"),
    color = "midnightblue",
    alpha = 0.65,
    width = 0.62
  ) +
  
  # --- Seuil d’alerte : ligne + points ---
  geom_line(
    data = dthr,
    aes(x = semaine, y = valeur, color = "Seuil d’alerte", group = 1),
    linewidth = 1.2
  ) +
  geom_point(
    data = dthr,
    aes(x = semaine, y = valeur, color = "Seuil d’alerte"),
    size = 2
  ) +
  
  # ---- Échelles ----
scale_y_continuous(labels = label_comma(big.mark = " ", decimal.mark = ",")) +
  
  # Légende BARRES (fill) : ordre explicite 2026 puis 2025
  scale_fill_manual(
    name   = NULL,
    breaks = c("2026", "2025"),
    values = c("2026" = "#2C7FB8", "2025" = "transparent"),
    labels = c("2026 (rempli)", "2025 (contour)")
  ) +
  
  # Légende LIGNE (color) : seuil
  scale_color_manual(
    name   = NULL,
    breaks = c("Seuil d’alerte"),
    values = c("Seuil d’alerte" = "#08306B")
  ) +
  
  # ---- Guides : icônes fidèles ----
guides(
  # Barres : on force l’icône à ressembler à vos barres (border + alpha)
  fill = guide_legend(
    order = 1,
    override.aes = list(
      alpha = c(0.65, 1),                        # 2026 un peu transparent, 2025 opaque
      color = c("midnightblue", "grey30"),       # bordures comme sur la carte
      linewidth = c(0.3, 0.9)                    # bord plus fin pour 2026, plus visible pour 2025
    )
  ),
  # Ligne : on force une vraie icône "ligne + point"
  color = guide_legend(
    order = 2,
    override.aes = list(
      linetype = 1,
      shape = 16,
      linewidth = 1.2
    )
  )
) +
  
  labs(
    title = "Cas de paludisme : 2026 vs 2025 et seuil d’alerte",
    x = "Semaine épidémiologique",
    y = "Nombre de cas"
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",        # affiche les guides sur la même ligne/zone
    legend.direction = "horizontal",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 11)
  )

p

# Export (optionnel)
ggsave("graphique_superpose_paludisme_legend_oms.png", p, width = 10, height = 5.5, dpi = 300)




# 2) Harmoniser le libellé "seui alerte" (optionnel)
df <- malaria_epidemic %>%
  mutate(annees = str_squish(as.character(annees)))

# 3) Colonnes semaines (S1, S2, S3, ...)
week_cols <- names(df)[str_detect(names(df), "^S\\d+$")]

# 4) Extraire les lignes 2025 et 2026
row_2025 <- df %>% filter(annees == "2025") %>% slice(1)
row_2026 <- df %>% filter(annees == "2026") %>% slice(1)

# 5) Calcul tendance = (2026-2025)/2025*100
#    + gestion des divisions par 0 (renvoie NA si 2025=0)
trend_row <- row_2025
trend_row$annees <- "Tendance (%)"

trend_row[week_cols] <- lapply(week_cols, function(w){
  v25 <- as.numeric(row_2025[[w]])
  v26 <- as.numeric(row_2026[[w]])
  ifelse(is.na(v25) | v25 == 0, NA_real_, (v26 - v25) / v25 * 100)
}) %>% as.data.frame()

# 6) Ajouter la nouvelle ligne à la fin
df2 <- bind_rows(df, trend_row)

df2


# Analyse comparée des trois dernièers semaines de 2026 ----


# Aperçu rapide----
summary(idsr)
glimpse(idsr)
###SC - In the DHIS2 the NAs are really an issue. Normally it's the role of the district to send back to the CDS if NA is put in to check that it shouldn't be a 0
###SC - For the sake of the report I would replace NA by 0
###SC - Rajoute peut-etre des checks des données (par exemple pour duplicatats possibles)

names(idsr)<- c("province", "districts", "semaine", "annee", "deces_mat", 
                "deces_neo", "deces_peri", "TNN_CasSuspect", "TNN_Deces", 
                "cholera_deces", "Cholera_CasTraites", "diarrh_sang_deces", 
                "diarrh_sang__traites", "FHV_Deces", "FHV_Test_positif", "Meningite_Deces", "Meningite_CasTraités", 
                "PFA_Deces", "PFA_Cas", "palu_deces", "palu_positifs", 
                "rougeole_deces", "rougeole_suspect", "FièvreDeLaValléeDuRift_CS", 
                "FièvreDeLaValléeDuRift_CasConfirmés", "MPOX_CasSuspect", 
                "MPOX_CasConfirmés", "Covid19_CasSuspect", "Covid19_CasConfirmés", 
                "Nb rap. attendus", "Nb rap. reçus", "Nb rap. reçus en retard", 
                "Associatif", "Confessionnel", "Prive", "Public", "Promptitude", 
                "completude")

## Apperçu sue la complétude de la semaine----
## Filtre sur la semaine dernière semaine

DS_idsr<- idsr %>%
  filter(semaine == max(semaine))

quantile(DS_idsr$completude)
DS_idsr$class_copltud <- cut(DS_idsr$completude,  c(50, 75, 99.9, 100),
                             rigth=TRUE,
                             include.lowest= TRUE)


comp_moyenne<- DS_idsr %>%
  tbl_summary(include = c(completude, class_copltud),
              statistic = all_continuous() ~ "Moy. : {mean} [min-max : {min} - {max}]",
              label = list(completude ~ "Complétude", class_copltud ~ "Complétudes groupées par DS"))
comp_moyenne

ds_cible<- DS_idsr %>%
  select(districts, semaine, Promptitude, completude, class_copltud) %>%
  filter(districts=="Cibitoke"| districts=="Ndava"| districts=="Ruyigi")


## selection des maladies à surveiller ----
### SC - j'ai rajouté ANNEE

surveillance<- idsr %>%
  select(districts, semaine, annee, palu_positifs, palu_deces, rougeole_suspect, rougeole_deces, 
         MPOX_CasSuspect, FHV_Test_positif, FHV_Deces, diarrh_sang__traites, diarrh_sang_deces,
         Cholera_CasTraites, cholera_deces, PFA_Cas, PFA_Deces, TNN_CasSuspect, TNN_Deces)


## Data processing----
## Réordonnancement de cas_SE$semaine

### SC - Here I would directly create ISOweeks to save you time later on instead of your relevel code below.
# Example Step 1: Create ISO week string
surveillance <- surveillance %>%
  mutate(
    semaine = paste0(annee, "-W", sprintf("%02d", semaine))
  ) %>%
  select(-annee)


### SC - Pour le reste du script, tu fais comme si les données étaient uniquement de 2025. Pour éviter les ennuis si ta base de donnée a des erreurs, je rajouterais un filtre pour l'année


#### Regroupage par semaine épidemiologiquw ###
MPE_SE <- surveillance %>% 
  arrange(semaine) %>%
  group_by(semaine) %>% 
  summarise_if(is.numeric,sum , na.rm = TRUE)
###SC - Here I would make sure to use complete() to have all epi weeks. Otherwise, imagine in one week there are no cases reported, the week will not appear and you will have a gap.
###It is not likely to happen here but it's good practice

###SC que s'est-il passé avec ce chiffre? C'est aussi cette semaine là qu'on voit un pic palu à l'hôpital. 
MPE_SE <- MPE_SE %>% mutate(palu_positifs = ifelse(palu_positifs== 203918, 105630, palu_positifs))

#### Total de cas rapporté depuis les semaine 1 ###
total_cas<- surveillance %>% 
  summarise_if(is.numeric,sum, na.rm = TRUE)

# Combiner les deux dataframes ensemble
MPE_SE <- bind_rows(MPE_SE, total_cas) %>%
  mutate(semaine = case_when(
    is.na(semaine) == TRUE ~ "Total",
    T ~ semaine
  ))

# Garder les 3 dernière semaines
#SE_3D<- slice(MPE_SE, 38:41)

SE_3D <- MPE_SE %>%
  arrange(semaine) %>%
  slice_tail(n = 4)


###SC Pour cette sélection des trois dernières semaines et recodage, 
### tu peux automatiser pour éviter d'oublier d'une semaine 
### à l'autre (par exemple ici avec S41). Je t'ai mis un exemple ci-dessus 


tab1<- SE_3D %>%
  pivot_longer(
    cols = c(`palu_positifs`, `palu_deces`, `rougeole_suspect`, `rougeole_deces`, 
             `MPOX_CasSuspect`,`FHV_Test_positif`, `FHV_Deces`, `diarrh_sang__traites`,
             `diarrh_sang_deces`,`Cholera_CasTraites`, `cholera_deces`, `PFA_Cas`,
             `PFA_Deces`,`TNN_CasSuspect`, `TNN_Deces`)
  )


tab2<- tab1 %>% 
 pivot_wider(
   names_from = "semaine",
   values_from = "value"
 )


# On sépare le nom de la maladie et si cas ou décès
tab3 <- tab2 %>%
  mutate(name = tolower(name),
    name = case_when(
    name ==  "diarrh_sang__traites" ~ "diarrsang_traites",
    name ==  "diarrh_sang_deces" ~ "diarrsang_deces",
    T ~ name)) %>%
  filter(name!= "diarrh_sang__traites" & name!="diarrh_sang_deces") %>%
  separate(name, into = c("Disease", "Type"), sep = "_") %>%
  mutate(Type = case_when(
    Type ==  "deces" ~ "Deces",
    T ~ "Cas"))

# Pivot longer then wide and compute fatality rate
df_final <- tab3 %>%
  pivot_longer(
    cols = 3:6,
    names_to = "Week",
    values_to = "Value"
  ) %>%
  pivot_wider(names_from = Type, values_from = Value) %>%
mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(FR = if_else(Cas > 0, Deces / Cas * 100, 0)) %>%
  pivot_longer(cols = c(Cas, Deces, FR),
               names_to = "metric",
               values_to = "val") %>%
  unite(col = "week_metric", Week, metric, sep = "_") %>%
  pivot_wider(names_from = week_metric, values_from = val) %>%
  mutate(Disease = case_when(
    Disease == "cholera" ~ "Choléra" ,
    Disease == "diarrsang" ~ "Diarrhée sanglante",
    Disease == "fhv" ~ "Fièvre hemorr. virale" ,
    Disease == "mpox"~ "Mpox" ,
    Disease == "palu" ~ "Paludisme confirmé" ,
    Disease == "pfa" ~ "PFA" ,
    Disease == "rougeole" ~ "Rougeole" ,
    Disease == "tnn" ~ "Tétanos néonatal" 
  ))

# Pour l'instant arrangé par ordre alphabétique, tu peux arranger comme tu veux
df_final <- df_final %>% arrange(Disease)


#maintenant on peut créer le joli tableau
# Prendre les noms des semaines et le total de manière dynamique
weeks <- df_final %>%
  select(-Disease) %>%           
  names() %>%
  str_remove("_(Cas|Deces|FR)$") %>%     
  unique()

# ON crée le tableau initial 
gt_tbl <- df_final %>% gt(rowname_col = "Disease") %>%  
  fmt_number(columns = contains("FR"), decimals = 1)

# Ensuite on utilise le nom des semaines pour créer les différentes sur-colonnes (un peu comme si tu mergeais)
for (wk in weeks) {
  cols_for_week <- df_final %>%
    select(starts_with(wk)) %>%
    names()
  
  gt_tbl <- gt_tbl %>% tab_spanner(label = wk, columns = all_of(cols_for_week))
}

#On crée de jolis noms de sous-colonnes
col_names <- df_final %>% select(-Disease) %>% names()
new_labels <- ifelse(str_detect(col_names, "_Cas"), "Cas",
                     ifelse(str_detect(col_names, "_Deces"), "Deces",
                            ifelse(str_detect(col_names, "_FR"), "TM(%)", col_names)))
names(new_labels) <- col_names  # names = original columns

#Et on finalise le joli tableau
gt_tbl <- gt_tbl %>%
  cols_label(!!!new_labels) %>%  
tab_header(
  title = "Surveillance Épidémiologique des Maladies DS Burundi"
) %>%
  # Center-align all columns
  cols_align(
    align = "center",
    columns = - 1
  ) %>%
  # Highlight the "Cholera" row
  tab_style(
    style = cell_fill(color = "#FF69B4"),   # background color
    locations = cells_body(
      rows = `Disease` == "Rougeole"
    )
  )

# View the table
gt_tbl


# Export as PNG 
# gtsave(gt_tbl, "Tablename.png")

## Voir les tendances entre les deux derières semaines

tendance<- df_final %>% select(Disease, `2026-W16_Cas`, `2026-W17_Cas`) %>% 
  mutate(
  tendance= round(
    (((`2026-W17_Cas` - `2026-W16_Cas`)/`2026-W16_Cas`))*100, 0)
                                                                 )

tendance<- flextable::flextable(tendance)
tendance
# Entre les deux semaine on note une baise de cas soit `r inline_text(tendance, row_level = "Disease", col_level = "Choléra")`%. 

## Calculons les incidences des maladies aucours de la semaine----
 # importation de bse population des districts sanitaires

pop<- read_excel("population_ds.xlsx")

pop$num<- seq(1, 56, by = 1)


 last_week<- surveillance %>% 
   slice_tail(n = 56)%>% 
   arrange(districts)
 pop$num<- seq(1, 56, by = 1)
 last_week$num<- seq(1, 56, by = 1)
 
 # Fusionner avec la population 
  last_week_pop <- last_week %>%
    left_join(pop, by = "num")
  
 # Sélectionner les colonnes necessaires
 
 last_week_sel <- last_week_pop %>%
   select(num, district, pop_total, semaine, palu_positifs, rougeole_suspect, MPOX_CasSuspect, Cholera_CasTraites)

 
 # Calculer les incidences pour 10000 
 last_week_inc <- last_week_sel %>%
   mutate(
     incidence_palu      = (palu_positifs     / pop_total) * 1e4,
     incidence_rougeole  = (rougeole_suspect  / pop_total) * 1e4,
     incidence_mpox      = (MPOX_CasSuspect   / pop_total) * 1e4,
     incidence_cholera   = (Cholera_CasTraites / pop_total) * 1e4
   )
 
  # Incidence par maladie arrondir à 2 décimales et trier 
 incicence_palu <- last_week_inc %>%
   select(district, pop_total, palu_positifs, incidence_palu) %>%
   mutate(across(starts_with("incidence_"), ~round(., 2))) %>%
   arrange(desc(incidence_palu))%>% 
   slice(1:5)
   
 
 incidence_rougeole <- last_week_inc %>%
   select(district, pop_total, rougeole_suspect, incidence_rougeole) %>%
   mutate(across(starts_with("incidence_"), ~round(., 2))) %>%
   arrange(desc(incidence_rougeole)) %>% 
   slice(1:5)
 
 
 incidence_mpox <- last_week_inc %>%
   select(district, pop_total, MPOX_CasSuspect, incidence_mpox) %>%
   mutate(across(starts_with("incidence_"), ~round(., 2))) %>%
   arrange(desc(incidence_mpox)) %>% 
   slice(1:5)
 
 incidence_cholera <- last_week_inc %>%
   select(district, pop_total, Cholera_CasTraites, incidence_cholera) %>%
   mutate(across(starts_with("incidence_"), ~round(., 2))) %>%
   arrange(desc(incidence_cholera)) %>% 
   slice(1:5)
 
 
 flextable::flextable(incicence_palu)
 flextable::flextable(incidence_mpox)
 flextable::flextable(incidence_cholera)
flextable::flextable(incidence_rougeole)

# Graphique des cas de Mpox
mpox <- idsr %>%
  select(semaine, MPOX_CasSuspect) %>%
  group_by(semaine)%>% 
  summarise_if(is.numeric,sum , na.rm = TRUE) %>%
  mutate(
    week_num = as.integer(semaine),
    semaine  = factor(semaine, levels = paste0(sort(unique(week_num))))
  ) %>%
  arrange(week_num)

graph_mpox <- ggplot() +
  geom_col(
    data = mpox,
    aes(x = semaine, y = MPOX_CasSuspect),
    color = "#104E8B",
    linewidth = 0.9,
    width = 0.78
  )+
  # --- Axes / format ---
  scale_y_continuous(labels = label_comma(big.mark = " ", decimal.mark = ",")) +
  #scale_fill_manual(values = c("rougeole_suspect" = "#104E8B")) +
  guides(
    fill  = guide_legend(title = NULL, order = 1),
    color = guide_legend(title = NULL, order = 2)
  ) +
  labs(
    title = "Evolution des cas de Mpox, Burundi 2026",
    x = "Semaine épidémiologique",
    y = "Nombre de cas"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 11)
  )
graph_mpox

# Graphique des cas de Cholera
cholera <- idsr %>%
  select(semaine, Cholera_CasTraites) %>%
  group_by(semaine)%>% 
  summarise_if(is.numeric,sum , na.rm = TRUE) %>%
  mutate(
    week_num = as.integer(semaine),
    semaine  = factor(semaine, levels = paste0(sort(unique(week_num))))
  ) %>%
  arrange(week_num)

graph_cholera <- ggplot() +
  geom_col(
    data = cholera,
    aes(x = semaine, y = Cholera_CasTraites),
    color = "#104E8B",
    linewidth = 0.9,
    width = 0.78
  )+
  # --- Axes / format ---
  scale_y_continuous(labels = label_comma(big.mark = " ", decimal.mark = ",")) +
  #scale_fill_manual(values = c("rougeole_suspect" = "#104E8B")) +
  guides(
    fill  = guide_legend(title = NULL, order = 1),
    color = guide_legend(title = NULL, order = 2)
  ) +
  labs(
    title = "Evolution des cas de cholera traité, Burundi 2026",
    x = "Semaine épidémiologique",
    y = "Nombre de cas"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 11)
  )
graph_cholera

# Graphique des cas de Rougeole
 rougeole <- idsr %>%
   select(semaine, rougeole_suspect, rougeole_deces) %>%
   group_by(semaine)%>% 
   summarise_if(is.numeric,sum , na.rm = TRUE) %>%
 mutate(
   week_num = as.integer(semaine),
   semaine  = factor(semaine, levels = paste0(sort(unique(week_num))))
 ) %>%
   arrange(week_num)
 
 graph_rougeol <- ggplot() +
      geom_col(
     data = rougeole,
     aes(x = semaine, y = rougeole_suspect),
     color = "#104E8B",
     linewidth = 0.9,
     width = 0.78
   )+
   # --- Axes / format ---
   scale_y_continuous(labels = label_comma(big.mark = " ", decimal.mark = ",")) +
   #scale_fill_manual(values = c("rougeole_suspect" = "#104E8B")) +
   guides(
     fill  = guide_legend(title = NULL, order = 1),
     color = guide_legend(title = NULL, order = 2)
   ) +
   labs(
     title = "Evolution des cas de rougeole, Burundi 2026",
     x = "Semaine épidémiologique",
     y = "Nombre de cas"
   ) +
   theme_classic(base_size = 13) +
   theme(
     legend.position = "bottom",
     plot.title = element_text(face = "bold"),
     axis.text.x = element_text(size = 11)
   )

  #------------------------------------------------------------------------------------
 #  CARTOGRAPHIE DES INCIDENCES PALUDISME ET ROUGEOLE----
 #------------------------------------------------------------------------------------
# Incidence du paludisme par DS
 week10_inc <- last_week_inc %>% 
   mutate(district = ifelse(district== "DS Bujumbura centre", "DS Zone Centre",
                             ifelse(district== "DS Bujumbura nord", "DS Zone Nord",
                                    ifelse(district== "DS Bujumbura sud", "DS Zone Sud", 
                                           ifelse(district== "DS Isare", "DS Isale", district)))))

  incidence <- merge(ds_bui, as.data.frame(week10_inc),
                    by.x ="NOM_DS2", by.y ="district",
                    all.x = TRUE)
  
 brks_incidP <- quantile(incidence$incidence_palu, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
 brks_incidP <- unique(as.numeric(brks_incidP))
  
 tmap_mode("plot")
 inci_palu <- tm_shape(incidence) +
   tm_polygons(col = "incidence_palu",
               palette = "Reds",
               style = "fixed", breaks = brks_incidP,
               title = "Incidence Paludisme +",
               textNA = "") +
   tm_borders( col = "grey40") +
   tm_text(
     text = "NOM_DS",          # <-- remplacez par le vrai nom de colonne
     size = 0.6,
     col = "black",
     shadow = TRUE,
     remove.overlap = TRUE       # réduit les chevauchements si possible
   ) +
   
   # --- Flèche du Nord ---
   tm_compass(type = "arrow",        # "arrow", "rose", "8star", "4star", etc.
              position = c("left", "top"),
              size = 2) +
   
   # --- Échelle graphique ---
   tm_scale_bar(position = c("left", "bottom"),
                text.size = 0.7) +
   
    tm_layout (legend.position = c("right","bottom"),
              frame = FALSE,
              main.title = "Taux d'incidence des positif de Paludisme par DS du Burundi à la semaine 10",
              msin.title.size = 1) 
 inci_palu
 
 # Incidence de la rougeole par DS
 
 brks_incidR <- quantile(incidence$incidence_rougeole, na.rm = TRUE)
 
 brks_incidR <- mf_get_breaks(x = incidence$incidence_rougeole, nbreaks = 5, breaks = "quantile")
 brks_incidR <- unique(as.numeric(brks_incidR))
 
 tmap_mode("plot")
 inci_rougeole <- tm_shape(incidence) +
   tm_polygons(col = "incidence_rougeole",
               palette = "Reds",
               style = "fixed", breaks = brks_incidP,
               title = "Incidence Rougeole",
               textNA = "") +
   tm_borders( col = "grey40") +
   tm_text(
     text = "NOM_DS",          
     size = 0.6,
     col = "black",
     shadow = TRUE,
     remove.overlap = TRUE       # réduit les chevauchements si possible
   ) +
   
   # --- Flèche du Nord ---
   tm_compass(type = "arrow",        # "arrow", "rose", "8star", "4star", etc.
              position = c("left", "top"),
              size = 2) +
   
   # --- Échelle graphique ---
   tm_scale_bar(position = c("left", "bottom"),
                text.size = 0.7) +
   
   tm_layout (legend.outside = TRUE,
              frame = FALSE,
              main.title = "Taux d'incidence des cas suspects de Rougeole par DS du Burundi à la semaine 10",
              msin.title.size = 1) 
 
 inci_rougeole
 
 
 
 
 
 
 
 
 
 
 #------------------------------------------------------------------------------------
 #  FOCUS DS CIBITOKE------
 #------------------------------------------------------------------------------------
 cbtk<- surveillance %>%
   filter(districts=="Cibitoke")
 
 
 #### Regroupage par semaine épidemiologiquw ###
 MPE_SEcbk <- cbtk %>% 
   arrange(semaine) %>%
   group_by(semaine) %>% 
   summarise_if(is.numeric,sum , na.rm = TRUE)
 ###SC - Here I would make sure to use complete() to have all epi weeks. Otherwise, imagine in one week there are no cases reported, the week will not appear and you will have a gap.
 ###It is not likely to happen here but it's good practice
 
#### Total de cas rapporté depuis les semaine 1 ###
 cbtkT_cas<- cbtk %>% 
   summarise_if(is.numeric,sum, na.rm = TRUE)
 
 # Combiner les deux dataframes ensemble
 MPE_SEcbk <- bind_rows(MPE_SEcbk, cbtkT_cas) %>%
   mutate(semaine = case_when(
     is.na(semaine) == TRUE ~ "Total",
     T ~ semaine
   ))
 
 # Garder les 3 dernière semaines
 #SE_3D<- slice(MPE_SE, 38:41)
 
 cbk_3DSE <- MPE_SEcbk %>%
   arrange(semaine) %>%
   slice_tail(n = 4)
 
 
 ###SC Pour cette sélection des trois dernières semaines et recodage, 
 ### tu peux automatiser pour éviter d'oublier d'une semaine 
 ### à l'autre (par exemple ici avec S41). Je t'ai mis un exemple ci-dessus 
 
 
 tab4<- cbk_3DSE %>%
   pivot_longer(
     cols = c(`palu_positifs`, `palu_deces`, `rougeole_suspect`, `rougeole_deces`, 
              `MPOX_CasSuspect`,`FHV_Test_positif`, `FHV_Deces`, `diarrh_sang__traites`,
              `diarrh_sang_deces`,`Cholera_CasTraites`, `cholera_deces`, `PFA_Cas`,
              `PFA_Deces`,`TNN_CasSuspect`, `TNN_Deces`)
   )
 
 
 tab5<- tab4 %>% 
   pivot_wider(
     names_from = "semaine",
     values_from = "value"
   )
 
 
 # On sépare le nom de la maladie et si cas ou décès
 tab6 <- tab5 %>%
   mutate(name = tolower(name),
          name = case_when(
            name ==  "diarrh_sang__traites" ~ "diarrsang_traites",
            name ==  "diarrh_sang_deces" ~ "diarrsang_deces",
            T ~ name)) %>%
   filter(name!= "diarrh_sang__traites" & name!="diarrh_sang_deces") %>%
   separate(name, into = c("Disease", "Type"), sep = "_") %>%
   mutate(Type = case_when(
     Type ==  "deces" ~ "Deces",
     T ~ "Cas"))
 
 # Pivot longer then wide and compute fatality rate
 CBKdf_final <- tab6 %>%
   pivot_longer(
     cols = 3:6,
     names_to = "Week",
     values_to = "Value"
   ) %>%
   pivot_wider(names_from = Type, values_from = Value) %>%
   mutate(across(everything(), ~ replace_na(., 0))) %>%
   mutate(FR = if_else(Cas > 0, Deces / Cas * 100, 0)) %>%
   pivot_longer(cols = c(Cas, Deces, FR),
                names_to = "metric",
                values_to = "val") %>%
   unite(col = "week_metric", Week, metric, sep = "_") %>%
   pivot_wider(names_from = week_metric, values_from = val) %>%
   mutate(Disease = case_when(
     Disease == "cholera" ~ "Choléra" ,
     Disease == "diarrsang" ~ "Diarrhée sanglante",
     Disease == "fhv" ~ "Fièvre hemorr. virale" ,
     Disease == "mpox"~ "Mpox" ,
     Disease == "palu" ~ "Paludisme confirmé" ,
     Disease == "pfa" ~ "PFA" ,
     Disease == "rougeole" ~ "Rougeole" ,
     Disease == "tnn" ~ "Tétanos néonatal" 
   ))
 
 # Pour l'instant arrangé par ordre alphabétique, tu peux arranger comme tu veux
 CBKdf_final <- CBKdf_final %>% arrange(Disease)
 
 
 #maintenant on peut créer le joli tableau
 # Prendre les noms des semaines et le total de manière dynamique
 cbk_weeks <- CBKdf_final %>%
   select(-Disease) %>%           
   names() %>%
   str_remove("_(Cas|Deces|FR)$") %>%     
   unique()
 
 # ON crée le tableau initial 
 gt_tbl2 <- CBKdf_final %>% gt(rowname_col = "Disease") %>%  
   fmt_number(columns = contains("FR"), decimals = 1)
 
 # Ensuite on utilise le nom des semaines pour créer les différentes sur-colonnes (un peu comme si tu mergeais)
 for (wk in cbk_weeks) {
   cols_for_week <- CBKdf_final %>%
     select(starts_with(wk)) %>%
     names()
   
   gt_tbl2 <- gt_tbl2 %>% tab_spanner(label = wk, columns = all_of(cols_for_week))
 }
 
 #On crée de jolis noms de sous-colonnes
 col_names2 <- CBKdf_final %>% select(-Disease) %>% names()
 new_labels2 <- ifelse(str_detect(col_names2, "_Cas"), "Cas",
                      ifelse(str_detect(col_names2, "_Deces"), "Deces",
                             ifelse(str_detect(col_names2, "_FR"), "TM(%)", col_names2)))
 names(new_labels2) <- col_names2  # names = original columns
 
 #Et on finalise le joli tableau
 gt_tbl2 <- gt_tbl2 %>%
   cols_label(!!!new_labels2) %>%  
   tab_header(
     title = "Surveillance Épidémiologique des Maladies DS Cibitoke"
   ) %>%
   # Center-align all columns
   cols_align(
     align = "center",
     columns = - 1
   ) %>%
   # Highlight the "Cholera" row
   tab_style(
     style = cell_fill(color = "#FF69B4"),   # background color
     locations = cells_body(
       rows = `Disease` == "Rougeole"
     )
   )
 
 # View the table
 gt_tbl2
 
 
 
 #------------------------------------------------------------------------------------
 #  FOCUS DS RUYIGI------
 #------------------------------------------------------------------------------------
 ruyigi<- surveillance %>%
   filter(districts=="Ruyigi")
 
 
 ggplot(data = ruyigi) +  
   geom_bar(aes(x = semaine, y = rougeole_suspect), position = "stack", stat = "identity")


 
 
  #### Regroupage par semaine épidemiologiquw ###
 MPE_SEcbk <- cbtk %>% 
   arrange(semaine) %>%
   group_by(semaine) %>% 
   summarise_if(is.numeric,sum , na.rm = TRUE)
 ###SC - Here I would make sure to use complete() to have all epi weeks. Otherwise, imagine in one week there are no cases reported, the week will not appear and you will have a gap.
 ###It is not likely to happen here but it's good practice
 
 #### Total de cas rapporté depuis les semaine 1 ###
 cbtkT_cas<- cbtk %>% 
   summarise_if(is.numeric,sum, na.rm = TRUE)
 
 # Combiner les deux dataframes ensemble
 MPE_SEcbk <- bind_rows(MPE_SEcbk, cbtkT_cas) %>%
   mutate(semaine = case_when(
     is.na(semaine) == TRUE ~ "Total",
     T ~ semaine
   ))
 
 # Garder les 3 dernière semaines
 #SE_3D<- slice(MPE_SE, 38:41)
 
 cbk_3DSE <- MPE_SEcbk %>%
   arrange(semaine) %>%
   slice_tail(n = 4)
 
 
 ###SC Pour cette sélection des trois dernières semaines et recodage, 
 ### tu peux automatiser pour éviter d'oublier d'une semaine 
 ### à l'autre (par exemple ici avec S41). Je t'ai mis un exemple ci-dessus 
 
 
 tab4<- cbk_3DSE %>%
   pivot_longer(
     cols = c(`palu_positifs`, `palu_deces`, `rougeole_suspect`, `rougeole_deces`, 
              `MPOX_CasSuspect`,`FHV_Test_positif`, `FHV_Deces`, `diarrh_sang__traites`,
              `diarrh_sang_deces`,`Cholera_CasTraites`, `cholera_deces`, `PFA_Cas`,
              `PFA_Deces`,`TNN_CasSuspect`, `TNN_Deces`)
   )
 
 
 tab5<- tab4 %>% 
   pivot_wider(
     names_from = "semaine",
     values_from = "value"
   )
 
 
 # On sépare le nom de la maladie et si cas ou décès
 tab6 <- tab5 %>%
   mutate(name = tolower(name),
          name = case_when(
            name ==  "diarrh_sang__traites" ~ "diarrsang_traites",
            name ==  "diarrh_sang_deces" ~ "diarrsang_deces",
            T ~ name)) %>%
   filter(name!= "diarrh_sang__traites" & name!="diarrh_sang_deces") %>%
   separate(name, into = c("Disease", "Type"), sep = "_") %>%
   mutate(Type = case_when(
     Type ==  "deces" ~ "Deces",
     T ~ "Cas"))
 
 # Pivot longer then wide and compute fatality rate
 CBKdf_final <- tab6 %>%
   pivot_longer(
     cols = 3:6,
     names_to = "Week",
     values_to = "Value"
   ) %>%
   pivot_wider(names_from = Type, values_from = Value) %>%
   mutate(across(everything(), ~ replace_na(., 0))) %>%
   mutate(FR = if_else(Cas > 0, Deces / Cas * 100, 0)) %>%
   pivot_longer(cols = c(Cas, Deces, FR),
                names_to = "metric",
                values_to = "val") %>%
   unite(col = "week_metric", Week, metric, sep = "_") %>%
   pivot_wider(names_from = week_metric, values_from = val) %>%
   mutate(Disease = case_when(
     Disease == "cholera" ~ "Choléra" ,
     Disease == "diarrsang" ~ "Diarrhée sanglante",
     Disease == "fhv" ~ "Fièvre hemorr. virale" ,
     Disease == "mpox"~ "Mpox" ,
     Disease == "palu" ~ "Paludisme confirmé" ,
     Disease == "pfa" ~ "PFA" ,
     Disease == "rougeole" ~ "Rougeole" ,
     Disease == "tnn" ~ "Tétanos néonatal" 
   ))
 
 # Pour l'instant arrangé par ordre alphabétique, tu peux arranger comme tu veux
 CBKdf_final <- CBKdf_final %>% arrange(Disease)
 
 
 #maintenant on peut créer le joli tableau
 # Prendre les noms des semaines et le total de manière dynamique
 cbk_weeks <- CBKdf_final %>%
   select(-Disease) %>%           
   names() %>%
   str_remove("_(Cas|Deces|FR)$") %>%     
   unique()
 
 # ON crée le tableau initial 
 gt_tbl2 <- CBKdf_final %>% gt(rowname_col = "Disease") %>%  
   fmt_number(columns = contains("FR"), decimals = 1)
 
 # Ensuite on utilise le nom des semaines pour créer les différentes sur-colonnes (un peu comme si tu mergeais)
 for (wk in cbk_weeks) {
   cols_for_week <- CBKdf_final %>%
     select(starts_with(wk)) %>%
     names()
   
   gt_tbl2 <- gt_tbl2 %>% tab_spanner(label = wk, columns = all_of(cols_for_week))
 }
 
 #On crée de jolis noms de sous-colonnes
 col_names2 <- CBKdf_final %>% select(-Disease) %>% names()
 new_labels2 <- ifelse(str_detect(col_names2, "_Cas"), "Cas",
                       ifelse(str_detect(col_names2, "_Deces"), "Deces",
                              ifelse(str_detect(col_names2, "_FR"), "TM(%)", col_names2)))
 names(new_labels2) <- col_names2  # names = original columns
 
 #Et on finalise le joli tableau
 gt_tbl2 <- gt_tbl2 %>%
   cols_label(!!!new_labels2) %>%  
   tab_header(
     title = "Surveillance Épidémiologique des Maladies DS Cibitoke"
   ) %>%
   # Center-align all columns
   cols_align(
     align = "center",
     columns = - 1
   ) %>%
   # Highlight the "Cholera" row
   tab_style(
     style = cell_fill(color = "#FF69B4"),   # background color
     locations = cells_body(
       rows = `Disease` == "Rougeole"
     )
   )
 
 # View the table
 gt_tbl2
 
 
 
 

 
 
 
 
 
 
 
 
 
 
 
 
