pacman::p_load(tidyverse, psych)
view(dataset)
dataset <- left_join(eutrophierung_fluesse_gueteklassen, Stickstoff_LW_kg_pro_hektar, by = "Jahr")
dataset <- left_join(dataset, PhosphatLW_pro_Jahr, by = "Jahr")
dataset <- left_join(dataset, eutrophierung_terrestrisch_flaechenanteil, by = "Jahr")

