## import datasets

## join datasets
pacman::p_load(tidyverse, psych)
view(dataset)
dataset <- left_join(eutrophierung_fluesse_gueteklassen, Stickstoff_LW_kg_pro_hektar, by = "Jahr")
dataset <- left_join(dataset, PhosphatLW_pro_Jahr, by = "Jahr")
dataset <- left_join(dataset, eutrophierung_terrestrisch_flaechenanteil, by = "Jahr")

df <- dataset
rm(dataset) ## dataset zu df umbenannt


Phosphate <- df$Verbrauch_PhosphatLW_t ## Nimmt Phos. Werte aus df und erstellt einzelne Value Liste
summary(Phosphate) ## erster Blick auf die Daten, Min., Max., Mean...

timeline_numeric <- as.numeric(as.character(df$Jahr)) ## neue Value Liste aus Jahreszahlen Spalte, mit Jahreszahlen als numerischer Wert statt Character (wichtig um später Limits in der Ansicht setzen zu können)
Phos_LW <- tibble(timeline_numeric, Phosphate) ##Neuer Tibble aus den eben erstellten Values
view(Phos_LW)

Phos_LW <- setNames(Phos_LW, c("Jahr","Phosphate_use")) ## Spalten im neuen Tibble umbennen, um sie leichter ansprechen zu können

plot (Phos_LW, type = "l", xlim=c(2000,2020), ylim=c(0,400)) ## erster sehr einfacherer Graph in R


ggplot(data=Phos_LW, aes(x=Jahr, y=Phosphate_use)) + ## erster ggplot Bar-Graph, Farbe hinter fill= lässt sich natürlich verändern, hier eine Liste mit Farbnamen: http://sape.inf.usi.ch/sites/default/files/ggplot2-colour-names.png
  geom_bar(stat="identity", width=0.9, fill="mediumaquamarine") +
  theme_minimal() +
  ggtitle("Use of phospate fertilizer in Germany 1985-2019") +
  xlab("Time (years)") +
  ylab("Phosphate (t)")
  
## falls noch nicht vorhanden: install.packages("plotly") für folgende Funktion
library(plotly) 

ggplotly(tooltip = c("y", "x")) ## zeigt genaue Werte wenn man mit der Maus über den letzten erstellten Graphen fährt, bin mir noch nicht sicher, ob das auch in RMArkdown/dem exportierten HTML Dokument funktioneren wird.

Phos_LW %>% ## und das ganze nochmal als Line chart mit Datapoints
  ggplot(aes(Jahr, Phosphate_use, group = 1)) +
  geom_line(linetype="solid", color="darkseagreen2", size=1) +
  geom_point(color="darkseagreen4", size=2) +
  theme_minimal() +
  labs(x = "Time (years)", 
       y = "Phosphate (t)", 
       title = "Use of phospate fertilizer in Germany 1985-2019") +
  xlim(2000, 2020) + #eingegränzt auf den Zeitraum 2000 - 2020 und die Werte 100 - 400
  ylim(100, 400)

ggplotly(tooltip = c("y", "x")) ## auch hier geht das natürlich wieder 

Phos_LW.new <- Phos_LW %>%
  mutate(
    Perc_change = (Phosphate_use - lag(Phosphate_use)) / lag(Phosphate_use)) ## Erstellt eine neue Version des zuvor genutzten Phospat-Datensets und fügt dem eine Spalte hinzu, in der die %-ualen Unterschieden zwischen den Jahren berechnet werden

view(Phos_LW.new)

library(ggplot2) ##nochmal sicherstellen ob auch alle nötigen tidyverse Pakete für den nächsten Schritt da sind
library(scales)

ggplot(Phos_LW.new, aes(x=Jahr, y=Perc_change, fill=factor(ifelse(Perc_change <0, "positive", "negative")))) + ## Bildliche darstellung der prozentualen Veränderung mit unterschiedlichen Farben für Positive und NEgative Werte (Zunahme oder Abnahme im Vergleich zum Vorjahr)
  geom_col() +
  theme_minimal() +
  scale_fill_manual(guide = FALSE, name = "Type of change", values=c("darkslategray3","darkslategray")) +
  scale_y_continuous(labels = scales::percent) +
  xlim(2000, 2020) +
  labs(x = "Time (years)", 
       y = "Change in comparison to the previous year", 
       title = "Increase and decline of phospate fertilizer use in Germany 2000-2019")

## Moving on with terrestrial eutrophication, also einfach nochmal die selben Schritte, aber diesmal mit %-Werten auf den Y-Achse

Eutro_terr <- df$`Flächenanteil_%_eutro_terr`
summary(Eutro_terr)

Area_eutro_terr <- tibble(timeline_numeric, Eutro_terr)
view(Area_eutro_terr)

Area_eutro_terr <- setNames(Area_eutro_terr, c("Jahr","Percentage_of_area"))
view(Area_eutro_terr)

Area_eutro_terr <- Area_eutro_terr %>% 
  mutate(Percent_value = Percentage_of_area / 100) ## da die Zahlenwerte in diesem Datensatz eigentlich % sind, R Prozentzahlen aber als 0. darstellte, habe ich hier eine neue Spalte mit den Werten/100 erstellt, dadurch kann ich die Y-Achse dann im Folgenden wieder als %-Scale darstellen, könnte für die Flussmessstellen auch nützlich sein, da die ja auch in % angegeben sind

view(Area_eutro_terr)

ggplot(data=Area_eutro_terr,
       aes(x=Jahr,
           y=Percent_value,
           fill=factor(ifelse(Jahr=="2030","Goal","Measurment")))) + ## 2030 anders farbig markiert, um darstellen zu können, dass es das Ziel ist, kein Messwert
  geom_bar(stat="identity", width=0.9) +
  theme_minimal() +
  scale_fill_manual(name = "Type of data", values=c("salmon4","lightsalmon2")) +
  scale_y_continuous(labels = scales::percent) + ## hier der Part der die Y-Achse dann zu % werden lässt
  xlab("Time (years)") +
  ylab("Percantage of land area exceeding critical eutrophication loads") +
  ggtitle("Vulnerable terrestrial ecosystems where critical loads of eutrophiciation are exceeded in Germany 2000-2015") 

ggplotly(tooltip = c("y", "x"))

Area_eutro_terr %>%
  ggplot(aes(Jahr, Percent_value, group = 1)) +
  geom_line(linetype="solid", color="lightpink2", size=1) +
  geom_point(color="lightpink4", size=2) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(x = "Time (years)", 
       y = "Percantage of land area exceeding critical eutrophication loads (%)", 
       title = "Vulnerable terrestrial ecosystems where critical loads of eutrophiciation are exceeded in Germany 2000-2015") + 
  xlim(2000, 2015)

ggplotly(tooltip = c("y", "x"))

Area_eutro_terr.new <- Area_eutro_terr %>%
  mutate(
    Perc_change = (Percentage_of_area - lag(Percentage_of_area)) / lag(Percentage_of_area)) ## again neue Tabelle die eine Spalte mit der %-Änderung im Jahresvergleich beinhaltet

view(Area_eutro_terr.new)


ggplot(Area_eutro_terr.new, aes(x=Jahr, y=Perc_change, fill=factor(ifelse(Perc_change <0, "-", "+")))) + 
  geom_col(position="dodge") +
  theme_minimal() +
  scale_fill_manual(guide = FALSE, name = "Type of change", values=c("rosybrown4","rosybrown2")) +
  scale_y_continuous(labels = scales::percent) +
  xlim(2000, 2016) +
  labs(x = "Time (years)", 
       y = "Change in comparison to the previous year", 
       title = "Increase and decline of area exceeding critical eutrophication loads in Germany 2000-2016")
