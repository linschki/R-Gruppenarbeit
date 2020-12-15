# Package
tidyvers, ggplot, hrbrthemes, gifski, av

view(dataset)

view(eutrophierung_fluesse_gueteklassen)

str(eutrophierung_fluesse_gueteklassen)

"Qualityclass II-III" <- c(eutrophierung_fluesse_gueteklassen$Güteklasse_II-III)

"Qualityclass III" <- c(eutrophierung_fluesse_gueteklassen$Güteklasse_III)

"Qualityclass III-IV" <- c(eutrophierung_fluesse_gueteklassen$Güteklasse_III-IV)

"Qualityclass IV" <- c(eutrophierung_fluesse_gueteklassen$Güteklasse_IV)

"Total eutrophication" <- c(eutrophierung_fluesse_gueteklassen$Summe_%_Flussmessstellen)

"Year eutrophication" <- c(eutrophierung_fluesse_gueteklassen$Jahr)

head(eutrophierung_fluesse_gueteklassen, n = 3)

tail(eutrophierung_fluesse_gueteklassen, c(5L, -1L))

tail(eutrophierung_fluesse_gueteklassen)

tail(eutrophierung_fluesse_gueteklassen, n = -3L)

boxplot(`Qualityclass II-III`, `Qualityclass III`, `Qualityclass III-IV`, `Qualityclass IV`)

boxplot(`Total eutrophication`)

plot(`Year eutrophication`, `Qualityclass II-III`)

plot(`Year eutrophication`, `Qualityclass III`)

plot(`Year eutrophication`, `Qualityclass III-IV`)

plot(`Year eutrophication`, `Qualityclass IV`)

# Vier nebeneinander 
par(mfrow = c(2,2))
par(col="red")
plot(`Year eutrophication`, `Qualityclass II-III`)
plot(`Year eutrophication`, `Qualityclass III`)
plot(`Year eutrophication`, `Qualityclass III-IV`)
plot(`Year eutrophication`, `Qualityclass IV`)


par(mfrow = c(1,1))
plot(`Year eutrophication`, `Total eutrophication`)
"lm_total eutrophication" <- lm(`Year eutrophication`~`Total eutrophication`)
lines(`Year eutrophication`, `Total eutrophication`)


"p" <- ggplot(eutrophierung_fluesse_gueteklassen, mapping = aes(`Qualityclass II-III`, `Year eutrophication`))
 "p"+ geom_bar() 
 "p" + geom_bar(fill = "red") 
 "p" + geom_bar(colour = "red")
 "p" + geom_bar(fill = "white", colour = "red")
 "p" + geom_bar(fill = "#00abff")
 "p" + geom_bar(fill = NA, colour = "#00abff")
 
ggplot(eutrophierung_fluesse_gueteklassen, aes(factor(`Qualityclass II-III`))) + geom_bar()
 
ggplot(eutrophierung_fluesse_gueteklassen = NULL, mapping = aes(`Qualityclass II-III`, `Year eutrophication`))

"p1" <- ggplot(eutrophierung_fluesse_gueteklassen, aes(x = `Year eutrophication`, y = `Total eutrophication`)) + geom_bar()

"p1" <- ggplot(eutrophierung_fluesse_gueteklassen, aes(x = `Year eutrophication`, y = `Total eutrophication`))
"p1" + geom_line()
"p1" + geom_line(colour = "green")
"p1" + geom_point()
"p1" + geom_point(colour = "red")

"group" <- c(`Qualityclass II-III`, `Qualityclass III`, `Qualityclass III-IV`)

don <- eutrophierung_fluesse_gueteklassen %>% 
  filter(`Qualityclass II-III`, `Qualityclass III`, `Qualityclass III-IV`) %>%
  filter(`Year eutrophication`)
don %>%
  ggplot(aes(x=`Year eutrophication`, y=`Qualityclass II-III`, color=`Qualityclass II-III`)) +
  geom_line()

#ohne Punkte 
don %>%
  ggplot(aes(x=`Year eutrophication`, y=`Total eutrophication`, color=`Total eutrophication`)) +
  geom_line()

# mit Punkten 

don %>% ggplot(aes(x=`Year eutrophication`, y=`Total eutrophication`, color=`Total eutrophication`)) +
  geom_line()+
  geom_point() +
  transition_reveal(`Year eutrophication`)

"Qualityclass" <- c(`Qualityclass II-III`, `Qualityclass III`, `Qualityclass III-IV`, `Qualityclass IV`) 

view(Qualityclass)

plot(`Year eutrophication`, Qualityclass)

ggplot(eutrophierung_fluesse_gueteklassen aes(x=`Year eutrophication`, Y=q))

view(PhosphatLW_pro_Jahr)

str(PhosphatLW_pro_Jahr)

# nur NAmean(`Qualityclass II-III`, trim = 0, na.rm = FALSE)

# nur NA mean(`Qualityclass III`)

# nur NA mean(`Qualityclass III-IV`)

# nur NA mean(`Qualityclass IV`)

# nur NA mean(`Total eutrophication`, trim = 0.5, na.rm = FALSE)

# nur NA median(`Qualityclass II-III`, na.rm = FALSE)

"Phosphat" <- c(PhosphatLW_pro_Jahr$Verbrauch_PhosphatLW_t)

"Year Phosphat" <- c(PhosphatLW_pro_Jahr$Jahr)

head(PhosphatLW_pro_Jahr, n = 3)

tail(PhosphatLW_pro_Jahr, n = -3L)

tail(PhosphatLW_pro_Jahr, n = -7L)

tail(PhosphatLW_pro_Jahr, c(5L, -1L))

plot(`Year Phosphat`, Phosphat)

# nur NA mean(Phosphat, trim = 0, na.rm = FALSE)

# nur NA median(Phosphat, na.rm = FALSE)

boxplot(Phosphat)

# Macht keinen Sinn ggplot(PhosphatLW_pro_Jahr, aes(factor(Phosphat))) + geom_bar()

"s" <- ggplot(PhosphatLW_pro_Jahr, mapping = aes(Phosphat, `Year Phosphat`))
"s"+ geom_bar() 
"s" + geom_bar(fill = "red") 
"s" + geom_bar(colour = "red")
"s" + geom_bar(fill = "white", colour = "red")
"s" + geom_bar(fill = "#00abff")
"s" + geom_bar(fill = NA, colour = "#00abff")

don1 <- PhosphatLW_pro_Jahr %>% 
  filter(Phosphat) %>%
  filter(`Year Phosphat`)

don1 %>%
  ggplot(aes(x=`Year Phosphat`, y=Phosphat, color=Phosphat)) +
  geom_line()+
  geom_point() +
  transition_reveal(`Year Phosphat`, range = NULL)

# Package 

install.packages("gifski")

install.packages("av")

hrbrthemes::import_roboto_condensed()

# Analyse zusammen 

Year <- dataset$Jahr

boxplot(`Total eutrophication`, Phosphat)

shapiro.test(`Total eutrophication`)

shapiro.test(Phosphat)

hist(`Total eutrophication`)

hist(Phosphat)
