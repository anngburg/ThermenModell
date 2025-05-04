install.packages("lubridate")
library(tidyverse)

ThermenAuslastung_data <- read_csv("grafana_dashboard_data.csv")

head(ThermenAuslastung_data)

#umbenennen
ThermenAuslastung_data <- rename(ThermenAuslastung_data, 
                                 Parkhaus = 'Carolus Parkhaus',
                                 Sauna = 'Carolus Saunawelt',
                                 Therme = 'Carolus Thermalbad')

#%-Zeichen droppen
ThermenAuslastung_data$Parkhaus = sub(".$", "", ThermenAuslastung_data$Parkhaus)
ThermenAuslastung_data$Sauna = sub(".$", "", ThermenAuslastung_data$Sauna)
ThermenAuslastung_data$Therme = sub(".$", "", ThermenAuslastung_data$Therme)

#als Zahl
ThermenAuslastung_data$Parkhaus = as.numeric(ThermenAuslastung_data$Parkhaus)
ThermenAuslastung_data$Sauna = as.numeric(ThermenAuslastung_data$Sauna)
ThermenAuslastung_data$Therme = as.numeric(ThermenAuslastung_data$Therme)

#umstrukturieren
ThermenAuslastung_data <- ThermenAuslastung_data %>% 
                                        mutate(Date = date(Time),
                                               Time = format(Time, "%H:%M:%S")) %>%
                                        pivot_wider(names_from = Time, 
                                                    values_from = Parkhaus:Therme) %>%
                                        select(order(colnames(.))) 
                                        

ThermenAuslastung_data <- ThermenAuslastung_data %>% 
  unite(col = "Parkhaus_00:01", 'Parkhaus_00:00:00', 'Parkhaus_01:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Parkhaus_02:03", 'Parkhaus_02:00:00', 'Parkhaus_03:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Parkhaus_04:05", 'Parkhaus_04:00:00', 'Parkhaus_05:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Parkhaus_06:07", 'Parkhaus_06:00:00', 'Parkhaus_07:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Parkhaus_08:09", 'Parkhaus_08:00:00', 'Parkhaus_09:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Parkhaus_10:11", 'Parkhaus_10:00:00', 'Parkhaus_11:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Parkhaus_12:13", 'Parkhaus_12:00:00', 'Parkhaus_13:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Parkhaus_14:15", 'Parkhaus_14:00:00', 'Parkhaus_15:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Parkhaus_16:17", 'Parkhaus_16:00:00', 'Parkhaus_17:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Parkhaus_18:19", 'Parkhaus_18:00:00', 'Parkhaus_19:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Parkhaus_20:21", 'Parkhaus_20:00:00', 'Parkhaus_21:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Parkhaus_22:23", 'Parkhaus_22:00:00', 'Parkhaus_23:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>%
  mutate(across(starts_with("Parkhaus_"), as.numeric))%>% 
  unite(col = "Sauna_00:01", 'Sauna_00:00:00', 'Sauna_01:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Sauna_02:03", 'Sauna_02:00:00', 'Sauna_03:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Sauna_04:05", 'Sauna_04:00:00', 'Sauna_05:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Sauna_06:07", 'Sauna_06:00:00', 'Sauna_07:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Sauna_08:09", 'Sauna_08:00:00', 'Sauna_09:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Sauna_10:11", 'Sauna_10:00:00', 'Sauna_11:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Sauna_12:13", 'Sauna_12:00:00', 'Sauna_13:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Sauna_14:15", 'Sauna_14:00:00', 'Sauna_15:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Sauna_16:17", 'Sauna_16:00:00', 'Sauna_17:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Sauna_18:19", 'Sauna_18:00:00', 'Sauna_19:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Sauna_20:21", 'Sauna_20:00:00', 'Sauna_21:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Sauna_22:23", 'Sauna_22:00:00', 'Sauna_23:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>%
  mutate(across(starts_with("Sauna_"), as.numeric))%>% 
  unite(col = "Therme_00:01", 'Therme_00:00:00', 'Therme_01:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Therme_02:03", 'Therme_02:00:00', 'Therme_03:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Therme_04:05", 'Therme_04:00:00', 'Therme_05:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Therme_06:07", 'Therme_06:00:00', 'Therme_07:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Therme_08:09", 'Therme_08:00:00', 'Therme_09:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Therme_10:11", 'Therme_10:00:00', 'Therme_11:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Therme_12:13", 'Therme_12:00:00', 'Therme_13:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Therme_14:15", 'Therme_14:00:00', 'Therme_15:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Therme_16:17", 'Therme_16:00:00', 'Therme_17:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Therme_18:19", 'Therme_18:00:00', 'Therme_19:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Therme_20:21", 'Therme_20:00:00', 'Therme_21:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "Therme_22:23", 'Therme_22:00:00', 'Therme_23:00:00', sep = "", remove = TRUE, na.rm = TRUE) %>%
  mutate(across(starts_with("Therme_"), as.numeric))

#Wetterdaten hinzufügen    
niederschlag <- read_csv("cnwppd9c.csv")
temperatur <- read_csv("7h617c8y.csv")
sonnenstunden <- read_csv("lgnx5i6x.csv")

wetter <- cbind(temperatur, niederschlag$'Niederschlag (6 bis 6 UTC)', sonnenstunden$Sonnenschein)

wetter <- rename(wetter,
                 Date = category,
                 Temp_high = Höchstwert,
                 Temp_mid = Mittelwert,
                 Temp_low = Tiefstwert,
                 Rain = 'niederschlag$\"Niederschlag (6 bis 6 UTC)\"',
                 Sun = 'sonnenstunden$Sonnenschein')

wetter <- wetter %>% mutate(Date = dmy(paste0(Date, "2025")))

Thermen_data <- ThermenAuslastung_data %>% left_join(wetter, by = "Date")

Thermen_data <- Thermen_data %>%
                  rowwise() %>%
                  mutate(
                    Parkhaus_max = max(c_across(starts_with("Parkhaus_")), na.rm = TRUE),
                    Sauna_max = max(c_across(starts_with("Sauna_")), na.rm = TRUE),
                    Therme_max = max(c_across(starts_with("Therme_")), na.rm = TRUE)) 
                  

Thermen_data$prevSauna_max <- dplyr::lag(Thermen_data$Sauna_max, n = 1)
Thermen_data$prevParkh_max <- dplyr::lag(Thermen_data$Parkhaus_max, n = 1)
Thermen_data$prevTherme_max <- dplyr::lag(Thermen_data$Therme_max, n = 1)

Thermen_data$Weekday <- weekdays(Thermen_data$Date)

#Modell bauen
Thermen_Modell = lm(formula = Sauna_max ~ prevSauna_max + prevSauna_max + prevTherme_max + `Parkhaus_00:01` + `Parkhaus_02:03` + `Parkhaus_04:05` + `Parkhaus_06:07` + `Parkhaus_08:09` +
                      `Parkhaus_10:11` + `Parkhaus_12:13` +`Sauna_08:09` + `Sauna_10:11` + `Sauna_12:13` + `Therme_08:09` + `Therme_10:11` + `Therme_12:13` +
                      Temp_high + Temp_mid + Temp_low + Sun + Rain + as.factor(Weekday), data = Thermen_data, na.action = na.exclude)

summary(Thermen_Modell)
