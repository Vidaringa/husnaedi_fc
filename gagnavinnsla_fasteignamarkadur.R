
# Gagnavinnsla

library(tidyverse)
library(lubridate)
# library(tidymodels)
# library(caret)


df <- readxl::read_excel("data.xlsx") %>% janitor::clean_names()
df$date <- ymd(df$date)

utborgad <- read_csv("utborgud_laun.csv") %>% dplyr::select(arma_dags, utborgad)

df <- df %>% 
  left_join(utborgad, by = c("date" = "arma_dags"))


df %>% 
  pivot_longer(cols = ibudarhus:utborgad,
               names_to = "breyta",
               values_to = "gildi") %>% 
  ggplot(aes(x = date,
             y = gildi)) + 
  facet_wrap(~breyta, ncol = 3, scales = "free_y") +
  geom_line()


df %>% 
  mutate(verd_ratio = ibudarhus/greidd_husaleiga) %>% 
  ggplot(aes(x = date, y = verd_ratio)) +
  geom_line()




# Feature engineering -----------------------------------------------------

df <- df %>% 
  mutate(utlan_breyting = utlan_heimila/lag(utlan_heimila, 12) - 1,
         utlan_hrodun = utlan_breyting - lag(utlan_breyting),
         d_hus = ibudarhus/lag(ibudarhus, 12) - 1,
         d_utb = utborgad/lag(utborgad, 12) - 1,
         d_leiga = greidd_husaleiga/lag(greidd_husaleiga, 12) - 1)


write_csv(df, "final_data.csv")
