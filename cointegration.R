
library(tidyverse)
library(vars)
library(lubridate)

df_final <- read_csv("final_data.csv")

df <- readxl::read_excel("data.xlsx") %>% janitor::clean_names()
df$date <- ymd(df$date)
df_na <- df %>% na.omit()


df_var <- df %>% dplyr::select(date,
                               d_hus,
                               d_utb,
                               max_hlutfall,
                               laegstu_vextir,
                               utlan_breyting,
                               utlan_hrodun) %>% 
  na.omit()



# -------------------------------------------------------------------------


VARselect(df_var[,-1], lag.max = 24, type = c("const"))

co_ibud <- ca.jo(as.matrix(df_long[,-1]),
                 spec = "longrun",
                 type = "eigen",
                 K = 12,
                 ecdet = c("none"),
                 season = 12)

summary(co_ibud)

