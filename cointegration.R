
library(tidyverse)
library(vars)


df <- read_csv("final_data.csv")


df_na <- df %>% na.omit()
df_long <- df %>% dplyr::select(-c(utlan_heimila, utlan_breyting, utlan_hrodun, greidd_husaleiga)) %>% na.omit()



# -------------------------------------------------------------------------


VARselect(df_long[,-1], lag.max = 12, type = "both")

co_ibud <- ca.jo(as.matrix(df_long[,-1]),
                 spec = "longrun",
                 type = "eigen",
                 K = 12,
                 ecdet = c("none"),
                 season = 12)

summary(co_ibud)
