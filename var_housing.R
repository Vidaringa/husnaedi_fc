
# VAR model for the Icelandic housing market


library(tidyverse)
library(vars)


df_final <- read_csv("final_data.csv")
df_na <- df %>% na.omit()


df_var <- df_final %>% dplyr::select(date,
                               d_hus,
                               d_utb,
                               max_hlutfall,
                               laegstu_vextir,
                               # utlan_breyting,
                               utlan_hrodun) %>% 
  na.omit()



# -------------------------------------------------------------------------

VARselect(df_var[,-1])

house_var <- VAR(df_var[,-1],
                 lag.max = 12,
                 ic = "FPE",
                 season = 12)


house_var_pred <- predict(house_var,
                          n.ahead = 12)


house_irf <- irf(house_var)

plot(house_irf)
