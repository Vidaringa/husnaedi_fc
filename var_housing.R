
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
                               utlan_breyting) %>% 
  na.omit()



# -------------------------------------------------------------------------

VARselect(df_var[,-1])

house_var <- VAR(df_var[,-1],
                 lag.max = 24,
                 ic = "SC",
                 season = 12)


house_var_pred <- predict(house_var,
                          n.ahead = 12)

house_irf <- irf(house_var,
                 response = "d_hus",
                 n.ahead = 12,
                 ortho = FALSE)


plot(house_irf)
