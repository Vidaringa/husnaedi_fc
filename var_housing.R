
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

house_var <- VAR(df_var[,-1],
                 lag.max = 24,
                 ic = "FPE",
                 season = 12,
                 type = "const")


house_irf <- irf(house_var,
                 response = "d_hus",
                 n.ahead = 12,
                 ortho = FALSE)

plot(house_irf)


pred <- predict(house_var,
                n.ahead = 12,
                response = "d_hus")

plot(pred)
