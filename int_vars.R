
# Interactioin terms ------------------------------------------------------

df <- read_csv("final_data.csv")
df_na <- df %>% na.omit()

df_train <- df_na %>% 
  filter(year(date) < 2018)

df_test <- df_na %>% 
  filter(year(date) >= 2018)


library(tidyverse)
library(lubridate)
library(tidymodels)
library(caret)


# -------------------------------------------------------------------------


main_rec <- 
  recipe(ibudarhus ~ max_hlutfall + laegstu_vextir + greidd_husaleiga + utborgad + utlan_breyting + utlan_hrodun,
         data = df_na)


# -------------------------------------------------------------------------


int_vars <- main_rec %>% 
  pluck("var_info") %>% 
  dplyr::filter(role == "predictor") %>% 
  pull(variable)

interactions <- t(combn(as.character(int_vars), 2))
colnames(interactions) <- c("var1", "var2")


interactions <- 
  interactions %>% 
  as_tibble() %>% 
  mutate(
    term = 
      paste0(
        "starts_with('",
        var1,
        "'):starts_with('",
        var2,
        "')"
      )
  ) %>%
  pull(term) %>% 
  paste(collapse = "+")


interactions <- paste("~", interactions)
interactions <- as.formula(interactions)


int_rec <- 
  recipe(ibudarhus ~ max_hlutfall + laegstu_vextir + greidd_husaleiga + utborgad + utlan_breyting + utlan_hrodun,
         data = df_na) %>% 
  step_interact(interactions) %>% 
  step_interact(terms = ~ max_hlutfall:max_hlutfall) %>% 
  step_interact(terms = ~ laegstu_vextir:laegstu_vextir) %>% 
  step_interact(terms = ~ greidd_husaleiga:greidd_husaleiga) %>% 
  step_interact(terms = ~ utlan_breyting:utlan_breyting) %>% 
  step_interact(terms = ~ utlan_hrodun:utlan_hrodun) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())



# -------------------------------------------------------------------------



ctrl <- trainControl(method = "timeslice",
                     initialWindow = 60,
                     fixedWindow = FALSE,
                     horizon = 24)


int_glmnet <- train(int_rec,
                    data = df_train,
                    method = "enet",
                    tuneLength = 20,
                    trControl = ctrl)


# ---
enet_result <- as_tibble(int_glmnet$results) %>% 
  filter(lambda > 0)

enet_res_g <- ggplot(enet_result,
                     aes(x = fraction,
                         y = RMSE,
                         col = as.character(lambda))) + 
  geom_line()

plotly::ggplotly(enet_res_g)


# ---
studlar <- as_tibble(predict(int_glmnet$finalModel, type = "coefficient")$coefficient) %>% head(17) %>% tail(1)

studlar <- studlar %>% 
  pivot_longer(cols = max_hlutfall:utlan_breyting_x_utlan_hrodun,
               names_to = "breyta",
               values_to = "studlamat")

studlar <- studlar %>% 
  arrange(desc(studlamat))

ggplot(studlar, aes(x = fct_reorder(breyta, -studlamat), y =  studlamat)) + geom_col() + coord_flip()


# -------------------------------------------------------------------------

library(randomForest)

y <- df_train$ibudarhus
x <- df_train %>% dplyr::select(-date, -ibudarhus)

rf_main <- randomForest(x = x,
                        y = y,
                        importance = TRUE)