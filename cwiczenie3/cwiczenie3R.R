library(tidymodels) 
library(skimr) 
library(GGally) 
library(openair) 
tidymodels_prefer()

air <- mydata |> selectByDate(year = 2002) 
air |> skim()
air
air <- air |> na.omit()
air
set.seed(222)
air[sample(1:nrow(air), size = 300, replace = F),] |> 
  select(nox, no2) |> 
  ggpairs()

library(ggpubr)
# wykres regresji liniowej, do sprawdzenia danych 
set.seed(222)
air[sample(1:nrow(air), size = 300, replace = F),] |> 
  select(nox, no2) |> 
  ggplot(aes(nox, no2)) +
  geom_point() +
  geom_smooth(method = "lm", se = T, formula = y ~ x) + 
  stat_cor(label.x = 10, label.y = 80) + 
  stat_regline_equation(label.x = 10, label.y = 82) +
  theme_bw()

air |>    
  ggplot(aes(date, o3)) +     
  geom_line() +     
  theme_bw()

air |> 
  pull(o3) |> 
  range()  

air <-
  air |>
  mutate(ozone = cut(
    o3,
    breaks = c(-0.1, 10, 53),
    labels = c("Niskie", "Wysokie")
  ))
air |> count(ozone)

set.seed(222)
data_split <- initial_split(data = air, prop = 3/4)
train_data <- training(data_split)
test_data <-  testing(data_split)

air
air_recipe <- recipe(ozone ~ ., data = train_data) |> 
  update_role(date, nox, o3 ,new_role="nowe_inf") 

regresja <- 
  logistic_reg() |> 
  set_engine("glm")

regresja_work <- 
  workflow() |> 
  add_model(regresja) |> 
  add_recipe(air_recipe)

las_losowy <- 
  rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification")

las_work <- 
  workflow() |> 
  add_model(las_losowy) |> 
  add_recipe(air_recipe)

#walidacja krzyżowa

folds <- vfold_cv(train_data, v = 10, strata = ozone)
folds

results_regresja <-
  regresja_work |>
  fit_resamples(folds)

results_regresja |> 
  collect_metrics() |> 
  knitr::kable(digits = 3)

results_las <-
  las_work |>
  fit_resamples(folds)

results_las |> 
  collect_metrics() |> 
  knitr::kable(digits = 3)

# Repeated cross validation

folds <- vfold_cv(train_data, v = 10, strata = ozone, repeats = 5)
folds

results_regresja <-
  regresja_work |>
  fit_resamples(folds)

results_regresja |> 
  collect_metrics() |> 
  knitr::kable(digits = 3)

results_las <-
  las_work |>
  fit_resamples(folds)

results_las |> 
  collect_metrics() |> 
  knitr::kable(digits = 3)



# Bootstrap

bootstrap <- bootstraps(train_data, times = 100, strata = ozone)


results_regresja <-
  regresja_work |>
  fit_resamples(bootstrap)

results_regresja |> 
  collect_metrics() |> 
  knitr::kable(digits = 3)

results_las <-
  las_work |>
  fit_resamples(bootstrap)

results_las |> 
  collect_metrics() |> 
  knitr::kable(digits = 3)

