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
args(decision_tree)
# lub 
?decision_tree()

decision_tree <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) |>
  set_engine("rpart") |>
  set_mode("classification")

# Utworzenie workflow
tree_work <- workflow() |>
  add_model(decision_tree) |>
  add_recipe(air_recipe)
# Stworzenie siatki parametrów
siatka <- grid_regular(
  cost_complexity(),
  tree_depth(),
  min_n(),
  levels = 5
)
siatka
# Przygotowanie foldów CV
set.seed(234)
folds <- vfold_cv(train_data, v = 5)

# Zdefiniowanie metryk oceny
metrics <- metric_set(
  accuracy,
  roc_auc,
  precision,
  recall
)

# Dostrajanie modelu
tree_tuning <- tune_grid(
  tree_work,
  resamples = folds,
  grid = siatka,
  metrics = metrics
)

best_model <- select_best(tree_tuning, metric ="accuracy")
# Wyświetlenie najlepszych wyników
print("Najlepsze wyniki dla drzewa decyzyjnego:")
show_best(tree_tuning,  metric ="accuracy")

tree_tuning |> collect_metrics()
fit_tree %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

final_mod <-  
  work |> 
  finalize_workflow(best_model)

final_fit <- 
  final_mod |> 
  last_fit(split = split)

final_fit %>%
  collect_metrics()

final_fit |> 
  collect_predictions() |> 
  roc_curve(truth = class, .pred_PS) |> 
  autoplot()

final_fit |> extract_workflow()

# wykres 

final_fit |> 
  extract_workflow() |> 
  extract_fit_parsnip() |>
  vip() 




rand_forest <- 
  rand_forest(mtry = tune(),
              min_n = tune(),
              trees = tune()) |>
  set_engine(engine = "ranger") |> 
  set_mode("classification")

# Utworzenie workflow
tree_work <- workflow() |>
  add_model(rand_forest) |>
  add_recipe(air_recipe)
# Stworzenie siatki parametrów
siatka <- grid_regular(
  mtry(range=c(1,5)),
  trees(),
  min_n(),
  levels = 5
)
siatka
# Przygotowanie foldów CV
set.seed(234)
folds <- vfold_cv(train_data, v = 5)

# Zdefiniowanie metryk oceny
metrics <- metric_set(
  accuracy,
  roc_auc,
  precision,
  recall
)

# Dostrajanie modelu
tree_tuning <- tune_grid(
  tree_work,
  resamples = folds,
  grid = siatka,
  metrics = metrics
)

best_model <- select_best(tree_tuning, metric ="accuracy")
# Wyświetlenie najlepszych wyników
print("Najlepsze wyniki dla drzewa decyzyjnego:")
show_best(tree_tuning,  metric ="accuracy")

tree_tuning |> collect_metrics()
fit_tree %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

final_mod <-  
  work |> 
  finalize_workflow(best_model)

final_fit <- 
  final_mod |> 
  last_fit(split = split)

final_fit %>%
  collect_metrics()

final_fit |> 
  collect_predictions() |> 
  roc_curve(truth = class, .pred_PS) |> 
  autoplot()

final_fit |> extract_workflow()

# wykres 

final_fit |> 
  extract_workflow() |> 
  extract_fit_parsnip() |>
  vip() 


