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

air_recipe |> summary()

model <- 
  logistic_reg() |> 
  set_engine("glm")

model_work <- 
  workflow() |> 
  add_model(model) |> 
  add_recipe(air_recipe)

model_work_fit <-  
  model_work |> 
  fit(data = train_data)

model_work_fit |> 
  extract_fit_parsnip() |> 
  tidy()

model_work_fit|> 
  extract_recipe()

prediction <- 
  augment(model_work_fit, test_data)


prediction  |> 
  roc_curve(truth = ozone, .pred_Niskie) |> 
  autoplot()

