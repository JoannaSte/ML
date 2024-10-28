library(tidymodels)
library(parsnip)
library(readr)       
library(broom.mixed) 
library(ggplot2)
library(dotwhisker)
library(dplyr)
library(rstanarm)
library(GGally)

colnames(airquality) <- tolower(colnames(airquality))

air <-
  airquality |>
  as_tibble() |>
  na.omit() |> 
  select(-c(day)) |> 
  mutate(month = factor(month)) 

GGally::ggpairs(air, aes(color = month))


cor.test(air$ozone, air$solar.r)
cor.test(air$ozone, air$wind)
cor.test(air$ozone, air$temp)
cor.test(air$ozone, as.integer(air$month))




scaled_data <- air |>
  mutate(across(c(temp,wind, solar.r), scale))

print(scaled_data)

set.seed(30)
data_split <- initial_split(scaled_data, strata = ozone)
train_data <- training(data_split)
test_data <- testing(data_split)
x = test_data$ozone
y <-
  test_data |>
  select(-c(ozone))
print(y)
print(x)

model <-lm(ozone ~ temp + wind + solar.r + month, data = train_data)
summary(model)
predictions <- predict(model, newdata = y)
print(test_data)
print(predictions)
class(x)
class(predictions)


ggplot(air, aes(x=predictions, y=ozone)) + 
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predykcje', y='Prawdziwe wartości')


