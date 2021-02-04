library(tidyverse)
library(olsrr)

library(spdep)

neigh_matrix <- poly2nb(counties_geom)

neigh_list <- nb2listw(neigh_matrix)

summary(neigh_list)

neigh_mm <- nb2mat(neigh_matrix)

d <- readxl::read_xlsx("data/data_counties.xlsx") 

d2 <- d %>% 
  mutate(pop_500plus=number_500plus/population*100,
         TERYT=substr(TERYT,1,4))

d_model <- d2 %>% 
  select(-TERYT, -average_salary100, -population, -number_500plus)

model <- lm(average_salary ~ ., data = d_model)

ols_step_both_aic(model)

options(scipen = 9)

# lm

model_lm <- lm(average_salary ~ pop_500plus + unemployment_rate + capital_distance, data = d_geom)
summary(model_lm)

predict(m1, d_model)

d_geom <- inner_join(counties_geom, d2)

class(d_geom)

ggplot(d_geom) +
  geom_sf(aes(fill = average_salary, geometry = geometry))

moran.test(m1$residuals, neigh_list)
moran.plot(m1$residuals, neigh_list)

moran.test(d_geom$average_salary, neigh_list)
moran.plot(d_geom$average_salary, neigh_list)

# sar

library(spatialreg)

model_sar <- lagsarlm(average_salary ~ pop_500plus + unemployment_rate + capital_distance, data = d_geom, listw = neigh_list)

summary(model_sar)

moran.test(model_sar$residuals, neigh_list)
moran.plot(model_sar$residuals, neigh_list)

# sem

model_sem <- errorsarlm(average_salary ~ pop_500plus + unemployment_rate + capital_distance, data = d_geom, listw = neigh_list)

summary(model_sem)

moran.test(model_sem$residuals, neigh_list)
moran.plot(model_sem$residuals, neigh_list)

AIC(model_lm, model_sar, model_sem)

# predict

lm_pred <- predict(model_lm, d_geom)

sar_pred <- as.data.frame(predict(model_sar, d_geom, neigh_list))

sem_pred <- as.data.frame(predict(model_sem, d_geom, neigh_list))

caret::postResample(pred = lm_pred, obs = d_geom$average_salary)

caret::postResample(pred = sar_pred$fit, obs = d_geom$average_salary)

caret::postResample(pred = sem_pred$fit, obs = d_geom$average_salary)


