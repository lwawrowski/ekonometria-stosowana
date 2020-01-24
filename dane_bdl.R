library(tidyverse)
library(bdl)
library(plm)

get_subjects()

get_subjects("K3")

get_subjects("G535")

search_subjects("rozwody")

search_subjects(name = "rozwody", parentId = "G535")

get_variables(subjectId = "P1347")

search_variables("stopa")

get_units(parentId = "011200000000")

search_units("wielkopolskie")

d <- get_data_by_variable(varId = "3723", unitLevel = 2, year = 2018)

d <- get_data_by_unit(unitId = "023000000000", varId = c("151866", "151867"), year = 2015:2018, type = "label")

# plm

stopa_bezr <- get_data_by_variable(varId=60270, unitLevel=2, year=2010:2018) %>% 
  rename(stopa_bezr=val)

lud_prod <- get_data_by_variable(varId=60566, unitLevel=2, year=2010:2018) %>% 
  rename(lud_prod=val)

dane <- inner_join(stopa_bezr, lud_prod) %>% 
  select(-id, -attrId, woj=name)

ggplot(dane, aes(x=woj, y=stopa_bezr)) + 
  geom_boxplot() +
  coord_flip()

m_pooling <- plm(stopa_bezr~lud_prod, data = dane, model = "pooling")
summary(m_pooling)

m_lsdv <- plm(stopa_bezr~lud_prod+woj-1, data = dane, model = "pooling")
summary(m_lsdv)

m_within <- plm(stopa_bezr~lud_prod, data = dane, model = "within")
summary(m_within)

summary(fixef(m_within))

m_between <- plm(stopa_bezr~lud_prod, data = dane, model = "between")
summary(m_between)

pFtest(m_within, m_pooling)

m_random <- plm(stopa_bezr~lud_prod, data = dane, model = "random")
summary(m_random)

ranef(m_random)
