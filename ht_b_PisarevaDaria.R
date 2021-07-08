library(readr)
library(tidyverse)
library(dplyr)
library(lmtest)
library(stats)
library(sandwich)
library(ggplot2)
library(hrbrthemes)
library(ggExtra)

#Зафиксируем seed для воспроизводимости результатов
set.seed(100)

#Назовем наш датасет country
country <- read_csv("/Users/daraprohorova/Desktop/Эконометрика/country_profile_variables.csv")
#Разделим здесь labor force participation rateдля мужчин и женщин (male_part и fem_part, соответственно)
d <- country %>% separate(`Labour force participation (female/male pop. %)`, c("fem_part", "male_part"), sep="/") %>% 
  select(country, Region, fem_part, male_part) 
country_m <- left_join(country, d, by=c("country", "Region")) 
country_m$gdp_per_cap <- country_m$`GDP per capita (current US$)`

# Почистим данные по participation rate
country_m$fem_part[country_m$fem_part == "..."] <- NA
country_m$male_part[country_m$male_part == "..."] <- NA
country_m$fem_part <- as.numeric(country_m$fem_part)
country_m$male_part <- as.numeric(country_m$male_part)
country_m <- mutate(country_m, gender_gap_part = male_part - fem_part)

#Выберу наиболее релевантные индикаторы, по моему мнению
country_an <- country_m %>% select(country, Region, `Population in thousands (2017)`, gdp_per_cap, 
                                 `Economy: Services and other activity (% of GVA)`, 
                                 `Employment: Services (% of employed)`, `Employment: Industry (% of employed)`,
                                 `International trade: Balance (million US$)`, 
                                 `Population growth rate (average annual %)`, 
                                 `Urban population (% of total population)`, 
                                 `Urban population growth rate (average annual %)`, 
                                 `Fertility rate, total (live births per woman)`,
                                 `Life expectancy at birth (females/males, years)`, 
                                 `Population age distribution (0-14 / 60+ years, %)`,
                                 `Infant mortality rate (per 1000 live births`, 
                                 `Health: Total expenditure (% of GDP)`, 
                                 `Seats held by women in national parliaments %`, 
                                 `Education: Primary gross enrol. ratio (f/m per 100 pop.)`, 
                                 `Education: Secondary gross enrol. ratio (f/m per 100 pop.)`, 
                                 `Education: Tertiary gross enrol. ratio (f/m per 100 pop.)`, fem_part, male_part, 
                                 gender_gap_part)

# Почистим данные по регрессору life expectancy
d <- country_an %>% separate(`Life expectancy at birth (females/males, years)`, 
                                      c("fem_life_exp", "male_life_exp"), sep="/") %>% 
  select(country, Region, fem_life_exp, male_life_exp) 
d$fem_life_exp[d$fem_life_exp == "..."] <- NA
d$male_life_exp[d$male_life_exp == "..."] <- NA
d$fem_life_exp <- as.numeric(d$fem_life_exp)
d$male_life_exp <- as.numeric(d$male_life_exp)
d <- mutate(d, gender_gap_life_exp = male_life_exp - fem_life_exp)
country_an <- left_join(country_an, d, by=c("country", "Region")) 

# Почистим данные по регрессору population age distribution
d <- country_an %>% separate(`Population age distribution (0-14 / 60+ years, %)`, 
                             c("young", "old"), sep="/") %>% 
  select(country, Region, young, old) 
d$young[d$young == "..."] <- NA
d$old[d$old == "..."] <- NA
d$young <- as.numeric(d$young)
d$old <- as.numeric(d$old)
country_an <- left_join(country_an, d, by=c("country", "Region")) 

# Почистим данные по регрессору education primary
d <- country_an %>% separate(`Education: Primary gross enrol. ratio (f/m per 100 pop.)`, 
                             c("fem_prim", "male_prim"), sep="/") %>% 
  select(country, Region, fem_prim, male_prim) 
d$fem_prim[d$fem_prim == "..."] <- NA
d$male_prim[d$male_prim == "..."] <- NA
d$fem_prim <- as.numeric(d$fem_prim)
d$male_prim <- as.numeric(d$male_prim)
d <- mutate(d, gender_gap_primary = male_prim - fem_prim)
country_an <- left_join(country_an, d, by=c("country", "Region")) 

# Почистим данные по регрессору education secondary
d <- country_an %>% separate(`Education: Secondary gross enrol. ratio (f/m per 100 pop.)`, 
                             c("fem_sec", "male_sec"), sep="/") %>% 
  select(country, Region, fem_sec, male_sec) 
d$fem_sec[d$fem_sec == "..."] <- NA
d$male_sec[d$male_sec == "..."] <- NA
d$fem_sec <- as.numeric(d$fem_sec)
d$male_sec <- as.numeric(d$male_sec)
d <- mutate(d, gender_gap_sec = male_sec - fem_sec)
country_an <- left_join(country_an, d, by=c("country", "Region")) 

# Почистим данные по регрессору education tertiary
d <- country_an %>% separate(`Education: Tertiary gross enrol. ratio (f/m per 100 pop.)`, 
                             c("fem_tert", "male_tert"), sep="/") %>% 
  select(country, Region, fem_tert, male_tert) 
d$fem_tert[d$fem_tert == "..."] <- NA
d$male_tert[d$male_tert == "..."] <- NA
d$fem_tert <- as.numeric(d$fem_tert)
d$male_tert <- as.numeric(d$male_tert)
d <- mutate(d, gender_gap_tert = male_tert - fem_tert)
country_an <- left_join(country_an, d, by=c("country", "Region")) 

# Оставим только те данные, которые нам необходимы в дальнейшем для построения регрессии
country_upd <- country_an %>% select(country, Region, `Population in thousands (2017)`, gdp_per_cap, 
                                    `Economy: Services and other activity (% of GVA)`, 
                                    `Employment: Services (% of employed)`, `Employment: Industry (% of employed)`,
                                    `International trade: Balance (million US$)`, 
                                    `Population growth rate (average annual %)`, 
                                    `Urban population (% of total population)`, 
                                    `Urban population growth rate (average annual %)`, 
                                    `Fertility rate, total (live births per woman)`,
                                    `Infant mortality rate (per 1000 live births`, 
                                    `Health: Total expenditure (% of GDP)`, 
                                    `Seats held by women in national parliaments %`, fem_part, fem_life_exp, 
                                    fem_prim, fem_sec, fem_tert, male_part, male_life_exp, male_prim, male_sec, 
                                    male_tert, gender_gap_part,gender_gap_life_exp, gender_gap_primary, 
                                    gender_gap_sec, gender_gap_tert, young, old)

# Переименуем переменные, чтобы код в дальнейшем занимал меньше места и удобнее было обращаться к этим переменным.
country_upd$services_econ <- as.numeric(country_upd$`Economy: Services and other activity (% of GVA)`)
country_upd$services_empl <- as.numeric(country_upd$`Employment: Services (% of employed)`)
country_upd$balance <- as.numeric(country_upd$`International trade: Balance (million US$)`)
country_upd$pop_growth <- as.numeric(country_upd$`Population growth rate (average annual %)`)
country_upd$pop <- as.numeric(country_upd$`Population in thousands (2017)`)
country_upd$pop_urban <- as.numeric(country_upd$`Urban population (% of total population)`)
country_upd$pop_urban_growth <- as.numeric(country_upd$`Urban population growth rate (average annual %)`)
country_upd$fertility <- as.numeric(country_upd$`Fertility rate, total (live births per woman)`)
country_upd$inf_mort <- as.numeric(country_upd$`Infant mortality rate (per 1000 live births`)
country_upd$health <- as.numeric(country_upd$`Health: Total expenditure (% of GDP)`)
country_upd$fem_seats <- as.numeric(country_upd$`Seats held by women in national parliaments %`)

# imputing missing values (медиана устойчивая к выбросам)
country_upd$fem_life_exp[country_upd$fem_life_exp < 0] <- median(country_upd$fem_life_exp[
  country_upd$fem_life_exp > 0])
country_upd$fem_life_exp[is.na(country_upd$fem_life_exp)] <- median(country_upd$fem_life_exp[
  !is.na(country_upd$fem_life_exp)])

country_upd$fem_part[country_upd$fem_part < 0] <- median(country_upd$fem_part[country_upd$fem_part > 0])
country_upd$fem_part[is.na(country_upd$fem_part)] <- median(country_upd$fem_part[!is.na(country_upd$fem_part)])

country_upd$fem_prim[country_upd$fem_prim < 0] <- median(country_upd$fem_prim[country_upd$fem_prim > 0])
country_upd$fem_prim[is.na(country_upd$fem_prim)] <- median(country_upd$fem_prim[!is.na(country_upd$fem_prim)])

country_upd$fem_sec[country_upd$fem_sec < 0] <- median(country_upd$fem_sec[country_upd$fem_sec > 0])
country_upd$fem_sec[is.na(country_upd$fem_sec)] <- median(country_upd$fem_sec[!is.na(country_upd$fem_sec)])

country_upd$fem_tert[country_upd$fem_tert < 0] <- median(country_upd$fem_tert[country_upd$fem_tert > 0])
country_upd$fem_tert[is.na(country_upd$fem_tert)] <- median(country_upd$fem_tert[!is.na(country_upd$fem_tert)])

country_upd$inf_mort[country_upd$inf_mort < 0] <- median(country_upd$inf_mort[country_upd$inf_mort > 0])
country_upd$inf_mort[is.na(country_upd$inf_mort)] <- median(country_upd$inf_mort[!is.na(country_upd$inf_mort)])

country_upd$health[country_upd$health < 0] <- median(country_upd$health[country_upd$health > 0])
country_upd$health[is.na(country_upd$health)] <- median(country_upd$health[!is.na(country_upd$health)])

country_upd$fertility[country_upd$fertility < 0] <- median(country_upd$fertility[country_upd$fertility > 0])
country_upd$fertility[is.na(country_upd$fertility)] <- median(country_upd$fertility[!is.na(country_upd$fertility)])

country_upd$gdp_per_cap[country_upd$gdp_per_cap < 0] <- median(country_upd$gdp_per_cap[country_upd$gdp_per_cap > 0])
country_upd$gdp_per_cap[is.na(country_upd$gdp_per_cap)] <- median(country_upd$gdp_per_cap[
  !is.na(country_upd$gdp_per_cap)])

country_upd$fem_seats[country_upd$fem_seats < 0] <- median(country_upd$fem_seats[country_upd$fem_seats > 0])
country_upd$fem_seats[is.na(country_upd$fem_seats)] <- median(country_upd$fem_seats[!is.na(country_upd$fem_seats)])

#----------------------------------------------------------------------------------------------------------------------------------#
#Задание №1
#Исследовательский вопрос: Я собираюсь изучить, как влияет участие женщин в парламенте на ВВП на душу населения страны. 
#Таким образом, непрерывная зависимая переменная будет ВВП на душу населения.

#Задание №2
#Мы будем использовать следующие объясняющие переменные (в первой спецификации модели): inf_mort - коэффициент детской смерности; services_econ - сервисы и другая активность в экономике;
#pop_growth - коэффициент роста населения; fem_tert - коэффициент образования женщин; male_tert - коэффициент образования женщин
#fem_part - коэффицент участия женщин в политической жизни
#fem_seats - количество мест, которые занимают женщины в парламенте
#pop_urban_growth - коэффициент роста населения в городах
#Все переменные, кроме непосредственно количества мест в парламенте, которые занимают женщины в дальнейшем будут сопуствующими и влияющими на главную объясняющую переменную.
#Также возмодно изменение набора объясняющих переменных в ходе работы.

#Задание №3
ggplot(country_upd, aes(x=gender_gap_part, y=log(gdp_per_cap+1))) + 
  geom_point(color="#69b3a2") +
  geom_smooth(method=lm, color="red", fill="#69b3a2", se=TRUE)
#Можно заметить, что тут не наблюдается причинно-следственной связи между переменными

ggplot(country_upd, aes(x=fem_part, y=log(gdp_per_cap+1))) + 
  geom_point(color="#69b3a2") +
  geom_smooth(method=lm, color="red", fill="#69b3a2", se=TRUE)
#+1 здесь чтобы значение всегда было осмысленным

ggplot(country_upd, aes(x=fem_life_exp, y=inf_mort)) + 
  geom_point(color="#69b3a2") +
  geom_smooth(method=lm, color="red", fill="#69b3a2", se=TRUE) 
#В этом случае нельзя сказать, в какую сторону работает причинно-следственная связь (или младенцы много умирают потому что женщины не здоровы, или они не здоровы, потому что медицина в отношении маленьких детей не очень хорошо развита). 
#Так, можно использовать как регрессор только одну из них, иначе появляется риск возникновения мультиколлинеарности между регрессорами.

# Используем логарфим, так как это сглаживает разброс данных
ggplot(country_upd, aes(x=log(fem_life_exp), y=health)) + 
  geom_point(color="#69b3a2") +
  geom_smooth(method=lm, color="red", fill="#69b3a2", se=TRUE) 

ggplot(country_upd, aes(x=fem_seats, y=fem_tert)) + 
  geom_point(color="#69b3a2") +
  geom_smooth(method=lm, color="red", fill="#69b3a2", se=TRUE) 

ggplot(country_upd, aes(x=log(inf_mort+1), y=fertility)) +  
  geom_point(color="#69b3a2") +
  geom_smooth(method=lm, color="red", fill="#69b3a2", se=TRUE) 
#Здесь наблюдается интересная и логичная взаимосвязь -получается, в странах с более высокой рождаемостью будет скорее более высокая младенческая смертность. 
#Так, можно использовать как регрессор только одну из них, иначе появляется риск возникновения мультиколлинеарности между регрессорами.

#Задание №4/5
#Попробуем задать спецификацию модели lm (linear model)

reg1 <- lm(log(gdp_per_cap) ~ inf_mort + services_econ + pop_growth + fem_tert + male_tert + fem_part + 
             fem_seats + pop_urban_growth, data = country_upd)
coeftest(reg1, vcov = vcovHC(reg1, type = 'HC1'))
# Что мы видим. 
# 1. Рост доли женщин в парламенте на 1% приводит к росту ВВП на душу населения на 0.2%, но оценка не является статистически значимой; 
# 2. Рост младенческой смертности (на 1 ребенка на 1000 выживших) снижает ВВП на д.н. на 3.3% (значимость высокая);
# 3. Интересно, что повышение ростагородского населения снижает ВВП на д.н. на 21%(хорошая значимость у этой оценки);
# 4. Имеет смысл переопределить модель, поскольку согласно оцененной увеличение прироста населения на 1% увеличивает ВВП на душу на 30% (экономическая интуиция плачет в этот момент, по-моему).

#Давайте теперь проверим, какие детерминанты могут быть у доли женщин в парламенте.
#Для начала добавим долю долю людей которые не young и не old:
country_upd$avg_age <- 100 - country_upd$old - country_upd$young

reg2 <- lm(fem_seats ~ fem_tert + fem_life_exp + pop_urban_growth + fem_part + 
             avg_age + fertility, data = country_upd)
coeftest(reg2, vcov = vcovHC(reg2, type = 'HC1')) 

reg3 <- lm(fem_seats ~ fem_tert + fem_life_exp + pop_urban_growth + fem_part + 
             avg_age  + fertility, data = country_upd)
coeftest(reg3, vcov = vcovHC(reg3, type = 'HC1')) 

#Разобъем выборку стран на две группы исходя из младенческой смертности. Для начала взглянем на медиану и среднее:
median(country_upd$inf_mort)
mean(country_upd$inf_mort)

#Видим, что среднее выше медианы, потому можно сказать, что в среди стран находящихся по уровню детской смертности выше медианы, наблюдается высокий разброс в этом показателе (иными словами,есть страны с этим показателем немного выше медианы, а есть с очень высоким значением).
#Взглянем на bar charts для стран с самой высокой младенческой смертностью и для стран с самой низкой младенческой смертностью:
country_upd %>% 
  arrange(desc(inf_mort)) %>% 
  top_n(20, inf_mort) %>% 
  ggplot(aes(x = inf_mort, y = reorder(country, health), color = health, fill = health)) + 
  geom_bar(stat = "identity")

country_upd %>% 
  arrange(desc(inf_mort)) %>% 
  top_n(-20, inf_mort) %>% 
  ggplot(aes(x = inf_mort, y = reorder(country, health), color = health, fill = health)) + 
  geom_bar(stat = "identity")

# Мне кажется, расходы на здравоохранение не особо коррелируют с младенческой смертностью:
ggplot(country_upd, aes(health, inf_mort)) +
  geom_point() + 
  theme_bw() + 
  labs(y = 'Младенческая смертность', x = 'Расходы на здравоохранение') + 
  stat_smooth(method = "lm", formula = y ~ x)

# В общем, в качестве границы возьмем младенческую смертность, которую предскажет обычная линейная регрессия (как на графике ранее) для медианного значения расходов на здравоохранение:
reg_thres <- lm(inf_mort ~ health, data = country_upd)
med_heal <- median(country_upd$health)
threshold <- as.numeric(predict(reg_thres, data.frame(health = c(med_heal))))
#Создадим переменную, которая для значений выше threshold будет равна 0 (неразвитая медицина):
country_upd$inf_dummy <- as.numeric(as.numeric(country_upd$inf_mort) <= threshold)

# Воспользуемся нашей дамми-переменной:
reg4 <- lm(fem_seats ~ fem_tert + fem_life_exp + pop_urban_growth + fem_part +
             avg_age + fertility + inf_dummy * gender_gap_life_exp + gender_gap_life_exp
           + log(gdp_per_cap), 
           data = country_upd)
coeftest(reg4, vcov = vcovHC(reg4, type = 'HC1')) 
#Результат означает, что в странах с "развитой медициной" (где уровень младенческой смертности ниже найденного threshold) gender gap продолжительности жизни оказывает положительный эффект на долю женщин в парламенте (полный эффект gender gap оказывается положительным). 
#В странах с высокой младенческой смертностью эффект этого gender gap обратный -- его увеличение на 1 год (разница между мужской продолжительностью жизни и женской) приводит к снижению числа женщин в парламенте на 1.74%.

#Красивые графики:
plot1 <- ggplot(country_upd, aes(fem_seats, fem_part, color = fem_life_exp)) +
  geom_point() + 
  theme_bw() + 
  labs(x = 'Seats held by women in national parliaments (in %)', y = 'Female labor force participation rate (in %)', 
       colour = 'Female life \nexpectancy', title = 'Зависимость доли женщин в парламенте\nот участия женщин в рабочей силе') + 
  stat_smooth(method = "lm", formula = y ~ x)
ggMarginal(plot1, type = "histogram", xparams = list(binwidth = 2, fill = "darkgrey"),
           yparams = list(binwidth = 2, fill = "darkgrey"), color = 'white')

plot2 <- ggplot(country_upd, aes(avg_age, pop_urban, color = fem_life_exp)) +
  geom_point() + 
  theme_bw() + 
  labs(x = 'Average age', y = 'Urban population (in %)', 
       colour = 'Female life \nexpectancy', title = 'Зависимость городского населения (в %)\nот среднего возраста') + 
  stat_smooth(method = "lm", formula = y ~ x, color = 'darkgreen')
ggMarginal(plot2, type = "histogram", xparams = list(binwidth = 2, fill = "darkgrey"),
           yparams = list(binwidth = 2, fill = "darkgrey"), color = 'white')

plot3 <- ggplot(country_upd, aes(log(gdp_per_cap), fertility, color = fem_life_exp)) +
  geom_point() + 
  theme_bw() + 
  labs(x = 'Логарифм ВВП на душу населения', y = 'Фертильность', 
       colour = 'Female life \nexpectancy', title = 'Зависимость ВВП на душу населения \nот фертильности') + 
  stat_smooth(method = "lm", formula = y ~ x, color = 'darkgreen')
ggMarginal(plot3, type = "histogram", xparams = list(binwidth = .5, fill = "darkgrey"),
           yparams = list(binwidth = .5, fill = "darkgrey"), color = 'white')





