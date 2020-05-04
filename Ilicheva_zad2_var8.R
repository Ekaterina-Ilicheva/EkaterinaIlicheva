  #Ильичева Eкатерина, ПАЭ-123, Вариант 8

  #Задание 2. Cоздайте модель множественной линейной регрессии ночных потоков 
#паров воды за период 2013 года по данным измерений методом турбулентной пульсации.

  #Указание и проверка рабочей директории
  setwd ("/Users/ekaterina/Desktop/Ilicheva mat model/zad 2")
  getwd()

  #Работа с библиотеками и установкой пакетов
  install.packages("tidyverse")
  library(tidyverse)
  
  install.packages("nycflights13")
  library("nycflights13")
  
  install.packages("tidyr")
  library("tidyr")
  
  install.packages("stringr")
  library("stringr")
  
  install.packages("dplyr")
  library("dplyr")
  
  install.packages("tibble")
  library("tibble")
  
  install.packages("readr")
  library("readr")
  
  install.packages("rnoaa")
  library(rnoaa)
  
  install.packages("lubridate")
  library(lubridate)
  
  install.packages("ggplot2")
  library("ggplot2")
  
#Cчитывание данных из файла, пропуская первую строку, заменяя текстовые NA, не числовые значения на NA пустые, игнорируя строки с "[" 
  eddypro = read_csv("eddypro.csv", skip = 1, na = c("","NA","-9999","-9999.0"), comment = c("["))

  #Подготовка данных
#Удаление первой строки и ненужного пустого столбца "roll"
  eddypro = eddypro[-1,]
  glimpse(eddypro)
  eddypro = select(eddypro, -(roll))

#Преобразование переменных типа char в факторы
  eddypro = eddypro %>% mutate_if(is.character, factor)

#Изменение специальных символов в названии стобцов на допустимые для переменных названия
  names(eddypro) = names(eddypro) %>% 
    str_replace_all("[!]", "_emph_") %>% 
    str_replace_all("[?]", "_quest_") %>% 
    str_replace_all("[*]", "_star_") %>% 
    str_replace_all("[+]", "_plus_") %>%
    str_replace_all("[-]", "_minus_") %>%
    str_replace_all("[@]", "_at_") %>%
    str_replace_all("[$]", "_dollar_") %>%
    str_replace_all("[#]", "_hash_") %>%
    str_replace_all("[/]", "_div_") %>%
    str_replace_all("[%]", "_perc_") %>%
    str_replace_all("[&]", "_amp_") %>%
    str_replace_all("[\\^]", "_power_") %>%
    str_replace_all("[()]", "_") 
  glimpse(eddypro)
    
#Удаление NA
  eddypro = drop_na(eddypro)  
  
#Отбор всех переменные типа numeric
  sapply(eddypro,is.numeric)
  
#Отфильтруем ночное время
  eddypro = filter(eddypro, daytime == FALSE)
  
#Получение таблицы из интересующими нас колонок
  eddy_numeric = eddypro[,sapply(eddypro,is.numeric)]
#Получение таблицы с остальными колонками
  eddy_non_numeric = eddypro[,!sapply(eddypro,is.numeric)]

#Корелляционный анализ
  cor_td = cor(eddy_numeric)
  cor_td

#Избавление от всех строк, где есть хотя бы одно значение NA, при помощи функции drop_na, и преобразование в таблицу
  cor_td = cor(drop_na(eddy_numeric)) %>% as.data.frame %>% select(h2o_flux)
#Выбор интересующего столбца и имен строк (переменных), для которых значения коэффициента детерминации было больше 0,1  
  vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude
  
#Создадим непересекабщиеся подвыборки
  row_numbers = 1:length(eddy_numeric$h2o_flux)
  teach = sample(row_numbers, floor(length(eddy_numeric$h2o_flux)*.7))
  test = row_numbers[-teach]
#Обучающая выборка
  teaching_tbl = eddy_numeric[teach,]
#Тестирующая выборка
  testing_tbl = eddy_numeric[test,]
  
  #Модель 1 по обучающей выборке
  mod1 = lm(h2o_flux ~ (.), data = testing_tbl)

#Информация о моделе
  summary(mod1)
#Коэффициенты модели
  coef(mod1)
#Остатки
  resid(mod1)
#Доверительный интервал
  confint(mod1)
#Дисперсионный анализ модели
  anova(mod1)
#Графическое представление модели
  plot(mod1)
  
  #Модель 2 
  mod2 = lm(h2o_flux ~ DOY + file_records + Tau + qc_Tau + rand_err_Tau + H + qc_H 
            + rand_err_H + LE + qc_LE + rand_err_LE + co2_flux + qc_h2o_flux
            + rand_err_co2_flux + rand_err_h2o_flux + H_strg + co2_v_minus_adv
            + h2o_v_minus_adv + co2_molar_density + co2_mole_fraction + h2o_molar_density 
            + h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag + sonic_temperature 
            + air_temperature + air_pressure + air_density + air_heat_capacity + air_molar_volume 
            + water_vapor_density + e + es + RH + VPD + Tdew + u_unrot + v_unrot + w_unrot + u_rot
            + v_rot + max_speed + wind_dir + yaw + pitch + u_star_ + TKE + L + bowen_ratio
            + T_star_ + x_peak + x_offset + x_10_perc_ + x_30_perc_ + x_50_perc_ + x_90_perc_ 
            + un_Tau + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux + un_h2o_flux
            + u_spikes + ts_spikes + mean_value + ts_var + co2_var + w_div_ts_cov + h2o_1, data = teaching_tbl)
  
#Информация о модели
  summary(mod2)
  coef(mod2)
  resid(mod2)
  confint(mod2)
  anova(mod2)
  plot(mod2) 
  
#Модель 3 
  mod3 = lm(h2o_flux ~ DOY + file_records + Tau + qc_Tau + rand_err_Tau + H + qc_H 
             + rand_err_H + LE + qc_LE + rand_err_LE + co2_flux + rand_err_co2_flux 
             + rand_err_h2o_flux + co2_v_minus_adv + + co2_molar_density + co2_mole_fraction 
             + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag 
             + sonic_temperature + air_temperature + air_pressure + air_density + air_heat_capacity
             + air_molar_volume + water_vapor_density + es + RH + VPD +  Tdew + u_unrot + v_unrot 
             + w_unrot + u_rot + v_rot + wind_dir + yaw + pitch + u_star_ + TKE + L + bowen_ratio
             + T_star_ + x_peak + x_offset + x_30_perc_ + x_90_perc_ + un_Tau + Tau_scf + un_H 
             + H_scf + un_LE + LE_scf + un_co2_flux + un_h2o_flux + w_div_ts_cov + h2o_1, data = teaching_tbl)
  
#Информация о модели
  summary(mod3)
  coef(mod3)
  resid(mod3)
  confint(mod3)
  anova(mod3)
  anova(mod3, mod2)
  plot(mod3)
  
#Корреляционный анализ переменных участвующих в линейной модели
  cor_teaching_tbl = select(teaching_tbl, DOY, file_records, Tau, qc_Tau, rand_err_Tau, H, qc_H, 
                            rand_err_H, LE, qc_LE, rand_err_LE, co2_flux, qc_h2o_flux,
                            rand_err_co2_flux, rand_err_h2o_flux, H_strg, co2_v_minus_adv,
                            h2o_v_minus_adv, co2_molar_density, co2_mole_fraction, h2o_molar_density,
                            h2o_mole_fraction, h2o_mixing_ratio, h2o_time_lag, sonic_temperature,
                            air_temperature, air_pressure, air_density, air_heat_capacity, air_molar_volume,
                            water_vapor_density, e, es, RH, VPD, Tdew, u_unrot, v_unrot, w_unrot, u_rot,
                            v_rot, max_speed, wind_dir, yaw, pitch, u_star_, TKE, L, bowen_ratio,
                            T_star_, x_peak, x_offset, x_10_perc_, x_30_perc_, x_50_perc_, x_90_perc_, 
                            un_Tau, Tau_scf, un_H, H_scf, un_LE, LE_scf, un_co2_flux, un_h2o_flux,
                            u_spikes, ts_spikes, mean_value, ts_var, co2_var, w_div_ts_cov, h2o_1)
                             
#Получение таблицы коэффициентов корреляций
  cor_td = cor(cor_teaching_tbl) %>% as.data.frame
  
  #Построение графиков по полученной моделе
#Построение точек по значениями обучающей выборки и наложение предсказанных значений по модели 3
  qplot(h2o_flux, h2o_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))
#Построение точек по значением тестирующей выборки и наложение предсказанных значений по модели 3
  qplot(h2o_flux , h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
  
#Примеры
  qplot(DOY, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
  qplot(Tau, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
  qplot(co2_flux, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))