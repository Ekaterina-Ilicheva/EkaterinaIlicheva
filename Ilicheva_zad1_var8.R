  #Ильичева Eкатерина, ПАЭ-123, Вариант 8
#64 регион - Саратовская область

  #Задание 1. Рассчитать урожайность пшеницы в период с 2002 по 2012 год, 
#взяв для рассчета средние суммы активных температур за эти годы, с 18 ближайших метеостанций.

  #Указание и проверка рабочей директории
  setwd ("/Users/ekaterina/Desktop/Ilicheva mat model/zad 1")
  getwd()

  #Работа с библиотеками и установкой пакетов
  install.packages("tidyverse")
  library(tidyverse)
  
  install.packages("rnoaa")
  library(rnoaa)
  
  install.packages("dplyr")
  library("dplyr")
  
  install.packages("lubridate")
  library(lubridate)
  
  #1. Скачивание списка метеостанций
  station_data = ghcnd_stations()
  
#Запись в файл для последующей работы 
  write.csv (station_data, "stations.csv")
  station_data = read.csv("stations.csv")
  
  #2. Формирование списка метеостанций
#Таблица с именем региона и координатами его столицы
  saratov = data.frame(id = "SARATOV", latitude = 51.5667,  longitude = 46.0331); saratov
  
#Cписок, содержащий идентификаторы 18 ближайщих метеостанций отсортированных по их удалленности от Саратова
  saratov_around = meteo_nearby_stations(lat_lon_df = saratov, station_data = station_data,
                                         limit = 18, var = c("PRCP", "TAVG"),
                                         year_min = 2002, year_max = 2012); saratov_around
  
  #Работа со списками
#1). Идентификация метеостанции Саратова
  saratov_id = saratov_around[["SARATOV"]][["id"]][1]; saratov_id
#2).Таблица с 18 метеостанциями, ранжированных по расстоянию от Саратова
  saratov_table = saratov_around[[1]]; saratov_table
#Список необходимых станций
  saratov_table$id
  
  #3. Скачивание погодных данных для выбранных метеостанций
#Получение всех данных с метеостанции Саратова
  all_saratov_data = meteo_tidy_ghcnd(stationid = saratov_id)
  
  #Цикл, в котором будут скачиваться нужные данные для всех метеостанций из созданного списка 
#Промежуточный объект, куда будут скачиваться данные с конкретной метеостанции
  all_i = data.frame()
#Объект, куда скачаются все данные всех метеостанций
  all_saratov_meteodata = data.frame()    
#Цикл для всех метеостанций
  for(i in 1:18) 
  {
    all_i = meteo_tidy_ghcnd(stationid = saratov_table$id[i])
#Выбор нужных свойств
    all_i = all_i[,c("id","date","tavg")] 
#Соединение данных при помощи команды rbind, полученные на предыдущих и данном этапах цикла   
    all_saratov_meteodata = rbind(all_saratov_meteodata, all_i)
  }
  print(all_saratov_meteodata)

#Запись полученных результатов и их считывание из файла
  write.csv(all_saratov_meteodata, "all_saratov_meteodata.csv")
  all_saratov_meteodata = read.csv("all_saratov_meteodata.csv")
  
  #4. Разбивка даты на составляющие (год, месяц, года) 
#Добавление столбцов год, месяц, день в таблицу
  all_saratov_meteodata = mutate(all_saratov_meteodata, year = year(date),
                                   month = month(date), day = day(date))

#Фильтрация данных за 2002-2012 года
  years_saratov_meteodata = filter(all_saratov_meteodata, year %in% c(2002:2012))

  #5. Средняя (по годам и метеостанциям) сумма активных температур за месяц
#1). Приведение средней суммы температур в подходящую форму, при помощи делиния на 10
  years_saratov_meteodata[,"tavg"] = years_saratov_meteodata$tavg/10
#2). Превращение всех NA и tavg <5 в нули 
  years_saratov_meteodata[is.na(years_saratov_meteodata$tavg),"tavg"] = 0
  years_saratov_meteodata[years_saratov_meteodata$tavg<5, "tavg"] = 0
#3). Cуммарная температура за месяц за 10 лет для всех станций
#Группировка по метеостанциям, годам и месяцам при помощи функции group_by
  alldays = group_by(years_saratov_meteodata, id, year, month)

#Сумма температуру по этим группам с помощью sum 
  sumT_alldays_saratov = summarize(alldays, tsum = sum(tavg))
  summary(sumT_alldays_saratov)
  
#Группировка данных по месяцам  
  groups_saratov_months = group_by(sumT_alldays_saratov, month); groups_saratov_months
  
#Расчет среднего по месяцам для всех метеостанций и всех лет
  sumT_months = summarize(groups_saratov_months, St = mean(tsum)); sumT_months
  
  #6. Подготовка к расчету по формуле Урожая
#Ввод констант
  afi = c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00)
  bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
  di = c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)

#Коэффициент для экспозиции склона - считаем, что все поля идеально ровные
  y = 1.0

#Коэффициент использования ФАР посевом
  Kf = 300

#Калорийность урожая культуры
  Qj = 1600
  
#Коэффициент "сумма частей основной и побочной продукции"
  Lj = 2.2
  
#Коэффициент "стандартная влажность культуры"
  Ej = 25  
  
#Рассчет Fi по месяцам
  sumT_months = mutate(sumT_months, Fi = afi + bfi * y * St)
  
#Рассчет Yi
  sumT_months = mutate(sumT_months, Yi = ((Fi * di) * Kf) / (Qj * Lj * (100 - Ej)))

#Расчет урожая, как сумму по месяцам
  Yield = sum(sumT_months$Yi); Yield
  #урожайность пшеницы пшеницы в период с 2002 по 2012 год в Саратовской области составила 24.08674 ц/га
  
  
