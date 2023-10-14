#Biblioteki

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tseries)
library(tseries)
library(forecast)
library(AICcmodavg)
library(DMwR2)


#Załadowanie zbioru danych
dax = read.csv("C:/Users/kawec/OneDrive/Projekty Github/Stock price prediction/^dax_d.csv")

#Ustawienie wyłącznie dwóch potrzebnych kolumn
dax = select(dax, Data, Zamkniecie)

dax$Data = as.Date(dax$Data)

#Wizualizacja szeregu czasowego

ggplot(data=dax, aes(x=Data, y=Zamkniecie, group = 1)) +
  geom_line()


#Ucięcie liczby danych

n = 10069
p = nrow(dax)


dax = dax[row.names(dax) %in% n:p, ]


#Sprawdzenie stacjonarności szeregu czasowego. 


adf.test(dax$Zamkniecie, alternative = 'stationary')

kpss.test(dax$Zamkniecie)

acf(dax$Zamkniecie)

pacf(dax$Zamkniecie)

#Wyniki testów wskazują na niestacjonarność szeregu czasowego. 


#Utworzenie logarytmicznych stóp zwrotu

dax_log = diff(log(dax$Zamkniecie))

plot(dax_log,type='l', main='Wykres logarytmicznych stóp zwrotu')

#Wykres wskazuje na stacjonarność badanego szeregu. Niemniej jednak sprawdzone to zostanie testami.

adf.test(dax_log, alternative = "stationary")

kpss.test(dax_log)

#Oba testy wykazały, iż szereg jest niestacjonarny. 

#----Dobór ARIMA------------

acf(dax_log)

pacf(dax_log)

#Automatyczny dobór najlepszej ARIMY


ARIMA_model = auto.arima(dax_log, trace = TRUE)

plot(as.ts(dax_log))
lines(fitted(ARIMA_model), col="blue")

#Predykcja pięciu następnych wartości 
ARIMA_pred = forecast(ARIMA_model, level = c(95), h=5 ) 

print(ARIMA_pred)

predict(ARIMA_model, 10)

#Proponowany model ARIMA (3,0,3)

(fit <- arima(dax_log, c(3, 0, 3)))

#Przeprowadzenie procedury predykcji na wartościach stałych

auto.arima(dax$Zamkniecie, trace = TRUE)

ARIMA_model = arima(dax$Zamkniecie, c(0,1,0))

predict(ARIMA_model, 10)


#Weryfikacja wyników w podziale na testowy

nrow(dax)


train_dax = dax$Zamkniecie[1:5988]
test_dax = dax$Zamkniecie[5989:nrow(dax)]

arimaModel=arima(train_dax, order=c(3,0,3))

forecast=predict(arimaModel, 3)

forecast$pred[1]


#DF na przewidywane wyniki

Data <- c("2023-07-28", "2023-07-31", "2023-08-01")
Zamkniecie <- c(forecast$pred[1], forecast$pred[2], forecast$pred[3])

df_prognoza <- data.frame(Data, Zamkniecie)

df_prognoza$Data = as.Date(df_prognoza$Data)




dax_viz = dax[5980:5991,]

dax_viz$Data = as.Date(dax_viz$Data)

ggplot(data=df_prognoza, aes(x=Data, y=Zamkniecie, group = 1)) +
  geom_line()

#Zrobić porównanie wartości RMSE/MSE/itd. jaka różnica pomiedzy predyckją a prawdziwą wartoscią

mean(ARIMA_model$residuals^2)

sqrt(mean((test_dax - forecast$pred)^2))

#Jako jedną z wad modelu ARIMA uważa się jego krótkookresowe prognozowanie.

#Z tego powodu w niniejszym opracowaniu skupiłem się prognozie dla 3 kolejnych dni. Wynik średniego błedu wyniósł 70 zł. Takie wyniki w obszarze giełdowym są zrozumiałem. Przeważnie dochodzi w tej dziedzinie do bardzo dużej fluktuacji i problemów z popranymi predykcjami cen akcji. 

View(dax)

#Wykres prezentujący kilka dni przed wyniki, tendencję, trend oraz nowe prognozowane wartosci



ggplot() + 
  geom_line(data=dax_viz, aes(x = Data, y = Zamkniecie, color = 'realne')) + 
  stat_smooth(data=df_prognoza, aes(x = Data, y = Zamkniecie, color = 'prognoza'))


#Wnioski: Model krótkoterminowo zachowuje niestety błedy predykcji, ale pozwala oszacować nieznacznie cenę w najbliższym czasie.

