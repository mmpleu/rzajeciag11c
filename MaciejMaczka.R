# 1. Napisz funkcję sprawdzająca czy 1 liczba jest podzielna przez druga użyj - %%
Check_two_numbers <- function()
{ 
  x <- 0
  myvector = vector()
  while(x<2) {
    question<-paste("Podaj liczbę nr ",x+1,": ",sep="")
    n <- readline(prompt=question)
    
    if (!is.na(as.numeric(n))){
      myvector = c(myvector,n)
      x <- x+1}
    else {
      print("To nie liczba!")
    }
  } 
  nums<-as.numeric(myvector)
  text1<-paste("Liczba",nums[1],"jest podzielna przez",nums[2],"bez reszty.")
  text2<-paste("Liczba",nums[1],"NIE jest podzielna przez",nums[2],"bez reszty.")
  return (ifelse(nums[1]%/%nums[2],text1,text2))

  }
print(Check_two_numbers())

# 2. Pociąg z Lublina do Warszawy przejechał połowę drogi 
# ze średnią prędkością 120 km/h.
# Drugą połowę przejechał ze średnią prędkością 90 km/h.
# Jaka była średnia prędkość pociągu.

speed1<-120
speed2<-90
avg_speed<-mean(c(speed1,speed2))
print(paste("Średnia prędkość pociągu to",avg_speed,"km/h."))

# 3. Utwórz funkcję obliczającą współczynnik korelacji r Pearsona 
# dla 2 wektorów o tej samej długości.
# Wczytaj dane plik dane.csv i oblicz współczynnik dla wagi i wzrostu.
# W komentarzu napisz co oznacza wynik.

k<-function(x,y)
{
  if (length(x)!=length(y)) {return (print("Wektory o różnej długości!"))}
  else{
    avg_x<-mean(x)
    avg_y<-mean(y)
    kor_Pear=sum((x-avg_x)%*%(y-avg_y))/(sqrt(sum((x-avg_x)^2))*sqrt(sum((y-avg_y)^2)))
    text1<-paste(" - obliczona własną funkcją wynosi:",round(kor_Pear,2))
    text2<-paste(" - obliczona funkcją cor() wynosi:",round(cor(x,y),2),"(dla sprawdzenia)")
    text<-paste("Korelacja Pearsona podanych wektorów.",text1,text2, sep="\n")
    return (cat(text))
    
  }
}
dane<-read.csv2('dane.csv')
k(dane$waga,dane$wzrost)

# 4. Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika 
# stworzDataFrame <- function(ile=1)
# W pierwszym wierszu użytkownik podaje nazwy kolumn. 
# w kolejnych wierszach zawartość wierszy ramki danych 
# (tyle wierszy ile podaliśmy w argumencie ile. 
# ile=1 oznacza, że gdy użytkownik nie poda żadnej wartości jako parametr, 
# domyślna wartością będzie 1)

stworzDataFrame <- function(ile=1)
{
  df<-data.frame(array(0,c(ile,ile)))
  y <- 0
  while(y<ile) {
    question<-paste("Podaj nazwę kolumny nr ",y+1,": ",sep="")
    col_name <- readline(prompt=question)
    y <- y+1
    x <- 0
    row_vector = vector()
    while(x<ile) {
      question<-paste("Podaj zawartość wiersza nr ",x+1,": ",sep="")
      n <- readline(prompt=question)
      x <- x+1
      df[y,x]<-n
    }
    colnames(df)[y]<-col_name
  }
  return(df)
}

stworzDataFrame()
stworzDataFrame(2)

# 5. Napisz funkcję , która pobiera sciezkeKatalogu, nazweKolumny, jakaFunkcje, 
# DlaIluPlikow i liczy: 
# mean, median, min, max w zależności od podanej nazwy funkcji w argumencie, 
# z katologu który podaliśmy  i z tylu plików ilu podaliśmy 
# dla wybranej nazwy kolumny. 
# UWAGA: w podanych plikach R pobierając komórki 
# nazwane liczbami R wstawi przed nazwy X. 
# Funkcję przetestuj dla katalogu smogKrakow.zip.  
# Wykonując obliczenia pomiń brakujące wartości.

install.packages("stringr")
library(stringr)

liczZplikow <- function(sciezka,nazwaKolumny,jakaFunkcja="mean",DlaIluPlikow=1){ 
  check_dir<-list.files(path=sciezka)
  if (length(check_dir)<DlaIluPlikow){return(print(paste('Za dużo plików!')))}
  else{
    x<-0
    data_vector<-vector()
    na_count<-data.frame(0)
    colnames(na_count)<-'FALSE'
    na_count['TRUE']<-0
    while(x<DlaIluPlikow){
      f1<-read.csv(paste(sciezka,check_dir[x+1],sep='/'),header=TRUE)
      all_columns<-str_subset(colnames(f1),nazwaKolumny)
      na_count<-na_count+table(is.na(unlist(f1[all_columns])))
      data_vector<-c(data_vector,as.numeric(na.omit(unlist(f1[all_columns]))))
      x<-x+1
    }
    if (jakaFunkcja=="mean"){
      feat<-round(mean(data_vector),2)}
    if (jakaFunkcja=="median"){
      feat<-median(data_vector)}
    if (jakaFunkcja=="min"){
      feat<-min(data_vector)}
    if (jakaFunkcja=="max"){
      feat<-max(data_vector)}
    result<-c(DlaIluPlikow,nazwaKolumny,feat,as.numeric(na_count['FALSE']),as.numeric(na_count['TRUE']))
    df<-data.frame(result,row.names=c('DlaIluPlikow','nazwaKolumny',jakaFunkcja,'ilość rekordów','odrzucono (NA)'))
    colnames(df)<-'raport'
    return(df)}
}
liczZplikow(sciezka='smogKrakow',nazwaKolumny='temperature')
liczZplikow(sciezka='smogKrakow',nazwaKolumny='humidity',jakaFunkcja="median",DlaIluPlikow=2)
liczZplikow(sciezka='smogKrakow',nazwaKolumny='pressure',jakaFunkcja="min",DlaIluPlikow=3)
liczZplikow(sciezka='smogKrakow',nazwaKolumny='pm1',jakaFunkcja="max",DlaIluPlikow=4)
liczZplikow(sciezka='smogKrakow',nazwaKolumny='pm25',DlaIluPlikow=5)
liczZplikow(sciezka='smogKrakow',nazwaKolumny='pm10',jakaFunkcja="median",DlaIluPlikow=12)
                                                             
                                                             
                                                             
                                                             