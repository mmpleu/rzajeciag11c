### UWAGA! Wersja dla celów prezentacji zadania. Ograniczono ilość pobieranych danych do kliku.
### Spis treści:
### 1. Scrapping z otomoto.pl przy użyciu Rselenium
### 2. Scrapping z otomoto.pl przy użyciu xml2 i rvest


## 1. Scrapping z otomoto.pl przy użyciu Selenium
## najpierw - w folderze zawierającym sterownik przeglądarki (np. chromedriver)
## uruchomić java -jar selenium-server-standalone-3.0.1.jar -port 4445



library(RSelenium)
library(seleniumPipes)
library(dplyr)
library(stringr)
library(gtools)

remDr<-remoteDr(remoteServerAddr = "http://localhost",
                port=4445,
                browserName = "chrome",
                newSession = TRUE
)
remDr%>% go("https://www.otomoto.pl/osobowe/")

wektorLinkow<-c()
liczbaStron<-10 #przykładowo
for(i in 1:liczbaStron){
  newUrl<- paste0("https://www.otomoto.pl/osobowe/?page=",i)
  print(paste0("(",i,"/",liczbaStron,")",newUrl))
  remDr%>%go(newUrl)
  elems<-remDr%>%findElements(using = "tag name", "h2")
  
  for ( j in 1:length(elems)){
    e<-findElementsFromElement(elems[[j]],using = "tag name", "a")
    if(length(e)>0){
      link<-e[[1]]%>%getElementAttribute("href")
      wektorLinkow<-c(wektorLinkow,link)
    }
  }
}

wektorLinkowU<-wektorLinkow%>%unique()
zrobWiersz<-function(w,wektorLinkowU,remDr){
  remDr%>%go(wektorLinkowU[w])
  el<- remDr%>%findElement(using = "class name","offer-price")
  cena<-NA
  cena<-el%>%getElementAttribute("data-price")
  szczegoly<- remDr%>%findElements("class name", "offer-params__item") 
  listaSzczegolyOpis<-c()
  listaSzczegolyWartosci<-c()
  for( i in 1: length(szczegoly)){
    listaSzczegolyOpis<- c(listaSzczegolyOpis,szczegoly[[i]]%>%findElementsFromElement("class name","offer-params__label"))
    listaSzczegolyWartosci<- c(listaSzczegolyWartosci,szczegoly[[i]]%>%findElementsFromElement("class name","offer-params__value"))
  }
  nazwyKolumn<-  lapply(listaSzczegolyOpis,getElementText) %>% str_replace_all(":","") 
  wartosci<-  unlist(lapply(listaSzczegolyWartosci,getElementText))
  
  df1<- data.frame( matrix(wartosci,nrow=1,ncol=length(wartosci)))
  names(df1)<-nazwyKolumn
  df1<- cbind(cena,df1)
  df1
}

samochody<-NULL
liczbaLinkowU<-length(wektorLinkowU)
liczbaLinkowU<-as.integer(liczbaLinkowU/100) #przykładowo
for(w in 1: liczbaLinkowU){
  print(paste0(w," / ",liczbaLinkowU) )
  skip<-FALSE
  tryCatch(
    df1<-zrobWiersz(w,wektorLinkowU,remDr),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(samochody)){
    samochody<-df1
  }else{
    samochody<-smartbind(samochody,df1)
  }
}
View(samochody)



## 2. Scrapping z otomoto.pl przy użyciu xml2
library(gtools)
library(rvest)
library(xml2)

zrobWierszRvest<-function(w,wektorLinkow){
  newUrl<-wektorLinkow[w]
  page<-read_html(newUrl)
  cena<-html_node(page,".offer-price")%>%html_attr('data-price')
  lst<- page%>% xml_find_all(xpath='//*[@class="offer-params__item"]')
  v<-lapply(lst, function(node) {
    # find the first span
    first_span_node = xml_find_first(node, "./span[@class='offer-params__label']")
    label = xml_text(first_span_node, trim = TRUE)

    # find the div/a
    value_node = xml_find_first(first_span_node, "./following-sibling::div/a")
    
    # check if the div/a exists
    if(length(value_node) != 0) {
      value <- xml_text(xml_find_first(value_node, "./@title"))
    } else {
      value <- paste0(xml_text(xml_find_all(node, "./div/text()")), collapse = " ")
    }
    
    c(label=label, value=value) 
  }) 
  v<-unlist(v)
  indexy<-seq(1,length(v),1)
  nazwyKolumn<- v[indexy%%2==1]
  wartosci<- v[indexy%%2==0]
  df1<- data.frame  (matrix(wartosci,nrow = 1,ncol=length(wartosci)) )
  names(df1) <- nazwyKolumn
  df1<-cbind(cena,df1)
  }

wektorLinkow<-c()
liczbaStronRvest<-10
for(i in 1:liczbaStronRvest){
  print(i)
  newUrl<- paste0("https://www.otomoto.pl/osobowe/?page=",i)
  page<-read_html(newUrl)
  result<-page%>%html_nodes(xpath='/html/body/div[4]/div[2]/section/div[2]/div[1]/div/div[1]/div/article[*]/div[2]/div[1]/div[1]/h2/a')
  wektorLinkow<-c(wektorLinkow,xml_attr(result,"href"))
}
wektorLinkowU<-wektorLinkow%>%unique()

samochodyRvest<-NULL
liczbaLinkowU<-length(wektorLinkow)
liczbaLinkowU<-as.integer(liczbaLinkowU/100) #przykładowo
for(w in 1: liczbaLinkowU ){
  print(paste0(w," / ",liczbaLinkowU ) )
  skip<-FALSE
  tryCatch(
    df1<-zrobWierszRvest(w,wektorLinkowU),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(samochodyRvest)){
    samochodyRvest<-df1
  }else{
    samochodyRvest<-smartbind(samochodyRvest,df1)
  }
}
View(samochodyRvest)
