---
title: "Projekt z analizy danych - analiza długości śledzia oceanicznego wyławianego w Europie."
author: Tomasz Sienkiewicz, Jakub Zdanowski
date: "05 December, 2019"
output: 
  html_document:
    toc: true
    toc_float: true
---

# Podsumowanie analizy
Celem analizy było określenie głównych przyczyn stopniowego zmniejszania się długości śledzi oceanicznych wyławianych w Europie. Analiza doprowadziła do wniosków, że istotny wpływ na długość miała temperatura przy powierzchni wody i to ona mogła doprowadzić do zmian długości. Przy wzroscie temperatury z roku na rok długość śledzia stopniowo malała. Duży wpływ na długość śledzia miały również wartości związane z dostępnością planktonu, co jest logiczne gdyż plankton jest pożywieniem dla śledzi. Wszystkie te wartości mogą jednak być związane z oscylacją północnoatlantycką, która jest zjawiskiem meteorologicznym i może mieć związek z temperaturą wody przy powierzchni i poprzez cyrkulację wody oceanicznej z dostępnością planktonu. Wartość ta jest przeciwnie skorelowana z długością, jednakże żaden regresor nie uznał jej za istotną. W danych brakuje informacji o roku pomiaru co utrudnia analizę, jednakże można ją wyłuskać z atrybutów rocznego narybku i rocznego natężenia połowów w rejonie. 

# Wprowadzenie
### Opis problemu
Celem projektu jest określenie głównych przyczyn stopniowego zmniejszania się długości śledzi oceanicznych wyławionych w Europie.

### Opis danych 
Zbiór danych do analizy składa się z pomiarów śledzi i warunków w jakich żyją z ostatnich 60 lat. Dane były pobierane z połowów komercyjnych jednostek. W ramach połowu jednej jednostki losowo wybierano od 50 do 100 sztuk trzyletnich śledzi.

### Opis zmiennych: 

Wszystkie dane, są danymi liczbowymi, nie występują żadne dane kategoryczne.

* __<span style="color:red">length</span>__: długość złowionego śledzia [cm];
* __cfin1__: dostępność planktonu [zagęszczenie Calanus finmarchicus gat. 1];
* __cfin2__: dostępność planktonu [zagęszczenie Calanus finmarchicus gat. 2];
* __chel1__: dostępność planktonu [zagęszczenie Calanus helgolandicus gat. 1];
* __chel2__: dostępność planktonu [zagęszczenie Calanus helgolandicus gat. 2];
* __lcop1__: dostępność planktonu [zagęszczenie widłonogów gat. 1];
* __lcop2__: dostępność planktonu [zagęszczenie widłonogów gat. 2];
* __fbar__: natężenie połowów w regionie [ułamek pozostawionego narybku];
* __recr__: roczny narybek [liczba śledzi];
* __cumf__: łączne roczne natężenie połowów w regionie [ułamek pozostawionego narybku];
* __totaln__: łączna liczba ryb złowionych w ramach połowu [liczba śledzi];
* __sst__: temperatura przy powierzchni wody [°C];
* __sal__: poziom zasolenia wody [Knudsen ppt];
* __xmonth__: miesiąc połowu [numer miesiąca];
* __nao__: oscylacja północnoatlantycka [mb].

### Inicjalizacja środowiska 


```r
library(dplyr)
library(ggplot2)
library(GGally) # ggpairplot
library(DT)
library(mice) # for missing data
library(ggpubr) # ggarange
```

```
## Error in library(ggpubr): there is no package called 'ggpubr'
```

```r
library(corrplot) # ggarange
library(plotly)
library(caret)
library(gbm)
library(doMC)

columns_names <- c("length" = "długość złowionego śledzia [cm]",
  "cfin1" = "dostępność planktonu [zagęszczenie Calanus finmarchicus gat. 1]",
  "cfin2" = "dostępność planktonu [zagęszczenie Calanus finmarchicus gat. 2]",
  "chel1" = "dostępność planktonu [zagęszczenie Calanus helgolandicus gat. 1]",
  "chel2" = "dostępność planktonu [zagęszczenie Calanus helgolandicus gat. 2]",
  "lcop1" = "dostępność planktonu [zagęszczenie widłonogów gat. 1]",
  "lcop2" = "dostępność planktonu [zagęszczenie widłonogów gat. 2]",
  "fbar" = "natężenie połowów w regionie [ułamek pozostawionego narybku]",
  "recr" = "roczny narybek [liczba śledzi]",
  "cumf" = "łączne roczne natężenie połowów w regionie [ułamek pozostawionego narybku]",
  "totaln" = "łączna liczba ryb złowionych w ramach połowu [liczba śledzi]",
  "sst" = "temperatura przy powierzchni wody [°C]",
  "sal"  = "poziom zasolenia wody [Knudsen ppt]",
  "xmonth" = "miesiąc połowu [numer miesiąca]",
  "nao" = "oscylacja północnoatlantycka [mb]" )

prettyTable <- function(table_df, round_columns=numeric(), round_digits=2) {
    DT::datatable(table_df, style="bootstrap", filter = "top", rownames = FALSE, extensions = "Buttons", options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
    formatRound(round_columns, round_digits)
}
```



# Analiza

### Wczytanie i wstepne oczyszczenie danych

Pierwszym krokiem jest wczytanie danych oraz bliższe przyjżenie się im. Ponieważ w pliku csv brakujące rekordy zapisane są jako "?" trzeba je poprawnie wczytać. Brakujące dane występują w kolumnach cfin1, cfin2, chel1, chel2, lcop1, lcop2 i lsst. 
Jako że kolumna x to po prostu numer pomiaru, nie będzie nam potrzebna.


```r
set.seed(23)
data <- read.csv("sledzie.csv", na.strings = c("?"))
lapply(data, function(x) any(is.na(x)))
```

```
## $X
## [1] FALSE
## 
## $length
## [1] FALSE
## 
## $cfin1
## [1] TRUE
## 
## $cfin2
## [1] TRUE
## 
## $chel1
## [1] TRUE
## 
## $chel2
## [1] TRUE
## 
## $lcop1
## [1] TRUE
## 
## $lcop2
## [1] TRUE
## 
## $fbar
## [1] FALSE
## 
## $recr
## [1] FALSE
## 
## $cumf
## [1] FALSE
## 
## $totaln
## [1] FALSE
## 
## $sst
## [1] TRUE
## 
## $sal
## [1] FALSE
## 
## $xmonth
## [1] FALSE
## 
## $nao
## [1] FALSE
```

```r
data <- subset(data, select = -c(X))

summary(data)
```

```
##      length         cfin1             cfin2             chel1            chel2            lcop1              lcop2             fbar       
##  Min.   :19.0   Min.   : 0.0000   Min.   : 0.0000   Min.   : 0.000   Min.   : 5.238   Min.   :  0.3074   Min.   : 7.849   Min.   :0.0680  
##  1st Qu.:24.0   1st Qu.: 0.0000   1st Qu.: 0.2778   1st Qu.: 2.469   1st Qu.:13.427   1st Qu.:  2.5479   1st Qu.:17.808   1st Qu.:0.2270  
##  Median :25.5   Median : 0.1111   Median : 0.7012   Median : 5.750   Median :21.673   Median :  7.0000   Median :24.859   Median :0.3320  
##  Mean   :25.3   Mean   : 0.4458   Mean   : 2.0248   Mean   :10.006   Mean   :21.221   Mean   : 12.8108   Mean   :28.419   Mean   :0.3304  
##  3rd Qu.:26.5   3rd Qu.: 0.3333   3rd Qu.: 1.7936   3rd Qu.:11.500   3rd Qu.:27.193   3rd Qu.: 21.2315   3rd Qu.:37.232   3rd Qu.:0.4560  
##  Max.   :32.5   Max.   :37.6667   Max.   :19.3958   Max.   :75.000   Max.   :57.706   Max.   :115.5833   Max.   :68.736   Max.   :0.8490  
##                 NA's   :1581      NA's   :1536      NA's   :1555     NA's   :1556     NA's   :1653       NA's   :1591                     
##       recr              cumf             totaln             sst             sal            xmonth            nao          
##  Min.   : 140515   Min.   :0.06833   Min.   : 144137   Min.   :12.77   Min.   :35.40   Min.   : 1.000   Min.   :-4.89000  
##  1st Qu.: 360061   1st Qu.:0.14809   1st Qu.: 306068   1st Qu.:13.60   1st Qu.:35.51   1st Qu.: 5.000   1st Qu.:-1.89000  
##  Median : 421391   Median :0.23191   Median : 539558   Median :13.86   Median :35.51   Median : 8.000   Median : 0.20000  
##  Mean   : 520366   Mean   :0.22981   Mean   : 514973   Mean   :13.87   Mean   :35.51   Mean   : 7.258   Mean   :-0.09236  
##  3rd Qu.: 724151   3rd Qu.:0.29803   3rd Qu.: 730351   3rd Qu.:14.16   3rd Qu.:35.52   3rd Qu.: 9.000   3rd Qu.: 1.63000  
##  Max.   :1565890   Max.   :0.39801   Max.   :1015595   Max.   :14.73   Max.   :35.61   Max.   :12.000   Max.   : 5.08000  
##                                                        NA's   :1584
```

```r
paste0("Ilość badanych przypadków: ", nrow(data), ", ilość zmiennych: ", ncol(data))
```

```
## [1] "Ilość badanych przypadków: 52582, ilość zmiennych: 15"
```

```r
head(data, 10)
```

```
##    length   cfin1   cfin2   chel1    chel2   lcop1    lcop2  fbar   recr      cumf   totaln      sst      sal xmonth nao
## 1    23.0 0.02778 0.27785 2.46875       NA 2.54787 26.35881 0.356 482831 0.3059879 267380.8 14.30693 35.51234      7 2.8
## 2    22.5 0.02778 0.27785 2.46875 21.43548 2.54787 26.35881 0.356 482831 0.3059879 267380.8 14.30693 35.51234      7 2.8
## 3    25.0 0.02778 0.27785 2.46875 21.43548 2.54787 26.35881 0.356 482831 0.3059879 267380.8 14.30693 35.51234      7 2.8
## 4    25.5 0.02778 0.27785 2.46875 21.43548 2.54787 26.35881 0.356 482831 0.3059879 267380.8 14.30693 35.51234      7 2.8
## 5    24.0 0.02778 0.27785 2.46875 21.43548 2.54787 26.35881 0.356 482831 0.3059879 267380.8 14.30693 35.51234      7 2.8
## 6    22.0 0.02778 0.27785 2.46875 21.43548 2.54787       NA 0.356 482831 0.3059879 267380.8 14.30693 35.51234      7 2.8
## 7    24.0 0.02778 0.27785 2.46875 21.43548 2.54787 26.35881 0.356 482831 0.3059879 267380.8 14.30693 35.51234      7 2.8
## 8    23.5 0.02778 0.27785 2.46875 21.43548 2.54787 26.35881 0.356 482831 0.3059879 267380.8 14.30693 35.51234      7 2.8
## 9    22.5 0.02778 0.27785 2.46875 21.43548 2.54787 26.35881 0.356 482831 0.3059879 267380.8 14.30693 35.51234      7 2.8
## 10   22.5 0.02778 0.27785 2.46875 21.43548 2.54787 26.35881 0.356 482831 0.3059879 267380.8 14.30693 35.51234      7 2.8
```
W celu poradzenia sobie z brakującymi danymi zostaje wykorzystana metoda MICE (Multivariate Imputation via Chained Equations). Metoda ta pozwala na pozbycie się brakujących danych z utrzymaniem podobnego rozkładu.


```r
plot_histograms <- function(data, columns=NULL, ncol=1, nrow=1, createName=function(x) x, bins=30){
  if(is.null(columns)){
    columns <- names(data)
  } 
  
  i <- 1
  plots <- list()
  for (column_name in columns){
    p <- ggplot(data, aes_string(x = column_name)) + 
      geom_histogram(color="black", fill="orange", bins=bins) +
      # geom_density() +
      ggtitle(createName(column_name)) + 
      ylab("Liczba obserwacji")
    plots[[i]] <- p
    i <- i + 1
  }
  figure <- ggarrange(plotlist = plots, ncol = ncol, nrow = nrow)
  figure
}


  

missing_data_columns <- c("cfin1", "cfin2", "chel1", "chel2", "lcop1", "lcop2", "sst")
# plot_histograms(data, columns = missing_data_columns, createName = function(x) paste0("Histogram ", x, " przed pozbyciem się brakujących danych"), nrow=2)
# mice_plot <- aggr(data, col=c('navyblue','yellow'),
#                     numbers=TRUE, sortVars=TRUE,
#                     labels=names(data), cex.axis=.7,
#                     gap=3, ylab=c("Missing data","Pattern"))
completed_data <- data %>%
  mice(m=3,  method = 'cart', seed = 63) %>%
  complete(2)
```

```
## 
##  iter imp variable
##   1   1  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   1   2  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   1   3  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   2   1  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   2   2  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   2   3  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   3   1  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   3   2  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   3   3  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   4   1  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   4   2  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   4   3  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   5   1  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   5   2  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   5   3  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
```

```r
# plot_histograms(completed_data, columns = missing_data_columns, createName = function(x) paste0("Histogram ", x, " po pozbyciu się brakujących danych"), nrow=2)
```

### Eksploracja danych 

Podsumowanie danych ich podstawowych statystyk.

```r
summary(completed_data)
```

```
##      length         cfin1             cfin2             chel1            chel2            lcop1              lcop2             fbar       
##  Min.   :19.0   Min.   : 0.0000   Min.   : 0.0000   Min.   : 0.000   Min.   : 5.238   Min.   :  0.3074   Min.   : 7.849   Min.   :0.0680  
##  1st Qu.:24.0   1st Qu.: 0.0000   1st Qu.: 0.2778   1st Qu.: 2.469   1st Qu.:13.427   1st Qu.:  2.5479   1st Qu.:17.808   1st Qu.:0.2270  
##  Median :25.5   Median : 0.1111   Median : 0.7012   Median : 5.750   Median :21.673   Median :  7.0000   Median :24.859   Median :0.3320  
##  Mean   :25.3   Mean   : 0.4460   Mean   : 2.0257   Mean   : 9.999   Mean   :21.220   Mean   : 12.8051   Mean   :28.422   Mean   :0.3304  
##  3rd Qu.:26.5   3rd Qu.: 0.3333   3rd Qu.: 1.7936   3rd Qu.:11.500   3rd Qu.:27.193   3rd Qu.: 21.2315   3rd Qu.:37.232   3rd Qu.:0.4560  
##  Max.   :32.5   Max.   :37.6667   Max.   :19.3958   Max.   :75.000   Max.   :57.706   Max.   :115.5833   Max.   :68.736   Max.   :0.8490  
##       recr              cumf             totaln             sst             sal            xmonth            nao          
##  Min.   : 140515   Min.   :0.06833   Min.   : 144137   Min.   :12.77   Min.   :35.40   Min.   : 1.000   Min.   :-4.89000  
##  1st Qu.: 360061   1st Qu.:0.14809   1st Qu.: 306068   1st Qu.:13.60   1st Qu.:35.51   1st Qu.: 5.000   1st Qu.:-1.89000  
##  Median : 421391   Median :0.23191   Median : 539558   Median :13.86   Median :35.51   Median : 8.000   Median : 0.20000  
##  Mean   : 520366   Mean   :0.22981   Mean   : 514973   Mean   :13.87   Mean   :35.51   Mean   : 7.258   Mean   :-0.09236  
##  3rd Qu.: 724151   3rd Qu.:0.29803   3rd Qu.: 730351   3rd Qu.:14.16   3rd Qu.:35.52   3rd Qu.: 9.000   3rd Qu.: 1.63000  
##  Max.   :1565890   Max.   :0.39801   Max.   :1015595   Max.   :14.73   Max.   :35.61   Max.   :12.000   Max.   : 5.08000
```

Histogramy wartości atrybutów w zbiorze danych.

Jak można zauważyć wartości długości śledzia przyjmują rozkład normalny ze średnią na poziomie 25.3 cm i przyjmują wartości w granicach 19-32.5cm.

Najwięcej połowów występuje w okresie letnim (czerwiec - wrzesień) oraz w październiku. 

Pozostałe atrybuty zostały opisane na poniższych histogramach.

```r
plot_histograms(completed_data, createName = function(x) paste0("Histogram - ", columns_names[x]), nrow=1, bins=40)
```

```
## Error in ggarrange(plotlist = plots, ncol = ncol, nrow = nrow): could not find function "ggarrange"
```

Poniżej została przedstawiona korelacja pomiędzy atrybutami.
Można zauważyć, że występuje wysoka korelacja pomiędzy parami atrybutów chel1-lcop1 oraz chel2-lcop2, ponieważ związane są one z dostępnością planktonu. Korelacja występuje także pomiędzy fbar-cumf, ponieważ obydwa atrybuty związane są z natężeniem popłowu w regionie. Można te pary zredukować, aby przeciwdziałać klątwie wielowymiarowości - jednakże tutaj zostaną one zostawione bez zmian. 


```r
corrplot(cor(completed_data), type = "upper",  tl.col = "black", tl.srt = 45, method="number")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

```r
ggplot(completed_data, aes(x=chel1, y=lcop1)) + 
  geom_point() + 
  geom_smooth(method=lm) +
  ggtitle("Zależność pomiędzy atrybutem chel1 i lcop1")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-2.png)

```r
linear_model_1 <- lm(chel1 ~ lcop1, data = completed_data)


ggplot(completed_data, aes(x=chel2, y=lcop2)) + 
  geom_point() + 
  geom_smooth(method=lm) +
  ggtitle("Zależność pomiędzy atrybutem chel2 i lcop2")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-3.png)

```r
linear_model_2 <- lm(chel2 ~ lcop2, data = completed_data)


ggplot(completed_data, aes(x=fbar, y=cumf)) + 
  geom_point() + 
  geom_smooth(method=lm) +
  ggtitle("Zależność pomiędzy atrybutem fbar i cumf")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-4.png)

```r
linear_model_3 <- lm(fbar ~ cumf, data = completed_data)
```

Poniżej przedstawiona została analiza długości śledzia na przestrzeni czasu. Jednakże czas nie był przedstawiony jawnie, ale wiadomo, że dane są ustawione chronologicznie dzięki czemu, zbiór można podzielić i na każdym z podziałów policzyć średnią. Można zauważyć, że na początku długość śledzia na przestrzeni czasu wzrastała od średniej wielkości w oklicach 24.5 cm do wielkości wielkości przekraczającej 27cm. Jednakże później trend się odwrócił i ostatecznie na sam koniec średnia ta była poniżej 24 cm. 

```r
aggregate_data <- function(data, cases_per_aggregate){
  new_aggregated_data <- aggregate(data, 
                        list(rep(1:(nrow(data)%/%cases_per_aggregate+1), each=cases_per_aggregate, len=nrow(data))),
                        mean) # Try using mean without outliners
  new_aggregated_data$time_period = c(1:nrow(new_aggregated_data))
  new_aggregated_data
}


aggregated_data <- aggregate_data(completed_data, 200)
ggplot(aggregated_data, aes(time_period, length)) +
  geom_point() + 
  geom_smooth() +
  ggtitle("Wykres zmiany długości śledzia na przestrzeni czasu") +
  ylab("Długość śledzia") +
  xlab("Przedział czasowy")
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)


```r
plot_ly(data = aggregated_data, x = ~time_period, y = ~length,
            text = ~paste("Length: ", length)) 
```

```
## No trace type specified:
##   Based on info supplied, a 'scatter' trace seems appropriate.
##   Read more about this trace type -> https://plot.ly/r/reference/#scatter
```

```
## No scatter mode specifed:
##   Setting the mode to markers
##   Read more about this attribute -> https://plot.ly/r/reference/#scatter-mode
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Warning in normalizePath(f2): path[1]="webshot585730818a3d.png": No such file or directory
```

```
## Warning in file(con, "rb"): cannot open file 'webshot585730818a3d.png': No such file or directory
```

```
## Error in file(con, "rb"): cannot open the connection
```

```r
# %>% layout(
#               scene = list(
#                  xaxis = list(title = "Przedział czasowy"),
#                  yaxis = list(title = "Długość śledzia")
#               )
#             )
```
# Regresor


```r
train_index <- createDataPartition(completed_data$length, p = .7, 
                                  list = FALSE, 
                                  times = 1)
training <- completed_data[ train_index,]
testing  <- completed_data[-train_index,]

r_squared <- function (x, y) cor(x, y) ^ 2

registerDoMC(cores=8)

ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3
  )

evaluate_model <- function(model, test_data) {
  print(model)
  plot <- ggplot(varImp(model))
  print(varImp(model))
  print(plot)
  predicted <- predict(model, newdata = test_data)
  rmse_ <- RMSE(predicted, test_data$length)
  rsquared <- r_squared(predicted, test_data$length)
  print(paste0("RMSE na zbiorze testowym: ", rmse_, " R-Squared na zbiorze testowym ", rsquared))
  c(rmse_, rsquared)
}
```

## Elastic Net



```r
modelLookup(model='glmnet')
```

```
##    model parameter                    label forReg forClass probModel
## 1 glmnet     alpha        Mixing Percentage   TRUE     TRUE      TRUE
## 2 glmnet    lambda Regularization Parameter   TRUE     TRUE      TRUE
```

```r
glmnet_grid = expand.grid(
  alpha = 0:1,
  lambda = seq(0.0001, 1, length = 100)
)

set.seed(23)

glmnet <- train(length ~ .,
             data = training,
             method = "glmnet",
             preProc = c("center", "scale"),
             metric = "RMSE",
             trControl = ctrl,
             tuneGrid = glmnet_grid,
             importance=TRUE)
```

```
## 1 package is needed for this model and is not installed. (glmnet). Would you like to try to install it now?
## 1: yes
## 2: no
## 
## Enter an item from the menu, or 0 to exit
## Enter an item from the menu, or 0 to exit
## Enter an item from the menu, or 0 to exit
## Enter an item from the menu, or 0 to exit
## Enter an item from the menu, or 0 to exit
## Enter an item from the menu, or 0 to exit
## Enter an item from the menu, or 0 to exit
## Enter an item from the menu, or 0 to exit
## Enter an item from the menu, or 0 to exit
## Enter an item from the menu, or 0 to exit
```

```
## Error: Required package is missing
```

```r
glmnet_metrics = evaluate_model(glmnet, testing)
```

```
## Error in print(model): object 'glmnet' not found
```


## Random Forest



```r
set.seed(23)
modelLookup(model='rf')
```

```
##   model parameter                         label forReg forClass probModel
## 1    rf      mtry #Randomly Selected Predictors   TRUE     TRUE      TRUE
```

```r
rf_grid <- expand.grid(mtry=c(2, 3, 4, 5, 6, 7, 8, 9, 11, 14))

random_forest <- train(length ~ .,
             data = training,
             method = "rf",
             preProc = c("center", "scale"),
             metric = "RMSE",
             trControl = ctrl,
             tuneGrid = rf_grid,
             ntree = 10,
             importance=TRUE)

rf_metrics = evaluate_model(random_forest, testing)
```

```
## Random Forest 
## 
## 36810 samples
##    14 predictor
## 
## Pre-processing: centered (14), scaled (14) 
## Resampling: Cross-Validated (5 fold, repeated 3 times) 
## Summary of sample sizes: 29449, 29448, 29448, 29447, 29448, 29447, ... 
## Resampling results across tuning parameters:
## 
##   mtry  RMSE      Rsquared   MAE      
##    2    1.167106  0.5007465  0.9235795
##    3    1.157972  0.5085100  0.9145922
##    4    1.151870  0.5136557  0.9092340
##    5    1.148646  0.5163890  0.9055689
##    6    1.147914  0.5170822  0.9039836
##    7    1.148776  0.5165275  0.9041951
##    8    1.149162  0.5162836  0.9045060
##    9    1.150128  0.5155638  0.9050218
##   11    1.152123  0.5140429  0.9065967
##   14    1.152723  0.5135804  0.9069719
## 
## RMSE was used to select the optimal model using the smallest value.
## The final value used for the model was mtry = 6.
## rf variable importance
## 
##        Overall
## xmonth 100.000
## chel1   53.328
## cfin2   23.974
## cfin1   18.897
## chel2   17.309
## lcop2   13.752
## sst     12.249
## lcop1   10.346
## totaln   9.111
## recr     7.737
## cumf     4.377
## sal      3.279
## fbar     3.104
## nao      0.000
```

![plot of chunk rf-chunk](figure/rf-chunk-1.png)

```
## [1] "RMSE na zbiorze testowym: 1.14629960746647 R-Squared na zbiorze testowym 0.520717961884495"
```



## Gradient Boosting Regressor

Podane parametry zostały znaliezone wcześniej wykorzystując grid search, jednakż trwało to bardzo długo i żeby nie ponawiać tych obliczeń parametry zostały tu wpisane na sztywno.


```r
modelLookup(model='gbm')
```

```
##   model         parameter                   label forReg forClass probModel
## 1   gbm           n.trees   # Boosting Iterations   TRUE     TRUE      TRUE
## 2   gbm interaction.depth          Max Tree Depth   TRUE     TRUE      TRUE
## 3   gbm         shrinkage               Shrinkage   TRUE     TRUE      TRUE
## 4   gbm    n.minobsinnode Min. Terminal Node Size   TRUE     TRUE      TRUE
```

```r
gbm_grid <- expand.grid(
  n.trees=c(10,20,50,100,500,1000),
  shrinkage=c(0.01,0.05,0.1,0.5),
  n.minobsinnode = c(3,5,10),
  interaction.depth=c(1,5,10)
  )

set.seed(23)

params <- data.frame( shrinkage = 0.1, n.trees = 1000, interaction.depth = 10, n.minobsinnode = 10)


gbm <- train(length ~ ., 
             data = training, 
             method = "gbm", 
             metric = "RMSE",
             tuneGrid = params,
             # trControl = ctrl,
             # tuneGrid = gbm_grid,
             preProc = c("center", "scale"),
             verbose = FALSE)

gbm_metrics = evaluate_model(gbm, testing)
```

```
## Stochastic Gradient Boosting 
## 
## 36810 samples
##    14 predictor
## 
## Pre-processing: centered (14), scaled (14) 
## Resampling: Bootstrapped (25 reps) 
## Summary of sample sizes: 36810, 36810, 36810, 36810, 36810, 36810, ... 
## Resampling results:
## 
##   RMSE      Rsquared   MAE      
##   1.148504  0.5175132  0.9034985
## 
## Tuning parameter 'n.trees' was held constant at a value of 1000
## Tuning parameter 'interaction.depth' was held constant at a value of 10
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.1
## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
## gbm variable importance
## 
##          Overall
## sst    100.00000
## recr    26.74172
## xmonth  20.10057
## lcop1   13.37639
## totaln   9.55304
## cfin2    5.58669
## chel2    4.78785
## lcop2    3.29433
## cfin1    1.97722
## nao      1.08061
## cumf     1.08037
## chel1    0.17895
## fbar     0.04366
## sal      0.00000
```

![plot of chunk gbm-chunk](figure/gbm-chunk-1.png)

```
## [1] "RMSE na zbiorze testowym: 1.13989357519391 R-Squared na zbiorze testowym 0.526063055803741"
```

```r
ggplot(varImp(gbm))
```

![plot of chunk gbm-chunk](figure/gbm-chunk-2.png)

## Podsumowanie regresorów 

Jak można zauważyć poniżej najlepszymi modelami okazały się modele oparte na algorytmach Gradient Boosting i Random Forest. Jest nieznaczna różnica pomiędzy tymi algorytmami. Nieco gorzej sprawdza się algorytm random forest. Najgorzej i poniżej oczekiwań działa algorytm Elastic Net, co może być związane z wykorzystaniem regresji liniowej wewnątrz algorytmu. 


```r
metrics <- data.frame(
   regressor = c("Elastic Net","Random Forest","Gradient Boosting Regressor"),
   rmse = c(glmnet_metrics[1], rf_metrics[1], gbm_metrics[1]), 
   rsquared = c(glmnet_metrics[2], rf_metrics[2], gbm_metrics[2])
)
```

```
## Error in data.frame(regressor = c("Elastic Net", "Random Forest", "Gradient Boosting Regressor"), : object 'glmnet_metrics' not found
```

```r
metrics
```

```
## Error in eval(expr, envir, enclos): object 'metrics' not found
```

Analizując istotność danych dla Elastic Net trzema najistotniejszymi atrybutami są fbar, cumf i sst, a najmniej istotnymi chel2, xmonth i sal. Dla algorytmu random forest najistotniejszymi  były xmonth, chel1 i cfin2, a najmniej istotne cumf, sal i mao. 
Najważniejszymi atrybutami dla algorytmu Gradient Boosting były sst, recr i xmonth, a najmniej istotnymi chel1, fbar i sal (wszystkie bardzo niskie).

Można się spodziewać, że atrybut xmonth jest skorelowany z długością (jest to najistotniejszy atrybut dla Random Forest i trzeci najbardziej istotny dla Gradient Boosting). Jednakże nie oznacza to, że jest to wynik wieloletniego trendu, a może na przykład wynikać z wahań związanych z okresami godowymi lub okresami zwiększonego połowu.  
Można zauważyć dużą istotność atrybutu sst - najistotniejszy atrybut dla Gradient Boosting, trzeci najbardziej istotny dla Elastic Net. Na wykresie poniżej można zauważyć że średnia długość śledzia jest przeciwnie skorelowana z temperaturą wody przy powierzchni. Może to świadczyć, że jest to powód zmian długości śledzi na przestrzeni lat.




```r
ggplot(aggregated_data, aes(x = time_period)) +
  geom_point(aes(y = sst * 2, colour = "sst")) +
  geom_point(aes(y = length, colour = "length")) +
  scale_y_continuous(sec.axis = sec_axis(~.*0.5, name = columns_names["lcop1"])) + 
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = columns_names["length"], x = "Przedział czasowy", colour = "Wartości") +
  theme(legend.position = c(0.25, 0.1)) + 
  ggtitle("Długość śledzia wraz z temperaturą pod powierzchnią wody na przestrzeni lat")
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png)
Mimo występowania korelacji pomiędzy długością, a oscylacją północnoatlantycką, żaden z regresorów nie uznawał tego atrybutu za istotny. 
Mniejszą zależność widać w przypadku lcop2 (i chel2). 


```r
ggplot(aggregate_data(completed_data, 1000), aes(x = time_period)) +
  geom_line(aes(y = nao + 25, colour = "nao")) +
  geom_line(aes(y = length, colour = "length")) +
  scale_y_continuous(sec.axis = sec_axis(~.-25, name = columns_names["nao"])) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = columns_names["length"], x = "Przedział czasowy", colour = "Wartości") +
  theme(legend.position = c(0.25, 0.1)) +
  ggtitle("Długość śledzia wraz z oscylacja północnoatlantycką na przestrzeni lat")
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png)

Często istotnym atrybutem jest atrybut lcop1 (jak i chel1 poprzez korelacje) czyli dostępność planktonu [zagęszczenie widłonogów gat. 1]. Można zauważyć podobny trend zmian tych wartości do trendu zmian wartości długości. Dodatkowo jest on skorelowany z oscylacją północnoatlantycką, przez co ten drugi atrybut może nie występować w istotnych atrybutach.  

```r
ggplot(aggregate_data(completed_data, 1000), aes(x = time_period)) +
  geom_line(aes(y = lcop1 / 10 + 25 , colour = "lcop1")) +
  geom_line(aes(y = length, colour = "length")) +
  scale_y_continuous(sec.axis = sec_axis(~ (.- 25) * 10, name = columns_names["lcop1"])) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = columns_names["length"], x = "Przedział czasowy", colour = "Wartości") +
  theme(legend.position = c(0.25, 0.1)) +
  ggtitle("Długość śledzia wraz z zagęszczeniem widłogonów gat. 1 na przestrzeni lat")
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png)

```r
ggplot(aggregate_data(completed_data, 1000), aes(x = time_period)) +
  geom_line(aes(y = chel1 / 10 + 25 , colour = "chel1")) +
  geom_line(aes(y = length, colour = "length")) +
  scale_y_continuous(sec.axis = sec_axis(~ (.- 25) * 10, name = columns_names["chel1"])) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = columns_names["length"], x = "Przedział czasowy", colour = "Wartości") +
  theme(legend.position = c(0.25, 0.1)) +
  ggtitle("Długość śledzia wraz z atrubutem chel1 na przestrzeni lat")
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-2.png)

```r
ggplot(aggregate_data(completed_data, 1000), aes(x = time_period)) +
  geom_line(aes(y = lcop2 / 10 + 22 , colour = "lcop2")) +
  geom_line(aes(y = length, colour = "length")) +
  scale_y_continuous(sec.axis = sec_axis(~ (.- 22) * 10, name = columns_names["lcop2"])) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = columns_names["length"], x = "Przedział czasowy", colour = "Wartości") +
  theme(legend.position = c(0.25, 0.1)) +
  ggtitle("Długość śledzia wraz z atrubutem lcop2 na przestrzeni lat")
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-3.png)

Wartości cumf i rect wydają się nie mieć wpływu na zmianę długości śledzia na przestrzeni lat. 

```r
ggplot(aggregate_data(completed_data, 1000), aes(x = time_period)) +
  geom_line(aes(y = cumf * 100 , colour = "cumf")) +
  geom_line(aes(y = length, colour = "length")) +
  scale_y_continuous(sec.axis = sec_axis(~. / 100, name = columns_names["cumf"])) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = columns_names["length"], x = "Przedział czasowy", colour = "Wartości") +
  theme(legend.position = c(0.25, 0.1)) +
  ggtitle("Długość śledzia wraz z rocznym natęzeniem połowów na przestrzeni lat")
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21-1.png)

```r
ggplot(aggregate_data(completed_data, 1000), aes(x = time_period)) +
  geom_line(aes(y = recr * 0.00005 , colour = "recr")) +
  geom_line(aes(y = length, colour = "length")) +
  scale_y_continuous(sec.axis = sec_axis(~. / 0.00005, name = columns_names["recr"])) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = columns_names["length"], x = "Przedział czasowy", colour = "Wartości") +
  theme(legend.position = c(0.25, 0.1)) +
  ggtitle("Długość śledzia wraz z rocznym narybkiem na przestrzeni lat")
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21-2.png)
