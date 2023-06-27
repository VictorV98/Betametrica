Trabajo final 7

AUTHOR

Víctor Vallejo 

[](#cb1-1)library(tidyverse)

[](#cb1-2)library(openxlsx)

[](#cb1-3)library(gt)

[](#cb1-4)library(modeltime)

[](#cb1-5)library(timetk)

[](#cb1-6)library(sknifedatar)

[](#cb1-7)library(tidymodels)

[](#cb1-8)library(feasts)

[](#cb1-9)library(forecast)

[](#cb1-10)library(tsibble)

[](#cb1-11)library(fable)

[](#cb1-12)library(fabletools)

[](#cb1-13)library(urca)

[](#cb1-14)library(plotly)
## **Estructuración de base de datos**
[](#cb2-1)CONSOLIDADO <- read.xlsx("C:\\Users\\admin\\OneDrive - Universidad Central del Ecuador\\Escritorio\\IESS\\Trabajo4\_Pronostico\\Data\\PORTAFOLIO\_CONSOLIDADO\_PROYECCION.xlsx",

[](#cb2-2)                         detectDates = T)

<a name="cb2-3"></a>[](#cb2-3)

[](#cb2-4)CONSOLIDADO1 <-  CONSOLIDADO %>% pivot\_longer(!INVERSION,

[](#cb2-5)                                              names\_to = "FECHA",

[](#cb2-6)                                              values\_to = "RENDIMIENTOS")%>% 

[](#cb2-7)                 mutate(FECHA = as.Date(FECHA))

<a name="cb2-8"></a>[](#cb2-8)

[](#cb2-9)listas <- split(CONSOLIDADO1,CONSOLIDADO1$INVERSION)

<a name="cb2-10"></a>[](#cb2-10)

[](#cb2-11)IP <- listas$PRIVATIVAS[-1]*#PRIVATIVAS*
## **Visualizando base de datos**
[](#cb3-1)tabla2 <- IP %>% 

[](#cb3-2)  gt() %>% 

[](#cb3-3)  tab\_header(title='Inversiones privativas', 

[](#cb3-4)             subtitle='Portafolio IESS') %>%

[](#cb3-5)  fmt\_number(

[](#cb3-6)    columns = vars(RENDIMIENTOS),

[](#cb3-7)    decimals = 2,

[](#cb3-8)    suffixing = TRUE,

[](#cb3-9)    use\_seps = TRUE

[](#cb3-10)  ) %>% 

[](#cb3-11)  tab\_footnote(

[](#cb3-12)    footnote = "Fuente: Histórico portafolio")%>% 

[](#cb3-13)  tab\_footnote(

[](#cb3-14)  footnote = "Elaborado por: Víctor Vallejo")

[](#cb3-15)gtsave(tabla2, file = "t2.html")

- Para la presente aplicación se cuenta con una serie histórica sobre inversiones privativas pertenecientes al IESS la cual va desde Diciembre 2013 hasta marzo 2023 con una periodicidad de tipo mensual.
## **Visualización gráfica de la serie**
[](#cb4-1)IP %>% 

[](#cb4-2)  plot\_time\_series(.date\_var = FECHA, 

[](#cb4-3)                   .value = RENDIMIENTOS, 

[](#cb4-4)                   .interactive = T, 

[](#cb4-5)                   .line\_size = 0.15,.title = "Evolución inversiones privativas",.x\_lab = "Tiempo",.y\_lab = "Valores")

- Se puede apreciar que la serie en estudio cuenta con un fuerte componente tendencial, creciente durante los meses, con pequeñas caidas a lo largo del año 2021, con esto ya da luces que las inversiones privativas no presentan un comportamiento estacional a lo largo de los años.
### **Contraste de hipótesis sobre estacionariedad**
#### **Phillips-Perron**
\(H\_0:\) Raíz unitaria (No estacionariedad)

\(H\_1:\) No raíz unitaria estacionariedad

[](#cb5-1)testpp <- ur.pp(tsdata,

[](#cb5-2)                type = c("Z-tau"),

[](#cb5-3)                model = c("trend"),

[](#cb5-4)                lags = c("short"))

[](#cb5-5)summary(testpp)

################################## 

\# Phillips-Perron Unit Root Test # 

################################## 

Test regression with intercept and trend 


Call:

lm(formula = y ~ y.l1 + trend)

Residuals:

`       `Min         1Q     Median         3Q        Max 

-161439916  -20628301    8513409   27379920   89930829 

Coefficients:

`             `Estimate Std. Error t value Pr(>|t|)    

(Intercept) 2.884e+08  7.186e+07   4.014 0.000111 \*\*\*

y.l1        9.753e-01  7.716e-03 126.401  < 2e-16 \*\*\*

trend       4.060e+05  4.256e+05   0.954 0.342146    

\---

Signif. codes:  0 '\*\*\*' 0.001 '\*\*' 0.01 '\*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 41380000 on 108 degrees of freedom

Multiple R-squared:  0.9994,    Adjusted R-squared:  0.9994 

F-statistic: 9.769e+04 on 2 and 108 DF,  p-value: < 2.2e-16


Value of test-statistic, type: Z-tau  is: -2.5551 

`           `aux. Z statistics

Z-tau-mu              2.4958

Z-tau-beta            0.9957

Critical values for Z statistics: 

`                     `1pct      5pct     10pct

critical values -4.042903 -3.450435 -3.150299

- Como el valor de tabla |-2.5551| no es mayor a ninguno de los valores criticos(-4.134754, -3.493511, -3.175277), entonces no se puede rechazar Ho por lo que se dice que hay raíz unitaria o la serie no es estacionaria.
#### **Elliot, Rothenberg and Stock Unit Root Test**
\(H\_0:\) Raíz unitaria (No estacionariedad)

\(H\_1:\) No raíz unitaria estacionariedad

[](#cb7-1)erstest <- ur.ers(tsdata,

[](#cb7-2)                  type = c("DF-GLS"),

[](#cb7-3)                  model = c("trend"),

[](#cb7-4)                  lag.max = 4)

[](#cb7-5)summary(erstest)

############################################### 

\# Elliot, Rothenberg and Stock Unit Root Test # 

############################################### 

Test of type DF-GLS 

detrending of series with intercept and trend 


Call:

lm(formula = dfgls.form, data = data.dfgls)

Residuals:

`       `Min         1Q     Median         3Q        Max 

-157929887  -18068835    3871461   19257608   92669508 

Coefficients:

`              `Estimate Std. Error t value Pr(>|t|)    

yd.lag       -0.005848   0.005932  -0.986   0.3265    

yd.diff.lag1  0.435966   0.097769   4.459 2.12e-05 \*\*\*

yd.diff.lag2  0.023942   0.104776   0.229   0.8197    

yd.diff.lag3  0.203372   0.104237   1.951   0.0538 .  

yd.diff.lag4  0.137567   0.097609   1.409   0.1618    

\---

Signif. codes:  0 '\*\*\*' 0.001 '\*\*' 0.01 '\*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 39840000 on 102 degrees of freedom

Multiple R-squared:  0.4361,    Adjusted R-squared:  0.4085 

F-statistic: 15.78 on 5 and 102 DF,  p-value: 1.74e-11


Value of test-statistic is: -0.9859 

Critical values of DF-GLS are:

`                 `1pct  5pct 10pct

critical values -3.46 -2.93 -2.64

- Como el valor de tabla |-0.9859| no es mayor ninguno de los valores críticos (-3.46,-2.93,-2.64). Entonces no se puede rechazar Ho por lo que se dice que hay raíz unitaria o la serie no es estacionaria.
## **Descomposición de la serie de tiempo**

- En el gráfico adjunto se pueden apreciar la descomposición de la serie de tiempo(tendencia, estacionalidad y ruído). Como ya se había comentado antes, la serie presenta un fuerte componente tendencial, con la descomposición se puede apreciar que pareciera no haber estacionalidad en la serie.
### **Estacionalidad de la serie**

- Analizando mas detalladamente la estacionalidad de manera gráfica, se puede observar que en todos los años el comportamiento se mantiene similar, no hay un comportamiento a parte de manera mensual ni trimestral.
## **Función de autocorrelación simple y parcial**

- Considerando el comportamiento bastante particular de la la función de autocorrelación simple no queda claro el número de rezagos para la media móvil para considerar en un eventual modelo ARIMA.
## **Modelamiento de la serie**
- Para este apartado dada las caracteristicas de la seria se hará uso de un modelo ‘Holt-winters doble’ sobre el cual se pretende modelar el componente tendencial de la serie para ser usada luego para pronóstico.

[](#cb9-1)modeloIP <- HoltWinters(tsdata,gamma = 0)*#Mejor modelo para inversiones privativas*

[](#cb9-2)modeloIP

Holt-Winters exponential smoothing with trend and additive seasonal component.

Call:

HoltWinters(x = tsdata, gamma = 0)

Smoothing parameters:

` `alpha: 1

` `beta : 0.1208015

` `gamma: 0

Coefficients:

`             `[,1]

a    1.168255e+10

b    4.189690e+07

s1  -2.012820e+07

s2  -3.182958e+07

s3  -7.185083e+07

s4  -2.433689e+07

s5  -4.136174e+04

s6   3.815950e+07

s7   5.250430e+07

s8   5.350019e+07

s9   3.107170e+07

s10  1.497194e+07

s11 -2.814405e+07

s12 -1.387672e+07
## **Pronóstico**
[](#cb11-1)forecast(modeloIP,h = 9,level = 95,alpha = 0.05)

`         `Point Forecast       Lo 95       Hi 95

Apr 2023    11704321036 11530922671 11877719402

May 2023    11734516550 11474061124 11994971977

Jun 2023    11736392198 11398475576 12074308819

Jul 2023    11825803034 11413496873 12238109195

Aug 2023    11891995461 11406052310 12377938612

Sep 2023    11972093217 11412184023 12532002412

Oct 2023    12028334920 11393554183 12663115657

Nov 2023    12071227707 11360338744 12782116669

Dec 2023    12090696114 11302263847 12879128381

[](#cb13-1)autoplot(forecast(modeloIP,h = 9,level = 95,alpha = 0.05))+

[](#cb13-2)  labs(title = "Pronóstico inversiones privativas Abril2023-Diciembre2023",

[](#cb13-3)       subtitle = "Suavización exponencial doble(Hold)",

[](#cb13-4)       x = "Tiempo",

[](#cb13-5)       y = "Valores",

[](#cb13-6)       caption = "Fuente: Histórico portafolio \n Elaboración: Víctor Vallejo")  

![](Aspose.Words.87f19943-9a34-4d7c-8258-22d83c189e4e.001.png)

- Con base en los pronósticos realizados para los meses de abril 2023 a Diciembre 2023, se puede apreciar que dichos valores comparte conservadoramente la tendencia de la serie original.
