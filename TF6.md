# **Trabajo Final Módulo 6**
Author

Víctor Vallejo 
# **Aplicación modelo SVM**
## **SECCIÓN A**
### **Cargando librerías**
[](#cb1-1)library(foreign)

[](#cb1-2)library(dplyr)

[](#cb1-3)library(caret)

[](#cb1-4)library(ROCR)

[](#cb1-5)library(e1071)*#svm*

[](#cb1-6)library(ROSE)

[](#cb1-7)library(ggplot2)

[](#cb1-8)library(gridExtra)
### **Cargando y estructurando base de datos**
[](#cb2-1)BASE <- read.spss("C:\\Users\\admin\\OneDrive - Universidad Central del Ecuador\\Escritorio\\Curso\_Betametrica\\Ciencia\_De\_Datos\\BD\\CURSO\_CIENCIA\_DE\_DATOS\\MODULO6\\DATOS\\ENV\_2017.sav",use.value.labels = F,to.data.frame = T)

<a name="cb2-2"></a>[](#cb2-2)

[](#cb2-3)*#Se trabajara con los registros pertenecientes a manabí*

[](#cb2-4)BASE$prov\_nac <- as.numeric(BASE$prov\_nac)

[](#cb2-5)nuevadata <- BASE %>% filter(prov\_nac==13)%>%

[](#cb2-6)  select(peso,talla,sem\_gest,sexo,

[](#cb2-7)         edad\_mad,sabe\_leer,

[](#cb2-8)         con\_pren)%>% 

[](#cb2-9)  filter(

[](#cb2-10)    peso!=99,

[](#cb2-11)    talla!=99,

[](#cb2-12)    sem\_gest!=99,

[](#cb2-13)    con\_pren!=99,

[](#cb2-14)    sabe\_leer!=9)%>%

[](#cb2-15)  mutate(peso=if\_else(peso > 2500,

[](#cb2-16)                      1,0),

[](#cb2-17)         sexo=if\_else(sexo == "Hombre",0,1),

[](#cb2-18)         sabe\_leer=if\_else(sabe\_leer=="Si",1,0),

[](#cb2-19)         con\_pren=if\_else(con\_pren >= 7,1,0),

[](#cb2-20)         edad2=edad\_mad^2)

<a name="cb2-21"></a>[](#cb2-21)

[](#cb2-22)nuevadata$peso <- factor(nuevadata$peso)

[](#cb2-23)nuevadata <- nuevadata %>% 

[](#cb2-24)                  mutate(peso = recode\_factor(peso,

[](#cb2-25)                                              `0` = "no.adecuado",

[](#cb2-26)                                              `1` = "adecuado"))

[](#cb2-27)nuevadata[,c(2,3,5,8)] <- scale(nuevadata[,c(2,3,5,8)])*#COMO EN LAS PREDICTORAS HAY TANTO VARIABLES DICOTOMICAS COMO NUMERICAS ES MEJOR ESTANDARIZAR PRIMERO LAS NUMERICAS DE LO CONTRARIO LA OPCION SCALE NO FUNCIONA BIEN DENTRO DE SVM NO FUNCIONA BIEN*
### **Partición train-test**
[](#cb3-1)*## Semilla*

[](#cb3-2)set.seed(1234)

[](#cb3-3)*## Crear muestra de entrenamiento*

[](#cb3-4)entrenamiento <- createDataPartition(nuevadata$peso,

[](#cb3-5)                                     p = 0.1,list = F)
### **Modelo SVM**
[](#cb4-1)modelo.tuneado <- tune(svm,peso~.,

[](#cb4-2)                       data = nuevadata[entrenamiento,],

[](#cb4-3)                       ranges = list(cost = c(0.001,0.01,0.1,1,5,10,50)),

[](#cb4-4)                       kernel = "linear",

[](#cb4-5)                       scale = F,

[](#cb4-6)                       probability = T)
### **Mejor modelo según cross-validation**
[](#cb5-1)ggplot(modelo.tuneado$performances,

[](#cb5-2)       aes(x = cost, y = error))+

[](#cb5-3)  geom\_line()+

[](#cb5-4)  geom\_point()+

[](#cb5-5)  labs(title = "Performance del modelo",subtitle  = "Error de validación vs hiperparametro C")+

[](#cb5-6)  theme\_bw()+

[](#cb5-7)  theme(plot.title = element\_text(hjust = 0.5))

![](Aspose.Words.4f60a754-3234-4a31-82b9-0bfa0c192f37.001.png)

[](#cb6-1)mejor.modelo <- modelo.tuneado$best.model

[](#cb6-2)summary(mejor.modelo)

Call:

best.tune(METHOD = svm, train.x = peso ~ ., data = nuevadata[entrenamiento, 

`    `], ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 50)), kernel = "linear", 

`    `scale = F, probability = T)


Parameters:

`   `SVM-Type:  C-classification 

` `SVM-Kernel:  linear 

`       `cost:  0.1 

Number of Support Vectors:  458

` `( 231 227 )


Number of Classes:  2 

Levels: 

` `no.adecuado adecuado

A partir del gráfico y de los resultados como tal del modelo se puede apreciar que siguiendo un proceso de validación cruzada con costo de 0.1 se ajusta un modelo bueno en terminos de la la baja tasa de error de clasifición.
## **SECCIÓN B**
### **Evaluación integral del modelo**
#### **Matriz de clasificación**
[](#cb8-1)ajustados.mejor.modelo <- predict(mejor.modelo,nuevadata[entrenamiento,],

[](#cb8-2)                                  type = "prob",

[](#cb8-3)                                  probability = T)

[](#cb8-4)confusionMatrix(ajustados.mejor.modelo,nuevadata$peso[entrenamiento],

[](#cb8-5)                positive = levels(nuevadata$peso)[2])

Confusion Matrix and Statistics

`             `Reference

Prediction    no.adecuado adecuado

`  `no.adecuado          54       20

`  `adecuado            176     2148



`               `Accuracy : 0.9183          

`                 `95% CI : (0.9066, 0.9289)

`    `No Information Rate : 0.9041          

`    `P-Value [Acc > NIR] : 0.008958        



`                  `Kappa : 0.3237          



` `Mcnemar's Test P-Value : < 2.2e-16       



`            `Sensitivity : 0.9908          

`            `Specificity : 0.2348          

`         `Pos Pred Value : 0.9243          

`         `Neg Pred Value : 0.7297          

`             `Prevalence : 0.9041          

`         `Detection Rate : 0.8957          

`   `Detection Prevalence : 0.9691          

`      `Balanced Accuracy : 0.6128          



`       `'Positive' Class : adecuado        


#### **Curva ROC**
[](#cb10-1)pred <- prediction(attr(ajustados.mejor.modelo,

[](#cb10-2)                        "probabilities")[,2],

[](#cb10-3)                   nuevadata$peso[entrenamiento])

[](#cb10-4)perf <- performance(pred,"tpr","fpr")

[](#cb10-5)plot(perf,colorize = T,lty = 3)

[](#cb10-6)abline(0,1,col = "black")

![](Aspose.Words.4f60a754-3234-4a31-82b9-0bfa0c192f37.001.png)
#### **Sensibilidad vs espeficidad**
[](#cb11-1)plot(performance(pred,

[](#cb11-2)                 measure = "sens",

[](#cb11-3)                 x.measure = "spec",

[](#cb11-4)                 colorize =T))

![](Aspose.Words.4f60a754-3234-4a31-82b9-0bfa0c192f37.001.png)

Tanto la curva ROC como el gráfico que se genera al representar tanto la especificidad como sensibilidad tiene el comportamiento esperado de un buen modelo.
#### **Área bajo la curva**
[](#cb12-1)aucmodel1 <- performance(pred,measure = "auc")

[](#cb12-2)aucmodel1 <- aucmodel1@y.values[[1]]*#Se puede entender como una especie de R2*

[](#cb12-3)aucmodel1

[1] 0.8537171

Recordando que el AUC se puede entender como una especie de R2, ese 85% indica que el modelo tiene una buena capacidad discriminatoria. ### **Pronóstico**

[](#cb14-1)max.accuracy <- performance(pred,measure = "acc")

[](#cb14-2)indice <- which.max(slot(max.accuracy,"y.values")[[1]])

[](#cb14-3)cutoff <- slot(max.accuracy,"x.values")[[1]][indice]

[](#cb14-4)newdata1 <- data.frame(

[](#cb14-5)                       talla = 46,

[](#cb14-6)                       sem\_gest=37,

[](#cb14-7)                       sexo = 0,

[](#cb14-8)                       edad\_mad = 25,

[](#cb14-9)                       sabe\_leer = 1,

[](#cb14-10)                       con\_pren = 1,

[](#cb14-11)                       edad2 = 625)

[](#cb14-12)pronostico1 <- predict(mejor.modelo,newdata1,probability = T)

[](#cb14-13)pronostico1

`          `1 

no.adecuado 

attr(,"probabilities")

`  `adecuado no.adecuado

1    1e-07   0.9999999

Levels: no.adecuado adecuado

[](#cb16-1)newdata2 <- data.frame(

[](#cb16-2)                       talla = 45,

[](#cb16-3)                       sem\_gest=38,

[](#cb16-4)                       sexo = 1,

[](#cb16-5)                       edad\_mad = 30,

[](#cb16-6)                       sabe\_leer = 1,

[](#cb16-7)                       con\_pren = 1,

[](#cb16-8)                       edad2 = 900)

[](#cb16-9)pronostico2 <- predict(mejor.modelo,newdata2,probability = T)

[](#cb16-10)pronostico2

`          `1 

no.adecuado 

attr(,"probabilities")

`  `adecuado no.adecuado

1    1e-07   0.9999999

Levels: no.adecuado adecuado

El primer pronóstico considera un umbral que coincide con el valor donde se maximiza el accuracy. Mientras que el segundo se toma el que es por defecto. Se puede apreciar que ambos considerando las carácteristicas dadas los individuos de ejemplos se clasifican como bebés con pesos no adecuados al momentos de nacer.
## **SECCIÓN C**
### **Remuestreo: ROSS**
[](#cb18-1)train\_data <- nuevadata[entrenamiento,]

[](#cb18-2)roses <- ROSE(peso~.,

[](#cb18-3)              data =train\_data,

[](#cb18-4)               seed = 1)$data
### **Modelo SVM**
[](#cb19-1)modelo.roses <- tune(svm,peso~.,

[](#cb19-2)                     data = roses,

[](#cb19-3)                     ranges = list(cost = c(0.001,0.01,0.1,1,5,10,50)),

[](#cb19-4)                     kernel = "linear",

[](#cb19-5)                     scale = F,

[](#cb19-6)                     probability = T)*#NO VOLVER A CORRER.Demoró mucho en converger el algoritmo*

[](#cb19-7)mejor.modelo.roses <- modelo.roses$best.model

[](#cb19-8)mejor.modelo.roses

Call:

best.tune(METHOD = svm, train.x = peso ~ ., data = roses, ranges = list(cost = c(0.001, 

`    `0.01, 0.1, 1, 5, 10, 50)), kernel = "linear", scale = F, probability = T)


Parameters:

`   `SVM-Type:  C-classification 

` `SVM-Kernel:  linear 

`       `cost:  1 

Number of Support Vectors:  1454
### **Evaluación integral del modelo**
#### **Matriz de clasificación**
[](#cb21-1)ajuststosroses <- predict(mejor.modelo.roses,

[](#cb21-2)                         roses,

[](#cb21-3)                         type = "prob",

[](#cb21-4)                         probability = T)

[](#cb21-5)confusionMatrix(roses$peso,ajuststosroses,

[](#cb21-6)                dnn = c("Actuales","Predichos"),

[](#cb21-7)                levels(ajuststosroses)[1])

Confusion Matrix and Statistics

`             `Predichos

Actuales      adecuado no.adecuado

`  `adecuado         980         269

`  `no.adecuado      351         798



`               `Accuracy : 0.7415          

`                 `95% CI : (0.7234, 0.7589)

`    `No Information Rate : 0.555           

`    `P-Value [Acc > NIR] : < 2.2e-16       



`                  `Kappa : 0.4805          



` `Mcnemar's Test P-Value : 0.001142        



`            `Sensitivity : 0.7363          

`            `Specificity : 0.7479          

`         `Pos Pred Value : 0.7846          

`         `Neg Pred Value : 0.6945          

`             `Prevalence : 0.5550          

`         `Detection Rate : 0.4087          

`   `Detection Prevalence : 0.5209          

`      `Balanced Accuracy : 0.7421          



`       `'Positive' Class : adecuado        


### **Curva ROC**
[](#cb23-1)predrose <- prediction(attr(ajuststosroses,

[](#cb23-2)                            "probabilities")[,2],

[](#cb23-3)                       roses$peso)

[](#cb23-4)roc.curve(roses$peso,attr(ajuststosroses,"probabilities")[,2],col = "blue")

![](Aspose.Words.4f60a754-3234-4a31-82b9-0bfa0c192f37.001.png)

Area under the curve (AUC): 0.819
#### **Área bajo la curva**
[](#cb25-1)aucmodel2 <- performance(predrose,measure = "auc")

[](#cb25-2)aucmodel2 <- aucmodel2@y.values[[1]]*#Se puede entender como una especie de R2*

[](#cb25-3)aucmodel2

[1] 0.8187828
### **Curva ROC: Sin remuestreo vs Con remuestreo**
[](#cb27-1)roc.curve(nuevadata$peso[entrenamiento],attr(ajustados.mejor.modelo,

[](#cb27-2)                          "probabilities")[,2],

[](#cb27-3)          col = "red")

Area under the curve (AUC): 0.854

[](#cb29-1)roc.curve(roses$peso,attr(ajuststosroses,"probabilities")[,2],col = "blue",add.roc = T)

![](Aspose.Words.4f60a754-3234-4a31-82b9-0bfa0c192f37.001.png)

Area under the curve (AUC): 0.819
### **Pronóstico**
[](#cb31-1)newdata3 <- data.frame(

[](#cb31-2)                       talla = 45,

[](#cb31-3)                       sem\_gest=38,

[](#cb31-4)                       sexo = 1,

[](#cb31-5)                       edad\_mad = 30,

[](#cb31-6)                       sabe\_leer = 1,

[](#cb31-7)                       con\_pren = 1,

[](#cb31-8)                       edad2 = 900)

[](#cb31-9)pronostico3 <- predict(mejor.modelo.roses,newdata3,probability = T)

[](#cb31-10)pronostico3

`       `1 

adecuado 

attr(,"probabilities")

`   `adecuado no.adecuado

1 0.9999999       1e-07

Levels: adecuado no.adecuado
## **Conclusión general**
Tanto el modelo que considera remuestreo como el que no tienen buena cpacidad discriminatoria. A partir de la curva ROC podemos notar que el modelo sin remuestreo se pone por encima de la curva del modelo con remuestreo. No obstante, tambien hay que decir que en las matrices de clasificación se puede apreciar que hay cierto desbalance entre la sensibilidad y especificidad, esto refiriendonos especificamente al modelo sin remuestreo. Por otra, parte el modelo con remuestreo balance de mejor manera ambos indicadores manteniendo un accuracy aceptable.
