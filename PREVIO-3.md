ANALISIS FACTORIAL
================
mileidy yojana rueda niño
1950104

# IMPORTAR LA BASE DE DATOS EN FORMA EXCEL

``` r
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 4.0.5

``` r
datos<- read_excel("C:/Users/Usuario/Documents/tabla 1.xlsx")
```

# TIPIFICACION O ESTANDARIZACION DE LAS VARIABLES

la tipificacion permite que todas las varfiables metricas gocen de una
misma unidad de medida estdistica.

``` r
datost<- datos #crear una nueva base de datos o data frame
datost<- scale(datost, center= T , scale= T)
datost<- as.data.frame(datost)
```

# NORMALIDAD MULTIVARIABLE

HO: normalidad multivariante

H1: no normalidad multivariante

confianza= 95%

Alfa= 5% = 0,05

P value &gt; alfa: no se rechaza a la HO (Normalidad)

P Value &lt; alfa: se rechaza a la HO (No normalidad)

``` r
library(MVN)
```

    ## Warning: package 'MVN' was built under R version 4.0.5

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## sROC 0.1-2 loaded

``` r
mvn(datost[2:7])
```

    ## $multivariateNormality
    ##              Test         Statistic              p value Result
    ## 1 Mardia Skewness  101.573164615458 0.000187831150911297     NO
    ## 2 Mardia Kurtosis 0.950835885806762    0.341687692539738    YES
    ## 3             MVN              <NA>                 <NA>     NO
    ## 
    ## $univariateNormality
    ##           Test       Variable Statistic   p value Normality
    ## 1 Shapiro-Wilk fisicoquimica     0.9487    0.1563    YES   
    ## 2 Shapiro-Wilk  matematicas      0.9422    0.1044    YES   
    ## 3 Shapiro-Wilk    calculo1       0.9293    0.0470    NO    
    ## 4 Shapiro-Wilk horas cursadas    0.8774    0.0025    NO    
    ## 5 Shapiro-Wilk    quimica        0.8970    0.0071    NO    
    ## 6 Shapiro-Wilk     logros        0.8604    0.0010    NO    
    ## 
    ## $Descriptives
    ##                 n          Mean Std.Dev      Median       Min      Max
    ## fisicoquimica  30 -5.514975e-18       1  0.03234464 -1.746611 1.487854
    ## matematicas    30  1.633098e-16       1  0.11864698 -1.822849 1.412978
    ## calculo1       30 -1.536061e-16       1  0.24983096 -2.134919 1.271867
    ## horas cursadas 30 -5.107170e-16       1  0.14782859 -1.515243 1.533722
    ## quimica        30 -3.460106e-16       1 -0.05083965 -1.576029 1.474350
    ## logros         30 -2.682750e-16       1 -0.07348723 -1.175796 2.131130
    ##                      25th      75th        Skew   Kurtosis
    ## fisicoquimica  -0.7762715 0.8409608 -0.16079909 -1.1513372
    ## matematicas    -0.7712054 0.7658123 -0.28331010 -1.0832097
    ## calculo1       -0.6870352 0.8460185 -0.56998238 -0.7232697
    ## horas cursadas -1.2380644 0.8407751 -0.09849128 -1.4855842
    ## quimica        -0.8134344 0.7117551  0.14809201 -1.2639199
    ## logros         -1.1757956 1.0288212  0.41154215 -0.7900379

# MATRIZ DE CORRELACIONES

HO: Correlacion entre las variables=0 (no hay correacion) H1:
Correlacion diferente de 0 (si hay correlacion)

cuando no se rechaza la HO, no se aplica AFE. Se rechace HO, si para
aplicar AFE.

``` r
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.0.5

``` r
corr.test(datost[,2:7])
```

    ## Call:corr.test(x = datost[, 2:7])
    ## Correlation matrix 
    ##                fisicoquimica matematicas calculo1 horas cursadas quimica logros
    ## fisicoquimica           1.00       -0.94    -0.43          -0.05   -0.16   0.13
    ## matematicas            -0.94        1.00     0.50           0.14    0.12  -0.05
    ## calculo1               -0.43        0.50     1.00           0.04    0.06  -0.05
    ## horas cursadas         -0.05        0.14     0.04           1.00    0.08   0.09
    ## quimica                -0.16        0.12     0.06           0.08    1.00  -0.38
    ## logros                  0.13       -0.05    -0.05           0.09   -0.38   1.00
    ## Sample Size 
    ## [1] 30
    ## Probability values (Entries above the diagonal are adjusted for multiple tests.) 
    ##                fisicoquimica matematicas calculo1 horas cursadas quimica logros
    ## fisicoquimica           0.00        0.00     0.23           1.00    1.00   1.00
    ## matematicas             0.00        0.00     0.07           1.00    1.00   1.00
    ## calculo1                0.02        0.01     0.00           1.00    1.00   1.00
    ## horas cursadas          0.81        0.46     0.81           0.00    1.00   1.00
    ## quimica                 0.40        0.54     0.76           0.69    0.00   0.46
    ## logros                  0.51        0.78     0.81           0.64    0.04   0.00
    ## 
    ##  To see confidence intervals of the correlations, print with the short=FALSE option

``` r
correlaciones<- corr.test(datost[,2:7]) #se crea la matriz de correlaciones
correlaciones$r #matriz de correlaciones
```

    ##                fisicoquimica matematicas    calculo1 horas cursadas     quimica
    ## fisicoquimica     1.00000000 -0.94051136 -0.43012596    -0.04668109 -0.15990280
    ## matematicas      -0.94051136  1.00000000  0.49771681     0.14113318  0.11685762
    ## calculo1         -0.43012596  0.49771681  1.00000000     0.04482655  0.05793235
    ## horas cursadas   -0.04668109  0.14113318  0.04482655     1.00000000  0.07701823
    ## quimica          -0.15990280  0.11685762  0.05793235     0.07701823  1.00000000
    ## logros            0.12540293 -0.05247822 -0.04575458     0.09025612 -0.38069214
    ##                     logros
    ## fisicoquimica   0.12540293
    ## matematicas    -0.05247822
    ## calculo1       -0.04575458
    ## horas cursadas  0.09025612
    ## quimica        -0.38069214
    ## logros          1.00000000

``` r
r <-as.matrix(correlaciones$r)
```

P value &gt; alfa: no se rechaza HO estamos en esta situacion ,por lo
tanto no se aplica Analisis Factorial Exploratorio P value &lt; alfa: se
rechaza HO

# INDICADORES DE APLICABILIDAD DEL AFE(BONDAD DEL AJUSTE)

## CONTRASTE DE ESFERICIDAD DE BARTLETT

HO: las correlaciones teoricas entre cada par de variables es nulo

H1: Las correlaciones teoricas entre cada par de variables no es nula

P value &gt; alfa: no s e aplica el AFE ( no se rechaza HO)

P value &lt; alfa: si se aplica el AFE (se rechaza HO)

``` r
dim(datost) #tamaño de la muestra=30 personas 
```

    ## [1] 30  7

``` r
cortest.bartlett(r, n=30)
```

    ## $chisq
    ## [1] 73.2822
    ## 
    ## $p.value
    ## [1] 1.154963e-09
    ## 
    ## $df
    ## [1] 15

## MEDIDA DE ADECUACION MUESTRAL DE KAISER, MEYER Y OKLIN( KMO)}

Estudia variable por variables,si son o no aceptadas en el modelo para
hacer AFE. (que variables elimino o mantengo)

Se mantiene una variable en el modelo,si el KNO es igual o mayor a 0,7.

se elimina una variable del modelo,si el KNO es menor a 0,7.

``` r
KMO(r)
```

    ## Kaiser-Meyer-Olkin factor adequacy
    ## Call: KMO(r = r)
    ## Overall MSA =  0.53
    ## MSA for each item = 
    ##  fisicoquimica    matematicas       calculo1 horas cursadas        quimica 
    ##           0.53           0.52           0.78           0.18           0.54 
    ##         logros 
    ##           0.48

Fisicoquimica= 0.53 modelo es miserable matematicas =0.52modelo
miserable calculo1 =0.78 regular horas cursadas =0,18 inaceptable
quimica=0.54 miserable logros=0.48 inaceptable

# DETERMINACION DEL NUMERO DE FACTORES A EXTRAER

## metodos de las componentes principales iteradas (o Ejes principales)

Este metodo de las ejes principales es de naturaliza no parametrica,es
decir, que se ocupa,cuando no hay normalidad multivariante;pero,tambien
es valido para modelos parametricos (normalidad multivariante)

``` r
fa.parallel(r, fm="pa", n.obs=30, ylabel = "Eigenvalues")
```

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

![](PREVIO-3_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  1

## Metodo de las componentes principales

metodos parametrico,sirve solo para modelos con normalidad
multivariante.

``` r
fa.parallel(r, fm="pc", n.obs=30, ylabel = "Eigenvalues")
```

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

![](PREVIO-3_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  1

con el metodo de los componentes principales son 1 factor

``` r
fa.parallel(r, fm="ml", n.obs=30, ylabel = "Eigenvalues")
```

![](PREVIO-3_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  1

con el metodo de los componentes de verisimilitud y sus principales
factor es 1

## Metodo paralelo con interaciones

metodo parametrico,sirve solo para modelos con normalidad multivariante.

``` r
library(paran)
```

    ## Warning: package 'paran' was built under R version 4.0.5

    ## Loading required package: MASS

``` r
paran(r, iterations= 100,graph= F)
```

    ## 
    ## Using eigendecomposition of correlation matrix.
    ## Computing: 10%  20%  30%  40%  50%  60%  70%  80%  90%  100%
    ## 
    ## 
    ## Results of Horn's Parallel Analysis for component retention
    ## 100 iterations, using the mean estimate
    ## 
    ## -------------------------------------------------- 
    ## Component   Adjusted    Unadjusted    Estimated 
    ##             Eigenvalue  Eigenvalue    Bias 
    ## -------------------------------------------------- 
    ## 1           1.319005    3.027785      1.708780
    ## -------------------------------------------------- 
    ## 
    ## Adjusted eigenvalues > 1 indicate dimensions to retain.
    ## (1 components retained)

con el metodo se recomienda extraer 1 factor.

# METODOS DE EXTRACCION DE FACTORES

## METODO DE ANALISIS DE LOS COMPONENTES PRINCIPALES(ACP)

``` r
acp<- principal(r, nfactors=1, rotate= "none")
acp
```

    ## Principal Components Analysis
    ## Call: principal(r = r, nfactors = 1, rotate = "none")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                  PC1    h2   u2 com
    ## fisicoquimica  -0.93 0.862 0.14   1
    ## matematicas     0.94 0.888 0.11   1
    ## calculo1        0.67 0.446 0.55   1
    ## horas cursadas  0.15 0.024 0.98   1
    ## quimica         0.29 0.084 0.92   1
    ## logros         -0.22 0.047 0.95   1
    ## 
    ##                 PC1
    ## SS loadings    2.35
    ## Proportion Var 0.39
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 component is sufficient.
    ## 
    ## The root mean square of the residuals (RMSR) is  0.14 
    ## 
    ## Fit based upon off diagonal values = 0.82

PC1: cargas factoriales de cada variable. h2: Comunalidad(varianza comun
explicado). FISICOQUIMICA es explicado en un 86% por el factor extraido.
matematicas un 88% por el factor extraido,calculo1por un 0,4%,hora
cursadas 0,02% quimica pir un 0.2% y logros por un 0.4%.

mientras mas alta sea h2 es mejor el modelo. 0;1

u2: Especificacidad (varianza residuall o varianza no explicada)
fisicoquimica no es explicada en un 86% matematicas en 88% calculo 1 por
0.4% , horas cursadas en 0.02% y logros un 0.4%.

Mientras mas pequeños sea u2 es mejor el modelo. 0;1

h2+u2=1 Comunidad+Especificada=1 varianza explicada+varianza no
explicada=1

SS loadings= 2.35 (es la varianza explicada en valores absolutos o la
suma de los h2 )

proportion var=0.39 = 39% ( El % que la varianza explicada representa
del total)

Lo ideal es que proportion var sea lo mas cercano a 1.

RMSR=0.14 (Raiz cuadrada media de los residuos) teoricamente un modelo
presenta una solcuion adecuada cuando el RSMR es menor o igual a 0.14.

## METODOS DE LOS EJES PRINCIPALES O COMPONENTES PRINCIPALES ITERADAS (CPI)

``` r
cpi<-fa(r, nfactors = 1, fm= "pa", rotate =  "none", n.obs= 30)
```

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

``` r
cpi
```

    ## Factor Analysis using method =  pa
    ## Call: fa(r = r, nfactors = 1, n.obs = 30, rotate = "none", fm = "pa")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                  PA1     h2    u2 com
    ## fisicoquimica  -0.94 0.8759 0.124   1
    ## matematicas     1.00 0.9980 0.002   1
    ## calculo1        0.48 0.2283 0.772   1
    ## horas cursadas  0.10 0.0095 0.991   1
    ## quimica         0.16 0.0264 0.974   1
    ## logros         -0.11 0.0131 0.987   1
    ## 
    ##                 PA1
    ## SS loadings    2.15
    ## Proportion Var 0.36
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  2.8 with Chi Square of  73.28
    ## The degrees of freedom for the model are 9  and the objective function was  0.33 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.1 
    ## The df corrected root mean square of the residuals is  0.13 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  9.39  with prob <  0.4 
    ## The total number of observations was  30  with Likelihood Chi Square =  8.51  with prob <  0.48 
    ## 
    ## Tucker Lewis Index of factoring reliability =  1.015
    ## RMSEA index =  0  and the 90 % confidence intervals are  0 0.201
    ## BIC =  -22.1
    ## Fit based upon off diagonal values = 0.9

Proportion Var=36% RSMR= 0,1

## METODO DE MAXIMA VEROSIMILITUD

``` r
mve<- fa(r,nfactors=1, fm= "ml", rotate= "none", n.obs= 30)
mve
```

    ## Factor Analysis using method =  ml
    ## Call: fa(r = r, nfactors = 1, n.obs = 30, rotate = "none", fm = "ml")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                  ML1     h2    u2 com
    ## fisicoquimica  -0.94 0.8887 0.111   1
    ## matematicas     1.00 0.9950 0.005   1
    ## calculo1        0.50 0.2473 0.753   1
    ## horas cursadas  0.14 0.0190 0.981   1
    ## quimica         0.12 0.0142 0.986   1
    ## logros         -0.06 0.0031 0.997   1
    ## 
    ##                 ML1
    ## SS loadings    2.17
    ## Proportion Var 0.36
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  2.8 with Chi Square of  73.28
    ## The degrees of freedom for the model are 9  and the objective function was  0.32 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.11 
    ## The df corrected root mean square of the residuals is  0.14 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  10.2  with prob <  0.33 
    ## The total number of observations was  30  with Likelihood Chi Square =  8.23  with prob <  0.51 
    ## 
    ## Tucker Lewis Index of factoring reliability =  1.023
    ## RMSEA index =  0  and the 90 % confidence intervals are  0 0.196
    ## BIC =  -22.39
    ## Fit based upon off diagonal values = 0.89
    ## Measures of factor score adequacy             
    ##                                                    ML1
    ## Correlation of (regression) scores with factors   1.00
    ## Multiple R square of scores with factors          1.00
    ## Minimum correlation of possible factor scores     0.99

roportion Var= 23% (RMSR)= 0.11

### RESUMEN

ACP= var=39% RSMR= 2.35 CPI= VAR=36% RSMR=2,15 MVE= VAR=36% RSMR=2.17

¿CON CUAL NOS QUEDAMOS? aquel modelo que tenga la proportion var mas
alta y el RMSR mas pequeña

## PRESENTACION GRAFICA DE LOS FACTORES EXTRAIDOS

## metodo de analisis de los componentes principales (ACP)

\#\#Solo se grafica cuando hay 2 factores a extraer,con 1 factor no hay
grafica

## METODO DE ANALISIS DE LAS COMPONENTES PRINCIPALES ITERADAS (ACP)

``` r
acp1<- principal(datost[,2:7], nfactors = 1, rotate = "none", scores = T)
acp1$scores
```

    ##              PC1
    ##  [1,]  1.3422890
    ##  [2,]  0.9521966
    ##  [3,]  0.8780044
    ##  [4,]  0.5823874
    ##  [5,]  0.7571427
    ##  [6,] -0.2682843
    ##  [7,] -0.8106511
    ##  [8,] -0.8826457
    ##  [9,] -1.4763205
    ## [10,] -1.9705232
    ## [11,]  1.0904943
    ## [12,]  1.1507852
    ## [13,]  1.1269519
    ## [14,]  0.5320284
    ## [15,]  0.1727657
    ## [16,] -0.0373689
    ## [17,] -0.3839944
    ## [18,] -0.9235140
    ## [19,] -1.4734734
    ## [20,] -2.1558249
    ## [21,]  1.1493948
    ## [22,]  1.0538635
    ## [23,]  0.5017066
    ## [24,]  0.3332266
    ## [25,]  0.7115290
    ## [26,]  0.1050166
    ## [27,]  0.2089261
    ## [28,] -0.6178079
    ## [29,] -0.7119891
    ## [30,] -0.9363115

``` r
puntacionesfactoriales_acp<- acp1$scores
puntacionesfactoriales_acp<- as.data.frame(puntacionesfactoriales_acp)
```

## METODO DE LAS COMPONENTES PRINCIPALES ITERADAS (CPI)

``` r
cpi1<- fa(datost[,2:7], nfactors = 1, fm= "pa", rotate = "none", n.obs = 30,scores = "regression")
```

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

``` r
cpi1$scores
```

    ##               PA1
    ##  [1,]  1.43860230
    ##  [2,]  1.08344224
    ##  [3,]  0.73921408
    ##  [4,]  0.34087936
    ##  [5,]  0.07912527
    ##  [6,] -0.09625819
    ##  [7,] -0.46532553
    ##  [8,] -0.69781224
    ##  [9,] -1.23420633
    ## [10,] -1.91216405
    ## [11,]  1.25368244
    ## [12,]  1.27322361
    ## [13,]  0.82168816
    ## [14,]  0.44818665
    ## [15,] -0.08043568
    ## [16,] -0.18163656
    ## [17,] -0.46336832
    ## [18,] -0.72998987
    ## [19,] -1.13176054
    ## [20,] -1.86435052
    ## [21,]  1.45371251
    ## [22,]  1.14346170
    ## [23,]  0.62994957
    ## [24,]  0.44128366
    ## [25,]  0.11669010
    ## [26,] -0.23140433
    ## [27,]  1.13947877
    ## [28,] -0.70317033
    ## [29,] -0.80927789
    ## [30,] -1.80146003

``` r
puntfact_cpi<- cpi1$scores
puntfact_cpi<- as.data.frame(puntfact_cpi)
```

## METODOS DE LA MAXIMA VEROSIMILITUD

``` r
mvel<- fa(datost[,2:7], nfactors = 1, fm= "ml", rotate = "none", n.obs = 30,scores = "regression")
mvel$scores
```

    ##              ML1
    ##  [1,]  1.4240529
    ##  [2,]  1.0886994
    ##  [3,]  0.7673851
    ##  [4,]  0.4449168
    ##  [5,]  0.1246635
    ##  [6,] -0.2058882
    ##  [7,] -0.5308383
    ##  [8,] -0.8535734
    ##  [9,] -1.1780072
    ## [10,] -1.8123930
    ## [11,]  1.4234837
    ## [12,]  1.0889474
    ## [13,]  0.7684461
    ## [14,]  0.4441038
    ## [15,]  0.1219523
    ## [16,] -0.2040430
    ## [17,] -0.5282533
    ## [18,] -0.8536952
    ## [19,] -1.1784991
    ## [20,] -1.8137708
    ## [21,]  1.4228212
    ## [22,]  1.0889962
    ## [23,]  0.7656131
    ## [24,]  0.4428846
    ## [25,]  0.1241961
    ## [26,] -0.2024952
    ## [27,]  1.0231285
    ## [28,] -0.5388616
    ## [29,] -0.8615738
    ## [30,] -1.8023987

``` r
puntfact_mve<- mvel$scores
puntfact_mve<- as.data.frame(puntfact_mve)
```

# OBTENCION DE LOS FACTORES EXTRAIDOS

trabaja con el metodo que el investigador decidio (ACP,CPI,MVE)

``` r
factor.scores(r, mvel,method = "trurstone")
```

    ## $scores
    ## NULL
    ## 
    ## $weights
    ##                          ML1
    ## fisicoquimica  -0.0406657212
    ## matematicas     0.9575006410
    ## calculo1        0.0031710238
    ## horas cursadas  0.0006745279
    ## quimica         0.0005809143
    ## logros         -0.0002692393
    ## 
    ## $r.scores
    ##     ML1
    ## ML1   1
    ## 
    ## $R2
    ## [1] 0.9952005

## AGREGAR FACTOR EXTRAIDO ( PUNTACIONES FACTORIALES) EN EL DATA FRAME ORIGINAL

``` r
datos_puntaciones<- c(datos, puntacionesfactoriales_acp)
datos_puntaciones<- as.data.frame(datos_puntaciones)
```

# GUARDAR EL DATA FRAME “DATOS\_PUNTACIONES”

``` r
setwd("C:/Users/Usuario/Documents") #efine donde guardar tu archivo excel csv
write.table(datos_puntaciones, file= "encuest.cvs", sep = ";", row.names = F, dec =";")
```

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Including Code

You can include R code in the document as follows:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](PREVIO-3_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
