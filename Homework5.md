# N741: Homework 5
Taylor Thul  
March 15, 2017  

# Homework 5 - DUE March 15, 2017

For this homework, we'll work with the "Wong" dataset built in to the `car` package. The "Wong" data frame has 331 row and 7 columns. The observations are longitudinal data on recovery of IQ after comas of varying duration for 200 subjects. The data are from Wong, Monette, and Weiner (2001) and are for 200 patients who sustained traumatic brain injuries resulting in comas of varying duration. After awakening from their comas, patients were periodically administered a standard IQ test, but the average number of measurements per patient is small (331/200 = 1.7). *To get more info type `??Wong`.*

```r
??Wong
```

The 7 variables in the dataset are:

* `id`
    + patient ID number.
* `days`
    + number of days post coma at which IQs were measured.
* `duration`
    + duration of the coma in days.
* `sex`
    + a factor with levels Female and Male.
* `age`
    + in years at the time of injury.
* `piq`
    + performance (i.e., mathematical) IQ.
* `viq`
    + verbal IQ.

## Load dataset in from `car` package


```r
library(car)
data(Wong)

library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following object is masked from 'package:car':
## 
##     recode
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
# add an age group variable
Wong$agegrp <- case_when(
  (Wong$age > 0 & Wong$age <= 10) ~ 1,
  (Wong$age > 10 & Wong$age <= 20) ~ 2,
  (Wong$age > 20 & Wong$age <= 30) ~ 3,
  (Wong$age > 30 & Wong$age <= 40) ~ 4,
  (Wong$age > 40 & Wong$age <= 50) ~ 5,
  (Wong$age > 50 & Wong$age <= 60) ~ 6,
  (Wong$age > 60 & Wong$age <= 70) ~ 7,
  (Wong$age > 70 & Wong$age <= 100) ~ 8)

# convert to factor, add code levels and labels
Wong$agegrp <- factor(Wong$agegrp,
                  levels = c(1,2,3,4,5,6,7,8),
                  labels = c("Ages 1-10",
                             "Ages 11-20",
                             "Ages 21-30",
                             "Ages 31-40",
                             "Ages 41-50",
                             "Ages 51-60",
                             "Ages 61-70",
                             "Ages 71-100"))
```

Using this dataset, and today's demos complete the following tasks:

1. Make a table of non-parametric statistics (median and IQR) for the number of days and duration grouped by `sex`. You'll be using `summarise()` from the `dplyr` package. For a given variable `x` you'll use `median(x, na.rm=TRUE)`, `quantile(x, 0.25, na.rm=TRUE)`, and `quantile(x, 0.75, na.rm=TRUE)`. Give the table a title using the `caption=` option and update the column names with something nice using the `col.names=` option in the `knitr::kable()` command. 



```r
library(dplyr)

#lable the variables
var.labels = c(id="Patient ID Number",
               days="Number of Days Post Coma",
               duration="Number of Days in Coma",
               sex="Patient Sex",
               age="Age at Time of Injury",
               piq="Performance IQ",
               viq="Verbal IQ")

#run summary statistics by gender on duration and number of days 
t1 <- Wong %>%  
  group_by(sex) %>%
  summarise(Q1dur = quantile(duration, 0.25, na.rm=TRUE),
            meddur  = median(duration, na.rm=TRUE),
            Q3dur = quantile(duration, 0.75, na.rm=TRUE),
            Q1days = quantile(days, 0.25, na.rm=TRUE),
            meddays = median(days, na.rm=TRUE),
            Q3days = quantile(days, 0.75, na.rm=TRUE))

knitr::kable(t1, 
             col.names = c("Sex",
                          "Duration Q1",
                           "Median Duration",
                           "Duration Q3",
                           "Days Q1",
                           "Median Days",
                           "Days Q3"),
             caption = "Summary Statistics for Duration and Number of Days in Coma by Sex")
```



Table: Summary Statistics for Duration and Number of Days in Coma by Sex

Sex       Duration Q1   Median Duration   Duration Q3   Days Q1   Median Days   Days Q3
-------  ------------  ----------------  ------------  --------  ------------  --------
Female              1                 4            11     58.50           135    361.00
Male                1                 7            18     59.75           163    431.25

```

2.  Make a table of parametric statistics (mean and SD) for the performance outcomes `piq` and `viq` grouped by `sex`. Like the table above, you'll be using `summarise()` from the `dplyr` package. Now you'll use `mean(x, na.rm=TRUE)` and `sd(x, na.rm=TRUE)`. Give the table a title using the `caption=` option and update the column names with something nice using the `col.names=` option in the `knitr::kable()` command. 


```r
library(dplyr)

t2 <- Wong %>%  
  group_by(sex) %>%
  summarise (meanpiq = mean(piq, na.rm=TRUE),
            SDpiq  = sd(piq, na.rm=TRUE),
            meanviq = mean(piq, na.rm=TRUE),
            SDviq = sd(piq, na.rm=TRUE))

knitr::kable(t2, 
             digits = (2),
             col.names = c("Sex",
                          "Performance IQ Mean",
                           "Performance IQ SD",
                           "Verbal IQ Mean",
                           "Verbal IQ SD"),
             caption = "Statistics for Performance and Verbal IQ by Sex")
```



Table: Statistics for Performance and Verbal IQ by Sex

Sex       Performance IQ Mean   Performance IQ SD   Verbal IQ Mean   Verbal IQ SD
-------  --------------------  ------------------  ---------------  -------------
Female                  89.18               18.00            89.18          18.00
Male                    87.11               14.26            87.11          14.26


3. Make a table containing the frequencies and relative percentages for `agegrp`. Use the example we did in class to help guide you.


```r
# find the sample size or number of rows in dataset
ss <- length(Wong$agegrp)

# frequencies and relative percentages 
t3 <- Wong %>%
  group_by(agegrp) %>%
  summarise(freq = n(),
            pct = n()*100/ss)

knitr::kable(t3, 
             digits = (2),
             col.names = c("Age Group",
                          "Frequency",
                           "Percent"),
             caption = "Frequency and Percent by Age Group")
```



Table: Frequency and Percent by Age Group

Age Group      Frequency   Percent
------------  ----------  --------
Ages 1-10              1      0.30
Ages 11-20            53     16.01
Ages 21-30           144     43.50
Ages 31-40            48     14.50
Ages 41-50            42     12.69
Ages 51-60            27      8.16
Ages 61-70            14      4.23
Ages 71-100            2      0.60


4. Make a regression model (Model 1) for the performance IQ (`piq`) using `age` and `sex`. Put the regression model results into a table.


```r
#Model 1
m1 <- lm(piq~age+sex, data=Wong)
sm1 <- summary(m1)
knitr::kable(sm1$coefficients,
             caption="Model 1 : PerformanceIQ~Age+Sex")
```



Table: Model 1 : PerformanceIQ~Age+Sex

                 Estimate   Std. Error     t value    Pr(>|t|)
------------  -----------  -----------  ----------  ----------
(Intercept)    86.8868114    2.5796740   33.681314   0.0000000
age             0.0743977    0.0600527    1.238874   0.2162781
sexMale        -2.1651531    2.0258169   -1.068780   0.2859546

5. Make a second regression model (Model 2) for performance IQ (`piq`) using `age` and `sex` plus `days` and `duration`. Put the regression model results into a table.


```r
#Model 2
m2 <- lm(piq~age+sex+days+duration, data=Wong)
sm2 <- summary(m2)
knitr::kable(sm2$coefficients,
             caption="Model 2 : PerformanceIQ~Age+Sex+Days+Duration")
```



Table: Model 2 : PerformanceIQ~Age+Sex+Days+Duration

                 Estimate   Std. Error      t value    Pr(>|t|)
------------  -----------  -----------  -----------  ----------
(Intercept)    88.0961373    2.6462149   33.2913764   0.0000000
age             0.0542142    0.0604989    0.8961181   0.3708509
sexMale        -1.7252891    2.0152576   -0.8561135   0.3925638
days            0.0011534    0.0007457    1.5468461   0.1228705
duration       -0.1026657    0.0328189   -3.1282468   0.0019172


6. Finally, make a table showing the results from the `anova()` command comparing Model 1 and Model 2 you made above using the example we did in class as a guide. 


```r
#Compare models using the `anova()` function
anova(m1,m2)
```

```
## Analysis of Variance Table
## 
## Model 1: piq ~ age + sex
## Model 2: piq ~ age + sex + days + duration
##   Res.Df   RSS Df Sum of Sq      F  Pr(>F)   
## 1    328 74968                               
## 2    326 72586  2    2381.9 5.3489 0.00518 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
#comparisons into table
m1m2 <- anova(m1,m2)
row.names(m1m2) <- c("Model 1","Model 2")
knitr::kable(m1m2,
             caption = "Compare M1 and M2")
```



Table: Compare M1 and M2

           Res.Df        RSS   Df   Sum of Sq          F      Pr(>F)
--------  -------  ---------  ---  ----------  ---------  ----------
Model 1       328   74967.59   NA          NA         NA          NA
Model 2       326   72585.66    2    2381.933   5.348922   0.0051796


7. STUDENT CHOICE - pick either a `htmlwidget` from [http://gallery.htmlwidgets.org/](http://gallery.htmlwidgets.org/) or do a "flexdashboard" using the templates at [http://rmarkdown.rstudio.com/flexdashboard/](http://rmarkdown.rstudio.com/flexdashboard/) as a guide.


```r
#make sure all library things installed 
library(ggplot2)
library(plotly)
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

```r
library(car)

#making the plot
p1 <- plot_ly(data = Wong, x = ~piq, y = ~viq, color= ~duration,      type='scatter', mode='markers')

#make it interactive
ggplotly(p1)
```

<!--html_preserve--><div id="htmlwidget-e0d49e4ca0ada14526c6" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-e0d49e4ca0ada14526c6">{"x":{"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"title":"piq"},"yaxis":{"domain":[0,1],"title":"viq"},"showlegend":false,"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"modeBarButtonsToRemove":["sendDataToCloud"]},"data":[{"x":[87,95,95,59,67,76,74,91,115,86,76,71,127,82,88,69,102,77,82,118,87,97,104,87,93,72,84,95,89,89,87,71,95,112,89,71,74,84,85,94,97,84,85,84,108,84,86,98,67,104,79,84,87,81,90,85,99,79,70,98,75,107,86,82,96,101,105,83,77,61,75,87,97,99,78,84,80,84,95,75,86,85,81,74,67,80,76,67,80,90,87,76,87,100,80,50,65,72,79,93,105,74,76,69,94,77,106,78,75,70,82,80,65,93,85,72,74,63,93,74,86,87,69,96,67,71,89,77,117,87,85,93,86,50,112,66,94,65,104,100,106,72,65,51,84,68,96,56,82,84,98,120,133,101,80,107,97,130,117,89,110,100,102,72,87,66,63,103,79,97,94,89,114,89,105,84,106,77,72,106,93,95,96,72,84,69,81,91,76,97,89,81,70,79,71,133,87,76,87,65,92,109,92,99,114,79,74,53,108,80,93,77,87,94,85,79,93,65,88,94,99,75,94,105,99,82,77,108,121,78,78,59,101,67,98,105,76,68,92,98,93,92,121,77,112,83,90,94,104,96,95,78,92,94,98,102,122,78,82,84,82,99,88,100,85,91,97,66,69,97,85,86,111,76,79,89,99,84,101,81,114,89,71,128,85,66,80,63,91,87,88,119,94,81,106,99,78,104,126,109,65,74,71,93,78,88,76,105,75,95,90,104,100,69,104,92,105,97,67,78,88,117,76,105,68,88,101,91,88,101,71],"y":[89,77,116,73,73,69,77,110,110,83,90,89,109,85,97,88,117,89,95,101,99,90,105,86,113,79,90,108,97,86,86,88,103,106,95,82,79,95,90,81,94,86,95,83,106,94,80,112,67,96,85,91,91,98,103,93,95,93,88,116,86,130,103,72,95,112,102,88,78,104,90,97,107,103,88,85,101,95,86,79,95,95,85,80,84,99,72,74,101,100,104,106,93,95,88,101,75,90,94,97,119,74,88,67,118,85,128,93,82,68,110,105,90,73,98,75,79,99,112,92,114,77,83,96,71,80,109,91,112,82,112,104,86,74,116,105,97,74,112,103,94,75,98,86,85,79,105,80,94,93,107,120,111,117,78,123,110,118,126,103,107,99,107,91,96,73,82,97,82,97,111,102,118,78,109,103,110,103,81,119,105,93,110,81,90,85,94,92,106,97,99,90,86,86,70,111,110,90,85,89,76,117,89,98,108,78,100,69,118,94,105,80,107,90,88,91,105,77,85,89,91,81,108,94,105,101,97,119,108,82,80,81,108,117,94,126,64,74,107,114,95,86,132,86,125,83,103,116,87,107,95,80,92,93,86,104,105,80,83,92,80,94,106,92,85,96,84,75,107,85,111,113,101,98,87,78,96,82,114,90,124,78,76,111,82,116,85,64,103,93,81,131,88,82,98,96,87,109,106,110,88,81,94,104,84,104,112,114,75,108,101,108,103,80,91,76,124,118,87,79,111,129,93,111,92,105,84,88,103,95,73],"mode":"markers","type":"scatter","marker":{"colorbar":{"title":"duration","ticklen":2},"cmin":0,"cmax":255,"colorscale":[["0","rgba(68,1,84,1)"],["0","rgba(68,1,84,1)"],["0","rgba(68,1,84,1)"],["0","rgba(68,1,84,1)"],["0.00392156862745098","rgba(68,3,85,1)"],["0.00392156862745098","rgba(68,3,85,1)"],["0.00392156862745098","rgba(68,3,85,1)"],["0.00784313725490196","rgba(68,4,86,1)"],["0.0117647058823529","rgba(69,6,88,1)"],["0.0117647058823529","rgba(69,6,88,1)"],["0.0156862745098039","rgba(69,8,89,1)"],["0.0245098039215686","rgba(69,12,92,1)"],["0.0274509803921569","rgba(70,13,93,1)"],["0.0274509803921569","rgba(70,13,93,1)"],["0.0333333333333332","rgba(70,15,95,1)"],["0.0401960784313726","rgba(70,18,97,1)"],["0.0509803921568627","rgba(71,22,100,1)"],["0.0549019607843137","rgba(71,23,102,1)"],["0.0627450980392157","rgba(71,26,104,1)"],["0.0705882352941176","rgba(71,28,107,1)"],["0.0823529411764706","rgba(72,32,110,1)"],["0.109803921568627","rgba(72,40,120,1)"],["0.137254901960784","rgba(71,49,124,1)"],["0.235294117647059","rgba(61,78,138,1)"],["1","rgba(253,231,37,1)"]],"showscale":false,"color":[4,17,1,10,6,3,5,7,3,7,8,11,1,25,0,4,5,1,5,1,4,8,1,3,7,7,1,1,8,9,1,8,1,1,2,7,1,0,0,0,1,0,0,0,0,14,9,3,9,1,18,9,28,14,5,6,1,2,14,3,4,0,14,17,7,4,18,10,12,7,7,1,0,3,1,1,14,14,2,1,7,2,3,5,12,3,0,0,0,14,0,5,2,0,1,35,28,45,9,10,25,14,21,1,12,30,1,21,14,14,1,21,21,0,14,4,0,10,28,28,11,8,0,4,2,7,5,30,3,7,12,10,7,4,7,18,4,0,0,4,3,0,17,0,18,13,6,42,14,17,7,4,1,0,14,5,7,1,3,0,1,8,2,14,3,2,94,11,9,4,7,2,3,17,18,35,21,7,7,12,1,1,2,21,1,68,12,7,30,14,0,4,28,0,1,8,14,7,14,21,0,1,1,1,3,4,17,42,25,21,1,13,2,18,28,1,35,2,30,7,2,0,14,10,0,28,43,7,0,0,60,60,1,130,14,28,0,42,1,11,17,8,5,7,12,1,21,12,1,7,2,21,7,1,14,35,2,25,8,0,13,60,0,3,15,94,23,60,60,1,130,0,14,0,20,0,28,14,7,30,1,3,1,3,13,0,55,42,0,20,0,0,2,1,7,2,17,0,8,1,44,0,21,0,255,35,7,7,2,130,180,14,18,0,7,17,4,25,44,42,130,28,10,7,21,130,9,14,42,57,0],"line":{"color":"transparent"}},"xaxis":"x","yaxis":"y"},{"x":[50,133],"y":[64,132],"type":"scatter","mode":"markers","opacity":0,"hoverinfo":"none","showlegend":false,"marker":{"colorbar":{"title":"duration","ticklen":2},"cmin":0,"cmax":255,"colorscale":[["0","rgba(68,1,84,1)"],["0","rgba(68,1,84,1)"],["0","rgba(68,1,84,1)"],["0","rgba(68,1,84,1)"],["0.00392156862745098","rgba(68,3,85,1)"],["0.00392156862745098","rgba(68,3,85,1)"],["0.00392156862745098","rgba(68,3,85,1)"],["0.00784313725490196","rgba(68,4,86,1)"],["0.0117647058823529","rgba(69,6,88,1)"],["0.0117647058823529","rgba(69,6,88,1)"],["0.0156862745098039","rgba(69,8,89,1)"],["0.0245098039215686","rgba(69,12,92,1)"],["0.0274509803921569","rgba(70,13,93,1)"],["0.0274509803921569","rgba(70,13,93,1)"],["0.0333333333333332","rgba(70,15,95,1)"],["0.0401960784313726","rgba(70,18,97,1)"],["0.0509803921568627","rgba(71,22,100,1)"],["0.0549019607843137","rgba(71,23,102,1)"],["0.0627450980392157","rgba(71,26,104,1)"],["0.0705882352941176","rgba(71,28,107,1)"],["0.0823529411764706","rgba(72,32,110,1)"],["0.109803921568627","rgba(72,40,120,1)"],["0.137254901960784","rgba(71,49,124,1)"],["0.235294117647059","rgba(61,78,138,1)"],["1","rgba(253,231,37,1)"]],"showscale":true,"color":[0,255]},"xaxis":"x","yaxis":"y"}],"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script><!--/html_preserve-->

This can be found in my GitHub Respository at https://github.com/taylorthul/N741interactive.  The HTML link for interactive plot is at file:///Users/thultaylor123/Documents/N741/N741interactive/Homework5.html 


### References

Wong, P. P., Monette, G., and Weiner, N. I. (2001) Mathematical models of cognitive recovery. Brain Injury, 15, 519–530.

Fox, J. (2016) Applied Regression Analysis and Generalized Linear Models, Third Edition. Sage.
