# Project 2: linear regression

max 6 pages, 12pt font incl relevant figures, results and interpretations
NO PCR or ridge regression!

## 1. Data inspection

### Univariate properties

univariate inspection shows imbalances in certain categories (holiday, weathersituation) up to x100
prop vars: windspeed & humidity already close to normality, others not
resp vars: both far from normality, 2 peaks suggest underlying multivariate distr (logically)

- temperature: looks like clean normal distribution, but density shows dimple (bivariate?)
- idem feelingtemp: nice boxplot, but low pvalue (shapiro) and rough density --> try boxcox?(())
- humidity: a bit rght skewed, might be fixable
- windspeed: a bit left skewed, might be fixable
- casual: chaotic left skew, multiple peaks (obv multivariate sources)
- registered: 2 heavy peaks, but relatively well centered. probably lighter model needed

### Relations between variables

Pairs(trn) shows seasonal variations, milder variations for other clusters (stdev varies more than mean)
both temp variables are of course highly correlated, at first sight no strong outliers
other normalized variables are non-normal clusters
further inspection needed to link to class variables (color plots?)

correlation matrix: temp vars strongly correlated with each other and response vars

multicollinearity (VIF & EVs) : strong MC due to temperature variables --> remove one of them
max VIF = 160, mean VIF = 41 --> strong MC
remove temperature var --> max = 1.8, mean = 1.3 : much better, MC mainly caused by temp vars

BUT ltsreg remains a problem, so variable selection required



## 2. Reference models

Looking at the linear regression models for full dataset with all, unaltered vars gives reasonable results, but we should be able to do better (compare on TST set?)

include this chapter as closing paragraph in report? 



## 3. Transformations

#### Proportion variables (power transforms don't work ~~well~~, logit & probit commonly used, see tb p94)

``````resulting pvals - table overview
      TRANSFORMATION     original        logit       probit    asin_sqrt
1        temperature 1.359506e-07 2.257233e-04 5.577747e-05 5.905972e-06
2 feelingtemperature 5.460503e-06 9.058396e-04 4.466652e-04 1.047975e-04
3           humidity 9.418048e-03 9.768850e-12 1.361361e-08 1.552829e-04
4          windspeed 9.796789e-04 2.383762e-04 6.697307e-02 9.496965e-01
``````

- temp : logit
- feelingtemp : logit
- humidity: minima at zero --> applied small manual translation of 0.01, but no transformation so return shift!!
- windspeed: asin(sqrt())

#### Categorical variables

- no transformations possible

#### Response variables: requires a reference regression model to check residual improvements

- lm(all) --> residuals increase variance for larger pred values (hetersked) 
- applied Box Cox transform --> only impactful for casual
- apply standardisation to both for consistent comparisons
- pvals and skedasticity (normality check) only improve for casual

``````
> shapiro.test(stdres(lm_cas))$p.value
[1] 2.18824e-09
> shapiro.test(stdres(lm_cas2))$p.value
[1] 0.4465285
> shapiro.test(stdres(lm_reg))$p.value
[1] 2.967909e-06
> shapiro.test(stdres(lm_reg2))$p.value
[1] 1.950181e-07
> shapiro.test(stdres(lm_reg3))$p.value
[1] 9.575064e-07
``````

Note: standardisation constants stored for application on TST set later on!!



## 4. Outlier detection

> Robust LTS typ. outperforms other options, but can lead to larger variances. 
> ltsReg uses the reweighted LTS regression by default to minimze these losses
>
> ref. from site:  `lmrob.S()` provides a fast S estimator with similar breakdown point as `ltsReg()` but better efficiency. For data analysis, rather use `lmrob` which is based on `lmrob.S`. 

ltsReg indicates presence of leverage points (many good, few bad) and some vertical outliers
--> give diagnostic plots

Outliers were removed quite strictly, but still only small portion (12 from casual, 4 from registered)
Note that low number of outliers indicates good predictability, so final results probably due to more global mismatch between TRN and TST



## 5. Variable selection

The R2 and AIC plots ifo numvar suggest that temperature contains most info, a set of 5 variables reaches a 0.7 bic, and more variables will have diminishing returns. A few defensible options are found:

- R2 method keeps all, but shows that most info comes from temperature in combination with the intercept
- AIC method proposes (-holiday1, -temperature)
- bic plot has interesting small sets that match intuition : 
  - (workingday1, temperature, humidity, windspeed)
  - idem previous, but add seasons (2-4)
  - idem previous, but replace humidity by weathersituations (2&3)

``````
CAS
# bic: (workingday, feelingtemp) to (-temperature -humidity -windspeed)
# Cp: same conclusion
# adjR2: in order of removal (-temperature -humidity -windspeed -holiday -season) %% adjR2=0.75
# aic: at numvar=7, same conclusions as registered

REG
# bic: best goes back to temp: (-holiday -feelingtemperature -humidity)
# Cp same conclusion
# adjr2: could go lower to (-holiday -temperature -humidity -windspeed)  %% adjR2=0.72
# aic: (-holiday -feelingtemperature -humidity -windspeed)
``````

Plots: use AIC ifo. numvar plots to explain decisions
Similar context, so makes sense that you can find matching variable selections

> !! regsubsets uses weights, consider correcting holidays!! maybe also weather & workingday
> However, correlation matrix indicates limited impact

### Interaction terms??

Difficult to inspect ALL possible combinations (fact(7)), doesn't seem like an interesting option

### Add weights : quick note

like WLS or simpler, as done in ex6 --> might help to reduce heteroskedasticity
Maybe be to use the counts of workingday for this? Most relevant categorical variable

Some short experiments were tried, but results were underwhelming so didn't investigate further

#### Final selection (same for both)

``````chosen vars
season, workingday, weathersituation, feelingtemperature
``````



## 6. Final models

linear regression models from selected variables after transformations etc show good results

Gauss-Markov conditions not fully met, but lm_selcas comes close
selreg has some heteroskedastic residuals, but zero-means seems acceptable

- Residual plots: 
  - QQ plot
  - standardized-fittedvals



## 7. Performance

### 7.1 data prepping

Apply same transformations, including standardisation with mean and sd values from TRN set
--> mismatches in mean and sd will add to performance problems!

``````
> mean(TRN$casual)
[1] 1.062137e-16
> mean(TST$casual)
[1] 0.5460628
> mean(TRN$registered)
[1] 6.262532e-17
> mean(TST$registered)
[1] 1.824052
> # standard deviation amplified for TST$registered
> sd(TRN$casual)
[1] 1
> sd(TST$casual)
[1] 1.010649
> sd(TRN$registered)
[1] 1
> sd(TST$registered)
[1] 1.345756
> # correlation (predictability) between casual and registered lower in TST set
> cor(TRN$casual,TRN$registered)
[1] 0.5784354
> cor(TST$casual,TST$registered)
[1] 0.4469186
``````



- Performance metrics (on predicted/fitted response vars)
  - RMSE (root mean square error)
  - MAE (mean absolute error)
  - Rsq (R-squared)
  - Rsq_adj (adjusted R-squared)
  - Cp (pearson correlation)

Residuals actually look quite good, except for the fact that the registered predictions have a mean of -1.7 (translational difference between TST and TRN) and a slightly higher stdev than casual

``````
         mean   stdev
casual   -0.46  0.49
regist   -1.7   0.74
``````

Final results:

``````
       RMSE      MAE        R2        R2a        Cp
1 0.6709274 0.549540 0.7996325  0.5416647 0.8840747 <-- casual
2 1.8633790 1.739227 2.0591360 -0.9938992 0.8579289 <-- registered
``````





NOTE: line 130 gives lm_refcas and lm_refreg, maybe also test on TST set for comparison?