HW6\_ntk2109\_Markdown
================
Noah Kreski
November 20, 2018

``` r
Washington_data = read_csv("./data/WashingtonPost/homicide-data.csv")%>%
                  mutate(city_state = paste(city,state, sep = ","))%>%
                  filter(city_state != "Tulsa,AL")%>%
                  filter(!(city_state %in% c("Dallas,TX", "Phoenix,AZ", "Kansas City,MO")))%>%
                  mutate(victim_age = as.numeric(victim_age))%>%
                  filter((victim_race != "Unknown"))%>%
                  mutate(victim_race = as.numeric(victim_race == "White"))
```

    ## Warning in evalq(as.numeric(victim_age), <environment>): NAs introduced by
    ## coercion

``` r
Washington_data$victim_race <- factor(Washington_data$victim_race, levels = c(1,0), labels = c("White","Non-White"))
Washington_data$Solved <- as.numeric(Washington_data$disposition == "Closed by arrest")
Washington_data$victim_race = fct_relevel(Washington_data$victim_race, "White")
```

``` r
fit_logistic = 
  Washington_data%>% 
  filter(city_state == "Baltimore,MD")%>%
  glm(Solved ~ victim_age + victim_race + victim_sex, data = ., family = binomial()) 

m1 = fit_logistic %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  select(term, log_OR = estimate, OR)%>%
  filter(term == "victim_raceNon-White")
```

``` r
birthweight_data = read_csv("./data/birthweight.csv")%>%
                   mutate(babysex = as.factor(babysex), frace = as.factor(frace), malform = as.factor(malform), mrace = as.factor(mrace))
                  
birthweight_data %>% 
  lm(bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + malform + menarche + mheight + mrace + momage +parity + pnumlbw + pnumsga +ppbmi + ppwt + smoken + wtgain, data = .) %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
```

| term        |   estimate|  std.error|  statistic|  p.value|
|:------------|----------:|----------:|----------:|--------:|
| (Intercept) |  -6265.391|    660.401|     -9.487|    0.000|
| babysex2    |     28.707|      8.465|      3.391|    0.001|
| bhead       |    130.778|      3.452|     37.881|    0.000|
| blength     |     74.954|      2.022|     37.075|    0.000|
| delwt       |      4.101|      0.395|     10.386|    0.000|
| fincome     |      0.290|      0.180|      1.614|    0.107|
| frace2      |     14.331|     46.150|      0.311|    0.756|
| frace3      |     21.236|     69.296|      0.306|    0.759|
| frace4      |    -46.996|     44.678|     -1.052|    0.293|
| frace8      |      4.297|     74.074|      0.058|    0.954|
| gaweeks     |     11.549|      1.465|      7.882|    0.000|
| malform1    |      9.765|     70.626|      0.138|    0.890|
| menarche    |     -3.551|      2.895|     -1.226|    0.220|
| mheight     |      9.787|     10.312|      0.949|    0.343|
| mrace2      |   -151.435|     46.045|     -3.289|    0.001|
| mrace3      |    -91.387|     71.919|     -1.271|    0.204|
| mrace4      |    -56.479|     45.137|     -1.251|    0.211|
| momage      |      0.759|      1.222|      0.621|    0.534|
| parity      |     95.541|     40.479|      2.360|    0.018|
| ppbmi       |      4.354|     14.891|      0.292|    0.770|
| ppwt        |     -3.472|      2.612|     -1.329|    0.184|
| smoken      |     -4.854|      0.587|     -8.269|    0.000|

``` r
birthweight_data %>% 
  lm(bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + malform + menarche + mheight + mrace + momage +parity + pnumlbw + pnumsga +ppbmi + ppwt + smoken + wtgain, data = .) %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
```

| term        |   estimate|  std.error|  statistic|  p.value|
|:------------|----------:|----------:|----------:|--------:|
| (Intercept) |  -6275.754|    660.106|     -9.507|    0.000|
| babysex2    |     28.677|      8.462|      3.389|    0.001|
| bhead       |    130.796|      3.450|     37.914|    0.000|
| blength     |     74.912|      2.021|     37.075|    0.000|
| delwt       |      4.100|      0.395|     10.392|    0.000|
| fincome     |      0.290|      0.179|      1.619|    0.106|
| gaweeks     |     11.565|      1.465|      7.895|    0.000|
| malform1    |      9.860|     70.604|      0.140|    0.889|
| menarche    |     -3.612|      2.892|     -1.249|    0.212|
| mheight     |      9.975|     10.307|      0.968|    0.333|
| mrace2      |   -137.203|     10.215|    -13.432|    0.000|
| mrace3      |    -73.858|     42.789|     -1.726|    0.084|
| mrace4      |    -99.158|     19.386|     -5.115|    0.000|
| momage      |      0.755|      1.221|      0.618|    0.536|
| parity      |     95.667|     40.468|      2.364|    0.018|
| ppbmi       |      4.563|     14.884|      0.307|    0.759|
| ppwt        |     -3.508|      2.611|     -1.344|    0.179|
| smoken      |     -4.828|      0.586|     -8.239|    0.000|

``` r
birthweight_data %>% 
  lm(bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + menarche + mheight + mrace + momage +parity + pnumlbw + pnumsga +ppbmi + ppwt + smoken + wtgain, data = .) %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
```

| term        |   estimate|  std.error|  statistic|  p.value|
|:------------|----------:|----------:|----------:|--------:|
| (Intercept) |  -6274.691|    659.987|     -9.507|    0.000|
| babysex2    |     28.660|      8.461|      3.387|    0.001|
| bhead       |    130.799|      3.449|     37.920|    0.000|
| blength     |     74.907|      2.020|     37.082|    0.000|
| delwt       |      4.102|      0.394|     10.403|    0.000|
| fincome     |      0.289|      0.179|      1.616|    0.106|
| gaweeks     |     11.563|      1.465|      7.895|    0.000|
| menarche    |     -3.616|      2.892|     -1.251|    0.211|
| mheight     |      9.961|     10.305|      0.967|    0.334|
| mrace2      |   -137.232|     10.212|    -13.439|    0.000|
| mrace3      |    -73.919|     42.782|     -1.728|    0.084|
| mrace4      |    -99.215|     19.380|     -5.120|    0.000|
| momage      |      0.758|      1.220|      0.622|    0.534|
| parity      |     95.643|     40.463|      2.364|    0.018|
| ppbmi       |      4.548|     14.882|      0.306|    0.760|
| ppwt        |     -3.507|      2.611|     -1.343|    0.179|
| smoken      |     -4.826|      0.586|     -8.239|    0.000|

``` r
birthweight_data %>% 
  lm(bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + menarche + mheight + mrace + momage +parity + pnumlbw + pnumsga + ppwt + smoken + wtgain, data = .) %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
```

| term        |   estimate|  std.error|  statistic|  p.value|
|:------------|----------:|----------:|----------:|--------:|
| (Intercept) |  -6077.604|    140.239|    -43.337|    0.000|
| babysex2    |     28.638|      8.459|      3.385|    0.001|
| bhead       |    130.831|      3.447|     37.950|    0.000|
| blength     |     74.910|      2.020|     37.087|    0.000|
| delwt       |      4.100|      0.394|     10.402|    0.000|
| fincome     |      0.290|      0.179|      1.622|    0.105|
| gaweeks     |     11.560|      1.464|      7.894|    0.000|
| menarche    |     -3.640|      2.890|     -1.259|    0.208|
| mheight     |      6.860|      1.799|      3.814|    0.000|
| mrace2      |   -137.202|     10.210|    -13.438|    0.000|
| mrace3      |    -74.616|     42.717|     -1.747|    0.081|
| mrace4      |    -99.429|     19.365|     -5.134|    0.000|
| momage      |      0.766|      1.220|      0.628|    0.530|
| parity      |     95.459|     40.454|      2.360|    0.018|
| ppwt        |     -2.720|      0.433|     -6.290|    0.000|
| smoken      |     -4.828|      0.586|     -8.243|    0.000|

``` r
birthweight_data %>% 
  lm(bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + menarche + mheight + mrace +parity + pnumlbw + pnumsga + ppwt + smoken + wtgain, data = .) %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
```

| term        |   estimate|  std.error|  statistic|  p.value|
|:------------|----------:|----------:|----------:|--------:|
| (Intercept) |  -6070.190|    139.731|    -43.442|    0.000|
| babysex2    |     28.477|      8.455|      3.368|    0.001|
| bhead       |    130.851|      3.447|     37.961|    0.000|
| blength     |     74.893|      2.020|     37.085|    0.000|
| delwt       |      4.079|      0.393|     10.387|    0.000|
| fincome     |      0.315|      0.175|      1.802|    0.072|
| gaweeks     |     11.610|      1.462|      7.941|    0.000|
| menarche    |     -3.298|      2.838|     -1.162|    0.245|
| mheight     |      6.854|      1.799|      3.810|    0.000|
| mrace2      |   -138.751|      9.907|    -14.006|    0.000|
| mrace3      |    -71.456|     42.416|     -1.685|    0.092|
| mrace4      |   -100.156|     19.329|     -5.182|    0.000|
| parity      |     97.309|     40.344|      2.412|    0.016|
| ppwt        |     -2.679|      0.427|     -6.268|    0.000|
| smoken      |     -4.831|      0.586|     -8.248|    0.000|

``` r
birthweight_data %>% 
  lm(bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + mheight + mrace +parity + pnumlbw + pnumsga + ppwt + smoken + wtgain, data = .) %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
```

| term        |   estimate|  std.error|  statistic|  p.value|
|:------------|----------:|----------:|----------:|--------:|
| (Intercept) |  -6098.822|    137.546|    -44.340|    0.000|
| babysex2    |     28.558|      8.455|      3.378|    0.001|
| bhead       |    130.777|      3.447|     37.944|    0.000|
| blength     |     74.947|      2.019|     37.120|    0.000|
| delwt       |      4.107|      0.392|     10.475|    0.000|
| fincome     |      0.318|      0.175|      1.820|    0.069|
| gaweeks     |     11.592|      1.462|      7.929|    0.000|
| mheight     |      6.594|      1.785|      3.694|    0.000|
| mrace2      |   -138.792|      9.907|    -14.009|    0.000|
| mrace3      |    -74.887|     42.315|     -1.770|    0.077|
| mrace4      |   -100.678|     19.325|     -5.210|    0.000|
| parity      |     96.305|     40.336|      2.388|    0.017|
| ppwt        |     -2.676|      0.427|     -6.261|    0.000|
| smoken      |     -4.843|      0.586|     -8.271|    0.000|

``` r
birthweight_data %>% 
  lm(bwt ~ babysex + bhead + blength + delwt + gaweeks + mheight + mrace +parity + pnumlbw + pnumsga + ppwt + smoken + wtgain, data = .) %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
```

| term        |   estimate|  std.error|  statistic|  p.value|
|:------------|----------:|----------:|----------:|--------:|
| (Intercept) |  -6101.819|    137.573|    -44.353|    0.000|
| babysex2    |     28.374|      8.457|      3.355|    0.001|
| bhead       |    131.023|      3.445|     38.035|    0.000|
| blength     |     74.793|      2.018|     37.066|    0.000|
| delwt       |      4.084|      0.392|     10.419|    0.000|
| gaweeks     |     11.679|      1.462|      7.990|    0.000|
| mheight     |      6.857|      1.780|      3.853|    0.000|
| mrace2      |   -145.375|      9.226|    -15.758|    0.000|
| mrace3      |    -77.978|     42.292|     -1.844|    0.065|
| mrace4      |   -105.987|     19.108|     -5.547|    0.000|
| parity      |     94.810|     40.339|      2.350|    0.019|
| ppwt        |     -2.651|      0.427|     -6.204|    0.000|
| smoken      |     -4.874|      0.585|     -8.324|    0.000|

``` r
fit = lm(bwt ~ babysex + bhead + blength + delwt + gaweeks + mheight + mrace +parity + pnumlbw + pnumsga + ppwt + smoken + wtgain, data = birthweight_data)

birthweight_data%>%
  modelr::add_predictions(fit)%>%
  modelr::add_residuals(fit)%>%
  ggplot(aes(x=pred, y = resid)) + geom_point()
```

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit
    ## may be misleading

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit
    ## may be misleading

![](Hw6_ntk2109_markdown_files/figure-markdown_github/Problem%20Two%20GLM-1.png)
