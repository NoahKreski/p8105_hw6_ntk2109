HW6\_ntk2109\_Markdown
================
Noah Kreski
November 20, 2018

``` r
 #I am creating city_state, eliminating data errors and unnecessary city-states, convert age to numeric, and applying a true/false statement to non-unknown race
Washington_data = read_csv("./data/WashingtonPost/homicide-data.csv")%>%
                  mutate(city_state = paste(city,state, sep = ","))%>%
                  filter(city_state != "Tulsa,AL")%>%
                  filter(!(city_state %in% c("Dallas,TX", "Phoenix,AZ", "Kansas City,MO")))%>%
                  mutate(victim_age = as.numeric(victim_age))%>%
                  filter((victim_race != "Unknown"))%>%
                  mutate(victim_race = as.numeric(victim_race == "White"))
#I am assigning white and non-white labels to race and numeric values to whether a case was resolved, while also making white the reference                  
Washington_data$victim_race <- factor(Washington_data$victim_race, levels = c(1,0), labels = c("White","Non-White"))
Washington_data$Solved <- as.numeric(Washington_data$disposition == "Closed by arrest")
Washington_data$victim_race = fct_relevel(Washington_data$victim_race, "White")
```

``` r
#I am modeling the solved outcome for Baltimore logistically.
fit_logistic = 
  Washington_data%>% 
  filter(city_state == "Baltimore,MD")%>%
  glm(Solved ~ victim_age + victim_race + victim_sex, data = ., family = binomial()) 
#I am tidying the data to get what I need.
m1 = fit_logistic %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  select(term, log_OR = estimate, OR)%>%
  filter(term == "victim_raceNon-White")
```

``` r
#I am getting the data with appropriate formats
birthweight_data = read_csv("./data/birthweight.csv")%>%
                   mutate(babysex = as.factor(babysex), frace = as.factor(frace), malform = as.factor(malform), mrace = as.factor(mrace))
 #I am using a backwards selection model, eliminating one of the full set of predictors based on whatever has the highest p value until all are below .05                 
birthweight_data %>% 
  lm(bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + malform + menarche + mheight + mrace + momage +parity + pnumlbw + pnumsga +ppbmi + ppwt + smoken + wtgain, data = .) %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
#Eliminated frace, all levels above .05
birthweight_data %>% 
  lm(bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + malform + menarche + mheight + mrace + momage +parity + pnumlbw + pnumsga +ppbmi + ppwt + smoken + wtgain, data = .) %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
#Eliminated malform, p = .889
birthweight_data %>% 
  lm(bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + menarche + mheight + mrace + momage +parity + pnumlbw + pnumsga +ppbmi + ppwt + smoken + wtgain, data = .) %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
#Eliminated ppbmi, p = .760
birthweight_data %>% 
  lm(bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + menarche + mheight + mrace + momage +parity + pnumlbw + pnumsga + ppwt + smoken + wtgain, data = .) %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
#Eliminated momage, p = .530
birthweight_data %>% 
  lm(bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + menarche + mheight + mrace +parity + pnumlbw + pnumsga + ppwt + smoken + wtgain, data = .) %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
#Eliminated menarche, p = .245
birthweight_data %>% 
  lm(bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + mheight + mrace +parity + pnumlbw + pnumsga + ppwt + smoken + wtgain, data = .) %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
#Eliminated fincome, p = .069
birthweight_data %>% 
  lm(bwt ~ babysex + bhead + blength + delwt + gaweeks + mheight + mrace +parity + pnumlbw + pnumsga + ppwt + smoken + wtgain, data = .) %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
#All variables p<.05 or multiple levels p<.05
```

Following data loading and tidying, creating factors where necessary, I used a backwards selection model to attain a final regression model for these data. Every step I took away to predictor with the highest p-value until all variables had a p&lt;.05, or multiple factor levels below that threshold. This final model has 13 predictors including baby sex, head circumference at birth in cm, length at birth in cm, mother's weight at delivery in pounds, gestational age in weeks, mother's height, mother's race, number of live births prior to this pregnancy, previous number of low birth weight babies, number of prior small for gestational age babies, mother's pre-pregnancy weight, average number of cigarettes smoked per day during pregnancy and weight gain during pregnancy.

``` r
#I'm creating the needed data set again in a chunk where eval does not equal False
birthweight_data = read_csv("./data/birthweight.csv")%>%
                   mutate(babysex = as.factor(babysex), frace = as.factor(frace), malform = as.factor(malform), mrace = as.factor(mrace))
#I'm assigning my final model to fit
fit = lm(bwt ~ babysex + bhead + blength + delwt + gaweeks + mheight + mrace +parity + pnumlbw + pnumsga + ppwt + smoken + wtgain, data = birthweight_data)
#I am plotting the predictions and residuals
birthweight_data%>%
  modelr::add_predictions(fit)%>%
  modelr::add_residuals(fit)%>%
  ggplot(aes(x=pred, y = resid)) + geom_point()
```

![](Hw6_ntk2109_markdown_files/figure-markdown_github/model%20fit-1.png)

The residuals appear to exhibit consistent behavior across predicted values, centralized at a value of 0 and evenly distributed above and below, with the majority of residuals at an absolute value of 500 or less.

``` r
#I'm fitting the first alternative model and mapping its predictions and residuals.
fit_length_age = lm(bwt ~ blength + gaweeks, data = birthweight_data)

birthweight_data%>%
  modelr::add_predictions(fit_length_age)%>%
  modelr::add_residuals(fit_length_age)%>%
  ggplot(aes(x=pred, y = resid)) + geom_point()
```

![](Hw6_ntk2109_markdown_files/figure-markdown_github/other%20models-1.png)

``` r
#I'm fitting the second alternative model and mapping its predictions and residuals.
fit_circumference_length_sex = lm (bwt ~ bhead + blength + babysex + bhead*blength + bhead*babysex + blength*babysex + bhead*blength*babysex, data = birthweight_data)

birthweight_data%>%
  modelr::add_predictions(fit_circumference_length_sex)%>%
  modelr::add_residuals(fit_circumference_length_sex)%>%
  ggplot(aes(x=pred, y = resid)) + geom_point()
```

![](Hw6_ntk2109_markdown_files/figure-markdown_github/other%20models-2.png)

These plots map the residuals and predicted values for the two alternative models in the same way that I did earlier for my own model.

``` r
#I am developing test and train subsets for cross-validation
cv_df = crossv_mc(birthweight_data, 100) 

cv_df %>% pull(train) %>% .[[1]] %>% as_tibble
```

    ## # A tibble: 3,473 x 20
    ##    babysex bhead blength   bwt delwt fincome frace gaweeks malform menarche
    ##    <fct>   <int>   <int> <int> <int>   <int> <fct>   <dbl> <fct>      <int>
    ##  1 2          34      51  3629   177      35 1        39.9 0             13
    ##  2 1          34      48  3062   156      65 2        25.9 0             14
    ##  3 2          36      50  3345   148      85 1        39.9 0             12
    ##  4 1          33      52  3374   129      55 1        40.7 0             12
    ##  5 2          33      46  2523   126      96 2        40.3 0             14
    ##  6 2          33      49  2778   140       5 1        37.4 0             12
    ##  7 1          33      50  3459   169      75 2        40.7 0             12
    ##  8 1          35      51  3459   146      55 1        39.4 0             12
    ##  9 1          36      53  3629   147      75 1        41.3 0             11
    ## 10 1          35      51  3544   129      65 1        39.6 0             12
    ## # ... with 3,463 more rows, and 10 more variables: mheight <int>,
    ## #   momage <int>, mrace <fct>, parity <int>, pnumlbw <int>, pnumsga <int>,
    ## #   ppbmi <dbl>, ppwt <int>, smoken <dbl>, wtgain <int>

``` r
cv_df %>% pull(test) %>% .[[1]] %>% as_tibble
```

    ## # A tibble: 869 x 20
    ##    babysex bhead blength   bwt delwt fincome frace gaweeks malform menarche
    ##    <fct>   <int>   <int> <int> <int>   <int> <fct>   <dbl> <fct>      <int>
    ##  1 1          34      52  3062   157      55 1        40   0             14
    ##  2 2          34      52  3374   156       5 1        41.6 0             13
    ##  3 1          36      52  3515   146      85 1        40.3 0             11
    ##  4 2          35      51  3317   130      55 1        43.4 0             13
    ##  5 2          35      48  3175   158      75 1        39.7 0             13
    ##  6 2          35      57  3374   147      45 1        39.6 0             12
    ##  7 2          35      52  3289   135      55 1        40.6 0             13
    ##  8 2          34      49  3118   161      45 2        38.9 0             10
    ##  9 1          35      53  3175   130      45 1        40.4 0             13
    ## 10 2          34      54  3345   130      95 1        42.1 0             10
    ## # ... with 859 more rows, and 10 more variables: mheight <int>,
    ## #   momage <int>, mrace <fct>, parity <int>, pnumlbw <int>, pnumsga <int>,
    ## #   ppbmi <dbl>, ppwt <int>, smoken <dbl>, wtgain <int>

``` r
cv_df =
  cv_df %>% 
  mutate(train = map(train, as_tibble),
         test = map(test, as_tibble))
#I am using my various models in cross-validation examining each model with the train set, and then examining RMSE with the test set
cv_df = 
  cv_df %>% 
  mutate( fit = map(train, ~lm(bwt ~ babysex + bhead + blength + delwt + gaweeks + mheight + mrace +parity + pnumlbw + pnumsga + ppwt + smoken + wtgain, data = .x)),
         fit_length_age = map(train, ~lm(bwt ~ blength + gaweeks, data = .x)),
         fit_circumference_length_sex = map(train, ~lm(bwt ~ bhead + blength + babysex + bhead*blength + bhead*babysex + blength*babysex + bhead*blength*babysex, data = .x))) %>% 
  mutate(rmse_fit    = map2_dbl(fit, test, ~rmse(model = .x, data = .y)),
         rmse_fit_length = map2_dbl(fit_length_age, test, ~rmse(model = .x, data = .y)),
         rmse_fit_circumference = map2_dbl(fit_circumference_length_sex, test, ~rmse(model = .x, data = .y)))
#I am visualizing the RMSE distributions for each model.
cv_df %>% 
  select(starts_with("rmse")) %>% 
  gather(key = model, value = rmse) %>% 
  mutate(model = str_replace(model, "rmse_", ""),
         model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse)) + geom_violin()
```

![](Hw6_ntk2109_markdown_files/figure-markdown_github/Cross-validation-1.png)

The above cross validation and RMSE visual shows a lower RMSE for my own model than for either of the two alternative models provides, evidenced by a distribution of RMSE values that is lower than either of the other models.
