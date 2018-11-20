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
