---
title: "Health Insurance Cost Analysis Report"
author: 
- group 17\newline
- presenters:\newline
              xue zhao\newline
              xin zhou\newline
- group members:\newline
                shuruq\newline   
                shihao fu\newline   
                sifan yu
date: "13/03/2020"
output:
  beamer_presentation: 
    theme: "Darmstadt"
    colortheme: "beaver"
  fig_height: 7
  fig_width: 10
  fontsize: 12pt
  fonttheme: structurebold
  ioslides_presentation: default
  number_sections: yes
  slidy_presentation: default
  theme: Berlin
  toc: yes
---


```{r setup, include=FALSE}
library(ggplot2)
library(dplyr)
library(moderndive)
library(gapminder)
library(skimr)
library(kableExtra)
library(gridExtra)
library(ggpubr)

#library(plotly)
library(tidyr)
library(broom)
pairs(sub)
```


## DAS GROUP PROJECT 

- 1 Introduction 
- 2 Exploratory data analysis 
- 3 Formal data analysis 
- 4 Conclusions 
- 5 Q&A

## Introduction 
- The dataset is obtained from Kaggle.
- Primary goal is to examine the relationship between medical costs paid by individuals and their age, bmi(Body Mass Index), smoking status and obese status.

## Introduction 
- charges(response variable):Individual medical costs billed by health insurance;
- age: age of primary beneficiary; 
- bmi: Body mass index; 
- smoker: smoking status,1 for smoker and 0 otherwise; 
- obese: a person with a bmi of 30 or more is considered obese, which is shown as 1 in the data , and 0 otherwise.



```{r , echo = FALSE}
ds <- read.csv("insurance.csv", header = TRUE)

set.seed(12)

sub <- sample_n(ds,size = 500,replace = FALSE)

sub$obese <- ifelse(sub$bmi >= 30, 1, 0)

sub <- sub %>% mutate(log_charges= log(charges), log_age=log(age) , log_bmi=log(bmi) , age2=age^2)

```

## Exploratory data analysis
```{r , echo=FALSE}
sub %>% 
  group_by(smoker) %>%
  summarize(n=n(), Mean=round(mean(charges),digits=2), 
            St.Dev=round(sd(charges),digits=2), 
            Min=min(charges), Q1 = quantile(charges,0.25), 
            Median=median(charges), 
            Q3 = quantile(charges,0.75), Max=max(charges)) %>%
  kable(caption = '\\label{tab:summaries} Summary statistics on charges by smoking status for 500 Individuals') %>%  
  kable_styling(latex_options = "hold_position",font_size = 6)

```

- The medical charges of the smokers were consistently greater than that of non-smokers.


## Exploratory data analysis
```{r, echo = FALSE, eval = TRUE,out.width = '90%' ,fig.width=15,fig.align = "center", fig.cap = "\\label{fig:point} The correlationship between charge and bmi, age.", fig.pos = 'H'}
  
plot_age <- ggplot(sub, aes(x = age, y = charges, color = smoker)) + 
  geom_point() + 
  labs(x = "Age", y = "Charges") + 
  geom_smooth(method = "lm", se = FALSE)

plot_bmi <- ggplot(sub, aes(x = bmi, y = charges, color = smoker)) + 
  geom_point() + 
  labs(x = "Body Mass Index", y = "Charges") + 
  geom_vline(aes(xintercept=30), color = "black", linetype = "dashed")

ggarrange(plot_age, plot_bmi, ncol = 2)

```
- Positive correlations between age and medical charges
- Obvious interaction for smoker and non-smoker after bmi=30




## Exploratory data analysis
```{r boxplot, echo = FALSE, eval = TRUE, out.width = '60%', fig.align = "center", fig.cap = "\\label{fig:box} Charges by Obese.", fig.pos = 'H'}
ggplot(sub, aes(y = charges, x = as.factor(obese))) + 
  geom_boxplot() + 
  labs(x = "Obese", y = "Charges", title = "Charges of 500 people") + 
  geom_hline(aes(yintercept=10387.879), color = "red", linetype = "dashed")

```
- Obese individuals pay higher medical charges.
- The medical charges of the obese individuals were more widely distributed.

## Formal Data Analysis
A multiple linear regression model that will be fitted to the data is as follows:
$$y_i = \alpha +\beta_1 \cdot age+ \beta_2 \cdot bmi+\beta_3 \cdot \mathbb{I}_T(smoker_{i})+ \beta_4 \cdot \mathbb{I}_T(obese_{i}) +$$ 
$$\beta_5  \cdot[ \mathbb{I}_T(smoker_{i}) \cdot \mathbb{I}_T(obese_{i})]+ \epsilon_i, ~~~~\epsilon_i \sim N(0, \sigma^2), $$


• $\mathbb{I}_{\mbox{T}}(x)$ is an indicator function such that:
$$\mathbb{I}_{\mbox{T}}(x)=\left\{ \begin{array}{ll} 1 ~~~~~  \mbox{ if  the $i^{th}$ observation is TRUE },\\ 0 ~~~~~ \mbox{Otherwise}.\\ \end{array} \right.$$

## Formal Data Analysis
```{r , echo = FALSE, eval = TRUE, warning = FALSE, message = FALSE}


multi_modelm <- lm(charges ~  bmi+ age +smoker+obese +obese*smoker , data = sub)
get_regression_table(multi_modelm) %>%
knitr::kable(
digits = 3,
caption = "\\label{tab:regtable} Summary table of the multiple linear regression model.") %>%
  kable_styling(latex_options = 'HOLD_position',font_size = 7)

```
- The p-value of bmi is greater than 0.05, drop it from the model
- The p-value of obese is greater than 0.05, keep it because the interaction term is significant

## Model selection
```{r model selection , echo = FALSE, eval = TRUE, warning = FALSE, message = FALSE}
model1 <- lm(charges ~ obese, data = sub)
model2 <- lm(charges ~ age, data = sub)
model3 <- lm(charges ~ smoker, data = sub)

multi_modelfull <- lm(charges ~  age+smoker+obese+obese*smoker, data = sub)

multi_model2 <- lm(charges ~  obese+age, data = sub)

multi_model3 <- lm(charges ~  obese+smoker, data = sub)

multi_model4 <- lm(charges ~  age+smoker, data = sub)

multi_model5 <- lm(charges ~  obese+smoker+obese*smoker, data = sub)

multi_model6 <- lm(charges ~  obese+age+smoker, data = sub)






slr.bmi<-glance(model1)
slr.age<-glance(model2)
slr.smoker<-glance(model3)





mlr1<-glance(multi_modelfull)
mlr2<-glance(multi_model2)
mlr3<-glance(multi_model3)
mlr4<-glance(multi_model4)
mlr5<-glance(multi_model5)
mlr6<-glance(multi_model6)





Models <- c('SLR(obese)','SLR(age)','SLR(smoker)','MLR1 (age+smoker+obese+obese*smoker)',
            'MLR2(obese+age)','MLR3(obese+smoker)','MLR4(age+smoker)','MLR5(obese+smoker+obese*smoker)','MLR6(obese+age+smoker)') 
bind_rows(slr.bmi, slr.age,slr.smoker,mlr1,mlr2,mlr3,mlr4,mlr5,mlr6,.id="Model") %>%
  dplyr::select(Model,adj.r.squared,AIC,BIC) %>%
  mutate(Model=Models) %>%  
  kable(
     digits = 2,
     caption = '\\label{tab:model selection} Model comparison values for different models') %>%
  kable_styling(latex_options = 'HOLD_position',font_size = 8 )

```

## Formal data analysis
```{r eval=TRUE, message=FALSE, warning=FALSE, , echo=FALSE}
multi_modelm2<- lm(charges ~  age +smoker+obese +obese*smoker , data = sub)
get_regression_table(multi_modelm2) %>%
knitr::kable(
digits = 3,
caption = "\\label{tab:regtable} Summary table of the multiple linear regression model.") %>%
  kable_styling(latex_options = 'HOLD_position',font_size = 7)

```
The best-fitting modelling is given as: 
$$\widehat{\mbox{charges}} = -3241.623	+ 286.802\cdot \mbox{age}+13108.624 \cdot \mathbb{I}_{\mbox{T}}(smoker_{i})+602.496	 $$
$$\cdot \mathbb{I}_{\mbox{T}}(obese_{i}) + 20236.692	 \cdot [\mathbb{I}_{\mbox{T}}(smoker_{i}) \cdot   \mathbb{I}_{\mbox{T}}(obese_{i})].$$

## Formal data analysis
- The equation of non-obese smoker is 
$$\widehat{\mbox{charges}} = -3241.623	+ 286.802\cdot \mbox{age}+13108.624$$
- The equation of obese non-smoker is 
$$\widehat{\mbox{charges}} = -3241.623	+ 286.802\cdot \mbox{age}+602.496$$
- The equation of obese smoker is 
$$\widehat{\mbox{charges}} = -3241.623	+ 286.802\cdot \mbox{age}+13108.624+$$
$$602.496+20236.692$$



## Assessing model fit

```{r scatterplots, echo = FALSE, eval = TRUE, out.width = '90%', fig.align = "center", fig.pos = "h", warning = FALSE, fig.cap = "\\label{fig:scat} Scatterplots of the residuals by smoker and Obese."}

regression.points <- get_regression_points(multi_modelm)
regression.points %>%
  ggplot(aes(x = charges_hat, y = residual)) + 
  geom_point(color = alpha("grey10", 0.5), fill = alpha("grey10", 0.1), shape = 21) +
  labs(x = "fitted value", y = "residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1) +
  facet_wrap(~ smoker + obese, labeller = label_both)

```

## Assessing model fit
```{r histogram, echo = FALSE, eval = TRUE, warning = FALSE, out.width = '90%', fig.align = "center", fig.pos = "H", fig.cap = "\\label{fig:hist} Histograms of the residuals by smoker and Obese."}
p1<-regression.points %>%
  ggplot(aes(residual)) +
  geom_histogram(bins = 50, fill = "grey", col = "white", aes(y = ..density..)) +
  geom_line(stat = "density", aes(y = ..density..), col = alpha("grey2", 0.7), size = 1) +
  facet_wrap(~ smoker , labeller = label_both)

p2<-regression.points %>%
  ggplot(aes(residual)) +
  geom_histogram(bins = 50, fill = "grey", col = "white", aes(y = ..density..)) +
  geom_line(stat = "density", aes(y = ..density..), col = alpha("grey2", 0.7), size = 1) +
  facet_wrap(~  obese, labeller = label_both)

grid.arrange(p1,p2, ncol =1)

```

## Assessing model fit(log-transformation)
```{r , echo = FALSE, eval = TRUE, warning = FALSE, message = FALSE}
multi_modellog <- lm(log_charges ~ age+age2+ obese +smoker +obese*smoker , data = sub)
```
```{r, echo = FALSE, eval = TRUE, out.width = '70%', fig.align = "center", fig.pos = "h", warning = FALSE, fig.cap = "\\label{fig:scat} Scatterplots of the residuals by smoker and Obese."}

regression.points <- get_regression_points(multi_modellog)
regression.points %>%
  ggplot(aes(x = log_charges_hat, y = residual)) + 
  geom_point(color = alpha("grey10", 0.5), fill = alpha("grey10", 0.1), shape = 21) +
  labs(x = "fitted value", y = "residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1) +
  facet_wrap(~ smoker + obese, labeller = label_both)

```

## Assessing model fit(log-transformation)
```{r, echo = FALSE, eval = TRUE, warning = FALSE, out.width = '70%', fig.align = "center", fig.pos = "H", fig.cap = "\\label{fig:hist} Histograms of the residuals by smoker and Obese."}
p1<-regression.points %>%
  ggplot(aes(residual)) +
  geom_histogram(bins = 50, fill = "grey", col = "white", aes(y = ..density..)) +
  geom_line(stat = "density", aes(y = ..density..), col = alpha("grey2", 0.7), size = 1) +
  facet_wrap(~ smoker , labeller = label_both)

p2<-regression.points %>%
  ggplot(aes(residual)) +
  geom_histogram(bins = 50, fill = "grey", col = "white", aes(y = ..density..)) +
  geom_line(stat = "density", aes(y = ..density..), col = alpha("grey2", 0.7), size = 1) +
  facet_wrap(~  obese, labeller = label_both)

grid.arrange(p1,p2, ncol =1)

```

## Conclusion

- The obese smokers pay, on average,the highest medical charges amongest all the groups follow by the non-obese smoker.
- The model assumptions were not supported.
- It is recommended to add more significant predictors and more observations to fit a better model which meet the model assumption.


## Question&Answer

- Q&A
