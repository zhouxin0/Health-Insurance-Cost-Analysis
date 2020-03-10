---
title: "Health Insurance Cost Analysis Report"
author: "Group 17_xin zhou"
output:
  pdf_document:
          latex_engine: xelatex
          number_sections: yes
fig_caption: yes
bibliography: bibliography.bib
nocite: |
  @ML, @data
---

```{r setup, include=FALSE}
# knit the rmd file and generate a formal report 
knitr::opts_chunk$set(echo = TRUE, comment = NA)
```

```{r libraries, echo = FALSE, eval = TRUE, warning = FALSE, message = FALSE}
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
```

```{r data, echo = FALSE, eval = TRUE, warning = FALSE, message = FALSE}
ds <- read.csv("insurance.csv", header = TRUE)

set.seed(12)

sub <- sample_n(ds,size = 500,replace = FALSE)

sub$obese <- ifelse(sub$bmi >= 30, 1, 0)

sub <- sub %>% mutate(log_charges= log(charges), log_age=log(age) , log_bmi=log(bmi) , age2=age^2)

```

# Introduction {#sec:intro}
Medical insurance can cover the whole or a part of the cost of medical treatment, there are different medical insurance plans for beneficiaries to choose. Citizens will pay different health insurance charges based on their circumstances. This dataset contains 1338 records from four regions (southwest, southeast, northwest and northeast). This report will select 500 observations from the dataset randomly, which contains their insurance charges, age, bmi (Body Mass Index), whether obese or not(1 for obese which is bmi>=30,0 for non-obese which is bmi<30) and  whether they smoke (shown by the variable "smoker") etc. Hence, the relationship between the insurance charges citizens paied and their age, bmi(Body Mass Index), smoking status and obese status will be examined by using linear regression method in this report.

This report will examine the contribution of the following variables in the yearly medical charges(charges):age,bmi,smoker and obese.

Section \ref{sec:EDA} consists of an exploratory data analysis of age, bmi, smoking status and explores the potential relationship between charges and age, bmi respectively. Besides, a new indicator variable named "obese" will be introduced in the Section \ref{sec:EDA}. Section \ref{sec:FDA} contains the model selection part and the results from fitting a linear regression model to the data, as well as the assessment of the model assumptions. Concluding remarks are given in Section \ref{sec:Conc}.

# Exploratory Data Analysis {#sec:EDA}
Summary statistics of the charges are presented in the following table for each sex separately.

```{r, echo = FALSE, eval = TRUE, warning = FALSE, message = FALSE}
sub %>% 
  group_by(smoker) %>%
  summarize(n=n(), Mean=round(mean(charges),digits=2), 
            St.Dev=round(sd(charges),digits=2), 
            Min=min(charges), Q1 = quantile(charges,0.25), 
            Median=median(charges), 
            Q3 = quantile(charges,0.75), Max=max(charges)) %>%
  kable(caption = '\\label{tab:summaries} Summary statistics on charges by smoker of 500 people.') %>% 
  kable_styling(latex_options = "hold_position")

```

Table \ref{tab:summaries} shows that the summaries of the charges of the smokers were consistently greater than that of non-smokers. For example, the mean charges of the smokers was 31893.75 compared to 8399.39 for that of non-smokers. The variability also can be seen in the smokers charges, as measured by the standard deviation of 11674.66. By comparing the Q1 and Q2 values of charges, it can be seen that the chagres of smokers were more widely distributed. 

In addition, summary statistics of the age and bmi are presented in the following table.

```{r, echo = FALSE, eval = TRUE}
sub_s <- sub %>%
  dplyr::select(age, bmi,charges)
my_skim <- skim_with(numeric = sfl(hist = NULL))
my_skim(sub_s)  %>% 
  dplyr::select(-skim_type) %>% 
  kable(col.names = c("Variable", "Missing", "Complete", "Mean", "SD", "Min.", "1st Q.", "Median", "3rd Q.", "Max."),
        caption = '\\label{tab:summary} Summary statistics on age and bmi.', digits = 2) %>%
  kable_styling(latex_options = "hold_position")

```

Table \ref{summary} contains summary statistics on the age and bmi of the 500 people. First, the middle 50% of age lie between 27 and 51, with an average age of 39. Secondly, the middle 50% of bmi lies between 26.16 and 34.52, with an average bmi of 30.51.

The correlationship between charge and bmi, age can be seen in the following figures.

```{r, echo = FALSE, eval = TRUE,out.width = '100%' ,fig.align = "center", fig.cap = "\\label{fig:point} The correlationship between charge and bmi, age.", fig.pos = 'H'}
  
plot_age <- ggplot(sub, aes(x = age, y = charges, color = smoker)) + 
  geom_point() + 
  labs(x = "Age", y = "Charges") + 
  geom_smooth(method = "lm", se = FALSE)

plot_bmi <- ggplot(sub, aes(x = bmi, y = charges, color = smoker)) + 
  geom_point() + 
  labs(x = "Body Mass Index", y = "Charges") + 
  geom_vline(aes(xintercept=30), color = "black", linetype = "dashed")

ggarrange(plot_age, plot_bmi, nrow = 2)

```

The charges against age point plot shows that the correlation between age and charges, which indicate a positive correlations, from the plot, two parellel lines can be fitted. 

For the charges against BMI point plot, compared with the bmi of 30 or above, when the bmi is less than 30 the correlation coefficient (shown by the slope) between charges and bmi is small.For the bmi greater than 30,the interaction between variable smoker and bmi is obvious with different slopes for smoker and non-smoker group.

Therefore, a new indicator variable called "obese" will be considered, value 1 of variable "obese" means the person is obese and the bmi is at least 30,otherwise the value of obese is 0. Boxplot of the obese are presented in the following figure for each group(0 or 1) separately.

```{r,echo = FALSE, eval = FALSE}
sub %>% 
  group_by(as.factor(obese)) %>% 
  summarize(n=n(), Mean=round(mean(charges),digits=2), 
            St.Dev=round(sd(charges),digits=2), 
            Min=min(charges), Q1 = quantile(charges,0.25), 
            Median=median(charges), 
            Q3 = quantile(charges,0.75), Max=max(charges))
```

```{r boxplot, echo = FALSE, eval = TRUE, out.width = '56%', fig.align = "center", fig.cap = "\\label{fig:box} Charges by obese.", fig.pos = 'H'}
ggplot(sub, aes(y = charges, x = as.factor(obese))) + 
  geom_boxplot() + 
  labs(x = "Obese", y = "Charges", title = "Charges of 500 people") + 
  geom_hline(aes(yintercept=10387.879), color = "red", linetype = "dashed")

```

The boxplot shows that obese people paying higher medical charges, in general, compared to the people who are not overweight, and that the medical charges of the obese people were more widely distributed. The red dashed line indicates that the median of charges for obese people is larger than than of non-obese people. There are also potentially outliers, which have unusually charges, as shown by the points shown beyond the “whiskers” of the boxplots.

# Formal Data Analysis {#sec:FDA}
A multiple linear regression model that will be fitted to the data is as follows:
$$y_i = \alpha +\beta_1 x_{1i}+\beta_2 x_{2i}+ \beta_3 \cdot \mathbb{I}_T(smoker_{i})+ \beta_4 \cdot \mathbb{I}_T(obese_{i}) +\beta_5  \cdot[ \mathbb{I}_T(smoker_{i}) \cdot \mathbb{I}_T(obese_{i})]+ \epsilon_i, ~~~~                         \epsilon_i \sim N(0, \sigma^2), $$
where: \
• $y_i$ is the expected value of the insurance charges of the $i^{th}$ indivatioal in the sample;\
• $x_{1i}$ and $x_{2i}$ denote the age and bmi of the $i^{th}$ indivatioal in the sample,respectively ;\
• $\alpha$ is the mean charges for the baseline category of non-smoker and non-obese;\
• $\beta_1$ the slope of the regression line (the effects by age) for the baseline of smoker and non-smokerand,obese and non-obese;\
• $\beta_2$  is the slope of the regression line (the effects by bmi) for baseline of smoker and non-smokerand,obese and non-obese;\
• $\beta_3$ is the difference in the mean charges of smoker relative to the baseline category non-smoker and non-obese ;\
• $\beta_4$ is the difference in the mean charges of obese relative to the baseline category;\
• $\beta_5$ is the difference in the mean charges of obese smoker (interaction tearm) relative to the baseline category;\
• $\mathbb{I}_{\mbox{T}}(x)$ is an indicator function such that:
$$\mathbb{I}_{\mbox{T}}(x)=\left\{ \begin{array}{ll} 1 ~~~~~  \mbox{ if  the $i^{th}$ observation is TRUE },\\ 0 ~~~~~ \mbox{Otherwise}.\\ \end{array} \right.$$
• $\epsilon_i$ refers to the random error term, which are normally distributed with mean zero and variance $\sigma^2$.

```{r , echo = FALSE, eval = TRUE, warning = FALSE, message = FALSE}


multi_modelm <- lm(charges ~  bmi+ age +smoker+obese +obese*smoker + age*smoker, data = sub)
get_regression_table(multi_modelm) %>%
knitr::kable(
digits = 3,
caption = "\\label{tab:regtable} Summary table of the multiple linear regression model.") %>%
  kable_styling(latex_options = 'HOLD_position')

```
In table \ref{tab:regtable},the variables age  and smokeryes have confidence intervals of approximately 255.820 to	313.120 and 11758.337 to 14548.627 respectively. These confidence intervals do not contain a zero therefore making them statistically significant.In contrast,the confidence interval of bmi which includes zero, therefore making bmi not statistically significant so we will drop bmi from the full model .In addition, the p-value for the variable obese is approximately 0.707, which is above the p-value (0.05) making this variable not statistically significant,but since the obese has an interaction term with smoker and this term is statistically significant so we have to keep obese in the model.

## Model selection

Model selection is a process of selecting the best fitting model from a set of possible models in the data. Many selection criteria such as AIC, BIC and adj.r.squared can be used in determining the best model. A model selection will be carried out to find the best fitting model from all possible sub models from the model above .

* SLR: This is a simple linear regression model with only 1 explanatory variable each time i.e with only obese,age, and smoker,respectively.

* MLR1(full model): This is a multiple linear regression model with the 4 explanatory variables, namely obese, age, smoker and interaction term between obese and smoker.

* MLR2: This is a multiple linear regression model with only 2 explanatory variables, namely obese, age.

* MLR3: This is a multiple linear regression model with only 2 explanatory variables, namely obese, smoker.

* MLR4: This is a multiple linear regression model with only 2 explanatory variables, namely age, smoker.

* MLR5: This is a multiple linear regression model with only 3 explanatory variables, namely obese,smoker and  interaction term between obese and smoker.

* MLR6:  This is a multiple linear regression model with only 3 explanatory variables, namely obese,age and smoker.



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





Models <- c('SLR(obese)','SLR(age)','SLR(smoker)','MLR1 (full model)',
            'MLR2','MLR3','MLR4','MLR5','MLR6') 
bind_rows(slr.bmi, slr.age,slr.smoker,mlr1,mlr2,mlr3,mlr4,mlr5,mlr6,.id="Model") %>%
  dplyr::select(Model,adj.r.squared,AIC,BIC) %>%
  mutate(Model=Models) %>%  
  kable(
     digits = 2,
     caption = '\\label{tab:model selection} Model comparison values for different models') %>%
  kable_styling(latex_options = 'HOLD_position' )

```

Table \ref{tab:model selection} displays a comparison between differen criterion values for the 9 fitted models.The best fitting model
maximizes Adjusted $R^2$ and minimizes the AIC and BIC values. It can be seen that the full model has the highest adj $R^2$ (0.87) and the lowerest AIC (9826.19) and BIC(9851.48).This is a same model we get when we performed the other model selections approaches such as stepwise, backward and forward.


```{r eval=TRUE, message=FALSE, warning=FALSE, , echo=FALSE}
multi_modelm2<- lm(charges ~  age +smoker+obese +obese*smoker , data = sub)
get_regression_table(multi_modelm2) %>%
knitr::kable(
digits = 3,
caption = "\\label{tab:regtable} Summary table of the multiple linear regression model.") %>%
  kable_styling(latex_options = 'HOLD_position')

```

```{r , echo = FALSE, eval = FALSE, warning = FALSE, message = FALSE}
  step(lm(charges~age+smoker+obese+obese*smoker,data=sub),direction="backward")
```

```{r, echo = FALSE, eval = FALSE, warning = FALSE, message = FALSE}
  step(lm(charges~age+smoker+obese+obese*smoker,data=sub),direction="forward")
```

```{r , echo = FALSE, eval = FALSE, warning = FALSE, message = FALSE}
  step(lm(charges~age+smoker+obese+obese*smoker,data=sub),direction="both")
```

Hence, the best-fitting modelling is given as: 
$$\widehat{\mbox{charges}} =  -3241.6	+ 286.8 \cdot \mbox{age}+ 13108.6 \cdot \mathbb{I}_{\mbox{T}}(smoker_{i})+602.5	 \cdot \mathbb{I}_{\mbox{T}}(obese_{i}) + 20236.7 \cdot [\mathbb{I}_{\mbox{T}}(smoker_{i}) \cdot   \mathbb{I}_{\mbox{T}}(obese_{i})].$$

The positave estimated coefficients indicate the increase in yearly medical charges for one unite increase in one in the exploratory varibles when the other exploratory varibles are held constant.For exsample,The intercept represents a decrease in an average of $3241.6$ in additional medical expenses each year for baseline category (non-smoker and non-obese). Also,considering all variables in the model and keeping them constant, there is an associated increase, on average in medical charges of $286.8$ for every one year increase in the age .However,the medical charges will increase by an average of $13108.6$ for non-obese smokers, holding all else constant;But for non-smoker obese the medical charges will increase by an avrage of $602.5$ .Finally,obese smoker have on average $20236.7$ higher medical costs each year relative to non-obese and non-smoker.


## Assessing model fit
The assumptions of model need to be checked in order for the model to be an appropriate fit to the data. The scatterplots plotting the residuals against the fitted values and histograms of the residuals are considered for assumptions in Figures below.

```{r scatterplots, echo = FALSE, eval = TRUE, out.width = '70%', fig.align = "center", fig.pos = "h", warning = FALSE, fig.cap = "\\label{fig:scat} Scatterplots of the residuals by smoker and by obese."}

regression.points <- get_regression_points(multi_modelm)
regression.points %>%
  ggplot(aes(x = charges_hat, y = residual)) + 
  geom_point(color = alpha("grey10", 0.5), fill = alpha("grey10", 0.1), shape = 21) +
  labs(x = "fitted value", y = "residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1) +
  facet_wrap(~ smoker + obese, labeller = label_both)

```
For the residual-fitted value plot, most of points are located below the y=0 line,which means 
the mean of residuals are not 0,and hence the assumptions of the residuals having mean zeroacross all levels of the fitted values appear to be invalid in this case. If the assumptions hold, there is no obvious pattern in the residuals which appear to be randomly scattered above and below the zero line.

```{r histogram, echo = FALSE, eval = TRUE, warning = FALSE, out.width = '70%', fig.align = "center", fig.pos = "H", fig.cap = "\\label{fig:hist} Histograms of the residuals by smoker and by obese."}
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

Residuals are not normal distributed and the histogram shows right-skewed, which suggests that some people's individual medical charges are underestimated, hence histogram does not support the assumption of normally distributed residuals in the model. For the assumption held, the histogram should be bell-shaped and centred at zero. 

The assumption is invalid.Some method have tried to solve it(see appendix). Log-transformation is used to fit the model, but the plots also shows the assumptions of normally distributed residuals and constant variance are invalid, which did not imporve the model, so the original model is kept.

There are two possible reasons why the assumptions associated with the established model are not valid.One is that the selected observations are insufficient, resulting in inaccurate model.The other is the final model only contains limited explanary variables which is not enough, some potential variables need to be considered.


# Conclusions {#sec:Conc}
In conclusion, with all other variables unchanged, each additional year of age added an average of $286.802$ to  medical charges.$-3241.6$ is the intercept of the model.For non-obese smokers, the average medical charges estimated are $13108.6$ higher than non-smoker and non-obese group.For non-smoking obese people, the average medical charges estimated are $602.5$ higher than non-smoker and non-obese group.For obese smoker, the average medical charges estimated are $33947.8$ higher than non-smoker and non-obese group.The model shows that both smoking and obesity can make medical charges higher.

It can be observed from the boxplot that the distribution of medical charges of obese and non-obese people is different in the center(median), and the distribution of medical charges of obese people is more extensive than that of non-obese people.For the model assiumption,it is obvious from the residuals plots that the assumptions are invalid.Therefore, more variables and more observations need to be considered to fit a more reasonalble model which meet the model assumption.


***
\newpage
# Bibliography


***
\newpage
# Appendix

```{r , echo = FALSE, eval = TRUE, warning = FALSE, message = FALSE}
multi_modellog <- lm(log_charges ~ age+age2+ obese +smoker +obese*smoker , data = sub)
```



```{r, echo = FALSE, eval = TRUE, out.width = '70%', fig.align = "center", fig.pos = "h", warning = FALSE, fig.cap = "\\label{fig:scat} Scatterplots of the residuals by smoker and by obese."}

regression.points <- get_regression_points(multi_modellog)
regression.points %>%
  ggplot(aes(x = log_charges_hat, y = residual)) + 
  geom_point(color = alpha("grey10", 0.5), fill = alpha("grey10", 0.1), shape = 21) +
  labs(x = "fitted value", y = "residual") +
  geom_hline(yintercept = 0, col = "black", size = 1) +
  facet_wrap(~ smoker + obese, labeller = label_both)

```


```{r, echo = FALSE, eval = TRUE, warning = FALSE, out.width = '70%', fig.align = "center", fig.pos = "H", fig.cap = "\\label{fig:hist} Histograms of the residuals by smoker and by obese."}
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
