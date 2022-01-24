## ----setup, include=FALSE-----------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = NA, warning = FALSE, message = FALSE, fig.align = "center")
library(tidyverse)


## -----------------------------------------------------------
nyb <- read.csv("New_York_bridges_2016.csv")
dim(nyb)
tcb <- read.csv("Tompkins_county_bridges_2016.csv")
names(tcb)
TCB <- nyb %>% 
  filter(County == "Tompkins") %>% 
  select(-County, -Manhattan)


## -----------------------------------------------------------
ggplot(data = tcb, aes(x = AgeAtInspection, y = Condition)) + 
  geom_point(color = "green4") + 
  theme_bw() + 
  labs(x = "Age At Inspection", title = "Safety score versus age at inspection") + 
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.5)


## -----------------------------------------------------------
tcb %>% 
  summarize(xbar = mean(AgeAtInspection), 
            sx = sd(AgeAtInspection),
            ybar = mean(Condition),
            sy = sd(Condition),
            r = cor(AgeAtInspection, Condition),
            b1 = r*sy/sx,
            b0 = ybar - b1*xbar)
# or
mod_tcb <- lm(Condition ~ AgeAtInspection, data = tcb)
summary(mod_tcb)
mod_tcb$coefficients


## -----------------------------------------------------------
# Take 1000 random samples of size 195
set.seed(3)
n <- 1000
b1 <- numeric(n)
for(i in 1:n){
DF <- sample_n(nyb, size = 195, replace = FALSE)
mod <- lm(Condition ~ AgeAtInspection, data = DF)
b1[i] <- mod$coefficients[2]
}
ep <- quantile(b1, probs = c(0.025, 0.975))
ep


## -----------------------------------------------------------
data1 = data.frame(b1 = b1) # create data frame to use with ggplot
ggplot(data = data1, aes(x = b1)) + 
  geom_histogram(binwidth = 0.0005, fill = "lightblue", color = "black") + 
  geom_histogram(data = subset(data1, b1 > ep[1] & b1 < ep[2]), binwidth = 0.0005, 
   color = "black", fill = "orange") +
  theme_bw() + 
  geom_vline(xintercept = ep, lty = "dashed") + 
  labs(x = expression(b[1]))


## -----------------------------------------------------------
# Your Code Goes HERE
bki <- read.csv("Burger_King_items.csv")
knitr::kable(head(bki))
library(janitor)
bki <- bki %>% 
  clean_names()
knitr::kable(head(bki))


## -----------------------------------------------------------
# Your Code Goes HERE
ggplot(data = bki, aes(x = protein_g, y = fat_g)) + 
  geom_point(color = "blue") + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(x = "Protein (g)", y = "Fat (g)")


## -----------------------------------------------------------
# Your Code Goes HERE
mod_bk <- lm(fat_g ~ protein_g, data = bki)
summary(mod_bk)
coefficients(summary(mod_bk))
mod_bk$coefficients -> lse
lse


## -----------------------------------------------------------
e <- residuals(mod_bk)
fat <- bki$fat_g - mean(bki$fat_g)
values <- c(e, fat)
type <- rep(c("Residuals", "Fat"), each = 122)
NDF <- data.frame(type = type, values = values)
ggplot(data = NDF, aes(x = type, y = values, fill = type)) + 
  geom_boxplot() + 
  theme_bw() + 
  scale_fill_manual(values = c("green", "purple"))

## -----------------------------------------------------------
1 - var(e)/var(fat)


## -----------------------------------------------------------
# Your Code Goes HERE
Cereals <- read.csv("Cereals.csv")
knitr::kable(head(Cereals))


## -----------------------------------------------------------
# Your Code Goes HERE
mod_cereal <- lm(calories ~ sugars, data = Cereals)
mod_cereal$coefficients -> lse
lse


## -----------------------------------------------------------
mod_cereal <- lm(calories ~ sugars, data = Cereals)
library(broom)
NS <- augment(mod_cereal)
ggplot(data = NS, aes(x = .fitted, y = .resid)) + 
  geom_point(color = "blue") + 
  theme_bw() + 
  labs(x = "Predicted Calories", y = "Residuals") + 
  geom_hline(yintercept = 0, lty = "dashed")


## -----------------------------------------------------------
library(readxl)
chap07ques04 <- read_xlsx("chap07ques04.xlsx")
chap07ques04 <- chap07ques04 %>% 
  clean_names()
knitr::kable(chap07ques04)
mod_lm <- lm(sales_in_1000 ~ number_of_sales_people_working, data = chap07ques04)
summary(mod_lm)
predict(mod_lm, newdata = data.frame(number_of_sales_people_working = 18))

