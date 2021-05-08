library(tidyverse)
library(data.table)
library(rstudioapi)
library(skimr)
library(inspectdf)
library(mice)
library(plotly)
library(highcharter)
library(recipes) 
library(caret) 
library(purrr) 
library(graphics) 
library(Hmisc) 
library(glue)
library(h2o) 
library(ggplot2)


raw <- read.csv("crimes.csv")
raw %>% view()
raw %>% skim()


raw %>% inspect_na()


df.num <- raw %>%
  select_if(is.numeric) %>%
  select(ViolentCrimesPerPop,everything())

df.chr <- raw %>%
  select_if(is.character)

names(raw)

# I've replaced outliers with max and min value
vect_forloop <- c()
for (n in 1:length(num)) {
  outvals <- boxplot(df[[num[n]]], plot=F)$out
  if (length(outvals)>0) {
    vect_forloop[n] <- num[n]
  }
}

vect_forloop <- vect_forloop %>% as.data.frame() %>% drop_na() %>% pull() %>% as.character()
vect_forloop %>% length()

for (o in vect_forloop) {
  outvals <- boxplot(df[[o]], plot=F)$out
  mean <- mean(df[[o]], na.rm = T)
  
  o3 <- ifelse(outvals > mean, outvals, NA) %>% na.omit()
  o1 <- ifelse(outvals < mean, outvals, NA) %>% na.omit()
  
  max3 <- quantile(df[[o]], 0.75, na.rm=T) + 1.5 * IQR(df[[o]], na.rm = T)
  df[which(df[[o]] %in% o3), o] <- max3
  
  min1 <- quantile(df[[o]], 0.25, na.rm = T) - 1.5 * IQR(df[[o]], na.rm=T)
  df[which(df[[o]] %in% o1), o] <- min1
}

#Multicollinearity

target <- 'ViolentCrimesPerPop'
features <- raw %>% select(-ViolentCrimesPerPop) %>% names()
f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = raw)


glm %>% summary()


coef_na <- attributes(alias(glm)$Complete)$dimnames[[1]]
features <- features[!features %in% coef_na]

f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = raw)

glm %>% summary()




while(glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[1] >= 1.5){
  afterVIF <- glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[-1] %>% names()
  f <- as.formula(paste(target, paste(afterVIF, collapse = " + "), sep = " ~ "))
  glm <- glm(f, data = raw)
}

glm %>% faraway::vif() %>% sort(decreasing = T) %>% names() -> features 

df <- raw %>% select(ViolentCrimesPerPop,features)

df %>% view()

df %>% glimpse()

# Standardize (Normalize) 

df[,-1] <- df[,-1] %>% scale() %>% as.data.frame()


#3. Fit Generalized Linear Model using H2O in R.

h2o.init()
h2o_data <- df %>% as.h2o()


h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]


target <- 'ViolentCrimesPerPop'
features <- df %>% select(-ViolentCrimesPerPop) %>% names()


model <- h2o.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = T )


model@model$coefficients_table %>%
  as.data.frame() %>%
  dplyr::select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) %>%
  .[-1,] %>%
  arrange(desc(p_value))


while(model@model$coefficients_table %>%
      as.data.frame() %>%
      dplyr::select(names,p_value) %>%
      mutate(p_value = round(p_value,3)) %>%
      .[-1,] %>%
      arrange(desc(p_value)) %>%
      .[1,2] > 0.05) {
  model@model$coefficients_table %>%
    as.data.frame() %>%
    dplyr::select(names,p_value) %>%
    mutate(p_value = round(p_value,3)) %>%
    filter(!is.nan(p_value)) %>%
    .[-1,] %>%
    arrange(desc(p_value)) %>%
    .[1,1] -> v
  features <- features[features!=v]
  
  train_h2o <- train %>% as.data.frame() %>% select(target,features) %>% as.h2o()
  test_h2o <- test %>% as.data.frame() %>% select(target,features) %>% as.h2o()
  
  model <- h2o.glm(
    x = features, y = target,
    training_frame = train,
    validation_frame = test,
    nfolds = 10, seed = 123,
    lambda = 0, compute_p_values = T)
}


model@model$coefficients_table %>%
  as.data.frame() %>%
  dplyr::select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) 

# Predicting the Test set results ----

y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
y_pred$predict

# Model evaluation
test_set <- test %>% as.data.frame()
residuals = test_set$ViolentCrimesPerPop - y_pred$predict

# RMSE (Root Mean Square Error) ----
RMSE = sqrt(mean(residuals^2))

# Adjusted R2 (R Squared) ----
y_test_mean = mean(test_set$ViolentCrimesPerPop)

tss = sum((test_set$ViolentCrimesPerPop - y_test_mean)^2) #total sum of squares
rss = sum(residuals^2) #residual sum of squares

R2 = 1 - (rss/tss); R2

n <- test_set %>% nrow() #sample size
k <- features %>% length() #number of independent variables
Adjusted_R2 = 1-(1-R2)*((n-1)/(n-k-1))

tibble(RMSE = round(RMSE,1), Adjusted_R2)



# Plotting actual & predicted

my_data <- cbind(predicted = y_pred$predict,
                 observed = test_set$ViolentCrimesPerPop) %>% 
  as.data.frame()

g <- my_data %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method=lm) + 
  labs(x="Predecited Power Output", 
       y="Observed Power Output",
       title=glue('Test: Adjusted R2 = {round(enexpr(Adjusted_R2),2)}')) +
  theme(plot.title = element_text(color="darkgreen",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

g %>% ggplotly()


# Check overfitting

y_pred_train <- model %>% h2o.predict(newdata = train) %>% as.data.frame()

train_set <- train %>% as.data.frame()
residuals = train_set$ViolentCrimesPerPop - y_pred_train$predict

RMSE_train = sqrt(mean(residuals^2))
y_train_mean = mean(train_set$ViolentCrimesPerPop)

tss = sum((train_set$ViolentCrimesPerPop - y_train_mean)^2)
rss = sum(residuals^2)

R2_train = 1 - (rss/tss); R2_train

n <- train_set %>% nrow() #sample size
k <- features %>% length() #number of independent variables
Adjusted_R2_train = 1-(1-R2_train)*((n-1)/(n-k-1))



my_data_train <- cbind(predicted = y_pred_train$predict,
                       observed = train_set$ViolentCrimesPerPop) %>% 
  as.data.frame()

g_train <- my_data_train %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method=lm) + 
  labs(x="Predecited Power Output", 
       y="Observed Power Output",
       title=glue('Train: Adjusted R2 = {round(enexpr(Adjusted_R2_train),2)}')) +
  theme(plot.title = element_text(color="darkgreen",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

g_train %>% ggplotly()


# Compare 
library(patchwork)
g_train + g

tibble(RMSE_train = round(RMSE_train,1),
       RMSE_test = round(RMSE,1),
       
       Adjusted_R2_train,
       Adjusted_R2_test = Adjusted_R2)

