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

#Q1. Add ggplot2::mpg dataset.
raw <- ggplot2::mpg
raw %>% view()

#Q2. Make data ready for analysis doing preprocessing techniques.
raw %>% skim()

#inspecting NA. We don't have any NA values
raw %>% inspect_na()

#We will now split our variables into numerical and categorical to get dummies for categorical features.
df.num <- raw %>%
  select_if(is.numeric)

df.chr <- raw %>%
  select_if(is.character)

df.chr <- dummyVars(" ~ .", data = df.chr) %>% 
  predict(newdata = df.chr) %>% 
  as.data.frame()

#We have  created dummies. Now we will create a united dataframe. We are putting the label (cty) as the first column.
df <- cbind(df.chr,df.num) %>%
  select(cty,everything())
df %>% view()
#Changing the column names to remove spaces 
names(df) <- names(df) %>% 
  str_replace_all(" ","_") %>%
  str_replace_all("-","_") %>%
  str_replace_all("\\(","") %>% 
  str_replace_all("\\)","") %>% 
  str_replace_all("\\'","")

#Q3. Fit Generalized Linear Model using H2O in R.

#We will first fit the generalized linear model, meaning that we will use all the columns that we have to predict cty.
target <- 'cty'
#Selecting all columns except cty
features <- df %>% select(-cty) %>% names()

f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = df)

#Removing aliases
coef_na <- attributes(alias(glm)$Complete)$dimnames[[1]]
features <- features[!features %in% coef_na]
f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = df)

#Removing multicolinear features
while(glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[1] >= 1.5){
  afterVIF <- glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[-1] %>% names()
  f <- as.formula(paste(target, paste(afterVIF, collapse = " + "), sep = " ~ "))
  glm <- glm(f, data = df)
}
glm %>% faraway::vif() %>% sort(decreasing = T) %>% names() -> features 
df <- df %>% select(cty,features)

#Scaling the data
df[,-1] <- df[,-1] %>% scale() %>% as.data.frame()

#Splitting the data
h2o.init()
h2o_data <- df %>% as.h2o()
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]
target <- 'cty'
features <- df %>% select(-cty) %>% names()

#Fitting the data into the model
model <- h2o.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = T)

#Doing the stepwise backward elimination and retraining the model
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

#We have now removed all the columns for which the p-value is higher than 0.05
#Our model had 40 features before backward elimination and now it has 25 features left.
model@model$coefficients_table %>%
  as.data.frame() %>%
  dplyr::select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) 

#Predicting the results
y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()

y_pred_train <- model %>% h2o.predict(newdata = train) %>% as.data.frame()

train_set <- train %>% as.data.frame()
test_set <- test %>% as.data.frame()

features_gen <- features






#Q4. Run GLM using following modelling structure. cty ~ year + cyl + displ.

#We will preprocess the data from the beginning
raw <- ggplot2::mpg

df.num <- raw %>%
  select_if(is.numeric)

df.chr <- raw %>%
  select_if(is.character)

df.chr <- dummyVars(" ~ .", data = df.chr) %>% 
  predict(newdata = df.chr) %>% 
  as.data.frame()

df <- cbind(df.chr,df.num) %>%
  select(cty,everything())

names(df) <- names(df) %>% 
  str_replace_all(" ","_") %>%
  str_replace_all("-","_") %>%
  str_replace_all("\\(","") %>% 
  str_replace_all("\\)","") %>% 
  str_replace_all("\\'","")

target <- 'cty'
#Here we select the features year, cyl, displ to create a model of a type cty ~ year + cyl + displ.
features <- df %>% select(year,cyl,displ) %>% names()

f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = df)

coef_na <- attributes(alias(glm)$Complete)$dimnames[[1]]
features <- features[!features %in% coef_na]
f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = df)
df <- df %>% select(cty,features)
f

df[,-1] <- df[,-1] %>% scale() %>% as.data.frame()

h2o.init()
h2o_data <- df %>% as.h2o()
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]
target <- 'cty'
features <- df %>% select(-cty) %>% names()


model <- h2o.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = T)


#We have not done VIF removal or stepwise backward elimination in this case because we are asked to train the model using 
#The features cyl, year, displ

#Defining the predictions of the second model

y_pred2 <- model %>% h2o.predict(newdata = test) %>% as.data.frame()

y_pred_train2 <- model %>% h2o.predict(newdata = train) %>% as.data.frame()

train_set2 <- train %>% as.data.frame()
test_set2 <- test %>% as.data.frame()


#Q5. Print coefficients table and give interpretation of results.

#We can see that the year variable has significance more than 0.05, so it might be a good idea to remove it.
model@model$coefficients_table %>%
  as.data.frame() %>%
  dplyr::select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) 

#We will now evaluate our models. First we start with the generalized model

residuals = test_set$cty - y_pred$predict
#Evaluating the generalized model
RMSE = sqrt(mean(residuals^2))

# Calculate Adjusted R2 (R Squared) ----
y_test_mean = mean(test_set$cty)

tss = sum((test_set$cty - y_test_mean)^2) #total sum of squares
rss = sum(residuals^2) #residual sum of squares

R2 = 1 - (rss/tss); R2

n <- test_set %>% nrow() #sample size
k <- features_gen %>% length() #number of independent variables
Adjusted_R2 = 1-(1-R2)*((n-1)/(n-k-1))

#Now we will evaluate the model with 3 features: cyl, year, displ
residuals2 = test_set2$cty - y_pred2$predict

RMSE2 = sqrt(mean(residuals2^2))

# Calculate Adjusted R2 (R Squared) ----
y_test_mean2 = mean(test_set2$cty)

tss2 = sum((test_set2$cty - y_test_mean2)^2) #total sum of squares
rss2 = sum(residuals2^2) #residual sum of squares

R2_2 = 1 - (rss2/tss2); R2_2

n2 <- test_set2 %>% nrow() #sample size
k2 <- features %>% length() #number of independent variables
Adjusted_R2_2 = 1-(1-R2_2)*((n2-1)/(n2-k2-1))

#We can see that the r squared is slightly higher and RMSE is slightly lower for the generalized model that uses all the features
#However, the adusted r squared is almost 2 times higher for the second model, meaning that the second model uses only the
#most important features and therefore it is the better model in this case. 
tibble(RMSE = round(RMSE,1),
       R2, Adjusted_R2)
tibble(RMSE2 = round(RMSE2,1),
       R2_2, Adjusted_R2_2)

#Plotting the residuals vs predicted values for the first and second model
my_data <- cbind(predicted = y_pred$predict,
                 observed = test_set$cty) %>% 
  as.data.frame()

p1 <- my_data %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method=lm) + 
  labs(x="Predecited Output", 
       y="Observed Output",
       title=glue('Generalized model: Adjusted R2 = {round(enexpr(Adjusted_R2),2)}')) +
  theme(plot.title = element_text(color="darkgreen",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

my_data2 <- cbind(predicted = y_pred2$predict,
                 observed = test_set2$cty) %>% 
  as.data.frame()

p2 <- my_data2 %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method=lm) + 
  labs(x="Predecited Output", 
       y="Observed Output",
       title=glue('Model with 3 columns: Adjusted R2 = {round(enexpr(Adjusted_R2_2),2)}')) +
  theme(plot.title = element_text(color="darkgreen",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

library(patchwork)

p1 / p2


#We will now check for overfitting for the second model
residuals2 = train_set2$cty - y_pred_train2$predict

RMSE_train2 = sqrt(mean(residuals2^2))
y_train_mean2 = mean(train_set2$cty)

tss2 = sum((train_set2$cty - y_train_mean2)^2)
rss2 = sum(residuals2^2)

R2_train2 = 1 - (rss2/tss2); R2_train2

n2 <- train_set2 %>% nrow() #sample size
k2 <- features %>% length() #number of independent variables
Adjusted_R2_train2 = 1-(1-R2_train2)*((n2-1)/(n2-k2-1))

#Plotting the results for train data
my_data_train <- cbind(predicted = y_pred_train2$predict,
                       observed = train_set2$cty) %>% 
  as.data.frame()

g_train <- my_data_train %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method=lm) + 
  labs(x="Predecited Output", 
       y="Observed Output",
       title=glue('Model with 3 columns (Train): Adjusted R2 = {round(enexpr(Adjusted_R2_train2),2)}')) +
  theme(plot.title = element_text(color="darkgreen",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

#Comparing the plots for training and testing data
g_train / p2

#We can see that RMSE is only slightly lower for train data than for test data. Adjusted R squared is even higher for test data
#which is good. There is definetily no overfitting here.
tibble(RMSE_train = round(RMSE_train2,1),
       RMSE_test = round(RMSE2,1),
       
       Adjusted_R2_train2,
       Adjusted_R2_test = Adjusted_R2_2)
