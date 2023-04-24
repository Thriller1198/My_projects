library(tidyverse)
data<-read_csv('facies_data.csv')

sum_missing<-sum(is.na(data))
print(sum_missing)
head(data)
duplicates<-sum(duplicated(data))
print(duplicates)

df<-unique(data)

nrow(data)
nrow(df)
colnames(df)[3]<-"Well_Name"

print(unique(df$Formation))
print(unique(df$Facies))
print(unique(df$`Well_Name`))

ggplot(df,aes(x=`Well_Name`,y=Facies,))+geom_violin()
#I do not see much ,that is perculiar about the violins, except for Recruit F9.
ggplot(df,aes(x=`Well_Name`,y=Facies))+geom_boxplot()
#Well Recruit F9 only had the facies  9

install.packages(c("ggpubr","gridExtra"))
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install DataExplorer package from GitHub
devtools::install_github("boxuancui/DataExplorer")

library(DataExplorer)

create_report(df)

df$Facies<-as.factor(df$Facies)
df$Formation<-as.factor(df$Formation)
df$Well_Name<-as.factor(df$Well_Name)
library(tidymodels)
library(randomForest)

set.seed(123)
df_split<-initial_split(df,strata = Facies)
df_train<-training(df_split)
df_test<-testing(df_split)

df_recipe<-recipe(Facies~.,data = df)%>%
  step_scale(all_numeric_predictors())

df_prep<-prep(df_recipe)
juiced<-juice(df_prep)

model_spec<-rand_forest(mtry = tune(),
                        trees = 1000,
                        min_n = tune()
                        )%>%set_mode("classification")%>%set_engine("ranger")


wf<-workflow()%>%add_recipe(df_recipe)%>%add_model(model_spec) 


set.seed(234)
folds<-vfold_cv(df_train)

doParallel::registerDoParallel()
set.seed(345)
tune_res<-tune_grid(wf,resamples = folds,grid = 20)


metrics<-tune_res%>%collect_metrics()
best_metrics<-tune_res%>%select_best("accuracy")


finalize_model<-finalize_model(model_spec,best_metrics)

final_wf<-workflow()%>%add_recipe(df_recipe)%>%add_model(finalize_model)

final_fit<-final_wf%>%last_fit(df_split)
final_fit%>%collect_metrics()

predictions<-final_fit%>%collect_predictions()
