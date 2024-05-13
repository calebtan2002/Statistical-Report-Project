diabetes = read.csv("diabetes_5050.csv", header = TRUE)
head(diabetes)
dim(diabetes)
attach(diabetes)
set.seed(1101)

#GenHlth, Age, Education and Income are ordinal input variables
for (col_name in names(diabetes)[c(1,2,3,4,6,7,8,9,10,11,12,13,14,18,19)]) {
  diabetes[[col_name]] <- as.factor(diabetes[[col_name]])
}
colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
            "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6",
            "#ffff99", "#666666", "#8dd3c7", "#fb8072", "#80b1d3",
            "#bebada", "#ffed6f", "#fdb462", "#b3de69", "#fccde5")


table_genhlth = table(GenHlth, Diabetes_binary)
prop_table_genhlth = table(GenHlth, Diabetes_binary)/rowSums(table_genhlth)

table_Age = table(Age, Diabetes_binary)
prop_table_Age = table(Age, Diabetes_binary)/rowSums(table_Age)

table_Education = table(Education, Diabetes_binary)
prop_table_Education = table(Education, Diabetes_binary)/rowSums(table_Education)

table_Income = table(Income, Diabetes_binary)
prop_table_Income = table(Income, Diabetes_binary)/rowSums(table_Income)

barplot(prop_table_genhlth, beside = TRUE,
        col = colors[10:16],
        legend = rownames(table(diabetes$GenHlth, diabetes$Diabetes_binary)),
        main = "Proportion of people of different GenHlth levels who got diabetes or not",
        xlab = "Diabetes (1 = Yes and 0 = No)", ylab = "Proportion")

barplot(prop_table_Age, beside = TRUE,
        col = colors[1:13],
        legend = rownames(table(diabetes$Age, diabetes$Diabetes_binary)), args.legend = list(x = "top", bty = "n"),
        main = "Proportion of people of different Age levels who got diabetes or not",
        xlab = "Diabetes (1 = Yes and 0 = No)", ylab = "Proportion")

barplot(prop_table_Education, beside = TRUE,
        col = colors[1:6],
        legend = rownames(table(diabetes$Education, diabetes$Diabetes_binary)),
        main = "Proportion of people of different Education levels who got diabetes or not",
        xlab = "Diabetes (1 = Yes and 0 = No)", ylab = "Proportion")

barplot(prop_table_Income, beside = TRUE,
        col = colors[1:8],
        legend = rownames(table(diabetes$Income, diabetes$Diabetes_binary)),
        main = "Proportion of people of different Income levels who got diabetes or not",
        xlab = "Diabetes (1 = Yes and 0 = No)", ylab = "Proportion")

#It seems that there is a strong relationship between GenHlth, Age, Education, Income levels and Diabetes binary.
#As  GenHlth, Age, Education, Income levels increase, proportion of people getting diabetes increases and vice versa

par(mfrow = c(3, 1))

table_BMI = table(BMI, Diabetes_binary)
prop_table_BMI = t(table(BMI, Diabetes_binary)/rowSums(table_BMI))

table_PhysHlth = table(PhysHlth, Diabetes_binary)
prop_table_PhysHlth = t(table(PhysHlth, Diabetes_binary)/rowSums(table_PhysHlth))

table_MentHlth = table(MentHlth, Diabetes_binary)
prop_table_MentHlth = t(table(MentHlth, Diabetes_binary)/rowSums(table_MentHlth))

barplot(prop_table_BMI, beside = TRUE,
        col = c("red", "blue"),
        legend = rownames(table(diabetes$Diabetes_binary, diabetes$BMI)),
        main = "Proportion of people of different BMI who got diabetes or not",
        xlab = "BMI", ylab = "Proportion")

barplot(prop_table_PhysHlth, beside = TRUE,
        col = c("orange", "yellow"),
        legend = rownames(table(diabetes$Diabetes_binary, diabetes$PhysHlth)),
        main = "Proportion of people of different PhysHlth who got diabetes or not",
        xlab = "PhysHlth", ylab = "Proportion")

barplot(prop_table_MentHlth, beside = TRUE,
        col = c("green", "red"),
        legend = rownames(table(diabetes$Diabetes_binary, diabetes$MentHlth)),
        main = "Proportion of people of different MentHlth who got diabetes or not",
        xlab = "MentHlth", ylab = "Proportion")

#It seems that there is no clear correlation between people with different PhysHealth, MentHealth and Diabetes_binary
#There is a positive linear relationship between BMI and proportion of people getting diabetes



#I will exclude PhysHealth, MentHealth from the dataset

M1<- glm( Diabetes_binary ~., data =diabetes,family = binomial)
summary(M1)

#It seems that AnyHealthcare, NoDocbcCost, Fruits, PhysActivity, Smoker are not significant so I will exclude them in dataset
diabetes = diabetes[,c(-6,-9,-10,-14,-13,-16,-17)]
diabetes = diabetes[order(Diabetes_binary),]
table(Diabetes_binary)
n_folds=5 

folds_j_1 <- sample(rep(1:n_folds, length.out = 35346 ))  # for diabetic

folds_j_2 <- sample(rep(1:n_folds, length.out = 35346 ))  # for non-diabetic
data1 = diabetes[1:35346,] # data for for diabetic
data2 = diabetes[35347:70692,] # data for non-diabetic

#######Decision Trees########

acc=numeric(n_folds)
type2err = numeric(n_folds)

j= 1

for (j in 1:n_folds) {
  
  test1 <- which(folds_j_1 == j)
  test2 <- which(folds_j_2 == j)
  
  train.1=data1[ -test1, ]
  train.2=data2[ -test2, ]
  
  train = rbind(train.1, train.2)
  
  test = rbind(data1[test1,], data2[test2,])
  
  fit.diabetes <- rpart(Diabetes_binary ~ .,
                    method = "class", data =train, control = rpart.control( minsplit =1000),
                    parms = list( split ='gini'))
  
  
  pred = predict(fit.diabetes, newdata = test[,2:15], type = 'class')
  
  confusion.matrix = table(pred, test[,1])
  
  acc[j] = sum(diag(confusion.matrix))/sum(confusion.matrix)
  type2err[j] = confusion.matrix[1,2]/sum(confusion.matrix[1,])
  
}
acc
mean(acc) #for decision trees, mean is 0.726
type2err
mean(type2err) # mean type 2 error rate is 0.233


#####KNN#######

K = 50

ave_acc_knn = numeric(K)
acc_knn = numeric(n_folds)

ave.type2 = numeric(K)
type2=numeric(n_folds)

for(i in 1:K){
  
  for (j in 1:n_folds) {
    
    test1 <- which(folds_j_1 == j)
    test2 <- which(folds_j_2 == j)
    
    train.1=data1[ -test1, ]
    train.2=data2[ -test2, ]
    
    train = rbind(train.1, train.2)
    
    test = rbind(data1[test1,], data2[test2,])
    knn.pred <- knn(train = train[,2:15], test = test[,2:15], cl=train[,1], k=i) 
    confusion.matrix=table(knn.pred, test[,1]) 
    
    acc_knn[j]= sum(diag(confusion.matrix))/sum(confusion.matrix) 
    type2[j]=confusion.matrix[1,2]/sum(confusion.matrix[1,])
  }
  ave_acc_knn[i] = round(mean(acc_knn), digits = 3)
  ave.type2[i] = round(mean(type2), digits = 3)
  
}

ave_acc_knn
best_k1 = which(ave_acc_knn == max(ave_acc_knn));best_k1
ave.type2
best_k2 = which(ave.type2 == min(ave.type2));best_k2
#best K value is 46 with 0.744 accuracy rate and 0.230 type 2 error rate


#####NaiveBayes#####

library(e1071)

acc_naivebayes=numeric(n_folds)
type2err_naivebayes = numeric(n_folds)

j= 1

for (j in 1:n_folds) {
  
  test1 <- which(folds_j_1 == j)
  test2 <- which(folds_j_2 == j)
  
  train.1=data1[ -test1, ]
  train.2=data2[ -test2, ]
  
  train = rbind(train.1, train.2)
  
  test = rbind(data1[test1,], data2[test2,])
  
  model <- naiveBayes(Diabetes_binary ~ ., train)
  
  results <- predict(model,test[,2:15],"class")
  
  confusion.matrix = table(results, test[,1])
  
  acc_naivebayes[j] = sum(diag(confusion.matrix))/sum(confusion.matrix)
  type2err_naivebayes[j] = confusion.matrix[1,2]/sum(confusion.matrix[1,])
  
}
acc_naivebayes
mean(acc_naivebayes) #for NaiveBayes, mean acc is 0.736
type2err_naivebayes
mean(type2err_naivebayes) #mean type 2 error rate is 0.266

#####Logistic Regression#####

acc_logistic=numeric(n_folds)
type2err_logistic = numeric(n_folds)

j= 1

for (j in 1:n_folds) {
  
  test1 <- which(folds_j_1 == j)
  test2 <- which(folds_j_2 == j)
  
  train.1=data1[ -test1, ]
  train.2=data2[ -test2, ]
  
  train = rbind(train.1, train.2)
  
  test = rbind(data1[test1,], data2[test2,])
  
  M1<- glm( Diabetes_binary ~.,
            data =train,family = binomial(link ="logit"))
  
  result = predict(M1, newdata = test[,2:15], type = 'response')
  result = ifelse(result > 0.5, 1, 0)
  confusion.matrix = table(result, test[,1])
  
  acc_logistic[j] = sum(diag(confusion.matrix))/sum(confusion.matrix)
  type2err_logistic[j] = confusion.matrix[1,2]/sum(confusion.matrix[1,])
  
}
acc_logistic
mean(acc_logistic) #for Logistic Regression, mean acc is 0.748
type2err_logistic
mean(type2err_logistic) #mean type 2 error rate is 0.242

#It seems that Logistic regression, knn and decision trees have the best accuracy and lowest type 2 error rate

###Now, I'll compare the classifiers using AUC#####

library(ROCR)

### AUC for Decision Trees #####
auc_value = numeric(n_folds)

j= 1

for (j in 1:n_folds) {
  
  test1 <- which(folds_j_1 == j)
  test2 <- which(folds_j_2 == j)
  
  train.1=data1[ -test1, ]
  train.2=data2[ -test2, ]
  
  train = rbind(train.1, train.2)
  
  test = rbind(data1[test1,], data2[test2,])
  
  fit.diabetes <- rpart(Diabetes_binary ~ .,
                        method = "class", data =train, control = rpart.control( minsplit =1000),
                        parms = list( split ='gini'))
  
  
  pred.diabetes = predict(fit.diabetes, test[,2:15], type = 'class')
  pred.diabetes= as.numeric(paste(pred.diabetes))
  pred_dt = prediction(pred.diabetes, test[,1])
  auc1 = performance(pred_dt , measure ="auc")
  auc_value[j] = auc1@y.values[[1]] 
}

auc_value
mean(auc_value) #0.726


### AUC for Logistic Regression #####
auc_value1 = numeric(n_folds)

j= 1

for (j in 1:n_folds) {
  
  test1 <- which(folds_j_1 == j)
  test2 <- which(folds_j_2 == j)
  
  train.1=data1[ -test1, ]
  train.2=data2[ -test2, ]
  
  train = rbind(train.1, train.2)
  
  test = rbind(data1[test1,], data2[test2,])
  
  M1<- glm( Diabetes_binary ~.,
            data =train,family = binomial(link ="logit"))
  
  result = predict(M1, newdata = test[,2:15], type = 'response')
  result = ifelse(result > 0.5, 1, 0)
  pred_dt = prediction(result, test[,1])
  auc1 = performance(pred_dt , measure ="auc")
  auc_value1[j] = auc1@y.values[[1]] 
}

auc_value1
mean(auc_value1) #0.748

####AUC for KNN######
auc_value2 = numeric(n_folds)

j= 1

for (j in 1:n_folds) {
  
  test1 <- which(folds_j_1 == j)
  test2 <- which(folds_j_2 == j)
  
  train.1=data1[ -test1, ]
  train.2=data2[ -test2, ]
  
  train = rbind(train.1, train.2)
  
  test = rbind(data1[test1,], data2[test2,])
  
  knn.pred <- knn(train = train[,2:15], test = test[,2:15], cl=train[,1], k=46) 
  knn.pred= as.numeric(paste(knn.pred))
  pred_dt = prediction(knn.pred, test[,1])
  auc1 = performance(pred_dt , measure ="auc")
  auc_value2[j] = auc1@y.values[[1]] 
}

auc_value2
mean(auc_value2) #0.743
