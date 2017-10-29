#predict home prices in Ames, Iowa using multiple linear regression
library(mice)

#setwd(select the folder which contains train.csv)
dataset = read.csv('train.csv')

#pick couple of relevant features
houseprice = data.frame(dataset$LotArea,dataset$BldgType,
                        dataset$OverallQual, dataset$FullBath,
                        dataset$SalePrice)

colnames(houseprice) = c('LotArea', 'BldgType', 'OverallQual',
                         'FullBath', 'SalePrice')

#check the modeling type of features
str(houseprice)
houseprice$BldgType = factor(houseprice$BldgType,
                             levels = c('1Fam','2fmCon','Duplex','Twnhs','TwnhsE'),
                             labels = c(1,2,3,4,5))

#check for missing data
md.pattern(houseprice)

#scale continuous features
houseprice[,c(1,3,4)] = scale(houseprice[,c(1,3,4)])

#split the data into training and test set
split = sample.split(Y = houseprice$SalePrice,
                     SplitRatio = 0.9)
training_set = subset(houseprice, split == TRUE)
test_set = subset(houseprice, split == FALSE)

#build the multiple linear regression using training set
houseprice_multireg = lm(formula = SalePrice ~ .,
                         data = training_set)
summary(houseprice_multireg)

#predict the house prices of the test set
y_pred = predict(houseprice_multireg, 
        newdata = test_set[,-5])
y_pred
