'''
This document uses a stroke dataset to find factos that are siginificantly 
associated with stroke. 
Methods: Logsitic Regression, AIC Model Selection, Hypothesis Testing 
Conclusion: age, average glucose level, hypertension, and heart disease are all postively related to 
the probability of having stroke at alpha = 5% level. 
For prediction results, refer to the python code. 
'''

stroke = read.table("/Users/mercuryliu/Downloads/healthcare-dataset-stroke-data 2.csv",sep=",", header = TRUE)
#delete missing values
na.omit(stroke)

#delete the "id" column
df <- stroke[ -c(1) ]

#transform categorical variable

str(df)

df$gender <- as.numeric(as.factor(df$gender))
df$ever_married <- as.numeric(as.factor(df$ever_married))
df$work_type <- as.numeric(as.factor(df$work_type))
df$Residence_type <- as.numeric(as.factor(df$Residence_type))
df$smoking_status <- as.numeric(as.factor(df$smoking_status))
#df$stroke <- as.factor(df$stroke)
df$bmi <-as.numeric(df$bmi)
#delete missing values
na.omit(df)


#test train data split 
#install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret)
set.seed(3456)
trainIndex <- createDataPartition(df$stroke, p = .7,
                                  list = FALSE,
                                  times = 1)
Train <- df[ trainIndex,]
Valid <- df[-trainIndex,]

#build a general logistic regression model
glm1 <- glm(stroke ~ . , family = binomial(link="logit"), data = Train)
summary(glm1)

#model selection with AIC
#need to define the smallest model, and the largest model, then apply aic function
initial = glm(stroke ~ 1, family = binomial(link = logit), data = Train)
final <- glm(stroke ~ . , family = binomial(link = logit), data = Train)
steplog = step(initial, scope = list(lower = initial, upper = final), direction = "both")
summary(steplog)


