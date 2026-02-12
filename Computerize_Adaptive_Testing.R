#Read Data
install.packages("readxl")
library(readxl)
df <- read_excel("data madya fix.xlsx")
head(df)

#Check data
anyNA(df)

#Prepare data
resp <- df[, 2:101]
str(resp)
head(resp)
dim(resp)

#Data Analysis for create item difficulty as an item bank. Package eRm used it.
install.packages("eRm")
library(eRm)
modRM <- RM(resp)

#Check convergence to ensure that the model parameters are successfully estimated.
#If the value is 1, the estimation has converged and the data can be used;
#if the value is 0, the estimation has not converged and the data should not be used.
modRM$conv

#-----------------------------Computerized Adaptive Test------------------------
library(catR)

#Preparation
##Create item bank for dichotomus and 1Parameters Logistic
parameterCAT <- (-modRM$betapar)
item_par <- genDichoMatrix(items = length(parameterCAT), model = "1PL")
item_par$b <- parameterCAT
bank <- item_par
bank

#Schema for simulation cat
start <-list(nrItems=1,theta=0)
test<-list(itemSelect="MFI",method="ML")
stop<-list(rule="precision", thr=0.3)
final <-list(method="ML")

## Create theta responden from the data
PR <- person.parameter(modRM)
thetas_true_data <- PR$theta.table$`Person Parameter`
thetas_true_data
str(thetas_true_data)

## Spesification to decide result ability person
hist(thetas_true_data)
summary(thetas_true_data)
sd(thetas_true_data)
mean_theta <- mean(thetas_true_data)
sd_theta   <- sd(thetas_true_data)
cut_high       <- mean_theta + sd_theta
cut_very_high  <- mean_theta + 2 * sd_theta
cut_norm <- mean_theta
cut_low <- mean_theta - sd_theta
cut_very_low <- mean_theta - 2 * sd_theta
criterion <- c(cut_high, cut_very_high, cut_norm, cut_low, cut_very_low)

# For see what this instrument appropriate to do CAT
# Analysis use theta from data
result <- simulateRespondents(thetas = thetas_true_data, itemBank = bank, 
                               model = NULL, start = start, test = test, 
                               stop = stop, final = final)
result
plot(result)

# For see what this instrument appropriate to do CAT
# Analysis use response pattern (post-hoc)
result1 <- simulateRespondents(thetas = thetas_true_data, 
                               responsesMatrix = resp,
                               itemBank = bank, model = NULL, start = start,
                               test = test, stop = stop, final = final)
result1
plot(result1)

# Example1: if the theta came from second partisipant and stopping 0.3
# other hand, max test length diset 66, its because based on result 1

stop1 <- list(rule = c("precision", "length"), thr =c(0.3, 66))
result2 <- randomCAT(trueTheta = thetas_true_data[2],itemBank=bank,
                   model=NULL, genSeed = 123,
                start=start,test=test,stop=stop1,final=final)
result2
plot(result2, classThr = cut_high, trueTh = TRUE)

# Example2: if the respond came from actual data (post-hoc)
x_true_data <- resp[2,] # response data from second respondent
x_true_data
result3 <- randomCAT(itemBank = bank, responses = as.matrix(x_true_data),
                  model = NULL, genSeed = 123,
                  start = start, test = test, stop = stop1,
                  final = final)
result3
plot(result3)
plot(result2, classThr = criterion)

# Example3: if theta came from higher partisipant
result4 <- randomCAT(trueTheta = max(thetas_true_data),itemBank=bank,
                     model=NULL, genSeed = 123,
                     start=start,test=test,stop=stop1,final=final)
result4
plot(result4, classThr = criterion, trueTh = TRUE)

# Example4: if the respond came from actual data (post-hoc) from higher ability
x_true_data_higher <- resp[339,] # response data from higher respondent
x_true_data_higher
result5 <- randomCAT(itemBank = bank, responses = as.matrix(x_true_data_higher),
                     model = NULL, genSeed = 123,
                     start = start, test = test, stop = stop1,
                     final = final)
result5
plot(result5)
plot(result5, classThr = criterion, trueTh = TRUE)

# Example5: if theta came from lower partisipant
result6 <- randomCAT(trueTheta = min(thetas_true_data),itemBank=bank,
                     model=NULL, genSeed = 123,
                     start=start,test=test,stop=stop1,final=final)
result6
plot(result6, classThr = criterion, trueTh = TRUE)

# Example4: if the respond came from actual data (post-hoc) from lower ability
x_true_data_lower <- resp[404,] # response data from lower respondent
x_true_data_lower
result7 <- randomCAT(itemBank = bank, responses = as.matrix(x_true_data_lower),
                     model = NULL, genSeed = 123,
                     start = start, test = test, stop = stop1,
                     final = final)
result7
plot(result7)
plot(result7, classThr = criterion, trueTh = TRUE)