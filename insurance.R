insurance=read.csv(file.choose(),stringsAsFactors = T, header = T)
str(insurance)
summary(insurance)
hist(insurance$charges)
cor(insurance[c("age","bmi","children","charges")])
model=lm(charges~age+bmi+children+smoker+region,data=insurance)
insurance_test=insurance[1200:1338,]
test=predict(model,insurance_test)
summary(model)