dati= na.omit(depression.and.the.internet) #para omitir valores NA

#paired = True, segundo a Joana 
#A media de depressão after é menor do que o before

#-----------------Analise Exploratoria----------------
#----Media de horas por semana na internet-----

summary(dati$Internet.use..mean.hours.per.week.)

range (dati$Internet.use..mean.hours.per.week.)

sd(dati$Internet.use..mean.hours.per.week.)
var(dati$Internet.use..mean.hours.per.week.)
fivenum((dati$Internet.use..mean.hours.per.week.))

hist(dati$Internet.use..mean.hours.per.week., main = "Media de horas por semana na internet",xlab = "Horas")

#---- Nivel de depressão antes do estudo -----
summary(dati$DepressionBefore)

range (dati$DepressionBefore)

sd(dati$DepressionBefore)
var(dati$DepressionBefore)
fivenum((dati$DepressionBefore))

hist(dati$DepressionBefore, main = "Niveis de depressão antes do estudo",xlab = "Grau de Depressão")

#---- Nivel de depressão depois do estudo -----
summary(dati$DepressionAfter)

range (dati$DepressionAfter)

sd(dati$DepressionAfter)
var(dati$DepressionAfter)
fivenum((dati$DepressionAfter))

hist(dati$DepressionAfter, main = "Niveis de depressão depois do estudo",xlab = "Grau de Depressão")

#---- Género -----
gender = table(dati$Gender)
gender
pie(gender, labels = names(gender),col=c("pink","cadetblue1"), main = "Género dos participantes")
barplot(gender, col = c("pink","cadetblue1"), main = "Género dos participantes" )


#---- Etnia -----
Etnia = table(dati$Race..white...1..minority...0.)
Etnia
labels =names.arg = c("Minority","White")

pie(Etnia,labels = labels,col=c("burlywood","antiquewhite1"), main = "Etnia dos participantes")
barplot(Etnia, col = c("pink","cadetblue1"), main = "Etnia dos participantes" )

#----- Estagio da vida ----
age = table(dati$Age)
age
labels =names.arg = c("Adulto","Adolescente")

pie(age,labels = labels,col=c("burlywood","antiquewhite1"), main = "Estágio de vida dos participantes")
barplot(age,names = labels, col = c("pink","cadetblue1"), main = "Estágio de vida dos participantes" )

#---- Salário Anual do Agregado ----
summary(dati$Household.income...000.)

range (dati$Household.income...000.)

sd(dati$Household.income...000.)
var(dati$Household.income...000.)
fivenum((dati$Household.income...000.))

hist(dati$Household.income...000., main = "Salário Anual do Agregado dos Participantes",xlab = "Salário (em milhares)")

sal= table(dati$Household.income...000.)
barplot(sal,main ="Salário Anual do Agregado dos Participantes", xlab="Salário (em milhares)", ylab = "Frequência", cex.names = 0.70,las= 2)

#-----Tamanho do Agregado Familiar----
summary(dati$Household.size)

range (dati$Household.size)

sd(dati$Household.size)
var(dati$Household.size)
fivenum((dati$Household.size))

hist(dati$Household.size, main = "Salário Anual do Agregado dos Participantes",xlab = "Número de pessoas do agregado")
agr= table(dati$Household.size)
barplot(agr,main ="Tamanho do Agregado dos Participantes", xlab="Número de pessoas do agregado", ylab = "Frequência", cex.names = 0.70,las= 2, col = rainbow(17))


#-----Correlações (Antes do Estudo)----
db = dati$DepressionBefore

#Depression by gender
model1b = lm (db~dati$Gender)
model1b
summary(model1b)
plot(model1b)

#Depression by race
model2b =lm(db~dati$Race..white...1..minority...0.)
model2b
summary(model2b)
plot(model2b)

#Depression by age
model3b = lm(db~dati$Age)
model3b
summary(model3b)
plot(model3b)

#Depression by income
model4b=lm(db~dati$Household.income...000.)
model4b
summary(model4b)
plot(model4b)

#Depression by size of household
model5b=lm(db~dati$Household.size)
model5b
summary(model5b)
plot(model5b)



#-----Correlações (Apos Estudo)-----

da=dati$DepressionAfter

#Depression com Horas de uso(ver hipoteses)
model=lm(dati$DepressionAfter~dati$Internet.use..mean.hours.per.week.)
model
summary(model)
plot(model)

#---- correlação entre depressoes ---

modelab = lm(da~db)
modelab
summary(modelab)
plot(modelab)

#---- correlação entre mais do que duas variaveis ----
#Depressão com horas de uso por genero
model21 = aov(da~dati$Internet.use..mean.hours.per.week.+dati$Gender)
model21
summary (model21)

interaction.plot(dati$Age,dati$Gender,da)

#Depressão com horas de uso por etnia

model22 = lm (da~dati$Internet.use..mean.hours.per.week.+dati$Race..white...1..minority...0.) 
model22
summary(model22)

#depressão com horas de uso por idade

model23 = lm(da~dati$Internet.use..mean.hours.per.week.+dati$Age)
model23
summary(model23)

#depressao com horas de uso por income

model24 = lm(da~dati$Internet.use..mean.hours.per.week.+dati$Household.income...000.)
model24
summary(model24)

#depressão com horas de uso por tamanho do agregado
model25 = lm (da~dati$Internet.use..mean.hours.per.week.+dati$Household.size)
model25
summary(model25)

#depressao com genero e idade
model26 = lm (da~dati$Gender+dati$Age)
model26
summary(model26)

interaction.plot(dati$Age,dati$Gender,da)


#depressao com genero e etnia
model27=lm(da~dati$Gender+dati$Race..white...1..minority...0.)
model27
summary(model27)

interaction.plot(dati$Race..white...1..minority...0.,dati$Gender,da)

#depressão com idade e etnia
model28 =lm(da ~dati$Age+dati$Race..white...1..minority...0.)
model28
summary(model28)
interaction.plot(dati$Age,dati$Race..white...1..minority...0.,da)

#----Depression Before---
#depressao com genero e idade
model26 = lm (db~dati$Gender+dati$Age)
model26
summary(model26)

interaction.plot(dati$Age,dati$Gender,db)

#depressao com genero e etnia
model27=lm(db~dati$Gender+dati$Race..white...1..minority...0.)
model27
summary(model27)

interaction.plot(dati$Race..white...1..minority...0.,dati$Gender,db)

#depressão com idade e etnia
model28 =lm(db ~dati$Age+dati$Race..white...1..minority...0.)
model28
summary(model28)
interaction.plot(dati$Age,dati$Race..white...1..minority...0.,db)


#Relacionamento de todos

model_final = lm (da~dati$Internet.use..mean.hours.per.week.+dati$Gender+dati$Race..white...1..minority...0.+dati$Age+dati$Household.income...000.+dati$Household.size)
model_final
summary(model_final)

plot(model_final)

