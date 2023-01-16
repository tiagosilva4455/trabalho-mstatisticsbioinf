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



#-----Correlações-----
model=lm(dati$DepressionAfter~dati$Internet.use..mean.hours.per.week.)
model
plot(model)
summary(model)
