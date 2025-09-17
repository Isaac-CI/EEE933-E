rm(list=ls())

library(car)

########################
# Esquemático EC1 (sugerido):
#   - Hipóteses;
#   - Tipo de teste;
#   - Verificação de homocedasticidade;
#   - Resultado teste t;
#   - Tamanho do efeito e intervalo de confiança no IMC
#   - Verificação e discussão de normalidade e independência;
#   - Conclusões e recomendações;
#   - Discussões sobre a potência do teste;
#   - Possíveis melhorias no experimento;
########################

########################
# Loading data

data_2016 <- read.csv('../data/imc_20162.csv')
data_2017 <- read.csv('../data/CS01_20172.csv', sep=';')

# Selecting only PPGEE students
data_PPGEE_2016 <- data_2016[data_2016$Course == 'PPGEE',]
data_PPGEE_2017 <- data_2017

# Labeling the years
data_PPGEE_2016$Year <- 2016
data_PPGEE_2017$Year <- 2017

########################
# IMC calc. (Weight[kg]/(Height[m]^2))

data_PPGEE_2016$IMC = data_PPGEE_2016$Weight.kg/(data_PPGEE_2016$Height.m**2)
data_PPGEE_2017$IMC = data_PPGEE_2017$Weight.kg/(data_PPGEE_2017$height.m**2)

########################
# Women DF
data_PPGEE_2016_F <- data_PPGEE_2016[data_PPGEE_2016$Gender == 'F',]
data_PPGEE_2017_F <- data_PPGEE_2017[data_PPGEE_2017$Sex == 'F',]

data_PPGEE_full_F <- data.frame(Year = c(data_PPGEE_2016_F$Year, data_PPGEE_2017_F$Year),
                           IMC = c(data_PPGEE_2016_F$IMC, data_PPGEE_2017_F$IMC))

# Men DF
data_PPGEE_2016_M <- data_PPGEE_2016[data_PPGEE_2016$Gender == 'M',]
data_PPGEE_2017_M <- data_PPGEE_2017[data_PPGEE_2017$Sex == 'M',]

data_PPGEE_full_M <- data.frame(Year = c(data_PPGEE_2016_M$Year, data_PPGEE_2017_M$Year),
                           IMC = c(data_PPGEE_2016_M$IMC, data_PPGEE_2017_M$IMC))

########################
####
# A princípio não se sabe se as variâncias são iguais, logo realiza-se o teste t de Welch.
# Por já termos os dados talvez possamos inverter a ordem, fazer as verificações de normalidade,
# homocedasticidade e independência e escolher o melhor método previamente.
####

# welch t-test - two_sample - women

t.test(data_PPGEE_full_F$IMC ~ data_PPGEE_full_F$Year,
       alternative = "two.sided",
       mu = 0,
       conf.level = 0.95)

# welch t-test - two_sample - men

t.test(data_PPGEE_full_M$IMC ~ data_PPGEE_full_M$Year,
       alternative = "two.sided",
       mu = 0,
       conf.level = 0.95)

####
# Verifica-se se as variâncias são iguais e realiza-se o teste t caso positivo.
####

fligner.test(IMC ~ Year, data = data_PPGEE_full_F)

resid_F <- tapply(X = data_PPGEE_full_F$IMC, INDEX = data_PPGEE_full_F$Year,
                  FUN = function(x){x-mean(x)})
stripchart(x=resid_F,
           vertical = TRUE,
           pch = 16,
           cex = 1.5,
           las = 1,
           xlab = 'mean',
           ylab = 'residuals')

fligner.test(IMC ~ Year, data = data_PPGEE_full_M)

resid_M <- tapply(X = data_PPGEE_full_M$IMC, INDEX = data_PPGEE_full_M$Year,
                  FUN = function(x){x-mean(x)})
stripchart(x=resid_M,
           vertical = TRUE,
           pch = 16,
           cex = 1.5,
           las = 1,
           xlab = 'mean',
           ylab = 'residuals')

###
# Os testes apontam que as variâncias podem ser consideradas iguais
###

# t-test - two_sample - women

res_ttest_f <- t.test(data_PPGEE_full_F$IMC ~ data_PPGEE_full_F$Year,
       alternative = "two.sided",
       mu = 0,
       var.equal = TRUE,
       conf.level = 0.95)

################## Resultado t-Test feminino ####################
print(res_ttest_f)

tam_efeito_f <- unname(abs(res_ttest_f$estimate[2] - res_ttest_f$estimate[1]))

print(paste("Tamanho do efeito: ", round(tam_efeito_f, 4)))

# t-test - two_sample - men

res_ttest_m <- t.test(data_PPGEE_full_M$IMC ~ data_PPGEE_full_M$Year,
       alternative = "two.sided",
       mu = 0,
       var.equal = TRUE,
       conf.level = 0.95)

################## Resultado t-Test masculino ####################
print(res_ttest_m)

tam_efeito_m <- unname(abs(res_ttest_m$estimate[2] - res_ttest_m$estimate[1]))

print(paste("Tamanho do efeito: ", round(tam_efeito_m,4)))

########################
# Normality tests

qqPlot(data_PPGEE_full_F$IMC, groups = data_PPGEE_full_F$Year,
       cex = 1.5, pch = 16, las = 1, layout = c(2,1))

# Resultado Shapiro feminino 2016
shapiro.test(data_PPGEE_full_F$IMC[data_PPGEE_full_F$Year == 2016])
# Resultado Shapiro feminino 2017
shapiro.test(data_PPGEE_full_F$IMC[data_PPGEE_full_F$Year == 2017])

qqPlot(data_PPGEE_full_M$IMC, groups = data_PPGEE_full_M$Year,
       cex = 1.5, pch = 16, las = 1, layout = c(2,1))

# Resultado Shapiro masculino 2016
shapiro.test(data_PPGEE_full_M$IMC[data_PPGEE_full_M$Year == 2016])
# Resultado Shapiro masculino 2017
shapiro.test(data_PPGEE_full_M$IMC[data_PPGEE_full_M$Year == 2017])

########################
# Verificando o poder dos testes

# Feminino
power_f <- power.t.test(n=nrow(data_PPGEE_full_F), delta=sd(data_PPGEE_full_F$IMC), sd=sd(data_PPGEE_full_F$IMC),
             sig.level=0.05, type="two.sample", alternative = "two.sided")$power

print(paste("Poder teste feminino: ", round(power_f,4)))

# Masculino
power_m <- power.t.test(n=nrow(data_PPGEE_full_M), delta=sd(data_PPGEE_full_M$IMC), sd=sd(data_PPGEE_full_M$IMC),
             sig.level=0.05, type="two.sample", alternative = "two.sided")$power

print(paste("Poder teste masculino: ", round(power_m,4)))
