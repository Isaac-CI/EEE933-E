## Trabalho Final - Planejamento e Analise de Experimentos
# Homero, Isaac e Stéfani

# clean workspace
rm(list=ls())

# install required packages if needed
packages_needed <- c("stringr","ggplot2", "multcomp", "e1071","ggridges")
for (package_name in packages_needed) {      
  if (!(package_name %in% rownames(installed.packages()))){
    install.packages(package_name)
  }
}


################################################################################
## Leitura dos dados gerados

data <- read.table("results_30execs.csv", header = TRUE, sep = ",")

# Modelo Regressao Linear
data_lr <- data[, c(1:11, ncol(data)-3)]
data_lr$DIM <- factor(data_lr$DIM, levels = c("5", "300"))       # Dimensionalidade
data_lr$BAL <- factor(data_lr$BAL, levels = c("0.5", "0.9"))     # Balanceamento
data_lr$LIN <- factor(data_lr$LIN, levels = c("True", "False"))  # Linearidade
data_lr$NOISE <- factor(data_lr$NOISE, levels = c("0", "0.3"))   # Superposição

head(data_lr)
summary(data_lr)

# Modelo SVM
data_svm <- data[, c(1:11, ncol(data)-2)]
data_svm$DIM <- factor(data_svm$DIM, levels = c("5", "300"))       # Dimensionalidade
data_svm$BAL <- factor(data_svm$BAL, levels = c("0.5", "0.9"))     # Balanceamento
data_svm$LIN <- factor(data_svm$LIN, levels = c("True", "False"))  # Linearidade
data_svm$NOISE <- factor(data_svm$NOISE, levels = c("0", "0.3"))   # Superposição

head(data_svm)
summary(data_svm)

# Modelo Random Forest
data_rf <- data[, c(1:11, ncol(data)-1)]
data_rf$DIM <- factor(data_rf$DIM, levels = c("5", "300"))       # Dimensionalidade
data_rf$BAL <- factor(data_rf$BAL, levels = c("0.5", "0.9"))     # Balanceamento
data_rf$LIN <- factor(data_rf$LIN, levels = c("True", "False"))  # Linearidade
data_rf$NOISE <- factor(data_rf$NOISE, levels = c("0", "0.3"))   # Superposição

head(data_rf)
summary(data_rf)

# Modelo XGB
data_xgb <- data[, c(1:11, ncol(data))]
data_xgb$DIM <- factor(data_xgb$DIM, levels = c("5", "300"))       # Dimensionalidade
data_xgb$BAL <- factor(data_xgb$BAL, levels = c("0.5", "0.9"))     # Balanceamento
data_xgb$LIN <- factor(data_xgb$LIN, levels = c("True", "False"))  # Linearidade
data_xgb$NOISE <- factor(data_xgb$NOISE, levels = c("0", "0.3"))   # Superposição

head(data_xgb)
summary(data_xgb)


################################################################################
## Análise Exploratória dos Dados

library(ggplot2)

# Plot grafico de AUC LR
p_lr <- ggplot(data_lr,
       aes(x = BAL,
           y = AUC_LR,
           fill = LIN)) +
  geom_boxplot() +
  facet_grid(DIM ~ NOISE) +
  theme_minimal() +
  theme(
    axis.text  = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "bottom"
  ) +
  labs(
    x = "Balanceamento",
    y = "Desempenho (AUC)",
    fill = "Linearidade",
    title = "Distribuição do Desempenho por 4 Fatores (Regressão Linear)"
  )

# Plot grafico de AUC SVM
p_svm <- ggplot(data_svm,
       aes(x = BAL,
           y = AUC_SVM,
           fill = LIN)) +
  geom_boxplot() +
  facet_grid(DIM ~ NOISE) +
  theme_minimal() +
  theme(
    axis.text  = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "bottom"
  ) +
  labs(
    x = "Balanceamento",
    y = "Desempenho (AUC)",
    fill = "Linearidade",
    title = "Distribuição do Desempenho por 4 Fatores (SVM)"
  )

# Plot grafico de AUC RF
p_rf <- ggplot(data_rf,
       aes(x = BAL,
           y = AUC_RF,
           fill = LIN)) +
  geom_boxplot() +
  facet_grid(DIM ~ NOISE) +
  theme_minimal() +
  theme(
    axis.text  = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "bottom"
  ) +
  labs(
    x = "Balanceamento",
    y = "Desempenho (AUC)",
    fill = "Linearidade",
    title = "Distribuição do Desempenho por 4 Fatores (Random Forest)"
  )

# Plot grafico de AUC XGBoost
p_xgb <- ggplot(data_xgb,
       aes(x = BAL,
           y = AUC_XGB,
           fill = LIN)) +
  geom_boxplot() +
  facet_grid(DIM ~ NOISE) +
  theme_minimal() +
  theme(
    axis.text  = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "bottom"
  ) +
  labs(
    x = "Balanceamento",
    y = "Desempenho (AUC)",
    fill = "Linearidade",
    title = "Distribuição do Desempenho por 4 Fatores (XGBoost)"
  )


################################################################################
## Teste de Hipósteses ExpFat Regressao Linear

model_lr <- aov(data_lr$AUC_LR~ (DIM + BAL + LIN + NOISE)^4,
                data = data_lr)
summary(model_lr)
summary.lm(model_lr)$r.squared

## Verificacao de premissas

# Normalidade dos residuos
library(car)
shapiro.test(model_lr$residuals)
qqPlot(model_lr$residuals,
       pch = 16,
       lwd = 3,
       cex = 0.5,
       las = 1)

# Homoscedasticidade
fligner.test(model_lr$residuals ~ interaction(DIM, BAL, LIN, NOISE),
             data = data_lr)



effect_est <- as.numeric(model_lr$effects[-1])
effect_names <- names(model_lr$effects)[-1]

par(mar = c(5,5,4,2) + 0.1)  # margens
qq.obj <- qqnorm(effect_est,
                 datax    = TRUE,
                 pch      = 16,
                 cex      = 1.4,
                 main     = "Normal Q-Q Plot (Efeitos do Modelo)",
                 xlab     = "Sample Quantiles",
                 ylab     = "Theoretical Quantiles")
qqline(effect_est, datax = TRUE, col = "red", lwd = 2)

text(x = qq.obj$x,
     y = qq.obj$y,
     labels = effect_names,
     pos = 4,
     offset = 0.4,
     cex = 0.9)


################################################################################
## Teste de Hipósteses ExpFat SVM

model_svm <- aov(data_svm$AUC_SVM~ (DIM + BAL + LIN + NOISE)^4,
                 data = data_svm)
summary(model_svm)
summary.lm(model_svm)$r.squared

## Verificacao de premissas

# Normalidade dos residuos
library(car)
shapiro.test(model_svm$residuals)
qqPlot(model_svm$residuals,
       pch = 16,
       lwd = 3,
       cex = 0.5,
       las = 1)

# Homoscedasticidade
fligner.test(model_svm$residuals ~ interaction(DIM, BAL, LIN, NOISE),
             data = data_svm)


## Daniel's plot para detectar efeitos
effect_est <- as.numeric(model_svm$effects[-1])
effect_names <- names(model_svm$effects)[-1]

par(mar = c(5,5,4,2) + 0.1)  # margens
qq.obj <- qqnorm(effect_est,
                 datax    = TRUE,
                 pch      = 16,
                 cex      = 1.4,
                 main     = "Normal Q-Q Plot (Efeitos do Modelo)",
                 xlab     = "Sample Quantiles",
                 ylab     = "Theoretical Quantiles")
qqline(effect_est, datax = TRUE, col = "red", lwd = 2)

text(x = qq.obj$x,
     y = qq.obj$y,
     labels = effect_names,
     pos = 4,
     offset = 0.4,
     cex = 0.9)

################################################################################
## Teste de Hipósteses ExpFat Random Forest

model_rf <- aov(data_rf$AUC_RF~ (DIM + BAL + LIN + NOISE)^4,
                data = data_rf)
summary(model_rf)
summary.lm(model_rf)$r.squared

## Verificacao de premissas

# Normalidade dos residuos
library(car)
shapiro.test(model_rf$residuals)
qqPlot(model_rf$residuals,
       pch = 16,
       lwd = 3,
       cex = 0.5,
       las = 1)

# Homoscedasticidade
fligner.test(model_rf$residuals ~ interaction(DIM, BAL, LIN, NOISE),
             data = data_rf)


## Daniel's plot para detectar efeitos
effect_est <- as.numeric(model_rf$effects[-1])
effect_names <- names(model_rf$effects)[-1]

par(mar = c(5,5,4,2) + 0.1)  # margens
qq.obj <- qqnorm(effect_est,
                 datax    = TRUE,
                 pch      = 16,
                 cex      = 1.4,
                 main     = "Normal Q-Q Plot (Efeitos do Modelo)",
                 xlab     = "Sample Quantiles",
                 ylab     = "Theoretical Quantiles")
qqline(effect_est, datax = TRUE, col = "red", lwd = 2)

text(x = qq.obj$x,
     y = qq.obj$y,
     labels = effect_names,
     pos = 4,
     offset = 0.4,
     cex = 0.9)

################################################################################
## Teste de Hipósteses ExpFat XGBoost

model_xgb <- aov(data_xgb$AUC_XGB~ (DIM + BAL + LIN + NOISE)^4,
                 data = data_xgb)
summary(model_xgb)
summary.lm(model_xgb)$r.squared

## Verificacao de premissas

# Normalidade dos residuos
library(car)
shapiro.test(model_xgb$residuals)
qqPlot(model_xgb$residuals,
       pch = 16,
       lwd = 3,
       cex = 0.5,
       las = 1)

# Homoscedasticidade
fligner.test(model_xgb$residuals ~ interaction(DIM, BAL, LIN, NOISE),
             data = data_xgb)


## Daniel's plot para detectar efeitos
effect_est <- as.numeric(model_xgb$effects[-1])
effect_names <- names(model_xgb$effects)[-1]

par(mar = c(5,5,4,2) + 0.1)  # margens
qq.obj <- qqnorm(effect_est,
                 datax    = TRUE,
                 pch      = 16,
                 cex      = 1.4,
                 main     = "Normal Q-Q Plot (Efeitos do Modelo)",
                 xlab     = "Sample Quantiles",
                 ylab     = "Theoretical Quantiles")
qqline(effect_est, datax = TRUE, col = "red", lwd = 2)

text(x = qq.obj$x,
     y = qq.obj$y,
     labels = effect_names,
     pos = 4,
     offset = 0.4,
     cex = 0.9)