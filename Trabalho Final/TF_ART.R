## Trabalho Final - Planejamento e Analise de Experimentos
# Homero, Isaac e Stéfani

# clean workspace
rm(list=ls())

# Instalar ARTool se necessário
packages_needed <- c("stringr","ggplot2", "multcomp", "e1071","ggridges", "car", "ARTool")
for (package_name in packages_needed) {      
  if (!(package_name %in% rownames(installed.packages()))){
    install.packages(package_name)
  }
}

library(ARTool) # Carregar biblioteca especifica para ANOVA Nao-Parametrica

################################################################################
## Leitura dos dados 
data <- read.table("results_30execs.csv", header = TRUE, sep = ",")

# Preparação dos Fatores (Crucial para o ART funcionar)
# O ART exige que as variáveis independentes sejam FACTOR
prepare_data <- function(df, col_idx_finais) {
  sub_df <- df[, c(1:11, col_idx_finais)]
  sub_df$DIM <- factor(sub_df$DIM)
  sub_df$BAL <- factor(sub_df$BAL)
  sub_df$LIN <- factor(sub_df$LIN)
  sub_df$NOISE <- factor(sub_df$NOISE)
  return(sub_df)
}

data_lr  <- prepare_data(data, ncol(data)-3)
data_svm <- prepare_data(data, ncol(data)-2)
data_rf  <- prepare_data(data, ncol(data)-1)
data_xgb <- prepare_data(data, ncol(data))

################################################################################
## Análise via ART (Aligned Rank Transform)
## Substitui a ANOVA tradicional quando a normalidade falha.

# --- 1. Regressão Linear ---
print("--- Resultados ART: Regressão Linear ---")
# O art() faz a transformação dos dados
m_art_lr <- art(AUC_LR ~ DIM * BAL * LIN * NOISE, data = data_lr)
# O anova() roda o teste F sobre os dados transformados
anova(m_art_lr)

# --- 2. SVM ---
print("--- Resultados ART: SVM ---")
m_art_svm <- art(AUC_SVM ~ DIM * BAL * LIN * NOISE, data = data_svm)
anova(m_art_svm)

# --- 3. Random Forest ---
print("--- Resultados ART: Random Forest ---")
m_art_rf <- art(AUC_RF ~ DIM * BAL * LIN * NOISE, data = data_rf)
anova(m_art_rf)

# --- 4. XGBoost ---
print("--- Resultados ART: XGBoost ---")
m_art_xgb <- art(AUC_XGB ~ DIM * BAL * LIN * NOISE, data = data_xgb)
anova(m_art_xgb)

################################################################################
## Geração dos Gráficos (Mantemos os originais pois são visuais)

library(ggplot2)

# Função auxiliar para plotar
plot_boxplot <- function(df, y_col, titulo) {
  ggplot(df, aes_string(x = "BAL", y = y_col, fill = "LIN")) +
    geom_boxplot() +
    facet_grid(DIM ~ NOISE) +
    theme_minimal() +
    theme(
      axis.text  = element_text(size = 12),
      axis.title = element_text(size = 14),
      strip.text = element_text(size = 12),
      legend.position = "bottom"
    ) +
    labs(x = "Balanceamento", y = "AUC", fill = "Linearidade", title = titulo)
}

p_lr  <- plot_boxplot(data_lr, "AUC_LR", "Regressão Linear")
p_svm <- plot_boxplot(data_svm, "AUC_SVM", "SVM")
p_rf  <- plot_boxplot(data_rf, "AUC_RF", "Random Forest")
p_xgb <- plot_boxplot(data_xgb, "AUC_XGB", "XGBoost")

# Salvar
ggsave("LR_Canva.png", p_lr, width=8, height=5, dpi=300)
ggsave("SVM_Canva.png", p_svm, width=8, height=5, dpi=300)
ggsave("RF_Canva.png", p_rf, width=8, height=5, dpi=300)
ggsave("XGB_Canva.png", p_xgb, width=8, height=5, dpi=300)