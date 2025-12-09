## Visualização da Importância dos Fatores (Baseado no F-Value do ART)
## Autor: Homero, Isaac e Stéfani

packages_needed <- c("viridis")
for (package_name in packages_needed) {      
  if (!(package_name %in% rownames(installed.packages()))){
    install.packages(package_name)
  }
}

library(dplyr)
library(ggplot2)
library(viridis) # Para cores acessíveis e bonitas

# 1. Função para extrair dados da tabela ANOVA do ART
extract_art_results <- function(model_art, model_name) {
  # Extrai a tabela anova
  df_anova <- as.data.frame(anova(model_art))
  
  # Cria uma coluna com os nomes dos fatores (que ficam nos rownames)
  df_anova$Term <- rownames(df_anova)
  
  # Adiciona o nome do modelo
  df_anova$Model <- model_name
  
  # Seleciona apenas o que importa: Termo, Valor F e P-valor
  df_anova <- df_anova %>% 
    select(Term, `F value`, `Pr(>F)`, Model) %>%
    rename(F_Value = `F value`, P_Value = `Pr(>F)`)
  
  return(df_anova)
}

# 2. Consolidar todos os resultados em um único Dataframe
results_lr  <- extract_art_results(m_art_lr, "Regressão Linear")
results_svm <- extract_art_results(m_art_svm, "SVM")
results_rf  <- extract_art_results(m_art_rf, "Random Forest")
results_xgb <- extract_art_results(m_art_xgb, "XGBoost")

all_results <- bind_rows(results_lr, results_svm, results_rf, results_xgb)

# Limpeza: Remover termos muito pequenos para limpar o gráfico (opcional)
# Vamos manter tudo, mas ordenar por importância.

################################################################################
## VISUALIZAÇÃO 1: Gráfico de Pareto (Barras de Importância)
## O melhor gráfico para "ranking"

# Reordenar os fatores baseados no F-Value médio para ficar bonito no gráfico
order_terms <- all_results %>%
  group_by(Term) %>%
  summarise(MeanF = mean(F_Value)) %>%
  arrange(MeanF) %>%
  pull(Term)

all_results$Term <- factor(all_results$Term, levels = order_terms)

p_pareto <- ggplot(all_results, aes(x = Term, y = F_Value, fill = Model)) +
  geom_bar(stat = "identity", width = 0.7, position = "dodge") +
  coord_flip() + # Barras horizontais são mais fáceis de ler
  facet_wrap(~Model, scales = "free_x") + # Um gráfico por modelo
  theme_bw() +
  scale_fill_viridis(discrete = TRUE) +
  labs(
    title = "Importância dos Fatores (F-Value do ART)",
    subtitle = "Barras maiores indicam maior impacto no desempenho (AUC)",
    x = "Fatores e Interações",
    y = "Magnitude do Efeito (F-Value)"
  ) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10, face = "bold"),
    strip.text = element_text(size = 12, face = "bold")
  )

print(p_pareto)
ggsave("ART_pareto.png", p_pareto, width = 12, height = 8)


################################################################################
## VISUALIZAÇÃO 2: Heatmap Comparativo
## Ótimo para ver padrões cruzados entre modelos

# 2. Consolidar e RENOMEAR (Aqui está a correção)
results_lr  <- extract_art_results(m_art_lr, "Regressão Linear")
results_svm <- extract_art_results(m_art_svm, "SVM")
results_rf  <- extract_art_results(m_art_rf, "Random Forest")
results_xgb <- extract_art_results(m_art_xgb, "XGBoost")

all_results <- bind_rows(results_lr, results_svm, results_rf, results_xgb)

# --- CORREÇÃO DE NOMES PARA O PLOT ---
# Transforma a coluna Model em fator e troca os rótulos (labels)
all_results$Model <- factor(all_results$Model, 
                            levels = c("Regressão Linear", "SVM", "Random Forest", "XGBoost"),
                            labels = c("LR", "SVM", "RF", "XGB"))


# 3. Ordenação dos Termos (Para o gráfico ficar organizado)
order_terms <- all_results %>%
  group_by(Term) %>%
  summarise(MeanF = mean(F_Value)) %>%
  arrange(MeanF) %>%
  pull(Term)

all_results$Term <- factor(all_results$Term, levels = order_terms)

p_heatmap <- ggplot(all_results, aes(x = Model, y = Term, fill = F_Value)) +
  geom_tile(color = "white") +
  # Usar escala de cor Logarítmica ajuda a ver diferenças quando há valores gigantes
  scale_fill_viridis(option = "magma", direction = -1, trans = "log10", 
                     name = "F-Value (Log)") + 
  geom_text(aes(label = round(F_Value, 0)), color = "white", size = 3) + # Adiciona o numero
  theme_minimal() +
  labs(
    title = "Heatmap de Importância Experimental",
    x = "Modelos Clássicos",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11, face = "bold"),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank()
  )

print(p_heatmap)
ggsave("ART_Heatmap.png", p_heatmap, width = 8, height = 6)