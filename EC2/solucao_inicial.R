#-------------------------------------------------------------------------------
# ETAPA 0: INSTALAÇÃO E CARREGAMENTO DOS PACOTES
#-------------------------------------------------------------------------------
if (!require("readr")) install.packages("readr", repos="http://cran.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos="http://cran.r-project.org")
if (!require("tidyr")) install.packages("tidyr", repos="http://cran.r-project.org")
if (!require("car")) install.packages("car", repos="http://cran.r-project.org")
if (!require("dunn.test")) install.packages("dunn.test", repos="http://cran.r-project.org")
if (!require("knitr")) install.packages("knitr", repos="http://cran.r-project.org")
if (!require("multcomp")) install.packages("multcomp", repos="http://cran.r-project.org")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("pwr")) install.packages("pwr")

library(readr)
library(dplyr)
library(tidyr)
library(car)
library(dunn.test)
library(knitr)
library(multcomp)
library(ggplot2)
library(pwr)

#-------------------------------------------------------------------------------
# ETAPA 0: Definição das funções auxiliares
#-------------------------------------------------------------------------------

#' Plota os intervalos de confiança de um teste de Tukey com destaques visuais.
#'
#' @param tukey_confint_obj Um objeto da classe 'confint.glht', geralmente o resultado de confint(glht(...)).
#' @param comp_significativas Um vetor de strings com os nomes das comparações a serem destacadas como "Significativo".
#' @param comp_marginais Um vetor de strings com os nomes das comparações a serem destacadas como "Marginalmente Significativo".
#' @param titulo O título do gráfico. Se for NULL (padrão), um título automático será gerado.
#'
#' @return Um objeto ggplot.
#'
plotar_tukey_com_destaque <- function(tukey_confint_obj,
                                      comp_significativas = NULL,
                                      comp_marginais = NULL,
                                      titulo = NULL) {
  
  # --- PASSO 1: Extrair os dados para um data.frame ---
  df_plot <- data.frame(
    Comparacao = names(tukey_confint_obj$confint[, "Estimate"]),
    Estimativa = tukey_confint_obj$confint[, "Estimate"],
    Limite_Inferior = tukey_confint_obj$confint[, "lwr"],
    Limite_Superior = tukey_confint_obj$confint[, "upr"]
  )
  
  # --- PASSO 2: Criar a coluna de destaque com base nos parâmetros ---
  df_plot <- df_plot %>%
    mutate(
      Destaque = case_when(
        Comparacao %in% comp_significativas ~ "Significativo",
        Comparacao %in% comp_marginais    ~ "Marginalmente Significativo",
        TRUE                              ~ "Não Significativo"
      )
    )
  
  # --- PASSO 3: Gerar título dinâmico se nenhum for fornecido ---
  if (is.null(titulo)) {
    nivel_confianca <- attr(tukey_confint_obj, "conf.level") * 100
    titulo_plot <- paste0("Intervalos de Confiança de 95% para Comparações Múltiplas (Tukey)")
  } else {
    titulo_plot <- titulo
  }
  
  # --- PASSO 4: Gerar o gráfico ---
  g <- ggplot(df_plot, aes(x = Estimativa, y = Comparacao, color = Destaque)) +
    geom_errorbarh(aes(xmin = Limite_Inferior, xmax = Limite_Superior), height = 0.2, size = 0.8) +
    geom_point(size = 3) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    
    # Controlar manualmente as cores e a legenda
    scale_color_manual(
      name = "Nível de Significância",
      # O argumento 'breaks' garante a ordem correta na legenda
      breaks = c("Significativo", "Marginalmente Significativo", "Não Significativo"),
      values = c(
        "Significativo" = "red",
        "Marginalmente Significativo" = "darkorange",
        "Não Significativo" = "gray50"
      ),
      # O argumento 'drop = FALSE' garante que todos os níveis apareçam na legenda, mesmo que não existam no dado
      drop = FALSE
    ) +
    
    # Rótulos e tema
    labs(
      title = titulo_plot,
      x = "Diferença Estimada nos Retornos Médios",
      y = "Comparação Par a Par"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  # Retornar o objeto ggplot
  return(g)
}


#-------------------------------------------------------------------------------
# ETAPA 1: CRIAÇÃO DA SÉRIE HISTÓRICA DA TAXA LIVRE DE RISCO (SELIC), dados disponíveis em: https://www.bcb.gov.br/controleinflacao/historicotaxasjuros
#-------------------------------------------------------------------------------
taxas_selic_anuais_historicas <- c(
  rep(0.1365, 12), 0.1315, rep(0.1265, 2), 0.1215, rep(0.1165, 2), 
  0.1115, rep(0.1065, 2), rep(0.1040, 3), rep(0.1065, 2), 0.1115, 
  0.1215, rep(0.1315, 2), rep(0.1415, 2), 0.1465, rep(0.1490, 2), 0.1500
)

taxas_selic_mensais_historicas <- (1 + taxas_selic_anuais_historicas)^(1/12) - 1
taxas_selic_mensais_periodo <- taxas_selic_mensais_historicas[2:36]

#-------------------------------------------------------------------------------
# ETAPA 2: Análise de variância
#-------------------------------------------------------------------------------

# --- Seção 1: Análise Descritiva ---
cat("\n--- 1. Análise Descritiva dos Dados ---\n")

# Carrega dados e calcula retornos mensais de cada uma das 5 ações

nomes_acoes <- paste0("Acao_", 1:5)

precos_df <- read_csv("DadosAcoesGrupoE.csv", col_names = nomes_acoes, show_col_types = FALSE)

retornos_df <- as.data.frame(sapply(precos_df, function(coluna_preco) {
  
  (coluna_preco[-length(coluna_preco)] - coluna_preco[-1]) / coluna_preco[-1]
  
})) 

excess_returns_df <- retornos_df - taxas_selic_mensais_periodo
retornos_long_df <- retornos_df %>%
  pivot_longer(cols = everything(), names_to = "Acao", values_to = "Retorno_Bruto") %>%
  mutate(Acao = as.factor(Acao))
excess_returns_long_df <- excess_returns_df %>%
  pivot_longer(cols = everything(), names_to = "Acao", values_to = "Excesso_Retorno")

# --- 1.1: Tabela de Estatísticas Descritivas ---
cat("\n--- 1.1: Tabela de Estatísticas Descritivas ---\n")

desc_stats <- retornos_long_df %>%
  group_by(Acao) %>%
  summarise(
    Media_Retorno = mean(Retorno_Bruto, na.rm = TRUE),
    Desvio_Padrao = sd(Retorno_Bruto, na.rm = TRUE),
    .groups = 'drop'
  )
mean_excess_returns <- excess_returns_long_df %>%
  group_by(Acao) %>%
  summarise(Media_Excesso_Retorno = mean(Excesso_Retorno, na.rm = TRUE))
desc_stats <- left_join(desc_stats, mean_excess_returns, by = "Acao")
desc_stats <- desc_stats %>%
  mutate(Indice_Sharpe = Media_Excesso_Retorno / Desvio_Padrao)

print(kable(desc_stats, "pipe", digits = 4, caption = "Estatísticas Descritivas (Sharpe com Selic Histórica)"))

# --- 1.2: Visualização da Distribuição Comparativa dos Retornos ---
cat("\n--- 1.2: Visualização da Distribuição Comparativa dos Retornos ---\n")

# PASSO 1: Criar um data frame para os retornos da Selic
# As colunas devem ter os mesmos nomes do data frame dos retornos das ações ('Acao', 'Retorno_Bruto')
selic_df <- data.frame(
  Acao = "Selic",
  Retorno_Bruto = taxas_selic_mensais_periodo
)

# PASSO 2: Unir os data frames das ações e da Selic
df_combinado <- rbind(retornos_long_df, selic_df)

# PASSO 3: Definir a ordem das categorias no eixo X para melhor visualização
# Colocamos a Selic primeiro para servir como benchmark de referência
df_combinado$Acao <- factor(df_combinado$Acao, levels = c("Selic", "Acao_1", "Acao_2", "Acao_3", "Acao_4", "Acao_5"))

# PASSO 4: Gerar o boxplot aprimorado
# Usaremos scale_fill_manual() para dar uma cor distinta à Selic
boxplot_comparativo <- ggplot(df_combinado, aes(x = Acao, y = Retorno_Bruto, fill = Acao)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Linha de referência no zero
  
  # Define uma paleta de cores manual para destacar a Selic
  scale_fill_manual(
    name = "Ativo",
    values = c(
      "Selic" = "skyblue", 
      "Acao_1" = "gray80", 
      "Acao_2" = "gray80", 
      "Acao_3" = "gray80", 
      "Acao_4" = "gray80", 
      "Acao_5" = "gray80"
    )
  ) +
  
  labs(
    title = "Distribuição dos Retornos Mensais: Ações vs. Selic",
    subtitle = "Período de análise: 2022-2025",
    x = "Ativo",
    y = "Retorno Bruto Mensal"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # Remove a legenda, pois a distinção de cores é clara

print(boxplot_comparativo)


# --- 1.3: Visualização do Desempenho Ajustado ao Risco ---
cat("\n--- 1.3: Visualização do Desempenho Ajustado ao Risco ---\n")

# Gráfico de barras para comparar o Índice de Sharpe
# Usamos reorder(Acao, -Indice_Sharpe) para ordenar as barras da maior para a menor
barchart_sharpe <- ggplot(desc_stats, aes(x = reorder(Acao, -Indice_Sharpe), y = Indice_Sharpe, fill = Acao)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Indice_Sharpe, 2)), vjust = ifelse(desc_stats$Indice_Sharpe > 0, 1.5, -0.5)) + # Adiciona o rótulo do valor
  labs(
    title = "Comparação do Índice de Sharpe por Ação",
    subtitle = "Maior valor indica melhor retorno ajustado ao risco",
    x = "Ação",
    y = "Índice de Sharpe"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(barchart_sharpe)


# Preparação para a próxima etapa do script
retornos_long_df_para_teste <- retornos_long_df %>% rename(Retorno = Retorno_Bruto)

# --- Seção 2: Teste de Hipóteses ---
cat("\n--- 2. Teste de Hipóteses ---\n")

anova_result <- aov(Retorno ~ Acao, data = retornos_long_df_para_teste)
cat("--> Usando ANOVA e Tukey HSD.\n\n")
anova_result <- aov(Retorno ~ Acao, data = retornos_long_df_para_teste)
cat("Resultado da ANOVA:\n"); print(summary(anova_result))
p_valor_anova <- summary(anova_result)[[1]][["Pr(>F)"]][1]

cat("Resultado da análise post-hoc por Tukey HSD:\n");
tukey_glht <- glht(anova_result, linfct = mcp(Acao = "Tukey"))
summary(tukey_glht)
tukey_CI <- confint(tukey_glht, level = 0.95)
sig <- c("Acao_2 - Acao_1", "Acao_4 - Acao_1")
marginal <- "Acao_5 - Acao_1"
CI_plot <- plotar_tukey_com_destaque(
  tukey_confint_obj = tukey_CI,
  comp_significativas = sig,
  comp_marginais = marginal
)
print(CI_plot)

if (p_valor_anova < 0.05) {
  cat("\nANOVA é significativa. Realizando Teste Post-Hoc de Tukey HSD:\n");
  print(TukeyHSD(anova_result))
} else {
    cat("\nANOVA não é significativa.\n")
}

# --- Seção 3: Verificação das Premissas ---
cat("\n--- 3. Verificação das Premissas ---\n")

# Extrai os resíduos do modelo ANOVA uma única vez
residuos <- residuals(anova_result)

# a) Teste de Normalidade (Shapiro-Wilk)
cat("a) Teste de Normalidade (Shapiro-Wilk) nos resíduos:\n")
shapiro_results <- shapiro.test(residuos)
print(shapiro_results)
if (shapiro_results$p.value > 0.05) {
  cat("-> Resultado: Os resíduos parecem seguir uma distribuição normal (p > 0.05). Premissa satisfeita.\n")
} else {
  cat("-> Resultado: Os resíduos não seguem uma distribuição normal (p <= 0.05). Premissa violada.\n")
}

qqPlot(residuos,
       main = "Gráfico Q-Q", # Título principal
       xlab = "Quantis Teóricos da Distribuição Normal",        # Legenda do eixo X
       ylab = "Resíduos do Modelo",      # Legenda do eixo Y
       pch = 16,
       lwd = 2,  # Um pouco menos espesso que antes para equilibrar
       cex = 1.5 # Um pouco menor para um visual mais limpo
)

# b) Teste de Homogeneidade de Variâncias (Fligner-Killeen)
cat("\nb) Teste de Homogeneidade de Variâncias (Fligner-Killeen):\n")
fligner_test_result <- fligner.test(Retorno ~ Acao, data = retornos_long_df_para_teste)
print(fligner_test_result)
if (fligner_test_result$p.value > 0.05) {
  cat("-> Resultado: As variâncias são homogêneas entre os grupos (p > 0.05). Premissa satisfeita.\n")
} else {
  cat("-> Resultado: As variâncias não são homogêneas entre os grupos (p <= 0.05). Premissa violada.\n")
}

# c) Teste de Independência dos Resíduos (Autocorrelação)
cat("\nc) Teste de Independência dos Resíduos (Autocorrelação):\n")
durbin_watson_result <- car::durbinWatsonTest(anova_result)
print(durbin_watson_result)

if (durbin_watson_result$p > 0.05) {
  cat("-> Resultado: Não há evidência de autocorrelação nos resíduos (p > 0.05). Premissa de independência satisfeita.\n")
} else {
  cat("-> Resultado: Há evidência de autocorrelação significativa nos resíduos (p <= 0.05). Premissa de independência violada.\n")
}

# Análise visual com o gráfico ACF
cat("\n   Análise Visual - Função de Autocorrelação (ACF) dos Resíduos:\n")
acf(residuos, main = "Função de Autocorrelação dos Resíduos da ANOVA")

# --- Seção 4: Conclusão e Recomendação ---
cat("\n--- 4. Conclusão e Recomendação ---\n")
melhor_acao <- desc_stats %>% arrange(desc(Indice_Sharpe)) %>% slice(1)
cat(paste0("\n> Recomendação para ", basename("DadosAcoesGrupoE.csv"), ":\n"))
cat(paste0("A análise sugere que a **", melhor_acao$Acao, "** apresentou o melhor desempenho histórico ajustado ao risco (Maior Índice de Sharpe = ", round(melhor_acao$Indice_Sharpe, 4), ").\n"))
cat("A decisão final deve ser confirmada pela significância estatística do teste post-hoc (se aplicável).\n")


#-------------------------------------------------------------------------------
# ETAPA 3: Poder do Teste
#-------------------------------------------------------------------------------

# 1. Obter os dados necessários da sua tabela 'desc_stats'
n1 <- n3 <- n5 <- nrow(retornos_df) # Número de observações por grupo (35 no seu caso)
media1 <- desc_stats$Media_Retorno[desc_stats$Acao == "Acao_1"]
media3 <- desc_stats$Media_Retorno[desc_stats$Acao == "Acao_3"]
media5 <- desc_stats$Media_Retorno[desc_stats$Acao == "Acao_5"]
sd1 <- desc_stats$Desvio_Padrao[desc_stats$Acao == "Acao_1"]
sd3 <- desc_stats$Desvio_Padrao[desc_stats$Acao == "Acao_3"]
sd5 <- desc_stats$Desvio_Padrao[desc_stats$Acao == "Acao_5"]

# 2. Calcular o Desvio Padrão Agrupado (Pooled Standard Deviation)
sd_pooled_3_1 <- sqrt(((n1 - 1) * sd1^2 + (n3 - 1) * sd3^2) / (n1 + n3 - 2))
sd_pooled_5_1 <- sqrt(((n1 - 1) * sd1^2 + (n5 - 1) * sd5^2) / (n1 + n5 - 2))

# 3. Calcular o Tamanho do Efeito (Cohen's d)
cohen_d_3_1 <- (media1 - media3) / sd_pooled_3_1
cohen_d_5_1 <- (media1 - media5) / sd_pooled_5_1

# 4. Calcular a potência usando pwr.t.test
analise_potencia_3_1 <- pwr.t.test(
  n = n1, # n por grupo
  d = cohen_d_3_1,
  sig.level = 0.05,
  type = "two.sample",
  alternative = "two.sided"
)

analise_potencia_5_1 <- pwr.t.test(
  n = n1, # n por grupo
  d = cohen_d_5_1,
  sig.level = 0.05,
  type = "two.sample",
  alternative = "two.sided"
)

# 5. Exibir o resultado
print(analise_potencia_3_1)
print(analise_potencia_5_1)