#-------------------------------------------------------------------------------
# ETAPA 0: INSTALAÇÃO E CARREGAMENTO DOS PACOTES
#-------------------------------------------------------------------------------
if (!require("readr")) install.packages("readr", repos="http://cran.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos="http://cran.r-project.org")
if (!require("tidyr")) install.packages("tidyr", repos="http://cran.r-project.org")
if (!require("car")) install.packages("car", repos="http://cran.r-project.org")
if (!require("dunn.test")) install.packages("dunn.test", repos="http://cran.r-project.org")
if (!require("knitr")) install.packages("knitr", repos="http://cran.r-project.org")

library(readr)
library(dplyr)
library(tidyr)
library(car)
library(dunn.test)
library(knitr)

#-------------------------------------------------------------------------------
# ETAPA 1: CRIAÇÃO DA SÉRIE HISTÓRICA DA TAXA LIVRE DE RISCO (SELIC), dados disponíveis em: https://www.bcb.gov.br/controleinflacao/historicotaxasjuros
#-------------------------------------------------------------------------------
taxas_selic_anuais_historicas <- c(
  rep(0.1365, 12), 0.1315, rep(0.1265, 2), 0.1215, rep(0.1165, 2), 
  0.1115, rep(0.1065, 2), rep(0.1040, 3), rep(0.1065, 2), 0.1115, 
  0.1215, rep(0.1315, 2), rep(0.1415, 2), 0.1465, rep(0.1490, 2), 0.1500
)

taxas_selic_mensais_historicas <- (1 + taxas_selic_anuais_historicas)^(1/12) - 1
taxas_selic_mensais_vetor_35 <- taxas_selic_mensais_historicas[2:36]

#-------------------------------------------------------------------------------
# ETAPA 2: DEFINIÇÃO DA FUNÇÃO DE ANÁLISE
#-------------------------------------------------------------------------------
analisar_grupo <- function(file_path, rf_rates_vector) {
  
  cat(paste("\n\n=====================================================\n"))
  cat(paste("Analisando o arquivo:", basename(file_path), "\n"))
  cat(paste("=====================================================\n"))
  
  # Carregar e calcular retornos
  nomes_acoes <- paste0("Acao_", 1:5)
  precos_df <- read_csv(file_path, col_names = nomes_acoes, show_col_types = FALSE)
  retornos_df <- as.data.frame(sapply(precos_df, function(coluna_preco) {
    (coluna_preco[-length(coluna_preco)] - coluna_preco[-1]) / coluna_preco[-1]
  }))
  
  # --- Seção 1: Análise Descritiva ---
  cat("\n--- 1. Análise Descritiva ---\n")
  
  excess_returns_df <- retornos_df - rf_rates_vector
  
  retornos_long_df <- retornos_df %>%
    pivot_longer(cols = everything(), names_to = "Acao", values_to = "Retorno_Bruto") %>%
    mutate(Acao = as.factor(Acao))
  
  excess_returns_long_df <- excess_returns_df %>%
    pivot_longer(cols = everything(), names_to = "Acao", values_to = "Excesso_Retorno")
  
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
  
  retornos_long_df_para_teste <- retornos_long_df %>% rename(Retorno = Retorno_Bruto)
  
  # --- Seção 2: Verificação das Premissas ---
  cat("\n--- 2. Verificação das Premissas ---\n")
  cat("a) Teste de Normalidade (Shapiro-Wilk):\n")
  shapiro_results <- retornos_df %>%
    summarise(across(everything(), ~ shapiro.test(.x)$p.value)) %>%
    pivot_longer(everything(), names_to = "Acao", values_to = "p_valor_shapiro")
  premissa_normalidade <- all(shapiro_results$p_valor_shapiro > 0.05)
  print(kable(shapiro_results, "pipe", digits = 4))
  if (premissa_normalidade) {cat("-> Resultado: Todos os grupos parecem seguir uma distribuição normal (p > 0.05).\n")} else {cat("-> Resultado: Pelo menos um grupo violou a premissa de normalidade (p <= 0.05).\n")}
  
  cat("\nb) Teste de Homogeneidade de Variâncias (Fligner-Killeen):\n")
  fligner_test_result <- fligner.test(Retorno ~ Acao, data = retornos_long_df_para_teste)
  p_valor_fligner <- fligner_test_result$p.value
  premissa_homocedasticidade <- p_valor_fligner > 0.05
  print(fligner_test_result)
  if (premissa_homocedasticidade) {cat("-> Resultado: As variâncias são homogêneas entre os grupos (p > 0.05).\n")} else {cat("-> Resultado: As variâncias não são homogêneas entre os grupos (p <= 0.05).\n")}
  
  # --- Seção 3: Teste de Hipóteses ---
  cat("\n--- 3. Teste de Hipóteses ---\n")
  if (premissa_normalidade && premissa_homocedasticidade) {
    cat("--> Premissas satisfeitas. Usando ANOVA e Tukey HSD.\n\n")
    anova_result <- aov(Retorno ~ Acao, data = retornos_long_df_para_teste)
    cat("Resultado da ANOVA:\n"); print(summary(anova_result))
    p_valor_anova <- summary(anova_result)[[1]][["Pr(>F)"]][1]
    if (p_valor_anova < 0.05) {cat("\nANOVA é significativa. Realizando Teste Post-Hoc de Tukey HSD:\n"); print(TukeyHSD(anova_result))} else {cat("\nANOVA não é significativa.\n")}
  } else {
    cat("--> Uma ou mais premissas foram violadas. Usando Kruskal-Wallis e Teste de Dunn.\n\n")
    kruskal_result <- kruskal.test(Retorno ~ Acao, data = retornos_long_df_para_teste)
    cat("Resultado do Teste de Kruskal-Wallis:\n"); print(kruskal_result)
    if (kruskal_result$p.value < 0.05) {cat("\nKruskal-Wallis é significativo. Realizando Teste Post-Hoc de Dunn:\n"); print(dunn.test(retornos_long_df_para_teste$Retorno, retornos_long_df_para_teste$Acao, method = "bonferroni"))} else {cat("\nKruskal-Wallis não é significativo.\n")}
  }
  
  # --- Seção 4: Conclusão e Recomendação ---
  cat("\n--- 4. Conclusão e Recomendação ---\n")
  melhor_acao <- desc_stats %>% arrange(desc(Indice_Sharpe)) %>% slice(1)
  cat(paste0("\n> Recomendação para ", basename(file_path), ":\n"))
  cat(paste0("A análise sugere que a **", melhor_acao$Acao, "** apresentou o melhor desempenho histórico ajustado ao risco (Maior Índice de Sharpe = ", round(melhor_acao$Indice_Sharpe, 4), ").\n"))
  cat("A decisão final deve ser confirmada pela significância estatística do teste post-hoc (se aplicável).\n")
}

#-------------------------------------------------------------------------------
# ETAPA 3: EXECUÇÃO DA ANÁLISE PARA TODOS OS GRUPOS
#-------------------------------------------------------------------------------
lista_arquivos <- c(
  "DadosAcoesGrupoA.csv", "DadosAcoesGrupoB.csv", "DadosAcoesGrupoC.csv",
  "DadosAcoesGrupoD.csv", "DadosAcoesGrupoE.csv", "DadosAcoesGrupoF.csv",
  "DadosAcoesGrupoG.csv", "DadosAcoesGrupoH.csv"
)

# Loop para executar a função de análise para cada arquivo
for (arquivo in lista_arquivos) {
  if (file.exists(arquivo)) {
    analisar_grupo(arquivo, rf_rates_vector = taxas_selic_mensais_vetor_35)
  } else {
    cat(paste("\nArquivo não encontrado:", arquivo, "- Pulando para o próximo.\n"))
  }
}