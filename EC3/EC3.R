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
## Definindo numero de grupos b / Tamanho amostral e poder

power.anova.test(groups = 2,
                 between.var = (0.5^2 * 1^2) / 2,
                 within.var  = 1,
                 sig.level   = 0.05,
                 power       = 0.80)
# Resultado: n = 63,77 -> 64 amostras necessárias para cada algoritmo (grupos) 

# Agrupamento das 149 instâncias em 64 blocos randomizados
inst_vector <- 2:150                     # as 149 instâncias
n_blocks <- 64                           # vindo do power.anova.test()

n <- length(inst_vector)                 # 149
q <- n %/% n_blocks                      # 2
r <- n %% n_blocks                       # 21

# vetor de tamanhos: 21 blocos com 3 instâncias, 43 blocos com 2 instâncias
sizes <- c(rep(q+1, r), rep(q, n_blocks - r))

# embaralha e separa
inst_shuffle <- sample(inst_vector)
blocos <- split(inst_shuffle, rep(1:n_blocks, times = sizes))

# verificação (tem que aparecer 64 grupos e soma = 149)
length(blocos)
sum(sapply(blocos, length))
sapply(blocos, length)


################################################################################
## Gerando funcoes de Rosenbrock de dimensão de 2 a 150

suppressWarnings(suppressPackageStartupMessages(library(smoof)))

instancias <- list()

for (dim in 2:150){
  fn <- local({ 
    d <- dim 
    function(X){
      if(!is.matrix(X)) X <- matrix(X, nrow = 1) # <- if a single vector is passed as X
      Y <- apply(X, MARGIN = 1,
                FUN = smoof::makeRosenbrockFunction(dimensions = d))
      return(Y)
    }
  })
  
  # parâmetros do problema
  selpars  <- list(name = "selection_standard")
  stopcrit <- list(names = "stop_maxeval", maxevals = 5000 * dim, maxiter = 100 * dim)
  probpars <- list(name = "fn", xmin = rep(-5, dim), xmax = rep(10, dim))
  popsize  <- 5 * dim 
  
  
  # salva a função como uma instância na lista
  instancias[[paste0("dim_", dim)]] <- list(
    fn       = fn,
    selpars  = selpars,
    stopcrit = stopcrit,
    probpars = probpars,
    popsize  = popsize
  )
}  


################################################################################
## Ajuste de Parâmetros das configurações dos algoritmos

# Equipe E

## Config 1
recpars1 <- list(name = "recombination_lbga")
mutpars1 <- list(name = "mutation_rand", f = 4.5)

## Config 2
recpars2 <- list(name = "recombination_blxAlphaBeta", alpha = 0.1, beta = 0.4)
mutpars2 <- list(name = "mutation_rand", f = 3)

################################################################################
## Definicao funcao para rodar uma combinação (algoritmo × instância × replicação)

suppressPackageStartupMessages(library(ExpDE))

run_one <- function(alg_label, inst, seed = NULL, mutpars, recpars){
  if(!is.null(seed)) set.seed(seed)
  
  # Le os parâmetros da instância
  selpars  <- inst$selpars
  stopcrit <- inst$stopcrit
  probpars <- inst$probpars
  popsize  <- inst$popsize
  
  # Disponibiliza a função objetivo com o nome 'fn' no GlobalEnv
  assign("fn", inst$fn, envir = .GlobalEnv)
  
  # Executa o algoritmo
  out <- ExpDE(mutpars = mutpars,
               recpars = recpars,
               popsize = popsize,
               selpars = selpars,
               stopcrit = stopcrit,
               probpars = probpars,
               showpars = list(show.iters = "none"))
  
  # Extrai o valor de interesse
  data.frame(Algorithm = alg_label, Result = out$Fbest)
}


################################################################################
## Teste piloto para encolher n_runs

library(e1071) # skewness

# blocos piloto (ex: 10 blocos aleatórios)
blocos_piloto_ids <- sample(1:length(blocos), 10)
blocos_piloto <- blocos[blocos_piloto_ids]

candidates <- c(5) #c(1,2,3,5,7,10,15,20,30)

results_symmetry <- data.frame(
  n_runs = candidates,
  skewness = NA,
  shapiro_p = NA
)

for(i in seq_along(candidates)){
  n_test <- candidates[i]
  diffs <- c()
  
  for(b in blocos_piloto){
    dims <- b
    
    for(r in 1:n_test){
      d <- sample(dims, 1)
      inst <- instancias[[paste0("dim_", d)]]
      
      a1 <- run_one("Alg1", inst, seed = NULL, mutpars = mutpars1, recpars = recpars1)
      a2 <- run_one("Alg2", inst, seed = NULL, mutpars = mutpars2, recpars = recpars2)
      
      diffs <- c(diffs, a2$Result - a1$Result)
    }
  }
  
  results_symmetry$skewness[i] <- skewness(diffs)
  results_symmetry$shapiro_p[i] <- shapiro.test(diffs)$p.value
}

print(results_symmetry)


################################################################################
## Gerando dados - rodando os dois algoritmos nos grupos de instancias do problema


# Replicações por algoritmo x grupo
n_runs <- 30


## Loop principal sobre as instâncias (dimensões 2..150)

res_list <- list()
idx <- 1
raw_results <- data.frame()

pb_outer <- txtProgressBar(min = 0, max = length(blocos), style = 3)

for(b in seq_along(blocos)){
  
  dims_bloco <- blocos[[b]]
  
  resultados_A1 <- c()
  resultados_A2 <- c()
  
  
  # barra progresso
  pb_inner <- txtProgressBar(min = 0, max = n_runs, style = 3)
  cat(sprintf("\nBloco %d/%d | instâncias no bloco: %s\n",
              b, length(blocos), paste(dims_bloco, collapse=", ")))
  flush.console()
    
  for(r in 1:n_runs){
    
    # SORTEIA UMA INSTÂNCIA do bloco para esta repetição
    d <- sample(dims_bloco, 1)
    
    inst <- instancias[[paste0("dim_", d)]]
    
    # Algoritmo 1
    a1 <- run_one(alg_label = "Alg1",
                  inst = inst,
                  seed = NULL,             
                  mutpars = mutpars1,
                  recpars = recpars1)
    
    # Algoritmo 2
    a2 <- run_one(alg_label = "Alg2",
                  inst = inst,
                  seed = NULL,             
                  mutpars = mutpars2,
                  recpars = recpars2)
    
    resultados_A1 <- c(resultados_A1, a1$Result)
    resultados_A2 <- c(resultados_A2, a2$Result)
    raw_results <- rbind(raw_results,
      data.frame(Block = b, Instancia = d, Rep = r, Algorithm="Alg1", Y = a1$Result),
      data.frame(Block = b, Instancia = d, Rep = r, Algorithm="Alg2", Y = a2$Result))
  
    
    setTxtProgressBar(pb_inner, r)
    flush.console()
    
  }
  
  close(pb_inner)
  
  # salva a média do bloco
  res_list[[idx]] <- data.frame(Block=b, Algorithm="Alg1", Y=mean(resultados_A1)); idx <- idx + 1
  res_list[[idx]] <- data.frame(Block=b, Algorithm="Alg2", Y=mean(resultados_A2)); idx <- idx + 1
  
  setTxtProgressBar(pb_outer, b)
  flush.console()
  
}

close(pb_outer)

################################################################################
## Formatando dados gerados

# Caso já exista o arquivo e deseje le-lo
data <- read.table("resultados_rosenbrock.csv",
                   header = TRUE)

# Data final no formato: Block | Algorithm | Mean
data <- do.call(rbind, res_list)


# Ordenar colunas
data$Algorithm <- factor(data$Algorithm, levels = c("Alg1", "Alg2"))
data$Block     <- factor(data$Block, levels = as.character(1:n_blocks))

# Salvar no arquivo .csv
write.csv(data, "resultados_rosenbrock_medias.csv", row.names = FALSE)
write.csv(raw_results, "resultados_rosenbrock_raw.csv", row.names = FALSE)

head(data)
summary(data)

# Agregar por grupo:
aggdata <- data

summary(aggdata)

################################################################################
## Analise exploratoria dos dados

library(ggplot2)

# Plot grafico de comparacao do algoritmo para todos os pontos
p <- ggplot(aggdata, aes(x = Block, 
                         y = Y, 
                         group = Algorithm, 
                         colour = Algorithm))
p + geom_line(linetype=2) + geom_point(size=2)

################################################################################
## Modelagem estatistica

model <- aov(Y~Algorithm+Block,
             data = aggdata)

summary(model)
summary.lm(model)$r.squared

# Testes gráficos para ver normalidade e independencia
par(mfrow = c(2, 2))
plot(model, pch = 20, las = 1)

# Eficiencia da blocagem
mydf        <- as.data.frame(summary(model)[[1]])
MSblocks    <- mydf["Block","Mean Sq"]
MSe         <- mydf["Residuals","Mean Sq"]
a           <- length(unique(aggdata$Algorithm))
b           <- length(unique(aggdata$Block))
((b - 1) * MSblocks + b * (a - 1) * MSe) / ((a * b - 1) * MSe)



################################################################################
## Validacao de Premissas e Teste Pareado

res <- residuals(model)

# Shapiro - normalidade dos residuos
shapiro.test(res)

# Diferenças por grupo (par por dimensão)
wide <- reshape(aggdata, idvar="Block", timevar="Algorithm", direction="wide")
wide$diff <- wide$Y.Alg2 - wide$Y.Alg1

par(mfrow = c(1,3))
hist(wide$diff, breaks = 15, main = "Histograma das diferenças", xlab = "Alg2 - Alg1")
qqnorm(wide$diff); qqline(wide$diff, col = "red")
boxplot(wide$diff, main = "Boxplot das diferenças", ylab = "Alg2 - Alg1")
par(mfrow = c(1,1))

# Shapiro - normalidade das diferenças pareadas por grupo
shapiro.test(wide$diff)  #resultado: p-value = 0.008303 - rejeita normalidade

# Teste pareado para as diferenças (caso normalidade OK)
#t.test(wide$Y.Alg2, wide$Y.Alg1, paired = TRUE)

# Teste pareado para as diferenças (caso normalidade NOK)
wilcox.test(wide$Y.Alg2, wide$Y.Alg1, paired = TRUE)

#Tamanho do efeito
diff_mean <- mean(wide$diff)
diff_sd   <- sd(wide$diff)
cohen_d <- diff_mean / diff_sd
cohen_d

################################################################################
## Teste Post-Hoc

library(multcomp)

duntest     <- glht(model,
                    linfct = mcp(Algorithm = "Dunnett"))

summary(duntest)

duntestCI   <- confint(duntest)
par(mar = c(5, 8, 4, 2), las = 1)
plot(duntestCI,
     xlab = "Mean difference (log scale)")


################################################################################
## Ridgeline dos dados gerados por dimensão x algoritmo

library(ggplot2)
library(ggridges)

ggplot(raw_results, aes(x = Y, 
                        y = as.factor(Instancia), 
                        fill = Algorithm)) +
  geom_density_ridges(alpha = 0.6, scale = 2, rel_min_height = 0.01, color = "black") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), 
                    name = "Algorithm",
                    labels = c("Alg1", "Alg2")) +
  labs(
    title = "Distribuição estimada dos resultados",
    subtitle = "para cada instância (dimensão) e algoritmo",
    x = "Resultado (Y)",
    y = "Instância (dimensão)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 7)
  )


