# clean workspace
rm(list=ls())

# install required packages if needed
packages_needed <- c("stringr","ggplot2", "multcomp")
for (package_name in packages_needed) {      
  if (!(package_name %in% rownames(installed.packages()))){
    install.packages(package_name)
  }
}

################################################################################
## Gerando funcoes de Rosenbrock de dimensão de 2 a 250

suppressWarnings(suppressPackageStartupMessages(library(smoof)))

instancias <- list()

for (dim in 2:250){
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

# testing the function on a matrix composed of 2 points
#X <- matrix(runif(2 * 250, min=-5, max=10), nrow = 2)
#instancias[["dim_250"]]$fn(X)
#instancias[["dim_250"]]$selpars
#instancias[["dim_250"]]$probpars
#instancias[["dim_250"]]$stopcrit
#instancias[["dim_250"]]$popsize


################################################################################
## Ajuste de Parâmetros das configurações

# Equipe E

## Config 1
recpars1 <- list(name = "recombination_lbga")
mutpars1 <- list(name = "mutation_rand", f = 4.5)

## Config 2
recpars2 <- list(name = "recombination_blxAlphaBeta", alpha = 0.1, beta = 0.4)
mutpars2 <- list(name = "mutation_rand", f = 3)

################################################################################
## Gerando dados - rodando os dois algoritmos nas instancias do problema

suppressPackageStartupMessages(library(ExpDE))

# Replicações por algoritmo x instância
n_runs <- 1

# Função para rodar uma combinação (algoritmo × instância × replicação)
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

# Loop principal sobre as instâncias (dimensões 2..250)
res_list <- list()
idx <- 1

for(d in 2:250){
  print("Rodando dimensão:", d, "\n")
  inst <- instancias[[paste0("dim_", d)]]
  
  for(r in 1:n_runs){
    # Algoritmo 1
    a1 <- run_one(alg_label = "Alg1",
                  inst = inst,
                  seed = NULL,             
                  mutpars = mutpars1,
                  recpars = recpars1)
    a1$Group      <- as.character(d)   # "Group" = dimensão (como seu agrupamento)
    a1$Replication <- r
    
    # Algoritmo 2
    a2 <- run_one(alg_label = "Alg2",
                  inst = inst,
                  seed = NULL,             
                  mutpars = mutpars2,
                  recpars = recpars2)
    a2$Group      <- as.character(d)
    a2$Replication <- r
    
    res_list[[idx]]   <- a1; idx <- idx + 1
    res_list[[idx]]   <- a2; idx <- idx + 1
  }
}

################################################################################
## Formatando dados gerados

# Caso já exista o arquivo e deseje le-lo
data <- read.table("resultados_rosenbrock.csv",
                   header = TRUE)

# Data final no formato: Algorithm | Result | Group | Replication
data <- do.call(rbind, res_list)


# Ordenar colunas
data$Algorithm <- factor(data$Algorithm, levels = c("Alg1", "Alg2"))
data$Group     <- factor(data$Group, levels = as.character(2:249))

# Salvar no arquivo .csv
write.csv(data, "resultados_rosenbrock.csv", row.names = FALSE)

head(data)
summary(data)

# Agregar por grupo:
aggdata <- with(data,
                aggregate(x = Result,
                          by = list(Algorithm, Group),
                          FUN = mean))
names(aggdata) <- c("Algorithm", "Instance_Group", "Y")
for (i in 1:2) aggdata[, i] <- as.factor(aggdata[, i])

summary(aggdata)

################################################################################
## Analise exploratoria dos dados

library(ggplot2)

# Plot grafico de comparacao do algoritmo para todos os pontos
p <- ggplot(aggdata, aes(x = Instance_Group, 
                         y = Y, 
                         group = Algorithm, 
                         colour = Algorithm))
p + geom_line(linetype=2) + geom_point(size=2)

################################################################################
## Modelagem estatistica

model <- aov(Y~Algorithm+Instance_Group,
             data = aggdata)

summary(model)
summary.lm(model)$r.squared

# Testes gráficos para ver normalidade e independencia
par(mfrow = c(2, 2))
plot(model, pch = 20, las = 1)

# Eficiencia da blocagem
mydf        <- as.data.frame(summary(model)[[1]])
MSblocks    <- mydf["Instance_Group","Mean Sq"]
MSe         <- mydf["Residuals","Mean Sq"]
a           <- length(unique(aggdata$Algorithm))
b           <- length(unique(aggdata$Instance_Group))
((b - 1) * MSblocks + b * (a - 1) * MSe) / ((a * b - 1) * MSe)



################################################################################
## Validacao de Premissas

res <- residuals(model)

# Shapiro - normalidade dos residuos
shapiro.test(res)

# Diferenças por grupo (par por dimensão)
wide <- reshape(aggdata, idvar="Instance_Group", timevar="Algorithm", direction="wide")
wide$diff <- wide$Y.Alg2 - wide$Y.Alg1

# Shapiro - normalidade das diferenças pareadas por grupo
shapiro.test(wide$diff)

wilcox.test(wide$Y.Alg2, wide$Y.Alg1, paired = TRUE)

################################################################################
## Rejeitada H0 - Teste Post-Hoc

library(multcomp)

duntest     <- glht(model,
                    linfct = mcp(Algorithm = "Dunnett"))

summary(duntest)

duntestCI   <- confint(duntest)
par(mar = c(5, 8, 4, 2), las = 1)
plot(duntestCI,
     xlab = "Mean difference (log scale)")


################################################################################
## Tamanho amostral e poder

# Poder para teste t pareado (efeito padronizado d*=0.5)
power.t.test(delta = 0.5, sd = 1, sig.level = 0.05, power = 0.80,
             type = "paired", alternative = "two.sided")
## Resultado: 34 instancias (testar agrupar os 249 em grupos de 7 instancias - 35 grupos)
