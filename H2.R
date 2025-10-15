
library(lme4)
library(dplyr)
library(readxl)
library(tidyr)


caminho_arquivo <- "brix_completo_todos.xlsx"
dados_originais <- readxl::read_excel(caminho_arquivo)

dados_long <- pivot_longer(dados_originais,
                           cols = c(primeira, segunda, terceira, quarta),
                           names_to = "coleta",
                           values_to = "brix")

dados_long$clone <- as.factor(dados_long$clone)
dados_long$bloco <- as.factor(dados_long$bloco)
dados_long$coleta <- as.factor(dados_long$coleta)
dados_long$brix <- as.character(dados_long$brix) # Converte para texto primeiro
dados_long$brix <- gsub(",", ".", dados_long$brix) # Substitui vírgula por ponto
dados_long$brix <- as.numeric(dados_long$brix) # Converte para número

dados_long <- na.omit(dados_long)
dados_long <- dados_long %>% filter(brix < 100)


dados_long <- dados_long %>%
  mutate(grupo = as.factor(ifelse(clone %in% c("7825", "5952"), "Testemunha", "Clone")))


#--------------------------------------------------------------------------
# PASSO 3: MODELO ESTATÍSTICO
#--------------------------------------------------------------------------
# Modelo simplificado, removendo o termo de interação clone:coleta
cat("\n--- Ajustando o modelo misto final para °Brix... ---\n")
modelo_brix_final <- lmer(brix ~ grupo + coleta + (1 | clone) + (1 | bloco),
                          data = dados_long)

print(summary(modelo_brix_final))


#--------------------------------------------------------------------------
# PASSO 4: CALCULAR A HERDABILIDADE (H²)
#--------------------------------------------------------------------------
variancias <- as.data.frame(VarCorr(modelo_brix_final))

# Extrai os componentes de variância
vg <- variancias[variancias$grp == "clone", "vcov"]
ve <- variancias[variancias$grp == "Residual", "vcov"]

# Obtém o número de ambientes (coletas) e repetições (blocos)
n_coletas <- n_distinct(dados_long$coleta)
n_blocos <- n_distinct(dados_long$bloco)

# Calcula a Herdabilidade 
h2 <- vg / (vg + (ve / (n_blocos * n_coletas)))
cat("Herdabilidade (H²) =", round(h2, 4), "\n")

# PASSO 5: SELEÇÃO DOS 40 MELHORES CLONES

blups <- ranef(modelo_brix_final)$clone

# Adiciona o intercepto geral (μ) para obter o valor genético final
intercepto <- fixef(modelo_brix_final)["(Intercept)"]
valores_geneticos <- blups$`(Intercept)` + intercepto

# Cria o ranking
ranking_clones <- data.frame(
  Clone = rownames(blups),
  BLUP_Efeito_Aleatorio = blups$`(Intercept)`,
  Valor_Genetico_Final = valores_geneticos
)

# Ordena o ranking do maior para o menor valor genético
ranking_clones_ordenado <- ranking_clones %>%
  arrange(desc(Valor_Genetico_Final))

# Seleciona e exibe os 40 melhores clones
top_40_clones <- head(ranking_clones_ordenado, 40)

print("--- Top 40 Melhores Clones Selecionados (Modelo Final) ---")
print(top_40_clones)


# 40 PIORES CLONES

# Ordena o ranking em ordem CRESCENTE
ranking_clones_piores <- ranking_clones %>%
  arrange(Valor_Genetico_Final)

# Seleciona os 40 primeiros da lista
bottom_40_clones <- head(ranking_clones_piores, 40)

print("--- Top 40 Piores Clones ---")
print(bottom_40_clones)

#fim
