#--------------------------------------------------------------------------
# PASSO 1: INSTALAR E CARREGAR OS PACOTES NECESSÁRIOS
#--------------------------------------------------------------------------
# Se for a primeira vez, remova o '#' da linha abaixo para instalar os pacotes
# install.packages(c("lme4", "dplyr", "readxl", "tidyr"))

library(lme4)
library(dplyr)
library(readxl)
library(tidyr)


#--------------------------------------------------------------------------
# PASSO 2: CARREGAR E PREPARAR OS DADOS
#--------------------------------------------------------------------------
# Certifique-se de que o arquivo Excel está na sua pasta de trabalho (working directory)
caminho_arquivo <- "brix_completo_todos.xlsx"
dados_originais <- readxl::read_excel(caminho_arquivo)

# Transforma os dados do formato 'largo' para o 'longo'
dados_long <- pivot_longer(dados_originais,
                           cols = c(primeira, segunda, terceira, quarta),
                           names_to = "coleta",
                           values_to = "brix")

# Limpeza completa e conversão dos tipos de dados
dados_long$clone <- as.factor(dados_long$clone)
dados_long$bloco <- as.factor(dados_long$bloco)
dados_long$coleta <- as.factor(dados_long$coleta)
dados_long$brix <- as.character(dados_long$brix) # Converte para texto primeiro
dados_long$brix <- gsub(",", ".", dados_long$brix) # Substitui vírgula por ponto
dados_long$brix <- as.numeric(dados_long$brix) # Converte para número

# Remove linhas com dados ausentes (NA) e o outlier identificado anteriormente
dados_long <- na.omit(dados_long)
dados_long <- dados_long %>% filter(brix < 100)

# Cria a variável 'grupo' para identificar as testemunhas (pj)
dados_long <- dados_long %>%
  mutate(grupo = as.factor(ifelse(clone %in% c("7825", "5952"), "Testemunha", "Clone")))


#--------------------------------------------------------------------------
# PASSO 3: AJUSTAR O MODELO ESTATÍSTICO CORRIGIDO
#--------------------------------------------------------------------------
# Modelo simplificado, removendo o termo de interação clone:coleta
cat("\n--- Ajustando o modelo misto final para °Brix... ---\n")
modelo_brix_final <- lmer(brix ~ grupo + coleta + (1 | clone) + (1 | bloco),
                          data = dados_long)

# Exibe o resumo completo do novo modelo (sem o aviso de 'singular fit')
print(summary(modelo_brix_final))


#--------------------------------------------------------------------------
# PASSO 4: CALCULAR A HERDABILIDADE (H²) COM O MODELO CORRIGIDO
#--------------------------------------------------------------------------
variancias <- as.data.frame(VarCorr(modelo_brix_final))

# Extrai os componentes de variância do novo modelo
vg <- variancias[variancias$grp == "clone", "vcov"]
ve <- variancias[variancias$grp == "Residual", "vcov"]

# Obtém o número de ambientes (coletas) e repetições (blocos)
n_coletas <- n_distinct(dados_long$coleta)
n_blocos <- n_distinct(dados_long$bloco)

# Calcula a Herdabilidade com a fórmula ajustada para o modelo sem interação
# O erro agora está dividido pelo número total de parcelas (blocos * coletas)
h2 <- vg / (vg + (ve / (n_blocos * n_coletas)))

cat("\n------------------------------------------------\n")
cat("Herdabilidade (H²) Final na média do clone:", round(h2, 4), "\n")
cat("------------------------------------------------\n\n")


#--------------------------------------------------------------------------
# PASSO 5: SELEÇÃO DOS 40 MELHORES CLONES (BLUPs) COM O MODELO CORRIGIDO
#--------------------------------------------------------------------------
# Extrai os efeitos aleatórios (BLUPs) do novo modelo
blups <- ranef(modelo_brix_final)$clone

# Adiciona o intercepto geral (μ) para obter o valor genético final
intercepto <- fixef(modelo_brix_final)["(Intercept)"]
valores_geneticos <- blups$`(Intercept)` + intercepto

# Cria um dataframe com os resultados para o ranking
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

#--------------------------------------------------------------------------
# PASSO 6: SELEÇÃO E VISUALIZAÇÃO DOS 40 PIORES CLONES
#--------------------------------------------------------------------------
# Ordena o ranking em ordem CRESCENTE (do menor para o maior valor)
ranking_clones_piores <- ranking_clones %>%
  arrange(Valor_Genetico_Final)

# Seleciona os 40 primeiros da lista (que agora são os piores)
bottom_40_clones <- head(ranking_clones_piores, 40)

print("--- Top 40 Piores Clones Selecionados (Modelo Final) ---")
print(bottom_40_clones)
