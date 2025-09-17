library(tidyverse)

# Carga e Preparação dos Dados
dados_longos <- read_excel("brix_completo_todos.xlsx") %>%
  pivot_longer(
    cols = c(primeira, segunda, terceira, quarta),
    names_to = "coleta_nome",
    values_to = "brix"
  ) %>%
  mutate(
    brix = gsub(",", ".", brix),
    brix = as.numeric(brix),
    coleta = factor(coleta_nome, levels = c("primeira", "segunda", "terceira", "quarta")),
    bloco = as.factor(bloco),
    clone = as.factor(clone)
  ) %>%
  filter(!is.na(brix)) %>%
  select(clone, bloco, coleta, brix)

# Separação dos Dados para o Gráfico
dados_pais <- dados_longos %>%
  filter(clone %in% c("7825", "5952"))

dados_outros_clones <- dados_longos %>%
  filter(!clone %in% c("7825", "5952"))

# Geração do Gráfico
grafico_dispersao <- ggplot(mapping = aes(x = coleta, y = brix)) +
  geom_jitter(
    data = dados_outros_clones,
    width = 0.25,
    alpha = 0.3,
    color = "grey60",
    size = 2
  ) +
  geom_jitter(
    data = dados_pais,
    mapping = aes(color = clone),
    width = 0.25,
    size = 3.5,
    alpha = 0.8
  ) +
  stat_summary(
    data = dados_longos,
    mapping = aes(group = 1),
    fun = "mean",
    geom = "line",
    color = "black",
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = c("7825" = "blue", "5952" = "gold")
  ) +
  scale_y_continuous(
    limits = c(5, 26),
    breaks = c(5, 10, 15, 20, 26)
  ) +
  labs(
    title = "Desempenho dos Clones ao Longo das Coletas",
    subtitle = "Pontos cinzas: clones individuais. Linha preta: média geral. Pontos coloridos: pais.",
    x = "Época de Coleta",
    y = "°Brix",
    color = "Pais"
  ) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom")

# Exibição do Gráfico
print(grafico_dispersao)

# Salvando o Gráfico em Alta Resolução
ggsave(
  "grafico_brix_coletas.png",
  plot = grafico_dispersao,
  width = 10,
  height = 7,
  dpi = 300
)
