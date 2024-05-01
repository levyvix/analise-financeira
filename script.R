library(pacman)

pacman::p_load("tidyverse", 'readxl', 'corrplot')


dados = read_excel("base_de_dados_31.xlsx")


summary(dados)


# Análise exploratória de dados
# Gráfico de dispersão para explorar relação entre variáveis-chave
plot(dados$Assets, dados$Debt)


# Correlação entre variáveis-chave
correlation_matrix <- cor(dados[c("Revenue", "Cost", "Profit", "Debt", "Assets")])
corrplot(correlation_matrix, method = "circle")
# Maior custo -> maior receita, nao significa maior lucro
# Maior ativo -> maior divida, nao significa mairo lucro

dados %>% ggplot(aes(Cost, Revenue, color = Profit))+
  geom_point()
# Maior custo -> maior receita, mas nao significa maior lucro


# Localização nao é um indicativo de lucro
dados  %>% ggplot(aes(Cost, Profit, color=Location))+
  geom_point()

# Mercado nao é um indicativo de lucro
dados %>% ggplot(aes(Cost, Profit, color = Market))+
  geom_point()

# tecnologia bom e industria ruim?
dados %>% 
  group_by(Market) %>% 
  summarise(
    mean_profit = median(Profit)
  )

dados %>%
  group_by(Location) %>%
  summarise(median_profit = median(Profit))
# porque isso acontece?

# tecnologia é bom ?
# para ativos de alto valor, não vale a pena, pois a taxa de ativos que dão lucro é menor que 50%
# para ativos de baixo valor <30.000, vale a pena, pois a taxa de ativos que dao lucro é maior que 50%
# tecnologia: ativos de baixo custo tem alta chance de dar lucro, diferentes dos ativos de alto custo.

dados_mutated = dados %>% 
  mutate(alto_custo = Cost > 40000,
         profitable = Profit > 0)

dados_mutated %>% filter(Market %in% c("Indústria", "Tecnologia")) %>% ggplot(aes(Profit, fill = Market))+
  geom_histogram(bins = 60)


dados %>% ggplot(aes(Cost)) +
  geom_histogram(bins = 50)

# baixo custo
dados %>% 
  filter(alto_custo == FALSE) %>% 
  group_by(Market) %>% 
  summarise(
    count_m = n(),
    avg_profit = median(Profit),
    avg_debt = median(Debt),
    avg_cost = median(Cost),
    positive_profit_rate = sum(Profit > 0) / count_m
  )

# alto custo
dados %>% 
  filter(alto_custo == TRUE) %>% 
  group_by(Market) %>% 
  summarise(
    count_m = n(),
    avg_profit = mean(Profit),
    avg_debt = mean(Debt),
    avg_cost = mean(Cost),
    positive_profit_rate = sum(Profit > 0) / count_m
  )

# qual é o melhor mercado em cada continente? em taxa de lucro e media de lucro
dados %>% 
  group_by(Market, Location) %>% 
  summarise(
    profitable_rate = sum(profitable) / n(),
    profit_avg = median(Profit)
  ) %>% 
  arrange(desc(profitable_rate))

# olhando o top 10, tecnologia tem uma tendência de alta chance de lucro em varios continentes



# Relação entre Ativos e Dívidas e sua Influência na Lucratividade:
ggplot(dados, aes(x = Assets, y = Profit, color = Debt)) +
  geom_point() +
  scale_x_continuous(labels = scales::dollar_format(suffix = "K")) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "K")) +
  labs(title = "Relação entre Ativos, Lucro e Dívida")

# ativos de alto custo trazem divida maior para o investidor


segment_analysis = dados %>% 
  group_by(Market) %>% 
  summarise(
    mean_profit = mean(Profit)
  )
ggplot(segment_analysis, aes(x = Market, y = mean_profit, fill = mean_profit > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Média de Lucro por Segmento de Mercado",
       x = 'Segmento', y = 'Média de lucro')
# tecnologia é um ótimo segmento para investir


location_analysis = dados %>% 
  group_by(Location) %>% 
  summarise(
    mean_profit = mean(Profit)
  )

ggplot(location_analysis, aes(x = Location, y = mean_profit, fill = mean_profit > 0)) +
  geom_bar(stat = "identity", show.legend=F) +
  labs(title = "Média de Lucro por Localização Geográfica", x = 'Localização', 
       y = 'Média de lucro')
# oceania é um ótimo lugar para investir, seguido por america do norte

