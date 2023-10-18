getwd()

library(readxl)
pnad = read_excel("E:/Machine Learning_CP/caret_supervisionada_hm/pnad_excel.xlsx")


# pacotes
pacman::p_load(
  caret, ggplot2, plotly, rattle
)
library(dplyr)

#criar banco pnad_cens
pnad_cens <- pnad %>% 
  filter(A002_idade >= 20 & A002_idade <=40) %>%
  filter(C01012_rendimento_maior14 <= 5195) #cinco sm em 2020


pnad_cens <- pnad_cens %>% 
  mutate (
    idade_grupos = case_when(
      A002_idade >=  20 &  A002_idade < 30 ~ "20-29",
      A002_idade >=  30 & A002_idade <= 40 ~ "30-40",
      is.na(A002_idade) ~ NA
    )
  )

# Pré-processamento
particao_pnad_cens = createDataPartition(1:nrow(pnad_cens), p=0.7) # cria a partição 70-30
treino_pnad_cens = pnad_cens[particao_pnad_cens$Resample1, ] # treino
teste_pnad_cens = pnad_cens[-particao_pnad_cens$Resample1, ] # - treino = teste

# Controle de treinamento
train.control <- trainControl(method = "cv", number = 100, verboseIter = T) # controle de treino

# Mineração e predição com Árvores de Decisão
## Árvore de Decisão
pnad_cens_RPART <- train(
  idade_grupos ~ A005_escolaridade + C01012_rendimento_maior14 + A003_sexo + A004_cor_raca,
  data = treino_pnad_cens, 
  method = "rpart", 
  trControl = train.control,
  tuneGrid = expand.grid(cp = c(0.00362, runif(19, 0, 0.25)))
  # , tuneLength = 20
)

plot(pnad_cens_RPART)

fancyRpartPlot(pnad_cens_RPART$finalModel) # desenho da árvore

plot(varImp(pnad_cens_RPART)) # importância das variáveis

predicaoTree = predict(pnad_cens_RPART, newdata = teste_pnad_cens)

postResample(teste_pnad_cens[ , 7], predicaoTree) # teste de performance da Árvore Condicional

base_avaliacao <- data.frame(
  Observado = teste_pnad_cens[ , 7],
  Predição = predicaoTree)

predicao_arvore <- base_avaliacao %>% 
  ggplot(aes(x=Observado, y=Predição)) + 
  geom_point() + # cria os pontos
  geom_smooth() # cria a curva de associação
ggplotly(predicao_arvore)
