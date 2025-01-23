# Instalar o pacote stringr (se ainda não tiver instalado)
install.packages("stringr")

# Carregar o pacote
library(stringr)

#Escolher o diretório onde estão os seus dados
setwd(choose.dir())


#Chamar os dados
dados <- read.csv2("dados.csv.CSV")
dados

############################################## Item A ###################################################
# Criar uma nova coluna "Data" com os últimos 10 caracteres
dados$Data <- substr(dados$seqName, nchar(dados$seqName) - 9, nchar(dados$seqName))

# Criar uma nova coluna "Alteracoes" com a soma das colunas 12, 13 e 14
dados$Alteracoes <- rowSums(dados[, c(14, 15, 16)], na.rm = TRUE)

# Reorganizar as colunas, colocando "Data" e "Alteracoes" nas posições 3 e 4
dados <- dados[, c(1:2, which(colnames(dados) == "Data"), setdiff(1:ncol(dados), c(1:2, which(colnames(dados) == "Data"))))]

dados <- dados[, c(1:3, which(colnames(dados) == "Alteracoes"), setdiff(1:ncol(dados), c(1:3, which(colnames(dados) == "Alteracoes"))))]

#Conferir se deu certo as reorganizações
colnames(dados)

# Garantir que a coluna "Data" seja do tipo Date
dados$Data <- as.Date(dados$Data)

# Garantir que "Alteracoes" seja numérica
dados$Alteracoes <- as.numeric(dados$Alteracoes)

# Criar o gráfico de linhas
plot(dados$Data, dados$Alteracoes, type = "l", col = "pink", 
     xlab = "Data", ylab = "Alterações", main = "Gráfico de Linhas de Alterações ao Longo do Tempo")

# Adicionar o eixo X com intervalos mensais
axis(1, at = seq(min(dados$Data), max(dados$Data), by = "month"), 
     labels = format(seq(min(dados$Data), max(dados$Data), by = "month"), "%b %Y"))

################################################ Item B ##################################################
# Usar uma expressão regular para extrair o estado
dados$Estados <- str_extract(dados[["seqName"]], "\\b[A-Z]{2}\\b")
dados <- dados[, c(1:4, which(colnames(dados) == "Estados"), setdiff(1:ncol(dados), c(1:4, which(colnames(dados) == "Estados"))))]

colnames(dados)

# Contar as ocorrências de cada estado
contagem_estados <- table(dados$Estados)

# Transformar a tabela em um dataframe para visualização e manipulação
dados_contagem <- as.data.frame(contagem_estados)
colnames(dados_contagem) <- c("Estado", "Frequencia")

# Instalar e carregar o pacote ggplot2, se necessário
install.packages("ggplot2")
library(ggplot2)

# Criar um gráfico de barras com ggplot2
ggplot(dados_contagem, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "pink", color = "black") +
  labs(title = "Frequência de Estados", x = "Estado", y = "Freq") +
  theme_minimal()
