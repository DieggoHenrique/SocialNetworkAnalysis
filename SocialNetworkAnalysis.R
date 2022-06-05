# Fonte:https://neo4j.com/blog/sentiment-social-network-analysis/
rm(list = ls())
conexoes <- readxl::read_excel('Modelo1B.xlsx');conexoes

# Tranformando em um DataFrame
conexoes <- as.data.frame(conexoes)

# A primeira linha é um indice
conexoes1 <- conexoes[,2:16]
# Colocando a primeira linha do df original como o nome das linhas
rownames(conexoes1) <- conexoes[,1];conexoes1


library(network)
library(sna)

gplot(conexoes1,            # Nome da rede
      gmode = "twonode",      # Tipo da vizualzação
      displaylabels = TRUE, 
      edge.col = "gray",
      usearrows=FALSE,
      label.cex = 0.7,
      vertex.cex = degree(conexoes1, gmode="twomode")/5)

# Quem é importante?
min(degree(conexoes1));max(degree(conexoes1))



# Cálculo da aproximidade::Closeness 
name <- rowSums(conexoes1);name
aproximacao <- closeness(conexoes1)
aproximacao1 <- name + aproximacao 
aproximacao2 <- aproximacao1 - name;aproximacao2

# Cálculo da Intermediação::Betweenness
intermediacao <- betweenness(conexoes1)
intermediacao1 <- name + intermediacao
intermediacao2 <- intermediacao1 - name;intermediacao2

aproximacao2;max(aproximacao2)
intermediacao2;max(intermediacao2)


# Questão 2-B  -----------------------------------------------------------------

rm(list = ls())
clique <- readxl::read_excel('Modelo1B.xlsx', sheet = 'Planilha2');clique
clique <- as.data.frame(clique)
grede3 <- clique[,2:14]
rownames(grede3) <- clique[,1]


gplot(grede3,gmode="graph",displaylabels = TRUE,edge.col="gray",usearrows=FALSE)
gden(grede3, g=NULL, diag=FALSE, mode="graph", ignore.eval=FALSE)
network.density(network(grede3))
efficiency(grede3, g=NULL, diag=FALSE)
connectedness(grede3)
mutuality(grede3)

# Cliques
clique.census(grede3, mode = "graph", tabulate.by.vertex = FALSE,
              clique.comembership = "sum", enumerate = FALSE,
              na.omit = TRUE)


