library(igraph)
g <- erdos.renyi.game(p.or.m=0.05, n=100)
# Graf nieważony
summary(g)
V(g)
E(g)
E(g)$weight <- runif(length(E(g)), 0.01, 1)
# Pojawia się W jak Weighted 
summary(g)
# Stopnie węzłów
degree(g)
hist(degree(g))
# Liczba komponentów
components(g)$no
# Wizualizacja
pr <- page_rank(g)$vector
plot(g, vertex.size=pr*300,  edge.width=E(g)$weight,
     vertex.label=NA, edge.arrow.size=.2)
.