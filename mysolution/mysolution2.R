library(igraph)
g <- barabasi.game(1000)
layout <- layout_with_fr(g)
plot(g, layout=layout, vertex.size=2,
     vertex.label=NA, edge.arrow.size=.2)
# Węzeł o maksymalnym betweenness
V(g)[betweenness(g)==max(betweenness(g))]
# Średnica
diameter(g)
# W modelu Barabási-Albert dodawane w czasie konstrukcji grafu węzły są łączone 
# z istniejącymi węzłami z prawdopodobieństwem proporcjonalnym do ich stopnia,
# co naturalnie prowadzi do tworzenie się hubów o dużej liczbie połączeń i
# peryferii złożonych z dużej liczby węzłów o małym stopniu.
# W modelu Erdős-Rényi każda możliwa krawędź ma jednakowe prawdopodobieństwo 
# zaistnienia, niezależnie od innych. Prowadzi to do sieci, w której rozkład
# stopni węzłów jest zbliżony do normalnego