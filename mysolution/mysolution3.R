library(shiny)
library(bslib)

library(igraph)
dfGraph <- read.csv2("C:\\Users\\Michal\\Downloads\\sieci\\out.radoslaw_email_email", skip=2, sep= " ")[, 1:2]
g <- graph_from_data_frame(dfGraph, directed = TRUE)
g <- simplify(g)
summary(g)

outgoing_subsets <- split(dfGraph, dfGraph$X1)

# Loop over all nodes
for (i in V(g)$name) {
  # Get the subset of dfGraph where X1 == i (all outgoing edges)
  df_out_i <- outgoing_subsets[[i]]
  
  # Skip nodes with no outgoing edges
  if (is.null(df_out_i)) next
  
  denom <- nrow(df_out_i)
  
  # For each possible target node j
  for (j in V(g)$name) {
    # Subset of that smaller df for i → j
    num <- nrow(subset(df_out_i, X2 == j))
    
    # Only assign if an edge actually exists
    if (num > 0) {
      eid <- get_edge_ids(g, c(i, j))
      if (eid > 0) {
        E(g)[eid]$weight <- num / denom
      }
    }
  }
}

run_experiment_iter <- function(g, active_nodes, m) {
  
  n <- vcount(g)
  active     <- rep(FALSE, n)
  activated  <- rep(FALSE, n)
  active[active_nodes] <- TRUE
  
  neigh_list <- lapply(1:n, function(v) neighbors(g, v, mode = "out"))
  edge_weights <- E(g)$weight
  
  iter_results <- c()
  
  while (any(active & !activated)) {
    
    current <- which(active & !activated)
    
    for (i in current) {
      neighs <- neigh_list[[i]]
      
      for (j in neighs) {
        eid <- get_edge_ids(g, c(i, j))
        if (eid > 0 && runif(1) < edge_weights[eid] * m) {
          active[j] <- TRUE
        }
      }
    }
    activated[current] <- TRUE
    iter_results <- c(iter_results, sum(activated))
  }
  return(iter_results)
}

add_pad_final <- function(x, y) {
  n <- max(length(x), length(y))
  x <- c(x, rep(x[length(x) - 1], n - length(x)))
  y <- c(y, rep(y[length(y) - 1], n - length(y)))
  x + y
}

# Define UI ----
ui <- page_sidebar(
  sidebar = sidebar(
    sliderInput(
      "iters",
      "Number of iterations",
      min = 1,
      max = 50,
      value = 10
    ),
    sliderInput(
      "prob",
      "Spread probability multiplier",
      min = 0.1,
      max = 2,
      value = 1
    )
  ),
  plotOutput(outputId = "main_plot", height = "300px"),
)

# Define server logic ----
server <- function(input, output) {
  
  output$main_plot <- renderPlot({
    res_outdegree_max <-  c(0)
    top_nodes <- order(degree(g, mode = "out"), decreasing = TRUE)[1:ceiling(0.05 * vcount(g))]
    for (i in 1:input$iters) {
      res_outdegree_max <- add_pad_final(res_outdegree_max, run_experiment_iter(g,  V(g)[top_nodes], input$prob))
    }
    res_outdegree_max <- res_outdegree_max / input$iters
    
    # Maksymalne betweenness
    res_betweenness <-  c(0)
    top_nodes <- order(betweenness(g), decreasing = TRUE)[1:ceiling(0.05 * vcount(g))]
    for (i in 1:input$iters) {
      res_betweenness <- add_pad_final(res_betweenness, run_experiment_iter(g,  V(g)[top_nodes], input$prob))
    }
    res_betweenness <- res_betweenness / input$iters
    
    # Maksymalne closeness
    res_closeness <-  c(0)
    top_nodes <- order(closeness(g), decreasing = TRUE)[1:ceiling(0.05 * vcount(g))]
    for (i in 1:input$iters) {
      res_closeness <- add_pad_final(res_closeness, run_experiment_iter(g,  V(g)[top_nodes], input$prob))
    }
    res_closeness <- res_closeness / input$iters
    
    # Minimalne outdegree - węzły o najmniejszym outdegree, z pominięciem zer
    res_outdegree_min <-  c(0)
    degrees <- degree(g, mode = "out")
    degrees <- degrees[degrees > 0]
    top_nodes <- order(degrees, decreasing = FALSE)[1:ceiling(0.05 * vcount(g))]
    for (i in 1:input$iters) {
      res_outdegree_min <- add_pad_final(res_outdegree_min, run_experiment_iter(g,  V(g)[top_nodes], input$prob))
    }
    res_outdegree_min <- res_outdegree_min / input$iters
    
    res_random <-  c(0)
    for (i in 1:input$iters) {
      res_random <- add_pad_final(res_random, run_experiment_iter(g,  sample(V(g), ceiling(0.05 * vcount(g))), input$prob))
    }
    res_random <- res_random / input$iters
    
    max_len <- max(length(res_outdegree_max),
                   length(res_betweenness),
                   length(res_closeness),
                   length(res_outdegree_min),
                   length(res_random))
    
    pad <- function(x) c(x, rep(x[length(x) - 1], max_len - length(x)))
    
    matplot(cbind(
      pad(res_outdegree_max),
      pad(res_betweenness),
      pad(res_closeness),
      pad(res_outdegree_min),
      pad(res_random)
    ),
    type = "l", lwd = 2, lty = 1,
    col = c("red", "blue", "green", "purple", "black"),
    xlab = "Iteration", ylab = "Activated nodes",
    main = "Activation Spread Comparison")
    
    legend("bottomright",
           legend = c("Max outdegree", "Max betweenness", "Max closeness",
                      "Min outdegree", "Random"),
           col = c("red", "blue", "green", "purple", "black"),
           lwd = 0.2)
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)