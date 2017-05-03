library(igraph)

GetDistances <- function(pair) {
  print(pair)
  distance=0
  for (i in sequence(ncol(pair))) {
    if(pair[1,i]!=pair[2,i]) {
      distance=distance+1
    }
  }
  print(distance)
  return(distance)
}

combos <- expand.grid(x=c("a","b"), y=c("a", "b"), z=c("a", "b", "c"))
g <- make_empty_graph()
g <- add.vertices(g, nrow(combos), name = apply(combos, 1, paste, collapse=""))
for (start.node in sequence(nrow(combos))) {
  for (end.node in sequence(nrow(combos)))
    if(GetDistances(combos[c(start.node, end.node),])==1) {
      g <- add.edges(g, c(start.node, end.node))
    }
}

plot.igraph(g, edge.arrow.mode=0)

WhichAreDifferent <- function(pair) {
  distance=0
  which.different=c()
  for (i in sequence(ncol(pair))) {
    if(pair[1,i]!=pair[2,i]) {
      distance=distance+1
      which.different <- c(which.different, i)
    }
  }
  return(which.different)
}

MakeQMatrix <- function(dep.var=NULL, combos=expand.grid(x=c("a","b"), y=c("a", "b"), z=c("a", "b", "c"))) {
  Q <- matrix(0, nrow=nrow(combos), ncol=nrow(combos))
  for (start.node in sequence(nrow(combos))) {
    for (end.node in sequence(nrow(combos))) {
      if(GetDistances(combos[c(start.node, end.node),])==1) {
        if(is.null(dep.var)) {
          Q[start.node, end.node] <- 1+max(Q)
        }
        if(dep.var=="x") {
          different.x <- WhichAreDifferent(combos[c(start.node, end.node),])
          
        }
        if(dep.var=="y") {
          different.y <- WhichAreDifferent(combos[c(start.node, end.node),])
        }
        if(dep.var=="z") {
          different.z <- WhichAreDifferent(combos[c(start.node, end.node),])
        }
      }
   }
}
  return(Q)
}



