library(ggstatsplot)
mydata <- readRDS("Cluster_identity.rds")
x <- ggbetweenstats(
  data = mydata,
  x = km_cluster_identity,
  y = Z.adjusted.T.S.log
)
x


