#' Checking whether functions work across different types of objects
#'
#' @param functions blah
#' @param objects blah
#' @examples
#'  library(manynet)
#'  functions <- c("node_in_optimal", "node_in_partition", "node_in_infomap",
#'                "node_in_spinglass", "node_in_fluid", "node_in_louvain", 
#'                "node_in_leiden", "node_in_betweenness", "node_in_greedy", 
#'                "node_in_eigen", "node_in_walktrap")
#'  objects <- table_data() %>% 
#'                 dplyr::distinct(directed, weighted, twomode, 
#'                 labelled, signed, .keep_all = TRUE) %>% 
#'                 dplyr::pull(dataset)
#'  functions_work(functions, objects)
#' @export
functions_work <- function(functions, objects){
  opts <- expand.grid(functions, objects)
  out <- apply(opts, 1, function(x) !berryFunctions::is.error(get(x[1])(get(x[2]))))
  out <- matrix(out, nrow = length(functions))
  rownames(out) <- functions
  colnames(out) <- objects
  out
}