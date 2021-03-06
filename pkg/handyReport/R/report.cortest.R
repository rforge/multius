#' CReport the results of correlation test
#'
#' @description The function reports the results of several (with several variables) correlation tests.
#' @param x The names of the first set of numerical variables.
#' @param y The names of the second set of numerical variables.
#' @param dataset The dataframe with numerical variables.
#' @examples
#' report.cortest(x = "drat", y = c("wt", "disp", "hp"), dataset = mtcars)
#' @author Marjan Cugmas
#' @export

report.cortest <- function(x, y, dataset, method = "pearson"){
  if (method == "pearson") {
    res <- matrix(NA, nrow = length(x)*length(y), ncol = 8)
    colnames(res) <- c("var", "t", "df", "p", "cor", "CI95 low", "CI95 high", "method")
  }
  if (method == "spearman") {
    res <- matrix(NA, nrow = length(x)*length(y), ncol = 5)
    colnames(res) <- c("var", "S", "p", "cor", "method")
  }
  stevec <- 1
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      cor.res <- cor.test(x = as.numeric(dataset[, x[i]]), y = as.numeric(dataset[,y[j]]), method = method, exact = FALSE)
      if (method == "pearson"){
        res[stevec, ] <- c(paste0(x[i], " and ", y[j]),
                           round(cor.res$statistic, 2),
                           ifelse(is.null(cor.res$parameter), yes = "", no = cor.res$parameter),
                           round(cor.res$p.value, 3),
                           round(cor.res$estimate, 2),
                           ifelse(is.null(cor.res$conf.int[1]), yes = "", no = round(cor.res$conf.int[1], 2)),
                           ifelse(is.null(cor.res$conf.int[2]), yes = "", no = round(cor.res$conf.int[2], 2)),
                           method)
      }
      if (method == "spearman"){
        res[stevec, ] <- c(paste0(x[i], " and ", y[j]),
                           round(cor.res$statistic, 2),
                           ifelse(cor.res$p.value < 0.001, yes = "< 0.01", no = round(cor.res$p.value, 3)),
                           round(cor.res$estimate, 2),
                           method)
      }
      stevec <- stevec+1
    }
  }
  return(res)
}
