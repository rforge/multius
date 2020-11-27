#' Report the results of ANOVA
#'
#' @description The function reports the results of ANOVA tests with one categorical variable and one or several numeric variables.
#' @param ordinalna The name of ordinal variable. One variable can be considered.
#' @param intervalna The name of interval/numberic variable(s). A string or a list. Several numeric variables can be considered.
#' @param omegaSq Whether to calculate Omega Square (adjusted Etha Square) which is the effect size measure.
#' @param dataset The dataframe with ordinal and numeric variables.
#' @examples
#' report.anova(ordinalna = "cyl", intervalna = c("hp", "disp", "qsec"), dataset = mtcars)
#' @author Marjan Cugmas
#' @references
#' Kirk, R. E. (1996). Practical significance: A concept whose time has come. Educational and psychological measurement, 56(5), 746-759.
#' Tunks, T. (1978). The use of omega squared in interpreting statistical significance. Bulletin of the Council for Research in Music Education, 28-34.
#' @export

report.anova <- function(ordinalna, intervalna, dataset, omegaSq = FALSE){
  if (is.factor(dataset[, which(names(dataset)==ordinalna)]) == FALSE){
    dataset[, which(names(dataset)==ordinalna)] <- as.factor(dataset[, which(names(dataset)==ordinalna)])
  }
  if (length(ordinalna) > 1) stop("More than one categorical variable is provided.")
  ravni <- levels(dataset[, which(names(dataset)==ordinalna)])
  nravni <- length(ravni)
  res <- matrix(NA, nrow = length(intervalna), ncol = 4 + nravni + as.numeric(omegaSq==TRUE))
  for (i in 1:length(intervalna)) {
    data <- dataset[, c(which(names(dataset)==ordinalna),  which(names(dataset)==intervalna[i]))]
    means <- round(as.vector(by(data = data[, intervalna[i]], INDICES = data[, ordinalna], mean)), 2)
    res[i, 1:nravni] <- means
    model <- summary(aov(data[, intervalna[i]] ~ as.factor(data[, ordinalna])))
    res[i, 1+nravni] <- round(model[[1]][1,4], 2)
    res[i, 2+nravni] <- model[[1]][1,1]
    res[i, 3+nravni] <- model[[1]][2,1]
    res[i, 4+nravni] <- ifelse(model[[1]][1,5] < 0.001, yes = "< 0.01", no = round(model[[1]][1,5], 3))
    if (omegaSq==TRUE) res[i, 4+nravni+1] <- round((model[[1]][1,2] - model[[1]][1,1]*(model[[1]][2,3]))/(model[[1]][1,2] + model[[1]][2,2] + model[[1]][2,3]), 2)
  }
  labele <- c(ravni, "F", "df 1", "df 2", "p")
  if (omegaSq==TRUE) labele <- c(labele, "Omega Sq")
  colnames(res) <- labele
  rownames(res) <- intervalna
  return(res)
}