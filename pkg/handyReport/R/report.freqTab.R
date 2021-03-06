#' A frequency table with missing values
#'
#' @description The function creates a frequency table (with percentages and cumulative percentages) with valid values and with missing values.
#' @param x A vector with the values of categorical variable.
#' @param dec Number of decimal places for percentages.
#' @param useNA Wheter to include NA values in the table. Default is always (it includes NAs even when the count of NAs is zero), other ppossible values are no (do not include NAs) and ifany (NAs are included if they are present in the data). See ?table for more.
#' @param cumulative Wheter to report cumulative frequencies and percentages or not. Default is TRUE.
#' @examples
#' x <- mtcars$gear
#' x[sample(1:length(x), size = 5, replace = FALSE)] <- NA
#' report.freqTab(x = x, dec = 2, useNA = "always")
#' @note This is a modified function freqTab from the package multiUS. The aouthor of the original function is Aleš Žiberna.
#' @author Marjan Cugmas
#' @export

report.freqTab <- function(x, dec = 2, cumulative = TRUE, useNA = "always"){
  if (is.factor(x) == FALSE) x <- as.factor(x)
  tblValid <- table(x)
  cumFreqValid <- cumsum(tblValid)
  percValid <- tblValid/sum(tblValid) * 100
  cumPercValid <- cumsum(percValid)
  if (!is.null(dec)) {
    percValid <- round(percValid, dec)
    cumPercValid <- round(cumPercValid, dec)
  }
  frekValid <- as.data.frame(cbind("Category" = names(tblValid), "Valid %" = percValid, "Valid Cum. %" = cumPercValid), stringsAsFactors = FALSE)
  frekValid <- rbind(frekValid, c(NA,0,0))
  if (useNA != "no"){
    tbl <- table(x, useNA = useNA)
    cumFreq <- cumsum(tbl)
    perc <- tbl/sum(tbl) * 100
    cumPerc <- cumsum(perc)
    if (!is.null(dec)) {
      perc <- round(perc, dec)
      cumPerc <- round(cumPerc, dec)
    }
    frekTab <- as.data.frame(cbind("Category" = names(tbl),  "Freq." = tbl, "Cum. Freq." = cumFreq, "%" = perc, "Cum. %" = cumPerc), stringsAsFactors = FALSE)
    freqTab <- merge(frekTab, frekValid, by = "Category", all = TRUE, sort = FALSE)
    freqTab[is.na(freqTab$Category), "Category"] <- "Missing value"
    freqTab[is.na(freqTab)] <- ""
  }
  if (useNA == "no") freqTab <- frekValid
  if (cumulative == FALSE) freqTab <- freqTab[, -which(colnames(freqTab) %in% c("Cum. Freq.", "Cum. %", "Valid Cum. %"))]
  return(freqTab)
}
