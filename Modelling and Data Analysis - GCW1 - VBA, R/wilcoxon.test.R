wilcoxon.test <- function(x, y, significance.level = 0.05, 
                          output.type = c("basic", "inter.cal")) {
  # Automatic adjustments of input ------------------------------------------------------
  # Transform data to numeric
  if (any(!is.numeric(x))) warning("x is not numeric and is transformed automatically.")
  x <- as.numeric(x)
  if (any(!is.numeric(y))) warning("y is not numeric and is transformed automatically.")
  y <- as.numeric(y)

  # Parameters check --------------------------------------------------------------------
  # sufficient data check
  if (length(x) < 10 ) stop("The number of data in x should be greater than 10.")
  if (length(y) < 10 ) stop("The number of data in y should be greater than 10.")
  # pairwise data check
  if (length(x) != length(y)) stop("x and y should be pairwise data.")
  # non-numeric significance level check
  if (!is.numeric(significance.level)) stop("Significance level should be numeric.")
  
  # Calculations for the Wilcoxon signed-rank test --------------------------------------
  # Create a dataframe to store the variables
  wilcoxon.cal <- data.frame(x_i = x, y_i = y)
  # Remove NAs in inputs
  if (any(is.na(wilcoxon.cal))) warning("There are NAs in x or y and those pairs are removed automatically.")
  wilcoxon.cal <- wilcoxon.cal[complete.cases(wilcoxon.cal),]
  # sufficient data check
  if ((dim(wilcoxon.cal["x_i"])[1]) < 10 ) stop("The number of data after removing NAs should be greater than 10.")
  # Calculate d_i
  d_i <- wilcoxon.cal["x_i"] - wilcoxon.cal["y_i"]
  d_i <- d_i$x_i
  wilcoxon.cal <- cbind(wilcoxon.cal, d_i)
  # Calculate d_i(non-zero)
  d_i.nonzero <- d_i
  d_i.nonzero[d_i.nonzero == 0] <- NA
  wilcoxon.cal <- cbind(wilcoxon.cal, d_i.nonzero)
  # Calculate ABS d_i
  abs.d_i <- abs(d_i)
  abs.d_i[abs.d_i == 0] <- NA
  wilcoxon.cal <- cbind(wilcoxon.cal, abs.d_i)
  # Calculate ranks
  ranks <- abs(d_i)
  ranks[ranks == 0] <- NA
  ranks <- rank(ranks, na.last = "keep", ties.method = "average")
  wilcoxon.cal <- cbind(wilcoxon.cal, ranks)
  # Calculate signed ranks
  signed.ranks <- ranks
  negative.index <- which((wilcoxon.cal["d_i"] < 0) %in% TRUE)
  signed.ranks[negative.index] <- -ranks[negative.index]
  wilcoxon.cal <- cbind(wilcoxon.cal, signed.ranks)
  # Replace NA with exclude
  wilcoxon.cal[is.na(wilcoxon.cal)] <- "exclude"
  # Rename columns
  names(wilcoxon.cal) <- c("x_i", "y_i", "d_i", "d_i(non-zero)", "Abs.d_i", "ranks",
                           "signed-ranks")
  
  # Results of the Wilcoxon signed-rank test --------------------------------------------
  # Create a dataframe to store the result
  wilcoxon.result <- data.frame()
  # Calculate n
  n <- as.numeric(length(x))
  wilcoxon.result <- rbind(n)
  # Calculate nr
  nr <- as.numeric(n - length(which(is.na(d_i.nonzero) %in% TRUE)))
  wilcoxon.result <- rbind(wilcoxon.result, nr)
  # Calculate SR(+)
  SR.positive <- sum(signed.ranks[which((signed.ranks > 0) %in% TRUE)])
  wilcoxon.result <- rbind(wilcoxon.result, SR.positive)
  # Calculate SR(-)
  SR.negative <- abs(sum(signed.ranks[which((signed.ranks < 0) %in% TRUE)]))
  wilcoxon.result <- rbind(wilcoxon.result, SR.negative)
  # Calculate T
  Wilcoxon.statistic <- min(SR.positive, SR.negative)
  wilcoxon.result <- rbind(wilcoxon.result, Wilcoxon.statistic)
  # Calculate E[T]
  mean.T <- (nr*(nr + 1))/4
  wilcoxon.result <- rbind(wilcoxon.result, mean.T)
  # Calculate Var(T)
  var.T <- (nr*(nr + 1)*(2*nr + 1))/24
  wilcoxon.result <- rbind(wilcoxon.result, var.T)
  # Calculate z
  z <- (Wilcoxon.statistic - mean.T)/(var.T^0.5)
  wilcoxon.result <- rbind(wilcoxon.result, z)
  # Calculate p-value
  p.value <- 2*(1 - pnorm(abs(z), 0, 1))
  wilcoxon.result <- rbind(wilcoxon.result, p.value)
  # Input significance level
  wilcoxon.result <- rbind(wilcoxon.result, significance.level)
  # Round the floats in the result
  wilcoxon.result[3:10] <- round(wilcoxon.result[3:10], 5)
  # Accetp or reject H0
  result <- ifelse(significance.level > p.value, "reject H0", "accept H0")
  wilcoxon.result <- rbind(wilcoxon.result, result)
  # Change the type of the results and rename the column
  wilcoxon.result <- as.data.frame(wilcoxon.result)
  names(wilcoxon.result) <- "Wilcoxon signed-rank test"

  # Set 2 types of output ---------------------------------------------------------------
  if (missing(output.type)) output.type <- "basic"
  if (output.type == "basic") {
    print(wilcoxon.result)
  } else if (output.type == "inter.cal") {
    print(wilcoxon.cal)
  } else {
    stop("Incorrect type of output: output.type should be 'basic' or 'inter.cal'.")
  }
}