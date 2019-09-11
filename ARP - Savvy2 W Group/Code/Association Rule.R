# Setup the sessions --------------------------------------------------------------------
setwd("C:/Users/Shirley/Desktop/ARP code")
library(readxl)
library(plyr)
library(arules)
library(RColorBrewer)
library(ggplot2)
library(pdp)

# Prepare datasets ----------------------------------------------------------------------
# Read the datasets
data <- read_excel("UK .xlsx", sheet = "Q3")
# Remove missing values
data <- data[complete.cases(data), ]
#str(data)

# Transform the data to basket format ---------------------------------------------------
# Transform the data to transaction data based on invoice
transaction <- ddply(data, c("`Invoice number`", "`Invoice date`"), 
                     function(df)paste(df$`Item/ Product Code`, collapse = ","))
# Only keep the product basket column
transaction <- transaction[, 3]
# Write the product basket data to csv file
write.table(transaction,"transactions.csv", quote = FALSE, row.names = FALSE,
            col.names = FALSE)

# Read the basket data for association rules analysis -----------------------------------
basket <- read.transactions('transactions.csv', header = FALSE, format = 'basket', 
                            sep = ",", quote = "", rm.duplicates = TRUE)
#summary(basket)

# Create an item frequency plot for the top 10 products ---------------------------------
itemFrequencyPlot(basket, topN = 10, type = 'absolute', col = brewer.pal(8, 'Pastel2'), 
                  main = "Absolute Item Frequency Plot")

# Generate association rules ------------------------------------------------------------
# Set support and confidence values to be tuned
support.levels <- c(0.01, 0.005, 0.0025, 0.001)
confidence.levels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Empty integers 
rules.sup1 <- integer(length = 9)
rules.sup0.5 <- integer(length = 9)
rules.sup0.25 <- integer(length = 9)
rules.sup0.1 <- integer(length = 9)

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidence.levels)) {
  rules.sup1[i] <- length(apriori(basket, parameter = list(sup = support.levels[1], conf = confidence.levels[i], 
                                                           maxlen = 2, target = "rules")))
}

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidence.levels)) {
  rules.sup0.5[i] <- length(apriori(basket, parameter = list(sup = support.levels[2], conf = confidence.levels[i], 
                                                             maxlen = 2, target = "rules")))
}

# Apriori algorithm with a support level of 0.25%
for (i in 1:length(confidence.levels)) {
  rules.sup0.25[i] <- length(apriori(basket, parameter = list(sup = support.levels[3], conf = confidence.levels[i],
                                                              maxlen = 2, target = "rules")))
}

# Apriori algorithm with a support level of 0.1%
for (i in 1:length(confidence.levels)) {
  rules.sup0.1[i] <- length(apriori(basket, parameter = list(sup = support.levels[4], conf = confidence.levels[i],
                                                             maxlen = 2, target = "rules")))
}

# Plot the tuned support and confidence values ------------------------------------------
# Number of rules found with a support level of 1%
plot1 <- qplot(confidence.levels, rules.sup1, geom = c("point", "line"), 
               xlab = "Confidence level", ylab = "Number of rules found", 
               main = "Apriori with a support level of 1%") + 
  scale_y_continuous(breaks = seq(50, 360, 20)) +
  theme_bw()

# Number of rules found with a support level of 0.5%
plot2 <- qplot(confidence.levels, rules.sup0.5, geom = c("point", "line"), 
               xlab = "Confidence level", ylab = "Number of rules found", 
               main = "Apriori with a support level of 0.5%") + 
  scale_y_continuous(breaks = seq(200, 1300, 100)) +
  theme_bw()

# Number of rules found with a support level of 0.25%
plot3 <- qplot(confidence.levels, rules.sup0.25, geom = c("point", "line"), 
               xlab = "Confidence level", ylab = "Number of rules found", 
               main = "Apriori with a support level of 0.25%") + 
  scale_y_continuous(breaks = seq(600, 3700, 200)) +
  theme_bw()

# Number of rules found with a support level of 0.1%
plot4 <- qplot(confidence.levels, rules.sup0.1, geom = c("point", "line"), 
               xlab = "Confidence level", ylab = "Number of rules found", 
               main = "Apriori with a support level of 0.1%") + 
  scale_y_continuous(breaks = seq(16000, 42000, 2000)) +
  theme_bw()

# Subplot
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

# Choose the defined association rule ---------------------------------------------------
association.rules <- apriori(basket, parameter = list(supp = 0.001, conf = 0.5, maxlen = 5))
summary(association.rules)
inspect(head(association.rules))
top10.rules <- inspect(head(association.rules, n = 10, by = c("confidence", "support")))

# write the association rules to csv file
write(association.rules, file = "association rules_Key_Q4.csv", sep = ",")