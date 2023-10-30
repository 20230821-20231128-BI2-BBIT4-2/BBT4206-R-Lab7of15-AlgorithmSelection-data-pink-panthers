
if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# STEP 1. Install and Load the Required Packages ----
## arules ----
if (require("arules")) {
  require("arules")
} else {
  install.packages("arules", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## arulesViz ----
if (require("arulesViz")) {
  require("arulesViz")
} else {
  install.packages("arulesViz", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## tidyverse ----
if (require("tidyverse")) {
  require("tidyverse")
} else {
  install.packages("tidyverse", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## readxl ----
if (require("readxl")) {
  require("readxl")
} else {
  install.packages("readxl", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## knitr ----
if (require("knitr")) {
  require("knitr")
} else {
  install.packages("knitr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## ggplot2 ----
if (require("ggplot2")) {
  require("ggplot2")
} else {
  install.packages("ggplot2", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## lubridate ----
if (require("lubridate")) {
  require("lubridate")
} else {
  install.packages("lubridate", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## plyr ----
if (require("plyr")) {
  require("plyr")
} else {
  install.packages("plyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## dplyr ----
if (require("dplyr")) {
  require("dplyr")
} else {
  install.packages("dplyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## naniar ----
if (require("naniar")) {
  require("naniar")
} else {
  install.packages("naniar", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## RColorBrewer ----
if (require("RColorBrewer")) {
  require("RColorBrewer")
} else {
  install.packages("RColorBrewer", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# STEP 2. Load and pre-process the dataset ----

# The "read.transactions" function in the "arules" package is used to read
# transaction data from a file and create a "transactions object".

# The transaction data can be specified in either of the following 2 formats:
sales <- read_csv("data/Updated_sales.csv")
dim(sales)



View(sales)
print(sales)
summary(sales)

# Are there missing values in the dataset?
is_na(sales)



#Rename Fields/Columns
colnames(sales)[1] <- "Order_ID"
colnames(sales)[3] <- "Quantity_Ordered"
colnames(sales)[4] <- "Price_Each"
colnames(sales)[5] <- "Order_Date"

#remove unwanted columns
sales_removed_vars <-
  sales %>% dplyr::select(-Address,-Quantity_Ordered,-Price_Each)

dim(sales_removed_vars)
View(sales_removed_vars)

# Also ensure that the product (name of the product purchased) is recorded
# as categorical data
sales_removed_vars %>% mutate(Product = as.factor(Product))
sales_removed_vars$Date <- as.Date(sales_removed_vars$Order_Date, format="%Y-%m-%d")

View(sales_removed_vars)

str(sales_removed_vars)
dim(sales_removed_vars)
head(sales_removed_vars)

#Split Data
transaction_data <-
  plyr::ddply(sales_removed_vars,
              c("Order_ID", "Date"),
              function(df1) {
                paste(df1$Product, collapse = ",")
              }
  )

View(transaction_data)

#Only remain with products
transaction_data <-
  transaction_data %>%
  dplyr::select("items" = V1)

View(transaction_data)


## Save the transactions in CSV format ----
write.csv(transaction_data,
          "data/transactions_single_format_sales.csv",
          quote = FALSE, row.names = FALSE)

## Read the transactions from the CSV file ----
# We can now, finally, read the single format transaction data as a
# transaction object.
tr <-
  read.transactions("data/transactions_single_format_sales.csv",
                    format = "basket",
                    header = TRUE,
                    rm.duplicates = TRUE,
                    sep = ","
  )


print(tr)
summary(tr)

# STEP 2. Basic EDA ----
# Create an item frequency plot for the top 5 items
itemFrequencyPlot(tr, topN = 5, type = "absolute",
                  col = brewer.pal(8, "Pastel2"),
                  main = "Absolute Item Frequency Plot",
                  horiz = TRUE,
                  mai = c(1, 1, 1, 1))

# STEP 3. Create the association rules ----
# We can set the minimum support and confidence levels for rules to be
# generated.

association_rules <- apriori(tr, 
                             parameter = list(minlen=2, 
                                              sup = 0.001, 
                                              conf = 0.05, 
                                              target="rules"))


# STEP 3. Print the association rules ----

summary(association_rules)
inspect(association_rules)
# To view the top 5 rules
inspect(association_rules[1:5])
plot(association_rules)

### Remove redundant rules ----
# We can remove the redundant rules as follows:
subset_rules <-
  which(colSums(is.subset(association_rules,
                          association_rules)) > 1)
length(subset_rules)# 0 redundant rules

ssummary(association_rules)
inspect(association_rules)

write(association_rules,
      file = "rules/association_rules_based_on_product_name.csv")

# STEP 4. Find specific rules ----
# Which product(s), if bought, result in a customer purchasing
# "usb c charging cable"?
USB_C_Charging_Cable <-  
  apriori(tr, parameter = list(supp = 0.001, conf = 0.05),
          appearance = list(default = "lhs",
                            rhs = "USB-C Charging Cable"))
inspect(head(USB_C_Charging_Cable))

# Which product(s) are bought if a customer purchases
# "iPhone,Google Phone"?
iPhone_Google_Phone <- # nolint
  apriori(tr, parameter = list(supp = 0.001, conf = 0.05),
          appearance = list(lhs = c("Google Phone", "iPhone"), # nolint
                            default = "rhs"))
inspect(head(iPhone_Google_Phone))

# STEP 5. Visualize the rules ----
# Filter rules with confidence greater than 0.85 or 85%
rules_to_plot <-
  association_rules[quality(association_rules)$confidence > 0.1] # nolint

#Plot SubRules.
plot(rules_to_plot)
plot(rules_to_plot, method = "two-key plot")

top_10_rules_to_plot <- head(rules_to_plot, n = 10, by = "confidence")
install.packages("visNetwork")
plot(top_10_rules_to_plot, method = "graph",  engine = "htmlwidget")

saveAsGraph(head(rules_to_plot, n = 1000, by = "lift"),
            file = "graph/association_rules.graphml")


# Filter top 20 rules with highest lift
rules_to_plot_by_lift <- head(rules_to_plot, n = 20, by = "lift")
plot(rules_to_plot_by_lift, method = "paracoord")

plot(top_10_rules_to_plot, method = "grouped")

