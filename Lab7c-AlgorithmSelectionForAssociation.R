# *****************************************************************************
# Lab 7.c.: Algorithm Selection for Association Rule Learning ----
#
# Course Code: BBT4206
# Course Name: Business Intelligence II
# Semester Duration: 21st August 2023 to 28th November 2023
#
# Lecturer: Allan Omondi
# Contact: aomondi [at] strathmore.edu
#
# Note: The lecture contains both theory and practice. This file forms part of
#       the practice. It has required lab work submissions that are graded for
#       coursework marks.
#
# License: GNU GPL-3.0-or-later
# See LICENSE file for licensing information.
# *****************************************************************************

# **[OPTIONAL] Initialization: Install and use renv ----
# The R Environment ("renv") package helps you create reproducible environments
# for your R projects. This is helpful when working in teams because it makes
# your R projects more isolated, portable and reproducible.

# Further reading:
#   Summary: https://rstudio.github.io/renv/
#   More detailed article: https://rstudio.github.io/renv/articles/renv.html

# "renv" It can be installed as follows:
# if (!is.element("renv", installed.packages()[, 1])) {
# install.packages("renv", dependencies = TRUE,
# repos = "https://cloud.r-project.org") # nolint
# }
# require("renv") # nolint

# Once installed, you can then use renv::init() to initialize renv in a new
# project.

# The prompt received after executing renv::init() is as shown below:
# This project already has a lockfile. What would you like to do?

# 1: Restore the project from the lockfile.
# 2: Discard the lockfile and re-initialize the project.
# 3: Activate the project without snapshotting or installing any packages.
# 4: Abort project initialization.

# Select option 1 to restore the project from the lockfile
# renv::init() # nolint

# This will set up a project library, containing all the packages you are
# currently using. The packages (and all the metadata needed to reinstall
# them) are recorded into a lockfile, renv.lock, and a .Rprofile ensures that
# the library is used every time you open the project.

# Consider a library as the location where packages are stored.
# Execute the following command to list all the libraries available in your
# computer:
.libPaths()

# One of the libraries should be a folder inside the project if you are using
# renv

# Then execute the following command to see which packages are available in
# each library:
lapply(.libPaths(), list.files)

# This can also be configured using the RStudio GUI when you click the project
# file, e.g., "BBT4206-R.Rproj" in the case of this project. Then
# navigate to the "Environments" tab and select "Use renv with this project".

# As you continue to work on your project, you can install and upgrade
# packages, using either:
# install.packages() and update.packages or
# renv::install() and renv::update()

# You can also clean up a project by removing unused packages using the
# following command: renv::clean()

# After you have confirmed that your code works as expected, use
# renv::snapshot(), AT THE END, to record the packages and their
# sources in the lockfile.

# Later, if you need to share your code with someone else or run your code on
# a new machine, your collaborator (or you) can call renv::restore() to
# reinstall the specific package versions recorded in the lockfile.

# [OPTIONAL]
# Execute the following code to reinstall the specific package versions
# recorded in the lockfile (restart R after executing the command):
# renv::restore() # nolint

# [OPTIONAL]
# If you get several errors setting up renv and you prefer not to use it, then
# you can deactivate it using the following command (restart R after executing
# the command):
# renv::deactivate() # nolint

# If renv::restore() did not install the "languageserver" package (required to
# use R for VS Code), then it can be installed manually as follows (restart R
# after executing the command):

if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# Introduction ----

# Association rule learning helps to boost business by determining which items
# are bought together.

# A business that has this information can use it for promotional pricing or
# for product placement.

# Association rules are employed in many application areas including Web usage
# mining, intrusion detection, continuous production, bioinformatics, etc.

# In contrast with sequence mining, association rule learning typically does
# not consider the order of items either within a transaction or across
# transactions.

# Association rule learning -> association rule mining -> market basket analysis
# The primary difference lies in the context and purpose of the analysis.
# Association rule mining is a more general technique that can be applied in
# various domains, while
# Market Basket Analysis is a specialized application used in the retail
# industry to understand customer purchasing behavior.

## Examples of Association Rule Learning Algorithms ----
### Apriori ----
# The name of the algorithm is Apriori because it uses prior knowledge of
# frequent itemset properties.

### Equivalence Class Transformation (ECLAT) ----

### Frequent Pattern (FP) - Growth ----

### ASSOC ----

### OPUS search ----

## Support ----
# Support is a measure used to identify the frequency with which a particular
# itemset (a set of items or products) appears in a transaction database.
# It is a crucial metric for determining the significance or popularity of
# itemsets in the dataset.

# Support is defined as the proportion of transactions in the dataset that
# contain a specific itemset. It is typically expressed as a percentage or a
# fraction of the total number of transactions.

# Support is calculated as follows:

# Support(Itemset X) = (Number of transactions containing Itemset X) /
#                      (Total number of transactions)

# Support helps you to identify the frequently occurring itemsets.
# It is a critical parameter in the Apriori algorithm because it is used to
# prune infrequent itemsets.

## Confidence ----
# Confidence quantifies how often the items in the
# antecedent (the "if" part of the rule) and the items in the
# consequent (the "then" part of the rule) appear together in the dataset
# compared to the frequency of the antecedent alone.

# Confidence is used to assess the strength of an association between
# two items in an association rule.

# Confidence is calculated as follows:

# Confidence(X → Y) = Support(X ∪ Y) / Support(X)

# Support(X ∪ Y) is the support of the combined itemset (X and Y).
# Support(X) is the support of the antecedent (itemset X).

# Confidence measures how likely it is for itemset Y to be found in transactions
# where itemset X is present. A higher confidence value indicates a stronger
# association between the two itemsets.

# In the context of the Apriori algorithm, you would set a minimum confidence
# threshold. Association rules with confidence above this threshold are
# considered strong and are retained, while those with lower confidence are
# filtered out. This helps in focusing on the most significant and actionable
# associations in the dataset.

## Coverage ----
# Coverage is a measure that assesses the proportion of transactions in a
# dataset that a specific association rule applies to. It helps determine how
# widely applicable or general a rule is within the dataset.

# The coverage of an association rule (X → Y) is calculated as:

# Coverage(X → Y) = Support(X ∪ Y)

# Coverage measures the extent to which the rule is valid or relevant across
# the dataset. High coverage indicates that the rule applies to a significant
# portion of transactions, making it broadly applicable.

# On the other hand, low coverage suggests that the rule is only relevant to a
# small fraction of transactions, limiting its overall practicality or
# applicability.

# High coverage alone does not indicate a strong or interesting association;
# you need to find rules with a balance of high support, high confidence,
# high coverage, and high lift to derive valuable insights from the data.

## Lift ----
# Lift is a measure of how much more often the items in the antecedent
# (the "if" part of the rule) and the items in the consequent (the "then" part
# of the rule) appear together in transactions compared to what would be
# expected if they were statistically independent.

# The formula for calculating lift for an association rule (X → Y) is as
# follows:

# Lift(X → Y) = (Support(X ∪ Y)) / (Support(X) * Support(Y))

### Lift = 1 ----
# This means that the items in the antecedent (X) and the items in the
# consequent (Y) are statistically independent. The rule does not provide any
# useful information about the association between the two itemsets.

### Lift > 1 ----
# A lift value greater than 1 suggests that the items in X and Y appear together
# more frequently than if they were independent. This indicates a positive
# association. The larger the lift value, the stronger the association.

### Lift < 1 ----
# A lift value less than 1 suggests that the items in X and Y appear together
# less often than if they were independent. This indicates a negative or inverse
# association. In such cases, the presence of X may actually decrease the
# likelihood of Y.

# Association rules are given in the form as below:

# A => B [Support, Confidence, Coverage, Lift]

# The part before => is referred to as the
# if, antecedent, left hand side, or lhs.

# and the part after => is referred to as the
# then, consequent, right hand side, or rhs.

# Where A and B are sets of items in the transaction data and
# A and B are disjoint sets.

# For example:
# Computer => Anti-virus Software [Support=20%, Confidence=60%, Coverage = 80%, Lift = 118] # nolint

# Above rule says:

# 20% of transactions show anti-virus software is bought with a computer.

# 60% of transactions that contain a computer also contain an anti-virus and
# only 40% of transactions contain a computer only.

# The rule that purchasing a computer is associated with purchasing an
# anti-virus is applicable to 80% of transactions

# A lift value of 118 (> 1) suggests that a computer and an anti-virus appear
# together often as opposed to each of them appearing independent of the other.

# Configurable association rule learning parameters include:

# (i) A minimum support threshold to find all the frequent itemsets that are in
# the database (default support = 0.1).
# (ii) A minimum confidence threshold to the frequent itemsets used to create
# rules (default confidence = 0.8).

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

## FORMAT 1: Single Format ----
# An example of the single format transaction data is presented
# here: "data/transactions_single_format.csv" and loaded as follows:
transactions_single_format <-
  read.transactions("data/transactions_single_format.csv",
                    format = "single", cols = c(1, 2))

View(transactions_single_format)
print(transactions_single_format)

## FORMAT 2: Basket Format----
# An example of the single format transaction data is presented
# here: "data/transactions_basket_format.csv" and loaded as follows:
transactions_basket_format <-
  read.transactions("data/transactions_basket_format.csv",
                    format = "basket", sep = ",", cols = 2)
View(transactions_basket_format)
print(transactions_basket_format)

## The online retail dataset ----
# We will use the "Online Retail (2015)" dataset available on UCI ML repository
# here: http://archive.ics.uci.edu/static/public/352/online+retail.zip

# Abstract:
# This is a transnational data set which contains all the transactions occurring
# between 01/12/2010 and 09/12/2011 for a UK-based and registered non-store
# online retail.The company mainly sells unique all-occasion gifts. Many
# customers of the company are wholesalers.

# Source: http://archive.ics.uci.edu/dataset/352/online+retail
# Metadata: http://archive.ics.uci.edu/dataset/352/online+retail

# We can read data from an Excel spreadsheet as follows:
retail <- read_excel("data/Online Retail.xlsx")
dim(retail)

### Handle missing values ----
# Are there missing values in the dataset?
any_na(retail)

# How many?
n_miss(retail)

# What is the proportion of missing data in the entire dataset?
prop_miss(retail)

# What is the number and percentage of missing values grouped by
# each variable?
miss_var_summary(retail)

# Which variables contain the most missing values?
gg_miss_var(retail)

# Which combinations of variables are missing together?
gg_miss_upset(retail)

#### OPTION 1: Remove the observations with missing values ----
retail_removed_obs <- retail %>% filter(complete.cases(.))

# We end up with 406,829 observations to create the association rules
# instead of the initial 541,909 observations.
dim(retail_removed_obs)

# Are there missing values in the dataset?
any_na(retail_removed_obs)

#### OPTION 2: Remove the variables with missing values ----
# The `CustomerID` variable will not be used to create association rules.
# We will use the `InvoiceNo` instead.
retail_removed_vars <-
  retail %>% dplyr::select(-CustomerID)

dim(retail_removed_vars)

# Are there missing values in the dataset?
any_na(retail_removed_vars)

# What is the number and percentage of missing values grouped by
# each variable?
miss_var_summary(retail_removed_vars)

# We now remove the observations that do not have a value for the description
# variable.
retail_removed_vars_obs <- retail_removed_vars %>% filter(complete.cases(.))

# We end up with 540,455 observations to create the association rules
# instead of the initial 541,909 observations.
# This is better than OPTION 1 which resulted in 406,829 observations to
# create the association rules.
dim(retail_removed_vars_obs)

## Identify categorical variables ----
# Ensure the customer's country is recorded as categorical data
retail_removed_vars_obs %>% mutate(Country = as.factor(Country))

# Also ensure that the description (name of the product purchased) is recorded
# as categorical data
retail_removed_vars_obs %>% mutate(Description = as.factor(Description))
str(retail_removed_vars_obs)

dim(retail_removed_vars_obs)
head(retail_removed_vars_obs)

## Record the date and time variables in the correct format ----
# Ensure that InvoiceDate is stored in the correct date format.
# We can separate the date and the time into 2 different variables.
retail_removed_vars_obs$trans_date <-
  as.Date(retail_removed_vars_obs$InvoiceDate)

# Extract time from InvoiceDate and store it in another variable
retail_removed_vars_obs$trans_time <-
  format(retail_removed_vars_obs$InvoiceDate, "%H:%M:%S")

## Record the InvoiceNo in the correct format (numeric) ----
# Convert InvoiceNo into numeric
retail_removed_vars_obs$invoice_no <-
  as.numeric(as.character(retail_removed_vars_obs$InvoiceNo))

# The NAs introduced by coercion represent cancelled invoices. The OLTP system
# of the business represents cancelled invoice with the prefix "C", e.g.
# "C536391".

# Are there missing values in the dataset?
any_na(retail_removed_vars_obs)

# What is the number and percentage of missing values grouped by
# each variable?
miss_var_summary(retail_removed_vars_obs)
dim(retail_removed_vars_obs)

# We now remove the observations that do not have a numeric value for the
# invoice_no variable, i.e., the cancelled invoices that did not lead to a
# complete purchase. There are a total of 9,291 cancelled invoices.
retail_removed_vars_obs <- retail_removed_vars_obs %>% filter(complete.cases(.))

# This leads to the dataset now having 531,164 observations to use to create the
# association rules.
dim(retail_removed_vars_obs)

# We then remove the duplicate variables that we do not need
# (InvoiceNo and InvoiceDate) and we also remove all commas to make it easier
# to identify individual products (some products have commas in their names).
retail_removed_vars_obs <-
  retail_removed_vars_obs %>%
  select(-InvoiceNo, -InvoiceDate) %>%
  mutate_all(~str_replace_all(., ",", " "))

# The pre-processed data frame now has 531,164 observations and 8 variables.
dim(retail_removed_vars_obs)
View(retail_removed_vars_obs)

# We can save the pre-processing progress made so far
write.csv(retail_removed_vars_obs,
          file = "data/retail_data_before_single_transaction_format.csv",
          row.names = FALSE)

retail_removed_vars_obs <-
  read.csv(file = "data/retail_data_before_single_transaction_format.csv")

## Create a transaction data frame using the "basket format" ----
str(retail_removed_vars_obs)
# To do this, we group the data by `invoice_no` and `trans_date`
# We then combine all products from one invoice and date as one row and
# separate each item by a comma. (Refer to STEP 2. FORMAT 1).

# The basket format that we want looks
# like this: "data/transactions_basket_format.csv"

# ddply is used to split a data frame, apply a function to the split data,
# and then return the result back in a data frame.
### OPTION 1 ----
transaction_data <-
  plyr::ddply(retail_removed_vars_obs,
    c("invoice_no", "trans_date"),
    function(df1) {
      paste(df1$Description, collapse = ",")
    }
  )

### OPTION 2 ----
# Alternatively, we can identify each item using its stock code. This is
# more accurate but harder to interpret the results directly.
transaction_data_stock_code <-
  plyr::ddply(retail_removed_vars_obs,
              c("invoice_no", "trans_date"),
              function(df1) paste(df1$StockCode, collapse = ","))

# The R function paste() concatenates vectors to characters and separates
# the results using collapse = [any optional character string].
# The separator we use in this case is a comma ",".
View(transaction_data)

# View option 2 which is based on the stock code
View(transaction_data_stock_code)

## Record only the `items` variable ----
# Notice that at this point, the single format ensures that each transaction is
# recorded in a single observation. We therefore no longer require the
# `invoice_no` variable and all the other variables in the data frame except
# the itemsets.

### OPTION 1 ----
transaction_data <-
  transaction_data %>%
  dplyr::select("items" = V1)
#  %>% mutate(items = paste("{", items, "}", sep = ""))

View(transaction_data)

### OPTION 2 ----
transaction_data_stock_code <-
  transaction_data_stock_code %>%
  dplyr::select("items" = V1)
#  %>% mutate(items = paste("{", items, "}", sep = ""))

View(transaction_data_stock_code)

## Save the transactions in CSV format ----
### OPTION 1 ----
write.csv(transaction_data,
          "data/transactions_basket_format_online_retail.csv",
          quote = FALSE, row.names = FALSE)

### OPTION 2 ----
write.csv(transaction_data_stock_code,
          "data/transactions_basket_format_online_retail_stock.csv",
          quote = FALSE, row.names = FALSE)

## Read the transactions fromm the CSV file ----
# We can now, finally, read the basket format transaction data as a
# transaction object.
### OPTION 1 ----
tr <-
  read.transactions("data/transactions_basket_format_online_retail.csv",
    format = "basket",
    header = TRUE,
    rm.duplicates = TRUE,
    sep = ","
  )

# This shows there are 20,607 transactions that have been identified
# and 8,906 items
print(tr)
summary(tr)

### OPTION 2 ----
tr_stock_code <-
  read.transactions("data/transactions_basket_format_online_retail_stock.csv",
    format = "basket",
    header = TRUE,
    rm.duplicates = TRUE,
    sep = ","
  )

# This shows there are 20,607 transactions that have been identified
# and 3,942 unique items (more accurate because we are using the
# stock code)
print(tr_stock_code)
summary(tr_stock_code)

# The difference between OPTION 1 and OPTION 2 denotes the fact that
# identifying items using the product name instead of the product code
# does not yield accurate results. We, therefore, use product codes only
# from this point onwards.

# STEP 2. Basic EDA ----
# Create an item frequency plot for the top 10 items
itemFrequencyPlot(tr_stock_code, topN = 10, type = "absolute",
                  col = brewer.pal(8, "Pastel2"),
                  main = "Absolute Item Frequency Plot",
                  horiz = TRUE,
                  mai = c(2, 2, 2, 2))

itemFrequencyPlot(tr_stock_code, topN = 10, type = "relative",
                  col = brewer.pal(8, "Pastel2"),
                  main = "Relative Item Frequency Plot",
                  horiz = TRUE,
                  mai = c(2, 2, 2, 2))

# As shown by the plots, the top 5 most purchased products in descending
# order are:
# Stock Code "85123A" corresponds to "WHITE HANGING HEART T-LIGHT HOLDER"
# Stock Code "85099B" corresponds to "JUMBO BAG RED RETROSPOT"
# Stock Code "22423" corresponds to "REGENCY CAKESTAND 3 TIER"
# Stock Code "47566" corresponds to "PARTY BUNTING"
# Stock Code "20725" corresponds to "LUNCH BAG RED RETROSPOT"


# STEP 3. Create the association rules ----
# We can set the minimum support and confidence levels for rules to be
# generated.

# The default behavior is to create rules with a minimum support of 0.1
# and a minimum confidence of 0.8. These parameters can be adjusted (lowered)
# if the algorithm does not lead to the creation of any rules.
# We also set maxlen = 10 which refers to the maximum number of items per
# item set.

## OPTION 1 ----
association_rules <- apriori(tr_stock_code,
                             parameter = list(support = 0.01,
                                              confidence = 0.8,
                                              maxlen = 10))

## OPTION 2 ----
association_rules_prod_name <- apriori(tr,
                                       parameter = list(support = 0.01,
                                                        confidence = 0.8,
                                                        maxlen = 10))

# STEP 3. Print the association rules ----
## OPTION 1 ----
# Threshold values of support = 0.01, confidence = 0.8, and
# maxlen = 10 results in a total of 83 rules when using the
# stock code to identify the products.
summary(association_rules)
inspect(association_rules)
# To view the top 10 rules
inspect(association_rules[1:10])
plot(association_rules)

### Remove redundant rules ----
# We can remove the redundant rules as follows:
subset_rules <-
  which(colSums(is.subset(association_rules,
                          association_rules)) > 1)
length(subset_rules)
association_rules_no_reps <- association_rules[-subset_rules]

# This results in 40 non-redundant rules (instead of the initial 83 rules)
summary(association_rules_no_reps)
inspect(association_rules_no_reps)

write(association_rules_no_reps,
      file = "rules/association_rules_based_on_stock_code.csv")

## OPTION 2 ----
# Threshold values of support = 0.01, confidence = 0.8, and
# maxlen = 10 results in a total of 14 rules when using the
# product name to identify the products.
summary(association_rules_prod_name)
inspect(association_rules_prod_name)
# To view the top 10 rules
inspect(association_rules_prod_name[1:10])
plot(association_rules_prod_name)

### Remove redundant rules ----
# We can remove the redundant rules as follows:
subset_rules <-
  which(colSums(is.subset(association_rules_prod_name,
                          association_rules_prod_name)) > 1)
length(subset_rules)
association_rules_prod_no_reps <- association_rules_prod_name[-subset_rules]

# This results in 8 non-redundant rules (instead of the initial 10 rules)
summary(association_rules_prod_no_reps)
inspect(association_rules_prod_no_reps)

write(association_rules_prod_no_reps,
      file = "rules/association_rules_based_on_product_name.csv")

# STEP 4. Find specific rules ----
# Which product(s), if bought, result in a customer purchasing
# "ROSES REGENCY TEACUP AND SAUCER"?
roses_regency_teacup_and_saucer_association_rules <- # nolint
  apriori(tr, parameter = list(supp = 0.01, conf = 0.8),
          appearance = list(default = "lhs",
                            rhs = "ROSES REGENCY TEACUP AND SAUCER"))
inspect(head(roses_regency_teacup_and_saucer_association_rules))

# Which product(s) are bought if a customer purchases
# "STRAWBERRY CHARLOTTE BAG,WOODLAND CHARLOTTE BAG"?
strawberry_charlotte_bag_association_rules <- # nolint
  apriori(tr, parameter = list(supp = 0.01, conf = 0.8),
          appearance = list(lhs = c("STRAWBERRY CHARLOTTE BAG", "WOODLAND CHARLOTTE BAG"), # nolint
                            default = "rhs"))
inspect(head(strawberry_charlotte_bag_association_rules))

# STEP 5. Visualize the rules ----
# Filter rules with confidence greater than 0.85 or 85%
rules_to_plot <-
  association_rules_prod_no_reps[quality(association_rules_prod_no_reps)$confidence > 0.85] # nolint

#Plot SubRules
plot(rules_to_plot)
plot(rules_to_plot, method = "two-key plot")

top_10_rules_to_plot <- head(rules_to_plot, n = 10, by = "confidence")
plot(top_10_rules_to_plot, method = "graph",  engine = "htmlwidget")

saveAsGraph(head(rules_to_plot, n = 1000, by = "lift"),
            file = "graph/association_rules_prod_no_reps.graphml")

# Filter top 20 rules with highest lift
rules_to_plot_by_lift <- head(rules_to_plot, n = 20, by = "lift")
plot(rules_to_plot_by_lift, method = "paracoord")

plot(top_10_rules_to_plot, method = "grouped")

# Applications of Association Rule Learning outside a Business Context ----
# Have a look at this example where association rule learning is used to
# determine who will survive the Titanic ship wreck:
# https://www.rdatamining.com/examples/association-rules

# This gives an example of the fact that association rule learning can be
# extended to many creative contexts, e.g., a set of symptoms associated with
# a disease, genomics, bioinformatics, macro economics, and so on.

# Healthcare: Association rule learning can be used to discover patterns in
# patient data, such as identifying co-occurring medical conditions or
# treatments. This information can be valuable for disease diagnosis and
# treatment planning.

# Recommendation Systems: Beyond retail, it's used in recommendation systems
# for movies, music, books, and more. It helps suggest items that users might
# like based on their historical choices or preferences.

# Fraud Detection: Association rules can help identify unusual patterns of
# behavior in financial transactions, potentially indicating fraud. Unusual
# combinations of transactions can be flagged for further investigation.

# Text Mining: In natural language processing, association rule learning can be
# used to discover frequent word combinations, which can be useful for text
# classification, sentiment analysis, and content recommendation.

# Web Usage Mining: Analyzing user behavior on websites to understand patterns
# in clicks, page views, and interactions. This information can be used to
# optimize website design and content.

# Supply Chain Optimization: Association rules can help optimize the supply
# chain by identifying patterns in the movement of goods and inventory
# management.

# Biology and Genetics: Identifying associations between genes and diseases or
# discovering patterns in DNA sequences can aid in genetic research and
# personalized medicine.

# Quality Control in Manufacturing: It can be used to find patterns in
# manufacturing data to improve product quality and reduce defects.

# Market Research: Beyond market baskets, it can be used to identify patterns in
# consumer behavior, such as brand loyalty, purchase sequences, and the impact
# of marketing campaigns.

# Sports Analytics: Analyzing game statistics and player performance to identify
# patterns and strategies that lead to success.

# These are just a few examples, and the creative applications of association
# rule learning continue to expand as data mining and machine learning
# techniques evolve. It's a versatile method for discovering patterns and
# associations in various types of data.

# References ----
## Online Retail. (2015). Online Retail [Dataset; XLSX]. University of California, Irvine (UCI) Machine Learning Repository. https://doi.org/10.24432/C5BW33 # nolint ----

# **Required Lab Work Submission** ----
## Part A ----
# Create a new file called
# "Lab7-Submission-AlgorithmSelection.R".
# Provide all the code you have used to demonstrate the training of a model.
# This can be a model for a classification, regression, clustering, or an
# association problem.
# Each group member should provide one dataset that can be used for this
# task as long as it is not one of the datasets we have used in Lab 7.

## Part B ----
# Upload *the link* to your
# "Lab7-Submission-AlgorithmSelection.R" hosted
# on Github (do not upload the .R file itself) through the submission link
# provided on eLearning.

## Part C ----
# Create a markdown file called "Lab-Submission-Markdown.Rmd"
# and place it inside the folder called "markdown". Use R Studio to ensure the
# .Rmd file is based on the "GitHub Document (Markdown)" template when it is
# being created.

# Refer to the following file in Lab 1 for an example of a .Rmd file based on
# the "GitHub Document (Markdown)" template:
#     https://github.com/course-files/BBT4206-R-Lab1of15-LoadingDatasets/blob/main/markdown/BIProject-Template.Rmd # nolint

# Include Line 1 to 14 of BIProject-Template.Rmd in your .Rmd file to make it
# displayable on GitHub when rendered into its .md version

# It should have code chunks that explain all the steps performed on the
# datasets.

## Part D ----
# Render the .Rmd (R markdown) file into its .md (markdown) version by using
# knitR in RStudio.

# You need to download and install "pandoc" to render the R markdown.
# Pandoc is a file converter that can be used to convert the following files:
#   https://pandoc.org/diagram.svgz?v=20230831075849

# Documentation:
#   https://pandoc.org/installing.html and
#   https://github.com/REditorSupport/vscode-R/wiki/R-Markdown

# By default, Rmd files are open as Markdown documents. To enable R Markdown
# features, you need to associate *.Rmd files with rmd language.
# Add an entry Item "*.Rmd" and Value "rmd" in the VS Code settings,
# "File Association" option.

# Documentation of knitR: https://www.rdocumentation.org/packages/knitr/

# Upload *the link* to "Lab-Submission-Markdown.md" (not .Rmd)
# markdown file hosted on Github (do not upload the .Rmd or .md markdown files)
# through the submission link provided on eLearning.