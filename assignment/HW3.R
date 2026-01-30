
# CS544 HW3
# Date: Jan 27-31, 2026
# R Version: 4.5.2


# Part 1 ======================================================================

# initializing dataset
forbes <- read.csv("https://people.bu.edu/kalathur/datasets/forbes_10_14_2025.csv")
head(forbes)


# a) barplot ------------------------------------------------------------------

# find count of billionaires by country
bln_by_country <- table(forbes$country)
head(bln_by_country)

# find rich countries
rich_countries <- bln_by_country[bln_by_country >= 10]
head(rich_countries)

# sort by count descending
sorted_countries <- sort(rich_countries, decreasing = TRUE)

# barplot
barplot(
  sorted_countreis,
  ylim = c(0, 1000),  # adapt y-axis limit to the max count
  col = "lightblue",  # define color
  xlab = "Countries",  # x label
  ylab = "Count of Billionaires",  # y label
  main = "Barplot for Count of Billionaires in Rich Countries"
)


# b) distribution of f/m ------------------------------------------------------

# find count of billionaires by gender
(table_gender <- table(forbes$gender))

# change column names
(slice_labels <- c("Female", "Male"))

# find percentages
(pct_gender <- round(table_gender/sum(table_gender), digits = 2)*100)

# join labels
(slice_labels <- paste0(slice_labels, " ", pct_gender, "%"))

# pie chart
pie(table_gender, 
    labels = slice_labels,
    col = c("wheat", "lightblue"),  # color
    main = "Pie Chart of Gender Distribution in Forbes Dataset"  # title
)


# c) f/m across top 5 categories ----------------------------------------------




# d) inferences ---------------------------------------------------------------






# Part2 =======================================================================

# initializing dataset
us_quarters <- read.csv("https://people.bu.edu/kalathur/datasets/us_quarters.csv")
head(us_quarters)


# a) state with highest num of quarters by each mint --------------------------





# b) barplot: matrix for the two mints ----------------------------------------





# two striking inferences







# c) side-by-side box plots for the two mints ---------------------------------




# two striking inferences for each of the box plots





# d) outliers states (5-num-summary) ------------------------------------------











# =============================================================================
# Part3

# initializing dataset
stocks <- read.csv("https://people.bu.edu/kalathur/datasets/stocks_2024.csv")
head(stocks)  # view the head


# a) pair wise plots (6 stocks in 1 plot) -------------------------------------







# b) correlation matrix (rounded to 2 decimals) -------------------------------






# c) interpretations (≥4) -----------------------------------------------------








# top 3 correlated stocks for each stock (use loops) --------------------------








# =============================================================================
# Part4

# initializing dataset
scores <- read.csv("https://people.bu.edu/kalathur/datasets/scores.csv")
head(scores)  # view the head


# a) histogram & custom output (only use counts & breaks) ---------------------









# b) breaks of histogram & custom output (only use counts & breaks) -----------


















