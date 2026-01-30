
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
  sorted_countries,
  ylim = c(0, 1000),  # 🔴[dont hard code] adapt y-axis limit to the max count
  col = "lightblue",  # define color
  xlab = "Countries",  # x label
  ylab = "Count of Billionaires",  # y label
  main = "Barplot for Count of Billionaires in Rich Countries"
)


# b) distribution of f/m ------------------------------------------------------

# find counts of billionaires by gender
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


# c) f/m across top 5 categories (bivariate) ----------------------------------------------

# find counts of unique categories
table_category <- table(forbes$category)

# sort by count descending
(table_category <- sort(table_category, decreasing = TRUE))

# find top 5 categories
(top5_categories <- names(table_category[1:5]))

# find subset of rows in top 5 categories
my_subset1 <- subset(forbes, category == top5_categories)
    # 🔴DONT USE == , it align two vectors by index of elements

my_subset2 <- subset(forbes, category %in% top5_categories)
    # 🟢USE %in%


head(my_subset1)
head(my_subset2)

nrow(my_subset1)  # [1] 343
nrow(my_subset2)  # [1] 1795

top5_categories

unique(my_subset1$category)
unique(my_subset2$category)





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











# Part 3 ======================================================================

# initializing dataset
stocks <- read.csv("https://people.bu.edu/kalathur/datasets/stocks_2024.csv")
head(stocks)  # view the head


# a) pair wise plots (6 stocks in 1 plot) -------------------------------------







# b) correlation matrix (rounded to 2 decimals) -------------------------------






# c) interpretations (≥4) -----------------------------------------------------








# top 3 correlated stocks for each stock (use loops)








# Part 4 ======================================================================

# initializing dataset
scores <- read.csv("https://people.bu.edu/kalathur/datasets/scores.csv")
head(scores)  # view the head


# a) histogram & custom output (only use counts & breaks) ---------------------









# b) breaks of histogram & custom output (only use counts & breaks) -----------


















