
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

# 🟢find a suitable y limit
(y_max <- ceiling(max(sorted_countries) / 100) * 100)

# barplot
barplot(
  sorted_countries,
  ylim = c(0, y_max),  # 🟢
  col = hcl(0),  # define color
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
(top5cats <- names(table_category[1:5]))

# find subset of rows in top 5 categories
top5cats_data <- subset(forbes, category %in% top5cats)
head(top5cats_data)

# create bivariate table gender*categories
(table_gender_cats <- table(top5cats_data$gender, top5cats_data$category))

# change level names for gender variable
rownames(table_gender_cats)
(rownames(table_gender_cats) <- c("Female", "Male"))

# barplot
barplot(
  table_gender_cats, 
  beside = TRUE,  # show gender groups beside each other
  legend.text = TRUE,  # show legend
  args.legend = list(x = "topright", horiz = TRUE),
  ylim = c(0, 500),
  col = c("wheat", "lightblue"),
  ylab = "Frequency",
  main = "Barplot of Gender Distribution Accross Top 5 Categories"
)


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


















