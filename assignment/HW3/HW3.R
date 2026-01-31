
# CS544 HW3
# Date: Jan 27-31, 2026
# R Version: 4.5.2


# Part 1 ======================================================================

# init dataset
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

# find a suitable y limit
(y_max_1a <- ceiling(max(sorted_countries) / 100) * 100)

# store default margin & modify margin
(default_mar <- par()$mar)
par(mar = c(9,7,4,4))

# barplot
barplot(
  sorted_countries,
  ylim = c(0, y_max_1a),  # avoid hard coding
  las = 2,  # par: labels perpendicular to axis
  col = "rosybrown",  # define color
  ylab = "Count of Billionaires",  # y label
  main = "Barplot for Count of Billionaires in Rich Countries"
)

# restore defualt margin
par(mar = default_mar)


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


# c) f/m across top 5 categories (bivariate) ----------------------------------

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

# find max of pretty y ticks
y_max_1c <- max(pretty(table_gender_cats))
    # pretty function returns numeric vector of n increasing numbers
    # which are "pretty" in decimal notation (eg. for axes ticks)

# barplot
barplot(
  table_gender_cats, 
  beside = TRUE,  # show gender groups beside each other
  legend.text = TRUE,  # show legend
  args.legend = list(
    x = "topright",  # legend position
    horiz = TRUE,  # horizontally arrange legend labels
    bty = "n"  # box type of the box surrounding legend, "n" means none
  ),
  ylim = c(0, y_max_1c),  # avoid hard coding
  col = c("wheat", "lightblue"),
  ylab = "Frequency",
  main = "Barplot of Gender Distribution Across Top 5 Categories"
)


# d) inferences ---------------------------------------------------------------

# According to Figure 1, the United States stands out significantly with 
# a far greater count of billionaires (about 9 hundred) than all other 
# countries, demonstrating its dominant possession of billionaire and worth 
# among the world. China ranks second following the US with a significant 
# number of billionaires (about 5 hundred) as well. 

# As for Figure 2, 87% percent of these billionaires are male, which is 
# more than 6 times that of female, showing male’s dominant status in 
# the top-ranking rich people, as well as indicating a great 
# gender unbalance regarding possessing huge amount of worth.

# Moving on to Figure 3, Finance and Investments field shows the greatest 
# number of billionaires. In the meantime, this figure also shows 
# a huge gap between the number of male and female billionaires, 
# where men account for the majority in all top 5 categories.


# Part2 =======================================================================

# init dataset
us_quarters <- read.csv("https://people.bu.edu/kalathur/datasets/us_quarters.csv")
head(us_quarters)


# a) state with min/max num of quarters by each mint --------------------------

# find states with highest/lowest number by each mint
(min_Denver <- us_quarters[which.min(us_quarters$DenverMint),"State"])
(max_Denver <- us_quarters[which.max(us_quarters$DenverMint),"State"])

(min_Philly <- us_quarters[which.min(us_quarters$PhillyMint),"State"])
(max_Philly <- us_quarters[which.max(us_quarters$PhillyMint),"State"])

# print out results
paste(
  sprintf(
    "DenverMint produced most quarters for %s and least for %s.", 
    max_Denver, min_Denver
  ),
  sprintf(
    "PhillyMint produced most quarters for %s and least for %s.",
    max_Philly, min_Philly
  )
)


# b) barplot: matrix for the two mints ----------------------------------------

# store the data into a matrix
m <- as.matrix(us_quarters[-1]) # exclude the 1st column

# find suitable y limit
y_max_2b <- max(pretty(m))

# adjust parameters
  par(
      mai=c(2, 1, 1, 0.5),  # margin in inches
      cex=0.8  # all text size become 80% of original
    )

# asjust global option to avoid scientific notation
options(scipen = 999)

# barplot
barplot(
  t(m),  # transpose matrix
  beside = TRUE,  # let bar groups beside each other
  ylim = c(0,y_max_2b),  # y limits
  names.arg = us_quarters$State,  # category names
  las = 2,  # let labels perpendicular to axis
  col = c('blue', 'grey'),  # colors

  legend.text = T,  # show legend
    args.legend = list(
      text.width = 17,  # legend width
      x = "topright",  # legend position
      x.intersp = 1,  # inter space
      y.intersp = 0.7,  # line space
      cex = 0.8  # text size becomes 80% of original
    )
)


# two striking inferences

# According to Figure 4, the PhillyMint shows a dominant production volume 
# for state Virginia, reaching a billion units, which is far more than 
# that of DenverMint in Virginia (less than 700 million). This is also 
# the highest peak in the entire dataset.

# In addition, Connecticut is the state for which the largest 
# production volume by DenverMint was (about 700 million).



# c) side-by-side box plots for the two mints ---------------------------------

# set parameters
par(
  mar=c(4, 8, 4, 6), cex=0.8,  # margin and general text size
  mgp = c(5, 1, 0)  # move y axis title left to avoid overlap with y ticks
)

# Drawing the side-by-side boxplot
boxplot(
  us_quarters$DenverMint, us_quarters$PhillyMint,
  col = c("blue", "grey"),  # colors
  las = 1,  # keep y labels horizontal
  outline = TRUE,  # show outliers if exist
  names = c("Denver Mint", "Philly Mint"),  # category names
  ylab = "Number of Quarters",  # y label
  main = "Comparison of Quarters Produced by Mint"  # title
)


# two striking inferences for each of the box plots

# 🔴从这继续




# d) outliers states (5-num-summary) ------------------------------------------








# reset global option & par
options(scipen = 0)
par(cex = 1)



# Part 3 ======================================================================

# init dataset
stocks <- read.csv("https://people.bu.edu/kalathur/datasets/stocks_2024.csv")
head(stocks)  # view the head


# a) pair wise plots (6 stocks in 1 plot) -------------------------------------







# b) correlation matrix (rounded to 2 decimals) -------------------------------






# c) interpretations (≥4) -----------------------------------------------------








# top 3 correlated stocks for each stock (use loops)








# Part 4 ======================================================================

# init dataset
scores <- read.csv("https://people.bu.edu/kalathur/datasets/scores.csv")
head(scores)  # view the head


# a) histogram & custom output (only use counts & breaks) ---------------------









# b) breaks of histogram & custom output (only use counts & breaks) -----------


















