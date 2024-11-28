# L11 - functions and iteration ----
# Stat 301-1

## load packages ----
library(palmerpenguins)
library(tidyverse)
library(lubridate)
library(forcats)

penguins <- penguins

## Exercises ----

### Ex 1 ----

# function 1

mean(is.na(x))
# supposed to find the proportion of values that are NAs

function_1 <- function(x) {mean(is.na(x))}

# test 
function_1(c(2, 7, NA, 3, NA))

# function 2
x / sum(x, na.rm = TRUE)

# supposed to find the proportion each value contributes to total

function_2 <- function(x) {x / sum(x, na.rm = TRUE)}

# test
function_2(c(3, 5, 7))

# function 3

round(x / sum(x, na.rm = TRUE) * 100, 1)

# supposed to find the percent each value contributes to total,
# rounded to tenths place

function_3 <- function(x) {round(x / sum(x, na.rm = TRUE) * 100, 1)}

function_3(c(3, 4, 5))


### Ex 2----

# Write a function that returns how old you are in years, given your birthday (as `"YYYY-MM-DD"`). 

age_years <- function(x) {interval(ymd(x), today()) |>  as.period() |>  year()}

age_years("2001-11-16")


### Ex 3----
# Write a function that computes the mean, standard deviation, and count given a dataset and a numeric variable.

dataset_summary <- function(df, variable) {
  df |> summarize(mean = mean(variable, na.rm = TRUE),
                  sd = sd(variable, na.rm = TRUE),
                  n = n())}

dataset_summary(penguins, penguins$body_mass_g)


### Ex 4----

# Write a function that counts the number of missing observations given a dataset and a variable.

dataset_missing <- function(df, variable) {
  df |> 
    filter(is.na(variable) == TRUE) |> 
    summarize(
    number_missing = n()
  )
}

# Test your function using the `penguins` dataset and a variable of your choice.
dataset_missing(penguins, penguins$body_mass_g)


### Ex 5----

# Write a function that draws a scatterplot given a dataset and x and y variables. Add a line of best fit and an appropriate title to the plot.

scatterplot <- function(df, x, y, title) {
  df |> 
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = loess, se = FALSE) +
    labs(title = title)}

# Test your function using the `penguins` dataset and an x and y variable of your choice.
scatterplot(penguins, penguins$body_mass_g, penguins$bill_length_mm, "Penguins Body Mass (g) vs. Bill Length (mm)")


### Ex 6----
# Use the `across()` function to...

# a) compute the number of unique values in each column of `penguins`

penguins |> 
  summarize(across(everything(), n_distinct))
`
# b) compute the mean of every numeric column of penguins.

penguins |> 
  summarize(
    means = across(where(is.numeric), ~ mean(., na.rm = TRUE)))

### Ex 7----

# Write code that uses one of the map functions to:

# a) compute the number of unique values in each column of penguins

number_unique <- function(df) {
  df |> 
    summarize(across(everything(), n_distinct))
}

number_unique(penguins)

# b) compute the mean of numeric every column in penguins.

means_numeric <- function(df) {
  df |> 
    summarize(across(where(is.numeric), ~ mean(., na.rm = TRUE)))
}

means_numeric(penguins)


### Ex 08----
# Write a for loop that uses your function in Exercise 3 to compute the mean, 
# standard deviation, and count of every numeric variable in the penguins dataset.
# Output the results as a nicely formatted table

dataset_summary <- function(df, variable) {
  df |> summarize(mean = mean(variable, na.rm = TRUE),
                  sd = sd(variable, na.rm = TRUE),
                  n = n())}

list_of_summaries <- list()

for (col in names(penguins)) {
  ifelse(is.numeric(penguins[[col]]) == TRUE,
         list_of_summaries[[col]] <- dataset_summary(penguins, penguins[[col]]),
         col_nonnumeric <- col
  )

}

as.tibble(list_of_summaries)


### Ex 09----

# Write a for loop that plots the distribution of each variable in the penguins
# dataset and saves each plot to a \plots subdirectory

numeric_plot <- function(df, variable) {
  ggplot(data = df, aes(x = {{variable}})) +
    geom_histogram(stat = "count") +
    labs(
      title = "Distribution"
    )
}

categorical_plot <- function(df, variable) {
  ggplot(data = df, aes(x = {{variable}})) +
    geom_bar() +
    labs(
      title = "Distribution"
    )
}

for (col in names(penguins)) {
  filename <- str_c("plots/", col, "distribution.png", sep = "")
  
  ifelse(is.numeric(penguins[[col]]) == TRUE, 
         plot <- numeric_plot(penguins, col),
         plot <- categorical_plot(penguins, col)
         )
  
  ggsave(filename, plot)
}


### Ex 10----

# Determine which variables in the mpg dataset should be factors and convert them to factors using a for loop.

variables_to_factor <- c("manufacturer", "model", "drv", "trans", "class")

for (var in variables_to_factor) {
  mpg[[var]] <- as_factor(mpg[[var]])
}


### Ex 11----

# First, store their paths in a vector: `files <- dir("data/", pattern = "\\.csv$", full.names = TRUE)`.

# a) Write a for loop that will read in each file individually.

files <- dir("data/", pattern = "\\.csv$", full.names = TRUE)

individual_files <- list()

for (file in files) {
  individual_files[[file]] <- read.csv(file)
}

# b) Write a for loop that will load the files into a single data frame.

single_frame <- list()
  
for (file in files) {
  df <- read_csv(file)
  single_frame <- append(single_frame, list(df))
}

single_frame <- bind_rows(single_frame)

