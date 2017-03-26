#
#                    Házi feladat 3     
#                    Programozás I.     
#                  2016/17. II. félév 
#                     Farsang Bence      
#                      2017.03.18.
#
#------------------------------------------------------
#                       I.feladat #
#------------------------------------------------------

# clear console & environment
cat("\014")    
rm(list=ls(all=TRUE))

# setting seed
set.seed(12345)

#------------------------------------------------------
#                       II.feladat #
#------------------------------------------------------
#------------------------------------------------------
# 1.
#------------------------------------------------------

# number of vectors
n = 5

# memory pre-allocation
x = NA
y = NA

for (i in 1:n)
{
  # random integers from uniform distr.
  x[i] = sample(10:20, 1, replace = T)
  
  # generating the elements of the vector from uniform distr.
  y[i] = list(runif(x[i], 1, 2))
}

# print the list of vectors
y

#------------------------------------------------------
# 2.
#------------------------------------------------------

# memory pre-allocation
len = NA

# calculating the length of list 'y'
for (i in 1:n)
{
  len [i] = length (y[[i]])
}

# print
len

#------------------------------------------------------
# 3.
#------------------------------------------------------

# calculating the length of list 'y' with lapply
len_apply = lapply(y, function(x) length(x))

# print
len_apply

#------------------------------------------------------
# 4.
#------------------------------------------------------

# calculating the length of list 'y' with sapply
len_sapply = sapply(y, function(x) length(x))

# print
len_sapply

#------------------------------------------------------
#                       III.feladat #
#------------------------------------------------------
#------------------------------------------------------
# 1.
#------------------------------------------------------

# reading data
data(chickwts)
head(chickwts)

#------------------------------------------------------
# 2.
#------------------------------------------------------

# aggregate it following the weight column using the mean function
chick_mean <- aggregate(chickwts$weight, list(chickwts$feed), mean)

#------------------------------------------------------
# 3.
#------------------------------------------------------

# exercise no.2 using decreasing order 
chick_mean[order(chick_mean$x,  decreasing = 1), ]

#------------------------------------------------------
#                       IV.feladat #
#------------------------------------------------------
#------------------------------------------------------
# 1.
#------------------------------------------------------

# matrix dimensions
rows    = 50
columns = 10

# memory pre-allocation
X <- matrix(NA, nrow = rows, ncol = columns)

# rows
for (i in 1:rows)
{
  # columns
  for (j in 1:columns)
  {
    X[i,j] <- rnorm(n = 1, mean = 0, sd = i)
  }}

#------------------------------------------------------
# 2.
#------------------------------------------------------

# memory pre-allocation
stdev = NA

# calculating sd
for (i in 1:rows)
{
  stdev[i] <- sd(X[i,])
}

# print
stdev

#------------------------------------------------------
# 3.
#------------------------------------------------------

# calculating sd using apply
stdev2 <- apply(X, 1, sd)

#print
stdev2

#------------------------------------------------------
# 4.
#------------------------------------------------------

# memory pre-allocation
X_std    <- matrix(NA, nrow = rows, ncol = columns)
X_scaled <- matrix(NA, nrow = rows, ncol = columns)

# defining the max and min values of the matrix
max =  1
min = -1

# rows
for (i in 1:rows)
{
  # columns
  for (j in 1:columns)
  {
    # standardization of the matrix
    X_std[i,j]     =  (X[i,j] - min(X) ) / (max(X) - min(X))
    # rescaling of the matrix
    X_scaled[i,j]  =  X_std[i,j] * (max - min) + min
  }}

# calculating means of the rows using apply
mean_rows <- apply(X_scaled, 1, mean)

#print
mean_rows

#------------------------------------------------------
#                       V.feladat #
#------------------------------------------------------
#------------------------------------------------------
# 1.
#------------------------------------------------------

# package installation
if (!("fivethirtyeight" %in% installed.packages())) {
  install.packages("fivethirtyeight", dependencies  =  T)}

# package loading
library(fivethirtyeight)

# data opening
data(package ="fivethirtyeight")
data(comic_characters)
head(comic_characters)

#------------------------------------------------------
# 2.
#------------------------------------------------------

# package installation
if (!("stringr" %in% installed.packages())){
  install.packages("stringer")
}
library(stringr)

# remove text enclosed in brackets from character names
cleaned_character_names <- str_replace_all(
  string = comic_characters$name,
  pattern = "\\(.*\\)",
  replacement = ""
)

# trim whitespace from start and ending of the character names
comic_characters$name <- str_trim(
  string = cleaned_character_names)