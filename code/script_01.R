

# test codes --------------------------------------------------------------

## produce 100 random numbers that follows a normal distribution
x <- rnorm(n = 100, mean = 0, sd = 1)

## estimate mean
mean(x)

## estimate SD
sd(x)


# vector ------------------------------------------------------------------

## vector
#ex.1a manually create a vector using c()
x <- c(1, 3, 4, 8)

#ex.1b character
x <- c("a", "b", "c")

#ex.1c logical
x <- c(TRUE, FALSE, FALSE)

#ex.2 sequence of numbers
x <- 1:5

#ex.3a replicate same numbers or characters
x <- rep(2, 5) # replicate 2 five times

#ex.3b replicate same numbers or characters
x <- rep("a", 5) # replicate "a" five times

#ex.4a use seq() function
x <- seq(1, 5, by = 1)

#ex.4b use seq() function
x <- seq(1, 5, by = 0.1)

#ex.4c use seq() function
x <- seq(1, 5, length = 7)

## vector - check features
x <- c(1.2, 3.1, 4.0, 8.2)
y <- c(1L, 2L)

class(x)
typeof(x)
length(x)
sum(x)
mean(x)

## element ID

# access element 1
x[1]

# access element 1 and 4
x[c(1, 4)]

# access 1 - 3
x[1:3]


# matrix ------------------------------------------------------------------

#ex.1 cbind: combine objects by column
x <- cbind(c(1,2,3), c(4,5,6))

#ex.2 rbind: combine objects by row
x <- rbind(c(1,2,3), c(4,5,6))

#ex.3 matrix: specify elements and the number of rows (nrow) and columns (ncol)
x <- matrix(1:9, nrow = 3, ncol = 3)
x <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)

# some functions
dim(x)
rowSums(x)
colSums(x)

# access
x[1, 2]
x[2, ]
x[c(1, 3), ]


# dataframe ---------------------------------------------------------------

# Create data frame
x <- c("Pristine", "Pristine", "Disturbed", "Disturbed", "Pristine") # Lake type
y <- c(1.2, 2.2, 10.9, 50.0, 3.0) # TSS: total suspended solids (mg/L)
df0 <- data.frame(LakeType = x, TSS = y) # x is named as "LakeType" while y is named as "TSS"

colnames(df0) # call column names

df0$LakeType

df0$TSS

df0[1, ]

df0[, 1]
