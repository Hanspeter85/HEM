library(tidyverse)
library(openxlsx)

# Raw data
EEIOT <- read.xlsx("example_EEIOT.xlsx") 

# Extension
Q <- EEIOT[7,2:6] |> as.numeric()

# Intermediate transactions
Z <- EEIOT[1:5, 2:6] |> as.matrix()

# Sector labels
sec_name <- colnames(Z)

# Final demand NULL# Final demand and value added
Y <- EEIOT[1:5, 7]
VA <- EEIOT[6,2:6] |> as.numeric()

# Gross production
x <- rowSums(Z) + Y 

# Check balances
x == colSums(Z) + VA

# Technology matrix
A <- Z %*% diag(1/x)

# Identity matrix
I <- diag(1,5,5)

# Inverse
L <-  solve( I  - A )

# Check 
rowSums(L %*% Y) - x

# Select target sector to extract
extr <- c(2,3)

# Remaining economy
A_star <- A
A_star[extr,] <- 0   

# Inverse depicting multipliers in remaining economy
L_star <- solve(I - A_star)

# Target sector
A_0 <- A - A_star

# Gross production in full economy associated production activities of target sectors
x_0 <- L %*% A_0 %*% L_star %*% Y |> as.numeric()

tmp <- A_0 %*% L_star %*% Y

# Compare gross production value
x_com <- data.frame("gross_production_full_economy" = x,
                    "associated_with_target_sectors" = x_0)

View(x_com)
x_0
x
Y
