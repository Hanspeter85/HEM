library(data.table)
library(tidyverse)
library(openxlsx)

# Download EXIOBASE MRIO from following link and store in folder
# https://zenodo.org/records/5589597 


## 1. Load and pre-process data for HEM

# Set path to where EXIOBASE MRIO is stored
path <- "C:/Users/hwieland/Data/EXIOBASE/v3_8_2/IOT_pxp/IOT_2015_pxp/"

# Basic dimensions
n <- list("reg" = 49,
          "sec" = 200)

# Load multi-regional transaction matrices 
Z_MRIO <- fread(str_c(path,"Z.txt"), skip = 3)[,3:9802] |> as.matrix()
Y_MRIO <- fread(str_c(path,"Y.txt"), skip = 3)[,3:345]

# Get products name and which sectors refer to materials (see column on the right)
products <- list("full" = read.xlsx("./material_products_for_HEM.xlsx", sheet = "material_products_for_HEM"),
                 "material" = read.xlsx("./material_products_for_HEM.xlsx", sheet = "unique_list"))

# Aggregation function
source("./Agg.R")

# Aggregate Z and Y to one region
key <- rep(1:200, n$reg) |> as.character()

Z <- Agg( Agg(Z_MRIO, key, 2) , key, 1)
Y <- Agg(Y_MRIO, key, 1) |> rowSums()

x <- rowSums(Z) + Y

# Calculate original technology matrix
A <- Z %*% diag(1/x)
A[is.na(A)] <- 0

# Identity matrix
I <- diag(1, n$sec, n$sec)

# Inverse
L <- solve(I - A)

# Check
sum(L %*% Y) == sum(x)

# Get extensions
Q_MRIO <- fread(str_c(path, "./impacts/F.txt"), skip = 3)[,2:9801]
Q_y_MRIO <- fread(str_c(path, "./impacts/F_Y.txt"), skip = 3)[,2:344]
unit <- fread(str_c(path, "./impacts/unit.txt"))

# Select and aggregate GWP100
Q <- Agg(Q_MRIO, key, 2)
Q_y <- rowSums(Q_y_MRIO)
q_gwp <- Q[4,]
q_y_gwp <- Q_y[4]

## 2. HEM

# Select target sectors (= matrials in this case) for extraction
extr <- products$full %>% filter(Materials == 1) %>% pull(Number)

# Remaining economy
A_star <- A
A_star[extr,] <- 0   

Y_star <- Y
Y_star[extr] <- 0

# Inverse depicting multipliers in remaining economy
L_star <- solve(I - A_star)

# Target sectors (= materials)
A_0 <- A - A_star
Y_0 <- Y - Y_star

# Gross production associated with production of materials (see equation 4 in Hertwich 2021 Nature Paper)
x_0 <- ( L %*% Y_0 ) + ( L %*% A_0 %*% L_star %*% Y_star ) |> as.numeric()

# Decompose into amount of materials used in the production of non-materials products 
# (the following lines are a more disaggregated perspective on x_0 as calculated above)
materials_used_by_non_materials <- A_0 %*% L_star %*% Y_star |> as.numeric()

# Gross production of full economy (products in rows) required for producing materials (in columns) 
x_0_matrix <- ( L %*% diag(Y_0) ) + ( L %*% diag(materials_used_by_non_materials) )

# according to Hertwich 2021 paper: Sort for materials in word; change unit to billion tons and give short names (compare result with table in supporting information 1)
materials_used_by_materials <- x_0_matrix[extr,extr] |> as.data.frame()
rownames(materials_used_by_materials) <- colnames(materials_used_by_materials) <- products$full %>% filter(Materials == 1) %>% pull(Name)
materials_used_by_materials <- materials_used_by_materials[products$material$Name, products$material$Name]
rownames(materials_used_by_materials) <- colnames(materials_used_by_materials) <- products$material$short_name
materials_used_by_materials <- round(materials_used_by_materials/1000,0)

write.xlsx(list("materials_by_materials" = materials_used_by_materials),
           "./output/gross_production_of_materials_used_by_materials.xlsx",
           rowNames = TRUE)

# Compare gross production value
x_com <- data.frame("product" = products$full$Name,
                    "material" = products$full$Materials,
                    "share_of_materials_in_sector_production[%]" = round(100*x_0/x,2),
                    "gross_production_full_economy" = round(x, 0),
                    "production_associated_with_materials" = round(x_0,0),
                    "Compare_row_sum_of_x_0_matrix" = round(rowSums(x_0_matrix),0))

x_com[is.na(x_com)] <- 0

colSums(x_com[,-1])

## 3. Estimate carbon footprint and embodied flows associated with materials production

# Direct intensities (characterisation factors)
f <- q_gwp/x
f[is.na(f)] <- 0

# Embodied carbon flow matrix (source of emissions in rows and materials production in column)
CF <- f * x_0_matrix[,products$material$Number] 
colSums(CF)
colnames(CF) <- products$material$short_name

df <- CF %>% 
  bind_cols(products$full) %>% 
  pivot_longer(cols = products$material$short_name,
               names_to = "material_production") %>% 
  rename("embodied_GHGs" = "value",
         "source_product_group" = "source_label",
         "source_product" = "Name") %>% 
  mutate("unit" = unit$unit[4],
         "year" = 2015,
         "region" = "Global") %>% 
  select(year, region, source_product, source_product_group,
         material_production, embodied_GHGs, unit)

write.xlsx(list("raw" = df), "./output/carbon footprint of materials production by source product.xlsx")
  

# Share of materials production in global GWP100 emissions
100* sum(CF) / ( sum(q_gwp) + sum(q_y_gwp) )
