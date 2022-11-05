### FAMD: Palmer Archipelago (Antarctica) Penguin Data ###

### Importing data
nsp<-read.csv("penguins.csv")

# Structure
str(nsp)

# Factor conversion 
nsp[,1]<-as.factor(nsp[,1])
nsp[,2]<-as.factor(nsp[,2])
nsp[,3]<-as.factor(nsp[,3])

str(nsp) 
df<-nsp

# Descriptive analysis per penguin species
summary(df[1:146,])
summary(df[147:214,])
summary(df[215:333,])

# Libraries
library("ggplot2")
library("FactoMineR")
library("factoextra")

# Compute FAMD
ob<- FAMD(df, ncp = 7, graph = FALSE)


# Eigenvalues / Variances
eig.val <- get_eigenvalue(ob)


# Scree plot 
fviz_screeplot(ob)

# Variables
var <- get_famd_var(ob)

# Coordinates of variables
var$coord

# Goodness of representation on the factor map
var$cos2

# Contributions to the  dimensions
var$contrib

## Plot of variables
fviz_famd_var(ob, repel = TRUE)

##  Contribution to the first dimension
fviz_contrib(ob, "var", axes = 1)

## Contribution to the second dimension
fviz_contrib(ob, "var", axes = 2)  

# Graphs of quantitative variables
quanti.var <- get_famd_var(ob, "quanti.var")

# Contribution of quantitative variables
fviz_famd_var(ob, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

# Goodness of representation for quantitative variables
fviz_famd_var(ob, "quanti.var", col.var = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

# Graphs of qualitative variables
quali.var <- get_famd_var(ob, "quali.var")

# Contribution of qualitative variable categories
fviz_famd_var(ob, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Goodness of representation for qualitative variable categories
fviz_famd_var(ob, "quali.var", col.var = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)  

# Graphs of individuals
ind <- get_famd_ind(ob)

#  Individuals goodness of representation
fviz_famd_ind(ob, col.ind = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)
# Individuals contribution to dimensions
fviz_famd_ind(ob, col.ind = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

# Factor Maps for 2 categorical variables combinations
fviz_ellipses(ob, c("island","species"), repel = TRUE)
fviz_ellipses(ob, c("island","sex"), repel = TRUE)
fviz_ellipses(ob, c("species","sex"), repel = TRUE)




















