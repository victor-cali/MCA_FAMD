### Multiple Correspondence Analysis (MCA) ###
##############################################
options(ggrepel.max.overlaps = 10)
# MCA is an extension of the simplest CA procedure. Remember: the purpose is to summarize and visualize a data (contigency) table, but now, with MCA, we consider the possible case of more than two categorical variables. 

# The general goals: 
# 1. Identify individuals with similar profile in their answers
# 2. Identify associations between categories of variables. 

# for the analysis, we're going to use the 'FactoMineR' (A) package and for visualization purposes, we'll use 'factoextra' (B).

library(FactoMineR)
library(factoextra)

data(tea)
colnames(tea)
head(tea)

# Some info about: 
help(tea)
summary(tea)
# The data used here concern a questionnaire on tea. We asked to 300 individuals how they drink tea (18 questions), what are their product's perception (12 questions) and some personal details (4 questions).

# Number of rows and columns
dim(tea)

#For the analysis we are interested in finding out possible relationships occuring in sampled tea consumers, emphasizing in what kind of tea they have (Tea),how they drink it and how they drink it (How) and if they use sugar (Sugar)
#We will create a subtable containing only certain variables 
active<-tea[,c(13,14,15)]

# Active Varialbles: 
# columns: 13(Tea), 14(How), 15(Sugar)
# All other variables will be considered as supplementary variables (both quantitative and qualitative)

### 1.1. Data Format: 
head(active)

### 1.2 Summary
# We may be interested in a quick summary of some or all variables. Let's try a summary of just 5 variables
summary(active)

# i.e., we see the frequency of observations in each of two categories per factor (variable) Or visually: 

par(mfrow=c(1,3))
for(i in 1:3){
  plot(active[,i], main=colnames(active)[i], ylab="counts", col="skyblue",
       ylim=c(0,300))
}

# Variable categories with a very low frequency may distort the analysis. The 
# recommendation is to remove them. 

### 1.3 R Code
# For conducting an MCA over the active individuals, we just type: 
# Check the help! help(MCA)

ob.mca<-MCA(active, ncp=5, graph=FALSE)

# Now, for understanding what is in ob.mca:
MCA(active, ncp=5, graph=TRUE)

# - ncp: number of dimensions kept in the results (by default 5)
# - graph=TRUE is the default option. Try it! I'm sure you'll get an idea of
# the meaning of it.

# ob.mca contains a lot of info. We'll see how to access and interpret them. 

### 2. Visualization and interpretation
# 2.1 Eigenvalues (a.k.a., variances)
eigenvals<-get_eigenvalue(ob.mca)
eigenvals

# Notice that the sum of eigenvalues is not 1. This means the data was not weighted so that all of the variables have the same importance. 

sum(eigenvals[,1])

# We can be quite confident that the first 5 dimensions (number of dimensions kept in the results) store up to the 88% of the total variability. It is quite a good representation. When it comes to 2 dimensions, it only accounts for 40%. It is barely an "acceptable representation"for a Bidimensional Plot

# Visualizing the variances (remember, some authors call these "inertias"):
fviz_screeplot(ob.mca, addlabels=TRUE, ylim=c(0,50))
#Considering 5 dimensions is quite good (as stated), but for graphical representations it gets almost close to being alright..

# 2.2 Biplot
fviz_mca_biplot(ob.mca, 
                repel=TRUE# Avoid text overlapping (can get slow if many points)
)

# The biplot is a complete representation of the data. 
# For both, individuals (blue) and variables (red), the distance between any row or column points is a measure of how (dis)similar they are (the more the distance,the more dissimilar). 
# Rows or column points with similar profiles are close in the biplot. 

# 2.3 Graph of variables
# We can use the function 'get_mca_var()' to extract the results for variable 
# categories. 
# This function returns a list containing
# - the coordinates, 
# - the cos2 and 
# - the contribution of variable categories.

var <- get_mca_var(ob.mca)
var

# The components of 'var' can be used in the plot of ROWS as follows:
# - var$coord: coordinates of variables to create a scatter plot
# - var$cos2: indicates the quality of the variables representation on the
# factor map.
# - var$contrib: contains the contributions (in percentage) of the variables to 
# each of the dimensions. 

# 2.3.1 Correlation between variables and principal dimensions
# To visualize the correlation between variables and MCA principal dimensions,
# type this:

fviz_mca_var(ob.mca, choice = "mca.cor",
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

# With this plot we can identify "clusters" of variables according to they degree of correlation with each dimension. 
# The plot above helps to identify variables that are the most correlated with each dimension; variables Sugar and Tea are the most correlated with (or along) dimension 1. Similarly, the variables How and Tea (again) are the most correlated with (or along) dimension 2.

round(var$coord, 2)
#We can see that black,other and No.sugar contribute (positively) the most to Dim 1, while Lemon and Sigar are the ones who do so but in a negative sense to the same dimension. We can extend this to each of the 5 variables who contain the resulting information 

fviz_mca_var(ob.mca,
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

# The plot above shows the relationships between variable categories.
# The interpretation goes as follows: 
# i) Variable categories with a similar profile are grouped together.
# ii) Negatively correlated variable categories are positioned on opposite sides of the plot (opposed quadrants).
# iii) The distance between category points and the origin measures the quality of the variable category on the factor map. 
# iv) Category points that are away from the origin are well represented on the factor map.

# 2.3.2  Quality of representation of variable categories
# The two dimensions 1 and 2 are sufficient to retain 40% of the total variation in the data.As said before, not that good...
# Thus, not all the points are equally well displayed in the two dimensions.
# We'd like to have a way of measuring the quality of their representation. 
# The quality of the representation is called "the squared cosine" (cos2), which measures the degree of association between variable categories and a particular axis. 
# The cos2 of variable categories can be extracted as follows:

head(var$cos2, 9)

# If a variable category is well represented by two dimensions, the sum of the cos2 is close to one. For some of the rows, more than 2 dimensions are required to perfectly represent the data.

# A nice way to visualize how well categories are represented is b y coloring them according to their cos2 values using the argument col.var = "cos2". 
# This produces a map with color gradient, which can be customized using the argument gradient.cols; e.g., 

# gradient.cols = c("white", "blue", "red") means that:
# - variable categories with low cos2 values will be colored in "white"
# - variable categories with mid cos2 values will be colored in "blue"
# - variable categories with high cos2 values will be colored in "red"

# OK, now putting all that in practice: 
# Color by cos2 values: quality on the factor map
fviz_mca_var(ob.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

# It's also possible to visualize the cos2 of row categories on all the dimensions
# using the corrplot package:
library("corrplot")
par(mfrow=c(1,1))
corrplot(var$cos2, is.corr=FALSE)

# Categories "lemon" and "milk" are not very well represented 
# by the first two dimensions. This implies that the position of the corresponding points on the scatter (bi)plot should be interpreted with some caution. 
# For sure, a higher dimensional solution is probably necessary.
# On the other side, "black", "Earl Grey", "Sugar" and "No Sugar" look (relatively) quite well expressed in 2 dimensions 

### 2.3.3 The contribution of the categories (in %) to the definition of the dimensions can be extracted as follows:

head(round(var$contrib,2), 9)

# Categories with larger values contribute the most to the definition of dimensions.
# Categories that contribute the most to Dim.1 and Dim.2 are the most important in explaining the variability in the data set.

# Contributions of rows to dimension 1
fviz_contrib(ob.mca, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(ob.mca, choice = "var", axes = 2, top = 15)

# Total contribution to dimension 1 and 2
fviz_contrib(ob.mca, choice = "var", axes = 1:2, top = 15)

# The red dashed line on the graph above indicates the expected average value if the contributions were uniform. 
# Calculations of that are out of the scope of the course. We can say that there is evidence to deny the idea of each eigenvalue following a uniform distribution. 

# It can be seen that:
# i) Categories black, sugar, No.sugar and Earl Grey are the most important for defining of the first component.
# ii) Categories green, other and alone (barely) contribute the most to the dimension 2
# iii) The most important (or, contributing) categories can be highlighted on the scatter plot as follows:

fviz_mca_var(ob.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)

# This last graph gives an idea of in which direction and in which component thecategories are contributing most importantly.
# The "black", "No.sugar" and "other" categories have an important 
# contribution in the positive part of the first component, while the "lemon" and "green" categories have a greater contribution in the positive and negative part of the same component (second)

### 3. Graph of individuals ###
# We can use the function 'get_mca_ind()' to extract the results for individuals. 
# This function returns a list containing the coordinates, the cos2 and the contri
# - butions of individuals:

ind <- get_mca_ind(ob.mca)
ind

# The result for individuals gives the same information as described for variable categories. 
# There is no point in repeating all that, right? 
# Here is the display of all that: 

# Get access to the different components by using:
# Coordinates of column points
head(ind$coord)
# Quality of representation
head(ind$cos2)
# Contributions
head(ind$contrib)

# To visualize individuals we use the function fviz_mca_ind() 
# It's also possible to color individuals by their cos2 values (just like categories
# of variables before):

fviz_mca_ind(ob.mca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())

# The next R code creates bar plots of individuals cos2 and their contributions:
# Cos2 of individuals
fviz_cos2(ob.mca, choice = "ind", axes = 1:2, top = 20)

# Contribution of individuals to the dimensions
fviz_contrib(ob.mca, choice = "ind", axes = 1:2, top = 20)

# The next piece of R code colors the individuals by groups using the levels of the variable Tea.
# The argument "habillage" is used to specify the factor variable for coloring the individuals by groups. 
# A concentration ellipse can be also added around each group using the argument addEllipses = TRUE. 
# If you want a confidence ellipse around the mean point of categories, use ellipse.type = "confidence"
# The argument palette is used to change group colors.

################### checkpoint #####################3
#fviz_mca_ind(ob.mca,
             #label = "none", # hide individual labels
             #habillage = "tea", # color by groups
             #palette = c("#00AFBB", "#E7B800","#FC4E07"),
             #addEllipses = TRUE, ellipse.type = "confidence",
             #ggtheme = theme_minimal())

########################## checkpoint #########################

# To specify the value of the argument "habillage", it's also possible to use the index of the column as (habillage = 2).
# Additionally, you can provide an external grouping variable like this habillage = poison$Vomiting.
# habillage = index of the column to be used as grouping variable
fviz_mca_ind(ob.mca, habillage = 1, addEllipses = TRUE)
fviz_mca_ind(ob.mca, habillage = 2, addEllipses = TRUE)
fviz_mca_ind(ob.mca, habillage = 3, addEllipses = TRUE)

# To color individuals using multiple categorical variables at the same time, use the function fviz_ellipses():
fviz_ellipses(ob.mca, habillage= 1, geom = "point")
fviz_ellipses(ob.mca, habillage= 1:2, geom = "point")
fviz_ellipses(ob.mca, habillage= 1:3, geom = "point")

res.mca <- MCA(tea, 
               ind.sup = 295:300,
               quanti.sup = 19:19, 
               quali.sup = c(1:12,16:19,20:36), 
               graph=FALSE)

# Biplot of individuals and variable categories
fviz_mca_biplot(res.mca, repel = TRUE,
                ggtheme = theme_minimal())
# Blue: Active individuals
# Darkblue: Supplementary individuals
# Red: Active variable categories 
# Darkgreen: Supplementary variable categories 































































