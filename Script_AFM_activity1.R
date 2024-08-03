
#####################################################################
# Analítica de Datos
# Análisis Factorial Múltiple - Actividad 1
# Ingeniería de Datos e IA
# Universidad Autónoma de Occidente

# Presentado por: Angélica Portocarrero y Xilena Rojas
#####################################################################

#########
## EDA ##
#########

to_be_loaded <- c("readxl", 
                  "dplyr",
                  "tidyr",
                  "ggplot2",
                  "xtable",
                  "corrplot",
                  "patchwork",
                  "FactoMineR",
                  "factoextra",
                  "cluster",
                  "tseries"
)

for (pck in to_be_loaded) {
  if (!require(pck, character.only = T)) {
    install.packages(pck, repos="http://cran.rstudio.com/")
    stopifnot(require(pck, character.only = T))
  }
}

# Load the necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)
library(corrplot)
library(patchwork)
library(FactoMineR)
library(factoextra)
library(cluster)
library(tseries)


# Getting the data from the xlsx file
consumer_preferences <- read_excel("df_consumer_preferences.xlsx")
consumer_preferences

# Dataset dimension
dim(consumer_preferences)

# Checking for the presence of null and duplicate values.
which(is.na(consumer_preferences)) # 0
which(duplicated(consumer_preferences)) # 0


# Descriptive statistics - quantitative variables

resumen <- summary(consumer_preferences[, 1:6])
resumen

# Print table in latex code
table_resumen <- xtable(resumen)
print(table_resumen, type = "latex")

cv <- function(x) {
  sd(x) / mean(x) * 100
} 
lapply(consumer_preferences[, 1:6], sd) # Standar Desviation 
lapply(consumer_preferences[, 1:6], cv) # Coefficient of Variation


# Descriptive analysis - qualitative variables

### Frequency dataframes
gender_data <- as.data.frame(table(consumer_preferences$gender))
colnames(gender_data) <- c("gender", "count")

education_level_data <- as.data.frame(table(consumer_preferences$education_level))
colnames(education_level_data) <- c("education_level", "count")

### Viewing information on barplots

gender_data <- gender_data %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(gender_data, aes(x = gender, y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count),
            vjust = -0.3,
            size = 6) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(y = "Number of participants", title = "Distribution by Gender") +
  theme_minimal()


education_level_data <- education_level_data %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(education_level_data, aes(x = education_level, y = count, fill = education_level)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count),
            vjust = -0.3,
            size = 5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(y = "Number of participants", title = "Distribution by Education Level") +
  theme_minimal()

### Contingency y marginal table
contigency_table <- table(consumer_preferences$gender, consumer_preferences$education_level)
marginal_table <- addmargins(contigency_table)
marginal_table

# Print table in latex code
marginal_table_latex <- xtable(marginal_table)
print(marginal_table_latex, type = "latex")

### Viewing information on piecharts

percentage_data <- consumer_preferences %>%
  group_by(gender, education_level) %>%
  summarise(count = n()) %>%
  group_by(gender) %>%
  mutate(percentage = count / sum(count) * 100) # Calculate percentages for education_level status by gender

ggplot(percentage_data, aes(x = "", y = percentage, fill = education_level)) +
  geom_bar(width = 1, stat = "identity", position = "fill") +
  coord_polar("y") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_fill(vjust = 0.5), size = 4) +
  labs(title = "Education Level Distribution by Gender", fill = "Education Level") +
  scale_fill_manual(values = c("Bachelor's" = "lightcoral", "High School" = "pink",
                               "Master's" = "skyblue2", "PhD" = "lightgreen")) +
  facet_wrap(~ gender) + # divides the chart into multiple panels (by gender)
  theme_void() # Create a pie chart with percentages for education_level status by gender



matriz_cor <- cor(consumer_preferences[, 1:6])
corrplot(matriz_cor, method="number", tl.pos="lt", tl.srt=45)


#Boxplots per gender
p1 <- ggplot(consumer_preferences, aes(x = gender, y = product_quality, fill = gender)) +
  geom_boxplot() +
  ggtitle("Product Quality")

p2 <- ggplot(consumer_preferences, aes(x = gender, y = product_durability, fill = gender)) +
  geom_boxplot() +
  ggtitle("Product Durability")

p3 <- ggplot(consumer_preferences, aes(x = gender, y = preference_taste, fill = gender)) +
  geom_boxplot() +
  ggtitle("Preference Taste")

p4 <- ggplot(consumer_preferences, aes(x = gender, y = preference_price, fill = gender)) +
  geom_boxplot() +
  ggtitle("Preference Price")

p5 <- ggplot(consumer_preferences, aes(x = gender, y = age, fill = gender)) +
  geom_boxplot() +
  ggtitle("Age")

p6 <- ggplot(consumer_preferences, aes(x = gender, y = income, fill = gender)) +
  geom_boxplot() +
  ggtitle("Income")

# Organize plots with Patchwork lib
(p1 | p2) / (p3 | p4) / (p5 | p6)

#Boxplots per Education level
p1 <- ggplot(consumer_preferences, aes(x = education_level, y = product_quality, fill = education_level)) +
  geom_boxplot() +
  ggtitle("Product Quality")

p2 <- ggplot(consumer_preferences, aes(x = education_level, y = product_durability, fill = education_level)) +
  geom_boxplot() +
  ggtitle("Product Durability")

p3 <- ggplot(consumer_preferences, aes(x = education_level, y = preference_taste, fill = education_level)) +
  geom_boxplot() +
  ggtitle("Preference Taste")

p4 <- ggplot(consumer_preferences, aes(x = education_level, y = preference_price, fill = education_level)) +
  geom_boxplot() +
  ggtitle("Preference Price")

p5 <- ggplot(consumer_preferences, aes(x = education_level, y = age, fill = education_level)) +
  geom_boxplot() +
  ggtitle("Age")

p6 <- ggplot(consumer_preferences, aes(x = education_level, y = income, fill = education_level)) +
  geom_boxplot() +
  ggtitle("Income")

# Organize plos with Patchwork lib
(p1 | p2) / (p3 | p4) / (p5 | p6)

#########
## MFA ##
#########

# Using FAMD
AFM <- FAMD(consumer_preferences, graph = TRUE)
AFM 
AFM$eig ## Valores propios


# Eigen values
valores.propios <- get_eigenvalue(AFM)

#Scree plot
fviz_screeplot(AFM)

# Variables analysis
Variables <- get_famd_var(AFM)
Variables

# Coordenates
head(Variables$coord)
# Cos2: representation quality
head(Variables$cos2)
# Contributions by dimension
head(Variables$contrib)

# Contributions by dimension (qualitative variables)
quali_contrib <- get_famd_var(AFM, "quali.var")$contrib
quali_contrib_df <- as.data.frame(quali_contrib)

quali_contrib_df$Variable <- c(rep("gender", 2), rep("education_level", 4))# Qualitative variable names to the corresponding rows
grouped_contrib <- aggregate(. ~ Variable, data = quali_contrib_df, sum)
grouped_contrib


# Variables graph
fviz_famd_var(AFM, choice = c("var","quanti.var", "quali.var"), 
              repel = TRUE, col.var = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) 


# First dimension contribution
fviz_contrib(AFM, "var", axes = 1)
# Second dimension contribution
fviz_contrib(AFM, "var", axes = 2)


# Analysis of quantitative variables
Var.quanti <- get_famd_var(AFM, "quanti.var")
Var.quanti

# Contribution chart
fviz_famd_var(AFM, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

# Representation quality chart
fviz_famd_var(AFM, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)

# Analysis of qualitative variables

Var.cuali <- get_famd_var(AFM, "quali.var")
Var.cuali

fviz_famd_var(AFM, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# Quality of representation
Var.cuali$cos2[,1:2]

apply(Var.cuali$cos2[,1:2],1,sum)


# Individuals chart
fviz_famd_ind(AFM, col.ind = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

fviz_famd_ind(AFM, habillage = "education_level", 
              palette = c("red", "blue", "green", "pink"),
              addEllipses = TRUE,
              repel = TRUE)


#Cluster
clusterMFA <- HCPC(AFM)
###################################################
#  Finding the optimal number of clusters

# Elbow Method
fviz_nbclust(consumer_preferences[,1:6], kmeans, method = "wss")
labs(subtitle = "Elbow method") # Entre 2 y 3

# Silhouette Method
fviz_nbclust(consumer_preferences[,1:6], kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method") # 8

# Gap Statistics
set.seed(123)
gap_stat <- clusGap(consumer_preferences[,1:6], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# nstart = 25 - 25 is used because it is the usual value to improve 
# the stability of the result.

fviz_gap_stat(gap_stat) + 
  labs(subtitle = "Gap Statistic method")

# ! Note:
# B (number of bootstrap simulations):
# Specifies how many times random data sets will be generated to compare
# the variation within the observed clusters with what is randomly expected. 
# Increasing B improves the precision of the gap estimate.

# nstart (number of random starts in k-means):
# Defines how many times the k-means algorithm will be run with different initializations
# of centers to avoid local solutions. Increasing nstart improves the robustness and # consistency of clustering results 
# consistency of clustering results.


###################################################

clusterMFA$desc.var ## test values

df_cluster <- clusterMFA$data.clust

AFM <- FAMD(df_cluster,
            sup.var = 9) # Setting “clust” as a supplementary variable

fviz_mfa_ind(AFM,
             habillage = "clust", # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,
             repel = TRUE # Avoid text overlapping
)

# EDA with clusters

p1 <- ggplot(df_cluster, aes(x = clust, y = product_quality, fill = clust)) +
  geom_boxplot() +
  ggtitle("Product Quality")

p2 <- ggplot(df_cluster, aes(x = clust, y = product_durability, fill = clust)) +
  geom_boxplot() +
  ggtitle("Product Durability")

p3 <- ggplot(df_cluster, aes(x = clust, y = preference_taste, fill = clust)) +
  geom_boxplot() +
  ggtitle("Preference Taste")

p4 <- ggplot(df_cluster, aes(x = clust, y = preference_price, fill = clust)) +
  geom_boxplot() +
  ggtitle("Preference Price")

p5 <- ggplot(df_cluster, aes(x = clust, y = age, fill = clust)) +
  geom_boxplot() +
  ggtitle("Age")

p6 <- ggplot(df_cluster, aes(x = clust, y = income, fill = clust)) +
  geom_boxplot() +
  ggtitle("Income")

(p1 | p2) / (p3 | p4) / (p5 | p6)

# Check differences in variables by cluster
t.test(product_quality ~ clust, data = df_cluster)
t.test(preference_taste ~ clust, data = df_cluster)
t.test(preference_price ~ clust, data = df_cluster)
t.test(product_durability ~ clust, data = df_cluster)
t.test(income ~ clust, data = df_cluster)
t.test(age ~ clust, data = df_cluster)


