exammarks <- read.table("exam.txt", header = T)
str(exammarks)
examcore <- exammarks[-1,-9]
n <- nrow(examcore)

library(corrplot)
library(psych)
library(reshape2)
library(ggplot2)
library(GGally)
library(gridExtra)

# Separate A and B questions
marks_A <- examcore[,1:4]
marks_B <- examcore[5:8]

# Let's standarize the data so that we can compare
marks_A_stand <- scale(marks_A)
marks_B_stand <- scale(marks_B)

median_A_stand <- median(unlist(marks_A_stand)) # 0.198
median_B_stand <- median(unlist(marks_B_stand)) # 0.2425

# Ajust the margins
par(mar = c(5, 5, 2, 2))
par(mfrow = c(1, 2)) 

# Boxplot for A marks
boxplot(marks_A_stand,main="A marks",xlab="A questions",ylab="Marks",col="lightblue")

# Boxplot for B marks
boxplot(marks_B_stand,main="B marks",xlab="B questions",ylab = "Marks",col="lightgreen")

# Parallel Coordinate Plots
par(mfrow = c(1, 2))
par(mar = c(4, 4, 2, 1))
plot1 <- ggparcoord(marks_A_stand)
plot2 <- ggparcoord(marks_B_stand)
grid.arrange(plot1, plot2, ncol = 2)

# Convert data to long format for ggplot
examcore_long <- melt(as.data.frame(examcore), variable.name = "Question", value.name = "Score")

# jitter plot of exam scores
ggplot(examcore_long, aes(x = Question, y = Score)) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  labs(title = "Jitter Plot of Exam Scores", x = "Exam Question", y = "Score") +
  theme_minimal()


###### CLUSTER ANALYSIS ######
par(mfrow =c(1,2))
label.x <- c("A1","A2","A3","A4","B1","B2","B3","B4")
unicor <- cor(examcore)
cordist <- 0.5-unicor/2
dist <-as.dist(cordist)


## Complete Linkage ##
h <- hclust(d = dist, method = "complete") # change d parameter to see if it better suits 
## Visulasing the path
plot(x = h, labels = label.x, main="Complete linkage")

h.cl<-c()
h.cl[1] <- h$height[1]
for(i in 2:length(h$height))h.cl[i] <- (h$height[i]-h$height[i-1])
which.max(h.cl)

h.max<-h$height[which.max(h.cl)]
abline(h=h.max, col="purple") 
# Highlights the cut-off level where 2 clusters appear to be the best solution.


## Average Linkage ##
h1 <- hclust(d = dist, method = "average") # change d parameter to see if it better suits 
## Visualazing the path
plot(x = h1, labels = label.x, main="Average linkage")

# Finding the optimal number of clusters Again
h.cl<-c()
h.cl[1] <- h1$height[1]
for(i in 2:length(h1$height))h.cl[i] <- (h1$height[i]-h1$height[i-1])
which.max(h.cl)
h.max1<-h1$height[which.max(h.cl)]
abline(h=h.max1, col="green")


#### PCA ####
# correlation matrix
R <- as.matrix(cor(examcore))
n <- nrow(examcore)
par(mfrow = c(1,1))
corrplot(R, method = "number", type = "upper", tl.cex = 0.8, number.cex = 0.8, col = colorRampPalette(c("white", "salmon", "darkblue"))(200))
melt_R <- melt(R)
sorted_melt_R <- melt_R[order(-abs(melt_R$value)), ]
head(sorted_melt_R, 20)
# B2, B3
# B4, B2
# B2, A4
# B3, A4 

pca_result <- prcomp(examcore, scale = TRUE) # pca on correlation matrix
summary(pca_result)           # Look at the proportion of variance explained
round(pca_result$rotation, 3)  # matrix of the loadings

# Choose the number of principal components to retain
plot(pca_result, type = "lines", main = "Scree Plot of PCA")  

#PCA biplot
biplot(pca_result, 
       col = c("blue", "red"),   
       cex = c(0.8, 1.2))  


# Extract rows corresponding to outlier students from exammarks (outliers in the biplot)
outliers <- exammarks[c("TWFC1", "XDWP6", "VXDZ7", # highest
                        "VNCR6", # centre high
                        "SKNR1", # lowest
                        "TCCR9", "TXSR2", # low, right
                        "WKBC4", # far left --> best score
                        "XBVY0", # origin
                        "SXYK4",# top left
                        "TLFB3" # PC2 = 0, on the right
                        ), ]

outliers$Student <- rownames(outliers)
# Rearrange so that "Student" appears first
outliers <- outliers[, c("Student", setdiff(colnames(outliers), "Student"))]
print(outliers)


#### FACTOR ANALYSIS - PFA ####
pfa1 <- fa(r = R, nfactors = 1, fm = "pa", rotate = "none") 
pfa2 <- fa(r = R, nfactors = 2, fm = "pa", rotate = "none") 
pfa3 <- fa(r = R, nfactors = 3, fm = "pa", rotate = "none") 

# Function to print loadings and communalities
print_results <- function(model, nfactors) {
  cat("\n---", nfactors, "Factor Model ---\n")
  cat("\nLoadings:\n")
  print(model$loadings)  # Print factor loadings
  cat("\nCommunalities:\n")
  print(model$communality)  # Print communalities
}

# Print results for all models
print_results(pfa1, 1)
print_results(pfa2, 2)
print_results(pfa3, 3)

# COMMUNALITIES = proportion of variance in each variable that is explained by the factors.
comm_1 <- pfa1$communality
comm_2 <- pfa2$communality
comm_3 <- pfa3$communality
communalities <- as.data.frame(cbind(comm_1, comm_2, comm_3))
colnames(communalities) <- c("1-Factor Model", "2-Factor Model", "3-Factor Model")
print(communalities)

# Rotation (e.g., varimax) only redistributes the factor loadings
# to make the structure more interpretable, but it does not change the total variance explained by the factors.
pfa1varimax <- fa(r = R, nfactors = 1, fm = "pa", rotate = "varimax")
pfa2varimax <- fa(r = R, nfactors = 2, fm = "pa", rotate = "varimax")
pfa3varimax <- fa(r = R, nfactors = 3, fm = "pa", rotate = "varimax")

pfa1varimax$loadings
pfa2varimax$loadings
pfa3varimax$loadings

# FA diagrams
par(mfrow = c(1,3))
fa.diagram(pfa1varimax, main = "1 factor")
fa.diagram(pfa2varimax, main = "2 factors")
fa.diagram(pfa3varimax, main = "3 factors")

# the 2nd factor is more highly correlated to the first 2 questions
# while the 1st factor with the others

# how well the models reproduces the observed covariance (or correlation) matrix?
# 1 factor model, 2 factor model, 3 factor model
mlfa1 <- factanal(covmat = R, factors = 1, n.obs = n, rotation="none")
mlfa2 <- factanal(covmat = R, factors = 2, n.obs = n, rotation="none")
mlfa3 <- factanal(covmat = R, factors = 3, n.obs = n, rotation="none")

# Print MLFA Results: Model Fit & Uniqueness
print(mlfa1, digits = 3)  # Factor 1 explains 50.1% variance, A1 & A2 have high uniqueness
print(mlfa2, digits = 3)  # Factor 2 adds little variance (3.8%), structure remains weak
print(mlfa3, digits = 3)  # Factor 3 improves fit (p = 0.699), A1 separated with PA3












