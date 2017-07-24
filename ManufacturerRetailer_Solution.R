# Load the libraries 
# To install pcalg library you may first need to execute the following commands:
# source("https://bioconductor.org/biocLite.R")
# biocLite("graph")
# biocLite("RBGL")
# biocLite("Rgraphviz")
library(vars)
library(pcalg)
library(Rgraphviz)
# Read the input data 
data_file <- read.csv('/media/rutvij/Projects/Lecture_Notes/ADBI/Project7/causality/data.csv')

# Build a VAR model 
# Select the lag order using the Schwarz Information Criterion with a maximum lag of 10 
VARselect(data_file, lag.max=10)
VAR_temp <- VAR(data_file, p=1)

# Extract the residuals from the VAR model 
residual_temp <- residuals(VAR_temp)

# Check for stationarity using the Augmented Dickey-Fuller test 
apply(residual_temp, 2, function(x) summary(ur.df(x, type="none")))
# The p-value for all the residuals of three variables indicates that there is evidence
# to reject the null hypothesis. Hence, the residuals follow a stationary pattern.

# Check whether the variables follow a Gaussian distribution  
apply(residual_temp, 2, function(x) ks.test(x, "pnorm"))
# Based on the KS test, we have evidence to reject the null hypothesis that the 
# residuals follow a normal (Gaussian) distribution.

# Write the residuals to a file to build causal graphs using Tetrad software
write.csv(residual_temp, '/media/rutvij/Projects/Lecture_Notes/ADBI/Project7/causality/residuals.csv', row.names=FALSE)

# OR Run the PC and LiNGAM algorithm in R as follows,

# PC Algorithm
pc_temp<-list(C=cor(residual_temp), n=nrow(residual_temp))
pc_fit <- pc(pc_temp, indepTest=gaussCItest, alpha=0.1, labels=colnames(residual_temp), skel.method="original", verbose=TRUE)
plot(pc_fit, main="PC Output")

# LiNGAM Algorithm
lingam_fit <- lingam(residual_temp, verbose=TRUE)
show(lingam_fit)

