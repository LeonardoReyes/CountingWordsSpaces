# Scripted by: J.L. Reyes Acosta
# e-mail: leonardo.reyes@mindshareworld.com
# MindShare WW Business planning

#CORRESPONDANCE ANALYSIS FOR CROSSTABS



# load packages
require(FactoMineR)
require(ggplot2)


# load data MCA1
Rawdata<-read.csv("MCA_FOREVER_MEDIA.csv", sep = ",")
data<-Rawdata[,1:12]
#colnames(data)<-as.matrix(Rawdata[1,2:14])
rownames(data)<-as.matrix(Rawdata[,1])

# load data MCA2
# Rawdata<-read.table("MCA2.csv", sep = ",")
# data2<-Rawdata[2:16,2:5]
# colnames(data2)<-as.matrix(Rawdata[1,2:5])
# rownames(data2)<-as.matrix(Rawdata[2:16,1])

#data(tea)
# select these columns
#newtea = tea[,c("Tea","How","how","sugar","where","always")]
# number of categories per variable
cats1 = apply(data, 2, function(x) nlevels(as.factor(x)))
#cats2 = apply(data2, 2, function(x) nlevels(as.factor(x)))

# apply MCA
mca1 <- MCA(data)
#completed <- imputeMCA(data)
res.mca <- MCA(data)
summary(res.mca)
plot(res.mca,invisible=c("var","quali.sup","quanti.sup"),cex=0.7)
plot(res.mca,invisible=c("ind","quali.sup","quanti.sup"),cex=0.8)
plot(res.mca,invisible=c("quali.sup","quanti.sup"),cex=0.8)
dimdesc(res.mca,axes=1:2)
plotellipses(res.mca,keepvar=4:7)
#plotellipses(res.mca,keepvar="Female")

#Save MCA
write.table(res.mca$var$coord,file="VarForevermark_MEDIA.csv",sep=",")
write.table(res.mca$ind$coord,file="IndForevermark_MEDIA.csv",sep=",")

mca2 = MCA(data2, graph=TRUE)
# table of eigenvalues
mca1$eig
# column coordinates
mca1$var$coord
# row coordinates
mca1$ind$coord

# data frames for ggplot
mca1_vars_df = data.frame(mca1$var$coord, Variable=rep(names(cats1), cats1))
mca1_obs_df = data.frame(mca1$ind$coord)

mca2_vars_df = data.frame(mca2$var$coord, Variable=rep(names(cats2), cats2))
mca2_obs_df = data.frame(mca2$ind$coord)


# plot of variable categories
ggplot(data=mca1_vars_df, aes(x=Dim.1, y=Dim.2, label=rownames(mca1_vars_df))) +
  geom_hline(yintercept=0, colour="gray70") +
  geom_vline(xintercept=0, colour="gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR")


# plot of variable categories
ggplot(data=mca2_vars_df, aes(x=Dim.1, y=Dim.2, label=rownames(mca2_vars_df))) +
  geom_hline(yintercept=0, colour="gray70") +
  geom_vline(xintercept=0, colour="gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR")


# MCA plot of observations and categories
ggplot(data=mca1_obs_df, aes(x=Dim.1, y=Dim.2)) +
  geom_hline(yintercept=0, colour="gray70") +
  geom_vline(xintercept=0, colour="gray70") +
  geom_point(colour="gray50", alpha=0.7) +
  geom_density2d(colour="gray80") +
  geom_text(data=mca1_vars_df, 
            aes(x=Dim.1, y=Dim.2, label=rownames(mca1_vars_df), colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR") +
  scale_colour_discrete(name="Variable")


ggplot(data=mca2_obs_df, aes(x=Dim.1, y=Dim.2)) +
  geom_hline(yintercept=0, colour="gray70") +
  geom_vline(xintercept=0, colour="gray70") +
  geom_point(colour="gray50", alpha=0.7) +
  geom_density2d(colour="gray80") +
  geom_text(data=mca2_vars_df, 
            aes(x=Dim.1, y=Dim.2, label=rownames(mca2_vars_df), colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR") +
  scale_colour_discrete(name="Variable")



#MDS

d <- dist(data) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric  MDS",	 type="n")
text(x, y, labels = row.names(data), cex=.7)



# Nonmetric MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

library(MASS)
d <- dist(data) # euclidean distances between the rows
fit <- isoMDS(d, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(data), cex=.7)
