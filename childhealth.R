library(cluster)
install(package.ggplot2)
#setwd("C:/myfolder/data")
setwd("C:/Users/nicken/Desktop/PROJREAL")
pca=read.csv("C:/Users/nicken/Desktop/PROJREAL/health.csv",HEAD=TRUE)
View(pca)
pca.features=pca
names(pca.features[,1:6])
View(pca.features[,1:6])
pca.features$X <- NULL
pca.features$County <- NULL
pca.features$A.1 <- NULL
pca.features$A.2 <- NULL
pca.features$A.3 <- NULL
pca.features$A.4<- NULL
pca.features$A.5 <- NULL
pca.features$A.6 <- NULL

#$$$$$$$$$$$$$$$$$$$$$$$$$4444

no.group<-3
no.iter<-10
c.kmeans<-kmeans(pca.features,no.group,no.iter)
plot(pca.features[,1:5],col=c.kmeans$cluster)
##PCA
princomp(pca.features,cor=TRUE,scores=TRUE)

cell.pca<-princomp(pca.features,cor=TRUE,scores=TRUE)
pca.dim1<-cell.pca$scores1[,1]
pca.dim2<-cell.pca$scores[,2]
plot(pca.dim1,pca.dim2,main("hh")xlab="x",ylab="y")
$^^^^^^^^^^^^^^^^^^^^^^^^^^^^
summary(pc.cr <- princomp(pca.features, cor = TRUE))
results <- kmeans(pca.features,3)
results <- kmeans(pca.features[,1:5],3)
results
plot(pca.features)
results$size
results$cluster
results$centers
results$totss
results$tot.withinss
results$betweens
results$size
results$iter
results$ifault
table(pca$bio,results$cluster)
plot(pca[c("Male","Female")],col=results$cluster)
plot(pca[c("bio","phys")],col=pca$total)
errortable<-matrix(c(pca[,4],results$cluster),nrow=10,ncol=2)
errortable
plot(pca[,2],col=results$cluster,main="Gender Analysis",xlab="obs #",ylab="Female")
title(main="Plots of Attributes \n by KMeans Clustering",outer=TRUE)
plot(pca[,3],col=results$cluster,main="Exam Analysis",xlab="obs #",ylab="phys")
plot(pca[,4],col=results$cluster,main="Exam Analysis",xlab="obs #",ylab="phys")

plot(pc.cr,type="l") 
loadings(pc.cr)
#biplot(birequire(graphics))
biplot(pc.cr)
sunflowerplot(pca.features)

plot(pca.features,main="",col.axis = "sky blue")
title("Child Health", sub = "plot",
      cex.main = 2,   font.main= 4, col.main= "blue",
      cex.sub = 2, font.sub = 3, col.sub = "red")


#%%%%%%%%%%%%%%%%%%%%55

x <- seq(-4, 4, len = 101)
y <- cbind(sin(x), cos(x))
matplot(x, y, type = "l", xaxt = "n",
        main = expression(paste(plain(sin) * phi, "  and  ",
                                plain(cos) * phi)),
        ylab = expression("sin" * phi, "cos" * phi), # only 1st is taken
        xlab = expression(paste("Phase Angle ", phi)),
        col.main = "blue")
axis(1, at = c(-pi, -pi/2, 0, pi/2, pi),
     labels = expression(-pi, -pi/2, 0, pi/2, pi))
abline(h = 0, v = pi/2 * c(-1,1), lty = 2, lwd = .1, col = "gray70")

#=====================

require(graphics)

# a 2-dimensional example
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 5),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colCounty(x) <- c("x", "y")
(results <- kmeans(x, 3))
plot(x, col = results$cluster)
points(results$centers, col = 2:6, pch = 8, cex = 2)

# sum of squares

#++++++++

plot(pca.features[pca.features.3means$cluster==1,],col="red") 
     #xlim=c(min(pca.features[,1]),max(pca.features[,1]),
        #    ylim=c(min(pca.features[,3]),max(pca.features[,3]), 
           # )
            
            points(pca.features[pca.features.3means$cluster==2,],col="blue")
            points(pca.features[pca.features.3means$cluster==3,],col="green")
     
     results.3means <- kmeans(results, centers=3)
     
     