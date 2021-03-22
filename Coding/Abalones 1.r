<!-- 1. Preliminaries  -->
> View(abalones)
> mydata <- abalones
> str(mydata) 
> library(ggplot2)
> library(moments)
> library(gridExtra)
> mydata$VOLUME <- mydata$LENGTH * mydata$DIAM * mydata$HEIGHT
> mydata$RATIO <- mydata$SHUCK / mydata$VOLUME

<!-- Programming code 1(a)-->
> sumwhole <- summary(mydata)
> t2 <- data.frame(mydata$LENGTH, mydata$DIAM, mydata$HEIGHT, mydata$WHOLE, mydata$SHUCK, mydata$RINGS)
> skew(t2)
> kurtosi(t2)
> par(mfrow = c(1, 2))
> hist(mydata$LENGTH, main = "Histogram Length", cex.main=1.5, col="dodgerblue3", xlab = "Abalone Leng", ylab = "Frequency", cex.lab=0.8, cex.axis=0.8)
> boxplot(mydata$LENGTH, main = "Boxplot Length", col="firebrick3", pch=16)
> 
> par(mfrow = c(1,1))
> par(mfrow = c(2,2))
> hist(mydata$DIAM, main = "Histogram Diam", cex.main=1.5, col="dodgerblue3", xlab = "Abalone Diam", ylab = "Frequency", cex.lab=0.8, cex.axis=0.8)
> hist(mydata$HEIGHT, main = "Histogram Height", cex.main=1.5, col="dodgerblue3", xlab = "Abalone Height", ylab = "Frequency", cex.lab=0.8, cex.axis=0.8)
> boxplot(mydata$DIAM, main = "Boxplot Diam", col="firebrick3", pch=16)
> boxplot(mydata$HEIGHT, main = "Boxplot Height", col="firebrick3", pch=16)
>
> par(mfrow = c(1,1))
> par(mfrow = c(2,3))
> hist(mydata$WHOLE, main = "Histogram Whole", cex.main=1.5, col="dodgerblue3", xlab = "Abalone Whole", ylab = "Frequency", cex.lab=0.8, cex.axis=0.8)
> hist(mydata$SHUCK, main = "Histogram Shuck", cex.main=1.5, col="dodgerblue3", xlab = "Abalone Shuck", ylab = "Frequency", cex.lab=0.8, cex.axis=0.8)
> hist(mydata$RINGS, main = "Histogram Rings", cex.main=1.5, col="dodgerblue3", xlab = "Abalone Rings", ylab = "Frequency", cex.lab=0.8, cex.axis=0.8)
> boxplot(mydata$WHOLE, main = "Boxplot Whole", col="firebrick3", pch=16)
> boxplot(mydata$SHUCK, main = "Boxplot Shuck", col="firebrick3", pch=16)
> boxplot(mydata$RINGS, main = "Boxplot Rings", col="firebrick3", pch=16)
>
> skew(mydata$LENGTH)
> skew(mydata$DIAM)
> skew(mydata$HEIGHT)
> skew(mydata$WHOLE)
> skew(mydata$SHUCK)
> skew(mydata$RINGS)
> kurtosi(mydata$LENGTH)
> kurtosi(mydata$DIAM)
> kurtosi(mydata$HEIGHT)
> kurtosi(mydata$WHOLE)
> kurtosi(mydata$SHUCK)
> kurtosi(mydata$RINGS)

<!-- Programming code 1(b)-->
> sexvsclass <- table(mydata$SEX, mydata$CLASS)
> rownames(sexvsclass) <- c("Female", "Infant", "Male")
> addmargins((sexvsclass))
> prob <- addmargins(prop.table(sexvsclass))
> round(prob, digits = 2)
> barplot(table(mydata$SEX, mydata$CLASS)[c(2, 1, 3),], beside = TRUE, main = "Comparison Abalone Sex Frequencies", xlab = "Age Class", ylab = "Frequency", cex.lab = .8, cex.axis = .7, col = c("dodgerblue4", "firebrick2", "lightgoldenrod2"), cex.names = .8, legend.text = c("Infant", "Female", "Male"),)

<!-- Programming code 1(c)-->
> set.seed(123)
> work <- mydata[sample(1:nrow(mydata), 200, replace = FALSE),]
> plot(work[,2:6], panel=panel.smooth, pch=18, col="dodgerblue3", lwd=1.5)

<!-- Programming code 2(a)-->

> df1a <- data.frame(mydata$VOLUME, mydata$WHOLE)
> plot(df1a, main = "Comparing Whole Abalone to Volume", cex.main=1.5, xlab = "Abalone Volume", ylab = "Abalone whole weight", cex.lab=0.8, cex.axis=0.8, pch= 16, col="dodgerblue3", panel.first = grid(15), frame.plot = FALSE)

<!-- Programming code 2(b)-->

> df <- data.frame(mydata$SHUCK, mydata$WHOLE)
> m <- mydata$SHUCK/mydata$WHOLE
>  total <- cbind(df,m)	
> colnames(total) <- c("Shuck", "Volume", "Ratio")
> max2b <- max(m)
> plot(mydata$WHOLE, mydata$SHUCK, main = "Shuck in function of Abalone whole wieght", cex=.8, cex.main = 1.5, xlab = "Whole weight", ylab = "Shuck", cex.lab = 0.8, cex.axis = 0.8, pch = 16, col = "dodgerblue3", panel.first = grid(15), frame.plot = FALSE)
> abline(a=0, b=max2b, col="firebrick3", lwd=2, lty=1)

<!-- Programming code 3(a)-->
> tableq3 <- data.frame(mydata$SEX, mydata$RATIO)
> tf <- subset(tableq3, mydata$SEX =="F")
> ti <- subset(tableq3, mydata$SEX =="I")
> tm <- subset(tableq3, mydata$SEX =="M")
> par(mfrow = c(3, 3))
> hist(tf$mydata.RATIO, main = "Histogram Female Ratio", panel.first=grid(15), xlab = "Shuck vs Volume Ratio", ylab = "Frequency", cex.main=1.5, cex.lab=0.8, cex.axis=0.8, col="dodgerblue3", plot=TRUE, xlim = c(0.05,0.34))
> hist(ti$mydata.RATIO, main = "Histogram Infant Ratio", panel.first=grid(15), xlab = "Shuck vs Volume Ratio", ylab = "Frequency", cex.main=1.5, cex.lab=0.8, cex.axis=0.8, col="dodgerblue3", plot=TRUE, xlim = c(0.05,0.30), ylim = c(0,100))
> hist(tm$mydata.RATIO, main = "Histogram Male Ratio", panel.first=grid(15), xlab = "Shuck vs Volume Ratio", ylab = "Frequency", cex.main=1.5, cex.lab=0.8, cex.axis=0.8, col="dodgerblue3", plot=TRUE, xlim = c(0.05,0.30), ylim = c(0,130))
> boxplot(tf$mydata.RATIO, horizontal=TRUE, main="Boxplot Female Ratio", cex.main=1.5, xlab="Ratio Shuck vs Volume", cex.lab=0.8, cex.axis=0.8, pch=16, lwd=1.7, col="dodgerblue3", notch = TRUE, staplewex = 0.5)
> boxplot(ti$mydata.RATIO, horizontal=TRUE, main="Boxplot Infant Ratio", cex.main=1.5, xlab="Ratio Shuck vs Volume", cex.lab=0.8, cex.axis=0.8, pch=16, lwd=1.7, col="dodgerblue3", notch = TRUE, staplewex = 0.5)
> boxplot(tm$mydata.RATIO, horizontal=TRUE, main="Boxplot Male Ratio", cex.main=1.5, xlab="Ratio Shuck vs Volume", cex.lab=0.8, cex.axis=0.8, pch=16, lwd=1.7, col="dodgerblue3", notch = TRUE, staplewex = 0.5)
> qqnorm(tf$mydata.RATIO, pch=16, col="dodgerblue3", panel.first=grid(20), main = "QQ-Plot Graph Female Ratio", cex.axis=0.7, cex.lab=0.8)
> qqline(tf$mydata.RATIO, col="firebrick3", lwd=2)
> qqnorm(ti$mydata.RATIO, pch=16, col="dodgerblue3", panel.first=grid(20), main = "QQ-Plot Graph Infant Ratio", cex.axis=0.7, cex.lab=0.8)
> qqline(ti$mydata.RATIO, col="firebrick3", lwd=2)
> qqnorm(tm$mydata.RATIO, pch=16, col="dodgerblue3", panel.first=grid(20), main = "QQ-Plot Graph Male Ratio", cex.axis=0.7, cex.lab=0.8)
> qqline(tm$mydata.RATIO, col="firebrick3", lwd=2)
> par(mfrow = c(1, 1))

<!-- Programming code 3(b)-->
> outfemale <- boxplot.stats(tf$mydata.RATIO, coef = 1.5, do.conf = TRUE, do.out=TRUE)
> outinfant <-  boxplot.stats(ti$mydata.RATIO, coef = 1.5, do.conf = TRUE, do.out=TRUE)
> outmale <-  boxplot.stats(tm$mydata.RATIO, coef = 1.5, do.conf = TRUE, do.out=TRUE)

<!-- Programming code 4(a)-->
> grid.arrange(
+ ggplot(
+ mydata, aes(x=factor(mydata$CLASS), y= mydata$VOLUME, fill=mydata$CLASS))+
+ geom_boxplot(outlier.color = "firebrick3", outlier.shape = 16, outlier.size = 1.2)+
+ labs(title="Abalone volume by class", x="Class", y="Volume")+
+ theme(legend.position = "none"),
+ ggplot(
+ mydata, aes(x=factor(mydata$CLASS), y= mydata$WHOLE, fill=mydata$CLASS))+
+ geom_boxplot(outlier.color = "firebrick3", outlier.shape = 16, outlier.size = 1.2)+
+ labs(title="Whole by class", x="Class", y="Whole")+
+ theme(legend.position = "none"),
+ nrow=1, top= "Comparing Abalones classes with Volume & Whole"
+ )

> grid.arrange(
+ ggplot(
+ mydata, aes(x=factor(mydata$RINGS), y=mydata$VOLUME, color=mydata$RINGS))+
+ geom_point()+
+ theme(legend.position = "none")+
+ labs(title="Volume by rings", x="Rings", y="Volume"),
+ ggplot(
+ mydata, aes(x=factor(mydata$RINGS), y=mydata$WHOLE, color=mydata$RINGS))+
+ geom_point()+
+ theme(legend.position = "none")+
+ labs(title="Whole by rings", x="Rings", y="Whole"),
+ nrow=1, top= "Comparing Abalones rings with Volume & Whole"
+ )

<!-- Programming code 5(a)-->
> aggdatavolume <- aggregate(mydata$VOLUME, by=list(mydata$SEX, mydata$CLASS), FUN=mean, na.rm=TRUE)
> a <- tapply(aggdatavolume$x, list(aggdatavolume$Group.1, aggdatavolume$Group.2), mean)
> rownames(a) <- c("Female", "Infant", "Male")
> colnames(a) <- c("A1", "A2", "A3", "A4", "A5", "A6")
> round(a, digits = 2)
> aggdataratio <- aggregate(mydata$RATIO, by=list(mydata$SEX, mydata$CLASS), FUN=mean, na.rm=TRUE)
> b <- tapply(aggdataratio$x, list(aggdataratio$Group.1, aggdataratio$Group.2), mean)
> rownames(b) <- c("Female", "Infant", "Male")
> colnames(b) <- c("A1", "A2", "A3", "A4", "A5", "A6")
> round(b, digits = 4)

<!-- Programming code 5(b)-->
> outv <- aggregate(VOLUME ~ SEX + CLASS, data = mydata, mean)
> outr <- aggregate(RATIO ~ SEX + CLASS, data = mydata, mean)

> grid.arrange( 
+ ggplot(data = outv, aes(x = CLASS, y = VOLUME, group = SEX, colour = SEX))+
+ geom_line() + geom_point(size = 2)+
+ ggtitle("Plot of Mean VOLUME versus CLASS for Three Sexes"),
+ ggplot(data = outr, aes(x = CLASS, y = RATIO, group = SEX, colour = SEX))+
+ geom_line() + geom_point(size = 2)+
+ ggtitle("Plot of Mean RATIO versus CLASS for Three Sexes")
+ )