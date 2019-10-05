#membaca data
data <- read.csv("D:/disk01.csv")

#mempartisi data
#70% training, 30% testing
library(caret)
set.seed(1000)
index <- createDataPartition(data$class, p = 0.7, list=FALSE)
data.tr <- data[index,]
data.ts <- data[-index,]

#membuat model regresi logistik dengan X asli
model.asli <- glm(class ~ x, data=data.tr, family="binomial")

#memprediksi data testing dan menghitung akurasinya
prediksi.prob.asli <- predict(model.asli, newdata=data.ts, type="response")
prediksi.asli <- ifelse(prediksi.prob.asli > 0.5, 1, 0)
table(data.ts$class, prediksi.asli)
mean(data.ts$class == prediksi.asli)

#melakukan diskretisasi data variabel x
#menggunakan metode EQUAL WIDTH
library(classInt)
eqwid <- classIntervals(data.tr$x, 10, style = 'equal')
data.tr$x.disk <- cut(data.tr$x, breaks=eqwid$brks, include.lowest=TRUE)

#membuat model regresi logistik dengan x hasil diskretisasi
model.disk <- glm(class ~ x.disk, data=data.tr, family="binomial")

#memprediksi data testing dan menghitung akurasinya
data.ts$x.disk <- cut(data.ts$x, breaks=eqwid$brks, include.lowest=TRUE)
prediksi.prob.disk <- predict(model.disk, newdata=data.ts, type="response")
prediksi.disk <- ifelse(prediksi.prob.disk> 0.5, 1, 0)
table(data.ts$class, prediksi.disk)
mean(data.ts$class == prediksi.disk)



proporsi <- prop.table(table(data.tr$x.disk, data.tr$class), margin=1)
barplot(t(proporsi))

#melakukan diskretisasi data variabel x
#menggunakan metode MDLP
set.seed(1000)
index <- createDataPartition(data$class, p = 0.7, list=FALSE)
data.tr <- data[index,]
data.ts <- data[-index,]

library(discretization)
mdlp <- mdlp(data.tr)
breaks <- c(min(data.tr$x), mdlp$cutp[[1]], max(data.tr$x))
data.tr$x.mdlp <- cut(data.tr$x, breaks=breaks, include.lowest=TRUE)


#membuat model regresi logistik dengan x hasil diskretisasi
model.mdlp <- glm(class ~ x.mdlp, data=data.tr, family="binomial")

#memprediksi data testing dan menghitung akurasinya
breaks <- c(min(data.ts$x), mdlp$cutp[[1]], max(data.ts$x))
data.ts$x.mdlp <- cut(data.ts$x, breaks=breaks, include.lowest=TRUE)
prediksi.prob.mdlp <- predict(model.mdlp, newdata=data.ts, type="response")
prediksi.mdlp <- ifelse(prediksi.prob.mdlp > 0.5, 1, 0)
table(data.ts$class, prediksi.mdlp)
mean(data.ts$class == prediksi.mdlp)



#melakukan diskretisasi data variabel x
#menggunakan metode ChiMerge
set.seed(1000)
index <- createDataPartition(data$class, p = 0.7, list=FALSE)
data.tr <- data[index,]
data.ts <- data[-index,]

library(discretization)
chimerge <- chiM(data.tr, 0.05)
breaks <- c(min(data.tr$x), chimerge$cutp[[1]], max(data.tr$x))
data.tr$x.chim <- cut(data.tr$x, breaks=breaks, include.lowest=TRUE)


#membuat model regresi logistik dengan x hasil diskretisasi
model.chim <- glm(class ~ x.chim, data=data.tr, family="binomial")

#memprediksi data testing dan menghitung akurasinya
breaks <- c(min(data.ts$x), chimerge$cutp[[1]], max(data.ts$x))
data.ts$x.chim <- cut(data.ts$x, breaks=breaks, include.lowest=TRUE)
prediksi.prob.chim <- predict(model.chim, newdata=data.ts, type="response")
prediksi.chim <- ifelse(prediksi.prob.chim > 0.5, 1, 0)
table(data.ts$class, prediksi.chim)
mean(data.ts$class == prediksi.chim)
