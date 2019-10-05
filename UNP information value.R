miopi <- read.csv("D:/myopia.csv")

#install.packages("Information")
library(Information)

#kelas target diubah jadi binary
miopi$miopi <- ifelse(miopi$myopic == "Yes", 1, 0)

#membuang kolom yang yang tidak digunakan
miopi <- miopi[,-c(1, 2, 3, 4)]

#menghitung WoE dan IV
IV <- create_infotables(data=miopi, y="miopi", bins=8, parallel=FALSE)

#menampilkan IV
IV_Value = data.frame(IV$Summary)
IV_Value

#menampilkan WoE variabel "MOMMY"
print(IV$Tables$mommy, row.names=FALSE)





#install.packages("klaR")
library(klaR)

library(caret)
set.seed(1000)
index <- createDataPartition(miopi$miopi, p = 0.7, list=FALSE)
data.tr <- miopi[index,]
data.ts <- miopi[-index,]

glmodel.a     <- glm(miopi ~., data.tr, family=binomial)
pred.val <- predict(glmodel.a, data.ts, type = "response")
kelas <- ifelse(pred.val > 0.5, 1, 0)
table(data.ts$miopi, kelas)


library(classInt)
data.tr1 <- data.tr
data.tr1$age <- as.factor(data.tr1$age)

eqwid <- classIntervals(data.tr$spheq, 5, style = 'equal')
data.tr1$spheq <- cut(data.tr1$spheq, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$al, 5, style = 'equal')
data.tr1$al <- cut(data.tr1$al, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$acd, 5, style = 'equal')
data.tr1$acd <- cut(data.tr1$acd, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$lt, 5, style = 'equal')
data.tr1$lt <- cut(data.tr1$lt, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$vcd, 5, style = 'equal')
data.tr1$vcd <- cut(data.tr1$vcd, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$sporthr, 5, style = 'equal')
data.tr1$sporthr <- cut(data.tr1$sporthr, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$tvhr, 5, style = 'equal')
data.tr1$tvhr <- cut(data.tr1$tvhr, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$readhr, 5, style = 'equal')
data.tr1$readhr <- cut(data.tr1$readhr, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$comphr, 5, style = 'equal')
data.tr1$comphr <- cut(data.tr1$comphr, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$studyhr, 5, style = 'equal')
data.tr1$studyhr <- cut(data.tr1$studyhr, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$diopterhr, 5, style = 'equal')
data.tr1$diopterhr <- cut(data.tr1$diopterhr, breaks=eqwid$brks, include.lowest=TRUE)

woemodel <- woe(x=data.frame(data.tr1[,-16]), grouping=as.factor(data.tr1$miopi),zeroadj=0.5, applyontrain = TRUE)
woemodel

traindata <- predict(woemodel, data.tr1, replace = TRUE)
#head(traindata)

glmodel.b     <- glm(miopi ~., traindata, family=binomial)



data.ts$age <- as.factor(data.ts$age)

eqwid <- classIntervals(data.tr$spheq, 5, style = 'equal')
data.ts$spheq <- cut(data.ts$spheq, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$al, 5, style = 'equal')
data.ts$al <- cut(data.ts$al, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$acd, 5, style = 'equal')
data.ts$acd <- cut(data.ts$acd, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$lt, 5, style = 'equal')
data.ts$lt <- cut(data.ts$lt, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$vcd, 5, style = 'equal')
data.ts$vcd <- cut(data.ts$vcd, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$sporthr, 5, style = 'equal')
data.ts$sporthr <- cut(data.ts$sporthr, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$tvhr, 5, style = 'equal')
data.ts$tvhr <- cut(data.ts$tvhr, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$readhr, 5, style = 'equal')
data.ts$readhr <- cut(data.ts$readhr, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$comphr, 5, style = 'equal')
data.ts$comphr <- cut(data.ts$comphr, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$studyhr, 5, style = 'equal')
data.ts$studyhr <- cut(data.ts$studyhr, breaks=eqwid$brks, include.lowest=TRUE)

eqwid <- classIntervals(data.tr$diopterhr, 5, style = 'equal')
data.ts$diopterhr <- cut(data.ts$diopterhr, breaks=eqwid$brks, include.lowest=TRUE)


validata <- predict(woemodel, data.ts, replace = TRUE)

pred.val <- predict(glmodel.b, validata, type = "response")
kelas <- ifelse(pred.val > 0.5, 1, 0)
table(validata$miopi, kelas)

