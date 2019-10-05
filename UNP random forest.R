#membaca data
miopi <- read.csv("D:/myopia.csv")

#membuang kolom yang tidak diperlukan
miopi <- miopi[,-c(1, 2, 3)]

#membuat model random forest
library(randomForest)
forest <- randomForest(myopic ~ ., data=miopi, method="class")
forest

#(1) bagi data menjadi dua bagian: training dan testing
#(2) buat model random forest menggunakan data training
#(3) hitung akurasi prediksi dari data testing
library(caret)
set.seed(100)
index <- createDataPartition(miopi$myopic, p = 0.7, list=FALSE)
data.tr <- miopi[index,]
data.ts <- miopi[-index,]

forest <- randomForest(myopic ~ ., data=data.tr, method="class")
prediksi <- predict(forest, data.ts, type= "class")
mean(data.ts$myopic == prediksi)
table(data.ts$myopic, prediksi)


library(caretEnsemble)
boost.caret <- train(myopic ~ ., data=data.tr, method='gbm')
boost.caret.pred <- predict(boost.caret, data.ts)
mean(data.ts$myopic == boost.caret.pred)
table(data.ts$myopic, boost.caret.pred)

library(caretEnsemble)
rf <- train(myopic ~ ., data=data.tr, method='rf')
rf.pred <- predict(rf, data.ts)
mean(data.ts$myopic == rf.pred)
table(data.ts$myopic, rf.pred)

