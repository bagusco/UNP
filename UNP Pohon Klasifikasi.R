#membaca data
miopi <- read.csv("D:/myopia.csv")

#membuang kolom yang tidak diperlukan
miopi <- miopi[,-c(1, 2, 3)]

#membuat model pohon klasifikasi
library(rpart)
rpart(myopic ~ ., data=miopi, method="class", 
      control=rpart.control(cp=0, minsplit=25))

#menampilkan grafik pohon klasifikasi
pohon <- rpart(myopic ~ ., data=miopi, method="class", 
               control=rpart.control(cp=0, minsplit=25))

library(rpart.plot)
rpart.plot(pohon, extra=4)

#mengubah opsi minsplit menjadi 20
pohon <- rpart(myopic ~ ., data=miopi, method="class", 
               control=rpart.control(cp=0, minsplit=20))

library(rpart.plot)
rpart.plot(pohon, extra=4)


#membandingkan tingkat akurasi dari pohon dengan minsplit 25 dan 20
#yang dilakukan adalah
#(1) bagi data menjadi dua bagian: training dan testing
#(2) buat model pohon klasifikasi menggunakan data training
#(3) hitung akurasi prediksi dari data testing
library(caret)
set.seed(100)
index <- createDataPartition(miopi$myopic, p = 0.7, list=FALSE)
data.tr <- miopi[index,]
data.ts <- miopi[-index,]

pohon <- rpart(myopic ~ ., data=data.tr, method="class", 
               control=rpart.control(cp=0, minsplit=25))
prediksi <- predict(pohon, data.ts, type= "class")
mean(data.ts$myopic == prediksi)

pohon <- rpart(myopic ~ ., data=data.tr, method="class", 
               control=rpart.control(cp=0, minsplit=20))
prediksi <- predict(pohon, data.ts, type= "class")
mean(data.ts$myopic == prediksi)


#menentukan nilai minsplit yang optimal
akurasi <- NULL
for(mm in 1:10){
pohon <- rpart(myopic ~ ., data=data.tr, method="class", 
               control=rpart.control(cp=0, minsplit= (mm*3)))
prediksi <- predict(pohon, data.ts, type= "class")
akurasi[mm] <- mean(data.ts$myopic == prediksi)
}
akurasi
plot(1:10*3, akurasi, type="h", xlab="minsplit", lty=2)
points(1:10*3, akurasi, type="b", xlab="minsplit", pch=19)
