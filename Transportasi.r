#Rafli Abdul Malik(17523150)
#Dimas Setyawan Ramadhansyah(17523152)

#Pengaruh biaya Transportasi terhadap Harga Penjualan Laptop di Pulau Lombok setiap toko
#(Data ditulis dalam jutaaan)

harga<-read.csv(file.choose(),header=TRUE)
#pilih file BiayaTransportasi.csv

harga

model <-lm(BiayaTransportasi ~ HargaJual , data=harga)
summary(model)

plot(BiayaTransportasi ~ HargaJual, data=harga)
abline(model, col = "green", lwd=3)

predict(model, data.frame(HargaJual = 80))

harga

poly_model <- lm(BiayaTransportasi ~ poly(HargaJual,degree=2), data = harga)
poly_model

x <- with(harga, seq(min(HargaJual), max(HargaJual), length.out=2000))
y <- predict(poly_model, newdata = data.frame(HargaJual = x))

plot(BiayaTransportasi ~ HargaJual, data = harga)
lines(x, y, col = "green")

#Sumber Data : https://suaraterpuji.wordpress.com/statistik-2/regresi-linear-sederhana/