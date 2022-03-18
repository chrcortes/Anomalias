library(psych)

setwd("~/CIBNOR/III TRIMESTRE/CAMBIO CLIMÁTICO/datos")

load("sst costa.RData")
load("ssta costa.RData")
# espectral: frecuencia de repetición de un fenomeno, mínimo mensual es cada 12 meses: frecuencia es un valor con respecto al tiempo.
dat.pca <- datos.ssta[, 6:61]
dat.clu <- t(datos.ssta[, 6:61]) # t: transponer, es una rotación de 90 grados, RENGLONES QUE SEAN LOS INDIVIDUOS, EN PCA LAS COLUMNAS SON LAS VARIABLES Y EN RENGLONES SON LAS OBSERVACIONES

pca.ssta <- principal(dat.pca, nfactor = 3, rotate = "varimax") # VARIMAX: de acuerdo a nuestros intervalos de confianza, ahora en esos 3 vamos a agrupar toda la variación
# descompone, transforma y calcula distancias euclidianas a partir de similitudes
clu.ssta <- hclust(dist(dat.clu), "ward.D2") #calcular las distancias euclidianas

old.par <- par(mar = c(4.6, 4.6, 3.1, 1), oma = rep(1, 4), mfrow = c(1, 1)) # son los parámetros gráficos

par(mar = c(2.1, 4.6, 3.1, 1))
plot(clu.ssta, hang = -1, sub = "", xlab = "") # hang 1 los alinea y no conforme a sus valores
rect.hclust(clu.ssta, 3, border = 1)

par(mar = c(4.6, 4.6, 1, 1))
image(1:56, 1:56, cor(dat.pca), col = rev(gray.colors(50)), xlab = "Areas", ylab = "Areas", zlim = c(0, 1), asp = 1)
contour(1:56, 1:56, cor(dat.pca), add = T)
# matriz espacial de autocorrelación para ver qué cuadrantes se parecen más
clu.ser <- data.frame(Time = datos.ssta$Time, CL1 = rowMeans(dat.pca[, 1:21]), CL2 = rowMeans(dat.pca[, 35:56]), CL3 = rowMeans(dat.pca[, 22:34])) # cada uno de los 22 meses 2 y le sacas un promedio a esas 22 series (cuadrantes)

par(mar = rep(0, 4), oma = c(4, 5, 2, 5), mfrow = c(3, 1))
plot(clu.ser$Time, clu.ser$CL3, type = "l", xaxt = "n", ylim = c(-2, 2)); abline(h = 0, lty = 2)
mtext("CL3", side = 2, line = 3)
plot(clu.ser$Time, clu.ser$CL2, type = "l", xaxt = "n", yaxt = "n", ylim = c(-2, 2)); abline(h = 0, lty = 2)
mtext("CL2", side = 4, line = 3); axis(4)
plot(clu.ser$Time, clu.ser$CL1, type = "l", ylim = c(-2, 2)); abline(h = 0, lty = 2)
mtext("CL1", side = 2, line = 3) # CL3:medias, CL2:sur y CL1:norte

par(old.par) # parametros graficos casi-originales

par(mar = c(4.6, 4.6, 1, 1))
plot(1:56, pca.ssta$loadings[, 1], type = "o", pch = 16, ylim = c(-.25, 1), xlab = "Areas", ylab = "Eigen vector")
lines(1:56, pca.ssta$loadings[, 2], type = "o", pch = 15)
lines(1:56, pca.ssta$loadings[, 3], type = "o", pch = 17)
legend("bottom", c("PC1", "PC2", "PC3"), horiz = T, bty = "n", pch = c(16, 15, 17), lty = 1)

par(mar = rep(0, 4), oma = c(4, 5, 2, 5), mfrow = c(3, 1))
plot(datos.ssta$Time, pca.ssta$scores[, 3], type = "l", xaxt = "n", ylim = c(-4, 4)); abline(h = 0, lty = 2)
mtext("PC3", side = 2, line = 3)
plot(datos.ssta$Time, pca.ssta$scores[, 2], type = "l", xaxt = "n", yaxt = "n", ylim = c(-4, 4)); abline(h = 0, lty = 2)
mtext("PC2", side = 4, line = 3); axis(4)
plot(datos.ssta$Time, pca.ssta$scores[, 1], type = "l", ylim = c(-4, 4)); abline(h = 0, lty = 2)
mtext("PC1", side = 2, line = 3) # el PC1 es el del norte, el PC2 es el sur y el PC3 es la latitud media
# eigenVECTORES: zonas geográficas y eigenvALORES: tiempo
par(old.par)
# ts: timeseries, crea un vector de tiempo. en el primero escogi temperatura y el cuadrante 40, 12 = anual
ts.sst <- ts(datos.sst$V40, frequency = 12, start = c(1950, 1))
ts.mei <- ts(datos.sst$MEI, frequency = 12, start = c(1950, 1))
ts.pdo <- ts(datos.sst$PDO, frequency = 12, start = c(1950, 1))
ts.pc1 <- ts(pca.ssta$scores[, 1], frequency = 12, start = c(1950, 1))
ts.pc2 <- ts(pca.ssta$scores[, 2], frequency = 12, start = c(1950, 1))
ts.pc3 <- ts(pca.ssta$scores[, 3], frequency = 12, start = c(1950, 1))

par(mar = c(5.1, 4.1, 3.1, 1))
spectrum(ts.sst) # el primer periodo del periodograma es cada 1 = año
1/(1/12)
2/(1/12)

spectrum(ts.sst, spans = c(5, 5)) # es como un intervalo de confianza por lo que solo hay realmente 1
spectrum(ts.mei, spans = c(5, 5))
spectrum(ts.pdo, spans = c(5, 5))
spectrum(ts.pc1, spans = c(5, 5))
spectrum(ts.pc2, spans = c(5, 5))
spectrum(ts.pc3, spans = c(5, 5))

(12/1)/12
(12/2)/12
(12/6)/12

(12/0.1646)/12
(12/0.3108)/12

(12/1)/12 # temperatura
(12/0.22730)/12 # MEI
(12/0.015)/12 # PDO
(12/0.03936)/12 # PC1 - norte
(12/0.22730)/12 # PC1 - ecuador
(12/0.06024)/12 # PC1 - california


spe.mei <- spectrum(ts.mei, spans = c(5, 5))
spe.pdo <- spectrum(ts.pdo, spans = c(5, 5))
spe.pc1 <- spectrum(ts.pc1, spans = c(5, 5))
spe.pc2 <- spectrum(ts.pc2, spans = c(5, 5))
spe.pc3 <- spectrum(ts.pc3, spans = c(5, 5))

which(spe.mei$spec == max(spe.mei$spec))
spe.mei$freq[18]

spe.mei$freq[which(spe.mei$spec == max(spe.mei$spec))]

(12/spe.mei$freq[which(spe.mei$spec == max(spe.mei$spec))])/12
(12/spe.pdo$freq[which(spe.pdo$spec == max(spe.pdo$spec))])/12
(12/spe.pc1$freq[which(spe.pc1$spec == max(spe.pc1$spec))])/12
(12/spe.pc2$freq[which(spe.pc2$spec == max(spe.pc2$spec))])/12
(12/spe.pc3$freq[which(spe.pc3$spec == max(spe.pc3$spec))])/12

spe.mei2 <- spectrum(ts.mei) # sin suavizado
(12/spe.mei2$freq[which(spe.mei2$spec == max(spe.mei2$spec))])/12