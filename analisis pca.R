getwd("~/CIBNOR/III TRIMESTRE/CAMBIO CLIMÁTICO")
load("datos/ssta costa.RData")

head(datos.ssta)
# El PCA nos va a ayudar a determinar cuales variables (cuadrantes en este caso) comparten una señal comun que seria igual al PC1
pca.datos <- datos.ssta[ , 6:61]
pca.ssta <- prcomp(pca.datos)
plot(pca.ssta)
summary(pca.ssta)
biplot(pca.ssta)


par(mfrow = c(3, 1 ), mar = rep(0, 4), oma = c(5, 5, 2, 3))
plot(pca.ssta$rotation[ , 1], type = "o", xaxt = "n") # graficar el componente 1
legend("topright", "PC1", bty = "n")
plot(pca.ssta$rotation[ , 2], type = "o", xaxt = "n", yaxt = "n") 
axis(4)
legend("topright", "PC2", bty = "n")
abline(h = 0, lty = 2)
plot(pca.ssta$rotation[ , 3], type = "o") 
legend("topright", "PC3", bty = "n")
abline(h = 0, lty = 2)
mtext("Cuadrante", side = 1, line = 3, outer = T)
mtext("Eigen vector", side = 2, line = 3, outer = T)

datos.ssta[c(571, 572, 573), ] # de los 56 cuadrantes te dice cuales fueron los más caliente o más fríos pero en toda la costa
datos.ssta[c(285, 306, 311), ]
datos.ssta[c(57, 274), c(6, 49)]


par(mfrow = c(3, 1 ), mar = rep(0, 4), oma = c(5, 5, 2, 3))
plot(datos.ssta$Time, pca.ssta$x[ , 1], type = "l", xaxt = "n") # graficar el componente 1
legend("topright", "PC1", bty = "n")
abline(h = 0, lty = 2)
plot(datos.ssta$Time, pca.ssta$x[ , 2], type = "l", xaxt = "n", yaxt = "n") 
axis(4)
legend("topright", "PC2", bty = "n")
abline(h = 0, lty = 2)
plot(datos.ssta$Time, pca.ssta$x[ , 3], type = "l") 
legend("topright", "PC3", bty = "n")
abline(h = 0, lty = 2)
mtext("Tiempo", side = 1, line = 3, outer = T)
mtext("Eigen valor", side = 2, line = 3, outer = T)


cor(datos.ssta$MEI, pca.ssta$x[, 1])
cor(datos.ssta$PDO, pca.ssta$x[, 1])

cor(datos.ssta$MEI, pca.ssta$x[, 2])
cor(datos.ssta$PDO, pca.ssta$x[, 2])

cor(datos.ssta$MEI, pca.ssta$x[, 3])
cor(datos.ssta$PDO, pca.ssta$x[, 3])

cor(datos.ssta$MEI, datos.ssta$PDO)
