library(raster)
library(maps)

## datos ##
load("datos/cuadrantes.RData")
load("datos/ssta costa.RData")

head(xy.a)
head(datos.ssta)

map("world", xlim = c(-170, -70), ylim = c(-20, 70))
points(xy.a, col = 2)

## correlaciones

dat.cor <- data.frame(Area = paste0("A", 1:56), cor.MEI = NA, cor.PDO = NA) # pegar la letra A con el numero 1 al 56
dat.cor

cor(datos.ssta$V1, datos.ssta$MEI)

for(i in 1:56){
  dat.cor$cor.MEI[i] <- cor(datos.ssta[, i+5], datos.ssta$MEI)
  dat.cor$cor.PDO[i] <- cor(datos.ssta[, i+5], datos.ssta$PDO)
}
dat.cor

plot(1:56, dat.cor$cor.MEI, type = "l", ylim = c(0, 1), xlab = "Cuadrante", ylab = "correlación")
lines(1:56, dat.cor$cor.PDO, col = 2)
legend("topright", c("MEI", "PDO"), lty = 1, col = c(1, 2), bty = "n")

lmxx <- lm(V24 ~ MEI + PDO, data = datos.ssta)
summary(lmxx)

dat.lm <- data.frame(Area = paste0("A", 1:56), coef.MEI = NA, coef.PDO = NA, r2 = NA) # pegar la letra A con el numero 1 al 56
dat.lm


lm(datos.ssta$V24 ~ datos.ssta$MEI + datos.ssta$PDO)
lm(datos.ssta[, i+5] ~ datos.ssta$MEI + datos.ssta$PDO)
lm(datos.ssta[, i+5] ~ datos.ssta$MEI + datos.ssta$PDO)[2]
lm(datos.ssta[, i+5] ~ datos.ssta$MEI + datos.ssta$PDO)[3]
summary(lm(datos.ssta[, i+5] ~ datos.ssta$MEI + datos.ssta$PDO)$r.squared


                
for(i in 1:56){
  dat.lm$coef.MEI[i] <- coef(lm(datos.ssta[, i+5] ~ datos.ssta$MEI + datos.ssta$PDO))[2]
  dat.lm$coef.PDO[i] <- coef(lm(datos.ssta[, i+5] ~ datos.ssta$MEI + datos.ssta$PDO))[3]
  dat.lm$r2[i] <- summary(lm(datos.ssta[, i+5] ~ datos.ssta$MEI + datos.ssta$PDO))$r.squared
}
dat.lm

plot(1:56, dat.lm$coef.MEI, type = "l", ylim = c(0, 1), xlab = "Cuadrante", ylab = "coeficientes parciales")
lines(1:56, dat.lm$coef.PDO, col = 2)
legend("topright", c("MEI", "PDO"), lty = 1, col = c(1, 2), bty = "n")

plot(1:56, dat.lm$r2, type = "l", ylim = c(0, 1), xlab = "Cuadrante", ylab = expression (R^2))


filled.contour(as.matrix(datos.ssta[, 6:61]), zlim = c(-5, 5))

col.ssta <- colorRampPalette(c("darkblue", "blue", "cyan", "white", "yellow", "red", "darkred"))
filled.contour(x = datos.ssta$Time, y = 1:56, as.matrix(datos.ssta[, 6:61]), zlim = c(-5, 5), color.palette = col.ssta) # representacion espacial de las anomalias en cuadrantes costeros (tipo superficie de respuesta)
