library(raster)

setwd("~/CIBNOR/III TRIMESTRE/CAMBIO CLIMÁTICO")

load("datos/ssta 1950 2017.RData")

ssta
names(ssta)
nlayers(ssta)

col.ssta <- colorRampPalette(c("darkblue", "blue", "white", "red", "darkred"))

plot(ssta[[807]], zlim = c(-5.5, 5.5), col = col.ssta(150)) 
plot(ssta[[793]], zlim = c(-5.5, 5.5), col = col.ssta(150))
plot(ssta[["X2016.01.01"]], zlim = c(-5.5, 5.5), col = col.ssta(150))

locator(1)
# el ecuador por redondeo va a ser 276, 0

locator(1)
# califnornia por redondeo va a ser 236, 38

plot(ssta[[807]], zlim = c(-5.5, 5.5), col = col.ssta(150)) 
points(x = c(276, 236), y = c(0, 38))

fechas <- seq(as.Date("1950-01-01"), as.Date("2017-03-01"), by = "month")

datos.ssta <- data.frame(Date = fechas, Year = as.numeric(substr(fechas, 1, 4)), Month = as.numeric(substr(fechas, 6, 7)))
datos.ssta$Time <- datos.ssta$Year + ((datos.ssta$Month - 1)/12)
datos.ssta

datos.ssta$SSTA_EC <- as.vector(extract(ssta, data.frame(x = 276, y = 0)))
datos.ssta$SSTA_CA <- as.vector(extract(ssta, data.frame(x = 236, y = 38)))

datos.ssta

par(mar = c(4.6, 4.6, 3.1, 1)) #inf, izquierdo, arriba, derecha, como manecilla del reloj
plot(datos.ssta$Time, datos.ssta$SSTA_EC, type = "l", main = "Anomalías de temperatura (Ecuador)", xlab = "Tiempo", ylab = "SSTA (°C)") # vector de tiempo
plot(datos.ssta$Date, datos.ssta$SSTA_EC, type = "l", main = "Anomalías de temperatura (Ecuador)", xlab = "Tiempo", ylab = "SSTA (°C)") # vector de fechas

plot(datos.ssta$Time, datos.ssta$SSTA_CA, type = "l", main = "Anomalías de temperatura (California)", xlab = "Tiempo", ylab = "SSTA (°C)") # vector de fechas

# figura con dos lineas de tiempo
plot(datos.ssta$Time, datos.ssta$SSTA_EC, type = "l", main = "Anomalías de temperatura", xlab = "Tiempo", ylab = "SSTA (°C)") 
lines(datos.ssta$Time, datos.ssta$SSTA_CA, col = 2)
legend("topleft", c("Ecuador", "California"), lty = 1, col = c(1, 2))

library(caTools)

plot(datos.ssta$Time, datos.ssta$SSTA_EC, type = "l", main = "Anomalías de temperatura", xlab = "Tiempo", ylab = "SSTA (°C)") 
lines(datos.ssta$Time, runmean(datos.ssta$SSTA_EC, k = 60), col = 2)
lines(datos.ssta$Time, runmean(datos.ssta$SSTA_EC, k = 36), col = 3)
lines(datos.ssta$Time, runmean(datos.ssta$SSTA_EC, k = 12), col = 4)
legend("topleft", c("Obs.", "5 años", "3 años", "1 año"), lty = 1, col = c(1: 4), bty = "n")

plot(datos.ssta$Time, runmean(datos.ssta$SSTA_EC, k = 12), type = "l", main = "Anomalías de temperatura", xlab = "Tiempo", ylab = "SSTA (°C)", ylim = c(-3.5, 3.5)) 
lines(datos.ssta$Time, runmean(datos.ssta$SSTA_CA, k = 12), col = 2)
legend("topleft", c("Ecuador", "California"), lty = 1, col = c(1, 2), bty = "n")

plot(datos.ssta$Time, runmean(datos.ssta$SSTA_EC, k = 36), type = "l", main = "Anomalías de temperatura", xlab = "Tiempo", ylab = "SSTA (°C)", ylim = c(-1.5, 1.5)) 
lines(datos.ssta$Time, runmean(datos.ssta$SSTA_CA, k = 36), col = 2)
legend("topleft", c("Ecuador", "California"), lty = 1, col = c(1, 2), bty = "n")

plot(datos.ssta$Time, runmean(datos.ssta$SSTA_EC, k = 60), type = "l", main = "Anomalías de temperatura", xlab = "Tiempo", ylab = "SSTA (°C)", ylim = c(-1.5, 1.5)) 
lines(datos.ssta$Time, runmean(datos.ssta$SSTA_CA, k = 60), col = 2)
legend("topleft", c("Ecuador", "California"), lty = 1, col = c(1, 2), bty = "n")

## figura con las 4 cosas
par(mfrow = c(2, 2), mar = c(4.6, 4.6, 1, 1)) # las acomoda por lineas y despues acomoda el siguiente renglon y de margen pones abajo, izq, arriba y derecha
plot(datos.ssta$Time, datos.ssta$SSTA_EC, type = "l", xlab = "Tiempo", ylab = "SSTA (°C)", ylim = c(-4.5, 4.5)) 
lines(datos.ssta$Time, datos.ssta$SSTA_CA, col = 2)
abline(h = 0, lty = 2) # pone una linea en el eje horizontal cuando Y vale cero y el lty=2 es una linea punteada
legend("topleft", "A)", bty = "n")

plot(datos.ssta$Time, runmean(datos.ssta$SSTA_EC, k = 12), type = "l", xlab = "Tiempo", ylab = "SSTA (°C)", ylim = c(-3.5, 3.5)) 
lines(datos.ssta$Time, runmean(datos.ssta$SSTA_CA, k = 12), col = 2)
abline(h = 0, lty = 2) 
legend("topleft", "B)", bty = "n")

plot(datos.ssta$Time, runmean(datos.ssta$SSTA_EC, k = 36), type = "l", xlab = "Tiempo", ylab = "SSTA (°C)", ylim = c(-1.5, 1.5)) 
lines(datos.ssta$Time, runmean(datos.ssta$SSTA_CA, k = 36), col = 2)
abline(h = 0, lty = 2) 
legend("topleft", "C)", bty = "n")

plot(datos.ssta$Time, runmean(datos.ssta$SSTA_EC, k = 60), type = "l", xlab = "Tiempo", ylab = "SSTA (°C)", ylim = c(-1.5, 1.5)) 
lines(datos.ssta$Time, runmean(datos.ssta$SSTA_CA, k = 60), col = 2)
abline(h = 0, lty = 2) 
legend("topleft", "D)", bty = "n")

## nueva figura 4 x 1
par(mfrow = c(4, 1), mar = c(0, 3, 0, 3), oma = c(4.6, 2, 2, 0)) 
plot(datos.ssta$Time, datos.ssta$SSTA_EC, type = "l", ylab = "SSTA (°C)", ylim = c(-4.5, 4.5), xaxt = "n") # xaxt quita el eje x
lines(datos.ssta$Time, datos.ssta$SSTA_CA, col = 2)
abline(h = 0, lty = 2) 
legend("topleft", "A)", bty = "n")

plot(datos.ssta$Time, runmean(datos.ssta$SSTA_EC, k = 12), type = "l", xlab = "Tiempo", ylab = "SSTA (°C)", ylim = c(-3.5, 3.5), xaxt = "n", yaxt = "n" )
axis(4)
lines(datos.ssta$Time, runmean(datos.ssta$SSTA_CA, k = 12), col = 2)
abline(h = 0, lty = 2) 
legend("topleft", "B)", bty = "n")

plot(datos.ssta$Time, runmean(datos.ssta$SSTA_EC, k = 36), type = "l", xlab = "Tiempo", ylab = "SSTA (°C)", ylim = c(-1.5, 1.5), xaxt = "n") 
lines(datos.ssta$Time, runmean(datos.ssta$SSTA_CA, k = 36), col = 2)
abline(h = 0, lty = 2) 
legend("topleft", "C)", bty = "n")

plot(datos.ssta$Time, runmean(datos.ssta$SSTA_EC, k = 60), type = "l", xlab = "Tiempo", ylab = "SSTA (°C)", ylim = c(-1.5, 1.5), yaxt = "n" ) 
axis(4)
lines(datos.ssta$Time, runmean(datos.ssta$SSTA_CA, k = 60), col = 2)
abline(h = 0, lty = 2) 
legend("topleft", "D)", bty = "n")
mtext("Tiempo", side = 1, line = 3)
mtext("SSTA (°C)", side = 2, line = 0, outer = T)

par(mfrow = c(1, 1), mar = c(4.6, 4.6, 3.1, 1), oma = c(0, 0, 0,0))

plot(1)
plot(1, las = 0) # 
plot(1, las = 1) # eje y perpendicular
plot(1, las = 2) # eje x y y perpendiculares
plot(1, las = 3) # eje x perpendicular

par(bg = "black", fg = "white", col.axis = "white", col.lab = "white")
plot(1)

?par
