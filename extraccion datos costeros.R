library(raster)
library(maps)

#### 1. importar datos de anomalias mei pdo y cuadrantes ####
load("datos/ssta 1950 2017.RData")
load("datos/MEI y PDO.RData")
load("datos/cuadrantes.RData")

n <- nlayers(ssta.pac)

names(mei.pdo) <- c("Year", "Month", "Time", "MEI", "PDO")

mei.pdo <- subset(mei.pdo, Year > 1949)
mei.pdo <- mei.pdo[1:n, ]

ssta.pac <- crop(ssta, extent(190, 300, -20, 68))
extent(ssta.pac) <- extent(189-360, 301-360, -19, 69)

plot(ssta.pac, 1)
points(xy.a)

#### 2. extraer series de temperatura de los cuadrantes costeros ####
ersst.PN.df <- extract(ssta.pac, xy.a)
ersst.PN.df <- as.data.frame(t(ersst.PN.df))
ersst.PN.df$Time <- seq(1950, 2017 + (3/12), length = n)
ersst.PN.df$Year <- trunc(ersst.PN.df$Time)
ersst.PN.df$Month <- c(rep(1:12, trunc(n / 12)), 1:3)

par.old <- par(mar = c(5.1, 4.1, 4.1, 2.1), mfrow = c(1, 1), oma = rep(0, 4))

map("world", xlim = c(-170, -70), ylim = c(-10, 70), col = "khaki", fill = T); box()
map.axes()
points(xy, pch = 16, cex = 1)
par(par.old)

#### 3. combinar series de temperatura con MEI y PDO. Nota datos a partir de 1950 ####
datos.ssta <- ersst.PN.df
datos.ssta <- merge(datos.ssta, mei.pdo[, -3])
datos.ssta <- datos.ssta[order(datos.ssta$Time), ]

datos.ssta <- subset(datos.ssta, Year < 2017)

datos.ssta <- datos.ssta[, c(1, 2, 59:61, 3:58)]
rownames(datos.ssta) <- 1:nrow(datos.ssta)

save(datos.ssta, file = "datos/ssta costa.RData")