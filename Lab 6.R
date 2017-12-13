dat = read.csv("compact_nogrowth.txt", sep = " ", header = FALSE)
colnames(dat) = c("node", "t", "k")
dat1 = dat[which(dat[,1] == 1),]
dat10 = dat[which(dat[,1] == 10),]
dat100 = dat[which(dat[,1] == 100),]
dat1000 = dat[which(dat[,1] == 1000),]

plot(dat1$t, dat1$k*sqrt(1), type = "l", col = "green",
     xlab = "t", ylab = "k", ylim = c(0, 2500))
lines(dat1000$k * sqrt(1000), col = "blue")
lines(dat10$k * sqrt(10), col = "red")
lines(dat100$k * sqrt(100), col = "yellow")


m0 = 5
plot(rdat1$t, rdat1$k * (m0 * log(m0 + 1 - 1) - m0), type = "l", col = "green",
     xlab = "t", ylab = "k", ylim = c(0, 1300))
lines(rdat1000$k * (m0 * log(m0 + 1000 - 1) - m0), col = "blue")
lines(rdat10$k * (m0 * log(m0 + 10 - 1) - m0), col = "red")
lines(rdat100$k * (m0 * log(m0 + 100 - 1) - m0), col = "yellow")