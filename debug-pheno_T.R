set.seed(1000)
x = rnorm(365)
t = seq(as.Date("2010-01-01"), as.Date("2010-12-31"), "day")

x = 1:365
A = 2*pi/365
y = sin(A*x)*10 + rnorm(365)


plot(t, y)
# Pheno_thermal(y, t, trs = c(5, 10)*5, n = 3, adjust = T)
Pheno_thermal(y, t, trs = c(10, 10), n = 3, adjust = T)

{
    sitename <- "CA-NS1"
    d = df0[site == "CA-Man" & group == 1, ]

    # year <= 2001 & year >= 2001
    info <- d[, Pheno_thermal(LST_Night_1km, date, trs = c(5, 10)), .(year2)]

    plot(LST_Night_1km~date, d, type = "l"); grid()
    points(sos_val~sos_date, info, col = "blue")
    points(eos_val~eos_date, info, col = "red")
}
