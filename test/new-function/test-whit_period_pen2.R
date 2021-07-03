whit_period_pen2 <- function(y, period, lamb_anom = 1, lamb_season = 1e3) {
  m = length(y)
  # Basis
  E = diag.spam(m)
  B = cbind(E, E)

  # Penalty 1
  D = diff(E, diff = 2)
  P1 = t(D) %*% D

  # Penalty 2
  # p = (1/60) / 0.004 # T
  om = cos(2 * pi / period)
  D2 = matrix(0, m - 2, m)
  for (i in 1:(m - 2)) {
    D2[i, i] = D2[i, i + 2] = 1
    D2[i, i + 1] = -om * 2
  }
  D2 = as.spam(D2)
  P2 = t(D2) %*% D2

  # Make penalty matrix
  P = diag.spam(2 * m)
  r1 = 1:m
  # lambda1 = 0.1
  P[r1, r1] = lamb_anom * P1
  r2 = r1 + m

  # lambda2 = 100000
  P[r2, r2] = lamb_season * P2

  # Solve the system
  BtB = t(B) %*% B
  z = solve(BtB + P, t(B) %*% y)
  z1 = z[r1]
  z2 = z[r2]
  mu = B %*% z

  # Diagnostics
  K = solve(BtB + P, BtB)
  dk1 = diag(K[r1, r1])
  ed1 = sum(dk1)
  dk2 = diag(K[r2, r2])
  ed2 = sum(dk2)
  cat('lamb_anom, lamb_season, ed1, ed2', lamb_anom, lamb_season, ed1, ed2, '\n')
  data.frame(z1, z2)
}

{
    t <- d$date
    y <- d$GPP_DT
    y[is.na(y)] <- 0.1
    r <- whit_pen2(y, 365, lamb_anom = 100, lamb_season = 100)

    write_fig(
        {
            par(mfrow = c(2, 1), mar = c(3, 3, 1, 1))
            plot(t, r$z1, type = "l")
            plot(t, y, col = "red", type = "l", ylim = c(-2, 12))
            lines(t, r$z2)
            add_gridLine(t)
        },
        "a.pdf"
    )
}

library(JOPS)
d = df_part[site == "IT-Noe", .(date, GPP_DT,
                                x = difftime(date, date[1] - 1, units = "days") %>% as.integer())]
{
    load_all("h:/github/smooth/JOPS")
    y = d$GPP_DT
    ylu = quantile(y, c(0.1, 99.5)/100, na.rm = TRUE); ylu[1] = 0
    # profvis::profvis(
        r <- psNormal(d$x, y, lambda = 5, nseg = 9*12*3, .vcurve = T)
    # )
    # ypred = predict_psnorm(r, xout = d$x)

    write_fig({
        plot(d$date, d$GPP_DT, cex = 0.2, type = "p", ylim = ylu)
        add_gridLine(d$date)
        lines(d$date, r$ypred$yout, type = "l", col = "blue")
        lines(d$date, d$GPP_DT, lwd = 0.1)
    }, "b.pdf")
}
