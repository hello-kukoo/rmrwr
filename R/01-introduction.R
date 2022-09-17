x <- 0:12
y <- sin(pi / 5 * x)
op <- par(mfrow = c(3, 3), mar = .1 + c(2, 2, 3, 1))
for (tp in c("p", "l", "b",  "c", "o", "h",  "s", "S", "n")) {
  plot(y ~ x,
       type = tp,
       main = paste0("plot(*, type = \"", tp, "\")"))
  if (tp == "S") {
    lines(x,
          y,
          type = "s",
          col = "red",
          lty = 2)
    mtext("lines(*, type = \"s\", ...)",
          col = "red",
          cex = 0.8)
  }
}
par(op)

m <- matrix(data = c(1, 2, 3, 4),
            nrow = 2,
            ncol = 2)



