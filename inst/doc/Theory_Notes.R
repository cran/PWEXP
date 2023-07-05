## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>"
)

## ----setup, message=FALSE, results='hide', warning=FALSE----------------------
library(PWEXP)
library(RColorBrewer)
set.seed(1818)

## ----fig.show="hold", out.width="50%", out.height="100%", fig.width=6, fig.height=5.8----
# Left Figure ------------------------------------------------------------
# use rpwexp function to generate piecewise exp samples with rate 2, 1, 3
r_sample <- rpwexp(50000, rate=c(2, 1, 3), breakpoint=c(0.3, 0.8))
hist(r_sample, freq=F, breaks=200, main="Density of Piecewsie Exp Dist", xlab='t', xlim=c(0, 1.2))

# piecewise exp density with rate 2, 1, 3 
t <- seq(0, 1.5, 0.01)
f2 <- dpwexp(t, rate=c(2, 1, 3), breakpoint=c(0.3, 0.8))
points(t, f2, col='red', pch=16)

# exp distribution can be a special case of piecewise exp distribution
f1 <- dpwexp(t, rate=2) 
lines(t, f1, lwd=2)
legend('topright', c('exp dist with rate 2','piecewise exp dist with rate 2, 1, 3',
                     'histogram of piecewise exp dist with rate 2, 1, 3'), 
       col=c('black','red'), fill=c(NA, NA, 'grey'), border=c('white', 'white', 'black'), 
       lty=c(1, NA, NA), pch=c(NA, 16, NA), lwd=2)

# Right Figure ------------------------------------------------------------
# CDF of piecewise exp with rate 2, 1, 3
F2 <- ppwexp(t, rate=c(2, 1, 3), breakpoint=c(0.3, 0.8), lower.tail=T)
plot(t, F2, type='l', col='red', lwd=2, main="CDF and Quantile Function of Piecewsie Exp Dist", 
     xlim=c(0, 1.5), ylim=c(0, 1.5))

# CDF of exp dist is compatible with our package
F1 <- ppwexp(t, rate=2, lower.tail=T)
lines(t, F1, lwd=2)

# plot quantile functions of both distributions
lines(F1, qpwexp(F1, rate=2, lower.tail=T), lty=2, lwd=2)
lines(F2, qpwexp(F2, rate=c(2, 1, 3), breakpoint=c(0.3,0.8), lower.tail=T), col='red', 
      lty=2, lwd=2)

abline(0, 1, col='grey')
legend('topleft', c('CDF of piecewise exp with rate 2, 1, 3', 'quantile function of 
                    piecewise exp with rate 2, 1, 3', 
                    'CDF of exp with rate 2', 'quantile function of exp with rate 2'), 
       col=c('red', 'red', 'black', 'black'), lty=c(1, 2, 1, 2), lwd=2)

## ----fig.show="hold", out.width="50%", out.height="100%", fig.width=6, fig.height=5.8----
# Left Figure ------------------------------------------------------------
# CDF and qunatile function of conditional piecewise exp with rate 2, 1, 3 given t > 0.1
t <- seq(0.1, 1.2, 0.01)
F2_con <- ppwexp_conditional(t, qT=0.1, rate=c(2, 1, 3), breakpoint=c(0.3, 0.8))
plot(t, F2_con, type='l', col='red', lwd=2, main="CDF and Quantile Function of 
     Conditional \nPiecewsie Exp Dist", xlim=c(0, 1.2), ylim=c(0, 1.2))
lines(F2_con, qpwexp_conditional(F2_con, qT=0.1, rate=c(2, 1, 3), breakpoint=c(0.3,0.8)), 
      lty=2, lwd=2, col='red')

# compare with CDF and quantile function of unconditional piecewise exp with rate 2, 1, 3
t <- seq(0, 1.2, 0.01)
F2 <- ppwexp(t, rate=c(2, 1, 3), breakpoint=c(0.3,0.8))
lines(t, F2, lwd=2)
lines(F2, qpwexp(F2, rate=c(2, 1, 3), breakpoint=c(0.3,0.8)), lty=2, lwd=2)
abline(v=0.1, col='grey')
abline(h=0.1, col='grey')
legend('topleft', c('CDF of piecewise exp dist given t > 0.1', 'quantile function of 
                    piecewise exp dist given t > 0.1', 'CDF of piecewise exp dist', 
                    'quantile function of piecewise exp dist'), col=c('red', 'red', 'black', 'black'),
       lty=c(1, 2, 1, 2), lwd=2)

# Right Figure ------------------------------------------------------------
# use rpwexp_conditional function to generate piecewise exp samples with rate 2, 1, 3 given t > 0.1
r_sample_con <- rpwexp_conditional(3000, qT=0.1, rate=c(2, 1, 3), breakpoint=c(0.3,0.8))
plot(ecdf(r_sample_con), col='red', lwd=2,  main="Empirical CDF of Conditional 
     Piecewsie Exp Dist", xlim=c(0, 1.2), ylim=c(0, 1))

# compare with its CDF
lines(seq(0.1, 1.2, 0.01), F2_con, lwd=2)
legend('topleft', c('empirial CDF of piecewise exp dist given t > 0.1', 'true CDF of 
                    piecewise exp dist given t > 0.1'), col=c('red', 'black'), lty=c(1,2), lwd=2)

