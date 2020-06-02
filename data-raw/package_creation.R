# Creating minimal functional package
# https://www.hvitfeldt.me/blog/usethis-workflow-for-package-development/


## Before creation
##############

#install.packages(c("devtools", "roxygen2", "usethis"))
library(devtools)
library(roxygen2)
library(usethis)

#install.packages("available")
library(available)
available("trendex")


## Creating minimal functional package
###############

create_package("~/Documents/R/trendex")
use_git()
use_github()


## One time modifications
##############

use_gpl3_license()

###

use_readme_rmd()


### to include the testing capasity of testthat in your package simply run the following
use_testthat()

###

use_spell_check()

###

use_data_raw()


## packages uses

use_package("Rlibeemd")

## functions constructions
use_r("macro")

use_r("trendex")

## tests constructions

use_test("test_trendex")






use_r("phasefcimf")

use_r("nvar")

use_r("plot_pairedimfs")

use_r("ostest")

data("macro")
lgdp <- ts(macro[,3], freq=4, start = c(1970, 1))

teste <- trendex(lgdp)
plot(teste)
lines(lgdp)


unemp.hp <- mFilter(unemp,filter="HP")  # Hodrick-Prescott filter
print(unemp.hp)
summary(unemp.hp)
residuals(unemp.hp)
fitted(unemp.hp)
plot(unemp.hp)


unemp.bk <- mFilter(unemp,filter="BK")  # Baxter-King filter
unemp.cf <- mFilter(unemp,filter="CF")  # Christiano-Fitzgerald filter
unemp.bw <- mFilter(unemp,filter="BW")  # Butterworth filter
unemp.tr <- mFilter(unemp,filter="TR")  # Trigonometric regression filter

par(mfrow=c(2,1),mar=c(3,3,2,1),cex=.8)
plot(unemp,main="Unemployment Series & Estimated Trend", col=1, ylab="")
lines(unemp.hp$trend,col=2)
lines(unemp.bk$trend,col=3)
lines(unemp.cf$trend,col=4)
lines(unemp.bw$trend,col=5)
lines(unemp.tr$trend,col=6)



legend("topleft",legend=c("series", "HP","BK","CF","BW","TR"),
       col=1:6,lty=rep(1,6),ncol=2)

plot(unemp.hp$cycle,main="Estimated Cyclical Component",
     ylim=c(-2,2.5),col=2,ylab="")
lines(unemp.bk$cycle,col=3)
lines(unemp.cf$cycle,col=4)
lines(unemp.bw$cycle,col=5)
lines(unemp.tr$cycle,col=6)
#
