runAll <- function(binwidth){
  assign("binwidth", binwidth, envir=globalenv())
  source("./01-make-composite-timeseries-plots.R")
  source("./02-fit-linear-models.R")
  source("./03-regression-plots-SI.R")
  source("./04-do-sites-get-bigger.R")
  source("./05-lithics-versus-bones.R")
  source("./06-plot-REC-time-series.R")
  source("./07-plot-arch-sites-in-bins.R")
}



runAll(0.1)

