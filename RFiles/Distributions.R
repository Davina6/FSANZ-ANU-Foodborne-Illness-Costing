# Alternate parameterisations of various distributions
# for the standrd PERT distribution parameterised by min, mode, max
library(mc2d) #(version "0.1-21")

## PERT
ppert_alt <- function (q, mode = 0, lowq = -1, highq = 1,
                       lowp = 0.025, highp = 0.975, median = NULL,
                       shape = 4, lower.tail = TRUE,
                       log.p = FALSE) {
  if(is.null(median)){
    MinMax <- getPertMinMaxFromQuant(mode, lowq, highq,
                                     lowp = lowp, highp = highp,
                                     shape = shape)
  }else{
    MinMax <- getPertMinMaxModeFromQuant(median, lowq, highq,
                                         lowp = lowp, highp = highp,
                                         shape = shape)
    mode <- MinMax$mode
  }
  ppert(q, min = MinMax$min, mode = mode, max = MinMax$max, shape = shape, lower.tail = lower.tail,
        log.p = log.p)
}

qpert_alt <- function (p, mode = 0, lowq = -1, highq = 1,
                       lowp = 0.025, highp = 0.975, median = NULL,
                       shape = 4, lower.tail = TRUE,
                       log.p = FALSE) {
  if(is.null(median)){
    MinMax <- getPertMinMaxFromQuant(mode, lowq, highq,
                                     lowp = lowp, highp = highp,
                                     shape = shape)
  }else{
    MinMax <- getPertMinMaxModeFromQuant(median, lowq, highq,
                                         lowp = lowp, highp = highp,
                                         shape = shape)
    mode <- MinMax$mode
  }
  qpert(p, min = MinMax$min, mode = mode, max = MinMax$max, shape = shape, lower.tail = lower.tail,
        log.p = log.p)
}

dpert_alt <- function (x, mode = 0, lowq = -1, highq = 1,
                       lowp = 0.025, highp = 0.975, median = NULL,
                       shape = 4, log = FALSE) {
  if(is.null(median)){
    MinMax <- getPertMinMaxFromQuant(mode, lowq, highq,
                                     lowp = lowp, highp = highp,
                                     shape = shape)
  }else{
    MinMax <- getPertMinMaxModeFromQuant(median, lowq, highq,
                                         lowp = lowp, highp = highp,
                                         shape = shape)
    mode <- MinMax$mode
  }
  dpert(x, min = MinMax$min, mode = mode, max = MinMax$max, shape = shape, log = log)
}

rpert_alt <- function (n, mode = 0, lowq = -1, highq = 1,
                       lowp = 0.025, highp = 0.975, median = NULL,
                       shape = 4) {
  if(is.null(median)){
    MinMax <- getPertMinMaxFromQuant(mode, lowq, highq,
                                     lowp = lowp, highp = highp,
                                     shape = shape)
  }else{
    MinMax <- getPertMinMaxModeFromQuant(median, lowq, highq,
                                         lowp = lowp, highp = highp,
                                         shape = shape)
    mode <- MinMax$mode
  }
  rpert(n, min = MinMax$min, mode = mode, max = MinMax$max, shape = shape)
}


getPertMinMaxFromQuant <- function(mode, lowq, highq,
                                   lowp = 0.025, highp = 0.975,
                                   shape = 4){
  if(lowq > mode || highq < mode || lowq > highq){
    stop('Invalid (or extreme) distributional parameters. Arguments must satisfy lowq < mode < highq')
  }
  if(lowp > highp || lowp < 0 || highp > 1){
    stop('Invalid distributional parameters. Arguments must satisfy  0 < lowp < highp < 1')
  }

  f <- function(x){
    suppressWarnings(sum((qpert(c(lowp, highp), min = x[1], max = x[2], mode = mode, shape = shape) - c(lowq, highq))^2))
  }
  opt <- optim(c(lowq, highq), f , method = "Nelder-Mead", control = list(reltol = 1e-10, fnscale = highq-lowq))
  if(opt$convergence){
    stop(paste(paste('Convergence code was', opt$convergence, 'and sqrt(opt$value)/(highq-lowq) =',sqrt(opt$value)/(highq-lowq)),
               '  Distributional parameters may be invalid, or very extreme.',
               '    Sometimes, though the parameters satisfy:',
               '        lowq < mode < highq',
               '    and',
               '        0 < lowp < highp < 1',
               '    these parameters can imply a distribution which is extremely',
               '    (or impossibly) left or right skewed.',sep = '\n'))
  }
  list(min = opt$par[1], max = opt$par[2])
}

getPertMinMaxModeFromQuant <- function(median, lowq, highq,
                                   lowp = 0.025, highp = 0.975,
                                   shape = 4){

  if(lowq > median || highq < median || lowq > highq){
    stop('Invalid distributional parameters. Arguments must satisfy lowq < median < highq')
  }
  if(lowp > 0.5 || highp < 0.5 || lowp > highp || lowp < 0 || highp > 1){
    stop('Invalid distributional parameters. Arguments must satisfy 0 < lowp < 0.5 < highp < 1')
  }

  f <- function(x){
    suppressWarnings(sum((qpert(c(lowp, 0.5, highp), min = x[1], max = x[2], mode = x[3], shape = shape) - c(lowq, median, highq))^2))
  }

  opt <- optim(c(lowq, highq, median), f , method = "Nelder-Mead", control = list(reltol = 1e-10,fnscale = highq-lowq))
  if(opt$convergence){
    stop(paste(paste(median, lowq, highq,lowp, highp),
               paste('Convergence code was', opt$convergence, 'and sqrt(opt$value)/(highq-lowq) =',sqrt(opt$value)/(highq-lowq)),
               'Distributional parameters may be invalid, or very extreme.',
               '    Sometimes, though the parameters satisfy:',
               '        lowq < median < highq',
               '    and',
               '        0 < lowp < 0.5 < highp < 1',
               '    these parameters can imply a distribution which is extremely',
               '    (or impossibly) left or right skewed.',sep = '\n'))
  }

  list(min =opt$par[1], max = opt$par[2], mode = opt$par[3])
}

## log normal
dlnorm_alt <- function(x, mean = exp(0.5), sd = exp(0.5)*sqrt(exp(1)-1), log = FALSE){
  sdlog <- sqrt(log(1 + (sd/mean)^2))
  meanlog <- log(mean) - log(1 + (sd/mean)^2)/2
  dlnorm(x, meanlog, sdlog, log = log)
}

qlnorm_alt <- function(p, mean = exp(0.5), sd = exp(0.5)*sqrt(exp(1)-1), lower.tail = TRUE, log.p = FALSE){
  sdlog <- sqrt(log(1 + (sd/mean)^2))
  meanlog <- log(mean) - log(1 + (sd/mean)^2)/2
  qlnorm(p, meanlog, sdlog, lower.tail = lower.tail, log.p = log.p)
}

plnorm_alt <- function(q, mean = exp(0.5), sd = exp(0.5)*sqrt(exp(1)-1), lower.tail = TRUE, log.p = FALSE){
  sdlog <- sqrt(log(1 + (sd/mean)^2))
  meanlog <- log(mean) - log(1 + (sd/mean)^2)/2
  plnorm(q, meanlog, sdlog, lower.tail = lower.tail, log.p = log.p)
}

rlnorm_alt <- function(n, mean = exp(0.5), sd = exp(0.5)*sqrt(exp(1)-1)){
  sdlog <- sqrt(log(1 + (sd/mean)^2))
  meanlog <- log(mean) - log(1 + (sd/mean)^2)/2
  rlnorm(n, meanlog, sdlog)
}

# discrete distribution over finite set
rdiscrete <- function(n, value, prob = NULL){
  if(length(value) == 1){
    return(rep(value,n))
  }
  sample(value, n, TRUE, prob)
}

pdiscrete <- function(q, value, prob = NULL){
  if(is.null(prob)){
    prob <- rep(1, length(value))
  }else if(prob < 0) stop('negative probability')
  prob <- prob/sum(prob)
  sapply(q, function(y)sum(prob[value<=y]))
}

ddiscrete <- function(x, value, prob = NULL){
  if(is.null(prob)){
    prob <- rep(1, length(value))
  }else if(prob < 0) stop('negative probability')
  prob <- prob/sum(prob)
  sapply(x, function(.x){sum(prob[value == .x])})
}


#random samples from a step distribution: a continuous distribution on 0-100
#specified by quantiles that assumes the density is uniform between the given
#quantiles. The default  quantiles are for 5%, 50% and 95% and with the lower
#(0% quantile) and upper (100% quantiles) bounds set to 0 and 1. Modified from
#code from Anca Hanea
rstep <- function(n,quantiles,
                  percentiles = c(0.05,0.5,0.95),
                  lower = 0,upper = 1){
  y <- c(lower,quantiles,upper)
  x <- c(0, percentiles, 1)
  sample_points <- approx(x,y,xout = runif(n))$y
  return (sample_points)
}


