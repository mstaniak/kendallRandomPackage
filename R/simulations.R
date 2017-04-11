#' Helper function

Z <- function(x,y){
  min(x,y)/max(x,y)
}

#' Helper function

Qn <- function(x, y, alfa){
  p <- Z(abs(x), abs(y))^alfa
  sample(c(0,1), 1, prob=c(1-p, p))
}

#' Helper function

U <- function(x, y){
  if(abs(x)>abs(y)) sign(x)
  else sign(y)
}

#' Simulate one trajectory of a Kendall random walk

simulateOneTrajectory <- function(trajectoryLength, stepDist,
                                  parAlpha, ...) {
  Y <- stepDist(trajectoryLength, ...)
  theta <- rpareto( trajectoryLength, 1, 2*parAlpha)*sample(c(-1, 1), trajectoryLength, prob = c(0.5, 0.5), replace = TRUE)
  Xn <- vector("numeric", trajectoryLength )
  Xn[1:2] <- c(0, Y[1])
  
  for(i in 2:(trajectoryLength - 1)) {
    Xn[i+1] <- max(abs(Xn[i]), abs(Y[i+1]))*theta[i]^Qn(Xn[i], 
                                                        Y[i+1], 
                                                        parAlpha)*U(Xn[i], Y[i + 1])
  }
  Xn
}

#' Simulate multiple trajectories of Kendall random walk
#' 
#' @param simulationNumber number of trajectories to generate
#' @param trajectoryLength length of trajectories
#' @param stepDist function returning random numbers from step dist.
#' @param parAlpha alpha parameter
#' 
 
simulation <- function(simulationNumber, trajectoryLength,
                       stepDist, parAlpha, ...) {
  listTmp <- as.list(1:simulationNumber)
  tmp <- lapply(listTmp, function(l) 
    simulateOneTrajectory(trajectoryLength, stepDist,
                          parAlpha, ...))
  lapply(listTmp, 
         function(x) tibble(simNo = x, sim = tmp[[x]])) %>%
    bind_rows() 
} 


#' Normalising Kendall random walks
#' 
#' @param simulations tibble returned by simulation function
#' @param AnSeq sequence that the trajectories will be multiplied by
#' @param BnSeq sequence that will be substracted from scaled trajectory
#' 
#' @return tibble
#' 

normingSequences <- function(simulations, AnSeq = 1, BnSeq = 0) {
  simulations %>%
    mutate(simNo = as.factor(as.character(simNo))) %>%
    group_by(simNo) %>%
    mutate(sim = AnSeq*sim - BnSeq)
}

#' Draw simulated trajectories
#' 
#' @param simulations tibble returned by normingSequences function
#' @param ogrX maximum value on x axis
#' 
#' @return ggplot2 object
#' 

convergenceVis <- function(simulations, ogrX = NULL) {
  nSim <- max(unique(as.integer(as.character(simulations$simNo))))
  trajectoryLength <- dim(simulations)[1]/nSim
  if(is.null(ogrX)) ogrX <- trajectoryLength
  simulations %>%
    ungroup() %>%
    mutate(x = rep(1:trajectoryLength, nSim)) %>%
    filter(x <= ogrX) %>% 
    ggplot(aes(x = x, y = sim, group = simNo)) +
    geom_line() +
    theme_bw() +
    xlab("") +
    ylab("") +
    guides(color = "none")
}