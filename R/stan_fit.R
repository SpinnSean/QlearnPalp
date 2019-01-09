library(rstan)
library(stringr)
source('helpers.R')
options(mc.cores = parallel::detectCores())

combine_groups <- function(g1,g1Params, g2, g2Params){

  cue <- concat3DMat(g1$cue, g2$cue)
  choice <- concat3DMat(g1$choice, g2$choice)
  outcome <- concat3DMat(g1$outcome, g2$outcome)
  pretreat <- concat3DMat(g1$pretreat, g2$pretreat)
  prerewards <- concat3DMat(g1$prerwds, g2$prerwds)
  group <- c(rep(1,g1$N), rep(2,g2$N))

  shuffled_ind <- sample(seq_len(nrow(cue))-1)

  group <- group[shuffled_ind]
  combinedTestSet <- list(N = dim(choice[shuffled_ind,,])[1],
                          I = 6,
                          T = dim(choice[shuffled_ind,,])[3],
                          B = dim(choice[shuffled_ind,,])[2],
                          cue = cue[shuffled_ind,,],
                          outcome = outcome[shuffled_ind,,],
                          choice = choice[shuffled_ind,,],
                          pretreat=pretreat[shuffled_ind,,],
                          prerwds= prerewards[shuffled_ind,,],
                          mu_mu_alpha_1 = g1Params$mu_mu_alpha,
                          mu_mu_re_re_1 = g1Params$mu_mu_re_re,
                          mu_mu_re_pu_1 = g1Params$mu_mu_re_pu,
                          mu_mu_gobias_1 = g1Params$mu_mu_gobias,
                          std_mu_alpha_1 = g1Params$std_mu_alpha,
                          std_mu_re_re_1 = g1Params$std_mu_re_re,
                          std_mu_re_pu_1 = g1Params$std_mu_re_pu,
                          std_mu_gobias_1 = g1Params$std_mu_gobias,
                          mu_std_alpha_1 = g1Params$mu_std_alpha,
                          mu_std_re_re_1 = g1Params$mu_std_re_re,
                          mu_std_re_pu_1 = g1Params$mu_std_re_pu,
                          mu_std_gobias_1 = g1Params$mu_std_gobias,
                          std_std_alpha_1 = g1Params$std_std_alpha,
                          std_std_re_re_1 = g1Params$std_std_re_re,
                          std_std_re_pu_1 = g1Params$std_std_re_pu,
                          std_std_gobias_1 = g1Params$std_std_gobias,
                          mu_mu_alpha_2 = g2Params$mu_mu_alpha,
                          mu_mu_re_re_2 = g2Params$mu_mu_re_re,
                          mu_mu_re_pu_2 = g2Params$mu_mu_re_pu,
                          mu_mu_gobias_2 = g2Params$mu_mu_gobias,
                          std_mu_alpha_2 = g2Params$std_mu_alpha,
                          std_mu_re_re_2 = g2Params$std_mu_re_re,
                          std_mu_re_pu_2 = g2Params$std_mu_re_pu,
                          std_mu_gobias_2 = g2Params$std_mu_gobias,
                          mu_std_alpha_2 = g2Params$mu_std_alpha,
                          mu_std_re_re_2 = g2Params$mu_std_re_re,
                          mu_std_re_pu_2 = g2Params$mu_std_re_pu,
                          mu_std_gobias_2 = g2Params$mu_std_gobias,
                          std_std_alpha_2 = g2Params$std_std_alpha,
                          std_std_re_re_2 = g2Params$std_std_re_re,
                          std_std_re_pu_2 = g2Params$std_std_re_pu,
                          std_std_gobias_2 = g2Params$std_std_gobias)

  return(list(data=combinedTestSet, group=group, subInd=shuffled_ind))
}


extract_parameters <- function(fitPath){

  fit <- readRDS(fitPath)

  # Select file name for output of parameter stats for group
  outFile = '"fit18_mean_parameters_Y1_test.csv"'

  mu_mu_alpha <- summary(fit,pars="mu_alpha")$summary[,"mean"]
  mu_mu_gobias <- summary(fit,pars="mu_gobias")$summary[,"mean"]
  mu_mu_re_re <- summary(fit,pars="mu_re_re")$summary[,"mean"]
  mu_mu_re_pu <- summary(fit,pars="mu_re_pu")$summary[,"mean"]

  std_mu_alpha <- summary(fit,pars="mu_alpha")$summary[,"sd"]
  std_mu_gobias <- summary(fit,pars="mu_gobias")$summary[,"sd"]
  std_mu_re_re <- summary(fit,pars="mu_re_re")$summary[,"sd"]
  std_mu_re_pu <- summary(fit,pars="mu_re_pu")$summary[,"sd"]

  mu_std_alpha <- summary(fit,pars="sigma[1]")$summary[,"mean"]
  mu_std_gobias <- summary(fit,pars="sigma[4]")$summary[,"mean"]
  mu_std_re_re <- summary(fit,pars="sigma[2]")$summary[,"mean"]
  mu_std_re_pu <- summary(fit,pars="sigma[3]")$summary[,"mean"]

  std_std_alpha <- summary(fit,pars="sigma[1]")$summary[,"sd"]
  std_std_gobias <- summary(fit,pars="sigma[4]")$summary[,"sd"]
  std_std_re_re <- summary(fit,pars="sigma[2]")$summary[,"sd"]
  std_std_re_pu <- summary(fit,pars="sigma[3]")$summary[,"sd"]



  paramsDf <- data.frame(cbind(mu_mu_alpha,
                               mu_mu_gobias,
                               mu_mu_re_re,
                               mu_mu_re_pu,
                               std_mu_alpha,
                               std_mu_gobias,
                               std_mu_re_re,
                               std_mu_re_pu,
                               mu_std_alpha,
                               mu_std_gobias,
                               mu_std_re_re,
                               mu_std_re_pu,
                               std_std_alpha,
                               std_std_gobias,
                               std_std_re_re,
                               std_std_re_pu))

  #write.csv(paramsDf,file=outFile)
  return(paramsDf)
}



<<<<<<< HEAD
stan_fit <- function(mainDir,out,palpDataPath,clobber=FALSE,nchains=2,niter=1500,warmup=1000, adelta=0.99) {
=======
stan_fit <- function(mainDir,out,palpDataPath,nchains=2,niter=1500,warmup=1000, adelta=0.99,verbose=FALSE) {
>>>>>>> 41168f1f7c65d26a97888e828e6722f258a505cb

  group <- str_extract_all(palpDataPath,"\\(?[0-9,.]+\\)?")[[1]][1]
  outname = file.path(mainDir, 'fits', paste0(out,'_', group, '_','fit.rds'))

  if (file.exists(outname) & clobber==FALSE) {
    return(outname)
  }

  palp_data <- readRDS(palpDataPath)

  ## INITIAL PARAMETERS
  # init1 <- function() {
  #   list(chain1=list(alpha=rep(0.4,N),gobias=rep(1,N),re_re=rep(1,N),re_pu=rep(1,N)),
  #        chain2=list(alpha=rep(0.4,N),gobias=rep(1,N),re_re=rep(1,N),re_pu=rep(1,N)))
  # }


  fit <- stan(file = file.path(mainDir, 'models', 'model18.stan'),
              data = palp_data,
              iter = niter,
              warmup= warmup,
              chains = nchains,
              control = list(adapt_delta = adelta),
	      verbose=verbose)


  saveRDS(fit, outname)
  return(outname)
}

stan_mix_fit <- function(mainDir, out, g1Params, g2Params, palpDataPathTest,nchains=2,niter=1500,warmup=1000, adelta=0.99) {

  palpDataG1 <- readRDS(palpDataPathTest$g1)
  palpDataG2 <- readRDS(palpDataPathTest$g2)
  palp_data <- combine_groups(palpDataG1, g1Params, palpDataG2, g2Params)

  fit <- stan(file = file.path(mainDir, 'models', 'model18_mix.stan'),
              data = palp_data$data,
              iter = niter,
              warmup= warmup,
              chains = nchains,
              control = list(adapt_delta = adelta))

  group = palp_data$group

  outname = file.path(mainDir, 'fits', paste0(out,'_mix_','fit.rds'))
  saveRDS(fit, outname)
  return(list(outname=outname, group=group, subInd=palp_data$subInd))
}
