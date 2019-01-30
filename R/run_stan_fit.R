#!/usr/bin/ Rscript

source('import_libs.R')
import_libs()
source('aggregate_PALP2.R')
source('stan_fit.R')
source('label_split.R')
source('helpers.R')



#option_list = list(
#  make_option(c("-", "--main"), type="character", default=NULL,
#              help="main directory where project is located", metavar="character"),
#  make_option(c("-l", "--label"), type="character", default=NULL,
#              help="label for groups in covariate file", metavar="character"),
#  make_option(c("-c", "--cov"), type="character", default="COVENTURE_COMPLETE.txt",
#              help="covariate file full path [default= %default]", metavar="character"),
#  make_option(c("-y", "--year"), type="character", default='Y1',
#              help="year [default= %default]", metavar="character"),
#  make_option(c("-s", "--sim"), type="character", default=FALSE,
##              help="run simulation [default= %default]", metavar="character"),
#  make_option(c("-p", "--predict"), type="character", default=FALSE,
#              help="predict group membership using mixture model [default= %default]", metavar="character"),
#  make_option(c("-o", "--out"), type="character", default=FALSE,
#              help="out string for creating lists of subjects by modality (ex.: Gender)", metavar="character"),
#  make_option(c("-r", "--ratio"), type="array", default= c(.7,.3),
#              help="train/test ratio [default= %default]", metavar="character")
#);

#opt_parser = OptionParser(option_list=option_list);
#opt = parse_args(opt_parser);

opt = list(main= '/home/spinney/scripts/r/QlearnPalp',
           label= 'DEM_01',
           cov= '/home/spinney/scripts/r/QlearnPalp/data/COVENTURE_COMPLETE.csv',
           year= 5,
           sim= FALSE,
           predict=TRUE,
           out='gender',
           ratio=c(0.7,0.3),
      	   nchains=4,
      	   niter=1000,
      	   warmup=1000,
      	   adelta=0.99,
           clobber=TRUE)

if (is.null(opt$main)){
  print_help(opt_parser)
  stop("At least one argument must be supplied.", call.=FALSE)
}

if (sum(opt$ratio) != 1) {
  print_help(opt_parser)
  stop("Ratio must sum to 1.", call.=FALSE)

}

# Check if python data prep scripts have been run
# 1. seperate_years.py
# 2. extract_part_data.py
# 3. get_exclude_list.py

# Create lists of subjects by label
label_split(opt$main,opt$year,opt$cov,opt$label,opt$out,test=TRUE)

# Create rds datasets for both groups to be fed into Stan
p1 <- file.path(opt$main,'data',paste0(opt$out,'_1','.csv'))
p2 <- file.path(opt$main,'data',paste0(opt$out,'_2','.csv'))
palpDataPathG1 <- get_allsub(opt$main,p1,opt$year,opt$out,opt$ratio)
palpDataPathG2 <- get_allsub(opt$main,p2,opt$year,opt$out,opt$ratio)


# fit the palp on each goup seperately to get group level parameters

# Check if files exist, or if clobber = True
print('Beginning fit...')
#fitPathG1 <- stan_fit(opt$main,opt$out,palpDataPathG1$train,opt$clobber,opt$nchains,opt$niter,opt$warmup,opt$adelta,verbose=TRUE)
print("Group 1 done. Starting group 2.")
fitPathG2 <- stan_fit(opt$main,opt$out,palpDataPathG2$train,opt$clobber,opt$nchains,opt$niter,opt$warmup,opt$adelta,verbose=TRUE)
print("Group level fit is complete.")

if (isTRUE(opt$predict)) {
  print("Beginning mixture model fit for classification...")
  # extract group level means and stds for all parameters
  g1Params <- extract_parameters(fitPathG1)
  g2Params <- extract_parameters(fitPathG2)

  print(rbind(g1Params,g2Params))
  # using the parameters estimated on a train set we evaluate the test set group membership
  fitMix <- stan_mix_fit(opt$main,
                         opt$out,
                         g1Params,
                         g2Params,
                         list(g1 = palpDataPathG1$test, g2 = palpDataPathG2$test),
                         nchains=6,
                         niter=3000,
                         warmup=1000,
                         adelta=0.99)

  # extract group prediction and calculate accuracy
  resultDf <- create_result_df(fitMix$outname,fitMix$group,fitMix$subInd)
  accuracy <- sum(resultDf$zmean == resultDf$group)/nrow(resultDf)
}
# extract the mean and std of the fitted group level parameters

