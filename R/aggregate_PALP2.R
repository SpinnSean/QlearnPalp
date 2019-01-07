
library(plyr)
# Get all data from either simulated or participant based on passed argument

get_allsim <- function(p,T){
  n = 1
  files = dir(p, 'PALP')
  N = length(files)

  cue <- array(0,dim=c(N,2,T)) # The cues for all participants, blocks and trial
  choice <- array(0,dim=c(N,2,T)) # choices
  outcome <- array(0,dim=c(N,2,T)) # outcome (reward)
  pretreat <- array(0,dim=c(N,2,9)) # pretreatment cues (9 for each block)
  prerewards <- array(0,dim=c(N,2,9)) # pretreatment rewards

  for (f in files){
    simdat <- readRDS(paste(p,f,sep=""))
    choice[n,,] <- simdat$c
    outcome[n,,] <- simdat$r
    cue[n,,] <- simdat$cues
    pretreat[n,,] <- simdat$pretreat_cues
    prerewards[n,,] <- simdat$pretreat_rwds
    n=n+1
  }

  palp_data <- list(N = N,
                  I = 6,
                  T = dim(choice)[3],
                  B = dim(choice)[2],
                  cue = cue,
                  outcome = outcome,
                  choice = choice,
                  pretreat=pretreat,
                  prerwds= prerewards)

  outname = '/RQusagers/spinney/data/RL_PALP/processed/allsimpalp.rds'
  saveRDS(palp_data, outname)

}


filesQC <- function(files){

  flist = c()

  for (f in files){
    # print(f)
    subdat <- read.table(f, sep=",")
    colnames(subdat) <- c('id', 'block', 'cue', 'choice', 'outcome', 'pretreat')

    # Check if participant completed entire task (42 trials,both conditions)
    blockcounts <- count(subdat$block[which(subdat[,'pretreat']==0)])
    if (is.na(blockcounts[1,2]) | is.na(blockcounts[2,2])) {
      #missblockparts <- c(missblockparts, f)
      print(f)
      next
    } else if (blockcounts[1,2] != 42 | blockcounts[2,2] != 42) {
      #missblockparts <- c(missblockparts, f)
      print(f)
      next
    } else {

      flist <- c(flist,f)
    }
  }

  return(flist)
}

getSubPaths = function(mainDir,p,tp){
  subList = read.csv(p)
  subList <- lapply(subList$subjects, function (x) {paste('PALP_',x,'.txt',sep = "")} )
  subPaths <- lapply(subList, function (x) {file.path(mainDir,'data',paste('extractedY',tp,sep = ""),x)})
  return(subPaths)
}


get_allsub <- function(mainDir,p,tp,out,ratio){
  n = 1
  files = getSubPaths(mainDir,p,tp)
  #files=files[1:50]
  files = filesQC(files)
  N <- length(files) # num of participants
  T <- 42 # Number of trials (per conditions)
  parts <- c()

  cue <- array(0,dim=c(N,2,T)) # The cues for all participants, blocks and trial
  choice <- array(0,dim=c(N,2,T)) # choices
  outcome <- array(0,dim=c(N,2,T)) # outcome (reward)
  pretreat <- array(0,dim=c(N,2,9)) # pretreatment cues (9 for each block)
  prerewards <- array(0,dim=c(N,2,9)) # pretreatment rewards

 # missblockparts <- list()

  for (f in files){
    # print(f)
    subdat <- read.table(f, sep=",")
    colnames(subdat) <- c('id', 'block', 'cue', 'choice', 'outcome', 'pretreat')


    RRchoices  <- subdat[ which(subdat[,'block']==1 & subdat[,'pretreat']==0), 'choice']
    RRoutcomes <- subdat[ which(subdat[,'block']==1 & subdat[,'pretreat']==0) ,'outcome']
    RRcues <- subdat[ which(subdat[,'block']==1 & subdat[,'pretreat']==0) ,'cue']
    RRpretreat <- subdat[ which(subdat[,'block']==1 & subdat[,'pretreat']==1) ,'cue']
    RRprerwds <- subdat[ which(subdat[,'block']==1 & subdat[,'pretreat']==1) ,'outcome']

    RPchoices  <- subdat[ which(subdat[,'block']==0 & subdat[,'pretreat']==0), 'choice']
    RPoutcomes <- subdat[ which(subdat[,'block']==0 & subdat[,'pretreat']==0) ,'outcome']
    RPcues <- subdat[ which(subdat[,'block']==0 & subdat[,'pretreat']==0) ,'cue']
    RPpretreat <- subdat[ which(subdat[,'block']==0 & subdat[,'pretreat']==1) ,'cue']
    RPprerwds <- subdat[ which(subdat[,'block']==0 & subdat[,'pretreat']==1) ,'outcome']

    # Remap all cues to 1 -> 6
    RRcuerepl <- unique(RRpretreat)
    RRcues <- mapvalues(RRcues, from=RRcuerepl, to=seq.int(1,6))
    RRpretreat <- mapvalues(RRpretreat, from=RRcuerepl, to=seq.int(1,6))
    RPcuerepl <- unique(RPpretreat)
    RPcues <- mapvalues(RPcues, from=RPcuerepl, to=seq.int(1,6))
    RPpretreat <- mapvalues(RPpretreat, from=RPcuerepl, to=seq.int(1,6))

    c <- rbind(RRchoices, RPchoices)
    r <- rbind(RRoutcomes, RPoutcomes)
    q <- rbind(RRcues, RPcues)
    pt <- rbind(RRpretreat, RPpretreat)
    pr <- rbind(RRprerwds, RPprerwds)

    # Added this part to deal with missing part data
    # which was identified by seeing 0's in cue

    if ( any(q==0)){
      print(f)
      next
    }else {
      choice[n,,] = c
      outcome[n,,] = r
      cue[n,,] = q
      pretreat[n,,] = pt
      prerewards[n,,] = pr
    }
    n = n + 1
    parts <- c(parts,f)
  }

  # 75% of the sample size
  train_size <- floor(ratio[1] * n)

  # set seed for reproducible example if needed
  #set.seed(123)
  train_ind <- sample(seq_len(n)-1, size = train_size)

  train <- list(N = dim(choice[train_ind,,])[1],
                I = 6,
                T = dim(choice[train_ind,,])[3],
                B = dim(choice[train_ind,,])[2],
                cue = cue[train_ind,,],
                outcome = outcome[train_ind,,],
                choice = choice[train_ind,,],
                pretreat=pretreat[train_ind,,],
                prerwds= prerewards[train_ind,,])

  test <- list(N = dim(choice[-train_ind,,])[1],
                I = 6,
                T = dim(choice[-train_ind,,])[3],
                B = dim(choice[-train_ind,,])[2],
                cue = cue[-train_ind,,],
                outcome = outcome[-train_ind,,],
                choice = choice[-train_ind,,],
                pretreat=pretreat[-train_ind,,],
                prerwds= prerewards[-train_ind,,])


  group <- substr(p,nchar(p)-4,nchar(p)-4)

  outnameDataTrain = file.path(mainDir, 'data', paste(out,'_', group,'_Y',tp,'train.rds', sep = ""))
  outnameDataTest = file.path(mainDir, 'data', paste(out,'_', group,'_Y',tp,'test.rds', sep = ""))

  outnamePartsTrain = file.path(mainDir, 'data', paste(out,'_', group,'_Y',tp,'_palp_qc_sub_train' ,'.txt', sep = ""))
  outnamePartsTest = file.path(mainDir, 'data', paste(out,'_', group,'_Y',tp,'_palp_qc_sub_test' ,'.txt', sep = ""))

  saveRDS(train, outnameDataTrain)
  saveRDS(test, outnameDataTest)

  write.csv(parts,outnamePartsTrain, row.names = FALSE, col.names = FALSE)
  #write.csv(parts,outnamePartsTest, row.names = FALSE, col.names = FALSE)

  print(sprintf("Completed PALP data formatting for group %s in rds format.", group))

  return(list(train=outnameDataTrain, test=outnameDataTest))
}



