# This script will create seperate datasets based on a particular modality, for example gender.


label_split = function(mainDir,tp,cov,label,out,test=FALSE){


  palpPath = file.path(mainDir,'data',paste0('extractedY',tp,""))

  if (tp == 1) {
    subjects = 'Baseline'
  } else {
    subjects = paste('Year',tp, sep = '.')
  }

  label = paste(label,'_Y', tp, sep= "")
  #fname = paste(fname,tp,"")

  # Get behavioral file
  bDf <- read.csv(cov, stringsAsFactors=FALSE)

  # Extract modality and subject codes
  bDf.sub <- bDf[,c(subjects,label)]
  bDf.sub <- bDf.sub[which(!is.na(bDf.sub[,subjects])),]

  # Match between participants which are extracted from the diretory after parsing per year PALP raw

  parts <- list.files(palpPath, pattern="PALP*")

  # remove unnecessary strings in parts name
  parts <- lapply(parts, function (x) substr(x,6,nchar(x)-4))

  if (test) {
    parts <- parts[1:1000]
  }

  bDf.sub <- bDf.sub[bDf.sub[,subjects] %in% parts,]

  # next is create lists based on modality and save those
  label.unique <- unique(bDf.sub[,label])

  print(sprintf('Creating labelled groups: %s_%s, %s_%s', out, 1, out, 2))

  for (mod in label.unique) {
    if (is.na(mod)) {
      outPath = file.path(mainDir,'data',paste(out,'_missing','.csv', sep = ""))
      subjList <- data.frame(bDf.sub[which(is.na(bDf.sub[,label])),subjects])

    } else {
      outPath = file.path(mainDir,'data',paste(out,'_',mod,'.csv', sep = ""))
      subjList <- data.frame(bDf.sub[which(bDf.sub[,label]==mod), subjects])
    }

    write.table(subjList,outPath,sep=',',row.names = FALSE, col.names = c('subjects'))
  }

  print("Completed the dataset splitting into both groups.")
}

