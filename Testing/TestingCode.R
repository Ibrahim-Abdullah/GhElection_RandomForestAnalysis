# authorship.R, Francis Yinbi, 04/Dec/2016
# determining authorship via relative stopwords
# per http://wiekvoet.blogspot.ca/2012/12/common-words-in-gathering-storm.html
library(randomForest) # for randomForest()
library(lattice) # for bwplot()
# count up the number of occurences of a specified word in a chapter
numwords <- function(what,where) {
  g1 <- gregexpr(paste('[[:blank:]]+[[:punct:]]*',what,'[[:punct:]]*[[:blank:]]+',sep=''),where,perl=TRUE,ignore.case=TRUE)
  if (g1[[1]][1]==-1) 0L
  else length(g1[[1]])
}
# count up the fraction of the words in a chapter that are the
# each of the English stop words
countwords <- function(book) {
  sw <- tm::stopwords("English")
  la <- lapply(book,function(where) {        
    sa <- sapply(sw,function(what) numwords(what,where))
    ntot <- length(gregexpr('[[:blank:]]+',
                            where,perl=TRUE,ignore.case=TRUE)[[1]])
    sa/ntot
  } )
  mla <- t(do.call(cbind,la))
  return(mla)
}
# figure out how to read Gutenberg books into vectors
# with one chapter per element
load.my.library = function(books) {
  bookchapters = numeric()
  for(ibook in seq(along=books)) { 
    txt = readLines(books[ibook]) # read next book
    bk = character()
    # look for special lines to indicate chapter starts
    chaps = grep("^CHAPTER [IVXLC0-9.]+$",txt,value=FALSE,ignore.case=TRUE)
    # and a special line after the last chapter
    endlast = grep("End of manifesto",txt,value=FALSE,ignore.case=TRUE)
    chaps = c(chaps,endlast)
    # paste together chapters into single strings
    for(ch in 1:(length(chaps)-1)){
      bk = c(bk, paste(txt[(chaps[ch]+1):(chaps[ch+1]-1)],sep="",collapse=" "))
    }
    cat(paste(books[ibook])," has ",length(bk)," chapters.\n",sep="")
    bookchapters = c(bookchapters,length(bk))
    # compile the stopwords matrix
    # the columns are stop words, rows are chapters
    if(ibook==1) { monster = countwords(bk) } else {
      monster = rbind(monster,countwords(bk)) }
  }
  return(list(fractions=monster,bookchapters=bookchapters))
}

setwd("C:/Users/Francis/Desktop/Data Mining Project/GhElection_RandomForestAnalysis/Testing")
# get list of books
books = list.files(pattern="[.]txt$")
books
authors = rep("APC",length(books))
authors[grepl("CPP",books)] = "CPP"
authors[grepl("NDC",books)] = "NDC"
authors[grepl("NPP",books)] = "NPP"
authors[grepl("PPP",books)] = "PPP"
findme = rep(0,length(books))
findme[max(seq(along=books)[authors=="APC"])] = 1
findme[max(seq(along=books)[authors=="CPP"])] = 2
findme[max(seq(along=books)[authors=="NDC"])] = 3
findme[max(seq(along=books)[authors=="NPP"])] = 4
findme[max(seq(along=books)[authors=="PPP"])] = 5
# set up a vector to count the chapters per book

cat("Reading books, this may take a while...\n")
my.library.summary = load.my.library(books)
# stop word frequencies are reported by chapter
bookchapters = my.library.summary$bookchapters
monster = my.library.summary$fractions
guesschapters = rep(findme,bookchapters)
authorbychapter = rep(authors,bookchapters)
# select the training rows and build model
all <- monster[guesschapters==0,]
cats <- factor(authorbychapter[guesschapters==0])

set.seed(2015)
rf1 <- randomForest(y=cats,x=all,importance=TRUE)
rf1
# show top discriminating words
im <- importance(rf1)
toshow <- rownames(im)[order(-im[,'MeanDecreaseGini'])][1:9]
tall <- as.data.frame(scale(all[,toshow]))
tall$chapters <- rownames(tall)
tall$cats <- cats
rownames(tall) <- 1:nrow(tall)
propshow <- reshape(tall,direction='long',
                    timevar='Word',
                    v.names='ScaledScore',
                    times=toshow,
                    varying=list(toshow))
bwplot(  cats ~ScaledScore  | Word,data=propshow)
# now try to predict on new texts:
pAPC <- predict(rf1,monster[guesschapters==1,],type='prob')
#pCPP <- predict(rf1,monster[guesschapters==2,],type='prob')
#pNDC <- predict(rf1,monster[guesschapters==3,],type='prob')
#pNPP <- predict(rf1,monster[guesschapters==4,],type='prob')
#pPPP <- predict(rf1,monster[guesschapters==5,],type='prob')

dimnames(pAPC) = NULL
#dimnames(pCPP) = NULL
#dimnames(pNDC) = NULL
#dimnames(pNPP) = NULL
#dimnames(pPPP) = NULL

preds <- as.data.frame(rbind(pAPC))
names(preds) = c("APC")
winner = apply(rbind(pAPC),1,max)
guess = rep("unkn",dim(preds)[2])
guess[preds[,1]==winner] = names(preds)[1]
#guess[preds[,2]==winner] = names(preds)[2]
#guess[preds[,3]==winner] = names(preds)[3]
#guess[preds[,4]==winner] = names(preds)[4]
#guess[preds[,5]==winner] = names(preds)[5]

guess = factor(guess,levels=names(preds)[1])
preds$Book <- c(rep('APC-2016-part2',nrow(pAPC))
                #rep('CPP-2016-part2',nrow(pCPP)),
                #rep('NDC-2016-part2',nrow(pNDC)),
                #rep('NPP-2016-part2',nrow(pNPP)),
                #rep('PPP-2016-part2',nrow(pPPP))
                )
predshow <- reshape(preds,direction='long',
                    timevar='Prediction',v.names='Score',times=c('APC'),
                    varying=list(w=c('APC')))
densityplot(~Score | Prediction + Book,data=predshow)
table(preds$Book,guess)

