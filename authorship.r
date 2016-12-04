# authorship.R, cwjackson, 9/Nov/2016
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
endlast = grep("End of the Project Gutenberg",txt,value=FALSE,ignore.case=TRUE)
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

setwd("C:\\Users\\JUNIOR\\OneDrive\\Documents\\Junior Year\\Data Minning\\My Books")
# get list of books
books = list.files(pattern="[.]txt$")
# this was my library:
# train 44 chapters: "a-connecticut-yankee-in-king-arthurs-court-mark-twain.txt" 
# train 42 chapters: "adventures-huckleberry-finn-mark-twain.txt"               
# train 25 chapters: "agnes-grey-emily-bronte.txt"                              
# train 55 chapters: "emma-jane-austen.txt"                                     
# train 37 chapters: "jane-eyre-charlotte-bronte.txt"                           
# train 31 chapters: "northanger-abbey-jane-austen.txt"                         
# test  61 chapters: "pride-and-prejudice-jane-austen.txt"                      
# train 37 chapters: "shirley-charlotte-bronte.txt"                             
# test  35 chapters: "tom-sawyer-mark-twain.txt"                                
# test  42 chapters: "villette-charlotte-bronte.txt"                            
# test  34 chapters: "wuthering-heights-emily-bronte.txt"
# the authors were charlotte bronte, emily bronte, jane austen,
# and mark twain, and their names appear in the filenames
# try to predict one of each, the last of the list by
# each author
authors = rep("LMA",length(books))
authors[grepl("stephen-crane",books)] = "SC"
authors[grepl("henry-james",books)] = "HJ"
findme = rep(0,length(books))
findme[max(seq(along=books)[authors=="LMA"])] = 1
findme[max(seq(along=books)[authors=="SC"])] = 2
findme[max(seq(along=books)[authors=="HJ"])] = 3
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
pLMA <- predict(rf1,monster[guesschapters==1,],type='prob')
pSC <- predict(rf1,monster[guesschapters==2,],type='prob')
pHJ <- predict(rf1,monster[guesschapters==3,],type='prob')

dimnames(pLMA) = NULL
dimnames(pSC) = NULL
dimnames(pHJ) = NULL

preds <- as.data.frame(rbind(pLMA,pSC,pHJ))
names(preds) = c("LMA","SC","HJ")
winner = apply(rbind(pLMA,pSC,pHJ),1,max)
guess = rep("unkn",dim(preds)[2])
guess[preds[,1]==winner] = names(preds)[1]
guess[preds[,2]==winner] = names(preds)[2]
guess[preds[,3]==winner] = names(preds)[3]

guess = factor(guess,levels=names(preds)[1:4])
preds$Book <- c(rep('Rose in Bloom',nrow(pLMA)),
    rep('The Jolly Corner',nrow(pSC)),
	rep('Wounds in the Rain',nrow(pHJ)))
predshow <- reshape(preds,direction='long',
    timevar='Prediction',v.names='Score',times=c('LMA','SC','HJ'),
    varying=list(w=c('LMA','SC','HJ')))
densityplot(~Score | Prediction + Book,data=predshow)
table(preds$Book,guess)

