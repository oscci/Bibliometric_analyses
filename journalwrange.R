


#files created from Psychology2019
#There are all journals found on WOS search for topic 'Psychology' with at least 4 hits for 2019.
colnames(journals)[6:7]<-c('editor','publisher')
j1 <- read.csv('psychology2019_1_50.csv',stringsAsFactors = F)
j2 <- read.csv('psychology2019_51_100.csv',stringsAsFactors = F)
j3 <- read.csv('psychology2019_101_200.csv',stringsAsFactors = F)
j4 <- read.csv('psychology2019_201_300.csv',stringsAsFactors = F)
j5 <- read.csv('psychology2019_301_400.csv',stringsAsFactors = F)
j6 <- read.csv('psychology2019_401_529.csv',stringsAsFactors = F)

j1$Journal<-toupper(j1$Journal)
j2$Journal<-toupper(j2$Journal)
j3$Journal<-toupper(j3$Journal)
j4$Journal<-toupper(j4$Journal)
j5$Journal<-toupper(j5$Journal)
j6$Journal<-toupper(j6$Journal)

colnames(j2)[6:7]<-c('editor','publisher')
colnames(j5)[6:7]<-c('editor','publisher')
j6$editor<-NA
j6$publisher<-NA

allj<-rbind(j1,j2,j3,j4,j5,j6)
allj$sourcelist <- 'psych2019'

journals <- read.csv('taylor_francis_1_304.csv',stringsAsFactors=F)
journals$Journal<-toupper(journals$Journal)
journals$editor <- NA
journals$publisher <- 'TF'
journals$sourcelist <- 'TF'

j7<-read.csv('elsevierlist_out.csv',stringsAsFactors = F)
j7$editor<-NA
j7$publisher<-'Elsevier'
j7$sourcelist <- 'Elsevier'
j7$Journal<-toupper(j7$Journal)

j8<-read.csv('sage_psychology_out.csv',stringsAsFactors = F)
j8$editor<-NA
j8$publisher<-'Sage'
j8$sourcelist <- 'Sage'
j8$Journal<-toupper(j8$Journal)

j9<-read.csv('springerlist_out.csv',stringsAsFactors = F)
j9$editor<-NA
j9$publisher<-'Springer'
j9$sourcelist <- 'Springer'
j9$Journal<-toupper(j9$Journal)

j10<-read.csv('wiley_allcounts.csv',stringsAsFactors = F)
j10$editor<-NA
j10$publisher<-'Wiley'
j10$sourcelist <- 'Wiley'
j10$Journal<-toupper(j10$Journal)

j11<-read.csv('TFextra_1_21.csv',stringsAsFactors = F)
j11$editor<-NA
j11$publisher<-'TF'
j11$sourcelist <- 'TFextra'
j11$Journal<-toupper(j11$Journal)

allj<-rbind(allj,j7,j8,j9,j10,j11,journals)

write.csv(allj,'enormousj.csv',row.names=F)

#This will have many duplicates - cases where journal exists both in Psych2019 and in publisher list
#Where there are duplicates, we want to keep version with publisher, but N from Psych2019 (narrower date range to 2019)

allj$matchstring<-paste0(allj$Journal,allj$Author)
myrange<-which(allj$sourcelist=='psych2019')
range2<-(max(myrange)+1):nrow(allj)
for (i in myrange){
  w<-which(allj$matchstring[range2]==allj$matchstring[i])
  if(length(w)==1){
  allj$publisher[i]<-allj$publisher[w]
  if(!is.na(allj$editor[w])){
  allj$editor[i]<-allj$editor[w]
  }
  allj$matchstring[w]<-'x' #mark for deletion
  }
}
#remove duplicates
w<-which(allj$matchstring=='x')
allj<-allj[-w,]

#find journals with no entries in WOS
w0 <- which(allj$Narticle==0)
dropj<-allj[w0,]
tabzero <- table(dropj$publisher)
wosj <- allj[-w0,]
nobad<-which(wosj$Nau.paper==0)
nobadj<-wosj[nobad,]
tabnobad<-table(nobadj$publisher)
w15<-wosj[-nobad,]

#remove cochrane database
wc<-which(w15$Journal=='COCHRANE DATABASE OF SYSTEMATIC REVIEWS')

w15<-w15[-wc,]

w15<-w15[order(w15$Journal),]

#write.csv(w15,'mergedrough.csv',row.names=F)

#This file now needs editing by hand to remove duplicates, add editors, publishers etc

#Once that is done, we should be able to categorise into Journals with 15+ who don't have any editors among most prolific, 
#those that have EIC, and those that have AEs etc.

#Main interest is, for each pubilsher, what proportion of journals with entries on WOS have an EIC who is PEPIOP
#and what proportion has either EIC or AE who is PEPIOP

#We can get N without any 15+ from tabnobad
#We then need to categorise from w15 each journal as EIC (plus), AE only, none
#SHould be able to do that using table of w15$Journal by w15$editor by w15$publisher

#Have now made merged_checked.csv which has editors and publishers added.
#NB numbers may be slightly different depending on whether taken from original searches that included 2020 or current search which is for 2015-2019.
#SHould be possible to fix that with a re-search just on the journals in this 15+ set.

myset<- read.csv('merged_checked2.csv',stringsAsFactors = F)
w<-which(myset$publisher=='_other')
myset<-myset[-w,]
str(myset)

pubtab<-table(myset$publisher,myset$editor)
pubtab
prop.table(pubtab,1)

w<-which(myset$editor=='EIC')
eics<-myset[w,c(1,3,4,7)]

#created checkjournals to recompute data for these journals for correct year range
# checkj <- unique(eics$Journal)
# write.csv(checkj,'checkjournals.csv',row.names=F)


w<-which(myset$editor=='AE')
aes<-myset[w,c(1,3,4,7,8)]

# which(aes$sourcelist%in%c('Elsevier','Wiley','Springer','TF','TFextra'))
# checkj2 <- unique(aes$Journal)
# write.csv(checkj2,'checkjournals2.csv',row.names=F)

myset$Journal<-as.factor(myset$Journal)
myset$rankau <- 0
for (i in 1:length(levels(myset$Journal))){
  thisj <-levels(myset$Journal)[i]
  w<-which(myset$Journal==thisj)
  myset$rankau[w]<-length(w)-rank(myset$Nau.paper[w])+1
}
myset$rankau<-round(myset$rankau,0)
x<-which(myset$rankau>3)
myset$rankau[x]<-3

myset$rankau<-as.factor(myset$rankau)
levels(myset$rankau)<-c('1st or only','2nd','3rd or more')

table(myset$rankau,myset$editor) #rank will depend on how many in the set - if only 1 hyperprofilic, then have to be rank 1!

