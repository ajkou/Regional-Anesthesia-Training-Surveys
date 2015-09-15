```{r warning=FALSE, fig.width=7, fig.height=8, fig.path='Figs/'}
options(width=200)

setwd("C://Users//KOU//Desktop//iflow survey")
svpre <- read.table("surveypre.txt", header=T, sep="\t", strip.white=T)
svpre.1 <- cbind(1:nrow(svpre), svpre)


svpost <- read.table("surveypost.txt", header=T, sep="\t", strip.white=T)
svpost.1 <- cbind(1:nrow(svpost), svpost) 
#santize post names
mynames <- 
sub("/", "",
sub(",", "",
paste(
substr(svpost.1[,2], regexpr(",", svpost.1[,2]), nchar(as.character(svpost.1[,2]))),
substr(svpost.1[,2], rep(1, nrow(svpost.1)),regexpr(",", svpost.1[,2])-1 )
)
)
)
mynames.1 <- gsub("^\\s+|\\s+$", "", mynames)

svpost.1["ï..Name"] <- mynames.1
m1 <- merge(svpre.1, svpost.1, by.x = "Name", by.y = "ï..Name")
m1.1 <- m1[,c(1:4, 6:69, 75, 77:ncol(m1), 70:74)] 
m2 <- merge(svpre.1, svpost.1, by.x = "EmailAddress", by.y = "EmailAddress")
m2.1 <- m2[,c(3,2, 4:69, 75, 77:ncol(m2), 70:74)] 
combm <- rbind(m1.1, m2.1)
fdata <- combm[!duplicated(combm[,1]),]

#missing people from merge
#merge( fdata[c("1:nrow(svpost)", "Name")], svpost.1[1:2], by.x="1:nrow(svpost)", by.y="1:nrow(svpost)", all.y=T)
#masked
fdata <-  rbind(fdata, cbind(svpre.1[62,], svpost.1[55,])[,c(2,1, 3:4, 6:69, 75, 78:(ncol(svpre.1)+ncol(svpost.1)), 70:74)])
#masked no pre
#masked
fdata <-  rbind(fdata, cbind(svpre.1[32,], svpost.1[41,])[,c(2,1, 3:4, 6:69, 75, 78:(ncol(svpre.1)+ncol(svpost.1)), 70:74)])
#masked
fdata <-  rbind(fdata, cbind(svpre.1[59,], svpost.1[33,])[,c(2,1, 3:4, 6:69, 75, 78:(ncol(svpre.1)+ncol(svpost.1)), 70:74)])
#masked
fdata <-  rbind(fdata, cbind(svpre.1[70,], svpost.1[26,])[,c(2,1, 3:4, 6:69, 75, 78:(ncol(svpre.1)+ncol(svpost.1)), 70:74)])
#masked
fdata <-  rbind(fdata, cbind(svpre.1[9,], svpost.1[24,])[,c(2,1, 3:4, 6:69, 75, 78:(ncol(svpre.1)+ncol(svpost.1)), 70:74)])

#Decision to eliminate early form of survey and all related subjects August 2015 (n=21)
svpre.1 <- svpre.1[1:78,]
fdata <- fdata[fdata[10]==1000,]


#############Descriptive figures

# obstacle ratings
# Note: Obstacle is ranked opposite on workshop survey (ex. Always an obstacle (biggest obstacle)=1)
# Never an obstacle=1
# Rarely an obstacle=2
# Sometimes an obstacle=3
# Often an obstacle=4
# Always an obstacle=5

  par(mfrow=c(9,2), mar=c(2,0,1,0))
  lmethods.data <- sapply(fdata[,123:131], table)
  lmethods.data[[8]] <- c(lmethods.data[[8]][1:3], 0, lmethods.data[[8]][4])
  myorder <- order(sapply(fdata[,123:131], mean))
  lmethods.data.labels <- substr(names(lmethods.data), regexpr("nerualcat....", names(lmethods.data))+13, nchar(names(lmethods.data)))
  
  data.pre <- apply(svpre.1[,70:74], 2, as.character)
  data.pre <- replace(data.pre, data.pre=="1000", "NA")
  data.pre <- replace(data.pre, data.pre=="checked but not ranked", "X")
  data.pre <- replace(data.pre, data.pre=="crossed but not ranked", "X")
  data.pre.filter.data <- replace(data.pre, data.pre=="NA", "0") 
  data.pre.filter.sum <- apply(apply(data.pre.filter.data, 2, as.numeric), 1, sum)
  data.pre.filter <- (data.pre.filter.sum != 0 & data.pre.filter.sum != 15 & !is.na(data.pre.filter.sum))
  lmethods.data.pre <- apply(data.pre[data.pre.filter,], 2, table)
  lmethods.data.pre <- c(lmethods.data.pre[1:4], 0, lmethods.data.pre[5], 0,0,0)
  lmethods.data.pre[[6]] <- c(lmethods.data.pre[[6]][1], 0, lmethods.data.pre[[6]][2], 0, lmethods.data.pre[[6]][3:4])
  lmethods.data.pre[[2]] <- c(0, lmethods.data.pre[[2]])
  lmethods.data.pre[[1]] <- c(lmethods.data.pre[[1]][1:3], 0, 0, lmethods.data.pre[[1]][4])
  lmethods.data.pre[[4]] <- c(lmethods.data.pre[[4]][1:4], 0, lmethods.data.pre[[4]][5])
  lmethods.data.pre[[3]] <- c(lmethods.data.pre[[3]][1:4], 0, lmethods.data.pre[[3]][5])
  for (i in myorder) {
  	if (i==2) {
  		barplot(lmethods.data.pre[[i]], yaxt="n", xlim=c(-6,7), ylim=c(0,max(lmethods.data.pre[[i]])*1.2), main="Obstacle rating (pre, n=15)")
  	  text(-6, max(lmethods.data.pre[[i]])/2, lmethods.data.labels[i], pos=4)
  	  barplot(lmethods.data[[i]], yaxt="n", xlim=c(-6,7), ylim=c(0,max(lmethods.data[[i]])*1.2), main=paste("Obstacle rating (pre, n=", nrow(fdata), ")"))
  	  
  	} else {
  		barplot(lmethods.data.pre[[i]], yaxt="n", xlim=c(-6,7))
  	  if(sum(lmethods.data.pre[[i]])!=0) {text(-6, max(lmethods.data.pre[[i]])/2, lmethods.data.labels[i], pos=4)}
  	  barplot(lmethods.data[[i]], yaxt="n", xlim=c(-6,7))
  	}
  	text(-6, max(lmethods.data[[i]])/2, lmethods.data.labels[i], pos=4)
  }
  #Table of obstacles; pre; count table; converted to yes/no defined by a non-answer signalling "no", while any answer (checkmark or number) is defined as a "yes"
  data.pre <- apply(svpre.1[,70:74], 2, as.character)
  data.pre <- replace(data.pre, data.pre!="1000", 1)
  data.pre <- replace(data.pre, data.pre=="1000", 0)
  lmethods.data.pre <- apply(data.pre, 2, table)
  obstacle.table.pre <- NULL
  for (i in 1:ncol(lmethods.data.pre)) {
    if (sum(lmethods.data.pre[,i])==0) {
      obstacle.table.pre <- rbind(obstacle.table.pre, rep(0,6))
    }
    else {
      obstacle.table.pre <- rbind(obstacle.table.pre, lmethods.data.pre[,i])
    }
  }
  colnames(obstacle.table.pre) <- c("No/NA", "Yes")
  rownames(obstacle.table.pre) <- substr(colnames(lmethods.data.pre), regexpr("PNBrankobstacles.", colnames(lmethods.data.pre))+17, nchar(colnames(lmethods.data.pre)))
  obstacle.table.pre
  
  #Table of obstacles; post; count table; Post answers cannot be converted to "yes" or "no" by the same motif
  obstacle.table.post <- NULL
  for (i in 1:length(lmethods.data)) {obstacle.table.post <- rbind(obstacle.table.post, lmethods.data[[i]])}
  rownames(obstacle.table.post) <- lmethods.data.labels
  obstacle.table.post
  

```{r warning=FALSE, fig.width=3, fig.height=3, fig.path='Figs/'}
#Misc %metrics
  par(mfrow=c(3,1), mar=c(2,0,1,0))
  data.1 <- sapply(fdata[,120:122], as.character)
  data.1[,1] <- replace(data.1[,1], data.1[,1]=="1000", NA)
  data.1[,1] <- replace(data.1[,1], data.1[,1]=="99%", 99)
  data.1[,1] <- replace(data.1[,1], data.1[,1]=="80+", 80)
  
  levels(data.1[,1])[3] <- NA 
  levels(data.1[,1])[13] <- NA 
  lmethods.data <- apply(data.1, 2, table)
  lmethods.data[[1]] <- lmethods.data[[1]][c(1, 3:11, 2)]
  data.1 <- apply(data.1, 2,as.numeric)
  hist(data.1[,1], breaks=20, main="Av block success rate")
  hist(data.1[,2], breaks=10, main="vascular puncture")
  hist(data.1[,3], breaks=5, main="cardiac arrest")

  
```{r warning=FALSE, fig.width=7, fig.height=8, fig.path='Figs/'}
#post average numbers and %cath
  par(mfrow=c(11,3), mar=c(2,0,1,0))
  data.1 <- sapply(fdata[,76:97], as.character)
  data.1[,1:ncol(data.1)] <- replace(data.1[1:length(data.1)], data.1[1:length(data.1)]=="1000", "NA")
  data.1[,1:ncol(data.1)] <- replace(data.1[1:length(data.1)], data.1[1:length(data.1)]=="100%", "100")
  data.1 <- apply(data.1, 2, as.numeric)
  lmethods.data <- apply(data.1, 2, table, exclude=c("1000", "100%"))
  nncol <- seq(1,ncol(fdata[,76:97]), by=2) 
  lmethods.data.labels <- substr(names(lmethods.data[nncol]), regexpr("theaveragenum....", names(lmethods.data)[nncol])+17, regexpr("..Averageof", names(lmethods.data)[nncol]))
  myorder <- order(apply(data.1[,nncol],2, mean, na.rm=T))
  i = 11
  for (i in myorder) {
    plot(0, main="", yaxt="n", xaxt="n", xlim=c(-1,0), bty="n")
    text(-0.90, 0.3, lmethods.data.labels[i], pos=4)
    if (max(data.1[,i*2-1], na.rm=T)==0) {
      hist(data.1[,i*2-1]+0.2, main="", yaxt="n", breaks=10, xlim=c(0,20))
    } else {
      hist(data.1[,i*2-1], main="", yaxt="n", breaks=10, xlim=c(0,20))
    }
    if (lmethods.data.labels[i]=="Paravertebral"){text(10, max(lmethods.data[[i*2-1]])*0.80, "Single Injection", pos=4) }
    hist(data.1[,i*2]+0.2, main="", yaxt="n", breaks=10, xlim=c(0,25))
    if (lmethods.data.labels[i]=="Paravertebral"){text(10, max(lmethods.data[[i*2-1]])*0.80, "Catheter", pos=4) }
  }

  
# event encounters
    par(mfrow=c(1,1), mar=c(5,2,4,2))
    ee.myFunctionall <- function(myTable){return(c(myTable["1"],myTable["1000"],myTable["2"]))}
    lmethods.table <- sapply(fdata[,60:66], table)
    lmethods.table <- sapply(lmethods.table, ee.myFunctionall)
    lmethods.table.1 <- replace(lmethods.table, is.na(lmethods.table), 0)[,order(lmethods.table[1,], decreasing=T)]
    barplot( lmethods.table.1, main="events prior to workshop (pre)", horiz=T, yaxt="n", xaxt="n", xlim=c(-70,70), ylim=c(0,9), legend=c("yes","NA", "no"))
    lmethods.data.labels <- substr(colnames(lmethods.table), regexpr("RegionalAnesthesia.", colnames(lmethods.table))+19, nchar(colnames(lmethods.table)))
    text(rep(-70,length(lmethods.data.labels)), seq(1,1.18*length(lmethods.data.labels), by=1.18), lmethods.data.labels, pos=4, cex=0.75)
    text(seq(0,60, by=20), rep(0,4), seq(0,60, by=20))
  

```{r warning=FALSE, fig.width=10, fig.height=8, fig.path='Figs/'}  
#perception of teching effectiveness
  par(mfrow=c(11,3), mar=c(2,0,1,0))
  x=23
  layout(matrix(c(1,2,x, 3,4,x, 5,6,x, 7,8,x, 9,10,x, 11,12,x, 13,14,x, 15,16,x, 17,18,x, 19,20,x, 21,22,x), 11, 3, byrow = TRUE))
  
  data.1_pre <- sapply(svpre.1[,52:60], as.character)
  data.1_pre <- replace(data.1_pre, data.1_pre==1000, "NA")
  data.1_pre <- replace(data.1_pre, data.1_pre=="?", "NA")
  lmethods.data_pre <- apply(data.1_pre, 2, table)
  lmethods.data_pre <- c(lmethods.data_pre[c(1:7)], 0, lmethods.data_pre[9:8])
  lmethods.data.labels_pre <- substr(names(lmethods.data_pre), regexpr("teachingmethods.", names(lmethods.data_pre))+16, nchar(names(lmethods.data_pre)))
  
  lmethods.data_post <- sapply(fdata[,132:140], table)
  myfunction <- function(myinput) {return (c(myinput,0))}
  lmethods.data_post <- lapply(lmethods.data_post, myfunction)
  
  lmethods.data_post[[2]] <- c(0,0, lmethods.data_post[[2]])
  lmethods.data_post[[4]] <- c(0, lmethods.data_post[[4]])
  lmethods.data_post[[5]] <- c(0, lmethods.data_post[[5]])
  lmethods.data_post[[7]] <- c(0, lmethods.data_post[[7]])
  lmethods.data_post <- c(lmethods.data_post, 0)[c(1:7,9,8,10)]
  lmethods.data.labels_post <- substr(names(lmethods.data_post), regexpr("effectiv....", names(lmethods.data_post))+12, nchar(names(lmethods.data_post)))
  
  #myorder <- order(sapply(lapply(lapply(data.1_pre, as.character), as.numeric), mean, na.rm=T)) #Pre sort
  #myorder <- order(sapply(fdata[,132:140], mean)) #Post sort 
  myorder <-order(sapply(fdata[,141:149], table)[1,], decreasing=T) #Used since workshop
  
  plot(0, yaxt="n", xaxt="n", xlim=c(-1,0), bty="n", main="Teaching effectiveness (pre)")
  plot(0, yaxt="n", xaxt="n", xlim=c(-1,0), bty="n", main="Teaching effectiveness (post)")
  i = 9
  for (i in c(myorder,10)) {
    if (sum(lmethods.data_pre[[i]])==0) {
      plot(0, yaxt="n", xaxt="n", xlim=c(-1,0), bty="n")
    } else {
      barplot(lmethods.data_pre[[i]], yaxt="n", xlim=c(-6,8))
      text(-6, max(lmethods.data_pre[[i]])/2, lmethods.data.labels_pre[i], pos=4)
    }
    if (sum(lmethods.data_post[[i]])==0) {
      plot(0, yaxt="n", xaxt="n", xlim=c(-1,0), bty="n")
    } else {
      barplot(lmethods.data_post[[i]], yaxt="n", xlim=c(-6,8))
      text(-6, max(lmethods.data_post[[i]])/2, lmethods.data.labels_post[i], pos=4)
    }
  }
  lmethods.data <- sapply(fdata[,141:149], table)[,myorder]
  lmethods.data <- lmethods.data[,rev(c(1:7,9,8))]
  barplot(lmethods.data, main="Learning methods since workshop (sort order)", horiz=T, yaxt="n", xaxt="n", xlim=c(-80,80), ylim=c(0,11.5), legend=c("yes","no"))
  lmethods.data.labels <- substr(colnames(lmethods.data), regexpr("blocks..", colnames(lmethods.data))+8, nchar(colnames(lmethods.data)))
  text(rep(-70,length(lmethods.data.labels)), seq(1,11, by=1.18), lmethods.data.labels, pos=4)
  text(seq(0,60, by=20), rep(11,4), seq(0,60, by=20))
  
  
# technique comfort and preference; pre survey
#   Not performed=0
#   US-guided=1
#   Nerve stimulation=2
#   US-guided along with nerve stimulation=3
#   other=4
#   Performed=5
  
  par(mfrow=c(10,3), mar=c(2,0,1,0))
  layout(matrix(c(1,1,1,2,3,3, 4,4,4,5,6,6, 7,7,7,8,9,9, 10,10,10,11,12,12, 13,13,13,14,15,15, 16,16,16,17,18,18, 19,19,19,20,21,21, 22,22,22,23,24,24, 25,25,25,26,27,27, 28,28,28,29,30,30), 10, 6, byrow = TRUE))
  data.1 <- sapply(svpre.1[,21:50], as.character)
  data.1 <- replace(data.1, data.1=="1000", "NA")
  data.1 <- replace(data.1, is.na(data.1), "NA")
  data.1 <- replace(data.1, data.1=="100%", "100")
  data.1 <- replace(data.1, data.1=="1, 0(catheter)", "1,0")
  lmethods.data <- apply(data.1, 2,table, exclude=c("1000", "100%"))
  lmethods.data.labels <- substr(names(lmethods.data[seq(1,30, by=3)]), regexpr("technique.", names(lmethods.data)[seq(1,30, by=3)])+10, nchar(names(lmethods.data)[seq(1,30, by=3)]))
  myorder <- order(apply(apply(data.1[,seq(3,24, by=3)],2,as.numeric),2, mean, na.rm=T))
  for (i in c(myorder,9,10)) {
    barplot(lmethods.data[[i*3-2]], yaxt="n", xlim=c(-4,11))
    text(-4, max(lmethods.data[[i*3-2]])/2, lmethods.data.labels[i], pos=4)
    if(i==3){
      text(4, max(lmethods.data[[i*3-2]]/1.2), "preference", pos=4)
    }
    if (all(data.1[,i*3-1]=="NA")) {
      hist(c(0), main="", yaxt="n", breaks=10, xlim=c(0,100))
    } else {
      hist(as.numeric(data.1[,i*3-1]), main="", yaxt="n", breaks=10, xlim=c(0,100))
    }
    if(i==3){
      text(5, 0.5, "%cath histogram (old surv)", pos=4)
    }
    barplot(lmethods.data[[i*3]], yaxt="n", xlim=c(0,8))
    if(i==3){
      text(2, max(lmethods.data[[i*3]]/1.2), "comfort rating (sort order)", pos=4)
    }
  }
  
  
#################By Pre/post comparison
  
#technique comfort no preference
#   Not comfortable=1
#   Rarely comfortable=2
#   Somewhat comfortable=3
#   Comfortable=4
#   Extremely comfortable=5
  
  par(mfrow=c(13,2), mar=c(2,0,1,0))
  data.1 <- sapply(svpre.1[,21:50], as.character)
  data.1[,1:ncol(data.1)] <- replace(data.1[1:length(data.1)], data.1[1:length(data.1)]=="1000", "NA")
  data.1[,1:ncol(data.1)] <- replace(data.1[1:length(data.1)], data.1[1:length(data.1)]=="100%", "100")
  lmethods.data_pre <- apply(data.1, 2, table, exclude=c("1000", "100%"))
  lmethods.data_pre <- c(lmethods.data_pre[1:27], rep(0,6), lmethods.data_pre[28:30])
  nncol <- seq(1,34, by=3)
  lmethods.data.labels_pre <- substr(names(lmethods.data_pre[nncol]), regexpr("technique.", names(lmethods.data_pre)[nncol])+10, nchar(names(lmethods.data_pre)[nncol ]))
  
  lmethods.data_post <- apply(fdata[,109:119], 2, table)
  lmethods.data_post <- cbind(lmethods.data_post[,c(1:4,6,8,9,10)], 0, lmethods.data_post[,c(5,7,11)])
  lmethods.data.labels_post <- substr(colnames(lmethods.data_post), regexpr("ceatte....", colnames(lmethods.data_post))+10, nchar(colnames(lmethods.data_post)))
  
  myorder <- order(sapply(c(fdata[,108+c(1:4,6,8,9,10)], 0, fdata[,108+c(5,7,11)]), mean)) #Post order
  #myorder <- order(apply(apply(data.1[,seq(1,30, by=3)],2,as.numeric),2, mean, na.rm=T)) #Pre order
  
  plot(0, yaxt="n", xaxt="n", xlim=c(-1,0), bty="n", main="Technique Comfort (pre)")
  plot(0, yaxt="n", xaxt="n", xlim=c(-1,0), bty="n", main="Technique Comfort (post) (sort order)")
  for (i in myorder ) {
    if (sum(lmethods.data_pre[[i*3]])==0) {
      plot(0, main="", yaxt="n", xaxt="n", xlim=c(-1,0), bty="n")
    }else {	
      barplot(lmethods.data_pre[[i*3]], yaxt="n", xlim=c(-4,8))
      text(-4, max(lmethods.data_pre[[i*3]])/2, lmethods.data.labels_pre[i], pos=4)
    }
    if (sum(lmethods.data_post[,i])==0) {
      plot(0, main="", yaxt="n", xaxt="n", xlim=c(-1,0), bty="n")
    }else {	
      barplot(lmethods.data_post[,i], yaxt="n", xlim=c(-4,8))
    }	
    text(-4, max(lmethods.data_post[,i])/2, lmethods.data.labels_post[i], pos=4)
  }
  
  
#technique preference no comfort
  #   Not performed=0
  #   US-guided=1
  #   Nerve stimulation=2
  #   US-guided along with nerve stimulation=3
  #   other=4
  #   Performed=5
  par(mfrow=c(13,2), mar=c(2,0,1,0))
  layout(matrix(c(1,1,2, 3,3,4, 5,5,6, 7,7,8, 9,9,10, 11,11,12, 13,13,14, 15,15,16, 17,17,18, 19,19,20, 21,21,22, 23,23,24, 25,25,26), 13, 3, byrow = TRUE))
  
  data.1 <- sapply(svpre.1[,21:50], as.character)
  data.1[,1:ncol(data.1)] <- replace(data.1[1:length(data.1)], data.1[1:length(data.1)]=="1000", "NA")
  data.1[,1:ncol(data.1)] <- replace(data.1[1:length(data.1)], data.1[1:length(data.1)]=="100%", "100")
  lmethods.data_pre <- apply(data.1, 2, table, exclude=c("1000", "100%", NA))
  lmethods.data_pre <- c(lmethods.data_pre[1:27], rep(0,6), lmethods.data_pre[28:30])
  nncol <- seq(1,34, by=3)
  lmethods.data.labels_pre <- substr(names(lmethods.data_pre[nncol]), regexpr("technique.", names(lmethods.data_pre)[nncol])+10, nchar(names(lmethods.data_pre)[nncol ]))
  
  lmethods.data_post <- apply(fdata[,98:108], 2, table)
  lmethods.data_post <- c(lmethods.data_post[c(1:4,6,8,9,10)], 0, lmethods.data_post[c(5,7,11)])
  lmethods.data.labels_post <- substr(names(lmethods.data_post), regexpr("attend....", names(lmethods.data_post))+10, nchar(names(lmethods.data_post)))
  
  myorder <- order(sapply(c(fdata[,97+c(1:4,6,8,9,10)], 0, fdata[,97+c(5,7,11)]), mean)) #Post order
  #myorder <- order(apply(apply(data.1[,seq(1,30, by=3)],2,as.numeric),2, mean, na.rm=T)) #Pre order
  
  plot(0, yaxt="n", xaxt="n", xlim=c(-1,0), bty="n", main="Technique preference (pre)")
  plot(0, yaxt="n", xaxt="n", xlim=c(-1,0), bty="n", main="Technique preference (post) (sort order)")
  for (i in myorder) {
    
    if (sum(lmethods.data_pre[[i*3-2]])==0) {
      plot(0, main="", yaxt="n", xaxt="n", xlim=c(-1,0), bty="n")
    }else {	
      barplot(lmethods.data_pre[[i*3-2]], yaxt="n", xlim=c(-3,11))
      text(-3, max(lmethods.data_pre[[i]])/3, lmethods.data.labels_pre[i], pos=4)
    }
    if (sum(lmethods.data_post[[i]])==0) {
      plot(0, main="", yaxt="n", xaxt="n", xlim=c(-1,0), bty="n")
    }else {	
      barplot(lmethods.data_post[[i]], yaxt="n", xlim=c(-3,4) )
      text(-3, max(lmethods.data_post[[i]])/3, lmethods.data.labels_post[i], pos=4)
    }	
  }

  
```{r warning=FALSE, fig.width=8, fig.height=8, fig.path='Figs/'}
##############Demographic data regression
  library(car)
  library(MASS)
  #factors
  data.1 <- sapply(fdata[,c(6:10,12:13,68)-1], as.character)
  data.1 <- replace(data.1, data.1=="1000", "NA")
  data.1 <- replace(data.1, data.1=="<10", "10")
  data.1 <- replace(data.1, data.1=="100+", "100")
  data.1 <- replace(data.1, data.1=="<20", "20")
  data.1 <- replace(data.1, data.1==">20", "20")
  data.1 <- replace(data.1, data.1=="1,2", "3")
  data.1 <- replace(data.1, data.1=="1, 2", "3")
  data.1 <- replace(data.1, data.1=="2000", "3")
  data.1 <- replace(data.1, data.1=="3000", "7")
  data.1 <- replace(data.1, data.1=="4000", "11")
  data.1 <- apply(data.1, 2, as.numeric)
  data.1[,4] <- replace(data.1[,4], data.1[,4]=="3", "1")
  data.1[,4] <- replace(data.1[,4], data.1[,4]=="4", "2")
  data.1 <- apply(data.1, 2, as.numeric)
  #outcomes
  data.2 <- fdata[,76:97]
  data.2 <- replace(data.2, data.2=="2-Feb", NA)
  data.2 <- apply(data.2, 2, as.numeric)
  
  a <- data.1[,1] #Age
  b <- data.1[,2] #Gender
  c1 <- data.1[,3] #Anesth experience postresidency
  d <- data.1[,4] #training in USPNB during residency
  e <- data.1[,5] #Hospital Affiliation
  e <- replace(e, e==2, "Non-Teaching")
  e <- replace(e, e==1, "Teaching")
  e <- replace(e, e==3, "Both")
  e.1 <- as.factor(data.1[,5])
  f <- data.1[,6] #PNB per month
  g <- data.1[,7] # Highest PNB rate in month
  h <- data.1[,8] # Lipid rescue pack
  i <- apply(data.2[,seq(1,22, by=2)],1, sum) #SI
  j <- apply(data.2[,seq(2,22, by=2)],1, sum) #Caths
  k <- (i+j)-f #All PNB change
  ddiff <- as.numeric(as.Date(fdata[,3], format="%m/%d/%Y") - as.Date(fdata[,71], format="%m/%d/%Y"))
  #Scattermatrix of most related variables to outcomes SI and Cont
  scatterplotMatrix(~i+j+f+g+c1+d+ddiff|e, var.labels=c("numSI (post)", "numCath (post)", "Av PNB/Month (pre)", "Highest PNB/Month (pre)", "Experience(postres)", "Training(postres)", "Time Elapsed (days)"))
  
  #SI (Single Injection) linear regression
  #summary(lm(i~c1+a+d+e.1+h+f+g+h, na.action=na.omit)) #full model
  mydata <- cbind(i,c1,a,b,d,e.1,h,f,g,h,ddiff)
  mydata <- apply(apply(mydata,2, as.character), 2, as.numeric)
  mydata <- data.frame(na.omit(mydata))
  mydata$e.1 <- as.factor(mydata$e.1)
  summary(lm(i~c1+a+b+d+e.1+h+f+g+h+ddiff, data=mydata))
  fit <- lm(i~c1+a+b+d+e.1++f+g+h+ddiff, data=mydata)
  stepAIC(fit, direciton="both")
  summary(lm(i~e.1*ddiff+f, data=mydata)) #final model
  #The reduced model shows that #PNB/Month after education is determined by #PNB/Month before attending. Time elapsed is significant, indicating a development time to build a PNB program. 
  scatterplotMatrix(~i+f+g+ddiff|e, var.labels=c("numSI (post)", "Av PNB/Month (pre)", "Highest PNB/Month (pre)", "Time Elapsed (days)"))
  
  #Continuous block linear regression
  #summary(lm(j~c1+a+d+e.1+h+f+g+h, na.action=na.omit)) #full model
  mydata <- cbind(j,c1,a,b,d,e.1,h,f,g,h,ddiff)
  mydata <- apply(apply(mydata,2, as.character), 2, as.numeric)
  mydata <- data.frame(na.omit(mydata))
  mydata$e.1 <- as.factor(mydata$e.1)
  summary(lm(j~c1+a+b+d+e.1+f+g+h+ddiff, data=mydata))
  fit <- lm(j~c1+a+b+d+e.1+f+g+h+ddiff, data=mydata)
  stepAIC(fit, direciton="both")
  summary(lm(j~e.1+ddiff+d, data=mydata)) #final model
  #Continuous blocks being rarely part of the attendees practice, are affected primarily by time elapsed and by affiliation (teaching/non-teaching). The effect of time elapsed decreases the effect of training during residency variable, which has a slight (but non-sig) effect.
  scatterplotMatrix(~j+d+ddiff|e, var.labels=c("numCath (post)", "Training(postres)", "Time Elapsed (days)"))
  
  #change in block linear regression
  mydata <- cbind(j,c1,a,b,d,e.1,h,f,g,h,ddiff,k)
  mydata <- apply(apply(mydata,2, as.character), 2, as.numeric)
  mydata <- data.frame(na.omit(mydata))
  mydata$e.1 <- as.factor(mydata$e.1)
  summary(lm(k~c1+a+b+d+e.1+h+ddiff, data=mydata))
  fit <- lm(k~c1+a+b+d+e.1+h+ddiff, data=mydata)
  stepAIC(fit, direciton="both")
  summary(lm(k~e.1*ddiff, data=mydata)) #final model
  #The change in practice before to after is also related to the time elapsed and by practice affiliation.
  scatterplotMatrix(~k+ddiff|e, var.labels=c("numCath (post)", "Time Elapsed (days)"))
  
  
  ##########Manuscript Figures and tables
  #US technique preference
  data.1 <- sapply(svpre.1[,21:50], as.character)
  data.1[,1:ncol(data.1)] <- replace(data.1[1:length(data.1)], data.1[1:length(data.1)]=="1000", "NA")
  data.1[,1:ncol(data.1)] <- replace(data.1[1:length(data.1)], data.1[1:length(data.1)]=="100%", "100")
  lmethods.data_pre <- apply(data.1, 2, table, exclude=c("1000", "100%", NA))
  lmethods.data_pre <- c(lmethods.data_pre[1:27], rep(0,6), lmethods.data_pre[28:30])
  nncol <- seq(1,34, by=3)
  lmethods.data.labels_pre <- substr(names(lmethods.data_pre[nncol]), regexpr("technique.", names(lmethods.data_pre)[nncol])+10, nchar(names(lmethods.data_pre)[nncol ]))
  
  lmethods.data_post <- apply(fdata[,98:108], 2, table)
  lmethods.data_post <- c(lmethods.data_post[c(1:4,6,8,9,10)], 0, lmethods.data_post[c(5,7,11)])
  lmethods.data.labels_post <- substr(names(lmethods.data_post), regexpr("attend....", names(lmethods.data_post))+10, nchar(names(lmethods.data_post)))
  
  #Baseline and post-training use of USG compared by proportion test, which tests for equal proportions of USG users before and after. npre =78 (not 99), npost=46 (not 59), p<0.05 means that proportions are significantly unequal.
  myFunction <- function(myTable){return(myTable["1"])}
  baseline <- sapply(lmethods.data_pre[seq(1,50-21+6,by=3)], myFunction)
  postTraining <- sapply(lmethods.data_post, myFunction)
  baseline.prop <- sapply(lmethods.data_pre[seq(1,50-21+6,by=3)], sum)
  postTraining.prop <- sapply(lmethods.data_post, sum)
  
  result.p <- NULL
  result.props <- NULL
  for (i in 1:length(baseline.prop)) {
    if (baseline.prop[i]==0 |  postTraining.prop[i]==0 | is.na(baseline[i])){
      result.p <-c(result.p, NA)
      result.props <-c(result.props, NA)
    }else {
      mytest <- prop.test(c(baseline[i], postTraining[i]),c(baseline.prop[i],postTraining.prop[i]))
      result.p <-c(result.p, mytest[3])
      result.props <-c(result.props, paste(round(mytest[[4]][1], 2), "vs", round(mytest[[4]][2],2)))
    }
  }
  result <- cbind(baseline, postTraining, result.props, sapply(result.p, round, digits=6))
  colnames(result)[4] <- "prop.test"
  rownames(result) <- lmethods.data.labels_post
  rownames(result)[9] <- lmethods.data.labels_pre[9]
  result
  
  
  #Teching method effectiveness
  #Teaching effectiveness change is a 1-5 Likert scale. The 2 acceptable ways that I know how to compare these score are by 5 group chi-squared test or with a paired t-test. The chi quare test compares the scores by subgroup, while the t-test takes into account the change expressed by each respondent. Both test methods were similar in this sample but do not always find the same conclusion.
  data.1_pre <- sapply(svpre.1[,52:60], as.character)
  data.1_pre <- replace(data.1_pre, data.1_pre==1000, "NA")
  data.1_pre <- replace(data.1_pre, data.1_pre=="?", "NA")
  lmethods.data_pre <- apply(data.1_pre, 2, table)
  lmethods.data_pre <- c(lmethods.data_pre[c(1:7,9)], 0, lmethods.data_pre[8])
  names(lmethods.data_pre)[10] <- colnames(data.1_pre)[8]
  lmethods.data.labels_pre <- substr(names(lmethods.data_pre), regexpr("teachingmethods.", names(lmethods.data_pre))+16, nchar(names(lmethods.data_pre)))
  
  lmethods.data_post <- sapply(fdata[,132:140], table)
  lmethods.data_post[[2]] <- c(0,0, lmethods.data_post[[2]])
  lmethods.data_post[[4]] <- c(0, lmethods.data_post[[4]])
  lmethods.data_post[[5]] <- c(0, lmethods.data_post[[5]])
  lmethods.data_post[[7]] <- c(0, lmethods.data_post[[7]])
  lmethods.data_post <- c(lmethods.data_post, 0)
  lmethods.data.labels_post <- substr(names(lmethods.data_post), regexpr("ofeffectiv....", names(lmethods.data_post))+14, nchar(names(lmethods.data_post)))
  
  data.1_pre.ttest <- apply(cbind(fdata[,c(51:57,59)], 0, fdata[,58]), 2, as.character)
  data.1_pre.ttest <- replace(data.1_pre.ttest, data.1_pre.ttest==1000, "NA")
  data.1_pre.ttest <- replace(data.1_pre.ttest, data.1_pre.ttest=="?", "NA")
  data.1_pre.ttest <-apply(data.1_pre.ttest, 2, as.numeric)
  data.1_post.ttest <- cbind(fdata[,132:140], 0)
  
  baseline <- apply(apply(data.1_pre, 2, as.numeric),2,median, na.rm=T)
  baseline <- c(baseline[c(1:7,9)], 0, baseline[8])
  baseline.summary <- apply(apply(data.1_pre, 2, as.numeric),2,summary, na.rm=T)
  baseline.summary <- cbind(baseline.summary[,c(1:7,9)], 0, baseline.summary[,8])  
  postTraining <- c(apply(fdata[,132:140], 2, median, na.rm=T), 0)
  postTraining.summary <- cbind(apply(apply(fdata[,132:140],2,as.numeric), 2, summary, na.rm=T), 0)
  
  result.p <- NULL
  result.baseline <- NULL
  result.postTraining <-NULL
  result.pairedt <- NULL
  for (i in 1:length(lmethods.data_pre)) {
    if (sum(lmethods.data_pre[[i]][names(lmethods.data_pre[[i]])!="NA"])==0 ||  sum(lmethods.data_post[[i]])==0 ){
      result.p <- c(result.p, NA)
      result.baseline <- c(result.baseline, NA)
      result.postTraining <- c(result.postTraining, NA)
      result.pairedt <- c(result.pairedt, NA)
    }else {
      result.p <-c(result.p, chisq.test(rbind(lmethods.data_pre[[i]][names(lmethods.data_pre[[i]])!="NA"], lmethods.data_post[[i]]))[3])
      result.baseline <- c(result.baseline, paste(baseline[i],"[",baseline.summary[2,i],"-",baseline.summary[5,i],"]", sep=""))
      result.postTraining <- c(result.postTraining, paste(postTraining[i],"[",postTraining.summary[2,i],"-",postTraining.summary[5,i],"]", sep=""))
      result.pairedt <- c(result.pairedt, t.test(data.1_pre.ttest[,i], data.1_post.ttest[,i], paired=T)[3] )
    }
  }
result <- cbind(result.baseline, result.postTraining, sapply(result.p, round, digits=4), sapply(result.pairedt, round, digits=4))
rownames(result) <- lmethods.data.labels_pre
rownames(result)[9] <- lmethods.data.labels_post[9]
colnames(result) <- c("baseline", "post", "chisq p", "test p")
result


#Pre-Post survey performance comparisons
data.1 <- sapply(fdata[,c(6:10,12:13,68)-1], as.character)
data.1 <- replace(data.1, data.1=="1000", "NA")
data.1 <- replace(data.1, data.1=="<10", "10")
data.1 <- replace(data.1, data.1=="100+", "100")
data.1 <- replace(data.1, data.1=="<20", "20")
data.1 <- replace(data.1, data.1==">20", "20")
data.1 <- replace(data.1, data.1=="1,2", "3")
data.1 <- replace(data.1, data.1=="1, 2", "3")
data.1 <- replace(data.1, data.1=="2000", "3")
data.1 <- replace(data.1, data.1=="3000", "7")
data.1 <- replace(data.1, data.1=="4000", "11")
data.1 <- apply(data.1, 2, as.numeric)
data.1[,4] <- replace(data.1[,4], data.1[,4]=="3", "1")
data.1[,4] <- replace(data.1[,4], data.1[,4]=="4", "2")
data.1 <- apply(data.1, 2, as.numeric)
#outcomes
data.2 <- fdata[,76:97]
data.2 <- replace(data.2, data.2=="2-Feb", NA)
data.2 <- apply(data.2, 2, as.numeric)

f <- data.1[,6] #PNB per month
i <- apply(data.2[,seq(1,22, by=2)],1, sum) #SI
j <- apply(data.2[,seq(2,22, by=2)],1, sum) #Caths

#This t-test of the performance number before (f) versus the performance number afterwards (i+j) indicates that there was a difference in performance. This is not only attributable to the seminar, but also all colinear factors related to the period between measurements like time, additional training, or varying combinations. Methods like multiple regression can explain the interplay between these factors. 
t.test(f, i+j, na.rm=T, paired=T)

# Bargraphs showing before and after on the same scale; bar labels show increase in practice for each subject
par(mfrow=c(2,1), mar=c(1,2,2,1))
barplot(f, ylim=c(0,170), main="Pre #PNB")
barplot(rbind(i,j), ylim=c(0,170), main="Post #SI + #Cath")
mylabels <- round((i+j)-f,0)
mylabels[mylabels>0 & !is.na(mylabels)] <- paste("+", mylabels[mylabels>0 & !is.na(mylabels)], sep="")
text(seq(0.75,(1.2*46),by=1.2), i+j+5, mylabels, cex=0.7)
#plot(rep(1:2, each=46), c(f,i+j))
#for (myi in 1:46){lines(1:2,  c(f[myi],i[myi]+j[myi]))}



#Does retention time affect change in Teaching Rating?
#The first question is, for which teaching rating?
data.1_pre <- sapply(fdata[,51:59], as.character)
data.1_pre <- replace(data.1_pre, data.1_pre==1000, NA)
data.1_pre <- replace(data.1_pre, data.1_pre=="?", NA)
data.1_pre <- cbind(data.1_pre[,c(1:7,9)], NA, data.1_pre[,8]) #Pre
data._post <- cbind(sapply(fdata[,132:140], as.character),NA) #Post
TEdiff <- apply(data._post,2,as.numeric) - apply(data.1_pre,2,as.numeric)
TEdiff <- replace(TEdiff, is.na(TEdiff), 0)
ddiff <- as.numeric(as.Date(fdata[,3], format="%m/%d/%Y") - as.Date(fdata[,71], format="%m/%d/%Y"))
lmethods.data.labels_pre <- substr(colnames(data.1_pre), regexpr("teachingmethods.", colnames(data.1_pre))+16, nchar(colnames(data.1_pre)))

par(mfrow=c(5,2), mar=c(4,2,1,1))
#dotplot shows the relationship between change in score and retention time
apply(TEdiff, 2, plot, ddiff)
#correlation between change in score and retention time
cor.list <- apply(TEdiff, 2, cor, ddiff)
names(cor.list) <- NULL
cbind(lmethods.data.labels_pre, cor.list)