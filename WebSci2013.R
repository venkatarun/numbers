library(igraph)
library(e1071)

wikipedia <- read.table("numbers-wikipedia.tsv.gz")
names(wikipedia) <- c("freq","number")

cc <- read.table("cc/cc.tsv.gz")
names(cc) <- c("freq","number")

wikipedia <- wikipedia[which(is.finite(wikipedia$number)),]
cc <- cc[which(is.finite(cc$number)),]

# test correlation frequency versus magnitude
cor(wikipedia$number,wikipedia$freq,method="pearson")
#[1] -8.955585e-06

# linearized correlation of positive numbers
cor(log(cc[which(cc$number > 0),]$number),
    log(cc[which(cc$number > 0),]$freq),method="pearson")
#[1] -0.1654824

# linearized correlation of the integers
cor(log(cc[which(cc$number %% 1 == 0 & cc$number > 0),]$number),
    log(cc[which(cc$number %% 1 == 0 & cc$number > 0),]$freq),method="pearson")
#[1] -0.2906848

cor(cc$number,cc$freq,method="pearson")
#[1] -7.694298e-05

# linearized correlation of positive numbers
cor(log(wikipedia[which(wikipedia$number > 0),]$number),
    log(wikipedia[which(wikipedia$number > 0),]$freq),method="pearson")
#[1] 0.06156126

# linearized correlation of the integers
cor(log(wikipedia[which(wikipedia$number %% 1 == 0 & wikipedia$number > 0),]$number),
    log(wikipedia[which(wikipedia$number %% 1 == 0 & wikipedia$number > 0),]$freq),method="pearson")
#[1] -0.1856765

wikipedia_freq_plot <- function(file,text=FALSE) {
  margins <- par("mar")
  pdf(file)
  par(mar=c(5.1,4.1,5.6,2.1))
  plot(c(1e-7,1e-7), c(1e-7,1e-7), log="xy",
       xlim=c(1e-5,1e+8), ylim=c(1,max(wikipedia$freq)),
       xlab="number", ylab="frequency",
       xaxt="n", yaxt="n")
  axis(1, cex.axis=0.7, at=c(1e-5,1e-3,1e-1,1,10,1e+3,1e+5,1e+7),
       labels=c("0.00001","0.001","0.1","1","10","1.000","100.000","10.000.000"))
  axis(2, cex.axis=0.7, at=c(1,10,100,1e+3,1e+4,1e+5,1e+6), 
       labels=c("1","10","100","1.000","10.000","100.000","1.000.000"))
  axis(3, cex.axis=0.7, at=c(0.3,3,100,2000,20000,200000), las=2, lty=2,
       labels=c("Basketball","Baseball","FM","Year","Year-Month","Year-Week"), col="gray70", col.axis="gray70")
  abline(v=c(0.3,3,100,2000,20000,200000),lty=2,col="gray70")

  #points(wikipedia$number,wikipedia$freq,cex=0.1,col="gray80")
  #k <- dim(wikipedia)[1]
  #text(wikipedia$number[1:k],wikipedia$freq[1:k],labels=wikipedia$number[1:k],srt=315,cex=0.81-(0.8*seq(1,k)/k))

  if(text) {
    points(wikipedia$number,wikipedia$freq,cex=0.1,col="gray80")
    text(wikipedia$number,wikipedia$freq,labels=wikipedia$number,srt=315,cex=0.3)
  } else {
    points(wikipedia$number,wikipedia$freq,cex=0.1,col="black")
  }
  dev.off()
  par(mar=margins)
}

cc_freq_plot <- function(file,text=FALSE) {
  margins <- par("mar")
  pdf(file)
  par(mar=c(5.1,4.1,5.6,2.1))
  plot(c(1e-7,1e-7), c(1e-7,1e-7), log="xy",
       xlim=c(1e-5,1e+8), ylim=c(1,max(cc$freq)),
       xlab="number", ylab="frequency",
       xaxt="n", yaxt="n")
  axis(1, cex.axis=0.7, at=c(1e-5,1e-3,1e-1,1,10,1e+3,1e+5,1e+7),
       labels=c("0.00001","0.001","0.1","1","10","1.000","100.000","10.000.000"))
  axis(2, cex.axis=0.7, at=c(1,10,100,1e+3,1e+4,1e+5,1e+6), 
       labels=c("1","10","100","1.000","10.000","100.000","1.000.000"))
  axis(3, cex.axis=0.7, at=c(2000,90210,20000000), las=2, lty=2,
       labels=c("Year","ZIP codes","Year-Month-Day"), col="gray70", col.axis="gray70")
  abline(v=c(2000,90210,20000000),lty=2,col="gray70")
  
  #points(cc$number,cc$freq,cex=0.1,col="gray80")
  #k <- dim(cc)[1]
  #text(cc$number[1:k],cc$freq[1:k],labels=cc$number[1:k],srt=315,cex=0.81-(0.8*seq(1,k)/k))
  
  if(text) {
    points(cc$number,cc$freq,cex=0.1,col="gray80")
    text(cc$number,cc$freq,labels=cc$number,srt=315,cex=0.3)
  } else {
    points(cc$number,cc$freq,cex=0.1,col="black")
  }
  dev.off()
  par(mar=margins)
}


# test Power law
zipf_plot <- function(file) {
  pdf(file)
  plot(wikipedia$freq,log="xy",xlab="rank",ylab="frequency",pch=3,cex=0.7,col="blue")
  legend("topright", c("Common Crawl","Wikipedia"),pch=2:3,col=c("red","blue"))
  points(cc$freq,pch=2,col="red")
  dev.off()
}
coef(power.law.fit(cc[['freq']]))
#alpha 
#1.934673 

coef(power.law.fit(wikipedia[['freq']]))
#alpha 
#2.554093 


# test Benford's law
benford <- data.frame(msd=seq(0,9),freq=append(0,sapply(seq(1,9),function(d) log10(1+1/d))))

cc_msd <- aggregate(freq~msd, sum, data=data.frame(msd=substr(sprintf("%e",abs(cc$number)),1,1),freq=cc$freq))
cc_benford <- cc_msd[which(as.character(cc_msd$msd) > "0" & 
                           as.character(cc_msd$msd) < "a"),]
cc_benford$freq <- cc_benford$freq / sum(cc_benford$freq)
ks.test(benford$freq,cc_benford$freq,alternative=c("two.sided"))

#Two-sample Kolmogorov-Smirnov test
#
#data:  benford$freq and cc_benford$freq 
#D = 0.2444, p-value = 0.8737
#alternative hypothesis: two-sided 

wikipedia_msd <- aggregate(freq~msd, sum, data=data.frame(msd=substr(sprintf("%e",abs(wikipedia$number)),1,1),freq=wikipedia$freq))
wikipedia_benford <- wikipedia_msd[which(as.character(wikipedia_msd$msd) > "0" & 
                                         as.character(wikipedia_msd$msd) < "a"),]
wikipedia_benford$freq <- wikipedia_benford$freq / sum(wikipedia_benford$freq)
ks.test(benford$freq,wikipedia_benford$freq,alternative=c("two.sided"))

#Two-sample Kolmogorov-Smirnov test
#
#data:  benford$freq and wikipedia_benford$freq 
#D = 0.5667, p-value = 0.07463
#alternative hypothesis: two-sided 

skewness(cc$freq)
#[1] 225.0601
# expected to quickly grow as a larger portion of Common Crawl is loaded

skewness(wikipedia$freq)
#[1] 185.495

median(cc$freq)
#[1] 1
median(wikipedia$freq)
#[1] 1

mean(cc$freq)
#[1] 96.32467
# expected to quickly grow as a larger portion of Common Crawl is loaded

mean(wikipedia$freq)
#[1] 51.31802



# plot Benford's law against actual distributions
benford_plot <- function(file) {
  pdf(file)
  plot(benford,col="black",xlim=c(1,9),ylim=c(0,0.5),type="p",xlab="most significant digit",ylab="ratio")
  axis(1, at=seq(1,9), labels=seq(1,9))
  legend("topright", c("Benford's law expectation","Common Crawl","Wikipedia"),pch=1:3,col=c("black","red","blue"))
  points(cc_benford$freq,col="red",pch=2)
  points(wikipedia_benford$freq,col="blue",pch=3)
  dev.off()
  par(mar=margins)
}


# relative frequency between Wikipedia and Common Crawl
cc_p <- data.frame(number=cc$number,p=cc$freq / sum(cc$freq))
cc_p <- cc_p[which(!is.infinite(cc_p$number)),]
wikipedia_p <- data.frame(number=wikipedia$number,p=wikipedia$freq / sum(wikipedia$freq))
wikipedia_p <- wikipedia_p[which(!is.infinite(wikipedia_p$number)),]

m <- merge(wikipedia_p,cc_p,by="number")
names(m) <- c("number","wikipedia_p","cc_p")
m$rel_p <- m$wikipedia_p - m$cc_p

m_wikipedia <- m[which(m$rel_p > 0),] # more common in Wikipedia
m_cc <- m[which(m$rel_p < 0),] # more common in Common Crawl
m_cc$rel_p <- -1 * m_cc$rel_p

wiki_gt_cc_plot <- function(file) {
  margins <- par("mar")
  pdf(file)
  par(mar=c(5.1,4.1,5.6,2.1))
  # more common in Wikipedia
  plot(c(1e-7,1e-7),c(1e-7,1e-7), log="xy", xlab="number", ylab="ratio difference Wikipedia > Common Crawl",
       xlim=c(0.01,1e+5),ylim=c(min(m_wikipedia$rel_p),max(m_wikipedia$rel_p)), xaxt="n", yaxt="n")
  axis(1, cex.axis=0.7, at=c(1e-1,1,10,100,1e+3,1e+4,1e+5),
       labels=c("0.1","1","10","100","1.000","10.000","100.000"))
  axis(2, cex.axis=0.7)
  points(m_wikipedia$number,m_wikipedia$rel_p,cex=0.1,col="gray80")
  text(m_wikipedia$number,m_wikipedia$rel_p,labels=m_wikipedia$number,srt=315,cex=0.6)
  dev.off()
  par(mar=margins)
}

cc_gt_wiki_plot <- function(file) {
  margins <- par("mar")
  pdf(file)
  par(mar=c(5.1,4.1,5.6,2.1))
  # more common in Common Crawl
  plot(c(1e-7,1e-7),c(1e-7,1e-7),log="xy", xlab="number", ylab="ratio difference Common Crawl > Wikipedia",
       xlim=c(0.01,1e+5),ylim=c(min(m_wikipedia$rel_p),max(m_wikipedia$rel_p)), xaxt="n", yaxt="n")
  axis(1, cex.axis=0.7, at=c(1e-1,1,10,100,1e+3,1e+4,1e+5),
       labels=c("0.1","1","10","100","1.000","10.000","100.000"))
  axis(2, cex.axis=0.7)
  points(m_cc$number,m_cc$rel_p,cex=0.1,col="gray80")
  text(m_cc$number,m_cc$rel_p,labels=m_cc$number,srt=315,cex=0.6)
  dev.off()
  par(mar=margins)
}

