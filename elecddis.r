library(tuneR)
library(plotly)
require('ggvis');
sndObj <- readWave('D:/hand/voicesamples/chanduhello1.wav')
a <-str(sndObj)
s2 <- sndObj@left
s2 <- s2 / 2^(sndObj@bit -1)
plot(s2)
sndObj <- readWave('D:/hand/voicesamples/chanduhello2.wav')
a <-str(sndObj)
s1 <- sndObj@left
s1 <- s1 / 2^(sndObj@bit -1)
sndObj <- readWave('D:/hand/voicesamples/suchihello1.wav')
a <-str(sndObj)
s3 <- sndObj@left
s3 <- s3 / 2^(sndObj@bit -1)
View(s11[["spec"]])
View(s22[["spec"]])
View(s33[["spec"]])
dist(rbind(s11[["spec"]], s22[["spec"]]))
dist(rbind(s11, s22))
dist(rbind(s11, s33))
s11=spectrum(s1)
s22=spectrum(s2)
s33=spectrum(s3)
sa=max(s11)
sb=max(s22)
plot(s2)
plot(s1)
m <- ccf(s11[["spec"]], s22[["spec"]])
m3 <- max(abs(m[["acf"]]))
m1 <- ccf(s11[["spec"]], s33[["spec"]])
m2 <- ccf(s33[["spec"]], s22[["spec"]])
m5 <- max(abs(m1[["acf"]]))
m6 <- max(abs(m2[["acf"]]))
del<-0.1 # sampling interval
 x.spec <- spectrum(s1,log="no",span=10,plot=FALSE)
spx <- x.spec$freq/del
spy <- 2*x.spec$spec
 plot(spy~spx,xlab="frequency",ylab="spectral density",type="l")
 max(spy)
 x.spec <- spectrum(s2,log="no",span=10,plot=FALSE)
 spx <- x.spec$freq/del
 spy <- 2*x.spec$spec
 plot(spy~spx,xlab="frequency",ylab="spectral density",type="l")
 max(spy)
 notefreq(sndObj, ref = 440, octave = 3)
a<-downsample(sndObj,10000)
plot(a)
pastew(manofive3.wav,jasfive4.wav , f, at = "end",
       join = FALSE, tjunction=0,
       choose = FALSE, plot = FALSE,
       marks = TRUE, output = "matrix")

