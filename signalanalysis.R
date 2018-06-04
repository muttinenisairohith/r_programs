library(rPython)
python.load('audanal.py')
library(tuneR)
library(plotly)
require('ggvis');
sndObj <- readWave('~/CHANDU WORK/chanduhello1.wav')
str(sndObj)
s2 <- sndObj@left
s2 <- s2 / 2^(sndObj@bit -1)
png("plot6.png")
plot(s2)
dev.off()
library(seewave)
n <- noisew(d=1,f=100)
a4 <- afilter(s2,f=100,colwave="blue")
plot(a4)
r4 <- rmnoise(s2,f=1)
plot(r4)

x4 <- spectro(s2,1000)
spec4 <- x4[["freq"]]

o4 <- oscillo(s2,1000)
par(new=TRUE)

env(s2,f=1000,msmooth=c(20,0),colwave=2)
subplot(r4,a4)
timer(s2,
      f=1000,
      threshold=6,
      smooth=100000,
      bty="l",
      xaxs="i",
      colval="blue"
)
diffenv(s2,s3,f=512,plot='TRUE')

spectro(s2,f=22050,
        ovlp=50,zp=16,
        collevels=seq(-40,0,0.5),
        osc=TRUE)

spectro(s2,f=1050,
        palette=temp.colors,
        collevels=seq(-100,0,1))

wf(s2,f=1000,
   ovlp=20,hoff=0.5,voff=0.2,
   border="#00000075")

spectro3D(s2,
          f=1050,wl=490,
          ovlp=55,zp=6,maga=8,
          palette=spectro.colors)

spec(s2,f=150,
     col="blue")

