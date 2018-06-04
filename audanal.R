system("python D:/hand/voicesamples/audanal.py")
library(tuneR)
library(seewave)
sndObj <- readWave('D:/hand/voicesamples/chanduhello1.wav')
str(sndObj)
s1 <- sndObj@left
s1 <- s1 / 2^(sndObj@bit -1)

sndObj <- readWave('D:/hand/voicesamples/chanduhello2.wav')
str(sndObj)
s2 <- sndObj@left
s2 <- s2 / 2^(sndObj@bit -1)
plot(s2)
m <- ccf(s1,s2)
m3 <- max(abs(m[["acf"]]))

sndObj <- readWave('D:/hand/voicesamples/chanduhello2.wav')
str(sndObj)
s3 <- sndObj@left
s3 <- s3 / 2^(sndObj@bit -1)
m <- ccf(s1,s3)
m4 <- max(abs(m[["acf"]]))
sndObj <- readWave('D:/hand/voicesamples/chanduhello3.wav')
str(sndObj)
s4 <- sndObj@left
s4 <- s4 / 2^(sndObj@bit -1)
m <- ccf(s1,s4)
m5 <- max(abs(m[["acf"]]))
sndObj <- readWave('D:/hand/voicesamples/chanduhello4.wav')
str(sndObj)
s5 <- sndObj@left
s5 <- s5 / 2^(sndObj@bit -1)
m <- ccf(s1,s5)
m6 <- max(abs(m[["acf"]]))
sndObj <- readWave('D:/hand/voicesamples/chanduhello5.wav')
str(sndObj)
s6 <- sndObj@left
s6 <- s6 / 2^(sndObj@bit -1)
m <- ccf(s1,s6)
m7 <- max(abs(m[["acf"]]))
m8<- 0.03
a <- c(  m3, m4,m5, m6, m7, m8)
b=max(a)
b

if(b <=m3){
  print("voice:chandu
         hello")
}else if(b <=m4){
  print("hello")
}else if(b ==m8  &  max(s1)<0.3){
  print("sorry")
}else if(b <=m5 ){
  print("hello")
}else if(b <=m6 ){
  print("hello")
}else if(b <=m7 ){
  print("hello")
}



