install.packages("tuneR", repos = "http://cran.r-project.org")
library(tuneR)
#Functions
sound_dist <- function(duration, samplingrate) {
  #Speed of sound is 1125 ft/sec
  return((duration/samplingrate)*1125/2)
}
sound_data <- function(dataset, threshold, samplingrate) {
  dataset <- snap@left
  threshold = 4000
  samplingrate = 44100
  data <- data.frame()
  max = 0
  maxindex = 0
  for (i in 1:length(dataset)) {
    if (dataset[i] > max) {
      max = dataset[i]
      maxindex = i
      data <- data.frame()
    }
    if (abs(dataset[i]) > threshold) {
      data <- rbind(data, c(i,dataset[i], sound_dist(i - maxindex, samplingrate)))
    }
  }
  colnames(data) <- c("x", "y", "dist")
  return(data)
}
#Analysis
snap <- readWave("D:/hand/voicesamples/suchihello1.wav")
print(snap)
play(snap)
plot(snap@left[30700:31500], type = "l", main = "Snap",
     xlab = "Time", ylab = "Frequency")
data <- sound_data(snap@left, 4000, 44100)
plot(data[,3], data[,2], type = "l", main = "Snap",
     xlab = "Dist", ylab = "Frequency")
