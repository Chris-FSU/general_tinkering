####################
### Audio Script ###
####################

# This is still a work in progress, but you should be able to play music with it already.
# If you know a bit about music and R code, you should be able to tinker with it.
# Please let me know if you get it to do something cool! - Chris

install.packages("audio")
library("audio")

# Sample note: F
play(sin(1:100000 / 20))

# Types of chords (for playchord):
M <- c(1, 5, 8)
m <- c(1, 4, 8)
sus2 <- c(1, 3, 8)
sus4 <- c(1, 6, 8)
M7 <- c(1, 5, 8, 11)
m7 <- c(1, 4, 8, 11)
dim <- c(1, 4, 7)

# Scales (for playprog):
Major<-c(1,3,5,6,8,10,12,13)
Minor<-c(1,3,4,6,8,9,11,13)

# Standard Western tuning
notes <- c(2 ^ (0:59 / 12))
notenames <-
  rep(c("F", "F#", "G", "G#", "A", "Bb", "B", "C", "C#", "D", "Eb", "E"), 5)
names(notes) <- notenames

# Harmonic Series Tuning
notes <-
  c(
    c(1, 16 / 15, 9 / 8, 6 / 5, 5 / 4, 4 / 3, sqrt(2), 3 / 2, 8 / 5, 5 / 3, 16 /
        9, 15 / 8),
    c(1, 16 / 15, 9 / 8, 6 / 5, 5 / 4, 4 / 3, sqrt(2), 3 / 2, 8 / 5, 5 / 3, 16 /
        9, 15 / 8) * 2,
    c(1, 16 / 15, 9 / 8, 6 / 5, 5 / 4, 4 / 3, sqrt(2), 3 / 2, 8 / 5, 5 / 3, 16 /
        9, 15 / 8) * 4,
    c(1, 16 / 15, 9 / 8, 6 / 5, 5 / 4, 4 / 3, sqrt(2), 3 / 2, 8 / 5, 5 / 3, 16 /
        9, 15 / 8) * 16,
    c(1, 16 / 15, 9 / 8, 6 / 5, 5 / 4, 4 / 3, sqrt(2), 3 / 2, 8 / 5, 5 / 3, 16 /
        9, 15 / 8) * 32
  )
notenames <-
  rep(c("F", "F#", "G", "G#", "A", "Bb", "B", "C", "C#", "D", "Eb", "E"), 5)
names(notes) <- notenames

##############
### Chords ###
##############

# Major chord:
play(sin(1:100000 / 20))
play(sin(1:100000 * 2 ^ (4 / 12) / 20))
play(sin(1:100000 * 2 ^ (7 / 12) / 20))

# Major chord with Harmonic Series Tuning:
play(sin(1:100000 / 20))
play(sin(1:100000 * (5 / 4) / 20))
play(sin(1:100000 * (6 / 4) / 20))
# This note doesn't exist in Western tuning ;-)
play(sin(1:100000 * (7 / 4) / 20))
play(sin(1:100000 * 2 / 20))

# Minor chord
play(sin(1:100000 / 20))
play(sin(1:100000 * 2 ^ (3 / 12) / 20))
play(sin(1:100000 * 2 ^ (7 / 12) / 20))

# Diminished chord
play(sin(1:100000 / 20))
play(sin(1:100000 * 2 ^ (3 / 12) / 20))
play(sin(1:100000 * 2 ^ (6 / 12) / 20))

#################
### Functions ###
#################
playnote <- function(n, dur = 1,oct=1) {
  note <- notes[n]
  play(sin(1:(100000 * dur) * (note) /(20*2^-oct)))
  Sys.sleep(dur*2)
}

playseries<-function(n,dur=1,oct=1){
  for(i in 1:length(n)){
    note <- notes[n[i]]
    play(sin(1:(100000 * dur) * (note) /(20*2^-oct)))
    Sys.sleep(dur*2)
  }
}

playnotes <- function(n, dur = 1) {
  for (i in 1:length(n)) {
    x <- n[i]
    note <- notes[n]
    play(sin(1:(100000 * dur) * (note) / 20))
  }
}

playchord <- function(tonic, chord = M, dur = 1) {
  n <- which(notenames %in% tonic)
  ch <- notenames[n + chord - 1]
  for (i in 1:length(ch)) {
    note <- notes[ch[i]]
    play(sin(1:(100000 * dur) * (note) / 20))
  }
  Sys.sleep(dur * 2)
}

#####################################
### Basket Case by Sara Bareilles ###
#####################################

# verse 1
playchord("A")
playchord("G", sus2, .5)
playchord("D", M, .5)
playchord("A")
playchord("G", sus2, .5)
playchord("D", M, .5)
playchord("A")
playchord("B", m7, .5)
playchord("G", sus2)
playchord("A", M, .5)
playchord("G", sus2, .5)
playchord("D")

# verse 2
playchord("A")
playchord("G", sus2, .5)
playchord("D", M, .5)
playchord("A")
playchord("G", sus2, .5)
playchord("D", M, .5)
playchord("A")
playchord("B", m7, .5)
playchord("G", sus2)
playchord("A", M, .5)
playchord("G", sus2, .5)
playchord("D")

# chorus
playchord("D")
playchord("G", sus2)
playchord("G", m)
playchord("D")
playchord("D")
playchord("G", sus2)
playchord("G", m)
playchord("D", M, .5)
playchord("A", M, .5)
playchord("G", sus2)
playchord("D", M, .5)
playchord("A", M, .5)
playchord("G", sus2)

# verse 3
playchord("A")
playchord("G", sus2, .5)
playchord("D", M, .5)
playchord("A")
playchord("G", sus2, .5)
playchord("D", M, .5)
playchord("A")
playchord("B", m7, .5)
playchord("G", sus2)
playchord("A", M, .5)
playchord("G", sus2, .5)
playchord("D")

# bridge
playchord("D", M, 1.5)
playchord("A", M, 1.5)
playchord("B", m7, .5)
playchord("G", sus2, .5)
playchord("D")
playchord("A")

# chorus
playchord("D")
playchord("G", sus2)
playchord("G", m)
playchord("D")
playchord("D")
playchord("G", sus2)
playchord("G", m)
playchord("D", M, .5)
playchord("A", M, .5)
playchord("G", sus2)
playchord("D", M, .5)
playchord("A", M, .5)
playchord("G", sus2)
playchord("D", M, .5)
playchord("A", M, .5)
playchord("G", sus2)
playchord("D", M, .5)
playchord("A", M, .5)
playchord("G", sus2)


###############################
### Never Gonna Give You Up ###
###############################
playnote("B",.1)
playnote("C#",.1)
playnote("D",.1)
playnote("D",.1)
playnote("E",.1)
playnote("C#",.15)
playnote("B",.05)
playnote("A",.3)
Sys.sleep(.15)

playnote("B",.1)
playnote("B",.1)
playnote("C#",.1)
playnote("D",.3)
Sys.sleep(.15)

playnote("B",.1)
playnote("A",.15,2)
playnote("A",.15,2)
playnote("E",.3)
Sys.sleep(.25)

playnote("B",.1)
playnote("C#",.1)
playnote("D",.1)
playnote("D",.1)
playnote("E",.1)
playnote("C#",.15)
playnote("B",.05)
playnote("A",.3)
Sys.sleep(.15)

playnote("B",.1)
playnote("B",.1)
playnote("C#",.1)
playnote("D",.3)
Sys.sleep(.15)

playnote("B",.1)
playnote("E",.1)
playnote("E",.1)
playnote("E",.1)
playnote("F#",.1,2)
playnote("E",.3)
Sys.sleep(.15)

playnote("D",.45)
playnote("E",.1)
playnote("F#",.1,2)
playnote("D",.1)
playnote("E",.1)
playnote("E",.1)
playnote("E",.1)
playnote("F#",.1,2)
playnote("E",.3)
playnote("B",.3)


#####################
### Experimenting ###
#####################

playchord("C",M,1)
playchord("E",m,1)
playchord("D",m,1)
playchord("G",M,.5)
playchord("F",M,.25)
playchord("E",m,.25)
