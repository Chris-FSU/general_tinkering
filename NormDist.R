####################################
### Study of Normal Distribution ###
####################################

# I made this because I had a mathematical understanding of normal distribution,
# but I wanted an intuitive understanding of it. I hope this helps you too!


# If you flip a coin n number of times, this will show the
# probabilities of all possible combinations of heads & tails.
flips<-function(n){
  init<-c(0,1,0)
  x<-NA
  for(i in 1:(n)){
    for(i in 1:(length(init)-1)){
      x[i]<-init[i]+init[i+1]
    }
    init<-c(0,x,0)
  }
  return(x)
}

# Plotting the output reveals a normal distribution
# of possible outcomes from a random coin toss.
# Examine these examples:
plot(flips(1))
plot(flips(3))
plot(flips(9))
plot(flips(27))
plot(flips(81))
plot(flips(243))

# What are the odds of getting h heads if you flip n times?
odds<-function(n,h){
  init<-c(0,1,0)
  x<-NA
  for(i in 1:(n)){
    for(i in 1:(length(init)-1)){
      x[i]<-init[i]+init[i+1]
    }
    init<-c(0,x,0)
  }
  o<-(x[h])/2^(n)
  return(o)
}




