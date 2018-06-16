#install.packages("plyr")
#library("plyr")

print ("Loading data ...")

data_main <- read.csv("/home/eman/R/mailingData_train")
data_main

print ("Processing data ...")

data_mainc<- 1:dim(data_main)[1]

#index of the data
data_main$customer 

#dates variables
dates <- data_main[, c(362:384)]
dates

## Calculating frequency 

# marketing interactions between PVA and the customers
# if date variable has not got missing values then interaction happened 
# if missing values .. no interactions
interactions <- dates
interactions[!is.na(interactions)] <- 1  #no NAs 
interactions[is.na(interactions)] <- 0   #is NA

interactions

interact_freq <- dates
interact_freq[] <- 0

n <- dim(interact_freq)[2]
n  #number of columns in dates (i.e. 23)
for (i in 1:n){
  if(1+n-i <n){  #from right to left
    interact_freq[, 1+n-i] <- interactions[, 2+n-i] + interact_freq[, 2+n-i]
  }
}

interact_freq  #frequency variable 

## Recency
interact_recen <- dates
interact_recen[] <- 0
n <- dim(interact_recen)[2]

# (interact_recen[, 2+n-i] + 1): matrix of ones
# (interactions[, 2+n-i] == 0): boolean matrix (either True of False)
for (i in 1:n){
  if(1+n-i < n){
    interact_recen[,1+n-i] <- (interact_recen[, 2+n-i] + 1)*(interactions[, 2+n-i] == 0)
  }
}
# keda enta bet generate matrix of ones if interactions happen recentely else zero

# combing el target_d with RAMNT variables
data <- data_main[, c(472,435:456)]
data

data[,1][data[,1] == 0] <- NA  #replacing zeros with NAs

## GET FREQUENCY OF DONATION DATA (NOT DATES)
transactions <- data
transactions[!is.na(transactions)] <- 1
transactions[is.na(transactions)] <- 0

freq <- data
freq[] <- 0
n <- dim(freq)[2]

for(i in 1:n){
  if(1+n-i < n){
    freq[, 1+n-i] <- freq[, 2+n-i] + transactions[, 2+n-i]
  }
}

freq

# GET RECENCY OF DONATION DATA (NOT DATE)
recen <- data
recen[] <- 0
n <- dim(recen)[2]

for(i in 1:n){
  if(1+n-i < n){
    recen[, 1+n-i] <- (recen[, 2+n-i]+1)*(transactions[, 2+n-i]==0)
  }
}

max(recen)

# GET REWARD DATA
# replace NAs with zeros back
reward <- data
reward[is.na((reward))] <- 0
reward

# GET AVERAGE MONETARY VALUE OF DONATIONS (NOT COUNTING ZEROS)
monet <- data
monet[] <- 0
n <- dim(monet)[2]

for(i in 1:n){
  if(1+n-i < n){
    monet[, 1+n-i] <- (monet[, 2+n-i]+freq[, (2+n-i)]) + (reward[, (2+n-i)])/(freq[, 1+n-i]+1*(freq[,1+n-i] ==0))
  }
}
monet

# GET ACTIONS (INCLUDING INACTIONS)
# we assign mailing type to each mailing based on data dict
action_set <- c(5,5,7,6,1,11,8,3,2,10,9,4,5,7,1,11,8,3,2,10,9,4,5)
length(action_set)

actions <- matrix(rep(action_set, dim(dates)[1]), nrow = dim(dates)[1], ncol = length(action_set), byrow = TRUE)  
actions ## 25ad el c and repeat it ! :S

# assign action-zero when data is missing
actions[is.na(dates)] <- 0


## SAVE DATA !
save_data <- function(recen, freq, monet, interact_recen, interact_freq, filename)
{
  ## constructing tuple
  for(i in 1:(dim(recen)[2]-1))
  {
    r0 <- recen[,1+dim(recen)[2]-i]
    r1 <- recen[,dim(recen)[2]-i]
    
    f0 <- freq[,1+dim(freq)[2]-i]
    f1 <- freq[,dim(freq)[2]-i]
    
    m0 <- monet[,1+dim(monet)[2]-i]
    m1 <- monet[,dim(monet)[2]-i]
    
    ir0 <- interact_recen[, 1+dim(monet)[2] -i]
    ir1 <- interact_recen[, dim(monet)[2] -i]
    if0 <- interact_freq[, 1+dim(monet)[2] -i]
    if1 <- interact_freq[, dim(monet)[2] -i]
    
    a <- actions[, 1+dim(recen)[2] -i]
    rew <- reward[, 1+dim(recen)[2] - i]
    
    if(i==1)
    {
      write.table(data.frame(cbind(r0, f0, m0, ir0, if0, a, r1, f1, m1, ir1, if1, rew)), filename, col.names = TRUE, sep=",", row.names = FALSE, eol = "\r\n")
    }
    else
    {
      write.table(data.frame(cbind(r0, f0, m0, ir0, if0, a, r1, f1, m1, ir1, if1, rew)), filename, col.names = TRUE, sep= ",", append = TRUE, row.names = FALSE, eol = "\r\n")
    }
  }
}

save_data(recen, freq, monet, interact_recen, interact_freq, "/home/eman/Python codes/tuple.csv")

print("DONE!")






