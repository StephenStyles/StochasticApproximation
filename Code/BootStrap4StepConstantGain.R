#########################################################################################################
# Initial Setup of working directory and functions used to calculate scores
#########################################################################################################

setwd("C:/Users/sjsty/Desktop/Masters/Algorithms/Simplified Problem")

leakyrelu <- function(eps,x){
  if(x<0){
    return(eps*x)
  } else{
    return(x)
  }
}

score <- function(x){
  rank = NULL
  for(i in 1:length(x)){
    rank[i] = exp(-x)
  }
  return(rank)
}

update <- function(A,b,w,i,n,chi){
  wnew = w + (1/((n+i)^chi) * (b-A%*%w))
  return(wnew)
}

mse <- function(x,y){
  temp = NULL
  for(i in 1:length(x)){
    temp[i] = (x[i]-y[i])^2
  }
  return(mean(temp))
}

#########################################################################################################
# Initial Data to estimate and validation set
#########################################################################################################

SAdata = read.table("12outputtraindata.txt", sep = "\t", header= TRUE)
row.names(SAdata) = NULL

#Setting the amount of data that will be in the sampling set and the amount that will be in the updating set
smp_size = floor(0.75 * nrow(SAdata))

#This section just creates datasets for the X's and y's
train_ind = sample(seq_len(nrow(SAdata)), size = smp_size)
traindata = SAdata[train_ind,1:4]
trainlabels = SAdata[train_ind,5:16]
row.names(traindata) = NULL
row.names(trainlabels) = NULL
testdata = SAdata[-train_ind,1:4]
testlabels = SAdata[-train_ind,5:16]
row.names(testdata) = NULL
row.names(testlabels) = NULL

validationdata = read.table("12outputNNdata.txt", sep = "\t", header= TRUE)

#########################################################################################################
# Initial Weights and Biases
#########################################################################################################

#These weights are generated from the sci-kit learn package in python
bias1 = t(read.table("final12firstlayerint.csv",sep = ","))
weights1 = read.table("final12firstlayercoef.csv",sep = ",")
w1df = rbind(bias1, weights1)
w1 = as.numeric(c(w1df[1,],w1df[2,],w1df[3,],w1df[4,],w1df[5,]))


bias2 = t(read.table("final12secondlayerint.csv",sep = ","))
weights2 = read.table("final12secondlayercoef.csv",sep = ",")
w2df = rbind(bias2, weights2)
w2 = as.numeric(c(w2df[1,],w2df[2,],w2df[3,],w2df[4,],w2df[5,],
                  w2df[6,],w2df[7,],w2df[8,],w2df[9,],w2df[10,],
                  w2df[11,],w2df[12,],w2df[13,],w2df[14,],w2df[15,],
                  w2df[16,],w2df[17,],w2df[18,],w2df[19,],w2df[20,],
                  w2df[21,]))

#Setting the weight variables so they can be averaged
w1check = w1
w2check = w2

#Reseting the accuracy so that we can track it throughout the algorithm
acc = NULL



#########################################################################################################
# Bootstrapped Algorithm
#########################################################################################################

#The slope of the x<0 section of the leaky RELU
eps=0.0001

#These are the sizes of the sampling set and the updating set

#Size of sampling set:
m=30000

#Size of updating set:
n=10000

#Setting the iteration number
it=1

for(k in 1:20){
  #These values move along the data sets so that we see new observations throughout the process
  a = 1 + m*(k-1)
  b = m*k
  c = 1 + n*(k-1)
  d = n*k
  data1 = traindata[a:b,]
  data2 = testdata[c:d,]
  data1labels = trainlabels[a:b,]
  data2labels = testlabels[c:d,]
  
  
  #Tracking the observations and all their hidden layer values during the feed forward process.
  #This just saves us from having to calculate any inverses
  inputs1 = data.frame(x1_1=numeric(),x1_2=numeric(),x1_3=numeric(),x1_4=numeric())
  outputs1 = data.frame(h1=numeric(),h2=numeric(),h3=numeric(),h4=numeric(),h5=numeric(),
                        h6=numeric(),h7=numeric(),h8=numeric(),h9=numeric(),h10=numeric(),
                        h11=numeric(),h12=numeric(),h13=numeric(),h14=numeric(),h15=numeric(),
                        h16=numeric(),h17=numeric(),h18=numeric(),h19=numeric(),h20=numeric())
  inputs2 = data.frame(x2_1=numeric(),x2_2=numeric(),x2_3=numeric(),x2_4=numeric(),x2_5=numeric(),
                       x2_6=numeric(),x2_7=numeric(),x2_8=numeric(),x2_9=numeric(),x2_10=numeric(),
                       x2_11=numeric(),x2_12=numeric(),x2_13=numeric(),x2_14=numeric(),x2_15=numeric(),
                       x2_16=numeric(),x2_17=numeric(),x2_18=numeric(),x2_19=numeric(),x2_20=numeric())
  outputs2 = data.frame(o1=numeric(),o2=numeric(),o3=numeric(),o4=numeric(),o5=numeric(),o6=numeric(),
                        o7=numeric(),o8=numeric(),o9=numeric(),o10=numeric(),o11=numeric(),o12=numeric())
  finalest = data.frame(yhat1=numeric(),yhat2=numeric(),yhat3=numeric(),yhat4=numeric(),yhat5=numeric(),yhat6=numeric(),
                        yhat7=numeric(),yhat8=numeric(),yhat9=numeric(),yhat10=numeric(),yhat11=numeric(),yhat12=numeric())
  trueval = data.frame(y1=numeric(),y2=numeric(),y3=numeric(),y4=numeric(),y5=numeric(),y6=numeric(),
                       y7=numeric(),y8=numeric(),y9=numeric(),y10=numeric(),y11=numeric(),y12=numeric())
  for(i in 1:m){
    inputs1[i,] = data1[i,1:4]
    x1 = as.numeric(inputs1[i,1:4])
    Xm = cbind(1*diag(20),x1[1]*diag(20),x1[2]*diag(20),x1[3]*diag(20),x1[4]*diag(20))
    outputs1[i,] = Xm %*% w1check
    for(j in 1:20){
      inputs2[i,j] = leakyrelu(eps,outputs1[i,j])
    }
    x2 =  as.numeric(inputs2[i,1:20])
    Xm = cbind(1*diag(12),x2[1]*diag(12),x2[2]*diag(12),x2[3]*diag(12),x2[4]*diag(12),
               x2[5]*diag(12),x2[6]*diag(12),x2[7]*diag(12),x2[8]*diag(12),
               x2[9]*diag(12),x2[10]*diag(12),x2[11]*diag(12),x2[12]*diag(12),
               x2[13]*diag(12),x2[14]*diag(12),x2[15]*diag(12),x2[16]*diag(12),
               x2[17]*diag(12),x2[18]*diag(12),x2[19]*diag(12),x2[20]*diag(12))
    outputs2[i,] = Xm %*% w2check
    for(j in 1:12){
      finalest[i,j] = leakyrelu(eps,outputs2[i,j])
    }
    trueval[i,] = data1labels[i,1:12]
  }
  approx = cbind(inputs1,outputs1,inputs2,outputs2,finalest,trueval)
  
  #Now that we have all of our estimates, we can score them using our score function. This will allow us to
  #create a probability distribution over the observations
  error = data.frame(err = numeric())
  for(i in 1:m){
    x = as.numeric(approx[i,57:68])
    x = x/sum(x)
    y = as.numeric(approx[i,69:80])
    error[i,1] = mse(x,y)
  }
  approx = cbind(approx,error)
  
  #Now we throw away all the incorrect observations
  correctclass = data.frame(correct = numeric())
  for(i in 1:m){
    y = as.numeric(approx[i,57:68])
    x = as.numeric(approx[i,69:80])
    if(max(y)!=0){
      for( j in 1:12){
        if(y[j] == max(y)){
          y[j]=1
        } else{
          y[j]=0
        }
      }
    }
    correctclass[i,1] = t(x) %*% y
  }
  approx = cbind(approx,correctclass)
  
  truevalues = approx[which(approx$correct == 1),]
  
  truey1 = truevalues[which(truevalues$y1 == 1),];truey2 = truevalues[which(truevalues$y2 == 1),]
  truey3 = truevalues[which(truevalues$y3 == 1),];truey4 = truevalues[which(truevalues$y4 == 1),]
  truey5 = truevalues[which(truevalues$y5 == 1),];truey6 = truevalues[which(truevalues$y6 == 1),]
  truey7 = truevalues[which(truevalues$y7 == 1),];truey8 = truevalues[which(truevalues$y8 == 1),]
  truey9 = truevalues[which(truevalues$y9 == 1),];truey10 = truevalues[which(truevalues$y10 == 1),]
  truey11 = truevalues[which(truevalues$y11 == 1),];truey12 = truevalues[which(truevalues$y12 == 1),]
  
  #We create probability distributions over all the 12 output values
  sy1 = sapply(truey1[,81],score);sy1 = sy1/sum(sy1)
  sy2 = sapply(truey2[,81],score);sy2 = sy2/sum(sy2)
  sy3 = sapply(truey3[,81],score);sy3 = sy3/sum(sy3)
  sy4 = sapply(truey4[,81],score);sy4 = sy4/sum(sy4)
  sy5 = sapply(truey5[,81],score);sy5 = sy5/sum(sy5)
  sy6 = sapply(truey6[,81],score);sy6 = sy6/sum(sy6)
  sy7 = sapply(truey7[,81],score);sy7 = sy7/sum(sy7)
  sy8 = sapply(truey8[,81],score);sy8 = sy8/sum(sy8)
  sy9 = sapply(truey9[,81],score);sy9 = sy9/sum(sy9)
  sy10 = sapply(truey10[,81],score);sy10 = sy10/sum(sy10)
  sy11 = sapply(truey11[,81],score);sy11 = sy11/sum(sy11)
  sy12 = sapply(truey12[,81],score);sy12 = sy12/sum(sy12)
  
  
  for(i in 1:n){
    x = as.numeric(data2[i,1:4])
    data = NULL
    y2 = 10*as.numeric(data2labels[i,1:12])
    if(y2[1] == 10){
      data = truey1;sc = sy1
    } else if(y2[2] == 10){
      data = truey2;sc = sy2
    } else if(y2[3] == 10){
      data = truey3;sc = sy3
    } else if(y2[4] == 10){
      data = truey4;sc = sy4
    } else if(y2[5] == 10){
      data = truey5;sc = sy5
    } else if(y2[6] == 10){
      data = truey6;sc = sy6
    } else if(y2[7] == 10){
      data = truey7;sc = sy7
    } else if(y2[8] == 10){
      data = truey8;sc = sy8
    } else if(y2[9] == 10){
      data = truey9;sc = sy9
    } else if(y2[10] == 10){
      data = truey10;sc = sy10
    } else if(y2[11] == 10){
      data = truey11;sc = sy11
    } else if(y2[12] == 10){
      data = truey12;sc = sy12
    }
    
    q = sample(1:dim(data)[1],size=1,prob=sc)
    X1 = cbind(1*diag(20),x[1]*diag(20),x[2]*diag(20),x[3]*diag(20),x[4]*diag(20))
    y1 = as.numeric(data[q,5:24])
    x2 = as.numeric(data[q,25:44])
    X2 = cbind(1*diag(12),x2[1]*diag(12),x2[2]*diag(12),x2[3]*diag(12),x2[4]*diag(12),
               x2[5]*diag(12),x2[6]*diag(12),x2[7]*diag(12),x2[8]*diag(12),
               x2[9]*diag(12),x2[10]*diag(12),x2[11]*diag(12),x2[12]*diag(12),
               x2[13]*diag(12),x2[14]*diag(12),x2[15]*diag(12),x2[16]*diag(12),
               x2[17]*diag(12),x2[18]*diag(12),x2[19]*diag(12),x2[20]*diag(12))
    
    if(it==1){
      A1check = t(X1)%*%X1
      A2check = t(X2)%*%X2
      B1check = t(X1)%*%y1
      B2check = t(X2)%*%y2
      
    }
    
    A1 = t(X1)%*%X1
    A2 = t(X2)%*%X2
    B1 = t(X1)%*%y1
    B2 = t(X2)%*%y2
    
    A1check = A1check +(A1 -A1check)/(it+1)
    B1check = B1check +(B1 -B1check)/(it+1)
    A2check = A2check +(A2 -A2check)/(it+1)
    B2check = B2check +(B2 -B2check)/(it+1)
    
    w1 = w1 + (1/4)*(B1check-A1check%*%w1)
    w2 = w2 + (1/400)*(B2check-A2check%*%w2)
    
    w1check = w1check + (w1-w1check)/(it+1)
    w2check = w2check + (w2-w2check)/(it+1)
    
    it=it+1
    
    
    if(i%%1000==0){
      tempvalidationdata = validationdata
      estimates = data.frame(yhat1=numeric(),yhat2=numeric(),yhat3=numeric(),yhat4=numeric(),yhat5=numeric(),yhat6=numeric(),
                             yhat7=numeric(),yhat8=numeric(),yhat9=numeric(),yhat10=numeric(),yhat11=numeric(),yhat12=numeric())
      
      for (s in 1:6000){
        x1 = as.numeric(tempvalidationdata[s,1:4])
        Xm = cbind(1*diag(20),x1[1]*diag(20),x1[2]*diag(20),x1[3]*diag(20),x1[4]*diag(20))
        output1 = Xm %*% w1check
        x2 = c(rep(0,20))
        for(j in 1:20){
          x2[j] = leakyrelu(eps,output1[j])
        }
        Xm = cbind(1*diag(12),x2[1]*diag(12),x2[2]*diag(12),x2[3]*diag(12),x2[4]*diag(12),
                   x2[5]*diag(12),x2[6]*diag(12),x2[7]*diag(12),x2[8]*diag(12),
                   x2[9]*diag(12),x2[10]*diag(12),x2[11]*diag(12),x2[12]*diag(12),
                   x2[13]*diag(12),x2[14]*diag(12),x2[15]*diag(12),x2[16]*diag(12),
                   x2[17]*diag(12),x2[18]*diag(12),x2[19]*diag(12),x2[20]*diag(12))
        output2 = Xm %*% w2check
        for(j in 1:12){
          estimates[s,j] = leakyrelu(eps,output2[j])
        }
      }
      
      tempvalidationdata=cbind(tempvalidationdata,estimates)
      
      correctclass = data.frame(correct = numeric(), error = numeric())
      for(s in 1:6000){
        y = as.numeric(tempvalidationdata[s,17:28])
        x = as.numeric(tempvalidationdata[s,5:16])
        temp = rep(0,12)
        temp[which.max(y)] = 1
        
        correctclass[s,1] = t(x) %*% temp
      }
      tempvalidationdata=cbind(tempvalidationdata,estimates,correctclass)
      acc = c(acc,sum(correctclass$correct))
      
      plot(acc/6000,type = "l", col = "blue", xlab = "1000 Steps", ylab = "Accuracy")
      
    }
  }
}
