setwd("C:/Users/sjsty/Desktop/Masters/Algorithms/Simplified Problem")

leakyrelu <- function(eps,x){
  if(x<0){
    return(eps*x)
  } else{
    return(x)
  }
}

sigmoid <- function(x){
  y = sum(x)
  return(exp(y)/(1+exp(y)))
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

########################################################################################################
# SKLearn Neural Network starting points
########################################################################################################

bias1 = t(read.table("12firstlayerint40.csv",sep = ","))
weights1 = read.table("12firstlayercoef40.csv",sep = ",")
w1df = rbind(bias1, weights1)
w1 = as.numeric(c(w1df[1,],w1df[2,],w1df[3,],w1df[4,],w1df[5,]))


bias2 = t(read.table("12secondlayerint40.csv",sep = ","))
weights2 = read.table("12secondlayercoef40.csv",sep = ",")
w2df = rbind(bias2, weights2)
w2 = as.numeric(c(w2df[1,],w2df[2,],w2df[3,],w2df[4,],w2df[5,],
                  w2df[6,],w2df[7,],w2df[8,],w2df[9,],w2df[10,],
                  w2df[11,],w2df[12,],w2df[13,],w2df[14,],w2df[15,],
                  w2df[16,],w2df[17,],w2df[18,],w2df[19,],w2df[20,],
                  w2df[21,]))

##########################################################################################################
# FULL NN Starting points
##########################################################################################################

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

##########################################################################################################
# Initial Data to estimate
##########################################################################################################

SAdata = read.table("12outputtraindata.txt", sep = "\t", header= TRUE)
row.names(SAdata) = NULL

smp_size <- floor(0.75 * nrow(SAdata))

train_ind <- sample(seq_len(nrow(SAdata)), size = smp_size)
traindata = SAdata[train_ind,]
row.names(traindata) = NULL
testdata = SAdata[-train_ind,]
row.names(testdata) = NULL

eps = 0.0001

###############################################################################################################################################
# Finding initial accuracy
###############################################################################################################################################
validationdata = read.table("12outputNNdata.txt", sep = "\t", header= TRUE)
estimates = data.frame(yhat1=numeric(),yhat2=numeric(),yhat3=numeric(),yhat4=numeric(),yhat5=numeric(),yhat6=numeric(),
                       yhat7=numeric(),yhat8=numeric(),yhat9=numeric(),yhat10=numeric(),yhat11=numeric(),yhat12=numeric())

for (s in 1:6000){
  x1 = as.numeric(validationdata[s,1:4])
  Xm = cbind(1*diag(20),x1[1]*diag(20),x1[2]*diag(20),x1[3]*diag(20),x1[4]*diag(20))
  output1 = Xm %*% w1
  x2 = c(rep(0,20))
  for(j in 1:20){
    x2[j] = leakyrelu(eps,output1[j])
  }
  Xm = cbind(1*diag(12),x2[1]*diag(12),x2[2]*diag(12),x2[3]*diag(12),x2[4]*diag(12),
             x2[5]*diag(12),x2[6]*diag(12),x2[7]*diag(12),x2[8]*diag(12),
             x2[9]*diag(12),x2[10]*diag(12),x2[11]*diag(12),x2[12]*diag(12),
             x2[13]*diag(12),x2[14]*diag(12),x2[15]*diag(12),x2[16]*diag(12),
             x2[17]*diag(12),x2[18]*diag(12),x2[19]*diag(12),x2[20]*diag(12))
  o2 = Xm %*% w2
  for(j in 1:12){
    estimates[s,j] = leakyrelu(eps,o2[j])
  }
  
}
validationdata_OG=cbind(validationdata,estimates)

correctclass = data.frame(correct = numeric(), error = numeric())
for(s in 1:6000){
  y = as.numeric(validationdata_OG[s,17:28])
  x = as.numeric(validationdata_OG[s,5:16])
  temp = rep(0,12)
  temp[which.max(y)] = 1
  
  correctclass[s,1] = t(x) %*% temp
  correctclass[s,2] = mse(x,y)
}
validationdata_OG=cbind(validationdata,estimates,correctclass)
acc = c(sum(correctclass$correct))
meanerror=c(sum(correctclass$error))
acc = c(5880)
meanerror=c(2180.471)

###################################################################################################################################
# Starting SA
###################################################################################################################################

w1_avg = w1
w2_avg = w2

weight1_log = matrix(NA, nrow=length(w1), ncol=0)
weight2_log = matrix(NA, nrow=length(w2), ncol=0)
weight1_log = cbind(weight1_log,w1)
weight2_log = cbind(weight2_log,w2)

n = 3000
m = n%/%3


runthrough = 0
epoc=runthrough*250000
for(t in 1:250){
  
  a = n*(t-1)+1
  b = n*t
  c = m*(t-1)+1
  d = m*t
  
  data1 = traindata[a:b,]
  data2 = testdata[c:d,]
  
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
  
  for(i in 1:n){
    inputs1[i,] = data1[i,1:4]
    x1 = as.numeric(inputs1[i,1:4])
    Xm = cbind(1*diag(20),x1[1]*diag(20),x1[2]*diag(20),x1[3]*diag(20),x1[4]*diag(20))
    outputs1[i,] = Xm %*% w1
    for(j in 1:20){
      inputs2[i,j] = leakyrelu(eps,outputs1[i,j])
    }
    x2 =  as.numeric(inputs2[i,1:20])
    Xm = cbind(1*diag(12),x2[1]*diag(12),x2[2]*diag(12),x2[3]*diag(12),x2[4]*diag(12),
               x2[5]*diag(12),x2[6]*diag(12),x2[7]*diag(12),x2[8]*diag(12),
               x2[9]*diag(12),x2[10]*diag(12),x2[11]*diag(12),x2[12]*diag(12),
               x2[13]*diag(12),x2[14]*diag(12),x2[15]*diag(12),x2[16]*diag(12),
               x2[17]*diag(12),x2[18]*diag(12),x2[19]*diag(12),x2[20]*diag(12))
    outputs2[i,] = Xm %*% w2
    for(j in 1:12){
      finalest[i,j] = leakyrelu(eps,outputs2[i,j])
    }
    trueval[i,] = data1[i,5:16]
  }
  
  
  approx = cbind(inputs1,outputs1,inputs2,outputs2,finalest,trueval)
  
  error = data.frame(err = numeric())
  for(i in 1:n){
    x = as.numeric(approx[i,57:68])
    x = x/sum(x)
    y = as.numeric(approx[i,69:80])
    error[i,1] = mse(x,y)
  }
  
  approx = cbind(approx,error)
  
  correctclass = data.frame(correct = numeric())
  for(i in 1:n){
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
  
  truey1 = truevalues[which(truevalues$y1 == 1),]
  truey2 = truevalues[which(truevalues$y2 == 1),]
  truey3 = truevalues[which(truevalues$y3 == 1),]
  truey4 = truevalues[which(truevalues$y4 == 1),]
  truey5 = truevalues[which(truevalues$y5 == 1),]
  truey6 = truevalues[which(truevalues$y6 == 1),]
  truey7 = truevalues[which(truevalues$y7 == 1),]
  truey8 = truevalues[which(truevalues$y8 == 1),]
  truey9 = truevalues[which(truevalues$y9 == 1),]
  truey10 = truevalues[which(truevalues$y10 == 1),]
  truey11 = truevalues[which(truevalues$y11 == 1),]
  truey12 = truevalues[which(truevalues$y12 == 1),]
  
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
  
  for(i in 1:m){
    x = as.numeric(data2[i,1:4])
    data = NULL
    y2 = 10*as.numeric(data2[i,5:16])
    if(y2[1] == 10){
      data = truey1
      sc = sy1
    } else if(y2[2] == 10){
      data = truey2
      sc = sy2
    } else if(y2[3] == 10){
      data = truey3
      sc = sy3
    } else if(y2[4] == 10){
      data = truey4
      sc = sy4
    } else if(y2[5] == 10){
      data = truey5
      sc = sy5
    } else if(y2[6] == 10){
      data = truey6
      sc = sy6
    } else if(y2[7] == 10){
      data = truey7
      sc = sy7
    } else if(y2[8] == 10){
      data = truey8
      sc = sy8
    } else if(y2[9] == 10){
      data = truey9
      sc = sy9
    } else if(y2[10] == 10){
      data = truey10
      sc = sy10
    } else if(y2[11] == 10){
      data = truey11
      sc = sy11
    } else if(y2[12] == 10){
      data = truey12
      sc = sy12
    }
    p = sample(1:dim(data)[1],size=1,prob=sc)
    X1 = cbind(1*diag(20),x[1]*diag(20),x[2]*diag(20),x[3]*diag(20),x[4]*diag(20))
    y1 = as.numeric(data[p,5:24])
    x2 = as.numeric(data[p,25:44])
    X2 = cbind(1*diag(12),x2[1]*diag(12),x2[2]*diag(12),x2[3]*diag(12),x2[4]*diag(12),
               x2[5]*diag(12),x2[6]*diag(12),x2[7]*diag(12),x2[8]*diag(12),
               x2[9]*diag(12),x2[10]*diag(12),x2[11]*diag(12),x2[12]*diag(12),
               x2[13]*diag(12),x2[14]*diag(12),x2[15]*diag(12),x2[16]*diag(12),
               x2[17]*diag(12),x2[18]*diag(12),x2[19]*diag(12),x2[20]*diag(12))
    
    if(i>=1){
      A1 = t(X1)%*%X1
      A2 = t(X2)%*%X2
      B1 = t(X1)%*%y1
      B2 = t(X2)%*%y2
    }
    #if(i>2){
    #  w1_avg = w1/(c+epoc-1+i)+ (c+epoc+i-2)/(c+epoc-1+i)*w1_avg
    #  w2_avg = w2/(c+epoc-1+i)+ (c+epoc+i-2)/(c+epoc-1+i)*w2_avg
    #}
    #if( i>= 2){
    #  A1 = t(X1)%*%X1
    #  A1 = t(X1)%*%X1/(c-1+i) + (c-1+i-1)/(c-1+i)*A1
    #  A2 = t(X2)%*%X2/(c-1+i) + (c-1+i-1)/(c-1+i)*A2
    #  B1 = t(X1)%*%y1/(c-1+i) + (c-1+i-1)/(c-1+i)*B1
    #  B1 = t(X1)%*%y1
    #  B2 = t(X2)%*%y2/(c-1+i) + (c-1+i-1)/(c-1+i)*B2
    #}
    w1 = update(A1, B1,w1,i,25000+c+epoc,0.75)
    w2 = update(A2, B2,w2,i,25000+c+epoc,0.55)
    
    if(i==1000){
      step = t + runthrough*250+1
      tempvalidationdata = validationdata
      estimates = data.frame(yhat1=numeric(),yhat2=numeric(),yhat3=numeric(),yhat4=numeric(),yhat5=numeric(),yhat6=numeric(),
                             yhat7=numeric(),yhat8=numeric(),yhat9=numeric(),yhat10=numeric(),yhat11=numeric(),yhat12=numeric())
      #estimates_avg = data.frame(yhat1=numeric(),yhat2=numeric(),yhat3=numeric(),yhat4=numeric(),yhat5=numeric(),yhat6=numeric(),
      #                           yhat7=numeric(),yhat8=numeric(),yhat9=numeric(),yhat10=numeric(),yhat11=numeric(),yhat12=numeric())
      for (s in 1:6000){
        x1 = as.numeric(tempvalidationdata[s,1:4])
        Xm = cbind(1*diag(20),x1[1]*diag(20),x1[2]*diag(20),x1[3]*diag(20),x1[4]*diag(20))
        output1 = Xm %*% w1
        x2 = c(rep(0,20))
        for(j in 1:20){
          x2[j] = leakyrelu(eps,output1[j])
        }
        Xm = cbind(1*diag(12),x2[1]*diag(12),x2[2]*diag(12),x2[3]*diag(12),x2[4]*diag(12),
                   x2[5]*diag(12),x2[6]*diag(12),x2[7]*diag(12),x2[8]*diag(12),
                   x2[9]*diag(12),x2[10]*diag(12),x2[11]*diag(12),x2[12]*diag(12),
                   x2[13]*diag(12),x2[14]*diag(12),x2[15]*diag(12),x2[16]*diag(12),
                   x2[17]*diag(12),x2[18]*diag(12),x2[19]*diag(12),x2[20]*diag(12))
        o2 = Xm %*% w2
        for(j in 1:12){
          estimates[s,j] = leakyrelu(eps,o2[j])
        }
        
      }
      #for (s in 1:6000){
      #  x1 = as.numeric(tempvalidationdata[s,1:4])
      #  Xm = cbind(1*diag(20),x1[1]*diag(20),x1[2]*diag(20),x1[3]*diag(20),x1[4]*diag(20))
      #  output1 = Xm %*% w1_avg
      #  x2 = c(rep(0,20))
      #  for(j in 1:20){
      #    x2[j] = leakyrelu(eps,output1[j])
      #  }
      #  Xm = cbind(1*diag(12),x2[1]*diag(12),x2[2]*diag(12),x2[3]*diag(12),x2[4]*diag(12),
      #             x2[5]*diag(12),x2[6]*diag(12),x2[7]*diag(12),x2[8]*diag(12),
      #             x2[9]*diag(12),x2[10]*diag(12),x2[11]*diag(12),x2[12]*diag(12),
      #             x2[13]*diag(12),x2[14]*diag(12),x2[15]*diag(12),x2[16]*diag(12),
      #             x2[17]*diag(12),x2[18]*diag(12),x2[19]*diag(12),x2[20]*diag(12))
      #  o2 = Xm %*% w2_avg
      #  for(j in 1:12){
      #    estimates_avg[s,j] = leakyrelu(eps,o2[j])
      #  }
      #  
      #}
      tempvalidationdata=cbind(tempvalidationdata,estimates)
      
      correctclass = data.frame(correct = numeric())
      for(s in 1:6000){
        #y_avg = as.numeric(tempvalidationdata[s,29:40])
        y = as.numeric(tempvalidationdata[s,17:28])
        x = as.numeric(tempvalidationdata[s,5:16])
        temp = rep(0,12)
        temp[which.max(y)] = 1
        
        correctclass[s,1] = t(x) %*% temp
        
        #temp2 = rep(0,12)
        #temp2[which.max(y_avg)]=1
        
        #correctclass[s,3] = t(x) %*% temp2
        #correctclass[s,4] = mse(x,y_avg)
      }
      acc = c(acc,sum(correctclass$correct))
      #meanerror_avg = c(meanerror_avg, sum(correctclass$error_avg))
      #acc_avg = c(acc_avg,sum(correctclass$correct_avg))
      
      weight1_log = cbind(weight1_log,w1)
      weight2_log = cbind(weight2_log,w2)
      plot(acc/6000,type = "l", col = "blue", xlab = "1000 Steps", ylab = "Accuracy")
      lines(1:step,NN_ACC[1:step]/6000, type = "l", col = "red", lwd = 2)
      #plot(meanerror,type = "l", col = "blue", xlab = "1000 Steps", ylab = "MSE")
      #plot(acc_avg/6000,type = "l", col = "blue", xlab = "1000 Steps", ylab = "Accuracy")
      #plot(meanerror_avg,type = "l", col = "red", xlab = "1000 Steps", ylab = "MSE")
    }
  }
}

######################################################################################################################
# Plotting the weights changing over time
######################################################################################################################

library(scales) 
col_pal = rainbow(n=nrow(weight1_log))
plot(c(weight1_log), ylim=range(weight1_log), type='n', xlim=c(1,ncol(weight1_log)), main = "Weight Log Layer 1", xlab = "1000 Steps", ylab = "Weights") # type 'n' plots nothing
for(i in 1:length(w1)){
  points(weight1_log[i,], type='l', pch=19, col=alpha(col_pal[i], 0.5)) # alpha f'n sets colour opacity
}
col_pal = rainbow(n=nrow(weight2_log))
plot(c(weight2_log), ylim=range(weight2_log), type='n', xlim=c(1,ncol(weight2_log)), main = "Weight Log Layer 2", xlab = "1000 Steps", ylab = "Weights") # type 'n' plots nothing
for(i in 1:length(w2)){
  points(weight2_log[i,], type='l', pch=19, col=alpha(col_pal[i], 0.5)) # alpha f'n sets colour opacity
}

plot(acc/6000,type = "l", col = "blue", xlab = "1000 Steps", ylab = "Accuracy")

write.table(weight1_log, "weight1log.txt", sep="\t")
write.table(weight2_log, "weight2log.txt", sep="\t")

############################################################################################################################################################################################
# changing to the shifted data
#############################################################################################################################################################################################


new_w1 = w1
new_w2 = w2

new_weight1_log = matrix(NA, nrow=length(new_w1), ncol=0)
new_weight2_log = matrix(NA, nrow=length(new_w2), ncol=0)
new_weight1_log = cbind(new_weight1_log,new_w1)
new_weight2_log = cbind(new_weight2_log,new_w2)


SAdata = read.table("shiftedtraindata.txt", sep = "\t", header= TRUE)
dim(SAdata)
SAdata = SAdata[complete.cases(SAdata),]
dim(SAdata)
row.names(SAdata) = NULL

smp_size <- floor(0.75 * nrow(SAdata))

train_ind <- sample(seq_len(nrow(SAdata)), size = smp_size)
traindata = SAdata[train_ind,]
row.names(traindata) = NULL
testdata = SAdata[-train_ind,]
row.names(testdata) = NULL

eps = 0.0001


validationdata = read.table("shiftedvalidationset.txt", sep = "\t", header= TRUE)
validationdata = validationdata[complete.cases(validationdata),]
val_len = dim(validationdata)[1]
estimates = data.frame(yhat1=numeric(),yhat2=numeric(),yhat3=numeric(),yhat4=numeric(),yhat5=numeric(),yhat6=numeric(),
                       yhat7=numeric(),yhat8=numeric(),yhat9=numeric(),yhat10=numeric(),yhat11=numeric(),yhat12=numeric())
dim(validationdata)
for (s in 1:val_len){
  x1 = as.numeric(validationdata[s,1:4])
  Xm = cbind(1*diag(20),x1[1]*diag(20),x1[2]*diag(20),x1[3]*diag(20),x1[4]*diag(20))
  output1 = Xm %*% new_w1
  x2 = c(rep(0,20))
  for(j in 1:20){
    x2[j] = leakyrelu(eps,output1[j])
  }
  Xm = cbind(1*diag(12),x2[1]*diag(12),x2[2]*diag(12),x2[3]*diag(12),x2[4]*diag(12),
             x2[5]*diag(12),x2[6]*diag(12),x2[7]*diag(12),x2[8]*diag(12),
             x2[9]*diag(12),x2[10]*diag(12),x2[11]*diag(12),x2[12]*diag(12),
             x2[13]*diag(12),x2[14]*diag(12),x2[15]*diag(12),x2[16]*diag(12),
             x2[17]*diag(12),x2[18]*diag(12),x2[19]*diag(12),x2[20]*diag(12))
  o2 = Xm %*% new_w2
  for(j in 1:12){
    estimates[s,j] = leakyrelu(eps,o2[j])
  }
  
}
validationdata=cbind(validationdata,estimates)

correctclass = data.frame(correct = numeric(), error= numeric())
for(s in 1:val_len){
  y = as.numeric(validationdata[s,17:28])
  x = as.numeric(validationdata[s,5:16])
  temp = rep(0,12)
  temp[which.max(y)] = 1
  
  correctclass[s,1] = t(x) %*% temp
  correctclass[s,2] = mse(x,y)
}
validationdata=cbind(validationdata,estimates,correctclass)
new_acc = c(sum(correctclass$correct))
new_acc
n = 3000
m = n%/%3

for(t in 1:250){
  
  a = n*(t-1)+1
  b = n*t
  c = m*(t-1)+1
  d = m*t
  
  
  data1 = traindata[a:b,]
  data2 = testdata[c:d,]
  
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
  
  for(i in 1:n){
    inputs1[i,] = data1[i,1:4]
    x1 = as.numeric(inputs1[i,1:4])
    Xm = cbind(1*diag(20),x1[1]*diag(20),x1[2]*diag(20),x1[3]*diag(20),x1[4]*diag(20))
    outputs1[i,] = Xm %*% new_w1
    for(j in 1:20){
      inputs2[i,j] = leakyrelu(eps,outputs1[i,j])
    }
    x2 =  as.numeric(inputs2[i,1:20])
    Xm = cbind(1*diag(12),x2[1]*diag(12),x2[2]*diag(12),x2[3]*diag(12),x2[4]*diag(12),
               x2[5]*diag(12),x2[6]*diag(12),x2[7]*diag(12),x2[8]*diag(12),
               x2[9]*diag(12),x2[10]*diag(12),x2[11]*diag(12),x2[12]*diag(12),
               x2[13]*diag(12),x2[14]*diag(12),x2[15]*diag(12),x2[16]*diag(12),
               x2[17]*diag(12),x2[18]*diag(12),x2[19]*diag(12),x2[20]*diag(12))
    outputs2[i,] = Xm %*% new_w2
    for(j in 1:12){
      finalest[i,j] = leakyrelu(eps,outputs2[i,j])
    }
    trueval[i,] = data1[i,5:16]
  }
  
  
  approx = cbind(inputs1,outputs1,inputs2,outputs2,finalest,trueval)
  
  error = data.frame(err = numeric())
  for(i in 1:n){
    x = as.numeric(approx[i,57:68])
    x = x/sum(x)
    y = as.numeric(approx[i,69:80])
    error[i,1] = mse(x,y)
  }
  
  approx = cbind(approx,error)
  
  correctclass = data.frame(correct = numeric())
  for(i in 1:n){
    y = as.numeric(approx[i,57:68])
    x = as.numeric(approx[i,69:80])
    temp = rep(0,12)
    temp[which.max(y)] = 1
    
    correctclass[i,1] = t(x) %*% temp
  }
  
  approx = cbind(approx,correctclass,row.names = NULL)
  
  truevalues = approx[which(approx$correct == 1),]
  
  truey1 = truevalues[which(truevalues$y1 == 1),]
  truey2 = truevalues[which(truevalues$y2 == 1),]
  truey3 = truevalues[which(truevalues$y3 == 1),]
  truey4 = truevalues[which(truevalues$y4 == 1),]
  truey5 = truevalues[which(truevalues$y5 == 1),]
  truey6 = truevalues[which(truevalues$y6 == 1),]
  truey7 = truevalues[which(truevalues$y7 == 1),]
  truey8 = truevalues[which(truevalues$y8 == 1),]
  truey9 = truevalues[which(truevalues$y9 == 1),]
  truey10 = truevalues[which(truevalues$y10 == 1),]
  truey11 = truevalues[which(truevalues$y11 == 1),]
  truey12 = truevalues[which(truevalues$y12 == 1),]
  
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
  
  for(i in 1:m){
    x = as.numeric(data2[i,1:4])
    data = NULL
    y2 = 10*as.numeric(data2[i,5:16])
    if(y2[1] == 10){
      data = truey1
      sc = sy1
    } else if(y2[2] == 10){
      data = truey2
      sc = sy2
    } else if(y2[3] == 10){
      data = truey3
      sc = sy3
    } else if(y2[4] == 10){
      data = truey4
      sc = sy4
    } else if(y2[5] == 10){
      data = truey5
      sc = sy5
    } else if(y2[6] == 10){
      data = truey6
      sc = sy6
    } else if(y2[7] == 10){
      data = truey7
      sc = sy7
    } else if(y2[8] == 10){
      data = truey8
      sc = sy8
    } else if(y2[9] == 10){
      data = truey9
      sc = sy9
    } else if(y2[10] == 10){
      data = truey10
      sc = sy10
    } else if(y2[11] == 10){
      data = truey11
      sc = sy11
    } else if(y2[12] == 10){
      data = truey12
      sc = sy12
    }
    p = sample(1:dim(data)[1],size=1,prob=sc)
    X1 = cbind(1*diag(20),x[1]*diag(20),x[2]*diag(20),x[3]*diag(20),x[4]*diag(20))
    y1 = as.numeric(data[p,5:24])
    x2 = as.numeric(data[p,25:44])
    X2 = cbind(1*diag(12),x2[1]*diag(12),x2[2]*diag(12),x2[3]*diag(12),x2[4]*diag(12),
               x2[5]*diag(12),x2[6]*diag(12),x2[7]*diag(12),x2[8]*diag(12),
               x2[9]*diag(12),x2[10]*diag(12),x2[11]*diag(12),x2[12]*diag(12),
               x2[13]*diag(12),x2[14]*diag(12),x2[15]*diag(12),x2[16]*diag(12),
               x2[17]*diag(12),x2[18]*diag(12),x2[19]*diag(12),x2[20]*diag(12))
    
    if(i>=1){
      A1 = t(X1)%*%X1
      A2 = t(X2)%*%X2
      B1 = t(X1)%*%y1
      B2 = t(X2)%*%y2
    }
    #if( i> 2){
    #  A1 = t(X1)%*%X1/(c-1+i) + (c-1+i-1)/(c-1+i)*A1
    #  A2 = t(X2)%*%X2/(c-1+i) + (c-1+i-1)/(c-1+i)*A2
    #  B1 = t(X1)%*%y1/(c-1+i) + (c-1+i-1)/(c-1+i)*B1
    #  B2 = t(X2)%*%y2/(c-1+i) + (c-1+i-1)/(c-1+i)*B2
    #}
    new_w1 = update(A1, B1,new_w1,i,25000+c,0.75)
    new_w2 = update(A2, B2,new_w2,i,25000+c,0.55)
    if(i%%1000 == 0){
      validationdata = read.table("shiftedvalidationset.txt", sep = "\t", header= TRUE)
      validationdata = validationdata[complete.cases(validationdata),]
      val_len = dim(validationdata)[1]
      estimates = data.frame(yhat1=numeric(),yhat2=numeric(),yhat3=numeric(),yhat4=numeric(),yhat5=numeric(),yhat6=numeric(),
                             yhat7=numeric(),yhat8=numeric(),yhat9=numeric(),yhat10=numeric(),yhat11=numeric(),yhat12=numeric())
      dim(validationdata)
      for (s in 1:val_len){
        x1 = as.numeric(validationdata[s,1:4])
        Xm = cbind(1*diag(20),x1[1]*diag(20),x1[2]*diag(20),x1[3]*diag(20),x1[4]*diag(20))
        output1 = Xm %*% new_w1
        x2 = c(rep(0,20))
        for(j in 1:20){
          x2[j] = leakyrelu(eps,output1[j])
        }
        Xm = cbind(1*diag(12),x2[1]*diag(12),x2[2]*diag(12),x2[3]*diag(12),x2[4]*diag(12),
                   x2[5]*diag(12),x2[6]*diag(12),x2[7]*diag(12),x2[8]*diag(12),
                   x2[9]*diag(12),x2[10]*diag(12),x2[11]*diag(12),x2[12]*diag(12),
                   x2[13]*diag(12),x2[14]*diag(12),x2[15]*diag(12),x2[16]*diag(12),
                   x2[17]*diag(12),x2[18]*diag(12),x2[19]*diag(12),x2[20]*diag(12))
        o2 = Xm %*% new_w2
        for(j in 1:12){
          estimates[s,j] = leakyrelu(eps,o2[j])
        }
        
      }
      validationdata=cbind(validationdata,estimates)
      
      correctclass = data.frame(correct = numeric())
      for(s in 1:val_len){
        y = as.numeric(validationdata[s,17:28])
        x = as.numeric(validationdata[s,5:16])
        temp = rep(0,12)
        temp[which.max(y)] = 1
        
        correctclass[s,1] = t(x) %*% temp
      }
      validationdata=cbind(validationdata,estimates,correctclass)
      new_acc = c(new_acc,sum(correctclass$correct))
      
      new_weight1_log = cbind(new_weight1_log,new_w1)
      new_weight2_log = cbind(new_weight2_log,new_w2)
      plot(new_acc/val_len,type = "l", col = "blue", xlab = "1000 Steps", ylab = "Accuracy")
      #plot(new_meanerror,type = "l", col = "red", xlab = "1000 Steps", ylab = "MSE")
    }
  }
}

#########################################################################################################################
# Plotting the shifted weights
#########################################################################################################################

library(scales) 
col_pal = rainbow(n=nrow(new_weight1_log))
plot(c(new_weight1_log), ylim=range(new_weight1_log), type='n', xlim=c(1,ncol(new_weight1_log)), main = "Weight Log Layer 1", xlab = "1000 Steps", ylab = "Weights") # type 'n' plots nothing
for(i in 1:length(new_w1)){
  points(new_weight1_log[i,], type='l', pch=19, col=alpha(col_pal[i], 0.5)) # alpha f'n sets colour opacity
}
col_pal = rainbow(n=nrow(new_weight2_log))
plot(c(new_weight2_log), ylim=range(new_weight2_log), type='n', xlim=c(1,ncol(new_weight2_log)), main = "Weight Log Layer 2", xlab = "1000 Steps", ylab = "Weights") # type 'n' plots nothing
for(i in 1:length(new_w2)){
  points(new_weight2_log[i,], type='l', pch=19, col=alpha(col_pal[i], 0.5)) # alpha f'n sets colour opacity
}



#######################################################################################################################
# writing talbes of accuracy
#######################################################################################################################
acc6590 = acc
write.table(acc6590, "acc6590.txt", sep="\t")


#######################################################################################################################
# Plotting Full NN results
#######################################################################################################################

NN_ACC = t(read.table("NN_ACC.csv",sep = ","))
NN_MSE = t(read.table("NN_MSE.csv",sep = ","))


plot(1:501,NN_ACC/6000, type="l", col = "red", xlab="1000 Steps", ylab = "Accuracy")
plot(1:501,NN_MSE, type="l", col = "red", xlab="1000 Steps", ylab = "MSE")

