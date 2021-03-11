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

momentum <- function(m,x,i,beta){
  temp = beta*m + (1-beta)*x
  return(temp)
}

rmsprop <- function(r,x,i,beta){
  temp = beta*r + (1-beta)*x^2
  return(temp)
}

##########################################################################################################
# Initial Data to estimate
##########################################################################################################

SAdata = read.table("12outputtraindata.txt", sep = "\t", header= TRUE)
row.names(SAdata) = NULL

smp_size <- floor(0.75 * nrow(SAdata))

train_ind <- sample(seq_len(nrow(SAdata)), size = smp_size)
traindata = SAdata[train_ind,1:4]
trainlabels = SAdata[train_ind,5:16]
row.names(traindata) = NULL
row.names(trainlabels) = NULL
testdata = SAdata[-train_ind,1:4]
testlabels = SAdata[-train_ind,5:16]
row.names(testdata) = NULL
row.names(testlabels) = NULL


eps = 0.0001

validationdata = read.table("12outputNNdata.txt", sep = "\t", header= TRUE)

library(keras)

model <- keras_model_sequential()
model %>%
  layer_dense(units = 20) %>%
  layer_activation_leaky_relu(alpha=eps) %>%
  layer_dense(units = 20) %>%
  layer_activation_leaky_relu(alpha=eps) %>%
  layer_dense(units = 12) %>%
  layer_activation_softmax()

model %>% compile(
  optimizer = 'adam', 
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)



model %>% fit(as.matrix(testdata[295000:300000,1:4]), as.matrix(testlabels[295000:300000,1:12]), epochs = 4)

w1 = c(get_weights(model)[[2]],get_weights(model)[[1]][1,],get_weights(model)[[1]][2,],get_weights(model)[[1]][3,],get_weights(model)[[1]][4,])
w2 = c(get_weights(model)[[4]],get_weights(model)[[3]][1,],get_weights(model)[[3]][2,],get_weights(model)[[3]][3,],get_weights(model)[[3]][4,],
       get_weights(model)[[3]][5,],get_weights(model)[[3]][6,],get_weights(model)[[3]][7,],get_weights(model)[[3]][8,],get_weights(model)[[3]][9,],
       get_weights(model)[[3]][10,],get_weights(model)[[3]][11,],get_weights(model)[[3]][12,],get_weights(model)[[3]][13,],get_weights(model)[[3]][14,],
       get_weights(model)[[3]][15,],get_weights(model)[[3]][16,],get_weights(model)[[3]][17,],get_weights(model)[[3]][18,],get_weights(model)[[3]][19,],
       get_weights(model)[[3]][20,])
w3 = c(get_weights(model)[[6]],get_weights(model)[[5]][1,],get_weights(model)[[5]][2,],get_weights(model)[[5]][3,],get_weights(model)[[5]][4,],
       get_weights(model)[[5]][5,],get_weights(model)[[5]][6,],get_weights(model)[[5]][7,],get_weights(model)[[5]][8,],get_weights(model)[[5]][9,],
       get_weights(model)[[5]][10,],get_weights(model)[[5]][11,],get_weights(model)[[5]][12,],get_weights(model)[[5]][13,],get_weights(model)[[5]][14,],
       get_weights(model)[[5]][15,],get_weights(model)[[5]][16,],get_weights(model)[[5]][17,],get_weights(model)[[5]][18,],get_weights(model)[[5]][19,],
       get_weights(model)[[5]][20,])


w3check = w3
acc = NULL

m=30000
n=10000

it=1

for(k in 1:10){
  a = 1 + m*(k-1)
  b = m*k
  c = 1 + n*(k-1)
  d = n*k
  
  data1 = traindata[a:b,]
  data2 = testdata[c:d,]
  data1labels = trainlabels[a:b,]
  data2labels = testlabels[c:d,]
  
  
  inputs1 = data.frame(x1_1=numeric(),x1_2=numeric(),x1_3=numeric(),x1_4=numeric())
  outputs1 = data.frame(h1=numeric(),h2=numeric(),h3=numeric(),h4=numeric(),h5=numeric(),
                        h6=numeric(),h7=numeric(),h8=numeric(),h9=numeric(),h10=numeric(),
                        h11=numeric(),h12=numeric(),h13=numeric(),h14=numeric(),h15=numeric(),
                        h16=numeric(),h17=numeric(),h18=numeric(),h19=numeric(),h20=numeric())
  inputs2 = data.frame(x2_1=numeric(),x2_2=numeric(),x2_3=numeric(),x2_4=numeric(),x2_5=numeric(),
                       x2_6=numeric(),x2_7=numeric(),x2_8=numeric(),x2_9=numeric(),x2_10=numeric(),
                       x2_11=numeric(),x2_12=numeric(),x2_13=numeric(),x2_14=numeric(),x2_15=numeric(),
                       x2_16=numeric(),x2_17=numeric(),x2_18=numeric(),x2_19=numeric(),x2_20=numeric())
  outputs2 =  data.frame(h1=numeric(),h2=numeric(),h3=numeric(),h4=numeric(),h5=numeric(),
                         h6=numeric(),h7=numeric(),h8=numeric(),h9=numeric(),h10=numeric(),
                         h11=numeric(),h12=numeric(),h13=numeric(),h14=numeric(),h15=numeric(),
                         h16=numeric(),h17=numeric(),h18=numeric(),h19=numeric(),h20=numeric())
  inputs3 = data.frame(x3_1=numeric(),x3_2=numeric(),x3_3=numeric(),x3_4=numeric(),x3_5=numeric(),
                       x3_6=numeric(),x3_7=numeric(),x3_8=numeric(),x3_9=numeric(),x3_10=numeric(),
                       x3_11=numeric(),x3_12=numeric(),x3_13=numeric(),x3_14=numeric(),x3_15=numeric(),
                       x3_16=numeric(),x3_17=numeric(),x3_18=numeric(),x3_19=numeric(),x3_20=numeric())
  outputs3 = data.frame(o1=numeric(),o2=numeric(),o3=numeric(),o4=numeric(),o5=numeric(),o6=numeric(),
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
    Xm = cbind(1*diag(20),x2[1]*diag(20),x2[2]*diag(20),x2[3]*diag(20),x2[4]*diag(20),
               x2[5]*diag(20),x2[6]*diag(20),x2[7]*diag(20),x2[8]*diag(20),
               x2[9]*diag(20),x2[10]*diag(20),x2[11]*diag(20),x2[12]*diag(20),
               x2[13]*diag(20),x2[14]*diag(20),x2[15]*diag(20),x2[16]*diag(20),
               x2[17]*diag(20),x2[18]*diag(20),x2[19]*diag(20),x2[20]*diag(20))
    outputs2[i,] = Xm %*% w2
    for(j in 1:20){
      inputs3[i,j] = leakyrelu(eps,outputs2[i,j])
    }
    x3 =  as.numeric(inputs3[i,1:20])
    Xm = cbind(1*diag(12),x3[1]*diag(12),x3[2]*diag(12),x3[3]*diag(12),x3[4]*diag(12),
               x3[5]*diag(12),x3[6]*diag(12),x3[7]*diag(12),x3[8]*diag(12),
               x3[9]*diag(12),x3[10]*diag(12),x3[11]*diag(12),x3[12]*diag(12),
               x3[13]*diag(12),x3[14]*diag(12),x3[15]*diag(12),x3[16]*diag(12),
               x3[17]*diag(12),x3[18]*diag(12),x3[19]*diag(12),x3[20]*diag(12))
    outputs3[i,] = Xm %*% w3check
    for(j in 1:12){
      finalest[i,j] = leakyrelu(eps,outputs3[i,j])
    }
    trueval[i,] = data1labels[i,1:12]
  }
  approx = cbind(inputs1,outputs1,inputs2,outputs2,inputs3,outputs3,finalest,trueval)
  
  error = data.frame(err = numeric())
  for(i in 1:n){
    x = as.numeric(approx[i,97:108])
    x = x/sum(x)
    y = as.numeric(approx[i,109:120])
    error[i,1] = mse(x,y)
  }
  approx = cbind(approx,error)
  
  correctclass = data.frame(correct = numeric())
  for(i in 1:n){
    y = as.numeric(approx[i,97:108])
    x = as.numeric(approx[i,109:120])
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
  
  sy1 = sapply(truey1[,121],score);sy1 = sy1/sum(sy1)
  sy2 = sapply(truey2[,121],score);sy2 = sy2/sum(sy2)
  sy3 = sapply(truey3[,121],score);sy3 = sy3/sum(sy3)
  sy4 = sapply(truey4[,121],score);sy4 = sy4/sum(sy4)
  sy5 = sapply(truey5[,121],score);sy5 = sy5/sum(sy5)
  sy6 = sapply(truey6[,121],score);sy6 = sy6/sum(sy6)
  sy7 = sapply(truey7[,121],score);sy7 = sy7/sum(sy7)
  sy8 = sapply(truey8[,121],score);sy8 = sy8/sum(sy8)
  sy9 = sapply(truey9[,121],score);sy9 = sy9/sum(sy9)
  sy10 = sapply(truey10[,121],score);sy10 = sy10/sum(sy10)
  sy11 = sapply(truey11[,121],score);sy11 = sy11/sum(sy11)
  sy12 = sapply(truey12[,121],score);sy12 = sy12/sum(sy12)
  
  model %>% fit(as.matrix(testdata[c:d,1:4]), as.matrix(testlabels[c:d,1:12]), epochs = 1, verbose = 2)
  
  w1 = c(get_weights(model)[[2]],get_weights(model)[[1]][1,],get_weights(model)[[1]][2,],get_weights(model)[[1]][3,],get_weights(model)[[1]][4,])
  w2 = c(get_weights(model)[[4]],get_weights(model)[[3]][1,],get_weights(model)[[3]][2,],get_weights(model)[[3]][3,],get_weights(model)[[3]][4,],
         get_weights(model)[[3]][5,],get_weights(model)[[3]][6,],get_weights(model)[[3]][7,],get_weights(model)[[3]][8,],get_weights(model)[[3]][9,],
         get_weights(model)[[3]][10,],get_weights(model)[[3]][11,],get_weights(model)[[3]][12,],get_weights(model)[[3]][13,],get_weights(model)[[3]][14,],
         get_weights(model)[[3]][15,],get_weights(model)[[3]][16,],get_weights(model)[[3]][17,],get_weights(model)[[3]][18,],get_weights(model)[[3]][19,],
         get_weights(model)[[3]][20,])
  
  for(i in 1:n){
    data = NULL
    y3 = 10*as.numeric(data2labels[i,1:12])
    if(y3[1] == 10){
      data = truey1;sc = sy1
    } else if(y3[2] == 10){
      data = truey2;sc = sy2
    } else if(y3[3] == 10){
      data = truey3;sc = sy3
    } else if(y3[4] == 10){
      data = truey4;sc = sy4
    } else if(y3[5] == 10){
      data = truey5;sc = sy5
    } else if(y3[6] == 10){
      data = truey6;sc = sy6
    } else if(y3[7] == 10){
      data = truey7;sc = sy7
    } else if(y3[8] == 10){
      data = truey8;sc = sy8
    } else if(y3[9] == 10){
      data = truey9;sc = sy9
    } else if(y3[10] == 10){
      data = truey10;sc = sy10
    } else if(y3[11] == 10){
      data = truey11;sc = sy11
    } else if(y3[12] == 10){
      data = truey12;sc = sy12
    }
    
    q = sample(1:dim(data)[1],size=1,prob=sc)
    x3 = as.numeric(data[q,65:84])
    X3 = cbind(1*diag(12),x2[1]*diag(12),x2[2]*diag(12),x2[3]*diag(12),x2[4]*diag(12),
               x2[5]*diag(12),x2[6]*diag(12),x2[7]*diag(12),x2[8]*diag(12),
               x2[9]*diag(12),x2[10]*diag(12),x2[11]*diag(12),x2[12]*diag(12),
               x2[13]*diag(12),x2[14]*diag(12),x2[15]*diag(12),x2[16]*diag(12),
               x2[17]*diag(12),x2[18]*diag(12),x2[19]*diag(12),x2[20]*diag(12))
    
    
    if(it==1){
      A3check = t(X3)%*%X3
      B3check = t(X3)%*%y3
    }
    
    A3 = t(X3)%*%X3
    B3 = t(X3)%*%y3
    
    A3check = A3check +(A3 -A3check)/(it+1)
    B3check = B3check +(B3 -B3check)/(it+1)
    
    w3 = w3 + (1/20)*(B3check-A3check%*%w3)
    
    w3check = w3check + (w3-w3check)/(it+1)
    
    it=it+1
    
    if(i%%1000==0){
      tempvalidationdata = validationdata
      estimates = data.frame(yhat1=numeric(),yhat2=numeric(),yhat3=numeric(),yhat4=numeric(),yhat5=numeric(),yhat6=numeric(),
                             yhat7=numeric(),yhat8=numeric(),yhat9=numeric(),yhat10=numeric(),yhat11=numeric(),yhat12=numeric())
      
      for (s in 1:6000){
        x1 = as.numeric(tempvalidationdata[s,1:4])
        Xm = cbind(1*diag(20),x1[1]*diag(20),x1[2]*diag(20),x1[3]*diag(20),x1[4]*diag(20))
        output1 = Xm %*% w1
        x2 = c(rep(0,20))
        for(j in 1:20){
          x2[j] = leakyrelu(eps,output1[j])
        }
        Xm = cbind(1*diag(20),x2[1]*diag(20),x2[2]*diag(20),x2[3]*diag(20),x2[4]*diag(20),
                   x2[5]*diag(20),x2[6]*diag(20),x2[7]*diag(20),x2[8]*diag(20),
                   x2[9]*diag(20),x2[10]*diag(20),x2[11]*diag(20),x2[12]*diag(20),
                   x2[13]*diag(20),x2[14]*diag(20),x2[15]*diag(20),x2[16]*diag(20),
                   x2[17]*diag(20),x2[18]*diag(20),x2[19]*diag(20),x2[20]*diag(20))
        output2 = Xm %*% w2
        x3 = c(rep(0,20))
        for(j in 1:20){
          x3[j] = leakyrelu(eps,output2[j])
        }
        Xm = cbind(1*diag(12),x3[1]*diag(12),x3[2]*diag(12),x3[3]*diag(12),x3[4]*diag(12),
                        x3[5]*diag(12),x3[6]*diag(12),x3[7]*diag(12),x3[8]*diag(12),
                        x3[9]*diag(12),x3[10]*diag(12),x3[11]*diag(12),x3[12]*diag(12),
                        x3[13]*diag(12),x3[14]*diag(12),x3[15]*diag(12),x3[16]*diag(12),
                        x3[17]*diag(12),x3[18]*diag(12),x3[19]*diag(12),x3[20]*diag(12))
        output3 = Xm %*% w3check
        for(j in 1:12){
          estimates[s,j] = leakyrelu(eps,output3[j])
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
    
    weight1 = array(get_weights(model)[[1]], dim = c(4,20))
    bias1 = array(get_weights(model)[[2]])
    weight2 = array(get_weights(model)[[3]], dim = c(20,20))
    bias2 = array(get_weights(model)[[4]])
    weight3 = array(matrix(w3check[13:length(w3check)],nrow=20), dim=c(20,12))
    bias3 = array(w3check[1:12])
    
    set_weights(model,list(weight1, bias1, weight2, bias2, weight3, bias3))
  }
}
