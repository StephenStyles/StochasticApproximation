setwd("C:/Users/sjsty/Desktop/Masters/Algorithms/Simplified Problem")

checkerboard <- function(x1,x2,x3,x4){
  if( abs(x2-x3)<0.15 && abs(x1-x4)<0.15){
    return(1)
  }
  else{
    return(0)
  }
}

hline <- function(x1,x2,x3,x4){
  if( abs(x1-x3)>0.2 && abs(x2-x4)>0.2){
    return(2)
  }
  else{
    return(0)
  }
}

vline <- function(x1,x2,x3,x4){
  if( abs(x1-x2)>0.2 && abs(x3-x4)>0.2){
    return(3)
  }
  else{
    return(0)
  }
}


# Set the number of observations for each classification
k = 2

# Set the proportion you want of intense vs non intense observations
p = 0.5
n1 = (1-p)*k
n2 = p*k

# Set upper lower limit on intensity observations
l = 0.35

# High intensity diagonal line

intdiagdata = data.frame(x1_1=numeric(),x1_2=numeric(),x1_3=numeric(),x1_4=numeric(),y1=numeric(),y2=numeric(),y3=numeric(), y4 = numeric(),
                    y5=numeric(),y6=numeric(),y7=numeric(), y8 = numeric(), y9=numeric(),y10=numeric(),y11=numeric(), y12 = numeric())
i=0
while(i <= n1){
  x = round(c(runif(1,0,1), runif(1,0,1), runif(1,0,1),runif(1,0,1)),2)
  if(mean(c(x[1],x[4]))-mean(c(x[2],x[3]))>l){
    temp = checkerboard(x[1],x[2],x[3],x[4])
    if(temp == 1){
      y = c(1,0,0,0,0,0,0,0,0,0,0,0)
      intdiagdata[i,] = c(x,y)
      i=i+1
    }
  } 
}

# Low intensity diagonal line 

nonintdiagdata = data.frame(x1_1=numeric(),x1_2=numeric(),x1_3=numeric(),x1_4=numeric(),y1=numeric(),y2=numeric(),y3=numeric(), y4 = numeric(),
                         y5=numeric(),y6=numeric(),y7=numeric(), y8 = numeric(), y9=numeric(),y10=numeric(),y11=numeric(), y12 = numeric())
i=0
while(i <= n2){
  x = round(c(runif(1,0,1), runif(1,0,1), runif(1,0,1),runif(1,0,1)),2)
  if(0.2 <= mean(c(x[1],x[4]))-mean(c(x[2],x[3])) &  mean(c(x[1],x[4]))-mean(c(x[2],x[3])) <= l){
    temp = checkerboard(x[1],x[2],x[3],x[4])
    if(temp == 1){
      y = c(0,1,0,0,0,0,0,0,0,0,0,0)
      nonintdiagdata[i,] = c(x,y)
      i=i+1
    }
  } 
}


# High intensity off diagonal line

intoffdiagdata = data.frame(x1_1=numeric(),x1_2=numeric(),x1_3=numeric(),x1_4=numeric(),y1=numeric(),y2=numeric(),y3=numeric(), y4 = numeric(),
                         y5=numeric(),y6=numeric(),y7=numeric(), y8 = numeric(), y9=numeric(),y10=numeric(),y11=numeric(), y12 = numeric())
i=0
while(i <= n1){
  x = round(c(runif(1,0,1), runif(1,0,1), runif(1,0,1),runif(1,0,1)),2)
  if(mean(c(x[2],x[3]))-mean(c(x[1],x[4]))>l){
    temp = checkerboard(x[1],x[2],x[3],x[4])
    if(temp == 1){
        y = c(0,0,1,0,0,0,0,0,0,0,0,0)
      intoffdiagdata[i,] = c(x,y)
      i=i+1
    }
  } 
}


# Low intesity off diagonal line

nonintoffdiagdata = data.frame(x1_1=numeric(),x1_2=numeric(),x1_3=numeric(),x1_4=numeric(),y1=numeric(),y2=numeric(),y3=numeric(), y4 = numeric(),
                            y5=numeric(),y6=numeric(),y7=numeric(), y8 = numeric(), y9=numeric(),y10=numeric(),y11=numeric(), y12 = numeric())
i=0
while(i <= n2){
  x = round(c(runif(1,0,1), runif(1,0,1), runif(1,0,1),runif(1,0,1)),2)
  if(0.2 <= mean(c(x[2],x[3]))-mean(c(x[1],x[4])) &  mean(c(x[2],x[3]))-mean(c(x[1],x[4])) <= l){
    temp = checkerboard(x[1],x[2],x[3],x[4])
    if(temp == 1){
      y = c(0,0,0,1,0,0,0,0,0,0,0,0)
      nonintoffdiagdata[i,] = c(x,y)
      i=i+1
    }
  } 
}


# High intensity top horizontal line 

int_tophdata = data.frame(x1_1=numeric(),x1_2=numeric(),x1_3=numeric(),x1_4=numeric(),y1=numeric(),y2=numeric(),y3=numeric(), y4 = numeric(),
                          y5=numeric(),y6=numeric(),y7=numeric(), y8 = numeric(), y9=numeric(),y10=numeric(),y11=numeric(), y12 = numeric())
i=0

while(i <= n1){
  x = round(c(runif(1,0,1), runif(1,0,1), runif(1,0,1),runif(1,0,1)),2)
  line = abs(x[1]-x[2])
  if(line<0.15){
    m = min(x[1],x[2])
    if(m-x[3]> l & m - x[4]> l){
      y = c(0,0,0,0,1,0,0,0,0,0,0,0)
      int_tophdata[i,] = c(x,y)
      i=i+1
    }
  } 
}


# Low intensity top horizontal line

nonint_tophdata = data.frame(x1_1=numeric(),x1_2=numeric(),x1_3=numeric(),x1_4=numeric(),y1=numeric(),y2=numeric(),y3=numeric(), y4 = numeric(),
                          y5=numeric(),y6=numeric(),y7=numeric(), y8 = numeric(), y9=numeric(),y10=numeric(),y11=numeric(), y12 = numeric())
i=0

while(i <= n2){
  x = round(c(runif(1,0,1), runif(1,0,1), runif(1,0,1),runif(1,0,1)),2)
  line = abs(x[1]-x[2])
  if(line<0.15){
    m = min(x[1],x[2])
    if(0.2 <= m-x[3]& m-x[3]  <= l & 0.2 <= m-x[4]& m-x[4]  <= l){
      y = c(0,0,0,0,0,1,0,0,0,0,0,0)
      nonint_tophdata[i,] = c(x,y)
      i=i+1
    }
  } 
}

# High intensity bottom horizontal line 

int_bothdata = data.frame(x1_1=numeric(),x1_2=numeric(),x1_3=numeric(),x1_4=numeric(),y1=numeric(),y2=numeric(),y3=numeric(), y4 = numeric(),
                          y5=numeric(),y6=numeric(),y7=numeric(), y8 = numeric(), y9=numeric(),y10=numeric(),y11=numeric(), y12 = numeric())
i=0

while(i <= n1){
  x = round(c(runif(1,0,1), runif(1,0,1), runif(1,0,1),runif(1,0,1)),2)
  line = abs(x[3]-x[4])
  if(line<0.15){
    m = min(x[3],x[4])
    if(m-x[1]>l & m - x[2]> l){
      y = c(0,0,0,0,0,0,1,0,0,0,0,0)
      int_bothdata[i,] = c(x,y)
      i=i+1
    }
  } 
}

# Low intensity bottom horizontal line

nonint_bothdata = data.frame(x1_1=numeric(),x1_2=numeric(),x1_3=numeric(),x1_4=numeric(),y1=numeric(),y2=numeric(),y3=numeric(), y4 = numeric(),
                             y5=numeric(),y6=numeric(),y7=numeric(), y8 = numeric(), y9=numeric(),y10=numeric(),y11=numeric(), y12 = numeric())
i=0

while(i <= n2){
  x = round(c(runif(1,0,1), runif(1,0,1), runif(1,0,1),runif(1,0,1)),2)
  line = abs(x[3]-x[4])
  if(line<0.15){
    m = min(x[3],x[4])
    if(0.2 <= m-x[1]& m-x[1]  <= l & 0.2 <= m-x[2]& m-x[2]  <= l){
      y = c(0,0,0,0,0,0,0,1,0,0,0,0)
      nonint_bothdata[i,] = c(x,y)
      i=i+1
    }
  } 
}


# High Intensity left vertical line

int_leftvdata = data.frame(x1_1=numeric(),x1_2=numeric(),x1_3=numeric(),x1_4=numeric(),y1=numeric(),y2=numeric(),y3=numeric(), y4 = numeric(),
                          y5=numeric(),y6=numeric(),y7=numeric(), y8 = numeric(), y9=numeric(),y10=numeric(),y11=numeric(), y12 = numeric())
i=0

while(i <= n1){
  x = round(c(runif(1,0,1), runif(1,0,1), runif(1,0,1),runif(1,0,1)),2)
  line = abs(x[1]-x[3])
  if(line<0.15){
    m = min(x[1],x[3])
    if(m-x[2]>l & m - x[4]> l){
      y = c(0,0,0,0,0,0,0,0,1,0,0,0)
      int_leftvdata[i,] = c(x,y)
      i=i+1
    }
  } 
}

# Low intensity left vertical line

nonint_leftvdata = data.frame(x1_1=numeric(),x1_2=numeric(),x1_3=numeric(),x1_4=numeric(),y1=numeric(),y2=numeric(),y3=numeric(), y4 = numeric(),
                             y5=numeric(),y6=numeric(),y7=numeric(), y8 = numeric(), y9=numeric(),y10=numeric(),y11=numeric(), y12 = numeric())
i=0

while(i <= n2){
  x = round(c(runif(1,0,1), runif(1,0,1), runif(1,0,1),runif(1,0,1)),2)
  line = abs(x[1]-x[3])
  if(line<0.15){
    m = min(x[1],x[3])
    if(0.2 <= m-x[2]& m-x[2]  <= l & 0.2 <= m-x[4]& m-x[4]  <= l){
      y = c(0,0,0,0,0,0,0,0,0,1,0,0)
      nonint_leftvdata[i,] = c(x,y)
      i=i+1
    }
  } 
}


# High Intensity right vertical line

int_rightvdata = data.frame(x1_1=numeric(),x1_2=numeric(),x1_3=numeric(),x1_4=numeric(),y1=numeric(),y2=numeric(),y3=numeric(), y4 = numeric(),
                           y5=numeric(),y6=numeric(),y7=numeric(), y8 = numeric(), y9=numeric(),y10=numeric(),y11=numeric(), y12 = numeric())
i=0

while(i <= n1){
  x = round(c(runif(1,0,1), runif(1,0,1), runif(1,0,1),runif(1,0,1)),2)
  line = abs(x[2]-x[4])
  if(line<0.15){
    m = min(x[2],x[4])
    if(m-x[1]>l & m - x[3]> l){
      y = c(0,0,0,0,0,0,0,0,0,0,1,0)
      int_rightvdata[i,] = c(x,y)
      i=i+1
    }
  } 
}


# Low intensity right vertical line

nonint_rightvdata = data.frame(x1_1=numeric(),x1_2=numeric(),x1_3=numeric(),x1_4=numeric(),y1=numeric(),y2=numeric(),y3=numeric(), y4 = numeric(),
                              y5=numeric(),y6=numeric(),y7=numeric(), y8 = numeric(), y9=numeric(),y10=numeric(),y11=numeric(), y12 = numeric())
i=0

while(i <= n2){
  x = round(c(runif(1,0,1), runif(1,0,1), runif(1,0,1),runif(1,0,1)),2)
  line = abs(x[2]-x[4])
  if(line<0.15){
    m = min(x[2],x[4])
    if(0.2 <= m-x[1]& m-x[1]  <= l & 0.2 <= m-x[3]& m-x[3]  <= l){
      y = c(0,0,0,0,0,0,0,0,0,0,0,1)
      nonint_rightvdata[i,] = c(x,y)
      i=i+1
    }
  } 
}


data = rbind(intdiagdata, nonintdiagdata, intoffdiagdata, nonintoffdiagdata,
             int_tophdata, nonint_tophdata, int_bothdata, nonint_bothdata,
             int_leftvdata, nonint_leftvdata, int_rightvdata, nonint_rightvdata)


row_numbers = 6*n1+6*n2
rows = sample(row_numbers)
data = data[rows,]
rownames(data) = NULL
fix(data)

# Plotting Image

par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 4.1))
library('plot.matrix')
samp = as.numeric(data[i,c(1:4)])
x = t(matrix(samp, nrow=2))
bks = seq(from = 0,to=1,by=0.05)
plot(x,breaks=bks,col= colorRampPalette(c("white","black")),
     main = "", key=NULL)

