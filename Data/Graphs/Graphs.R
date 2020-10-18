setwd("C:/Users/sjsty/Desktop/Masters/Algorithms/Simplified Problem")

####################################################################################################################
# Reading the Neural Network accuracy and all the mini-batch SA trials
####################################################################################################################

NN_ACC = t(read.table("finalACC.csv",sep = ","))

Batch1 = t(read.table("finalSA_ACC.txt", sep="\t"))
Batch10 = t(read.table("finalSA_ACCminibatch10.txt", sep="\t"))
Batch25 = t(read.table("finalSA_ACCminibatch25.txt", sep="\t"))
Batch40 = t(read.table("finalSA_ACCminibatch40.txt", sep="\t"))

####################################################################################################################
# Plotting NN vs SA
####################################################################################################################

plot(1:501, NN_ACC[1:501]/6000, type = "l", col = "red", xlab = "1000 Steps", ylab = "Accuracy",ylim=c(0.5,1))
lines(1:501, Batch1[1:501]/6000, type = "l", col = "blue")
legend("bottomright", legend = c("ADAM","SA - Individual Sample"),
       col = c("red", "blue"), lty=1)

####################################################################################################################
# Plotting mini-batches
####################################################################################################################

plot(1:251, NN_ACC[1:251]/6000, type = "l", col = "red", xlab = "1000 Steps", ylab = "Accuracy",ylim=c(0.5,1))
lines(1:251, Batch1[1:251]/6000, type = "l", col = "blue")
lines(1:251, Batch10/6000, type = "l", col = "black")
lines(1:251, Batch25/6000, type = "l", col = "purple")
lines(1:251, Batch40/6000, type = "l", col = "green")
legend("bottomright", legend = c("ADAM","SA - Individual Sample","SA - Batch Size 10","SA - Batch Size 25","SA - Batch Size 40"),
       col = c("red", "blue", "black", "purple","green"), lty=1)

####################################################################################################################
# Reading and plotting momentum
####################################################################################################################

Momentum10 = t(read.table("finalSA_ACCminibatch10_momentum.txt", sep="\t"))
Momentum25 = t(read.table("finalSA_ACCminibatch25_momentum.txt", sep="\t"))

plot(1:251, NN_ACC[1:251]/6000, type = "l", col = "red", xlab = "1000 Steps", ylab = "Accuracy",ylim=c(0.5,1))
lines(1:251, Batch1[1:251]/6000, type = "l", col = "blue")
lines(1:251, Momentum10/6000, type = "l", col = "black")
lines(1:251, Momentum25/6000, type = "l", col = "purple")
legend("bottomright", legend = c("ADAM","SA - Individual Sample","SA - Batch Size 10 - Momentum","SA - Batch Size 25 - Momentum"),
       col = c("red", "blue", "black", "purple"), lty=1)

####################################################################################################################
# Adding RMSProp to the previous algorithm and plotting
####################################################################################################################

ADAM = t(read.table("finalSA_ACCminibatch10_adam.txt", sep="\t"))
plot(1:251, NN_ACC[1:251]/6000, type = "l", col = "red", xlab = "1000 Steps", ylab = "Accuracy",ylim=c(0.5,1))
lines(1:251, Batch1[1:251]/6000, type = "l", col = "blue")
lines(1:251, ADAM/6000, type = "l", col = "black")
legend("bottomright", legend = c("ADAM","SA - Individual Sample","SA - Batch Size 10 - Momentum + RMSProp"),
       col = c("red", "blue", "black"), lty=1)
