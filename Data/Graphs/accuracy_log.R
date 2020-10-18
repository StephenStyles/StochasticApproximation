setwd("C:/Users/sjsty/Desktop/Masters/Algorithms/Simplified Problem")

ac6565 = read.table("acc6565.txt", sep="\t")
ac6570 = read.table("acc6570.txt", sep="\t")
ac6575 = read.table("acc6575.txt", sep="\t")
ac6580 = read.table("acc6580.txt", sep="\t")
ac6585 = read.table("acc6585.txt", sep="\t")
ac6590 = read.table("acc6590.txt", sep="\t")

accuracylog65 = matrix(c(c(ac6565[,1], rep(5230,395)),c(ac6570[,1], rep(5208,350)),c(ac6575[,1],rep(4846,274)),
                       c(ac6580[,1],rep(4700,29)),c(ac6585[,1],rep(3686,458)),c(ac6590[,1],rep(3752,420)))
                     ,ncol=6)

ac7065 = read.table("acc7065.txt", sep="\t")
ac7070 = read.table("acc7070.txt", sep="\t")
ac7075 = read.table("acc7075.txt", sep="\t")
ac7080 = read.table("acc7080.txt", sep="\t")
ac7085 = read.table("acc7085.txt", sep="\t")
ac7090 = read.table("acc7090.txt", sep="\t")

accuracylog70 = matrix(c(c(ac7065[,1],rep(5239,180)), ac7070[,1],ac7075[,1],
                         ac7080[,1],ac7085[,1],c(ac7090[,1],rep(3542,432)))
                       ,ncol=6)
                       
ac7565 = read.table("acc7565.txt", sep="\t")
ac7570 = read.table("acc7570.txt", sep="\t")
ac7575 = read.table("acc7575.txt", sep="\t")
ac7580 = read.table("acc7580.txt", sep="\t")
ac7585 = read.table("acc7585.txt", sep="\t")
ac7590 = read.table("acc7590.txt", sep="\t")

accuracylog75 = matrix(c(ac7565[,1], ac7570[,1],ac7575[,1],
                         ac7580[,1],ac7585[,1],ac7590[,1])
                       ,ncol=6)

ac8065 = read.table("acc8065.txt", sep="\t")
ac8070 = read.table("acc8070.txt", sep="\t")
ac8075 = read.table("acc8075.txt", sep="\t")
ac8080 = read.table("acc8080.txt", sep="\t")
ac8085 = read.table("acc8085.txt", sep="\t")
ac8090 = read.table("acc8090.txt", sep="\t")

accuracylog80 = matrix(c(ac8065[,1], ac8070[,1],ac8075[,1],
                         ac8080[,1],ac8085[,1],ac8090[,1])
                       ,ncol=6)

ac8565 = read.table("acc8565.txt", sep="\t")
ac8570 = read.table("acc8570.txt", sep="\t")
ac8575 = read.table("acc8575.txt", sep="\t")
ac8580 = read.table("acc8580.txt", sep="\t")
ac8585 = read.table("acc8585.txt", sep="\t")
ac8590 = read.table("acc8590.txt", sep="\t")

accuracylog85 = matrix(c(ac8565[,1], ac8570[,1],ac8575[,1],
                         ac8580[,1],ac8585[,1],ac8590[,1])
                       ,ncol=6)

ac9065 = read.table("acc9065.txt", sep="\t")
ac9070 = read.table("acc9070.txt", sep="\t")
ac9075 = read.table("acc9075.txt", sep="\t")
ac9080 = read.table("acc9080.txt", sep="\t")
ac9085 = read.table("acc9085.txt", sep="\t")
ac9090 = read.table("acc9090.txt", sep="\t")

accuracylog90 = matrix(c(ac9065[,1], ac9070[,1],ac9075[,1],
                         ac9080[,1],ac9085[,1],ac9090[,1])
                       ,ncol=6)

accuracylog =matrix(c(accuracylog65,accuracylog70,accuracylog75,
               accuracylog80,accuracylog85,accuracylog90), ncol=36)/6000

################################################################################################
# Setting up the chi_1 = 0.65 case
################################################################################################

plot(1:501, accuracylog65[,1]/6000, type = "l", col = "red", xlab = "1000 Steps", ylab = "Accuracy")
lines(1:501, accuracylog65[,2]/6000, type = "l", col = "yellow")
lines(1:501, accuracylog65[,3]/6000, type = "l", col = "green")
lines(1:501, accuracylog65[,4]/6000, type = "l", col = "blue")
lines(1:501, accuracylog65[,5]/6000, type = "l", col = "purple")
lines(1:501, accuracylog65[,6]/6000, type = "l", col = "black")
legend("bottomright", legend = c("Chi_2 = 0.65","Chi_2 = 0.70","Chi_2 = 0.75","Chi_2 = 0.80","Chi_2 = 85","Chi_2 = 0.90"),
       col = c("red", "yellow", "green", "blue", "purple", "black"), lty=1)


################################################################################################
# Setting up the chi_1 = 0.7 case
################################################################################################

plot(1:501, accuracylog70[,1]/6000, type = "l", col = "red", xlab = "1000 Steps", ylab = "Accuracy")
lines(1:501, accuracylog70[,2]/6000, type = "l", col = "yellow")
lines(1:501, accuracylog70[,3]/6000, type = "l", col = "green")
lines(1:501, accuracylog70[,4]/6000, type = "l", col = "blue")
lines(1:501, accuracylog70[,5]/6000, type = "l", col = "purple")
lines(1:501, accuracylog70[,6]/6000, type = "l", col = "black")
legend("bottomright", legend = c("Chi_2 = 0.65","Chi_2 = 0.70","Chi_2 = 0.75","Chi_2 = 0.80","Chi_2 = 85","Chi_2 = 0.90"),
       col = c("red", "yellow", "green", "blue", "purple", "black"), lty=1)


################################################################################################
# Setting up the chi_1 = 0.75 case
################################################################################################

plot(1:501, accuracylog75[,1]/6000, type = "l", col = "red", xlab = "1000 Steps", ylab = "Accuracy")
lines(1:501, accuracylog75[,2]/6000, type = "l", col = "yellow")
lines(1:501, accuracylog75[,3]/6000, type = "l", col = "green")
lines(1:501, accuracylog75[,4]/6000, type = "l", col = "blue")
lines(1:501, accuracylog75[,5]/6000, type = "l", col = "purple")
lines(1:501, accuracylog75[,6]/6000, type = "l", col = "black")
legend("bottomright", legend = c("Chi_2 = 0.65","Chi_2 = 0.70","Chi_2 = 0.75","Chi_2 = 0.80","Chi_2 = 85","Chi_2 = 0.90"),
       col = c("red", "yellow", "green", "blue", "purple", "black"), lty=1)


################################################################################################
# Setting up the chi_1 = 0.8 case
################################################################################################


plot(1:501, accuracylog80[,1]/6000, type = "l", col = "red", xlab = "1000 Steps", ylab = "Accuracy")
lines(1:501, accuracylog80[,2]/6000, type = "l", col = "yellow")
lines(1:501, accuracylog80[,3]/6000, type = "l", col = "green")
lines(1:501, accuracylog80[,4]/6000, type = "l", col = "blue")
lines(1:501, accuracylog80[,5]/6000, type = "l", col = "purple")
lines(1:501, accuracylog80[,6]/6000, type = "l", col = "black")
legend("bottomright", legend = c("Chi_2 = 0.65","Chi_2 = 0.70","Chi_2 = 0.75","Chi_2 = 0.80","Chi_2 = 85","Chi_2 = 0.90"),
       col = c("red", "yellow", "green", "blue", "purple", "black"), lty=1)


################################################################################################
# Setting up the chi_1 = 0.85 case
################################################################################################
plot(1:501, accuracylog85[,1]/6000, type = "l", col = "red", xlab = "1000 Steps", ylab = "Accuracy")
lines(1:501, accuracylog85[,2]/6000, type = "l", col = "yellow")
lines(1:501, accuracylog85[,3]/6000, type = "l", col = "green")
lines(1:501, accuracylog85[,4]/6000, type = "l", col = "blue")
lines(1:501, accuracylog85[,5]/6000, type = "l", col = "purple")
lines(1:501, accuracylog85[,6]/6000, type = "l", col = "black")
legend("bottomright", legend = c("Chi_2 = 0.65","Chi_2 = 0.70","Chi_2 = 0.75","Chi_2 = 0.80","Chi_2 = 85","Chi_2 = 0.90"),
       col = c("red", "yellow", "green", "blue", "purple", "black"), lty=1)

################################################################################################
# Setting up the chi_1 = 0.9 case
################################################################################################

plot(1:501, accuracylog90[,1]/6000, type = "l", col = "red", xlab = "1000 Steps", ylab = "Accuracy")
lines(1:501, accuracylog90[,2]/6000, type = "l", col = "yellow")
lines(1:501, accuracylog90[,3]/6000, type = "l", col = "green")
lines(1:501, accuracylog90[,4]/6000, type = "l", col = "blue")
lines(1:501, accuracylog90[,5]/6000, type = "l", col = "purple")
lines(1:501, accuracylog90[,6]/6000, type = "l", col = "black")
legend("bottomright", legend = c("Chi_2 = 0.65","Chi_2 = 0.70","Chi_2 = 0.75","Chi_2 = 0.80","Chi_2 = 85","Chi_2 = 0.90"),
       col = c("red", "yellow", "green", "blue", "purple", "black"), lty=1)


#################################################################################################
# Full plot
#################################################################################################

library(scales)
col_pal = rainbow(n=ncol(accuracylog))
plot(c(accuracylog), ylim=range(accuracylog), type='n', xlim=c(1,nrow(accuracylog)), xlab = "1000 Steps", ylab = "Accuracy") 
for(i in 1:36){
  points(accuracylog[,i], type='l', pch=19, col=alpha(col_pal[i], 0.5))
}
library("shape")
colorlegend(col = col_pal, zlim=c(1,36))
