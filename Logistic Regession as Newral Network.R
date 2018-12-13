### ---------------------------- Logistic Regressoin as neural network with iris data set -------------------------------------------------------------##

rm(list =ls())
library(dplyr)
data =  iris[1:100,c(1,3)]
attach(data)
plot(x=Sepal.Length,y= Petal.Length,pch= c(rep(1,50),rep(2,50)),col = c(rep(1,50),rep(2,50)),main = 'Logistic Regression as Nural Network')

## Logistic Regression with Gradent Decent Algorithem ##

## parameter intilization ##
alpha = 0.01



## forwoard propogation 

w =t(as.matrix(runif(2)))
b = t(as.matrix(runif(1)))
####-----------------------##

print(w)
print(b)
### ----------------------------##

for(i in 1:1000)
{ 

Z = w %*% t(data) + t(as.matrix(rep(b,100)))

# Sigmoid Function #
activations = 1/(1+exp(-Z))

# Estimated class at 0.5 probability

y_hat = if_else(activations >= 0.5 ,1,0)


# Backword Prpogation  and derivatives of functions #


dZ = activations - flag 
dw = (0.01) * t(data) %*% t(dZ) 
db = (0.01) * rowSums(dZ)
 # weight updates in backpropogations #
w = w - (alpha  * t(dw))
b = b - (alpha * db)
}

######################################################

print(w)
print(b)

#######################################






## Ploting the points on graphe ##

g = -(((w[1,1]*Sepal.Length) + b)/w[1,2])

lines(Sepal.Length,g,col ='green')



 




