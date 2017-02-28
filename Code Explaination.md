 #DATA

    iris = iris
    library(caTools)
    library(neuralnet)
    library(ISLR)
    library(rpart)
    library(rpart.plot)

    str(iris)
![](https://github.com/anagar20/Iris-Classify-plants-into-species/blob/master/images/img1.png)

    iris = iris[sample(nrow(iris)),]
    str(iris)

![](https://github.com/anagar20/Iris-Classify-plants-into-species/blob/master/images/img2.png)

##Splitting into training set and test set to convert categorical variables in training set into num variables
    set.seed(567)
    index=sample(1:nrow(iris),8/10* nrow(iris))
    train_ <- iris[index, ]
    test_ = iris[-index, ]

    Species = train_$Species
    u = which(Species=="setosa")
    y= which(Species=="versicolor")
    z =  which(Species=="virginica")

#SCALING THE DATA
    maxs = apply(iris[ ,1:4], 2, max)
    mins = apply(iris[ ,1:4], 2, min)
    scaled_data = as.data.frame(scale(iris[ ,1:4], center = mins, scale = maxs-mins ))
    data = scaled_data

    str(data)

![](https://github.com/anagar20/Iris-Classify-plants-into-species/blob/master/images/img3.png)

#SPLITTING INTO TRAINING AND TEST SET FOR PREDICTION
    
    set.seed(567)
    index=sample(1:nrow(data),8/10* nrow(data))
    train <- data[index, ]
    test = data[-index, ]

    train$setosa[1:nrow(train)]=0
    train$versicolor[1:nrow(train)]=0
    train$virginica[1:nrow(train)]=0

    train$setosa[u] =1
    train$versicolor[y] =1
    train$virginica[z] =1

    Species = test_$Species
    test$Species = Species

    str(train)

![](https://github.com/anagar20/Iris-Classify-plants-into-species/blob/master/images/img4.png)

    str(test)

![](https://github.com/anagar20/Iris-Classify-plants-into-species/blob/master/images/img5.png)

    feats = names(scaled_data)
    f = paste(feats,collapse='+')
    f = paste('setosa+versicolor+virginica~',f)
    f = as.formula(f)
    f
![](https://github.com/anagar20/Iris-Classify-plants-into-species/blob/master/images/img6.png)

#PREDICTION
    nn = neuralnet(f, train, hidden = c(10,10),learningrate=0.01, linear.output=FALSE)
    plot(nn)

![](https://github.com/anagar20/Iris-Classify-plants-into-species/blob/master/images/img7.png)

    predict = compute(nn,test[1:4])
    mypredict = predict$net.result
    maxidx <- function(arr) {
    return(which(arr == max(arr)))
    }

    idx = apply(mypredict, 1, maxidx)

    prediction <- c('setosa', 'versicolor', 'virginica')[idx]
    conf = table(prediction, test$Species)
    conf

![](https://github.com/anagar20/Iris-Classify-plants-into-species/blob/master/images/img8.png)

    acc = sum(diag(conf))/sum(conf)
    acc
[1] 0.9666666667
