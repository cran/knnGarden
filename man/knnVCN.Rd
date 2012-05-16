\name{knnVCN}
\alias{knnVCN}
\title{
k-Nearest Neighbour Classification of Versatile Distance Version
}
\description{
k-nearest neighbour classification of versatile Distance version for test set from training set. For each row of the test set, the k nearest (in multiple distances) training set vectors are found, and the classification is decided by majority vote. This function allows you measure the distance bewteen vectors by six different means. K Threshold Value Check and Same K_i Problem Dealing are also been considered.
}
\usage{
knnVCN(TrnX, OrigTrnG, TstX, K = 1, ShowObs=F,method = "euclidean",p =2)
}
\arguments{
  \item{TrnX}{
matrix or data frame of training set cases.
}
  \item{OrigTrnG}{
matrix or data frame of true classifications of training set.
}
  \item{TstX}{
matrix or data frame of test set cases. A vector will be interpreted as a row vector for a single case.
}
  \item{K}{
number of top K nearest neighbours considered.
}
  \item{ShowObs}{
logical, when it's ture, the funtion will output the imformation of training set cases.
}
  \item{method}{
the distance measure to be used. This must be one of "euclidean", "maximum", "manhattan","canberra", "binary" or "minkowski". Any unambiguous substring can be given.
}
  \item{p}{
The power of the Minkowski distance.
}
}
\details{

K Threshold Value is stipulated to be less than the minimum size of the class in training set, or a warning will be shown.

Sometimes a case may get same "ballot" from class A and class B (even C, D, ...), this time a weighted voting process will be activated. The weight is based on the actual distance calculated between the test case and  K cases in neighbor A and B. The test case belongs to the class with less total distance.

The multiple distances are implemented by transfering the function dist(). For the convenience of users, we quote the details of function "dist()" and show them here.

Available distance measures are :

euclidean:
Usual square distance between the two vectors (2 norm).

maximum:
Maximum distance between two components of x and y (supremum norm)

manhattan:
Absolute distance between the two vectors (1 norm).

canberra:
sum(abs(Xi-Yi)/abs(Xi+Yi))
Terms with zero numerator and denominator are omitted from the sum and treated as if the values were missing.

This is intended for non-negative values (e.g. counts): taking the absolute value of the denominator is a 1998 R modification to avoid negative distances.

binary:
(aka asymmetric binary): The vectors are regarded as binary bits, so non-zero elements are "on" and zero elements are "off". The distance is the proportion of bits in which only one is on amongst those in which at least one is on.

minkowski:
The p norm, the pth root of the sum of the pth powers of the differences of the components.

Missing values are allowed, and are excluded from all computations involving the rows within which they occur. Further, when Inf values are involved, all pairs of values are excluded when their contribution to the distance gave NaN or NA. If some columns are excluded in calculating a Euclidean, Manhattan, Canberra or Minkowski distance, the sum is scaled up proportionally to the number of columns used. If all pairs are excluded when calculating a particular distance, the value is NA.

}
\value{
result of classifications of test set will be returned. 
(When TstX is NULL, the function will automatically consider the user is trying to test the knn algorithm. Hence, a test result table and accuracy report will be shown on the R-console.)
}
\references{

Ripley, B. D. (1996) Pattern Recognition and Neural Networks. Cambridge.

Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.

}
\author{
Xinmiao Wang
}
\note{
If you want to use the distance measure "binary", the vectors must be binary bits, non-zero elements are "on" and zero elements are "off".
}
\seealso{
\code{\link{knnMCN}},	\code{\link{dataFiller}}

}
\examples{
library(knnGarden)
data(iris)
## Define data
TrnX<-iris[,1:4]
OrigTrnG<-iris[,5]
#
TstX<-iris[c(1:20,50:70,120:140),1:4]
#or
TstX<-NULL
## Call function
knnVCN(TrnX=TrnX,OrigTrnG=OrigTrnG,TstX=TstX,ShowObs=FALSE,K=5,method="euclidean",p = 2)


## The function is currently defined as
function (TrnX, OrigTrnG, TstX = NULL, K = 1, ShowObs = F, method = "euclidean", 
    p = 2) 
{
    OrigTrnG = as.factor(OrigTrnG)
    TrnG = as.numeric(OrigTrnG)
    CodeMeaning = data.frame(TrnG, OrigTrnG)
    TK = sort(as.matrix(table(TrnG)), decreasing = F)
    if (K > TK[1]) {
        stop(c("\nNOTES: \nsorry, the value of K ", "(K=", K, 
            ") ", "you have selected is bigger than the capacity of one class in your training data set", 
            "(", "the capacity is ", TK[1], ")", ",", "please choose a less value for K"))
    }
    if (is.null(TstX) == T) {
        IsTst = 1
        TstX <- as.matrix(TrnX)
    }
    else {
        IsTst = 0
    }
    if (is.matrix(TstX) == F) {
        TstX <- as.matrix(TstX)
    }
    TrnX <- as.matrix(TrnX)
    ElmTrnG = union(TrnG, TrnG)
    LevTrnG = length(ElmTrnG)
    TrnTotal = cbind(TrnG, TrnX)
    NTstX = nrow(TstX)
    NTrnTotal = nrow(TrnTotal)
    VoteResult = NULL
    VoteResultList = NULL
    for (i in 1:nrow(TstX)) {
        RankBoardI <- NULL
        RankBoardIJ <- NULL
        Total = rbind(TstX[i, ], TrnX)
        RankBoardI = as.matrix(dist(Total, method = method, p = p)[1:nrow(TrnX)])
        RankBoardIJ = cbind(TrnG, RankBoardI)
        VoteAndWeight = RankBoardIJ[sort(RankBoardIJ[, 2], index.return = T)$ix[1:K], 
            1:2]
        TempVote4TstXI = RankBoardIJ[sort(RankBoardIJ[, 2], index.return = T)$ix[1:K], 
            1]
        ElmVote = union(TempVote4TstXI, TempVote4TstXI)
        CountVote = as.matrix(sort(table(TempVote4TstXI), decreasing = T))
        TempWinner = as.numeric(rownames(CountVote))
        if (length(CountVote) == 1 | K == 1) {
            Winner = TempWinner[1]
            TstXIBelong = union(CodeMeaning$OrigTrnG[which(CodeMeaning$TrnG == 
                Winner)], CodeMeaning$OrigTrnG[which(CodeMeaning$TrnG == 
                Winner)])
            VoteResultNode = data.frame(TstXIBelong)
            VoteResultList = rbind(VoteResultList, VoteResultNode)
        }
        else {
            NumOfTie = CountVote[1]
            FinalList = NULL
            j = 1
            TempWeight = sum(VoteAndWeight[which(VoteAndWeight[, 
                1] == TempWinner[j]), 2])
            FinalList = data.frame(TempWinner[j], TempWeight)
            while (CountVote[j] == CountVote[j + 1] & j < length(CountVote)) {
                TempWeight = sum(VoteAndWeight[which(VoteAndWeight[, 
                  1] == TempWinner[j + 1]), 2])
                FinalListNode = c(TempWinner[j + 1], TempWeight)
                FinalList = rbind(FinalList, FinalListNode)
                j = j + 1
            }
            FinalList = FinalList[sort(FinalList$TempWeight, 
                index.return = T)$ix[1], ]
            TstXIBelong = union(CodeMeaning$OrigTrnG[which(CodeMeaning$TrnG == 
                FinalList[1, 1])], CodeMeaning$OrigTrnG[which(CodeMeaning$TrnG == 
                FinalList[1, 1])])
            VoteResultNode = data.frame(TstXIBelong)
            VoteResultList = rbind(VoteResultList, VoteResultNode)
        }
    }
    if (IsTst == 1) {
        CheckT = as.matrix(table(data.frame(VoteResultList, OrigTrnG)))
        AccuStat = 1 - sum(CheckT - diag(diag(CheckT)))/length(TrnG)
        print(CheckT)
        cat("the classification accuracy of this algorithm on this training dataset is: ", 
            AccuStat * 100, "\%", "\n\n\n")
    }
    if (IsTst == 1 & ShowObs == F) {
        result = data.frame(VoteResultList, OrigTrnG)
    }
    else {
        if (IsTst == 1 & ShowObs == T) {
            result = data.frame(TstX, VoteResultList, OrigTrnG)
        }
        else {
            if (ShowObs == F) {
                result = data.frame(VoteResultList)
            }
            else {
                result = data.frame(TstX, VoteResultList)
            }
        }
    }
    return(result)
  }
}
\keyword{ knn }
\keyword{ dist }












