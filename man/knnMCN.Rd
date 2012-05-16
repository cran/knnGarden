\name{knnMCN}
\alias{knnMCN}
\title{
k-Nearest Neighbour Classification of Mahalanobis Distance Version
}
\description{
k-nearest neighbour classification of Mahalanobis Distance version for test set from training set. This function allows you measure the distance bewteen vectors by Mahalanobis Distance. K Threshold Value Check and Same K_i Problem Dealing are also been considered.

}
\usage{
knnMCN(TrnX, OrigTrnG, TstX = NULL, K = 1, ShowObs = F)
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
}
\details{

The knnMCN function determines which class a undetermined case should belong to by following steps.
First, calculate the Mahalanobis Distance between all the cases in training dataset. Then, select top K cases with nearest distances. Finally, these selected cases represent their classes and vote for the undetermined case under the principle of the minority is subordinate to the majority. 

When calculating the Mahalanobis Distance, we use samples' covariance matrix (CM) and the Mahalanobis Distance is defined as follows:

MD=sqrt((X-Y)*inverse(CM)*transpose(X-Y))
, where X,Y are 1*n vectors and CM is n*n matrix.

Sometimes a case may get same "ballot" from class A and class B (even C, D, ...), this time a weighted voting process will be activated. The weight is based on the actual distance calculated between the test case and  K cases in neighbor A and B. The test case belongs to the class with less total distance.

Also, to avoid unfair voting for undetermined case, K Threshold Value is stipulated to be less than the minimum size of the class in training dataset, or a warning will be shown.

}
\value{
result of classifications of test set will be returned. 
(When TstX is NULL, the function will automatically consider the user is trying to test the knn algorithm. Hence, a test result table and accuracy report will be shown on the R-console.)

}
\references{
Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
}
\author{
Boxian Wei
}
\note{
Sometimes, singular covariance matrix may appear due to the input data and the size of the classes. This time the funtion will return a warning and hint the user try knnVCN to retry the classification.
}
\seealso{
\code{\link{knnVCN}},	\code{\link{dataFiller}}
}
\examples{
library(knnGarden)
data(iris)

## Define data
TrnX=iris[c(1:20,80:100,140:150),1:4]
OrigTrnG=iris[c(1:20,80:100,140:150),5]
#
TstX<-iris[c(1:20,50:70,120:140),1:4]
#or
TstX<-NULL
## Call function
knnMCN(TrnX=TrnX,OrigTrnG=OrigTrnG,TstX=TstX,ShowObs=FALSE,K=5)


## The function is currently defined as
function (TrnX, OrigTrnG, TstX = NULL, K = 1, ShowObs = F) 
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
    if (abs(det(cov(TrnX[which(TrnTotal[, 1] == ElmTrnG[1]), 
        ]))) < 1e-07) {
        stop("\nWarnings:\nsample variance-covariance matrix is singular,\nand larger class sample capacity is required ,\nor you can try other methods in knnWXM of this package")
    }
    else {
        MaDisList = list(solve(cov(TrnX[which(TrnTotal[, 1] == 
            ElmTrnG[1]), ]), LINPACK = T))
    }
    if (LevTrnG > 1) {
        for (i in (1 + 1):LevTrnG) {
            if (abs(det(cov(TrnX[which(TrnTotal[, 1] == ElmTrnG[i]), 
                ]))) < 1e-07) {
                stop("\nWarnings:\nsample variance-covariance matrix is singular,\nand larger class sample capacity is required ,\nor you can try other methods in knnWXM of this package")
            }
            else {
                MaDisNode = list(solve(cov(TrnX[which(TrnTotal[, 
                  1] == ElmTrnG[i]), ]), LINPACK = T))
                MaDisList = c(MaDisList, MaDisNode)
            }
        }
    }
    NTstX = nrow(TstX)
    NTrnTotal = nrow(TrnTotal)
    VoteResult = NULL
    VoteResultList = NULL
    for (i in 1:nrow(TstX)) {
        RankBoardI <- NULL
        RankBoardIJ <- NULL
        for (j in 1:LevTrnG) {
            TempTrnXI = TrnX[which(TrnTotal[, 1] == ElmTrnG[j]), 
                ]
            TempCovJ = as.matrix(MaDisList[[j]])
            TempTstXI = NULL
            for (k in 1:nrow(TempTrnXI)) {
                TempTstXI = rbind(TempTstXI, TstX[i, ])
            }
            TempMadisBoardI <- sqrt(diag((TempTstXI - TempTrnXI) \%*\% 
                TempCovJ \%*\% t(TempTstXI - TempTrnXI)))
            MadisBoardI <- as.matrix(TempMadisBoardI)
            GBoardI <- as.matrix(rep(ElmTrnG[j], nrow(TempTrnXI)))
            RankBoardI <- cbind(GBoardI, MadisBoardI)
            RankBoardIJ <- rbind(RankBoardIJ, RankBoardI)
        }
        VoteAndWeight = RankBoardIJ[sort(RankBoardIJ[, 2], index.return = T)$ix[1:k], 
            1:2]
        TempVote4TstXI = RankBoardIJ[sort(RankBoardIJ[, 2], index.return = T)$ix[1:k], 
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
        cat("test results", "\n")
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
\keyword{ Mahalanobis }












