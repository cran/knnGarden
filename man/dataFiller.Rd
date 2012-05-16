\name{dataFiller}
\alias{dataFiller}
\title{ Missing Observations Filling Function
}
\description{ fill in the missing observations in a dataset by exploring  similarities between cases

}
\usage{
dataFiller(data, NAstring = NA)
}
\arguments{
  \item{data}{ a dataset that contains missing observations in some cases
}
  \item{NAstring}{ a character or string that denotes missing values in the  input dataset 
}
}
\details{ fill the cases with missing observations by finding the median of  10 most similar cases with the current one. Of course, the missing in the same column of the 10 cases will be removed when calculating the median. 
The criterion we define "similar" is based on euclidian distance between standardized cases

}
\value{
A complete data set with missing observations filled will be returned.
}
\references{
Luis Torgo (2003) Data Mining with R:learning by case studies. LIACC-FEP, University of Porto
}
\author{
Boxian Wei(The ideas are inspired by Luis Torgo, and thanks)
}
\note{
The cases with missing values in the input dataset will be printed on the screen instead of being returned.
The return will be only the complete data set with missing observations filled.

}
\seealso{
\code{\link{knnMCN}},	\code{\link{knnVCN}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## Define Data 
library(knnGarden)
data(iris)
v1=c(iris[1:4,3],NA,iris[6:10,3])
v2=iris[101:110,4]
v3=iris[101:110,1]
v4=c(iris[11:18,3],NA,iris[20,3])
data1=data.frame(v1,v2,v3,v4)

## Call Function
data2=dataFiller(data1)

## The function is currently defined as
function (data, NAstring = NA) 
{
    central.value <- function(x) {
        if (is.numeric(x)) 
            median(x, na.rm = T)
        else if (is.factor(x)) 
            levels(x)[which.max(table(x))]
        else {
            f <- as.factor(x)
            levels(f)[which.max(table(f))]
        }
    }
    dist.mtx <- as.matrix(daisy(data, stand = T))
    ShowMissing = NULL
    ShowMissing = data[which(!complete.cases(data)), ]
    for (r in which(!complete.cases(data))) data[r, which(is.na(data[r, 
        ]))] <- apply(data.frame(data[c(as.integer(names(sort(dist.mtx[r, 
        ])[2:11]))), which(is.na(data[r, ]))]), 2, central.value)
    cat("the missing case(s) in the orignal dataset ", "\n\n")
    print(ShowMissing)
    cat("\n\n")
    return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ missing }
\keyword{ fill }% __ONLY ONE__ keyword per line












