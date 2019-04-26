# Query data is the input data that we want to warp to reference
data_reference <- 1:10
data_query <- seq(1,10,0.5) + rnorm(19)
data_query <- c(1.630884, 2.264566, 2.462387, 3.964498, 6.602972, 7.192787, 5.960292, 8.458906, 9.742724, 9.903882)
alignment <- dtw(x=data_query, y=data_reference, keep=TRUE)

#if keep.internals=TRUE, the directions of steps that would be taken at each 
#alignment pair (integers indexing production rules in the chosen step pattern)
alignment$directionMatrix
      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
[1,]   NA    2    2    2    2    2    2    2    2     2
[2,]    3    3    3    3    3    3    3    3    3     3
[3,]    3    1    2    2    2    2    2    2    2     2
[4,]    3    3    1    2    2    2    2    2    2     2
[5,]    3    3    3    2    2    2    2    2    2     2
[6,]    3    3    3    1    1    2    2    2    2     2
[7,]    3    3    3    3    3    2    2    2    2     2
[8,]    3    3    3    3    3    3    3    3    3     3
[9,]    3    3    3    3    3    1    2    2    2     2
[10,]   3    3    3    3    3    3    2    2    2     2
[11,]   3    3    3    3    3    3    1    2    2     2
[12,]   3    3    3    3    3    3    3    2    2     2
[13,]   3    3    3    3    3    3    3    2    2     2
[14,]   3    3    3    3    3    3    3    2    2     2
[15,]   3    3    3    3    3    3    3    1    2     2
[16,]   3    3    3    3    3    3    3    3    2     2
[17,]   3    3    3    3    3    3    3    3    1     2
[18,]   3    3    3    3    3    3    3    3    3     2
[19,]   3    3    3    3    3    3    3    3    3     3

#x query vector or local cost matrix
#index1 matched elements: indices in x
#y reference vector, unused if x given as cost matrix
#index2 corresponding mapped indices in y
length(alignment$index1)
[1] 21

alignment$index1
[1]  1  2  3  4  5  5  6  7  8  9 10 11 12 13 14 15 16 17 18 18 19

length(alignment$index2)
[1] 21

data_query
[1]  1.7106632  0.5603226  1.9762188  3.0420218  2.4337969  5.2237697  5.3171333  3.6073971
[9]  5.7144393  6.2438186  6.7814450  6.8963687  6.8812288  6.8656126  7.7278441  8.2920577
[17]  9.1962951  8.7509333 11.4112605

#Warping warps either into a query or into a reference
#index.reference	TRUE to warp a reference, FALSE to warp a query
wt<-dtw::warp(alignment,index.reference=FALSE)
plot(data_query,type="l",col="blue",
     main="Warping query")
points(data_query[wt])

plot(alignment,type="twoway")
wt
[1]  1.5  3.0  4.5  5.0  7.0  9.5 12.5 15.5 17.5 18.5
data_query[wt]
[1] 1.710663 1.976219 3.042022 2.433797 5.317133 5.714439 6.896369 7.727844 9.196295 8.750933

## Most useful: plot the warped query along with reference 
plot(reference)
lines(query[alignment$index1]~alignment$index2,col="blue")

## Plot the (unwarped) query and the inverse-warped reference
plot(query,type="l",col="blue")
points(reference[alignment$index2]~alignment$index1)



