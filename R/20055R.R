#' @title An function to judge whether a netural number is a prime number
#' @description to judge whether a netural number is a prime number 
#' @param n  the need number 
#' @return a string to see whether a number is aprime number and its factor
#' @examples
#' \dontrun{
#' f=function(x) c(x[1]^3-x[2]-1,x[1]^3-x[2]^2-1)
#' f(c(1,2))
#' s(c(1,2))
#' }
#' @export
s <- function(n){
  j=0
  for(i in 2:(n-1)){
    if (n %% i==0){
      print(i)
      j=1
    }
  }
  if (j==0){
    print('prime number')
  }
  else{
    print('composite number')
  }
}


#' @title to calculate the min edit distance of 2 string
#' @description to calculate the min edit distance of 2 string
#' @param word1 one string
#' @param word2 the other string
#' @examples
#' \dontrun{
#' f=function(x) c(x[1]^3-x[2]-1,x[1]^3-x[2]^2-1)
#' alg(c(1,2),f)
#' }
#' @export
alg <- function(word1, word2){
  len1 = nchar(word1)
  len2 = nchar(word2)
  dp = matrix(0,len1+1,len2+1)#define a matrix to store and calculate result
  dp[,1] = 0:len1
  dp[1,] = 0:len2
  #print(dp)
  #the algorithm
  for (i in 1:len1){
    for (j in 1:len2){
      if (substring(word1,i,i) == substring(word2,j,j)){
        delta=0
      }
      else{
        delta=1
      }
      #print(delta)
      dp[i+1,j+1] = min(dp[i,j] + delta, min(dp[i,j+1] + 1, dp[i+1,j] + 1))
    }
  }
  f = dp[len1+1,len2+1]
  print(dp)#to see the total result
  return(f)#return the value
}