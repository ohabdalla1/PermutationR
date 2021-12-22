#Function for One-Way or Two-Way Factorial Designs Only!
#Two-Way source code:
#Using an additive model with restricted permutations for main effects
#Or Using an interactive model with unrestricted permutations for main effects
#Using an interactive model with unrestricted permutations for main effects
#Note: max 10 levels for Two Way Factorial Design with restricted permutations!
#In this case, y is the dependent (numeric variable), x and z are independent factors
permuANOVA <- function(y,x,z, perm.type="unrestricted", reps=5000){

  if(!missing(z)){
      #For restricted permutations of main effects, Edgington's Approach
    if(perm.type=="restricted"){

    l <- summary(aov(y ~ x+z))[[1]]$F[1]
    x = as.factor(x)
    z = as.factor(z)

    results1<-numeric(reps)

    for (i in 1:reps) {

      for (n in 1:nlevels(z)){
        assign(paste("z", n, sep = ""), sample(y[z==levels(z)[n]]))
      }
      temp1 <- c(get0("z1"),get0("z2"), get0("z3"), get0("z4"),
                 get0("z5"), get0("z6"), get0("z7"), get0("z8"),
                 get0("z9"), get0("z10"))

      results1[i] <- summary(aov(temp1 ~ x[order(z)]+z[order(z)]))[[1]]$F[1]

    }
    p.value1 <- (sum(results1 >= l)) / reps

  k <- summary(aov(y ~ x+z))[[1]]$F[2]

  results<- numeric(reps)

  for (i in 1:reps) {
    for (n in 1:nlevels(x)){
      assign(paste("x", n, sep = ""), sample(y[x==levels(x)[n]]))
    }
    temp <- c(get0("x1"),get0("x2"), get0("x3"), get0("x4"),
              get0("x5"), get0("x6"), get0("x7"), get0("x8"),
              get0("x9"), get0("x10"))

    results[i] <- summary(aov(temp ~ x[order(x)]+z[order(x)]))[[1]]$F[2]

  }
  p.value2 <- (sum(results >= k)) / reps
}
else{

  #For unrestricted permutation of main-effects, Manly's Approach
  if(perm.type=="unrestricted"){
  h <- summary(aov(y ~ x*z))[[1]]$F[1]
  l <- summary(aov(y ~ x*z))[[1]]$F[2]

  results <- numeric(reps)
  results1 <- numeric(reps)
  for (i in 1:reps) {
    temp <- sample(y)
    results[i] <- summary(aov(temp ~ x*z))[[1]]$F[1]
    results1[i] <- summary(aov(temp ~ x*z))[[1]]$F[2]

  }

  p.value1 <- (sum(results >= h)) / reps
  p.value2 <- (sum(results1 >= l)) / reps}
else {return(" perm.type must be restricted or unrestricted")}
}
  #Unrestricted Permutation of Interaction, Edgington, Manly
  j <- summary(aov(y ~ x*z))[[1]]$F[3]
  results2 <- numeric(reps)
  for (i in 1:reps) {
    temp2 <- sample(y)
    results2[i] <- summary(aov(temp2 ~ x*z))[[1]]$F[3]

  }
  p.value3 <- (sum(results2 >= j)) / reps
  p.values <- c(p.value1, p.value2, p.value3)
  as.data.frame(p.values, row.names=c("Variable_x", "Variable_z", "x:z"))
   }

#One-Way permutational ANOVA Source Code:
  else{
    z <- summary(aov(y ~ x))[[1]]$F[1]
    results <- numeric(reps)
    for (i in 1:reps) {
      temp <- sample(y)
      results[i] <- summary(aov(temp ~ x))[[1]]$F[1]
    }
    p.value4 <- sum(results >= z) / reps
    p.value <- c(p.value4)
    as.data.frame(x =  p.value, row.names="Variable_x")

  }
}
