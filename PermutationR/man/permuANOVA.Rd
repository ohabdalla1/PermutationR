\name{permuANOVA}
\alias{permuANOVA}

\title{
permuANOVA
}
\description{
Conducts permutational one-way and two-way ANOVAs.
This code has been modified from Howell D. (2015)
}
\usage{
permuANOVA(y,x,z, perm.type="unrestricted", reps=5000)
}

\arguments{
  \item{y}{
Dependent variable, numeric vector
}
\item{x}{
Independent variable, integer vector or factor  up to 10 levels
}
\item{z}{
(Optional) Independent variable, integer vector or factor up to 10 levels
}
\item{perm.type}{
(Optional) Method of permutation for two-way designs. Either "unrestricted" or "restricted". Default is "unrestricted"
}
\item{reps}{
Number of permutations, default is 5000.
}
}

\details{"unrestricted" computes main effect permutations in an unrestricted fashion; "restricted" restricts main effect permutations within levels of the other independent variable.
In both cases, the interaction is computed in an unrestricted fashion

}

\value{
For two-way designs: returns a data-frame containing p-values from permutation test for "Variable_x", "Variable_z" and interaction "x:z"
For one-way designs: returns a data-frame containing p-values from permutation test for "Variable_x" only.}
\references{
Howell D. (2015). Permutation Tests for Factorial ANOVA Designs. Retrieved from: <https://www.uvm.edu/~statdhtx/StatPages/Permutation\%20Anova/PermTestsAnova.html>
}

\author{
Osama Abdalla
osama.abdalla@mail.utoronto.ca
}

\seealso{
Package permuco function aovperm() and Package RVAideMemoire function perm.anova()
}
\examples{
Data <- ToothGrowth
attach(ToothGrowth)
permuANOVA(y = len, x = supp, reps=99)

permuANOVA(y=len,x=supp,z=dose, perm.type="unrestricted", reps=99)

permuANOVA(y=len,x=supp,z=dose, perm.type="restricted", reps=99)

}
