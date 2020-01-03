# Theoretical Gini indices
# Jan Mares, 200103

# Libraries
library(data.table)
library(ineq)

# define the inputs
p = seq(0,1,0.01)
gini = 0.56

# lines(p,Lc.lognorm(p, parameter=0.5),col="red")
# lines(p,Lc.lognorm(p, parameter=1.0),col="blue")
# lines(p,Lc.lognorm(p, parameter=1.5),col="green")

# define the variables
s = sqrt(2)*qnorm((gini+1)/2) # for log-normal distribution
alpha = 1/(2*gini)+1/2 # for pareto distribution

# plot
plot(p,Lc.lognorm(p, parameter=s), type="l", col="brown")
lines(p,Lc.pareto(p, parameter=alpha), col="blue")

# compute the gini with specific alpha coefficient in Pareto distribution
gini = 1/((2*aplha+0.45)-1)
