# Theoretical Gini indices
# Jan Mares, 200103

# Libraries
library(data.table)
library(ineq)
library(ggplot2)
library(here)

# define the inputs
p = seq(0,1,0.01)
gini = 0.56

# lines(p,Lc.lognorm(p, parameter=0.5),col="red")
# lines(p,Lc.lognorm(p, parameter=1.0),col="blue")
# lines(p,Lc.lognorm(p, parameter=1.5),col="green")

# define the variables
s = sqrt(2)*qnorm((gini+1)/2) # for log-normal distribution
alpha = 1/(2*gini)+1/2 # for pareto distribution

# put data into data table
data <- data.table(p,gini, s=sqrt(2)*qnorm((gini+1)/2),alpha = 1/(2*gini)+1/2,
                   lognorm = Lc.lognorm(p, parameter=s), pareto = Lc.pareto(p, parameter=alpha))

# ggplot
pl_1 <- ggplot(data, aes(x=p,y=lognorm, colour='brown'))
pl_1 + geom_line() + geom_line(aes(x=p, y=pareto, color='blue')) + theme_bw() +
       scale_colour_manual(name='', values=c('brown','blue')) + xlab('Cumulative share of population') + ylab('Cumulative share of income') +
       theme(legend.position = 'none',
             legend.background=element_rect(color="#f9f9f9",fill="#f9f9f9"),
             plot.background=element_rect(color="#f9f9f9",fill="#f9f9f9"))

# write into file
cairo_ps(file = here("Paper/figures/theoretical_ginis_pres.eps"), width=9, height=6, family="Arial")
pl_1 + geom_line() + geom_line(aes(x=p, y=pareto, color='blue')) + theme_bw() +
       scale_colour_manual(name='', values=c('brown','blue')) + xlab('Cumulative share of population') + ylab('Cumulative share of income') +
       theme(legend.position = 'none',
             legend.background=element_rect(color="#f9f9f9",fill="#f9f9f9"),
             plot.background=element_rect(color="#f9f9f9",fill="#f9f9f9"))
dev.off()

# plot, base
plot(p, Lc.lognorm(p, parameter=s), type="l", col="brown")
lines(p,Lc.pareto(p, parameter=alpha), col="blue")

# compute the gini with specific alpha coefficient in Pareto distribution
# gini = 1/((2*alpha+0.45)-1)