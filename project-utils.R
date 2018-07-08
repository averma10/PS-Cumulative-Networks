# Final Project Utilities

# Based on Lab 5 utility
# Just a regular plot, but with male blue and female pink
# Assumes "gender" attribute
gaplot <- function(gr, names=TRUE)
{
  nlist <- rep("", vcount(gr))
  if (names)
  {
    nlist <- V(gr)$classname
  }
  plot(gr, vertex.color=c("#8888FF","pink")[1+(V(gr)$gender=="F")],
       vertex.label=nlist, 
       #   vertex.label.size=.75, 
       vertex.size=15,layout = layout_with_fr(gr))
}

# invlogit (x)
# Computes the inverse logit function.
# Useful for turning log-odds into probability

invlogit <- function(x) {
  exp(x) / (1 + exp(x))
}

# gof_trim
# Use this to avoid the error that occurs when plotting gof objects
# with a large number of parameters. The error is
# 'names' [y] attribute must be the same length as vector [x]. 
# Call with x as the second parameter to trim the number of parameters
# to what the plotting function expects.
# Input:
# gofm: A gofobject. Only tested on gof(g, GOF=~model) objects
# x: The desired length of the parameter vector. This can be determined
# from the error message.
# Output:
# A copy of the gofobject with the parameters trimmed.

# One could imagine a more elegant way to do this, via slices, boolean
# subsetting, etc.

gof_trim <- function(gofm, x) {
  gofm2 <- gofm
  
  # pval.model is a data frame dim y,5
  gofm2$pval.model <- gofm$pval.model[1:x,]
  # summary.model is a data frame dim y,5
  gofm2$summary.model <- gofm$summary.model[1:x,]
  # pobs.model is a named vector dim y
  gofm2$pobs.model <- gofm$pobs.model[1:x]
  # psim.model is a data frame dim k,y
  gofm2$psim.model <- gofm$psim.model[,1:x]
  # bds.model is a data frame dim 2,y
  gofm2$bds.model <- gofm$bds.model[,1:x]
  # obs.model is a named vector dim y
  gofm2$obs.model <- gofm$obs.model[1:x]
  # sim.model is a data frame dim k,y
  gofm2$sim.model <- gofm$sim.model[,1:x]
  
  return (gofm2)
}

