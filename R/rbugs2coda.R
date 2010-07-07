## Print the statistics summary of the chains
rbugs2coda <- function(model)
{
    
    if( class(model) != "rbugs") stop("\nThe model is not a rbugs obejct.\n")
    chain.name <- c()
    for (i in 1:model$n.chain)
      chain.name <- c(chain.name, paste("chain",i,sep=""))

    obj <- list()
    for (i in 1:model$n.chain)
      obj <- c(obj,list(as.mcmc(model[[chain.name[i]]])))
      
    obj <- as.mcmc.list(obj)
    
    obj
}
