##BEE_PHYLO STRUCTURE
library(MCMCglmm)
library(brms)
library(ggphylo)
##LOAD IN YOUR TREE
plot(tree)
any(duplicated(tree$node.label))
tree$node.label<-c("","","","","","","","","","","","","","","","")
any(duplicated(tree$node.label))
nodelabels()
tree$node.label<-c(1:length(tree$node.label))
tree$edge.length
##THIS USES A PHYLO OBJECT AS INPUT
inv.phylo <- MCMCglmm::inverseA(tree, nodes = "TIPS", scale = FALSE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)
isSymmetric(A, check.attributes = FALSE)

##RUN MODEL with species random term
#constrain random term by phylogeny - cov_ranef
bee_p1<- brm(log(Spec.wgt) ~ Sex + log(IT) + (1 |Species) + Sex:log(IT), data = bee_all,
             cores=4,
             family = gaussian(),cov_ranef = list("Species" = A),
             prior=bprior1,control = list(adapt_delta = 0.99,max_treedepth=15))


pp_check(bee_p1)