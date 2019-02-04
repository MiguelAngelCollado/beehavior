
#libraries
library(ape)
library(phytools)
library(stringi)
library(brms)

##LOAD TREE
bee.trees=read.tree(file="data/phylogeny_genus_level.txt")
#Load data
data<-Success8trials.ITf
#Species dataframe, -just make psythirus Bombus
species=c("Rhodanthidium_sticticum" ,"Osmia_latreillei"   ,    
"Megachile_willughbiella", "Bombus_terrestris"      ,
 "Bombus_pratorum"         ,"Bombus_pascuorum"       ,
 "Bombus_vestalis"         ,"Apis_mellifera"         ,
 "Lasioglossum_malachurum" ,"Lasioglossum_immunitum", 
 "Flavipanurgus_venustus" , "Andrena_angustior"   ,   
 "Andrena_hispania"       , "Andrena_flavipes"  ,     
 "Andrena_pilipes"        , "Andrena_sp." )

##Use tree 1 (376 genera) #Genera-level phylogney I used the first one
bee.mcmc=bee.trees[[1]]

###root with apoid wasp outgroup
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) 
bee.mcmc=as.phylo(bee.mcmc)

##Make ultrametric
bee.mcmc=chronos(bee.mcmc)

bee.tree=drop.tip(bee.mcmc, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                                    "Hylaeus", "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                                    "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                                    "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                                    "Ctenocolletes", "Alocandrena", "Megandrena",      
                                    "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                                    "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                                    "Perdita", "Clavipanurgus", "Panurginus",        
                                    "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                                    "Calliopsis", "Arhysosage", "Callonychium", "Cerceris",        
                                    "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                                    "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                                    "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                                    "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                                    "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                                    "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                                    "Samba", "Capicola", "Hesperapis",      
                                    "Eremaphanta", "Dasypoda", "Melitta", "Redivivoides",    
                                    "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                                    "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                                    "Sphecodopsis", "Pasites", "Oreopasites",     
                                    "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                                    "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                                    "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                                    "Nomada", "Hexepeolus", "Neolarra", "Biastes",         
                                    "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                                    "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                                    "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                                    "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                                    "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                                    "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                                    "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                                    "Peponapis", "Xenoglossa", "Tetraloniella",   
                                    "Tetralonia", "Svastra", "Melissodes", "Martinapis",      
                                    "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                                    "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                                    "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                                    "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                                    "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                                    "Aglae", "Eulaema", "Eufriesea",            
                                    "Tetragonilla", "Tetragonula", "Platytrigona",    
                                    "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                                    "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                                    "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                                    "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                                    "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                                    "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                                    "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                                    "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                                    "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                                    "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                                    "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                                    "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                                    "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                                    "Ctenoplectra", "Macrogalea", "Allodapula",      
                                    "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                                    "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                                    "Allodape", "Ceratina", "Fideliopsis", "Fidelia",         
                                    "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                                    "Dioxys", "Noteriades", "Radoszkowskiana", 
                                    "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                                    "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                                    "Haetosmia", "Wainia", "Hoplosmia",           
                                    "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                                    "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                                    "Anthidium", "Serapista", "Pseudoanthidium", "Bathanthidium",   
                                    "Dianthidium", "Anthidiellum", "Paranthidium",  
                                    "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                                    "Hypanthidium","Anthodioctes", "Hypanthidioides", 
                                    "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                                    "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                                    "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                                    "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                                    "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                                    "Xenochlora", "Megaloptidia", "Augochlora", "Augochlorella",   
                                    "Augochloropsis", "Agapostemon", "Dinagapostemon", "Rhinetula",       
                                    "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                                    "Eupetersia", "Sphecodes", "Mexalictus", "Patellapis",      
                                    "Thrincohalictus", "Halictus", "Homalictus",   
                                    "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                                    "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                                    "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                                    "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                                    "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                                    "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                                    "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                                    "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                                    "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                                    "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                                    "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                                    "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                                    "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                                    "Hemicotelles", "Colletes", "Mourecotelles", 
                                    "Ectemnius", "trigona", "Tetrapedia", "Neoceratina", "Nasutapis", "Apidae",        
                                    "Toromelissa", "Lonchopria", "Baeocolletes", "Astata", "Stigmus",       
                                    "Stangeella", "Crabro", "Pison", "Sphecius", "Zanysson", "Heterogyna", "Acamptopoeum", "Psaenythia",    
                                    "Austropanurgus", "Anthrenoides", "Ancylandrena", "Melittoides","Anthophora",
                                    "Eucera", "Chilicola", "Duckeanthidium",
                                    
                                    "Xylocopa", "Tachysphex" 
))
bee.tree
plot(bee.tree)
## Add species tips to genera tips

#change panurgus to flavipanurgus
bee.tree$tip.label[2]
bee.tree$tip.label[2]=c("Flavipanurgus")

#add dummy species labels
bee.tree$tip.label<-paste(bee.tree$tip.label,"_dum",sep="")

#Add species tips
for(i in 1:length(species)){
  bee.tree<-add.species.to.genus(bee.tree,species[i],
                                 where="root")
}
## prune out dummy taxa
ii<-grep("dum",bee.tree$tip.label)
bee.tree<-drop.tip(bee.tree,bee.tree$tip.label[ii])

plot(bee.tree)

##Check for missing species
setdiff(species,bee.tree$tip.label)

#Remove node labels
bee.tree$node.label=NULL


##Phylogenetic co-variance matrix
inv.phylo <- MCMCglmm::inverseA(bee.tree, nodes = "TIPS", scale = TRUE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)
isSymmetric(A, check.attributes = FALSE)



#if we compare A with our species column, they have different names
A
dataformcmc=data
dataformcmc$Species
#Let's fix it
dataformcmc$Species<-stri_replace_first_regex(dataformcmc$Species,pattern = " ", replacement = "_")
dataformcmc[dataformcmc$Species%in%"Psithyrus_vestalis","Species"]=c("Bombus_vestalis")

#no differences
setdiff(rownames(A),dataformcmc$Species)

#This is the formula of the first model I want to do
#I changed binomial to bernoulli as it is more efficient for data with just 0's and 1's
brm.prueba<-brm(Success.test ~ brain.IT + (1|Species), data = dataformcmc,
                cores=4,
                family = bernoulli, cov_ranef = list("Species" = A),
                control = list(adapt_delta = 0.99,max_treedepth=15))


##Posterior predictive checks - with these you are looking at how well yrep replicates y - if they are very different, it suggests your model is misspecified
pp_check(brm.prueba,nsamples=1000)

#we create the prio object, with the formula of our model
bprior1<-get_prior(Success.test ~ brain.IT + (1 |Species), data = dataformcmc, family = bernoulli(), autocor = NULL,
                   sparse = FALSE, internal = FALSE)

#this isn't how I would create priors - this is just giving you the default priors
#you can create priors using the following - although your model is performing well even with default
#I would use `normal` priors and use the output of the model with the defaults to define these 
#Usually you should use your domain expertise i.e. what do you know about what the baseline of the intercept and coefficient be given what you know about the data
#this is difficult however with non-gaussian families

bprior <- prior(normal(0,5), class = b) +
  prior(normal(0,2), class = Intercept) +
  prior(normal(0,1), class = sd) 

brm.prueba2<-brm(Success.test ~ brain.IT + (1|Species), data = dataformcmc,
                cores = 4,
                prior = bprior,
                family = bernoulli, 
                cov_ranef = list("Species" = A),
                control = list(adapt_delta = 0.99,max_treedepth=15))

pp_check(brm.prueba2,nsamples=1000)


#Add information criterion to model
brm.prueba=add_ic(brm.prueba,ic=c("waic"))
brm.prueba2=add_ic(brm.prueba2,ic=c("waic"))

#compare
compare_ic(brm.prueba,brm.prueba2,ic=c("waic"))

#see they are much the same
#brms is very good at fitting binomial/bernoulli models

#Bayesian R2
bayes_R2(brm.prueba2)

#I just had a bit of fun with your dataset below haha...

##I imagine your next model could be the number of successes?
# you can model this with poisson or negative binomial
# maybe even with an offset of log(total number of tests)
#Sounds like a cool model!!

#with default priors

#poisson
brm_successes=brm(n.of.success ~ brain.IT + (1|Species), data = dataformcmc,
    cores = 4,
    family = poisson, 
    cov_ranef = list("Species" = A),
    control = list(adapt_delta = 0.99,max_treedepth=15))

#negative binomial
brm_successes2=brm(n.of.success ~ brain.IT + (1|Species), data = dataformcmc,
                  cores = 4,
                  family = negbinomial, 
                  cov_ranef = list("Species" = A),
                  control = list(adapt_delta = 0.99,max_treedepth=15))

#Add information criterion to model
brm_successes=add_ic(brm_successes,ic=c("waic"))
brm_successes2=add_ic(brm_successes2,ic=c("waic"))

#compare
compare_ic(brm_successes,brm_successes2,ic=c("waic"))

#poisson is better but best to read more about posterior predictive checks
pp_check(brm_successes2,nsamples=1000)

#You can use marginal_effects to plot your model - it gives posterior median +- 95% credible interval
marginal_effects(brm_successes)

#This was fun! let me know if you need more help
#are you also going to calculate phylogenetic signal of your traits/response variables?