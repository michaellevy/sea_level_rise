library(statnet)
require(ergm.userterms)

net <- as.network(edgelist, directed = F) #create network w/ 615 nodes| 512 Orgs, 103 Projects

netNames <- net%v%'vertex.names'
attribIn<- read.csv("Attributes.csv", header = T, stringsAsFactors = F)

netNames <- data.frame(Entity = netNames)
attributes <- merge(netNames, attribIn, by = "Entity") 
net <- as.network(edgelist, directed = F) #create network

# add relevant attributes
net%v%"biType"<-attributes$BiType
net%v%"name" <- attributes$Organization
net%v%"typeName" <-attributes$Type.Name
net%v%"group"<-attributes$Grouped.Sectors
net%v%"bigGovs"<-attributes$Loc.Reg.Gov 
net%v%'degree' = summary(net~sociality(base=0))
net%v%'locReg' <- attributes$Loc.Reg.All


####Regional Network###############################
##Create 2-mode network of Orgs and Projs, Regional Orgs only: 327 nodes| 224 org , 103 proj
regNet <- get.inducedSubgraph(net,v=which(net%v%'locReg'== "Project"|net%v%'locReg'=="Regional") ) 

# remove 18 isolated projects: 320 nodes| 224 org, 96 proj
isos <- isolates(regNet)
delete.vertices(regNet, isos)


####Local Network###############################
##Create subgraph of Local Orgs and associated projects only: 391 nodes | 288 orgs, 103 proj
locNet <- get.inducedSubgraph(net,v=which(net%v%'locReg'== "Project"|net%v%'locReg'=="Local") )

# remove isolated projects: 378 nodes| 288 orgs, 90 proj
isos <- isolates(locNet)
delete.vertices(locNet, isos)


####ERGM########################################
regDeg<- summary(regNet~sociality(base=0))  #setting up degree bounds to fix high degree outliers
regDeg[regDeg < 25] <- NA

#single full model run
mod <- ergm(regNet~ edges + gwb1degree(.1, fixed = TRUE)+gwb1degree(.1, fixed = TRUE)+gwb1nsp(.1, fixed = TRUE)+gwb1nsp(.1, fixed = TRUE)+ concurrent 
            + b1factor("typeName", base = c(11))+b1starmix(2,"bigGovs", diff = F), 
                        constraints = ~bd(minout=regDeg, minin=regDeg), control = control.ergm(MCMC.burnin = 50000, MCMC.interval = 5000))

##### run model with multiple parameter configurations and save output
df.2p <- data.frame(p1 = seq(.1,.9,.1), p2 = rep(.2, 9)) #this is a 2 param example: p1 = (.1-.9) and p2 = .2
mat.2p <- as.matrix(df.2p)


run.2pReg <- function(x)
{regMod<-try(ergm(regNet ~ edges + concurrent +gwb1degree(x[1], fixed = T) + gwb2degree(x[1], fixed = T)+b1factor("typeName")+b1starmix(2,"typeName", diff = F), 
               constraints = ~bd(minout=regDeg, minin=regDeg), control = control.ergm(MCMC.burnin = 50000)))

saveRDS(regMod, file = paste("regMod",paste(x,collapse = ""),sep=""))

}

apply(mat.2p, 1,run.2pReg)

test <- readRDS("regMod0.10.2")
