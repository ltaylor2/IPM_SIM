require(jagsUI)

source("Scripts/Init_IPM.R")
jags.data <- list(nyears=nyears, marray=marray, R=R, Y=Y, numChicks=numChicks)

jags.inits <- function() {
	list(m.phi1=runif(1,0,1), m.phia=runif(1,0,1),
		 m.recap.1=runif(1,0,1), m.recap.a=runif(1,0,1),
		 m.omega=runif(1, 0, 10),
		 m.prod=runif(1,0,10),
		 N.imm=c(NA, round(runif(nyears-1,0,30))))
}

params <- c("phi1",
			"phia",
			"omega",
			"immRate",
			"prod",
			"N.1", "N.sad", "N.imm",
			"Ntot", "lambda", 
			"prod",
			"m.phi1", "m.phia", "m.prod", "m.lambda",
			"m.omega")

ipm <- jags(data=jags.data, inits=jags.inits, parameters.to.save=params,
			model.file="Scripts/ipm.SIM.bug",
			n.chains=3, n.thin=20, n.iter=500000, n.burnin=250000, parallel=FALSE)
