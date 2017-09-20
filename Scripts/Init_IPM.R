require(dplyr)

adultData <- read.csv("Data/a_LH.csv", skip=1, header=FALSE)
juvenileData <- read.csv("Data/j_LH.csv", skip=1, header=FALSE)

# --------
# Capture-recapture data
# --------
	# capture recapture data will rely on a CJS model that uses
	# an m-array summary format. Here, the m-array will seem sort of silly
	# because there is no observation process (resulting in no embedded zeros),
	# i.e., we're just starting by assuming a perfect recap rate

# purge the first years to move towards stable age distribution
adultObs <- adultData[,4:ncol(adultData)]
juvObs <- juvenileData[,4:ncol(juvenileData)]

createMArray <- function(obs) {
	nind <- dim(obs)[1]
	n.years <- dim(obs)[2]

	m.array <- matrix(0, nrow=n.years, ncol=n.years+1)
	m.array[,1] <- colSums(obs)
	for ( i in 1:nind) {
		pos <- which(obs[i,] != 0)
		for (z in 1:length(pos) - 1) {
			m.array[pos[z],pos[z+1]] <- m.array[pos[z], pos[z+1]] + 1
		}
	}
	for (t in 1:n.years) {
		m.array[t, n.years+1] <- m.array[t,1] - sum(m.array[t,2:n.years])
	}
	out <- m.array[1:(n.years-1), 2:(n.years+1)]
	return(out)
}

marray.J <- createMArray(juvObs)
marray.A <- createMArray(adultObs)

marray <- rbind(marray.J, marray.A) # single array to simplify model estimation

R <- rowSums(marray)

# --------
# Yearly count data
# --------
	# count data will just be one unique nest per adult present
	# we can think of this as a female-only model in a population
	# with no second clutches or re-nesting
Y <- as.vector(colSums(adultObs))
nyears <- length(Y)

# --------
# Productivity Data
# --------
	# total productivity for the year will just be each new juvenile alive
	# the IPM regresses based on total nests and total productivity so
	# no additional info is needed as to which juveniles correspond to which
	# adults
juvFirstYears <- matrix(0, nrow=nrow(juvObs), ncol=ncol(juvObs))
for (r in 1:nrow(juvFirstYears)) {
	firstYear <- min(which(juvObs[r,]==1))

	if (!is.na(firstYear) & !is.infinite(firstYear)) {
		juvFirstYears[r,firstYear] <- 1
	}
}

numChicks <- as.vector(colSums(juvFirstYears))

