# R code for NCTN 27 data
# Requires the dr and pls packages from CRAN

library(dr)
library(pls)
nctn27.tx = read.csv("RANDOM.csv",header=T)
nctn27.fu = read.csv("T_FRVSF.csv",header=T)
nctn27.dem = read.csv("T_FRDEM.csv",header=T)

# Create a data matrix of weight at week 12, baseline weight
# treatment, gender and ethnicity

nctn.wtw12 = NULL
nctn.wtbase = NULL
nctn.tx = NULL
nctn.gender = NULL
nctn.ethn = NULL
nctn.id = NULL
nctn.ran = NULL
for (i in 1:1934) {
	
nctn.ran = c(nctn.ran,ifelse(prod(nctn27.tx$patdeid != i) == 1,0,1))
nctn.tx = c(nctn.tx,ifelse(prod(nctn27.tx$patdeid != i) == 1,NA,nctn27.tx$TX_NUM[nctn27.tx$patdeid == i]))
nctn.gender = c(nctn.gender,ifelse(prod(nctn27.tx$patdeid != i) == 1,NA,nctn27.dem$DEM002[nctn27.dem$patdeid == i]))
nctn.ethn = c(nctn.ethn,ifelse(prod(nctn27.tx$patdeid != i) == 1,NA,nctn27.dem$DEM003A[nctn27.dem$patdeid == i]))
nctn.id = c(nctn.id,i)
tmp = 	nctn27.fu[nctn27.fu$patdeid == i,]
nrec = dim(tmp)[1]
nctn.wtbase = c(nctn.wtbase,tmp$VSF006[1])
if (nrec < 6)	
 	nctn.wtw12 = c(nctn.wtw12,NA)
else
    nctn.wtw12 = c(nctn.wtw12,tmp$VSF006[6])
}

nctn = data.frame(id=nctn.id,rab=nctn.ran,tx=nctn.tx,gender=nctn.gender,ethn=nctn.ethn,basewt=nctn.wtbase,wtw12=nctn.wtw12)

nas.per.subject = apply(is.na(nctn),1,sum)
nctn.sub = nctn[nas.per.subject == 0,]

wtchange = nctn.sub$wtw12-nctn.sub$basewt
nctn.sub$wtc <- wtchange

# Sufficient dimension reduction
sdr1 <- dr(wtc~tx+gender+ethn+basewt,data=nctn.sub,method="sir")

# Partial least squares
pls1 <- mvr(wtc~tx+gender+ethn+basewt,ncomp=1,data=nctn.sub)

