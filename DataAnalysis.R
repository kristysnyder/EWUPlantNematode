#################Native/introduced dataset######
rm(list=ls())
soil<-read.csv(file = "SoilData_Native.csv",header = TRUE)
names(soil)
#take columns in percent with n for row 1
speciesdesignation=soil[1,]
#take out that row
soil=soil[-1,]
names(soil)
for(i in 27:121) soil[,i]=as.numeric(soil[,i])
#for each column with na then make 0
for(i in 27:121) soil[which(is.na(soil[,i])),i]=0

names(soil)


#separate native and nonnatives, creates new columns
soil$native=rowSums(soil[,which(speciesdesignation=="n")])
soil$nonnative=rowSums(soil[,which(speciesdesignation=="i")])

#calculate total and percent cover, create new columns
soil$total=soil$native+soil$nonnative
soil$nativepercent=soil$native/soil$total
soil$nonnativepercent=soil$nonnative/soil$total

#check that new columns have been added
names(soil)

plot(soil$nativepercent)
plot(soil$nonnativepercent)
hist(soil$nativepercent)
hist(soil$nonnativepercent)

KB<-soil[soil$Site=="KB",]
SB<-soil[soil$Site=="SB",]
EWU<-soil[soil$Site=="EWU",]
ML<-soil[soil$Site=="ML",]

#ggplot
library(ggplot2)
ggplot(soil, aes(x = Site, y = nativepercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))



#subset, name of data, variable ==
ggplot(soil, aes(x = Site, y = NematodeAbundance, color = Month)) +  # ggplot function
  geom_boxplot()
?barplot

nemnative<-table(soil$Site, soil$Month, soil$NematodeAbundance, soil$nativepercent)

library(vegan)

####MANCOVA dont know if we can use with 
man1 <- manova(cbind(NematodeAbundance, nativepercent) ~ Site*Month, data = soil)
summary(man1)
summary.aov(man1)
TukeyHSD(man1)

boxplot(KB$nativepercent, SB$nativepercent, EWU$nativepercent, ML$nativepercent)
boxplot(KB$nonnativepercent, SB$nonnativepercent, EWU$nonnativepercent, ML$nonnativepercent)


######TUKEY of Site
soil$fsite=as.factor(soil$Site)
model1=lm(soil$nativepercent~soil$fsite)
anova(model1)
TukeyHSD(aov(model1))


speciesgroup=soil[,27:121]
for(i in 1:95) speciesgroup[which(is.na(speciesgroup[,i])),i]=0
names(speciesgroup)
library(vegan)
species.matrix=as.matrix(speciesgroup)
species_nmds=metaMDS(speciesgroup, k=6, try=20) 

adonis(species.matrix~soil$Site)
plot(species_nmds, type="p")
?plot
ordispider(species_nmds,groups=soil$Site,show.groups="KB",display="site",label=F,col="blue4")
ordispider(species_nmds,groups=soil$Site,show.groups="SB",display="sites",label=F,col="orange")
ordispider(species_nmds,groups=soil$Site,show.groups="EWU",display="sites",label=F,col="darkturquoise")
ordispider(species_nmds,groups=soil$Site,show.groups="ML",display="sites",label=F,col="yellow")



##########CA
#effect plot
library(effects)

names(soil)
#group functional groups

#group abiotic
enviro=soil[,11:26]
enviro
names(enviro)
for(i in 1:16) enviro[which(is.na(enviro[,i])),i]=0


#CA
library(vegan)

soil_ca=cca(speciesgroup)
summary(soil_ca)
screeplot(soil_ca)
plot(soil_ca,scaling=2, display="sites", type="t")
plot(soil_ca,scaling=2, type="t")
#correlation
plot(soil_ca,scaling=1, type="t")
plot(soil_ca,scaling=3, type="t")
plot(soil_ca,scaling=1)

###########CCA
soil_cca=cca(speciesgroup, enviro)
summary(soil_cca)
screeplot(soil_cca)
plot(soil_cca,scaling=2, type="t")
plot(soil_cca,scaling=1, type="t") 
plot(soil_cca,scaling=3, type="t") 
plot(soil_cca,scaling=1, type="t")
names(enviro)


####RDA
RIKZ_RDA2=rda(speciesgroup,enviro,scale=TRUE)
summary(RIKZ_RDA2)
screeplot(RIKZ_RDA2)
#beach grainsize sorting exposure are correlated together
#salinity is negatively related
#ccorrelation between p17 and salinity
plot(RIKZ_RDA2,scaling=2) #correlation
plot(RIKZ_RDA2,scaling=1) #distance

#################Annual/Perennial Analysis#######
#EWU has a higher proportion of annual species
#probably due to successional timing- tilled disturbed site has nigher proportion of annual sp

rm(list=ls())
an.per<-read.csv(file = "SoilData_annualperennial.csv",header = TRUE)

names(an.per)

#take columns in percent with n for row 1
speciesdesignation=an.per[1,]
#take out that row
an.per=an.per[-1,]
names(an.per)
for(i in 27:121) an.per[,i]=as.numeric(an.per[,i])
#for each column with na then make 0
for(i in 27:121) an.per[which(is.na(an.per[,i])),i]=0

names(an.per)


#separate native and nonnatives, creates new columns
an.per$annual=rowSums(an.per[,which(speciesdesignation=="a")])
an.per$perennial=rowSums(an.per[,which(speciesdesignation=="p")])

#calculate total and percent cover, create new columns
an.per$total=an.per$annual+an.per$perennial
an.per$annual=an.per$annual/an.per$total
an.per$perennial=an.per$perennial/an.per$total

#check that new columns have been added
names(an.per)

plot(an.per$annual)
plot(an.per$pernnial)
hist(an.per$perennial)
hist(an.per$annual)

KB<-an.per[an.per$Site=="KB",]
SB<-an.per[an.per$Site=="SB",]
EWU<-an.per[an.per$Site=="EWU",]
ML<-an.per[an.per$Site=="ML",]

###########An/Perennial BY Season#########
June<-an.per[an.per$Month=="June",]
July<-an.per[an.per$Month=="July",]

KBjune<-June[June$Site=="KB",]
SBjune<-June[June$Site=="SB",]
EWUjune<-June[June$Site=="EWU",]
MLjune<-June[June$Site=="ML",]

KBjuly<-July[July$Site=="KB",]
SBjuly<-July[July$Site=="SB",]
EWUjuly<-July[July$Site=="EWU",]
MLjuly<-July[July$Site=="ML",]

names(an.per)
boxplot(perennial~ Month, data=an.per, main="Native percent cover by treatment")

#subset, name of data, variable ==
boxplot(KB$annual, SB$annual, EWU$annual, ML$annual)
boxplot(KB$perennial, SB$perennial, EWU$perennial, ML$perennial)


######TUKEY of Site
an.per$fsite=as.factor(an.per$Site)
model1=lm(an.per$annual~an.per$fsite)
anova(model1)
TukeyHSD(aov(model1))


speciesgroup=an.per[,27:121]
for(i in 1:95) speciesgroup[which(is.na(speciesgroup[,i])),i]=0
names(speciesgroup)
library(vegan)
species.matrix=as.matrix(speciesgroup)
species_nmds=metaMDS(speciesgroup, k=6, try=20) 

adonis(species.matrix~an.per$Site)
plot(species_nmds, type="p")
?plot
ordispider(species_nmds,groups=an.per$Site,show.groups="KB",display="site",label=F,col="blue4")
ordispider(species_nmds,groups=an.per$Site,show.groups="SB",display="sites",label=F,col="orange")
ordispider(species_nmds,groups=an.per$Site,show.groups="EWU",display="sites",label=F,col="darkturquoise")
ordispider(species_nmds,groups=an.per$Site,show.groups="ML",display="sites",label=F,col="pink")



###########Annual/perennial and native/introduced (Combined) Data Analaysis#######
rm(list=ls())
both<-read.csv(file = "SoilData_Both.csv",header = TRUE)

names(both)

#take columns in percent with n for row 1
speciesdesignation=both[1,]
#take out that row
both=both[-1,]
names(both)
for(i in 27:121) both[,i]=as.numeric(both[,i])
#for each column with na then make 0
for(i in 27:121) both[which(is.na(both[,i])),i]=0

names(both)


#separate native and nonnatives, creates new columns 
both$an.native=rowSums(both[,which(speciesdesignation=="n.a")])
both$per.native=rowSums(both[,which(speciesdesignation=="n.p")])
both$an.intro=rowSums(both[,which(speciesdesignation=="i.a")])
both$per.intro=rowSums(both[,which(speciesdesignation=="i.p")])

#calculate total and percent cover, create new columns
both$total=both$an.native+both$per.native+both$an.intro+both$per.intro
both$an.native.per=both$an.native/both$total
both$per.native.per=both$per.native/both$total
both$an.intro.per=both$an.intro/both$total
both$per.intro.per=both$per.intro/both$total

#check that new columns have been added
names(both)

KB<-both[both$Site=="KB",]
SB<-both[both$Site=="SB",]
EWU<-both[both$Site=="EWU",]
ML<-both[both$Site=="ML",]
boxplot(both$an.native.per, both$per.native.per, both$an.intro.per, both$per.intro.per)


###########Annual/perennial and native/introduced BY Season#########
June<-both[both$Month=="June",]
July<-both[both$Month=="July",]


#vectors of sites by month
KBjune<-June[June$Site=="KB",]
SBjune<-June[June$Site=="SB",]
EWUjune<-June[June$Site=="EWU",]
MLjune<-June[June$Site=="ML",]

KBjuly<-July[July$Site=="KB",]
SBjuly<-July[July$Site=="SB",]
EWUjuly<-July[July$Site=="EWU",]
MLjuly<-July[July$Site=="ML",]

boxplot(June$Site)
boxplot(an.native.per~Site*Month, data=both)
library(ggplot2)
ggplot(both, aes(x = Site, y = an.native.per)) +            # Applying ggplot function
  geom_boxplot()

#annual native percent by season
ggplot(both, aes(x = Site, y = an.native.per, color = Month)) +  # ggplot function
  geom_boxplot()

#annual intro percent by season
ggplot(both, aes(x = Site, y = an.intro.per, color = Month)) +  # ggplot function
  geom_boxplot()

#perennial intro percent
ggplot(both, aes(x = Site, y = per.intro.per, color = Month)) +  # ggplot function
  geom_boxplot()

#perennial native percent
ggplot(both, aes(x = Site, y = per.native.per, color = Month)) +  # ggplot function
  geom_boxplot()

#######Native plant percentage vs nem abundance####
rm(list=ls())
soil<-read.csv(file = "SoilData_NativeNemAbundance.csv",header = TRUE)
names(soil)
#take columns in percent with n for row 1
speciesdesignation=soil[1,]
#take out that row
soil=soil[-1,]
names(soil)
for(i in 27:121) soil[,i]=as.numeric(soil[,i])
#for each column with na then make 0
for(i in 27:121) soil[which(is.na(soil[,i])),i]=0

names(soil)


#separate native and nonnatives, creates new columns
soil$native=rowSums(soil[,which(speciesdesignation=="n")])
soil$nonnative=rowSums(soil[,which(speciesdesignation=="i")])

#calculate total and percent cover, create new columns
soil$total=soil$native+soil$nonnative
soil$nativepercent=soil$native/soil$total
soil$nonnativepercent=soil$nonnative/soil$total

#check that new columns have been added
names(soil)

plot(soil$nativepercent)
plot(soil$nonnativepercent)
hist(soil$nativepercent)
hist(soil$nonnativepercent)

KB<-soil[soil$Site=="KB",]
SB<-soil[soil$Site=="SB",]
EWU<-soil[soil$Site=="EWU",]
ML<-soil[soil$Site=="ML",]


plot(soil$NematodeAbundance~soil$nativepercent)
#subset, name of data, variable ==
library(lattice)
qplot(interaction(Site,Month), NematodeAbundance, data=soil, geom="boxplot")
ggplot(soil, aes(x = Site, y = NematodeAbundance, color = Month)) +  # ggplot function
  geom_boxplot()
#compare nem abund to native plant cover by month and site
p <- ggplot(soil, aes(nativepercent, NematodeAbundance, shape= factor(Site)))
p + geom_point(aes(colour=factor(Month), size=2))
#by site color
p + geom_point(aes(colour=factor(Site)))

#Nem abun by site and month
p <- ggplot(soil, aes(NematodeAbundance, shape= factor(Site)))
p + geom_boxplot(aes(colour=factor(Month)))

ggplot(soil, aes(x = Site, y = NematodeAbundance)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Month)))
ggplot(soil, aes(x = Site, y = nativepercent)) +            # Applying ggplot function
  geom_boxplot(aes(colour=factor(Aspect)))


nemnative<-table(soil$Site, soil$Month, soil$NematodeAbundance, soil$nativepercent)
###########CCA-redundant-saving just in case, do not use##############
soil_cca=cca(speciesgroup, enviro)
summary(soil_cca)
screeplot(soil_cca)
plot(soil_cca,scaling=2, type="t")
plot(soil_cca,scaling=1, type="t") 
plot(soil_cca,scaling=3, type="t") 
plot(soil_cca,scaling=1, type="t")
names(enviro)


###CA
#effect plot
library(effects)

names(soil)
#group functional groups

#group abiotic
enviro=soil[,11:26]
enviro
names(enviro)
for(i in 1:16) enviro[which(is.na(enviro[,i])),i]=0


#CA
library(vegan)

soil_ca=cca(speciesgroup)
summary(soil_ca)
screeplot(soil_ca)
plot(soil_ca,scaling=2, display="sites", type="t")
plot(soil_ca,scaling=2, type="t")
#correlation
plot(soil_ca,scaling=1, type="t")
plot(soil_ca,scaling=3, type="t")
plot(soil_ca,scaling=1)
