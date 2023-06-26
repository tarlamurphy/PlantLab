#### LABORATORY OF PLANT CONSERVATION
## TARLA MURPHY AND LOLA NEUERT
## FLOWER VISITER OBSERVATIONS FROM THE ORTO BOTANICO, BOLOGNA
## SPECIES OBSERVED: LAMIUM MACULATUM
## MAY 2023

#########################################

#set working directory

#load libraries

library(tidyr)
library(ggplot2)
library(viridis)
library(RStoolbox)
library(patchwork)
library(GGally)
library(dplyr)


###################

# upload data

orto <- read.csv("orto.csv", header = T) #upload the excel file of observations

# Initial data cleaning

orto <- orto %>% 
  mutate(Stems = as.integer(Stems), 
         Flowers = as.integer(Flowers), 
         Taxon = as.factor(Taxon))

is.character(orto$Taxon)
typeof(orto$Stems)
typeof(orto$Flowers)

# subset to relevent columns
orto1 <- subset(orto, select = c("Insect.group", "Taxon", "Stems", "Flowers"))

c <- orto1 %>% 
  count(Taxon)

spec <- orto1 %>%
  group_by(Taxon) %>%
  summarise(counts = n())
spec # a list of all the taxa

#basic initial analysis to give an idea of the distribution
ggplot(spec, aes(x = Taxon, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3)


#set NAs to zero 
orto1[is.na(orto1)] <- 0

# prepare a special xlab with the number of obs. for each group
my_xlab <- paste(levels(orto1$Taxon),"\n(N=",table(orto1$Taxon),")",sep="")

# plot of all taxa and flower visitation rates
bxp <- ggplot(orto1, aes(x=Taxon, y=Flowers, fill=Taxon)) +
  geom_boxplot(varwidth = F, alpha=0.7) +
  theme(legend.position="none") +
  scale_x_discrete(labels=my_xlab)+
  labs(title = "Flower visitation rates of observed taxa", cex = 2, 
       x = "Taxon", y = "No. of flowers visited") +
  scale_fill_viridis(discrete = T)
bxp
ggsave(filename = "bxp_pollinators.png", plot = bxp, width = 10, height = 5)

#######################
##wild bees compared to domesticated bees

comp <- filter(orto1, Taxon != "Other Species")

comp <- cbind(treatment = "Wild bees", comp)

comp$treatment[comp$Taxon == "Apis mellifera"] <- "Apis mellifera"

my_xlab2 <- paste(c("Apis mellifera", "Wild bees"),"\n(N=",table(comp$treatment),")",sep="")

bxp1 <- ggplot(comp, aes(x=treatment, y=Flowers, fill = treatment)) +
  geom_boxplot(varwidth = T, alpha=0.7) +
  theme(legend.position="none") +
  scale_x_discrete(labels=my_xlab2)+
  labs(title = "Flower visitation rate of domesticated bees vs wild bees", cex = 2, 
       x = "", y = "No. of flowers visited") +
  scale_fill_viridis(discrete = T)
bxp1 

ggsave(filename = "bxp_comp.png", plot = bxp1, width = 7, height = 5.5)

c1 <- comp %>% 
  count(treatment)
c1


####################
#t-test
?t.test
t.test(comp$Flowers[comp$treatment == "Wild bees"], comp$Flowers[comp$treatment == "Apis mellifera"])
