# Compare pass-rates under the two models

install.packages("dplyr")
library(dplyr)
install.packages("vcd")
library("vcd")
install.packages("pairwiseCI")
library(pairwiseCI)

##############################
# where is new criteria less strict?
# examine products which passed 2018 NPM but failed 2004/5 NPM n = 7,941
# semi join returns only rows from the first table (passed 2018) which are also present in the second (fail 2004/5)
common <- semi_join(pass18, fail045, by = "EAN")
table(common$Category)
write.csv(common, file ="common.csv")

# where is new criteria more strict?
# examine products which fail 2018 NPM but passed 2004/5 NPM
diff <- semi_join(fail18, pass045, by = "EAN")
table(diff$Category)
write.csv(diff, file="diff.csv")

###############################

# recode passes as YES and fails as No by year of NPM
product_data$PASS[product_data$PASS == "Yes"] <- "Yes_045"
product_data$PASS[product_data$PASS == "No"] <- "No_045"
product_data18$PASS[product_data18$PASS == "Yes"] <- "Yes_18"
product_data18$PASS[product_data18$PASS == "No"] <- "No_18"

# produce contingency table for pass Y/N by model year
tbl1 <- table(product_data$PASS, product_data18$PASS)
tbl1

# calculate % products which fail 2004/5 NPM, which also fail 2018 NPM
# Positive predictive value (PPV)
tbl1[1,1]/(tbl1[1,1]+tbl1[1,2])

################################
# Calculate agreement between models

# absolute agreement
agree <- (tbl1[1,1]+tbl1[2,2])/sum(tbl1)
agree

# Compute kapa
res.k <- Kappa(tbl1)
res.k
# confint(res.k)

# perform chi-square test
chisq.test(tbl1)

################

# Compare passes by sub-category

# create list of categories
cats_list<-list("Bread and grains",
                "Cakes,Biscuits,chocolates and other snacks",
                "Breakfast Cereals",
                "Dairy and Eggs",
                "Homebaking, Jam, Spreads",
                "Sauces and Condiments",
                "Canned/tinned foods",
                "Meat and poultry",
                "Ready Meals, Quiches, Pizza, Pasta, Soup",
                "Frozen Foods",
                "Fish",
                "Speciality/Ethnic Foods",
                "Drinks-Fizzy",
                "Drinks-Fruit Juice",
                "Drinks-Hot",
                "Drinks-Other"
           )
cats_list

######################################

# compare passes for foods and drinks
Food045 <- subset(product_data, product_data$TYPE == "Food")
Food18 <- subset(product_data18, product_data18$TYPE == "Food")
Drink045 <- subset(product_data, product_data$TYPE == "Drink")
Drink18 <- subset(product_data18, product_data18$TYPE == "Drink")

#create contingency table
tbl2 <-table(Food045$PASS, Food18$PASS)
tbl2

tbl3 <-table(Drink045$PASS, Drink18$PASS)
tbl3

# calculate % of foods/drinks which fail 2004/5 NPM, which also fail 2018 NPM
# Positive predictive value (PPV)
tbl3[1,1]/(tbl3[1,1]+tbl3[1,2])

# absolute agreement
agree <- (tbl3[1,1]+tbl3[2,2])/sum(tbl3)
agree

chisq.test(tbl2)

res.k2 <- Kappa(tbl3)
res.k2
#confint(res.k2)

######################################

# compare passes by category
Cat045 <- subset(product_data, product_data$Category == cats_list[13])
# Cat045

Cat18 <- subset(product_data18, product_data18$Category == cats_list[13])
# Cat18

#create contingency table
tbl4 <-table(Cat045$PASS, Cat18$PASS)
tbl4

# absolute agreement
agree <- (tbl4[1,1]+tbl4[2,2])/sum(tbl4)
agree

chisq.test(tbl4)

res.k4 <- Kappa(tbl4)
res.k4
#confint(res.k4)

# calculate % of products which fail 2004/5 which also fail under 2018 NPM
# positive preductive vlue (PPV)
tbl4[1,1]/(tbl4[1,1]+tbl4[1,2])



###########################################
###########################################

# compare micronutrient quantities 

# recode zero values as 0.001 (very small negligible number) to avoid zeros being coded as -inf
pass18$FOLT[pass18$FOLT == 0] <- 0.001
fail18$FOLT[fail18$FOLT == 0] <- 0.001

pass045$FOLT[pass045$FOLT == 0] <- 0.001
fail045$FOLT[fail045$FOLT == 0] <- 0.001

# summary(pass18$FE)
# sd(pass18$FE)
# hist(pass18$FE)
# qqnorm(pass18$FE)
# qqline(pass18$FE, col = "red")

########################################
# distributions not normal
# convert to logarithmic and create histograms

pass18$logFOLT <- log(pass18$FOLT)
# hist(pass18$logFOLT)

fail18$logFOLT <- log(fail18$FOLT)
# hist(fail18$logFOLT)

pass045$logFOLT <- log(pass045$FOLT)
# hist(pass045$logFOLT)

fail045$logFOLT <- log(fail045$FOLT)
# hist(fail045$logFOLT)


#################################

# perform Mann-Whitney U test for difference in distribution (non-parametric)
# difference 2018 pass vs fail
wilcox.test(exp(pass18$logFOLT),exp(fail18$logFOLT),conf.int = TRUE)

# difference 2004/5 pass vs fail
wilcox.test(exp(pass045$logFOLT),exp(fail045$logFOLT),conf.int = TRUE)

# difference pass 2018 vs pass 2004/5
wilcox.test(exp(pass18$logFOLT),exp(pass045$logFOLT),conf.int = TRUE)

################################
# Difference in medians

# find medians
# for 2018
# median(pass18$logFE)  
exp(median(pass18$logFOLT)) #back-transformed
# median(fail18$logFE)
exp(median(fail18$logFOLT)) #back-transformed

# for 2004/5 
# median(pass045$logNA.)  
exp(median(pass045$logFOLT)) #back-transformed
# median(fail045$logNA.)
exp(median(fail045$logFOLT)) #back-transformed


# calculate diff in medians
# 2018 pass vs 2018 fail
Obs.Diff.In.Medians_18 <- (exp(median(pass18$logFOLT)) - exp(median(fail18$logFOLT)))  #diff in medians
Obs.Diff.In.Medians_18
# 2004/5 pass vs 2004/5 fail
Obs.Diff.In.Medians_045 <- (exp(median(pass045$logFOLT)) - exp(median(fail045$logFOLT)))
Obs.Diff.In.Medians_045
# 2018 pass vs 2004/5 pass
Obs.Diff.In.Medians <- (exp(median(pass18$logFOLT)) - exp(median(pass045$logFOLT)))
Obs.Diff.In.Medians

###################################
### BOOTSTRAP CONFIDENCE INTERVAL
###################################

# let's run through making conf ints for the difference in means and medians

# let's bootstrap...
set.seed(13579)   # set a seed for consistency/reproducability
n.p18 <- 11063  # the number of observations to sample from 2018 pass
n.f18 <- 19459  # the number of observations to sample from 2018 fail
n.p04 <- 12920  # the number of observations to sample from 2004/5 pass
n.f04 <- 17602  # the number of observations to sample from 2004/5 fail
B <- 100  # the number of bootstrap samples


# get those bootstrap samples
# stick each Boot-sample in a column...
Boot.pass18 <- matrix( sample(exp(pass18$logFOLT), size= B*n.p18, 
                               replace=TRUE), ncol=B, nrow=n.p18)
Boot.fail18 <- matrix( sample(exp(fail18$logFOLT), size= B*n.f18, 
                               replace=TRUE), nrow=n.f18, ncol=B)

Boot.pass045 <- matrix( sample(exp(pass045$logFOLT), size= B*n.p04, 
                              replace=TRUE), ncol=B, nrow=n.p04)
Boot.fail045 <- matrix( sample(exp(fail045$logFOLT), size= B*n.f04, 
                              replace=TRUE), nrow=n.f04, ncol=B)

# check those
dim(Boot.pass18); dim(Boot.fail18)
dim(Boot.pass045); dim(Boot.fail045)

# check to make sure they are not empty
Boot.pass18[1:5,1:5]
Boot.fail18[1:5,1:5]


# calculate the difference in MEDIANS for each of the bootsamples

# 2018 pass vs 2018 fail
Boot.Diff.In.Medians18 <- apply(Boot.pass18, MARGIN=2, FUN=median) -
  apply(Boot.fail18, MARGIN=2, FUN=median)
# check that
length(Boot.Diff.In.Medians18)
# and, look at the first 10 diff in medians
Boot.Diff.In.Medians18[1:10]

# 2004/5 pass vs 2004/5 fail
Boot.Diff.In.Medians045 <- apply(Boot.pass045, MARGIN=2, FUN=median) -
  apply(Boot.fail045, MARGIN=2, FUN=median)
# check that
length(Boot.Diff.In.Medians045)
# and, look at the first 10 diff in medians
Boot.Diff.In.Medians045[1:10]

# 2018 pass vs 2004/5 pass
Boot.Diff.In.Medians <- apply(Boot.pass18, MARGIN=2, FUN=median) -
  apply(Boot.pass045, MARGIN=2, FUN=median)
# check that
length(Boot.Diff.In.Medians)
# and, look at the first 10 diff in medians
Boot.Diff.In.Medians[1:10]

#### MAKE THE CONFIDENCE INTERVALS (using 95% confidence)

# let's look at the PERCENTILE METHOD
# the "PERCENTILE" bootstrap confidence interval

# and then, the difference in MEDIANS
# 2018 pass vs 2018 fail
quantile(Boot.Diff.In.Medians18, prob=0.025)
quantile(Boot.Diff.In.Medians18, prob=0.975)

# 2004/5 pass vs 2004/5 fail
quantile(Boot.Diff.In.Medians045, prob=0.025)
quantile(Boot.Diff.In.Medians045, prob=0.975)

# 2018 pass vs 2004/5 pass
quantile(Boot.Diff.In.Medians, prob=0.025)
quantile(Boot.Diff.In.Medians, prob=0.975)



