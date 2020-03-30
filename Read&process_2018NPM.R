# clear workspace
rm(list = ls())

library(dplyr)

# load in data
product_data18<- read.csv("BOP_extract_Copy.csv",header = TRUE)
# product_data18

# DATA PREP

# convert to lowercase
product_data18$Description <- tolower(product_data18$Description)

# remove out of scope products
product_data18 <- subset(product_data18, Category!= "Drinks-Alcoholic")
product_data18 <- subset(product_data18, Category!= "Oils")
product_data18 <- subset(product_data18, Category!= "Fruit and vegetables")

# code fruit and veg columns NA as 0
product_data18$Fruit <- replace(product_data18$Fruit, is.na(product_data18$Fruit),0)
product_data18$Veg <- replace (product_data18$Veg, is.na(product_data18$Veg),0)

# work out if product is food or drink
product_data18$TYPE <- NA
product_data18$TYPE <- ifelse(grepl("Drinks", product_data18$Category), "Drink", "Food")

# subset food and drinks
drinks <- subset(product_data18, TYPE == "Drink")
foods <- subset(product_data18, TYPE == "Food")

###################################
# calculate free sugars

# identify if drinks contain dairy
drinks$dairy <- NA
drinks$dairy <- grepl("Milk|milk|cream|yoghurt|yogurt|cheese|butter",drinks$Description,
                      drinks$Ingredients,
                      drinks$ingredients.2017)
# if no dairy = total sugar = free sugar

# if drinks contains dairy 
# identify if drinks contain added sugars
drinks$add_sug <- NA
drinks$add_sug <- ifelse(grepl("honey|Syrup|syrup|nectar|Glucose|glucose|Fructose|fructose|Sucrose|sucrose",drinks$Description, 
                        drinks$Ingredients,
                        drinks$ingredients.2017), "TRUE",
                  ifelse(grepl("sugar|Sugar", drinks$Ingredients, 
                               drinks$ingredients.2017), "TRUE",
                                     "FALSE"))

# identify if drinks contain other sugars
drinks$other_sug <- NA
drinks$other_sug <- grepl("juice|puree", drinks$Description,
                          drinks$Ingredients,
                          drinks$ingredients.2017)


# calculate free sugars for drinks
drinks$FSug <- NA
drinks$FSug <- ifelse(drinks$dairy == FALSE,drinks$TOTSUG,
                        ifelse(drinks$add_sug == FALSE & drinks$other_sug == FALSE, 0, 
                               ifelse(drinks$add_sug == TRUE| drinks$other_sug == TRUE, drinks$TOTSUG/2,NA)))
# for drinks containing dairy + added/other sugars - refer to Appendix I to calculate
# Appendix I - where proportion of free sugs can't be determined from ingredients list, assume free sugs = 50% of total sugs

# identify if drink falls under SDIL
drinks$SDIL <- NA
drinks$SDIL <- ifelse(drinks$add_sug == TRUE & 
                        ((drinks$LACT + drinks$GALACT)/drinks$TOTSUG)*100 <=75 &
                        drinks$TOTSUG >=5, "Yes", "No")

# subset only drinks in scope (under SDIL)
ineligible_drinks <- subset(drinks, SDIL == "No")
drinks<- subset(drinks, SDIL == "Yes")

  
########################################
# calculate free sugars for foods

# identify if foods contain added sugars
foods$add_sug <- NA

foods$add_sug <-  grepl("honey|Syrup|syrup|nectar|Glucose|glucose|Fructose|fructose|Sucrose|sucrose|chocolate", foods$Description)|
                            grepl("honey|Syrup|syrup|nectar|Glucose|glucose|Fructose|fructose|Sucrose|sucrose|sugar|Sugar", foods$Ingredients)|
                                  grepl("honey|Syrup|syrup|nectar|Glucose|glucose|Fructose|fructose|Sucrose|sucrose|sugar|Sugar", foods$ingredients.2017)

# included 'sugar' in lookup for ingredients list but not for description - in description could be in the context of 'no sugar' etc
# included 'chocolate' in lookup for food description as several chocolate products were coming up as FALSE due to no ingredient list
# still misclassifies some products without ingredient list e.g. cakes and biscuits

# check if food contains any fruit and veg purees, juice or pastes where the cell structure is broken down
# paste not included as brought up lots of irrelevent items e.g. fish paste, meat paste
foods$FVsug <- NA
foods$FVsug <- grepl("juice|puree", foods$Description)|
                    grepl("juice|puree", foods$Ingredients)|
                          grepl("juice|puree",foods$ingredients.2017)

# determine if free sugars in foods
foods$FSug <- NA
foods$FSug <- ifelse(foods$add_sug == FALSE & foods$FVsug == FALSE, 0,
                     ifelse(foods$add_sug == FALSE & foods$FVsug == TRUE,NA,
                            ifelse(foods$add_sug == TRUE & foods$FVsug == TRUE,NA,
                                   ifelse(foods$add_sug == TRUE & foods$FVsug == FALSE, NA, NA))))
# unless there are no added sugars and no fruit and vegetable free sugars, free sugars should be calculated from all sources

# subset foods that contain free sugars
Free <- subset(foods,is.na(foods$FSug))
NOFree <- subset(foods, !is.na(foods$FSug))

# calculate on a category level based on appendix I
Free$FSug <- ifelse (grepl("ice cream|sundae", Free$Description)|grepl("ice cream|sundae", Free$Ingredients)|
                       grepl("ice cream|sundae", Free$ingredients.2017),Free$TOTSUG *0.8, 
                     ifelse(grepl("pizza|fajita", Free$Description), Free$TOTSUG,
                     ifelse(grepl("chicken|turkey|fish", Free$Description),Free$TOTSUG,
                     ifelse(grepl("noodle|rice", Free$Description), Free$TOTSUG,
                     ifelse(grepl("yoghurt|yogurt|yoghourt|fromage frais", Free$Description), Free$TOTSUG*0.038,
                     ifelse(grepl("yorkshire pudding", Free$Description), 0,
                     ifelse(grepl("hummus|hummous|puree|baked beans|mushy peas", Free$Description), Free$TOTSUG,
                     ifelse(grepl("coleslaw", Free$Description), Free$TOTSUG - ((Free$TOTSUG*0.6)*0.041),
                     ifelse(grepl("jam|marmalade", Free$Description), Free$TOTSUG,
                     ifelse(grepl("crumpet", Free$Description),0, 
                     ifelse(grepl("lolly|lollies", Free$Description), Free$TOTSUG,
                     ifelse(grepl("pancake|crepe|brioche|croissant|pains", Free$Description), Free$TOTSUG,
                     ifelse(grepl("scone|hot cross| malt loaf| welsh cake", Free$Description), Free$TOTSUG*0.74,
                     ifelse(grepl("biscuit|cookie|shortbread", Free$Description) & !grepl("chocolate", Free$Description) & !grepl("chocolate", Free$Ingredients), 
                            Free$TOTSUG-(Free$TOTSUG*(Free$Fruit/100)),
                     ifelse(grepl("biscuit|cookie|shortbread", Free$Description), Free$TOTSUG-((Free$TOTSUG*0.7)*0.092),
                     ifelse(grepl("baguette|bagel|pitta", Free$Description), 0,
                     ifelse(Free$Category == "Canned/tinned foods" & grepl("juice|syrup", Free$Description), Free$TOTSUG*0.6,
                     ifelse(Free$Category == "Sauces and Condiments" & !grepl("cheese|gravy|tikka|korma", Free$Description),Free$TOTSUG,
                     ifelse(grepl("cracker|oatcake|breadstick|cheese biscuit|flatbread", Free$Description), Free$TOTSUG,
                     ifelse(grepl("jelly|sponge", Free$Description), Free$TOTSUG,
                     ifelse(grepl("cereal bar", Free$Description), Free$TOTSUG-(Free$TOTSUG*(Free$Fruit/100)),
                     ifelse(grepl("cereal", Free$Description)|grepl("Breakfast Cereals", Free$Category), Free$TOTSUG-(Free$TOTSUG*(Free$Fruit/100)),
                     ifelse(grepl("trifle", Free$Description), Free$TOTSUG*0.82,
                     ifelse(grepl("mousse|fool", Free$Description), Free$TOTSUG*0.77, 
                     ifelse(grepl("condensed milk", Free$Description), Free$TOTSUG*0.78,
                     ifelse(grepl("cake|gateaux|gateau|muffin|eclair|doughnut|donut|swiss roll|mini roll|tart|pastry|pastries|madeleine", Free$Description) & !grepl("chocolate", Free$Description) & !grepl("chocolate", Free$Ingredients)
                            & !grepl("chocolate", Free$ingredients.2017) & Free$Fruit==0 & !grepl("cream", Free$Description) & !grepl("cream", Free$Ingredients)
                            & !grepl("cream", Free$ingredients.2017), Free$TOTSUG, 
                     ifelse(grepl("cake|gateaux|gateau|muffin|eclair|doughnut|donut|swiss roll|mini roll|tart|pastry|pastries|madeleine", Free$Description) & grepl("cream", Free$Description)|grepl("cream", Free$Ingredients)|
                              grepl("cream", Free$ingredients.2017), Free$TOTSUG - ((Free$TOTSUG*0.35)*0.027),  
                     ifelse(grepl("cake|gateaux|gateau|muffin|eclair|doughnut|donut|swiss roll|mini roll|tart|pastry|pastries|madeleine", Free$Description) & grepl("chocolate", Free$Description)|grepl("chocolate", Free$Ingredients)|
                              grepl("chocolate", Free$ingredients.2017), (Free$TOTSUG*0.33)*0.092,
                     ifelse(grepl("pie", Free$Description), Free$TOTSUG-(Free$TOTSUG*((Free$Fruit + Free$Veg)/100)),
                     ifelse(grepl("chocolate|Chocolate|Choc|choc", Free$Description)|grepl("chocolate|Chocolate|Choc|choc", Free$Ingredients)|
                              grepl("chocolate|Chocolate|Choc|choc", Free$ingredients.2017), Free$TOTSUG*0.84,       
                        Free$TOTSUG))))))))))))))))))))))))))))))


########################################
# join together data tables from free sugar calculations
# join food tables
Foods <- rbind(Free, NOFree)
# remove unnecessary columns
Foods <- within(Foods, rm(add_sug, FVsug))
drinks <- within(drinks, rm(dairy, add_sug, other_sug, SDIL))

# join foods with drinks
product_data18 <- rbind (Foods, drinks)

#########################################

# allocate A points for Free sugars
product_data18$A_FSug <- NA
product_data18$A_FSug <- ifelse(product_data18$FSug >= 37.5,10,
                        ifelse(product_data18$FSug >= 33.75,9,
                               ifelse(product_data18$FSug >= 30, 8,
                                      ifelse(product_data18$FSug >= 26.25,7,
                                             ifelse(product_data18$FSug >= 22.5,6,
                                                    ifelse(product_data18$FSug >=18.75,5,
                                                           ifelse(product_data18$FSug >= 15,4,
                                                                  ifelse(product_data18$FSug >= 11.25,3,
                                                                         ifelse(product_data18$FSug >= 7.5,2,
                                                                                ifelse(product_data18$FSug >= 3.75,1,0))))))))))

########################################
# allocate A points for calories (Kcal)
# create new column
product_data18$A_Kcal <- NA
product_data18$A_Kcal <- ifelse(product_data18$KCALS >= 750,10,
                                ifelse(product_data18$KCALS >=675,9,
                                       ifelse(product_data18$KCALS >= 600,8,
                                              ifelse(product_data18$KCALS >= 525,7,
                                                     ifelse(product_data18$KCALS >=450,6,
                                                            ifelse(product_data18$KCALS >=375,5,
                                                                   ifelse(product_data18$KCALS >=300,4,
                                                                          ifelse(product_data18$KCALS >=225,3,
                                                                                 ifelse(product_data18$KCALS >=150,2,
                                                                                        ifelse(product_data18$KCALS >=75,1,0)
                                                                                 )
                                                                          )
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
                                )
)

##########################################

# allocate A points for saturated fat
# create new column
product_data18$A_satF <- NA
product_data18$A_satF <- ifelse(product_data18$SATFOD >= 9.17,10,
                                ifelse(product_data18$SATFOD >=8.25,9,
                                       ifelse(product_data18$SATFOD >= 7.33,8,
                                              ifelse(product_data18$SATFOD >= 6.42,7,
                                                     ifelse(product_data18$SATFOD >=5.50,6,
                                                            ifelse(product_data18$SATFOD >=4.58,5,
                                                                   ifelse(product_data18$SATFOD >=3.67,4,
                                                                          ifelse(product_data18$SATFOD >=2.75,3,
                                                                                 ifelse(product_data18$SATFOD >=1.83,2,
                                                                                        ifelse(product_data18$SATFOD >=0.92,1,0)
                                                                                 )
                                                                          )
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
                                )
)

####################################################

# convert sodium to salt
# convert sodium in mg to sodium in g
product_data18$NA.g <- product_data18$NA./1000
# convert sodium in g to salt in g
product_data18$salt <- product_data18$NA.g * 2.5

# allocate A points for sodium (NA)
# create new column
product_data18$A_salt. <- NA
product_data18$A_salt. <- ifelse(product_data18$salt >= 2.28,10,
                             ifelse(product_data18$salt >= 2.052,9,
                                    ifelse(product_data18$salt >= 1.824,8,
                                           ifelse(product_data18$salt >= 1.596,7,
                                                  ifelse(product_data18$salt >= 1.368,6,
                                                         ifelse(product_data18$salt >= 1.14,5,
                                                                ifelse(product_data18$salt >= 0.912,4,
                                                                       ifelse(product_data18$salt >= 0.684,3,
                                                                              ifelse(product_data18$salt >= 0.456,2,
                                                                                     ifelse(product_data18$salt >= 0.228,1,0)
                                                                              )
                                                                       )
                                                                )
                                                         )
                                                  )
                                           )
                                    )
                             )
)


# calculate total A points
product_data18$A_TOTAL <- NA
product_data18$A_TOTAL <- product_data18$A_Kcal + product_data18$A_satF +product_data18$A_FSug + product_data18$A_salt.

##################################

# calculate C points

# calculate total fruit, veg and nuts %

# assign value for nuts - excludes composite dishes containing nuts
product_data18$Nut <- NA
product_data18$Nut <- ifelse(product_data18$Category=="Fruit and vegetables" & grepl("nut", product_data18$Description), 100,0)

product_data18$FVN <- product_data18$Fruit + product_data18$Veg + product_data18$Nut
# ensure a maximum of 100%
product_data18$FVN <- ifelse(product_data18$FVN>100, 100, product_data18$FVN)

# allocate C points for F&V
product_data18$C_FVN <- NA
product_data18$C_FVN <- ifelse(product_data18$FVN >= 80,5,
                            ifelse(product_data18$FVN >= 60,2,
                                   ifelse(product_data18$FVN >= 40,1,0)
                            )
)

# allocate points for AOAC fibre
product_data18$C_FIB <- NA
product_data18$C_FIB <- ifelse(product_data18$AOACFIB >= 5.625,5,
                             ifelse(product_data18$AOACFIB >= 4.5,4,
                                    ifelse(product_data18$AOACFIB >= 3.375,3,
                                           ifelse(product_data18$AOACFIB >= 2.25,2,
                                                  ifelse(product_data18$AOACFIB >- 1.125,1,0)
                                           )
                                    )
                             )
)

# allocate points for protein
product_data18$C_PRO <- NA
product_data18$C_PRO <- ifelse(product_data18$PROT >= 8,5,
                             ifelse(product_data18$PROT >= 6.4,4,
                                    ifelse(product_data18$PROT >= 4.8,3,
                                           ifelse(product_data18$PROT >= 3.2,2,
                                                  ifelse(product_data18$PROT >- 1.6,1,0)
                                           )
                                    )
                             )
)

#########################
# calculate total score for 2018 NPM

# create new column
product_data18$TOTAL_18 <- NA

# check if food scores less that 11 A points
product_data18$A_less11 <- NA
product_data18$A_less11 <- ifelse(product_data18$A_TOTAL < 11, TRUE,
                                ifelse(product_data18$A_TOTAL >= 11, FALSE, NA))

# calculate total score for foods which score less than 11 'A' points
# unless A > 11 but also scores 5 or more for fruit veg nuts
# unless A > 11 and scores less than 5 for fruit veg nuts
product_data18$TOTAL_18 <- ifelse(product_data18$A_less11 == TRUE, product_data18$A_TOTAL - (product_data18$C_FVN +
                                                                                           product_data18$C_FIB +
                                                                                           product_data18$C_PRO), 
                                  ifelse(product_data18$A_less11 == FALSE & product_data18$C_FVN ==5, 
                                         product_data18$A_TOTAL - (product_data18$C_FVN +
                                                                   product_data18$C_FIB +
                                                                   product_data18$C_PRO), 
                                         ifelse(product_data18$A_less11 == FALSE & product_data18$C_FVN <5,
                                                product_data18$A_TOTAL - (product_data18$C_FVN +
                                                                          product_data18$C_FIB),NA)))

#########################
# work out whether a product complies with NPM cut-offs or not
# assign if food or drink to work out which cut-off applies

product_data18$PASS <- NA
product_data18$PASS <- ifelse(product_data18$TYPE == "Drink" & product_data18$TOTAL_18 <1,"Yes",
                            ifelse(product_data18$TYPE == "Drink" & product_data18$TOTAL_18>=1, "No",
                                   ifelse(product_data18$TYPE == "Food" & product_data18$TOTAL_18 <4, "Yes",
                                          ifelse(product_data18$TYPE == "Food" & product_data18$TOTAL_18 >=4, "No", "Unclassified"))))


# subset data to select those that pass or fail under the 2004/5 NPM
pass18 <- subset (product_data18, product_data18$PASS == "Yes")
fail18 <- subset (product_data18, product_data18$PASS == "No")
unclassified18 <- subset (product_data18, product_data18$PASS == "Unclassified")

# write out data
write.csv(product_data18, file = "2018NPM.csv")

##############################
# where is new criteria less strict?
# examine products which passed 2018 NPM but failed 2004/5 NPM n = 7,941
# semi join returns only rows from the first table (passed 2018) which are also present in the second (fail 2004/5)
common <- semi_join(pass18, fail045, by = "EAN")
table(common$Category)

# where is new criteria more strict?
# examine products which fail 2018 NPM but passed 2004/5 NPM
diff <- semi_join(fail18, pass045, by = "EAN")
table(diff$Category)

##############################
# compare micronutrient quantities using T-test

# recode zero values for iron as 0.001 (very small negligible number) to avoid zeros being coded as -inf
# t-test won't run if values are -inf (not recognised as a number)

#hist(pass18$FOLT, breaks = 500)


pass18$VITE[pass18$VITE == 0] <- 0.001
fail18$VITE[fail18$VITE == 0] <- 0.001

pass045$VITE[pass045$VITE == 0] <- 0.001
fail045$VITE[fail045$VITE == 0] <- 0.001

mean(pass18$VITE)
mean(fail18$VITE)
mean(pass045$VITE)
mean(fail045$VITE)

# summary(pass18$FE)
# sd(pass18$FE)
# hist(pass18$CA)
# qqnorm(pass18$FE)
# qqline(pass18$FE, col = "red")

pass18$logVITE <- log(pass18$VITE)
hist(pass18$logVITE)

fail18$logVITE <- log(fail18$VITE)
hist(fail18$logVITE)

pass045$logVITE <- log(pass045$VITE)
hist(pass045$logVITE)

fail045$logVITE <- log(fail045$VITE)
hist(fail045$logVITE)

t.test(pass18$logVITE, fail18$logVITE)
t.test(pass045$logVITE, fail045$logVITE)
t.test(pass18$logVITE, pass045$logVITE)


