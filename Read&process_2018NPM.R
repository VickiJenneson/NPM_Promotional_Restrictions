# clear workspace
rm(list = ls())


# load in data
product_data18<- read.csv("BOP_extract_Copy.csv",header = TRUE)
# product_data18

# DATA PREP

# convert to lowercase
product_data18$Description <- tolower(product_data18$Description)

##############################
# Identify HFSS products in scope according to PHE calorie and sugar reduction plans and SDIL

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
product_data18$A_FSug <- ifelse(product_data18$FSug > 9.3,10,
                        ifelse(product_data18$FSug > 8.3,9,
                               ifelse(product_data18$FSug > 7.4, 8,
                                      ifelse(product_data18$FSug > 6.5,7,
                                             ifelse(product_data18$FSug > 5.6,6,
                                                    ifelse(product_data18$FSug > 4.6,5,
                                                           ifelse(product_data18$FSug > 3.7,4,
                                                                  ifelse(product_data18$FSug > 2.8,3,
                                                                         ifelse(product_data18$FSug > 1.9,2,
                                                                                ifelse(product_data18$FSug > 0.9,1,0))))))))))

########################################
# allocate A points for calories (kJ)
# convert kcal to kJ
product_data18$KJ <- product_data18$KCALS*4.184
# create new column
product_data18$A_KJ <- NA
product_data18$A_KJ <- ifelse(product_data18$KJ > 3150,10,
                                ifelse(product_data18$KJ > 2835,9,
                                       ifelse(product_data18$KJ > 2520,8,
                                              ifelse(product_data18$KJ > 2205,7,
                                                     ifelse(product_data18$KJ > 1890,6,
                                                            ifelse(product_data18$KJ > 1575,5,
                                                                   ifelse(product_data18$KJ > 1260,4,
                                                                          ifelse(product_data18$KJ > 945,3,
                                                                                 ifelse(product_data18$KJ > 630,2,
                                                                                        ifelse(product_data18$KJ > 315,1,0)
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
product_data18$A_satF <- ifelse(product_data18$SATFOD > 9.4,10,
                                ifelse(product_data18$SATFOD > 8.4,9,
                                       ifelse(product_data18$SATFOD > 7.5,8,
                                              ifelse(product_data18$SATFOD > 6.6,7,
                                                     ifelse(product_data18$SATFOD > 5.6,6,
                                                            ifelse(product_data18$SATFOD > 4.7,5,
                                                                   ifelse(product_data18$SATFOD > 3.7,4,
                                                                          ifelse(product_data18$SATFOD > 2.8,3,
                                                                                 ifelse(product_data18$SATFOD > 1.9,2,
                                                                                        ifelse(product_data18$SATFOD > 0.9,1,0)
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
product_data18$A_salt. <- ifelse(product_data18$salt > 2.3,10,
                             ifelse(product_data18$salt > 2,9,
                                    ifelse(product_data18$salt > 1.8,8,
                                           ifelse(product_data18$salt > 1.6,7,
                                                  ifelse(product_data18$salt > 1.4,6,
                                                         ifelse(product_data18$salt > 1.1,5,
                                                                ifelse(product_data18$salt > 0.9,4,
                                                                       ifelse(product_data18$salt > 0.7,3,
                                                                              ifelse(product_data18$salt > 0.5,2,
                                                                                     ifelse(product_data18$salt > 0.2,1,0)
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
product_data18$A_TOTAL <- product_data18$A_KJ + product_data18$A_satF +product_data18$A_FSug + product_data18$A_salt.

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
product_data18$C_FVN <- ifelse(product_data18$FVN > 80,5,
                            ifelse(product_data18$FVN > 60,2,
                                   ifelse(product_data18$FVN > 40,1,0)
                            )
)

# allocate points for AOAC fibre
product_data18$C_FIB <- NA
product_data18$C_FIB <- ifelse(product_data18$AOACFIB > 5.8,8,
                             ifelse(product_data18$AOACFIB > 5,7,
                                    ifelse(product_data18$AOACFIB > 4.3,6,
                                           ifelse(product_data18$AOACFIB > 3.6,5,
                                                  ifelse(product_data18$AOACFIB > 2.9,4,
                                                         ifelse(product_data18$AOACFIB > 2.2,3,
                                                                ifelse(product_data18$AOACFIB > 1.4,2,
                                                                       ifelse(product_data18$AOACFIB > 0.7,1,0)
                                                                       )
                                                                )
                                                         )
                                                  )
                                           )
                                    )
                             )

# allocate points for protein
product_data18$C_PRO <- NA
product_data18$C_PRO <- ifelse(product_data18$PROT > 8,5,
                             ifelse(product_data18$PROT > 6.4,4,
                                    ifelse(product_data18$PROT > 4.8,3,
                                           ifelse(product_data18$PROT > 3.2,2,
                                                  ifelse(product_data18$PROT > 1.6,1,0)
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
write.csv(product_data18, file = "2018NPM_1.csv")


