
# ########################### STATISTICAL ANALYSIS################################

# Make use of the "diamonds" dataset that will contain? the price (among other interesting variables) of about 54,000 diamonds.
#
# The different indicators present in the "diamonds" dataset are the following:
# price: Price in US dollars
# carat: diamond weight
# cut: cut quality (Fair, Good, Very Good, Premium, Ideal)
# color: diamond color (from D best to J worst)
# clarity: measures how clear the diamond is (from the worst I1, SI2, SI1, VS2, VS1, VVS2, VVS1, to the best IF)
# x: length in mm
# y: width in mm
# z: depth in mm
# depth: total depth percentage
# table: width of the diamond top in relation to the widest point

# Clean up the workspace
rm ( list  = ls ( all  =  TRUE ))

# We load libraries
library ( ggplot2 )
library ( sampling )

#GGM: Tools for marginalization, conditioning and fitting by maximum likelihood.
library ( ggm )

getwd ()
setwd ( "C:/Users/Admin/Documents/R_Pro/Data-Mining-Diamonds" )

# We load the data frame and check its composition
df <- as.data.frame ( diamonds )
attach ( df )
head ( df )
str ( df )
summary ( df )
num_diamonds <- dim ( df ) [ 1 ]
num_variables <- dim ( df ) [ 2 ]



# ####### Representative sample ##########
# 1. Select a representative sample for "cut"

# We analyze the variable cut
str ( cut ) # We see that it is a qualitative ordinal, an ordinal factor
levels ( cut ) # Whose levels are "Fair" <"Good" <"Very Good" <"Premium" <"Ideal"
head ( df , 10 ) # We look at the first 10 values ​​to get an idea


# We analyze the frequency of the values ​​of the variable in the population
# In absolute data
summary ( cut ) # also with table (cut)
# In probability (very useful)
prop.table (table ( cut ))


# We can choose from the 53940 diamonds a representative random sample chosen at random
# that approximates the frequency of the cut values ​​in the entire population
# using sample () for this.
# The important thing is to choose the minimum size of the sample so that it is representative.
# For this we will base ourselves on the minimum number of samples that must be met:
#            n = N * Z ^ 2 * p * q / d ^ 2 * (N-1) + Z ^ 2 * p * q
# being:
#      p: proportion of the study phenomenon = 0.2 (1/5 pos.values)
#      q: 1-p = 0.8
#      N: population size = num_diamonds
#      Z: 95% confidence level = 1.96
#      d: precision level (in our case 0.04)

z_square <- 1.96 ^ 2
p <- 0.2
q <- 1 - p
d_square <- 0.04 ^ 2
size_sample_min <- ( num_diamonds * z_square * p * q ) / ( d_square * ( num_diamonds - 1 ) + z_square * ( p * q ))

sample_size_min  # The minimum sample size to be reliable is 381 elements

# We collect the sample
random_sample <- sample ( cut , min_sample_size )

# We check that the frequency of values ​​for the sample is similar to that of the population
layout ( matrix ( 1 : 2 , nrow  =  1 ))
pie (table ( random_sample ), main  =  " pie sample " )
pie (table ( cut ), main  =  " population pie " )

barplot (table ( random_sample ), main  =  " barplot sample " )
barplot (table ( cut ), main  =  " population barplot " )

prop.table (table ( random_sample ))
prop.table (table ( cut ))


# Another way to do the sampling more efficiently would be to do it by strata.
# As we know the proportion of the values ​​of cut on the population we need
# take strata with that proportion
# We multiply each probability by the minimum sample size
# With this we get to have a vector of quantities to collect for each cut level
freq_cut <- sapply (prop.table (table ( cut )), function ( x ) { x * min_sample_size })
freq_cut

# We have to sort diamods by type of cut
# And round the frequencies
strata  <- strata ( diamonds [order ( diamonds $ cut ),], stratanames  = c ( " cut " ), size  = round ( freq_cut ), method  =  " srswor " )
layered_sample  <- getdata ( cut , strata )
head ( layered_sample )

# We check that the strata are well distributed
pie (table ( layered_sample $ cut ), main  =  " pie sample est. " )
pie (table ( cut ), main  =  " population pie " )

barplot (table ( layered_sample $ cut ), main  =  " barplot est. sample " )
barplot (table ( cut ), main  =  " population barplot " )

prop.table (table ( layered_sample $ cut ))
prop.table (table ( cut ))

# Conclusion: As we can see, the stratified sample is much more adjusted. Since we started
# of the proportions. We would keep this sample.
# Observation we have taken 381 elements, but we could have taken a larger size; 381 is the minimum size.

layout ( matrix ( 1 : 1 , nrow  =  1 ))



# ###### Analysis of variables ########
# 2. Descriptive analysis of the variables: Type of variable, distribution and representation
# and Detection of atypical cases and their treatment

# The first thing is to see the values ​​of the variables. We are interested in finding null values
summary ( df )
df [ ! complete.cases ( df ),] # It seems there is no

# We can review one by one:
df [is.na ( price ),]
df [is.na ( carat ),]
df [is.na ( color ),]
df [is.na ( clarity ),]
df [is.na ( depth ),]
df [is.na ( table ),]
df [is.na ( x ),]
df [is.na ( y ),]
df [is.na ( z ),]
# .... There is none
# The (Other of clarity means other values)

# a) price: Price in US dollars
str ( price ) # Continuous quantitative variable
summary ( price ) # Very high maximum values ​​with respect to the mean
sd ( price ) # We confirm that it has a very high dispersion
boxplot ( price ) # Has a lot of outliers outside the upper whisker
# How many?
maximum <- quantile ( price , .75 ) + 1.5 * IQR ( price )
atypical_num <- length ( price [ price > maximum ]) # 3540 outliers
prop_val_atipicos <- num_val_atipicos / num_diamonds * 100  # 6.5% of the sample

# We see the distribution of the quantiles with normalized price
qqnorm ( price ) # It fits perfectly

# We review your distribution
hist ( price )
plot (density ( price )) # It is clearly a lognormal -> Which is logical that it has many outliers

# We treat it as a lognormal
log_price <- log ( price )
boxplot ( log_price ) # Outliers are gone
hist ( log_price ) # Now it looks like a normal

# Final histogram, with clusters
hist ( log_price , breaks = c ( 5 , 6.5 , 8.5 , 10 ), main  =  " Histogram log (price) " , xlab  =  " log (price) " )
lines (density ( log_price ))

# CONCLUSION: Price is a continuous quantitative variable that follows a distribution
# lognormal. Not much less can outliers be removed. It should be dealt with the log (price)
# since this function would preserve the mean and variance of the original function and eliminate outliers.

# b) carat: diamond weight
str ( carat ) # Continuous quantitative variable
summary ( carat )
# Values ​​between 0.2 and 5, but actually most are between 0.4 and 1
# Very high maximum values ​​with respect to the average, the lows are relatively close
sd ( carat ) # Has a relatively high dispersion
boxplot ( carat ) # Has a lot of outliers outside the upper whisker
# How many?
maximum <- quantile ( carat , .75 ) + 1.5 * IQR ( carat )
atypical_num <- length ( carat [ carat > maximum ]) # 1889 outliers
prop_val_atipicos <- num_val_atipicos / num_diamonds * 100  # 3.5% of the sample

# We see the distribution of the quantiles with normalized price
qqnorm ( carat ) # The same thing happens with price, this time more pronounced in the high quantiles

# We review your distribution
hist ( carat )
plot (density ( carat )) # Also looks like a lognormal

# We treat it as a lognormal
log_carat <- log ( carat )
boxplot ( log_carat ) # Effectively treating it as a lognormal we only have 2 acceptable outliers.
hist ( log_carat ) # Now looks like a normal

# Final histogram, with clusters
hist ( log_carat , breaks = c ( - 2 , - 1 , - 0.5 , 0 , 0.5 , 1.5 , 2 ), main  =  " Histogram log (carat) " , xlab  =  " log (carat) " )
lines (density ( log_carat ))

# CONCLUSION: carat is a continuous quantitative variable that follows a distribution
# lognormal. Not much less can outliers be removed. It should be dealt with the log (carat)
# since this function would conserve the mean and variance of the original function and
# leaves only 2 outliers that are assumable. It must be seen a posteriori whether or not they infer in the regression




# c) cut: cut quality (Fair, Good, Very Good, Premium, Ideal)
str ( cut ) # Discrete qualitative variable: Ordinal factor -> Fair <Good <Very Good <Premium <Ideal
levels ( cut )
summary ( cut ) # We see the number of elements per category

# We review proportion: We have greater proportion according to the order
pie (prop.table (table ( cut )))
barplot (prop.table (table ( cut )))
barplot (cumsum (prop.table (table ( cut ))))

# Obviously it has no outliers, it is an ordinal factor
# Although it does not make much sense, the boxplot can be raised to demonstrate it
boxplot ( cut )
# It is denoted that the average of diamonds would be "Premium"

# CONCLUSION: Ordinal qualitative variable.Each of the categories follows a
# bernuilli distribution.

# d) color: diamond color (from D best to J worst)

str ( color ) # Discrete qualitative variable: Ordinal factor -> D <E <F <G <H <I <J
levels ( color )
summary ( color ) # We see the number of elements per category

# We check proportion: We see that we have a dispersion greater than cut.
# In this case the order does not indicate higher frequency
foot (prop.table (table ( color )))
barplot (prop.table (table ( color )))
barplot (cumsum (prop.table (table ( color ))))

# Obviously it has no outliers, it is an ordinal factor
# Although it does not make much sense, the boxplot can be raised to demonstrate it
boxplot ( color )
# It is denoted that the average of diamonds would have color "G". It does not provide much information

# CONCLUSION: Qualitative variable.Each of the categories follows a
# bernuilli distribution. And in general it follows a normal distribution having
# take into account the order of your levels


# d) clarity: measures how clear the diamond is (from the worst I1, SI2, SI1, VS2, VS1, VVS2, VVS1, to the best IF)

str ( clarity ) # Discrete qualitative variable: Ordinal factor -> I1 <SI2 <SI1 <VS2 <VS1 <VVS2 <VVS1 <IF
levels ( clarity )
summary ( clarity ) # We see the number of elements per category

# We check proportion: We see that we have a dispersion greater than cut.
# In this case the order does not indicate higher frequency
pie (prop.table (table ( clarity )))
barplot (prop.table (table ( clarity )))
barplot (cumsum (prop.table (table ( clarity ))))

# Obviously it has no outliers, it is an ordinal factor
# Although it does not make much sense, the boxplot can be raised to demonstrate it
boxplot ( clarity )
# It is denoted that the average of diamonds would have clarity "VS2". It does not provide much information

# CONCLUSION: Qualitative variable.Each of the categories follows a
# bernuilli distribution. And in general it follows a normal distribution having
# take into account the order of your levels




# e) x: length in mm

str ( x ) # Continuous quantitative variable
summary ( x )
# Values ​​between 0 and 10, but actually most are between 4.7 and 6.5
# Very high / low maximum and minimum values ​​with respect to the average
sd ( x ) # Has a relatively high dispersion
boxplot ( x ) # Has a lot of outliers outside the upper whisker, and some in the lower whisker

# A 0mm long diamond does not make any sense -> It was an error in the data collection
# We remove it from the sample
length (which ( x == 0 )) # 8 outliers
df <- subset ( df , x != 0 ) # We remove them
num_diamonds <- dim ( df ) [ 1 ] # And now we have 53932 diamonds
attach ( df )

# We review
summary ( x ) # Now we have no outliers below
sd ( x ) # Smallest dispersion
boxplot ( x )

# How many do you have in your upper mustache?
maximum <- quantile ( x , .75 ) + 1.5 * IQR ( x )
atypical_num <- length ( x [ x > maximum ]) # 24 outliers
prop_val_atipicos <- num_val_atipicos / num_diamonds * 100  # A 0.044% of the sample
# It is a very low percentage to remove them and most are very close to the maximum

# We see the distribution of the quantiles with normalized price
qqnorm ( x ) # We confirm high outliers

# We review your distribution
hist ( x )
plot (density ( x )) # Also looks like a lognormal

# We treat it as a lognormal
log_x <- log ( x )
boxplot ( log_x ) # Outliers are gone
hist ( log_x ) # Now looks like a normal

# Final histogram, with clusters
hist ( log_x , breaks = c ( 1 , 1.5 , 1.7 , 2 , 2.2 , 2.4 ), main  =  " Histogram log (x) " , xlab  =  " log (x) " )
lines (density ( log_x ))

# CONCLUSION: Continuous quantitative variable that follows a normal or even lognormal distribution.
# A treatment has been done to remove the minimum outliers (due to lack of logic),
# we are not going to remove the maximum outliers (very low percentage), if at some point
# they bother us we can use the logarithm to adjust them




# f) y: width in mm

str ( y ) # Continuous quantitative variable
summary ( and )
# Values ​​between 4.6 and 58.9, but really most are between 4.7 and 6.5
# Very high maximum values ​​with respect to the average
sd ( y ) # Has a relatively high dispersion
boxplot ( y ) # Has a lot of outliers outside the upper whisker

# A diamond of more than 30mm in length does not make any sense -> It was an error in the data collection
# We remove it from the sample
length (which ( y > 30 )) # 2 outliers
df <- subset ( df , y < 30 ) # We remove them
num_diamonds <- dim ( df ) [ 1 ] # And now we have 53930 diamonds
attach ( df )

# We review
summary ( y ) # Now we don't have such high outliers
sd ( y ) # Less dispersion
boxplot ( and )

# How many are left?
maximum <- quantile ( y , .75 ) + 1.5 * IQR ( y )
atypical_num <- length ( y [ y > maximum ]) # 20 outliers
prop_val_atipicos <- num_val_atipicos / num_diamonds * 100  # A 0.037% of the sample
# It is a very low percentage to remove them and most are very close to the maximum

# We see the distribution of the quantiles with normalized price
qqnorm ( y ) # We confirm high outliers

# We review your distribution
hist ( and )
plot (density ( y )) # also looks like a lognormal

# We treat it as a lognormal
log_y <- log ( y )
boxplot ( log_y ) # Outliers are gone
hist ( log_y ) # Now it looks like a normal

# Final histogram, with clusters
hist ( log_y , breaks = c ( 1 , 1.5 , 1.7 , 2 , 2.2 , 2.4 ), main  =  " Histogram log (y) " , xlab  =  " log (y) " )
lines (density ( log_y ))

# CONCLUSION: Continuous quantitative variable that follows a normal or even lognormal distribution.
# A treatment has been done to remove very high outliers (due to lack of logic),
# we are not going to remove the outliers close to the maximum (very low percentage), if at some point
# they bother us we can use the logarithm to adjust them


# g) z: depth in mm


str ( z ) # Continuous quantitative variable
summary ( z )
# Values ​​between 0 and 31, but really most are between 2.9 4.04
# Very high / low maximum / minimum values ​​with respect to the average
sd ( z )
boxplot ( z ) # Has very many outliers outside the upper whisker

# It does not make any sense a diamond more than 30mm deep or 0mm -> It has been an error in the data collection
# We remove it from the sample
length (which ( z > 30 | z == 0 )) # 2 outliers
df <- subset ( df , z < 30 & z ! = 0 ) # We remove them
num_diamonds <- dim ( df ) [ 1 ] # And now we have 53917 diamonds
attach ( df )

# We review
summary ( z ) # Now we don't have such high outliers
sd ( z ) # Less dispersion
boxplot ( z )

# How many are left?
maximum <- quantile ( z , .75 ) + 1.5 * IQR ( z )
minimum <- quantile ( z , .25 ) - 1.5 * IQR ( z )
outlier_num <- length ( z [ z > maximum | z < minimum ]) # 27 outliers
prop_val_atipicos <- num_val_atipicos / num_diamonds * 100  # 0.05% of the sample
# It is a very low percentage to remove them and most are very close to the maximum

# We see the distribution of the quantiles with normalized price
qqnorm ( z ) # We confirm high outliers

# We review your distribution
hist ( z )
plot (density ( z )) # Normal Distribution


# CONCLUSION: Continuous quantitative variable that follows a normal distribution.
# A treatment has been done to remove very high outliers or 0 (due to lack of logic),
# outliers close to the maximum or minimum we are not going to remove (very low percentage), if at some point
# they bother us we can use the logarithm to adjust them

# h) depth: total depth percentage

str ( depth ) # Continuous quantitative variable (Probabilities)
summary ( depth )
# Values ​​between 43% and 79%, but actually most are between 61% and 62.5%
# Very high / low maximum and minimum values ​​with respect to the average
sd ( depth ) # Has a high dispersion
boxplot ( depth ) # Has very many outliers outside the upper and lower whiskers

# How many?
maximum <- quantile ( depth , .75 ) + 1.5 * IQR ( depth )
minimum <- quantile ( depth , .25 ) - 1.5 * IQR ( depth )
atypical_num <- length ( depth [ depth > maximum | depth < minimum ]) # 2543 outliers
prop_val_atipicos <- num_val_atipicos / num_diamonds * 100  # 4.7% of the sample
# It is a very high percentage to remove them and most are very close to the maximum and minimum

# We see the distribution of the quantiles with normalized price
qqnorm ( depth ) # We confirm high outliers

# We review your distribution
hist ( depth )
plot (density ( depth )) # It is a normal distribution but it has many outlayers

# It is best to reclassify it
df $ depth.clasif <- ifelse ( depth < 57 , " <57% " ,
                              ifelse ( depth < 60 , " 57-60% " ,
                                       ifelse ( depth < 62 , " 60-62% " ,
                                                ifelse ( depth < 64 , " 62-64% " ,
                                                         " > 64% " ))))
attach ( df )

summary ( factor ( depth.clasif ))
prop.table (table ( depth.clasif ))
pie (prop.table (table ( depth.clasif )))
barplot (prop.table (table ( depth.clasif )))
# In this way, having them ordered we would have perfectly a normal distribution
# And including all outlayers


# CONCLUSION: Continuous quantitative variable that follows a normal distribution.
# A reclassification has been made to include all outlayers since they had a percentage
# very high and therefore it was not logical to remove them. It was not a sample problem.
# The reclassification distribution follows a normal distribution



# i) table: width of the diamond top in relation to the widest point  

str ( table ) # Discrete quantitative variable
summary ( table )
# Values ​​between 43 and 95, but really most are between 56 and 59
# Very high / low maximum and minimum values ​​with respect to the average
sd ( table ) # Has a high dispersion
boxplot ( table ) # Has a lot of outliers outside the upper and lower whiskers

# How many?
maximum <- quantile ( table , .75 ) + 1.5 * IQR ( table )
minimum <- quantile ( table , .25 ) - 1.5 * IQR ( table )
atypical_num <- length ( depth [ depth > maximum | depth < minimum ]) # 3746 outliers
prop_val_atipicos <- num_val_atipicos / num_diamonds * 100  # 6.94% of the sample
# It is a very high percentage to remove them and most are very close to the maximum and minimum

# We see the distribution of the quantiles with normalized price
qqnorm ( table ) # We confirm high outliers

# We review your distribution
hist ( table )
plot (density ( table )) # It is a normal distribution but it has many outlayers

# It is the same case as the previous one. Another reclassification could be made. Also knowing that
# follow a normal you can choose from several options:
# 1. Leave it as is, removing only the most popular outliers
# 2. Substitute them for the average (I AM NOT A SUPPORTER)
# 3. Substitute them for max and min respectively.

# We this time (to change with respect to depth) we are going to leave them
# and we are going to remove only the very extreme ones that are clearly seen in qqnorm

# We remove it from the sample
length (which ( table > 75 | table < 50 )) # 7 outliers
df <- subset ( df , table < 75 & table > 50 ) # We remove them
num_diamonds <- dim ( df ) [ 1 ] # And now we have 53908 diamonds
attach ( df )

# We review
summary ( table )
sd ( table ) # Decrease dispersion
boxplot ( table )
qqnorm ( table )

# Normal distribution
hist ( table )
plot (density ( table ))

# CONCLUSION: Discrete quantitative variable, with normal distribution.
# A treatment has been applied to the outlayers only removing the most extreme ones (surely a data problem)
# but they have given up the respo because there were many. A reclassification could have been done.
# In fact, if the regression is taken into account, it will have to be done

# ############# Inference ###############
# 4. Calculate a confidence interval for the mean of "carat" and "depth"

# If we take the entire df as a sample with respect to the entire population of diamonds.
# We can infer by confidence interval the mean of the entire population
# By the TCL we know that the mean of a sample whose population is normal, also follows
# a normal distribution, such as carat and depth.
# We also know by Chebyshev's theorem the probability of the population mean
# this between sample_mean + -Z * population_dev / sqrt (n) = 1-alpha
# In our case:
# n> 30 and unknown population_dev -> We will use sample_dev and Z (since n> 30)

# For carat (ConfLevel: 95% -> alpha = 0.05)
n <- num_diamonds  # = 53908
sample_mean <- mean ( carat ) # = 0.797648
sample_dev <- sd ( carat ) # = 0.4737466
z.teorica <- qnorm (c ( 0.025 ), mean  =  0 , sd  =  1 , lower.tail  =  FALSE ) # = 1.959964

left_population_mean = sample_mean - meteoric_z * ( sample_dev / sqrt ( n ))
population_mean_der = sample_mean + meteoric_z * ( sample_dev / sqrt ( n ))

left_population_mean  # = 0.7936489
population_mean_der  # = 0.8016472
# Conclusion: mean_poblacional_carat belongs to [0.7936489,0.8016472] at 95% confidence

# For depth (Conf Level: 99% -> alpha = 0.01)
n <- num_diamonds  # = 53908
sample_mean <- mean ( depth )   # = 61.7493
sample_dev <- sd ( depth )   # = 1.431871
z.teorica <- qnorm (c ( 0.005 ), mean  =  0 , sd  =  1 , lower.tail  =  FALSE )   # = 2.575829

left_population_mean = sample_mean - meteoric_z * ( sample_dev / sqrt ( n ))
population_mean_der = sample_mean + meteoric_z * ( sample_dev / sqrt ( n ))

left_population_mean  # = 61.73342
population_mean_der  # = 61.76519
# Conclusion: population_mean_depth belongs to [61.73342,61.76519] at 99% confidence.
# OBS: Obviously we are more confident in this second interval.


# 5. Formulate a hypothesis test

# It has been known for other years that the average price of diamonds is usually around 3800 dollars
# We want to see if with this sample the population mean of this year would be higher or lower
# at a 95% confidence level
# H0: mu <= 3800, H1: mu> 3800
# As we do not know last year's sd, we use this year's sd,
# and since n> = 30 we can use Z (N (0,1)), otherwise we would have to use student's t

# We calculate the Z statistic
z_estimated <- (mean ( price ) - 3800 ) / (sd ( price ) / sqrt ( num_diamonds )) # 7.611842
z_estimated

# We calculate the theoretical value of the Z distribution
z.teorica <- qnorm ( 0.05 , 0 , 1 , FALSE ) # 1.644854
# or qnorm (0.95,0,1, TRUE)
z. meteoric

# Conclusion: The statistic is too high and falls within the rejection region
# => I have to reject H0 => H1: mu> 3800 is true with 95% CI



# Another possibility could be:
# The "Ideal" cut quality of a diamond is known to be a binomial of approximately p = 0.4
# With this sample we can infer in the population at 95% that the quality of the "Ideal" diamond
# follow that proportion?
# H0: p = 0.4, H1: p <> 0.4. We use the two-tailed prop.test

prop.test ( x = length ( cut [ cut == " Ideal " ]), n = num_diamonds , p = 0.4 , alternative  =  " two " , conf.level  =  0.99 )

# Conclusion: It gives us a very high value of p-value = 0.8833> 0.001 -> We will enter the acceptance region
# => We have to accept H0 => p = 0.4 for the "Ideal" cutoff within the population




# ### Relationships between variables ####
# Shows the relationships that exist between variables (at least two)
# As we have analyzed the variables, it seems clear that some are related
# as is the case of price with carat and with x, y, z.
# We check correlation
head ( df )
cor ( df [, c ( " price " , " carat " , " x " , " y " , " z " , " depth " , " table " )])
# The linear correlation that exists between them is clearly seen

cor ( price , carat ) # 0.9215722
cor ( price , x ) # 0.887206
cor ( price , y ) # 0.8888018
cor ( price , z ) # 0.8820979
# With depth and with table there is practically no correlation
cor ( price , depth ) # -0.01045122
cor ( price , table ) # 0.1273655

# If we represent it, we see that there is a certain linearity:
qplot ( carat , price , data = df )
qplot ( x , price , data = df )
qplot ( y , price , data = df )
qplot ( z , price , data = df )

# We can even try to show all
# Here you can see how as carat and price increase so does the volume = x * y * z
qplot ( carat , price , data = df , color = x * y * z )

# Or relate them to the qualitative variables:

# Here we see how depending on the type of clarity of the diamond, the weight increases
# to a greater or lesser extent the price
qplot ( carat , price , data = df , color = clarity ) # Clarity is also thought to influence price

# Here we can see how depending on the type of cut, the price does not increase when it increases
# the weight. It's independent.
qplot ( carat , price , data = diamonds , facets = cut ~ . )
qplot ( carat , price , data = diamonds , color = cut )

# Here we can see that the color also seems to vary the relation of the weight with
# the price. Although not as obvious as clarity
qplot ( carat , price , data = diamonds , color = color )

# CONCLUSION: Price, weight and volume (x, y, z) are highly linearly correlated
# Although also quantitative characteristics are -> It would be necessary to code them and see their correlation
# We for now will stick with the previous ones.

# In fact an even higher linear correlation can be found if we transform:
qplot (log ( carat ), log ( price ), data = df )
cor (log ( price ), log ( carat )) # 0.9659173 when before it was around 0.92

qplot (log ( price ), log ( x * y * z ), data = df )
cor (log ( price ), log ( x * y * z )) # 0.96657



# ###### Regression analysis ##########
# a) Formulate a regression model and analyze the results

# First we are going to create a new volume column
df $ volume <- x * y * z
# Then we are going to categorize the quantitative variables that we find
# seen are related: clarity, color
levels ( clarity )
df $ clarity_cat <- ifelse ( clarity == " I1 " , 1 ,
                             ifelse ( clarity == " SI2 " , 2 ,
                                      ifelse ( clarity == " SI1 " , 3 ,
                                               ifelse ( clarity == " VS2 " , 4 ,
                                                        ifelse ( clarity == " VS1 " , 5 ,
                                                                 ifelse ( clarity == " VVS2 " , 6 ,
                                                                          ifelse ( clarity == " VVS1 " , 7 ,
                                                                                   8 )))))))

levels ( color )
df $ color_cat <- ifelse ( color == " D " , 1 ,
                           ifelse ( color == " E " , 2 ,
                                    ifelse ( color == " F " , 3 ,
                                             ifelse ( color == " G " , 4 ,
                                                      ifelse ( color == " H " , 5 ,
                                                               ifelse ( color == " I " , 6 ,
                                                                        7 ))))))

attach ( df )


# EYE: To formulate a model you have to check the partial correlations first
cor ( df [, c ( " price " , " carat " , " volume " , " clarity_cat " , " color_cat " )])
# As we already saw, we have a very high correlation of price with carat and with volume,
# we see that we also have some correlation with clarity and color.

# 1. Let's see the partial correlation that carat has with volume
parcor (cov ( df [, c ( " carat " , " volume " )])) # 0.9989449
# In fact:
qplot (log ( carat ), log ( volume ), data = df )

# Therefore we are going to stay with carat which is the one that has the most correlation with price

# 2. We see if carat has correlation with clarity or with color
parcor (cov ( df [, c ( " carat " , " clarity_cat " )])) # -0.3527278
parcor (cov ( df [, c ( " carat " , " color_cat " )])) # 0.2913072
# It seems that it is acceptable, then we will see it as we include it in the model (by stepwise)

parcor (cov ( df [, c ( " clarity_cat " , " color_cat " )])) # Neither: 0.02579131

rm1  <- lm ( price ~ carat )
summary ( rm1 )
# We have a very low p-value (<2e-16) for both b0 and b1 (with a very high student's t).
# Then we have to reject H0: b0 = 0 and b1 = 0, that is, we have a lot of security in the coefficients
# However, the coefficient of determination is 85% and that is because the residual SC is
# very high
# Residuals:
# Min 1Q Median 3Q Max
# -18583.5 -804.3 -19.0 537.0 12731.6

# It is clearly seen in the regression
plot ( carat , price )
abline ( rm1 , col = " blue " , lwd = 2 )




# b) Show the residuals and analyze the results

summary ( rm1 $ residuals )
plot ( rm1 $ residuals )
abline ( h = 0 , col = " red " , lwd = 3 )

# We clearly see that there is a pattern, when the estimated price is between 2000 and 3000.
# We do not have a vertical dispersion like the horizontal
# => In this case it is clear to apply a logarithmic transformation as we have been seeing
# when analyzing variables and relating them



# c) Apply a transformation to the regression and analyze the results

rm2  <- lm (log ( price ) ~ log ( carat ))
summary ( rm2 )

# We continue to have a very low p-value (<2e-16), that is, we continue to trust our coeff
# However now the coefficient of determination is 93% because the residuals are much
# tighter
# Residuals:
# Min 1Q Median 3Q Max
# -1.50859 -0.16950 -0.00594 0.16632 1.3379
sd ( rm1 $ residuals ) # Before 1547.8
sd ( rm2 $ residuals ) # Now 0.2626104

# We can see it in the regression
plot (log ( price ) ~ log ( carat ))
abline ( rm2 , col = " blue " , lwd = 2 )

# Analyzing the waste
summary ( rm2 $ residuals )
plot (log ( price ), rm2 $ residuals )
# They have a perfect dispersion around 0

# At this point we have a fairly tight simple linear regression.
# log (price) = 1.675944 + log (carat) +8.448780

# But we can consider including some other variable such as clarity or color
# to see if our model fits a bit better.
rm3  <- lm (log ( price ) ~ log ( carat ) + clarity_cat )
summary ( rm3 )
# It seems that if, R adjusted = 96% and also our theoretical F is very high with its low p-value
# F-statistic: 6.903e + 05 on 2 and 53905 DF, p-value: <2.2e-16
# With which there is no combination between them that should be removed.

sd ( rm3 $ residuals ) # 0.1966681
# Analyzing the waste
summary ( rm3 $ residuals )
plot ( 1.8000229 * log ( carat ) + 0.1144724 * clarity_cat + 8.0340296 , rm3 $ residuals )
abline ( h = 0 , col = " red " , lwd = 3 )


# Add the color_cat
rm4  <- lm (log ( price ) ~ log ( carat ) + clarity_cat + color_cat )
summary ( rm4 )
# Perfect we adjust a little more: R adjusted = 97.8%, without damaging the rest:
#   Residual standard error: 0.1503 on 53904 degrees of freedom
#   Multiple R-squared: 0.978, Adjusted R-squared: 0.978
#   F-statistic: 8.005e + 05 on 3 and 53904 DF, p-value: <2.2e-16

sd ( rm4 $ residuals ) # 0.1503166
# Analyzing the waste
summary ( rm4 $ residuals )
plot ( 1.8718461 * log ( carat ) + 0.1263607 * clarity_cat - 0.0779723 * color_cat + 8.2944877 , rm3 $ residuals )
abline ( h = 0 , col = " red " , lwd = 3 )
# And we still have a correct distribution of errors.

# Let's finally test with the interactions of the variables
cor (log ( price ), log ( carat ) * clarity_cat ) # 0.8451771
cor (log ( price ), log ( carat ) * color_cat ) # 0.8667773
cor (log ( price ), clarity_cat * color_cat ) # -0.04731248
cor (log ( price ), log ( carat ) ** 2 ) # -0.7670203
cor (log ( price ), clarity_cat ** 2 ) # -0.2041066
cor (log ( price ), color_cat ** 2 ) # -0.1557188


# They are quite low, we tried the two highest:
inter_carat_clarity <- log ( carat ) * clarity_cat
inter_color_clarity <- log ( carat ) * color_cat
rm5  <- lm (log ( price ) ~ log ( carat ) + clarity_cat + color_cat + inter_carat_clarity + inter_color_clarity )
summary ( rm5 )

# DO NOT ADD MORE INFORMATION TO THE MODEL WITH WHICH THE MOST ADJUSTED MODEL WOULD BE:
# log (price) = 1.8718461 * log (carat) + 0.1263607 * clarity_cat-0.0779723 * color_cat + 8.2944877


# Interpret the standardized coefficients of the regression
# If we take a random value:
val_azar <- df [ 25340 ,]
val_azar
estimated.price = exp ( 1.8718461 * log ( random_val $ carat ) + 0.1263607 * random_val $ clarity_cat - 0.0779723 * random_val $ color_cat + 8.2944877 )
price.estimated  # 729.1531 versus real 642

# To really know which variable is contributing more we should type:
media_carat <- mean (log ( carat ))
media_clarity <- mean ( clarity_cat )
media_color <- mean ( color_cat )
sd_carat <- sd (log ( carat ))
sd_clarity <- sd ( clarity_cat )
sd_color <- sd ( color_cat )

carat_tip <- (log ( carat ) - media_carat ) / sd_carat
clarity_tip <- ( clarity_cat - media_clarity ) / sd_clarity
color_tip <- ( color_cat - media_color ) / sd_color

rm_tip  <- lm (log ( price ) ~ carat_tip + clarity_tip + color_tip )
summary ( rm_tip )

# Remaining as follows:
# log (price) = 1.0944921 * log (carat) + 0.2081267 * clarity-0.1326555 * color_tip + 0
# Where B0 = 0, and the one that contributes the most is carat, followed by clarity and finally color
# If we had 2 diamonds of the same clarity and the same color and different weights
# price = exp (1.0944 * weight_diff)
# Idem can be applied to the rest

# Finally, comment that the price estimates that generate the modest regression have an uncertainty
# These coefficients are not exactly the real ones, they are the best estimators. There is always an uncertainty:
# This uncertainty is defined by the following confidence intervals:
confint ( rm_tip )

#                   2.5% 97.5%
# (Intercept) 7.7850590 7.7875970
# carat_tip 1.0930545 1.0959297
# clarity_tip 0.2067380 0.2095154
# color_tip -0.1339828 -0.1313281


# ############ END ####################
detach ( df )

