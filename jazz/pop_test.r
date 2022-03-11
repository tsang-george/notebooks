# load track popularity data
track_pop <- read.csv("C:/your_path/track_pop.csv", quote="")

# Shapiro-Wilk test for normality
apply(track_pop, 2, shapiro.test)

# all p-values are way smaller than 0.05
# Ho that data are normally distributed is rejected

# import tidyr to reshape data (dropped NA)
library(tidyr)
track_long <- gather(track_pop, "category", "popularity", na.rm = TRUE)

# import car to conduct Brown-Forsythe test for equal variance
# Brown-Forsythe is more robust than Levene's test for non-normally distributed data
library(car)
leveneTest(track_long$popularity, track_long$category)

# p-value is way smaller than 0.05
# Ho of equal variance is rejected

# Kruskal-Wallis test for non-normally distributed data of unequal variance
kruskal.test(track_pop)

# p-value is way smaller than 0.05
# Ho that all groups having same median ranks is rejected

# import library for post-hoc: Dunn's test for identifying groups with different median
library(FSA)
dunnTest(track_long$popularity, track_long$category, method = 'bh')

# p-values of pair no.4 to 9 are way smaller than 0.05
# Ho that groups in each of those pairs having the same median is rejected


# load album popularity data
album_pop <- read.csv("C:/your_path/album_pop.csv", quote="")

# Shapiro-Wilk test for normality
apply(album_pop, 2, shapiro.test)

# all p-values are way smaller than 0.05
# Ho that data are normally distributed is rejected

# tidyr to reshape data (dropped NA)
album_long <- gather(album_pop, "category", "popularity", na.rm = TRUE)

# Brown-Forsythe test for equal variance
leveneTest(album_long$popularity, album_long$category)

# p-value is way smaller than 0.05
# Ho of equal variance is rejected

# Kruskal-Wallis test for unequal variance for non-normally distributed data
kruskal.test(album_pop)

# p-value is way smaller than 0.05
# Ho that all groups having same median ranks is rejected

# Post-hoc: Dunn's test for identifying groups with different median
dunnTest(album_long$popularity, album_long$category, method = 'bh')

# p-values of pair no.4 to 9 are way smaller than 0.05
# Ho that groups in each of those pairs having the same median is rejected


# load artist popularity data
artist_pop <- read.csv("C:/your_path/artist_pop.csv", quote="")

# Shapiro-Wilk test for normality
apply(artist_pop, 2, shapiro.test)

# p-values of artists competing in Latin Jazz Album and
# Large Jazz Ensemble Album categories are smaller than 0.05
# Ho that data are normally distributed is rejected
# Data of other groups are not normally distributed

# tidyr to reshape data (dropped NA)
artist_long <- gather(artist_pop, "category", "popularity", na.rm = TRUE)

# Brown-Forsythe test for equal variance
leveneTest(artist_long$popularity, artist_long$category)

# p-value is larger than 0.05
# Ho of equal variance is accepted

# Kruskal-Wallis test also works for data of equal variance
kruskal.test(artist_pop)

# p-value is way smaller than 0.05
# Ho that all groups having same median ranks is rejected

# Post-hoc: Dunn's test 
# for identifying groups with different median
dunnTest(artist_long$popularity, artist_long$category, method = 'bh')

# p-values of pair no.4 to 9 are way smaller than 0.05
# Ho that groups in each of those pairs having the same median is rejected

# library: car, tidyr, lattice, psych, cowbell, ggplot2, cowplot, fsa
