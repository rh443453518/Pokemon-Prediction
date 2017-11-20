library(corrplot)
library(ggplot2)
Pokemon = read.csv("Pokemon.csv")

# Boxplot comparing the stat between legendary and non-lengendary
leg_total = ggplot(Pokemon, aes(x=factor(Legendary), y=Total)) + geom_boxplot() + ggtitle("Total of Legendary vs Non-Legendary")
leg_total

leg_hp = ggplot(Pokemon, aes(x=factor(Legendary), y=HP)) + geom_boxplot() + ggtitle("HP of Legendary vs Non-Legendary")
leg_hp

leg_att = ggplot(Pokemon, aes(x=factor(Legendary), y=Attack)) + geom_boxplot() + ggtitle("Attack of Legendary vs Non-Legendary")
leg_att

leg_def = ggplot(Pokemon, aes(x=factor(Legendary), y=Defense)) + geom_boxplot() + ggtitle("Defense of Legendary vs Non-Legendary")
leg_def

leg_spa = ggplot(Pokemon, aes(x=factor(Legendary), y=Pokemon[,9])) + geom_boxplot() + ggtitle("Sp. Attack of Legendary vs Non-Legendary")
leg_spa

leg_spd = ggplot(Pokemon, aes(x=factor(Legendary), y=Pokemon[,10])) + geom_boxplot() + ggtitle("Sp. Defense of Legendary vs Non-Legendary")
leg_spd

leg_speed = ggplot(Pokemon, aes(x=factor(Legendary), y=Speed)) + geom_boxplot() + ggtitle("Speed of Legendary vs Non-Legendary")
leg_speed

# Boxplot comparing different types of Pokemon
type_total = ggplot(Pokemon, aes(x=Pokemon[,3], y=Total)) + geom_boxplot() + ggtitle("Total stat of different types") + labs(x="Pokemon Type")
type_total

type_hp = ggplot(Pokemon, aes(x=Pokemon[,3], y=HP)) + geom_boxplot() + ggtitle("HP of different types") + labs(x="Pokemon Type")
type_hp

type_att = ggplot(Pokemon, aes(x=Pokemon[,3], y=Attack)) + geom_boxplot() + ggtitle("Attack of different types") + labs(x="Pokemon Type")
type_att

type_def = ggplot(Pokemon, aes(x=Pokemon[,3], y=Defense)) + geom_boxplot() + ggtitle("Defense of different types") + labs(x="Pokemon Type")
type_def

type_spa = ggplot(Pokemon, aes(x=Pokemon[,3], y=Pokemon[,9])) + geom_boxplot() + ggtitle("Sp. Attack of different types") + labs(x="Pokemon Type")
type_spa

type_spd = ggplot(Pokemon, aes(x=Pokemon[,3], y=Pokemon[,10])) + geom_boxplot() + ggtitle("Sp. Defense of different types") + labs(x="Pokemon Type")
type_spd

type_speed = ggplot(Pokemon, aes(x=Pokemon[,3], y=Speed)) + geom_boxplot() + ggtitle("Speed of different types") + labs(x="Pokemon Type")
type_speed

# Boxplot comparing different generation
gen_total = ggplot(Pokemon, aes(x = factor(Generation), y=Total)) + geom_boxplot() + ggtitle("Total stat of different generations") + labs(x="Generation")
gen_total

# Compare Correlation of 6 stats
Pokemon_stats = Pokemon[,6:11]
M = cor(Pokemon_stats)
corrplot(M, method = "square")