fromJSON("C:/Users/Diag Davenport/Desktop/Edumacation, because degrees mean more than intelligence/Georgetown/Food-Network/train.json") -> d
head(d)
strsplit(d$ingredients, ",")
strsplit(d$ingredients, "[],]")
strsplit(d$ingredients, "[,]")
class(d$ingredients)
d$ingredients[1]

# Create the initial adjacency matrix with all the pairings, zero it out

# Then cycle through each of the 40,000recipes and populate

# Then apply the community identification algorithm