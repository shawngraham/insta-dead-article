### Ben Schmidt's WEM
library(devtools)
install_github("bmschmidt/wordVectors")
#install.packages("magrittr")

install.packages("rtools")

## get the diaries into WEM
#install.packages('dplyr')
library(rtools)
library(wordVectors)
library(tsne)
library(magrittr)
library(dplyr)
library(ggplot2)

setwd("/Insta-dead/")

## train a new model

model = train_word2vec("textforvectorsscript.txt",output="captions_vectors.bin",threads = 4,vectors = 1500,window=12,force=TRUE)

## or comment that line out and uncomment the line below to load an existing model
#model = read.vectors("captions_vectors.bin")

##Check
model
tail(rownames(model))

# below are example snippets looking for vectors aligned to particular words or phrases. 
# notforsale
nearest_to(model,model[["notforsale"]])

nfs_words = model %>% nearest_to(model[["notforsale"]],100) %>% names
sample(nfs_words,20)

# forsale
nearest_to(model,model[["forsale"]])

fs_words = model %>% nearest_to(model[[c("forsale")]],100) %>% names
sample(fs_words,10)

# archaeology

nearest_to(model,model[["archaeology"]])

archae_words = model %>% nearest_to(model[[c("archaeology")]],100) %>% names
sample(archae_words,50)

# archaeological

nearest_to(model,model[["archaeological"]])

archaeological_words = model %>% nearest_to(model[[c("archaeological")]],100) %>% names
sample(archaeological_words,50)

# in the examples below, we visualize some of these vectors
# legality

nearest_to(model,model[["legal"]])

legal_words = model %>% nearest_to(model[[c("legal","law","ownership")]],100) %>% names
sample(legal_words,10)

g7 = model[rownames(model) %in% legal_words [1:50],]

group_distances7 = cosineDist(g7,g7) %>% as.dist
plot(as.dendrogram(hclust(group_distances7)),cex=1, main="Cluster dendrogram of the fifty words closest to a 'legal' vector\nin Instagram posts")

# skull

nearest_to(model,model[["skullforsalehuman"]])
skull_words = model %>% nearest_to(model[[c("humanskulls", "realhumanskull", "skull", "humanskull", "skulls")]],100) %>% names
sample(skull_words,50)
g8 = model[rownames(model) %in% skull_words [1:50],]

group_distances8 = cosineDist(g8,g8) %>% as.dist
plot(as.dendrogram(hclust(group_distances8)),cex=1, main="Cluster dendrogram of the fifty words closest to a 'skull' vector\nin Instagram posts")
#

# "skullforsalehuman","skullforsale","humanskullforsale","realhumanskullforsale",
skull_words_for_sale = model %>% nearest_to(model[[c("skull4sale","skullforsalehuman","skullforsale","humanskullforsale","realhumanskullforsale")]],100) %>% names
sample(skull_words_for_sale,50)
g9 = model[rownames(model) %in% skull_words_for_sale [1:50],]

group_distances9 = cosineDist(g9,g9) %>% as.dist
plot(as.dendrogram(hclust(group_distances9)),cex=1, main="Cluster dendrogram of the fifty words closest to a 'skull_for_sale' vector\nin Instagram posts")


#tibetanskull

nearest_to(model,model[["indonesian"]])
tskull_words = model %>% nearest_to(model[[c("tibetanskull","kapala", "kapalaskull", "tibetan", "tibet", "damaru","indonesian")]],100) %>% names
sample(tskull_words,50)
g9 = model[rownames(model) %in% tskull_words [1:50],]

group_distances9 = cosineDist(g9,g9) %>% as.dist

dpi=600    #pixels per square inch
png("rplots-images/plot.png", width=8*dpi, height=8*dpi, res=dpi)
plot(as.dendrogram(hclust(group_distances9)),cex=1, main="Cluster dendrogram of the fifty words closest to a 'tibetanskull' vector\nin Instagram posts")
dev.off()

# bone

nearest_to(model,model[["bone"]])

bone_words = model %>% nearest_to(model[[c("bone")]],100) %>% names
sample(bone_words,50)

  ## get 50 close words to the 'bone' vector
g4 = model[rownames(model) %in% bone_words [1:50],]

group_distances = cosineDist(g4,g4) %>% as.dist
plot(as.dendrogram(hclust(group_distances)),cex=1, main="Cluster dendrogram of the fifty words closest to a 'bone' vector\nin Instagram posts")


#gender?

## get 50 close words to the 'she' vector
nearest_to(model,model[["hers"]])
she_words = model %>% nearest_to(model[[c("she","her","herself")]],100) %>% names
sample(she_words,50)
g = model[rownames(model) %in% she_words [1:50],]
group_distances = cosineDist(g,g) %>% as.dist
plot(as.dendrogram(hclust(group_distances)),horiz=F,cex=1,main="Cluster dendrogram of the fifty words closest to a 'she' vector\nin Instagram posts")


## get 50 close words to the 'he' vector

nearest_to(model,model[["himself"]])
he_words = model %>% nearest_to(model[[c("he","his","him","himself")]],100) %>% names
sample(he_words,50)

g2 = model[rownames(model) %in% he_words [1:50],]

group_distances = cosineDist(g2,g2) %>% as.dist
plot(as.dendrogram(hclust(group_distances)),horiz=F,cex=1,main="Cluster dendrogram of the fifty words closest to a 'he' vector\nin Instagram posts")

## get 50 close words to the "i" vecotr

nearest_to(model,model[["mine"]])
i_words = model %>% nearest_to(model[[c("i","me","mine")]],100) %>% names
sample(i_words,50)

g3 = model[rownames(model) %in% i_words [1:50],]

group_distances = cosineDist(g3,g3) %>% as.dist
plot(as.dendrogram(hclust(group_distances)),horiz=F,cex=1,main="Cluster dendrogram of the fifty words closest to an 'I' vector\nin Instagram posts")


###some tsne plots
plot(model)

dpi=600    #pixels per square inch
png("jan5-newsourcedata/fig-tsne.png", width=14*dpi, height=14*dpi, res=dpi)
some_groups = nearest_to(model,model[[c("forsale","onsale","nfs","notforsale")]],75)
plot(filter_to_rownames(model,names(some_groups)))

dev.off()


##wordscores
word_scores = data.frame(word=rownames(model))

##crossplots


##check that words exist in the model
nearest_to(model,model[["human"]])
nearest_to(model,model[["terrible"]])
#####good/bad, skull/bone

goodness_vector = model[[c("good","better","best")]]-model[[c("bad")]]
osteo_vector = model[[c("human")]] - model[[c("animal")]]

word_scores$osteo_score = model %>% cosineSimilarity(osteo_vector) %>% as.vector
word_scores$goodness_score = cosineSimilarity(model,goodness_vector) %>% as.vector

groups = c("osteo_score","goodness_score")

word_scores %>% mutate( osteoedness=ifelse(osteo_score>0,"human","animal"),goodness=ifelse(goodness_score>0,"positive","negative")) %>% group_by(goodness,osteoedness) %>% filter(rank(-(abs(osteo_score*goodness_score)))<=36) %>% mutate(eval=-1+rank(abs(goodness_score)/abs(osteo_score))) %>% ggplot() + geom_text(aes(x=eval %/% 12,y=eval%%12,label=word,fontface=ifelse(osteoedness=="human",2,3),color=goodness),hjust=0) + facet_grid(goodness~osteoedness) + theme_minimal() + scale_x_continuous("",lim=c(0,3)) + scale_y_continuous("") + labs(title="The top negative (red) and positive(blue)\nused to describe animals (italics) and humans (bold)") + theme(legend.position="none")

#####good/bad, buy/sell
nearest_to(model,model[["onsale"]])

dpi=600    #pixels per square inch
png("figure.png", width=9*dpi, height=8*dpi, res=dpi)

goodness_vector = model[[c("good","better","best")]]-model[[c("bad")]]
commerce_vector = model[[c("forsale","onsale")]] - model[[c("notforsale","nfs")]]

word_scores$commerce_score = model %>% cosineSimilarity(commerce_vector) %>% as.vector
word_scores$goodness_score = cosineSimilarity(model,goodness_vector) %>% as.vector

groups = c("commerce_score","goodness_score")
word_scores %>% mutate( commerceedness=ifelse(commerce_score>0,"forsale","notforsale"),goodness=ifelse(goodness_score>0,"positive","negative")) %>% group_by(goodness,commerceedness) %>% filter(rank(-(abs(commerce_score*goodness_score)))<=36) %>% mutate(eval=-1+rank(abs(goodness_score)/abs(commerce_score))) %>% ggplot() + geom_text(aes(x=eval %/% 12,y=eval%%12,label=word,fontface=ifelse(commerceedness=="forsale",2,3),color=goodness),hjust=0) + facet_grid(goodness~commerceedness) + theme_minimal() + scale_x_continuous("",lim=c(0,3)) + scale_y_continuous("") + labs(title="The top negative (red) and positive(blue)\nused with 'not for sale' (italics) and 'for sale'(bold) words") + theme(legend.position="none")

dev.off()

#####he/she, buy/sell
nearest_to(model,model[["art"]])


gender_vector = model[[c("woman","she","her","herself")]]-model[[c("man","he","his","him","himself")]]
commerce_vector = model[[c("forsale","onsale")]] - model[[c("notforsale","nfs")]]

word_scores$commerce_score = model %>% cosineSimilarity(commerce_vector) %>% as.vector
word_scores$gender_score = cosineSimilarity(model,gender_vector) %>% as.vector

groups = c("gender_score","commerce_score")
word_scores %>% mutate( genderedness=ifelse(gender_score>0,"female","male"),commerce=ifelse(commerce_score>0,"for sale","not for sale")) %>% group_by(commerce,genderedness) %>% filter(rank(-(abs(gender_score*commerce_score)))<=36) %>% mutate(eval=-1+rank(abs(commerce_score)/abs(gender_score))) %>% ggplot() + geom_text(aes(x=eval %/% 12,y=eval%%12,label=word,fontface=ifelse(genderedness=="female",2,3),color=commerce),hjust=0) + facet_grid(commerce~genderedness) + theme_minimal() + scale_x_continuous("",lim=c(0,3)) + scale_y_continuous("") + labs(title="The top not for sale (red) and for sale (blue) words\nused to describe 'male' words (italics) and 'female' words (bold)") + theme(legend.position="none")

### humans/animals, for sale or not

groups = c("commerce_score","osteo_score")
word_scores %>% mutate( commerceedness=ifelse(commerce_score>0,"forsale","notforsale"),osteo=ifelse(osteo_score>0,"human","animal")) %>% group_by(osteo,commerceedness) %>% filter(rank(-(abs(commerce_score*osteo_score)))<=36) %>% mutate(eval=-1+rank(abs(osteo_score)/abs(commerce_score))) %>% ggplot() + geom_text(aes(x=eval %/% 12,y=eval%%12,label=word,fontface=ifelse(commerceedness=="forsale",2,3),color=osteo),hjust=0) + facet_grid(osteo~commerceedness) + theme_minimal() + scale_x_continuous("",lim=c(0,3)) + scale_y_continuous("") + labs(title="The top animal (red) and human words(blue)\nused with 'not for sale' (italics) and 'for sale'(bold) words") + theme(legend.position="none")

####binaries

library(ggplot2)

word_scores$gender_score = model %>% cosineSimilarity(gender_vector) %>% as.vector
word_scores$commerce_score = model %>% cosineSimilarity(commerce_vector) %>% as.vector
word_scores$goodness_score = cosineSimilarity(model,goodness_vector) %>% as.vector
word_scores$osteo_score = model %>% cosineSimilarity(osteo_vector) %>% as.vector
joy_disgust_vector = model[[c("art","love","beautiful","progress","fun","good")]] - model[[c("death","oddity","horror","vampire","witch","weird")]]
word_scores$joy_disgust_score = model %>% cosineSimilarity(joy_disgust_vector) %>% as.vector

ggplot(word_scores %>% filter(abs(joy_disgust_score)>.30)) + geom_bar(aes(y=joy_disgust_score,x=reorder(word,joy_disgust_score),fill=joy_disgust_score<0),stat="identity") + coord_flip()+scale_fill_discrete("Indicative of",labels=c("joy","disgust")) + labs(title="The words showing the strongest skew along \n the continuum from 'joy' to 'disgust''")


ggplot(word_scores %>% filter(abs(gender_score)>.25)) + geom_bar(aes(y=gender_score,x=reorder(word,gender_score),fill=gender_score<0),stat="identity") + coord_flip()+scale_fill_discrete("Indicative of gender",labels=c("she","he")) + labs(title="The words showing the strongest skew along the gender binary")

ggplot(word_scores %>% filter(abs(commerce_score)>.33)) + geom_bar(aes(y=commerce_score,x=reorder(word,commerce_score),fill=commerce_score<0),stat="identity") + coord_flip()+scale_fill_discrete("Indicative of commerce",labels=c("for sale","not for sale")) + labs(title="The words showing the strongest skew along the commerce binary")

ggplot(word_scores %>% filter(abs(osteo_score)>.33)) + geom_bar(aes(y=osteo_score,x=reorder(word,osteo_score),fill=osteo_score<0),stat="identity") + coord_flip()+scale_fill_discrete("Indicative of creature",labels=c("human","animal")) + labs(title="The words showing the strongest skew along the human-animal binary")

### new thoughts

## clean out the tattooists, what do i see? or what other words?
model %>% nearest_to(model[[c("tattoo","tattoos","tattooist")]],20) %>% names

###rejecting vectors


model %>% nearest_to(model[[c("humanskulls", "realhumanskull", "skull", "humanskull", "skulls")]],20) %>% names

#reject selling words
model %>% nearest_to(model[[c("humanskulls", "realhumanskull", "skull", "humanskull", "skulls")]] %>% reject(model[[c("skull4sale","skullforsalehuman","skullforsale","humanskullforsale","realhumanskullforsale")]]),20) %>% names
#maybe reject curios?
model %>% nearest_to(model[[c("humanskulls", "realhumanskull", "skull", "humanskull", "skulls")]] %>% reject(model[[c("curiositycabinet","oddities","curiosities","vultureculture","curiosityreal")]]),20) %>% names


commerceless_model = model %>% reject(model[["forsale"]]-model[["notforsale"]])
commerceless_model %>% nearest_to(commerceless_model[["humanskull"]],20) %>% names

curiosityless_model = model %>% reject(model[[c("curiositycabinet","oddities","curiosities","vultureculture","curiosityreal")]])

curiosityless_model %>% nearest_to(curiosityless_model[["humanskull"]],20) %>% names
