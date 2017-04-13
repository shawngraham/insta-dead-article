install.packages("textreuse")
vignette("textreuse-introduction", package = "textreuse")
library(devtools) # windows users need Rtools installed, mac users need XCode installed

setwd("/Insta-dead/")
## create a directory with one file = one post from the original captions csv; this directory is called 'posts' below
## we used the split command at the terminal to split the original csv into separate files

library(textreuse)
dir <- ("posts", package = "textreuse")
minhash <- minhash_generator(200, seed = 235)
ats <- TextReuseCorpus(dir = dir,
                       tokenizer = tokenize_ngrams, n = 5,
                       minhash_func = minhash)
buckets <- lsh(ats, bands = 50, progress = FALSE)
candidates <- lsh_candidates(buckets)
scores <- lsh_compare(candidates, ats, jaccard_similarity, progress = FALSE)
scores
write.csv(scores, file="textreusescores.csv")

### from https://lmullen.github.io/civil-procedure-codes/102-clustering.html
library("Matrix")
install.packages("apcluster","readr")
library("apcluster")
library("dplyr")
library("readr")

scores_clustering <- scores %>% 
  filter(score >= 0.5)
scores_clustering
nrow(scores_clustering)

section_names <- lsh_subset(scores_clustering)
head(section_names, 10)

length(section_names)

lookup <- data_frame(section_names, index = 1:length(section_names))
lookup

scores_clustering <- scores_clustering %>% 
  left_join(lookup, by = c("a" = "section_names")) %>% 
  left_join(lookup, by = c("b" = "section_names")) 
scores_clustering

n <- length(section_names)
m <- sparseMatrix(i = scores_clustering$index.x,
                  j = scores_clustering$index.y,
                  x = scores_clustering$score,
                  dims = c(n, n), symmetric = TRUE)
colnames(m) <- section_names
rownames(m) <- section_names

cluster_cache <- "cache/clusters.rds"
if (!file.exists(cluster_cache)) {
  timing <- system.time(
    clu <- apcluster(s = m,
                     maxits = 100e4, convits = 10e4,
                     q = 0,
                     lam = 0.975,
                     seed = 42325, 
                     includeSim = TRUE,
    )
  )
  require("RPushbullet")
  pbPost(title = "Clustering finished",
         body = paste0("Finished in ", timing[["elapsed"]], "."))
  saveRDS(clu, cluster_cache)
} else {
  clu <- readRDS(cluster_cache)
}

clusters <- clu@clusters 
names(clusters) <- names(clu@exemplars)
clusters <- lapply(clusters, names)

