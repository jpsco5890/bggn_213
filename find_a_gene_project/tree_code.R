library(ape)
library(extrafont)

NJTree <- read.tree(file = "gor-MAFFT-20-NJ_tree.tre")


col_vect <- rep("black", length(NJTree$tip.label))
col_vect[grep("Chlorokybus", NJTree$tip.label)] <- "orange"
col_vect[grep("thaliana", NJTree$tip.label)] <- "slateblue2"

loadfonts(device="postscript")
postscript("gor_MAFFT-20-NJ_tree.eps",
           height = 9,
           width = 17,
           family = "Liberation Sans",
           paper = "special",
           onefile = FALSE,
           horizontal = FALSE)
plot(NJTree, 
     edge.width = 1,
     label.offset = 0.02,
     no.margin = TRUE,
     underscore = FALSE,
     lab4ut = "axial",
     align.tip.label = TRUE,
     type = "u",
     cex = 1,
     lwd = 2,
     rotate.tree = -40,
     font = 3,
     tip.color = col_vect)
add.scale.bar(x = 0.5, y = -0.1, lwd = 1)
dev.off()
