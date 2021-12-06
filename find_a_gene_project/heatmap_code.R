library(bio3d)
library(pheatmap)
library(extrafont)

aln <- read.fasta("gor-MAFFT_aln-20_short.faa")
seq_id <- seqidentity(aln)

IBMColorBlindSafe = c("#785ef0", "#648fff", "#ffffff", "#ffb000", "#fe6100")
colBlindScale <- colorRampPalette(IBMColorBlindSafe)
hmColors <- colBlindScale(512)

loadfonts(device="postscript")
postscript("seqID_heatmap.eps",
           height = 12,
           width = 18,
           family = "Liberation Sans",
           paper = "special",
           onefile = FALSE,
           horizontal = FALSE)
pheatmap(seq_id,
         color = hmColors,
         display_numbers = TRUE,
         fontsize_number = 10,
         cutree_rows = 8,
         treeheight_col = 0)
dev.off()
