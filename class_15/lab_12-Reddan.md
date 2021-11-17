Lab 12
================
Jack Reddan (PID: A59010543)

# Bioconductor and DESeq2 Setup

``` r
library(BiocManager)
library(DESeq2)
library(dplyr)
library(ggplot2)
library("AnnotationDbi")
library("org.Hs.eg.db")
```

# Import **countData** and **colData**

``` r
# Read in the scaled counts data as counts and the metadata data as metadata
counts <- read.csv("airway_scaledcounts.csv", row.names = 1)
metadata <- read.csv("airway_metadata.csv")
```

``` r
head(counts)
```

    ##                 SRR1039508 SRR1039509 SRR1039512 SRR1039513 SRR1039516
    ## ENSG00000000003        723        486        904        445       1170
    ## ENSG00000000005          0          0          0          0          0
    ## ENSG00000000419        467        523        616        371        582
    ## ENSG00000000457        347        258        364        237        318
    ## ENSG00000000460         96         81         73         66        118
    ## ENSG00000000938          0          0          1          0          2
    ##                 SRR1039517 SRR1039520 SRR1039521
    ## ENSG00000000003       1097        806        604
    ## ENSG00000000005          0          0          0
    ## ENSG00000000419        781        417        509
    ## ENSG00000000457        447        330        324
    ## ENSG00000000460         94        102         74
    ## ENSG00000000938          0          0          0

``` r
head(metadata)
```

    ##           id     dex celltype     geo_id
    ## 1 SRR1039508 control   N61311 GSM1275862
    ## 2 SRR1039509 treated   N61311 GSM1275863
    ## 3 SRR1039512 control  N052611 GSM1275866
    ## 4 SRR1039513 treated  N052611 GSM1275867
    ## 5 SRR1039516 control  N080611 GSM1275870
    ## 6 SRR1039517 treated  N080611 GSM1275871

Check if metadata and count data match:

``` r
all(metadata$id == colnames(counts))
```

    ## [1] TRUE

#### \[Q1\]: How many genes are in this data set?

``` r
nrow(counts)
```

    ## [1] 38694

#### \[Q2\]: How many ‘control’ cell lines do we have?

``` r
table(metadata$dex)
```

    ## 
    ## control treated 
    ##       4       4

# Toy Differential Gene Expression

``` r
control_md <- metadata[metadata$dex == "control",]
control_counts <- counts[,control_md$id]
control_mean <- rowSums(control_counts)/4
head(control_mean)
```

    ## ENSG00000000003 ENSG00000000005 ENSG00000000419 ENSG00000000457 ENSG00000000460 
    ##          900.75            0.00          520.50          339.75           97.25 
    ## ENSG00000000938 
    ##            0.75

Same as above, but using dplyr.

``` r
control_md <- metadata %>% filter(dex=="control")
control_counts <- counts %>% dplyr::select(control_md$id) 
control_mean <- rowSums(control_counts)/4
head(control_mean)
```

    ## ENSG00000000003 ENSG00000000005 ENSG00000000419 ENSG00000000457 ENSG00000000460 
    ##          900.75            0.00          520.50          339.75           97.25 
    ## ENSG00000000938 
    ##            0.75

#### \[Q3\]: How would you make the above code in either approach more robust?

Change rowSums to rowMeans to avoid hard-coding number of smaples.

``` r
control_md <- metadata %>% filter(dex=="control")
control_counts <- counts %>% dplyr::select(control_md$id) 
control_mean <- rowMeans(control_counts)
head(control_mean)
```

    ## ENSG00000000003 ENSG00000000005 ENSG00000000419 ENSG00000000457 ENSG00000000460 
    ##          900.75            0.00          520.50          339.75           97.25 
    ## ENSG00000000938 
    ##            0.75

#### \[Q4\]: Follow the same procedure for the `treated` samples.

``` r
treated_md <- metadata %>% filter(dex=="treated")
treated_counts <- counts %>% dplyr::select(treated_md$id) 
treated_mean <- rowSums(treated_counts)/nrow(treated_md)
head(treated_mean)
```

    ## ENSG00000000003 ENSG00000000005 ENSG00000000419 ENSG00000000457 ENSG00000000460 
    ##          658.00            0.00          546.00          316.50           78.75 
    ## ENSG00000000938 
    ##            0.00

``` r
mean_counts <- data.frame("control" = control_mean, "treated" = treated_mean)
colSums(mean_counts)
```

    ##  control  treated 
    ## 23005324 22196524

#### \[Q5a\]: Create a scatter plot showing the mean of the treated samples against the mean of the control samples.

``` r
plot(x = mean_counts$control, y = mean_counts$treated,
     xlab = "Control", ylab = "Treated")
```

![](lab_12-Reddan_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

#### \[Q5b\]: You could also use the ggplot2 package to make this figure. What geom\_?() function would you use for this plot?

``` r
ggplot(mean_counts) +
  aes(x = control, y = treated) +
  geom_point(alpha = 0.4) +
  labs(x = "Control", y = "Treated")
```

![](lab_12-Reddan_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

#### \[Q6\]: Try plotting both axes on a log scale. What is the argument to plot() that allows you to do this?

``` r
plot(x = mean_counts$control, y = mean_counts$treated,
     xlab = "log(Control)", ylab = "log(Treated)", log = "yx")
```

    ## Warning in xy.coords(x, y, xlabel, ylabel, log): 15032 x values <= 0 omitted
    ## from logarithmic plot

    ## Warning in xy.coords(x, y, xlabel, ylabel, log): 15281 y values <= 0 omitted
    ## from logarithmic plot

![](lab_12-Reddan_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
mean_counts$log2fc <- log2(mean_counts$treated/mean_counts$control)
head(mean_counts)
```

    ##                 control treated      log2fc
    ## ENSG00000000003  900.75  658.00 -0.45303916
    ## ENSG00000000005    0.00    0.00         NaN
    ## ENSG00000000419  520.50  546.00  0.06900279
    ## ENSG00000000457  339.75  316.50 -0.10226805
    ## ENSG00000000460   97.25   78.75 -0.30441833
    ## ENSG00000000938    0.75    0.00        -Inf

Remove the rows with zero values present.

``` r
zeros_rows <- which(mean_counts[,c(1,2)] == 0, arr.ind = TRUE)
remove_rows <- unique(zeros_rows[,1])
nonzero_counts <- mean_counts[-remove_rows,]
head(nonzero_counts)
```

    ##                 control treated      log2fc
    ## ENSG00000000003  900.75  658.00 -0.45303916
    ## ENSG00000000419  520.50  546.00  0.06900279
    ## ENSG00000000457  339.75  316.50 -0.10226805
    ## ENSG00000000460   97.25   78.75 -0.30441833
    ## ENSG00000000971 5219.00 6687.50  0.35769358
    ## ENSG00000001036 2327.00 1785.75 -0.38194109

#### \[Q7\]: What is the purpose of the arr.ind argument in the which() function call above? Why would we then take the first column of the output and need to call the unique() function?

The purpose of `arr.ind` is to return the array indices of for `TRUE`
values, rows and columns. The first column contains the rows which were
`TRUE` for a given column, specified in the second column of this
output. Therefore, unique will pull out all rows which have a zero in
either or both columns.

``` r
up_indx <- nonzero_counts$log2fc > 2
down_indx <- nonzero_counts$log2fc < -2
```

#### \[Q8\]: Using the `up_indx` vector, can you determine how many up regulated genes we have at the greater than 2 fc level?

``` r
sum(up_indx)
```

    ## [1] 250

#### \[Q9\]: Using the `down_indx` vector, can you determine how many down regulated genes we have at the greater than 2 fc level?

``` r
sum(down_indx)
```

    ## [1] 367

#### \[Q10\]: Do you trust these results? Why or why not?

I do not trust these results since there is not accountability for genes
with a high variance in gene expression data. Means do not represent the
data well enough to rely on solely, statistics would be needed to
identify with the fold change is significance or is observed by chance.

# DESeq2 Analysis

``` r
citation("DESeq2")
```

    ## 
    ##   Love, M.I., Huber, W., Anders, S. Moderated estimation of fold change
    ##   and dispersion for RNA-seq data with DESeq2 Genome Biology 15(12):550
    ##   (2014)
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Article{,
    ##     title = {Moderated estimation of fold change and dispersion for RNA-seq data with DESeq2},
    ##     author = {Michael I. Love and Wolfgang Huber and Simon Anders},
    ##     year = {2014},
    ##     journal = {Genome Biology},
    ##     doi = {10.1186/s13059-014-0550-8},
    ##     volume = {15},
    ##     issue = {12},
    ##     pages = {550},
    ##   }

## Importing Data

``` r
Dds <- DESeqDataSetFromMatrix(countData = counts,
                              colData = metadata,
                              design = ~dex)
```

    ## converting counts to integer mode

    ## Warning in DESeqDataSet(se, design = design, ignoreRank): some variables in
    ## design formula are characters, converting to factors

``` r
Dds
```

    ## class: DESeqDataSet 
    ## dim: 38694 8 
    ## metadata(1): version
    ## assays(1): counts
    ## rownames(38694): ENSG00000000003 ENSG00000000005 ... ENSG00000283120
    ##   ENSG00000283123
    ## rowData names(0):
    ## colnames(8): SRR1039508 SRR1039509 ... SRR1039520 SRR1039521
    ## colData names(4): id dex celltype geo_id

## DESeq Analysis

``` r
# Must run DESeq first
# results(Dds)

Dds <- DESeq(Dds)
```

    ## estimating size factors

    ## estimating dispersions

    ## gene-wise dispersion estimates

    ## mean-dispersion relationship

    ## final dispersion estimates

    ## fitting model and testing

## Getting Results

``` r
res <- results(Dds)
res
```

    ## log2 fold change (MLE): dex treated vs control 
    ## Wald test p-value: dex treated vs control 
    ## DataFrame with 38694 rows and 6 columns
    ##                  baseMean log2FoldChange     lfcSE      stat    pvalue
    ##                 <numeric>      <numeric> <numeric> <numeric> <numeric>
    ## ENSG00000000003  747.1942     -0.3507030  0.168246 -2.084470 0.0371175
    ## ENSG00000000005    0.0000             NA        NA        NA        NA
    ## ENSG00000000419  520.1342      0.2061078  0.101059  2.039475 0.0414026
    ## ENSG00000000457  322.6648      0.0245269  0.145145  0.168982 0.8658106
    ## ENSG00000000460   87.6826     -0.1471420  0.257007 -0.572521 0.5669691
    ## ...                   ...            ...       ...       ...       ...
    ## ENSG00000283115  0.000000             NA        NA        NA        NA
    ## ENSG00000283116  0.000000             NA        NA        NA        NA
    ## ENSG00000283119  0.000000             NA        NA        NA        NA
    ## ENSG00000283120  0.974916      -0.668258   1.69456 -0.394354  0.693319
    ## ENSG00000283123  0.000000             NA        NA        NA        NA
    ##                      padj
    ##                 <numeric>
    ## ENSG00000000003  0.163035
    ## ENSG00000000005        NA
    ## ENSG00000000419  0.176032
    ## ENSG00000000457  0.961694
    ## ENSG00000000460  0.815849
    ## ...                   ...
    ## ENSG00000283115        NA
    ## ENSG00000283116        NA
    ## ENSG00000283119        NA
    ## ENSG00000283120        NA
    ## ENSG00000283123        NA

``` r
summary(res)
```

    ## 
    ## out of 25258 with nonzero total read count
    ## adjusted p-value < 0.1
    ## LFC > 0 (up)       : 1563, 6.2%
    ## LFC < 0 (down)     : 1188, 4.7%
    ## outliers [1]       : 142, 0.56%
    ## low counts [2]     : 9971, 39%
    ## (mean count < 10)
    ## [1] see 'cooksCutoff' argument of ?results
    ## [2] see 'independentFiltering' argument of ?results

Change the alpha cut-off to 0.05 rather than the default 0.1.

``` r
res_005 <- results(Dds, alpha = 0.05)
summary(res_005)
```

    ## 
    ## out of 25258 with nonzero total read count
    ## adjusted p-value < 0.05
    ## LFC > 0 (up)       : 1236, 4.9%
    ## LFC < 0 (down)     : 933, 3.7%
    ## outliers [1]       : 142, 0.56%
    ## low counts [2]     : 9033, 36%
    ## (mean count < 6)
    ## [1] see 'cooksCutoff' argument of ?results
    ## [2] see 'independentFiltering' argument of ?results

# Adding Annotation Data

``` r
columns(org.Hs.eg.db)
```

    ##  [1] "ACCNUM"       "ALIAS"        "ENSEMBL"      "ENSEMBLPROT"  "ENSEMBLTRANS"
    ##  [6] "ENTREZID"     "ENZYME"       "EVIDENCE"     "EVIDENCEALL"  "GENENAME"    
    ## [11] "GENETYPE"     "GO"           "GOALL"        "IPI"          "MAP"         
    ## [16] "OMIM"         "ONTOLOGY"     "ONTOLOGYALL"  "PATH"         "PFAM"        
    ## [21] "PMID"         "PROSITE"      "REFSEQ"       "SYMBOL"       "UCSCKG"      
    ## [26] "UNIPROT"

``` r
res$symbol <- mapIds(org.Hs.eg.db,
                     keys = row.names(res),
                     keytype = "ENSEMBL",
                     column = "SYMBOL",
                     mutliVals = "first")
```

    ## 'select()' returned 1:many mapping between keys and columns

``` r
head(res)
```

    ## log2 fold change (MLE): dex treated vs control 
    ## Wald test p-value: dex treated vs control 
    ## DataFrame with 6 rows and 7 columns
    ##                   baseMean log2FoldChange     lfcSE      stat    pvalue
    ##                  <numeric>      <numeric> <numeric> <numeric> <numeric>
    ## ENSG00000000003 747.194195     -0.3507030  0.168246 -2.084470 0.0371175
    ## ENSG00000000005   0.000000             NA        NA        NA        NA
    ## ENSG00000000419 520.134160      0.2061078  0.101059  2.039475 0.0414026
    ## ENSG00000000457 322.664844      0.0245269  0.145145  0.168982 0.8658106
    ## ENSG00000000460  87.682625     -0.1471420  0.257007 -0.572521 0.5669691
    ## ENSG00000000938   0.319167     -1.7322890  3.493601 -0.495846 0.6200029
    ##                      padj      symbol
    ##                 <numeric> <character>
    ## ENSG00000000003  0.163035      TSPAN6
    ## ENSG00000000005        NA        TNMD
    ## ENSG00000000419  0.176032        DPM1
    ## ENSG00000000457  0.961694       SCYL3
    ## ENSG00000000460  0.815849    C1orf112
    ## ENSG00000000938        NA         FGR

#### \[Q11\]: Run the `mapIds()` function two more times to add the Entrez ID and UniProt accession and GENENAME as new columns called `res$entrez`, `res$uniprot` and `res$genename`.

``` r
res$entrez <- mapIds(org.Hs.eg.db,
                     keys = row.names(res),
                     keytype = "ENSEMBL",
                     column = "ENTREZID",
                     mutliVals = "first")
```

    ## 'select()' returned 1:many mapping between keys and columns

``` r
res$uniprot <- mapIds(org.Hs.eg.db,
                     keys = row.names(res),
                     keytype = "ENSEMBL",
                     column = "UNIPROT",
                     mutliVals = "first")
```

    ## 'select()' returned 1:many mapping between keys and columns

``` r
res$gene_name <- mapIds(org.Hs.eg.db,
                     keys = row.names(res),
                     keytype = "ENSEMBL",
                     column = "GENENAME",
                     mutliVals = "first")
```

    ## 'select()' returned 1:many mapping between keys and columns

``` r
head(res)
```

    ## log2 fold change (MLE): dex treated vs control 
    ## Wald test p-value: dex treated vs control 
    ## DataFrame with 6 rows and 10 columns
    ##                   baseMean log2FoldChange     lfcSE      stat    pvalue
    ##                  <numeric>      <numeric> <numeric> <numeric> <numeric>
    ## ENSG00000000003 747.194195     -0.3507030  0.168246 -2.084470 0.0371175
    ## ENSG00000000005   0.000000             NA        NA        NA        NA
    ## ENSG00000000419 520.134160      0.2061078  0.101059  2.039475 0.0414026
    ## ENSG00000000457 322.664844      0.0245269  0.145145  0.168982 0.8658106
    ## ENSG00000000460  87.682625     -0.1471420  0.257007 -0.572521 0.5669691
    ## ENSG00000000938   0.319167     -1.7322890  3.493601 -0.495846 0.6200029
    ##                      padj      symbol      entrez     uniprot
    ##                 <numeric> <character> <character> <character>
    ## ENSG00000000003  0.163035      TSPAN6        7105  A0A024RCI0
    ## ENSG00000000005        NA        TNMD       64102      Q9H2S6
    ## ENSG00000000419  0.176032        DPM1        8813      O60762
    ## ENSG00000000457  0.961694       SCYL3       57147      Q8IZE3
    ## ENSG00000000460  0.815849    C1orf112       55732  A0A024R922
    ## ENSG00000000938        NA         FGR        2268      P09769
    ##                              gene_name
    ##                            <character>
    ## ENSG00000000003          tetraspanin 6
    ## ENSG00000000005            tenomodulin
    ## ENSG00000000419 dolichyl-phosphate m..
    ## ENSG00000000457 SCY1 like pseudokina..
    ## ENSG00000000460 chromosome 1 open re..
    ## ENSG00000000938 FGR proto-oncogene, ..

``` r
p_val_order <- order(res$padj)
head(res[p_val_order,])
```

    ## log2 fold change (MLE): dex treated vs control 
    ## Wald test p-value: dex treated vs control 
    ## DataFrame with 6 rows and 10 columns
    ##                  baseMean log2FoldChange     lfcSE      stat      pvalue
    ##                 <numeric>      <numeric> <numeric> <numeric>   <numeric>
    ## ENSG00000152583   954.771        4.36836 0.2371268   18.4220 8.74490e-76
    ## ENSG00000179094   743.253        2.86389 0.1755693   16.3120 8.10784e-60
    ## ENSG00000116584  2277.913       -1.03470 0.0650984  -15.8944 6.92855e-57
    ## ENSG00000189221  2383.754        3.34154 0.2124058   15.7319 9.14433e-56
    ## ENSG00000120129  3440.704        2.96521 0.2036951   14.5571 5.26424e-48
    ## ENSG00000148175 13493.920        1.42717 0.1003890   14.2164 7.25128e-46
    ##                        padj      symbol      entrez     uniprot
    ##                   <numeric> <character> <character> <character>
    ## ENSG00000152583 1.32441e-71     SPARCL1        8404  A0A024RDE1
    ## ENSG00000179094 6.13966e-56        PER1        5187      O15534
    ## ENSG00000116584 3.49776e-53     ARHGEF2        9181      Q92974
    ## ENSG00000189221 3.46227e-52        MAOA        4128      P21397
    ## ENSG00000120129 1.59454e-44       DUSP1        1843      B4DU40
    ## ENSG00000148175 1.83034e-42        STOM        2040      F8VSL7
    ##                              gene_name
    ##                            <character>
    ## ENSG00000152583           SPARC like 1
    ## ENSG00000179094 period circadian reg..
    ## ENSG00000116584 Rho/Rac guanine nucl..
    ## ENSG00000189221    monoamine oxidase A
    ## ENSG00000120129 dual specificity pho..
    ## ENSG00000148175               stomatin

Write the DESeq2 results to disc.

``` r
write.csv(res[p_val_order,], "deseq_results.csv")
```

# Data Visualization

## Volcano Plots

``` r
plot(x = res$log2FoldChange, y = -log(res$padj),
     xlab = "Log2(FC)", ylab = "-Log(P-Value)")
```

![](lab_12-Reddan_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->
