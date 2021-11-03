Structural Bioinformatics (pt. 1)
================
Jack Reddan

# Introduction to the RCSB Protein Data Bank (PDB)

## PDB Statistics

``` r
pdb_stats <- read.csv("pdb_statistics.csv")

pdb_stats
```

    ##            Molecular.Type  X.ray   NMR   EM Multiple.methods Neutron Other
    ## 1          Protein (only) 142419 11807 6038              177      70    32
    ## 2 Protein/Oligosaccharide   8426    31  991                5       0     0
    ## 3              Protein/NA   7498   274 2000                3       0     0
    ## 4     Nucleic acid (only)   2368  1378   60                8       2     1
    ## 5                   Other    149    31    3                0       0     0
    ## 6  Oligosaccharide (only)     11     6    0                1       0     4
    ##    Total
    ## 1 160543
    ## 2   9453
    ## 3   9775
    ## 4   3817
    ## 5    183
    ## 6     22

### \[Q01\]: What percentage of structures in the PDB are solved by X-Ray and Electron Microscopy?

``` r
# x_ray_percent <- round(((sum(pdb_stats$X.ray) / sum(pdb_stats$Total)) * 100), 2)
# paste0("X-Ray: ", x_ray_percent, "%", sep = "")
# 
# EM_percent <- round(((sum(pdb_stats$EM) / sum(pdb_stats$Total)) * 100), 2)
# paste0("XElectron Microscopy: ", EM_percent, "%", sep = "")

round((colSums(pdb_stats[,2:(ncol(pdb_stats)-1)])/sum(pdb_stats$Total)*100), 2)
```

    ##            X.ray              NMR               EM Multiple.methods 
    ##            87.53             7.36             4.95             0.11 
    ##          Neutron            Other 
    ##             0.04             0.02

### \[Q02\]: What percentage of structures in the PDB are protein?

``` r
protein_percent <-round(
  (sum(pdb_stats$Total[grep("Protein", pdb_stats$Molecular.Type)])/sum(pdb_stats$Total)*100), 
  2)
paste0("Proteins: ", protein_percent, "%", sep = "")
```

    ## [1] "Proteins: 97.81%"

### \[Q03\]: Type HIV in the PDB website search box on the home page and determine how many HIV-1 protease structures are in the current PDB?

2343 structures.

## The PDB Format
