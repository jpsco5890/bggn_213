Mini Project
================
Jack Reddan (PID: A59010543)
10/27/2021

> Load libraries

``` r
library(ggplot2)
library(factoextra)
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(rgl)
```

# Exploratroy data analysis

## Organizing the data

First I read in the data from ’WisconsinCancer.csv” with the first
column as the row names.

``` r
fna_data <- "WisconsinCancer.csv"
wisconsin_df <- read.csv(fna_data, row.names = 1)
```

Then I look at the data to asses the structure of the data.

``` r
str(wisconsin_df)
```

    ## 'data.frame':    569 obs. of  32 variables:
    ##  $ diagnosis              : chr  "M" "M" "M" "M" ...
    ##  $ radius_mean            : num  18 20.6 19.7 11.4 20.3 ...
    ##  $ texture_mean           : num  10.4 17.8 21.2 20.4 14.3 ...
    ##  $ perimeter_mean         : num  122.8 132.9 130 77.6 135.1 ...
    ##  $ area_mean              : num  1001 1326 1203 386 1297 ...
    ##  $ smoothness_mean        : num  0.1184 0.0847 0.1096 0.1425 0.1003 ...
    ##  $ compactness_mean       : num  0.2776 0.0786 0.1599 0.2839 0.1328 ...
    ##  $ concavity_mean         : num  0.3001 0.0869 0.1974 0.2414 0.198 ...
    ##  $ concave.points_mean    : num  0.1471 0.0702 0.1279 0.1052 0.1043 ...
    ##  $ symmetry_mean          : num  0.242 0.181 0.207 0.26 0.181 ...
    ##  $ fractal_dimension_mean : num  0.0787 0.0567 0.06 0.0974 0.0588 ...
    ##  $ radius_se              : num  1.095 0.543 0.746 0.496 0.757 ...
    ##  $ texture_se             : num  0.905 0.734 0.787 1.156 0.781 ...
    ##  $ perimeter_se           : num  8.59 3.4 4.58 3.44 5.44 ...
    ##  $ area_se                : num  153.4 74.1 94 27.2 94.4 ...
    ##  $ smoothness_se          : num  0.0064 0.00522 0.00615 0.00911 0.01149 ...
    ##  $ compactness_se         : num  0.049 0.0131 0.0401 0.0746 0.0246 ...
    ##  $ concavity_se           : num  0.0537 0.0186 0.0383 0.0566 0.0569 ...
    ##  $ concave.points_se      : num  0.0159 0.0134 0.0206 0.0187 0.0188 ...
    ##  $ symmetry_se            : num  0.03 0.0139 0.0225 0.0596 0.0176 ...
    ##  $ fractal_dimension_se   : num  0.00619 0.00353 0.00457 0.00921 0.00511 ...
    ##  $ radius_worst           : num  25.4 25 23.6 14.9 22.5 ...
    ##  $ texture_worst          : num  17.3 23.4 25.5 26.5 16.7 ...
    ##  $ perimeter_worst        : num  184.6 158.8 152.5 98.9 152.2 ...
    ##  $ area_worst             : num  2019 1956 1709 568 1575 ...
    ##  $ smoothness_worst       : num  0.162 0.124 0.144 0.21 0.137 ...
    ##  $ compactness_worst      : num  0.666 0.187 0.424 0.866 0.205 ...
    ##  $ concavity_worst        : num  0.712 0.242 0.45 0.687 0.4 ...
    ##  $ concave.points_worst   : num  0.265 0.186 0.243 0.258 0.163 ...
    ##  $ symmetry_worst         : num  0.46 0.275 0.361 0.664 0.236 ...
    ##  $ fractal_dimension_worst: num  0.1189 0.089 0.0876 0.173 0.0768 ...
    ##  $ X                      : logi  NA NA NA NA NA NA ...

The first row contains the diagnoses which are the outputs we are trying
to derive from the data. This will be removed to generate the naive
dataframe. Additionally, these results are saved as the vector
‘diagnosis’.

``` r
# The data seems to have a extraneous ',' at the end of the column names
# Take out the artifact column 'X' from the read-in.
naive_wisconsin_df <- wisconsin_df[,c(-1, -ncol(wisconsin_df))]
diagnosis <- factor(wisconsin_df[,1])
```

## Explore the data

``` r
nrow(wisconsin_df)
```

    ## [1] 569

There are 569 rows/observations.

``` r
table(diagnosis)
```

    ## diagnosis
    ##   B   M 
    ## 357 212

212 of the observations have a “M” or malignant diagnoses.

``` r
length(grep("_mean", colnames(naive_wisconsin_df)))
```

    ## [1] 10

There are 10 variables/features (columns) which are suffixed with
`_mean`.

# Principal Component Analysis

## Performing PCA

Check the means and Standard deviations of the variables.

``` r
colMeans(naive_wisconsin_df)
```

    ##             radius_mean            texture_mean          perimeter_mean 
    ##            1.412729e+01            1.928965e+01            9.196903e+01 
    ##               area_mean         smoothness_mean        compactness_mean 
    ##            6.548891e+02            9.636028e-02            1.043410e-01 
    ##          concavity_mean     concave.points_mean           symmetry_mean 
    ##            8.879932e-02            4.891915e-02            1.811619e-01 
    ##  fractal_dimension_mean               radius_se              texture_se 
    ##            6.279761e-02            4.051721e-01            1.216853e+00 
    ##            perimeter_se                 area_se           smoothness_se 
    ##            2.866059e+00            4.033708e+01            7.040979e-03 
    ##          compactness_se            concavity_se       concave.points_se 
    ##            2.547814e-02            3.189372e-02            1.179614e-02 
    ##             symmetry_se    fractal_dimension_se            radius_worst 
    ##            2.054230e-02            3.794904e-03            1.626919e+01 
    ##           texture_worst         perimeter_worst              area_worst 
    ##            2.567722e+01            1.072612e+02            8.805831e+02 
    ##        smoothness_worst       compactness_worst         concavity_worst 
    ##            1.323686e-01            2.542650e-01            2.721885e-01 
    ##    concave.points_worst          symmetry_worst fractal_dimension_worst 
    ##            1.146062e-01            2.900756e-01            8.394582e-02

``` r
apply(naive_wisconsin_df, 2, sd)
```

    ##             radius_mean            texture_mean          perimeter_mean 
    ##            3.524049e+00            4.301036e+00            2.429898e+01 
    ##               area_mean         smoothness_mean        compactness_mean 
    ##            3.519141e+02            1.406413e-02            5.281276e-02 
    ##          concavity_mean     concave.points_mean           symmetry_mean 
    ##            7.971981e-02            3.880284e-02            2.741428e-02 
    ##  fractal_dimension_mean               radius_se              texture_se 
    ##            7.060363e-03            2.773127e-01            5.516484e-01 
    ##            perimeter_se                 area_se           smoothness_se 
    ##            2.021855e+00            4.549101e+01            3.002518e-03 
    ##          compactness_se            concavity_se       concave.points_se 
    ##            1.790818e-02            3.018606e-02            6.170285e-03 
    ##             symmetry_se    fractal_dimension_se            radius_worst 
    ##            8.266372e-03            2.646071e-03            4.833242e+00 
    ##           texture_worst         perimeter_worst              area_worst 
    ##            6.146258e+00            3.360254e+01            5.693570e+02 
    ##        smoothness_worst       compactness_worst         concavity_worst 
    ##            2.283243e-02            1.573365e-01            2.086243e-01 
    ##    concave.points_worst          symmetry_worst fractal_dimension_worst 
    ##            6.573234e-02            6.186747e-02            1.806127e-02

Run the ‘prcomp()’ on the Wisconsin dataframe with scaling to account
for different variables having variable scales of values.

``` r
wisconsin_pca <- prcomp(naive_wisconsin_df, scale = TRUE)
```

``` r
summary(wisconsin_pca)
```

    ## Importance of components:
    ##                           PC1    PC2     PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     3.6444 2.3857 1.67867 1.40735 1.28403 1.09880 0.82172
    ## Proportion of Variance 0.4427 0.1897 0.09393 0.06602 0.05496 0.04025 0.02251
    ## Cumulative Proportion  0.4427 0.6324 0.72636 0.79239 0.84734 0.88759 0.91010
    ##                            PC8    PC9    PC10   PC11    PC12    PC13    PC14
    ## Standard deviation     0.69037 0.6457 0.59219 0.5421 0.51104 0.49128 0.39624
    ## Proportion of Variance 0.01589 0.0139 0.01169 0.0098 0.00871 0.00805 0.00523
    ## Cumulative Proportion  0.92598 0.9399 0.95157 0.9614 0.97007 0.97812 0.98335
    ##                           PC15    PC16    PC17    PC18    PC19    PC20   PC21
    ## Standard deviation     0.30681 0.28260 0.24372 0.22939 0.22244 0.17652 0.1731
    ## Proportion of Variance 0.00314 0.00266 0.00198 0.00175 0.00165 0.00104 0.0010
    ## Cumulative Proportion  0.98649 0.98915 0.99113 0.99288 0.99453 0.99557 0.9966
    ##                           PC22    PC23   PC24    PC25    PC26    PC27    PC28
    ## Standard deviation     0.16565 0.15602 0.1344 0.12442 0.09043 0.08307 0.03987
    ## Proportion of Variance 0.00091 0.00081 0.0006 0.00052 0.00027 0.00023 0.00005
    ## Cumulative Proportion  0.99749 0.99830 0.9989 0.99942 0.99969 0.99992 0.99997
    ##                           PC29    PC30
    ## Standard deviation     0.02736 0.01153
    ## Proportion of Variance 0.00002 0.00000
    ## Cumulative Proportion  1.00000 1.00000

> by the first principal component (PC1)?

*See above*  
44.27%

> \[Q05\]: How many principal components (PCs) are required to describe
> at least 70% of the original variance in the data?

*See above*  
Three PCs \[PC1 - PC3\], explains 72.636%.

> \[Q06\]: How many principal components (PCs) are required to describe
> at least 90% of the original variance in the data?

*See above*  
Seven PCs \[PC1 - PC7\], explains 91.010%.

## Interpreting PCA Results

``` r
biplot(wisconsin_pca)
```

![](lab_09-Reddan_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

> understand? Why?

``` r
plot(x = wisconsin_pca$x[,1], y = wisconsin_pca$x[,2], 
     col = diagnosis, 
     xlab = "PC1", ylab = "PC2")
```

![](lab_09-Reddan_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

> notice about these plots?

``` r
plot(x = wisconsin_pca$x[,1], y = wisconsin_pca$x[,3], 
     col = diagnosis, 
     xlab = "PC1", 
     ylab = "PC3")
```

![](lab_09-Reddan_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
wisconsin_pca_df <- as.data.frame(wisconsin_pca$x)
wisconsin_pca_df$diagnosis <- diagnosis 
```

``` r
ggplot(data = wisconsin_pca_df) +
  aes(x = PC1, y = PC2,
      col = diagnosis) +
  geom_point()
```

![](lab_09-Reddan_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## Variance Explained

``` r
wisconsin_pca_variance <- wisconsin_pca$sdev^2
head(wisconsin_pca_variance)
```

    ## [1] 13.281608  5.691355  2.817949  1.980640  1.648731  1.207357

``` r
wisconsin_pca_variance_prop <- wisconsin_pca_variance / sum(wisconsin_pca_variance)
```

``` r
plot(wisconsin_pca_variance_prop, 
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1),
     type = "o")
```

![](lab_09-Reddan_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
barplot(wisconsin_pca_variance_prop,
        ylab = "Percent of Variance Explained",
        names.arg = paste0("PC", 1:length(wisconsin_pca_variance_prop)),
        las = 2,
        axes = FALSE)
axis(2, 
     at=wisconsin_pca_variance_prop,
     labels = round(wisconsin_pca_variance_prop, 2)*100)
```

![](lab_09-Reddan_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
fviz_eig(wisconsin_pca, 
         addlabels = TRUE)
```

![](lab_09-Reddan_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

## Communicatin PCA Results

> \[Q09\] For the first principal component, what is the component of
> the loading vector (i.e. `wisc.pr$rotation[,1]`) for the feature
> `concave.points_mean`?

``` r
wisconsin_pca$rotation[grep("concave.points_mean", row.names(wisconsin_pca$rotation)),1]
```

    ## [1] -0.2608538

> \[Q10\] What is the minimum number of principal components required to
> explain 80% of the variance of the data?

``` r
vals = c("sum" = 0, "count" = 1)
while(vals[1] < 0.8){
  vals[1] = vals[1] + wisconsin_pca_variance_prop[vals[2]]
  vals[2] = vals[2] + 1
}

paste0("It takes a minimum of ",  vals[2] - 1, " PCs to explain ", round(vals[1]*100, 2), "% of the data",
       sep = "")
```

    ## [1] "It takes a minimum of 5 PCs to explain 84.73% of the data"

# Hierarchical Clustering

``` r
wisconsin_data_scaled <- scale(naive_wisconsin_df)
wisconsin_data_distance <- dist(wisconsin_data_scaled)
```

``` r
wisconsin_data_hclust <- hclust(wisconsin_data_distance, method = "complete")
```

> clustering model has 4 clusters?

``` r
plot(wisconsin_data_hclust)
abline(a= 19, b = 0,
       col = "red",
       lty = 2)
```

![](lab_09-Reddan_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
wisconsin_hclust_clusters <- cutree(wisconsin_data_hclust, k = 4)
```

``` r
table(wisconsin_hclust_clusters, diagnosis)
```

    ##                          diagnosis
    ## wisconsin_hclust_clusters   B   M
    ##                         1  12 165
    ##                         2   2   5
    ##                         3 343  40
    ##                         4   0   2

> different number of clusters between 2 and 10?

``` r
for(i in 2:10){
  wisconsin_hclust_clusters <- cutree(wisconsin_data_hclust, k = i)
  cat(paste0(i, "\n", sep = ""))
  print(table(wisconsin_hclust_clusters, diagnosis))
}
```

    ## 2
    ##                          diagnosis
    ## wisconsin_hclust_clusters   B   M
    ##                         1 357 210
    ##                         2   0   2
    ## 3
    ##                          diagnosis
    ## wisconsin_hclust_clusters   B   M
    ##                         1 355 205
    ##                         2   2   5
    ##                         3   0   2
    ## 4
    ##                          diagnosis
    ## wisconsin_hclust_clusters   B   M
    ##                         1  12 165
    ##                         2   2   5
    ##                         3 343  40
    ##                         4   0   2
    ## 5
    ##                          diagnosis
    ## wisconsin_hclust_clusters   B   M
    ##                         1  12 165
    ##                         2   0   5
    ##                         3 343  40
    ##                         4   2   0
    ##                         5   0   2
    ## 6
    ##                          diagnosis
    ## wisconsin_hclust_clusters   B   M
    ##                         1  12 165
    ##                         2   0   5
    ##                         3 331  39
    ##                         4   2   0
    ##                         5  12   1
    ##                         6   0   2
    ## 7
    ##                          diagnosis
    ## wisconsin_hclust_clusters   B   M
    ##                         1  12 165
    ##                         2   0   3
    ##                         3 331  39
    ##                         4   2   0
    ##                         5  12   1
    ##                         6   0   2
    ##                         7   0   2
    ## 8
    ##                          diagnosis
    ## wisconsin_hclust_clusters   B   M
    ##                         1  12  86
    ##                         2   0  79
    ##                         3   0   3
    ##                         4 331  39
    ##                         5   2   0
    ##                         6  12   1
    ##                         7   0   2
    ##                         8   0   2
    ## 9
    ##                          diagnosis
    ## wisconsin_hclust_clusters   B   M
    ##                         1  12  86
    ##                         2   0  79
    ##                         3   0   3
    ##                         4 331  39
    ##                         5   2   0
    ##                         6  12   0
    ##                         7   0   2
    ##                         8   0   2
    ##                         9   0   1
    ## 10
    ##                          diagnosis
    ## wisconsin_hclust_clusters   B   M
    ##                        1   12  86
    ##                        2    0  59
    ##                        3    0   3
    ##                        4  331  39
    ##                        5    0  20
    ##                        6    2   0
    ##                        7   12   0
    ##                        8    0   2
    ##                        9    0   2
    ##                        10   0   1

``` r
wisconsin_kmeans <- kmeans(wisconsin_data_scaled, centers = 2, nstart = 20)
```

``` r
table(wisconsin_kmeans$cluster, diagnosis)
```

    ##    diagnosis
    ##       B   M
    ##   1 343  37
    ##   2  14 175

``` r
table(wisconsin_hclust_clusters, wisconsin_kmeans$cluster)
```

    ##                          
    ## wisconsin_hclust_clusters   1   2
    ##                        1   17  81
    ##                        2    0  59
    ##                        3    0   3
    ##                        4  358  12
    ##                        5    0  20
    ##                        6    0   2
    ##                        7    5   7
    ##                        8    0   2
    ##                        9    0   2
    ##                        10   0   1

# Combining Methods

``` r
wisconsin_pr_hclust <- hclust(dist(wisconsin_pca$x[,1:7]), method = "ward.D2")
```

``` r
groups <- cutree(wisconsin_pr_hclust, k = 2)
table(groups)
```

    ## groups
    ##   1   2 
    ## 216 353

``` r
table(groups, diagnosis)
```

    ##       diagnosis
    ## groups   B   M
    ##      1  28 188
    ##      2 329  24

``` r
plot(wisconsin_pca$x[,1:2], col=groups)
```

![](lab_09-Reddan_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
plot(wisconsin_pca$x[,1:2], col=diagnosis)
```

![](lab_09-Reddan_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

``` r
re_group <- as.factor(groups)
levels(re_group)
```

    ## [1] "1" "2"

``` r
re_group <- relevel(re_group, 2)
levels(re_group)
```

    ## [1] "2" "1"

``` r
plot(wisconsin_pca$x[,1:2], col=re_group)
```

![](lab_09-Reddan_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
plot3d(wisconsin_pca$x[,1:3], 
       xlab = "PC 1",
       ylab = "PC 2",
       zlab = "PC 3",
       cex = 1.5,
       size = 1,
       type = "s",
       col = groups)
rglwidget(width = 400, height = 400)
```

    ## Warning in snapshot3d(scene = x, width = width, height = height): webshot = TRUE
    ## requires the webshot2 package; using rgl.snapshot() instead

![](/tmp/RtmppWKI9x/file4ab74b04102.png)<!-- -->

``` r
table(wisconsin_kmeans$cluster, diagnosis)
```

    ##    diagnosis
    ##       B   M
    ##   1 343  37
    ##   2  14 175

``` r
table(wisconsin_hclust_clusters, diagnosis)
```

    ##                          diagnosis
    ## wisconsin_hclust_clusters   B   M
    ##                        1   12  86
    ##                        2    0  59
    ##                        3    0   3
    ##                        4  331  39
    ##                        5    0  20
    ##                        6    2   0
    ##                        7   12   0
    ##                        8    0   2
    ##                        9    0   2
    ##                        10   0   1
