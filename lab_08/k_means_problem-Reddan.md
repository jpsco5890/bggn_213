K-Means Problem
================
Jack Reddan
10/21/2021

# Try K-Means Clustering

Generate fake data and explore how the method works.

## Generate example data

``` r
tmp <- c(rnorm(30,-3), rnorm(30,3))
hist(tmp)
```

![](k_means_problem-Reddan_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

## Generate multidimensional example data

``` r
x <- cbind(x = tmp, y = rev(tmp))

plot(x)
```

![](k_means_problem-Reddan_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Use the kmeans() function to explore the fake data

Use it while specifying 2 expected clusters and iterating 20 times.

``` r
clusters <- kmeans(x, centers = 2, nstart = 20)

clusters
```

    ## K-means clustering with 2 clusters of sizes 30, 30
    ## 
    ## Cluster means:
    ##           x         y
    ## 1  2.988735 -2.873831
    ## 2 -2.873831  2.988735
    ## 
    ## Clustering vector:
    ##  [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1
    ## [39] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 43.18625 43.18625
    ##  (between_SS / total_SS =  92.3 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

> \[Q\] How many points are in each cluster?

There are 30 points in each cluster.

``` r
clusters$size
```

    ## [1] 30 30

> \[Q\] What component of your results object dteails:

> > Cluster size

``` r
clusters$size
```

    ## [1] 30 30

> > Cluster assignment

``` r
clusters$cluster
```

    ##  [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1
    ## [39] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

> > Cluster center

``` r
clusters$centers
```

    ##           x         y
    ## 1  2.988735 -2.873831
    ## 2 -2.873831  2.988735

### Plot x colored by the kmeans cluster centers as blue points

Load ggplot2

``` r
library(ggplot2)
```

Convert matrices to be used in ggplot to data frames.

``` r
df <- data.frame(x)
centroids <- data.frame(clusters$centers)
```

Plot the original data colored by kmenas clusters and add blue
centroids. IBM’s colorblind palette is used.

``` r
ggplot(data = df) +
  aes(x = x, y = y, color = factor(clusters$cluster)) +
  geom_point() +
  scale_color_manual(values = c("#785EF0", "#FE6100"), name = "Cluster") +
  geom_point(data = centroids, aes(x = x, y = y), color = "#648FFF", shape = 8, size = 5)
```

![](k_means_problem-Reddan_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
# Try Hierarchical Clustering

Using the same example data *‘x’*.

## Generate the distance matrix

``` r
dm <- dist(x)

str(dm)
```

    ##  'dist' num [1:1770] 1.861 0.929 0.944 0.479 1.141 ...
    ##  - attr(*, "Size")= int 60
    ##  - attr(*, "Diag")= logi FALSE
    ##  - attr(*, "Upper")= logi FALSE
    ##  - attr(*, "method")= chr "euclidean"
    ##  - attr(*, "call")= language dist(x = x)

## Call hclust() to determine clusters

``` r
hc <- hclust(dm)
hc
```

    ## 
    ## Call:
    ## hclust(d = dm)
    ## 
    ## Cluster method   : complete 
    ## Distance         : euclidean 
    ## Number of objects: 60

## Plot the hierachical cluster

``` r
plot(hc)
```

![](k_means_problem-Reddan_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
## ‘*Cut*’ the dendrogram to assign membership of ‘*leaves*’ to clusters
Can either specify the height (*h*) or desired number of clusters (*k*).

``` r
trimmed_hc <- cutree(hc, h = 6)
trimmed_hc
```

    ##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
    ## [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

``` r
trimmed_hc_v2 <- cutree(hc, k = 2)
trimmed_hc == trimmed_hc_v2
```

    ##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [16] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [31] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [46] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

## Plot x data colored by the hierarchical cluster membership

``` r
ggplot(data = df) +
  aes(x = x, y = y, color = factor(trimmed_hc)) +
  geom_point() +
  scale_color_manual(values = c("#785EF0", "#FE6100"), name = "Cluster")
```

![](k_means_problem-Reddan_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
