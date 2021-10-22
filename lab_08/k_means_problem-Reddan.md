K-Means Problem
================
Jack Reddan
10/21/2021

# Try K-Means clustering

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

``` r
clusters <- kmeans(x, centers = 2, nstart = 20)

clusters
```

    ## K-means clustering with 2 clusters of sizes 30, 30
    ## 
    ## Cluster means:
    ##           x         y
    ## 1  3.116737 -2.922568
    ## 2 -2.922568  3.116737
    ## 
    ## Clustering vector:
    ##  [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1
    ## [39] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 51.09436 51.09436
    ##  (between_SS / total_SS =  91.5 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

### \[Q\] How many points are in each cluster?

There are 30 points in each cluster.

``` r
clusters$size
```

    ## [1] 30 30

### \[Q\] What component of your results object dteails:

-   Cluster size

``` r
clusters$size
```

    ## [1] 30 30

-   Cluster assignment

``` r
clusters$cluster
```

    ##  [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1
    ## [39] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

-   Cluster center

``` r
clusters$centers
```

    ##           x         y
    ## 1  3.116737 -2.922568
    ## 2 -2.922568  3.116737

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
  geom_point(data = centroids, aes(x = x, y = y), color = "#648FFF", shape = 8)
```

![](k_means_problem-Reddan_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
