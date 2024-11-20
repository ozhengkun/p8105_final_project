exploratory correlation
================
Senna
2024-11-20

``` r
spotify_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
```

    ## Rows: 32833 Columns: 23
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (10): track_id, track_name, track_artist, track_album_id, track_album_na...
    ## dbl (13): track_popularity, danceability, energy, key, loudness, mode, speec...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
library(ggplot2)
library(reshape2)
```

    ## Warning: package 'reshape2' was built under R version 4.4.2

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
numeric_var = spotify_df[sapply(spotify_df, is.numeric)]

cor_matr = melt(cor(numeric_var, use = "complete.obs"))
```

``` r
ggplot(cor_matr, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables", fill = "Correlation")
```

![](correlation_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
cor_pop = spotify_df |>
  select(where(is.numeric)) |> 
  select(-track_popularity) |> 
  summarise(across(everything(), ~ cor(.x, spotify_df$track_popularity, use = "complete.obs"))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "correlation") |> 
  arrange(desc(correlation))

cor_pop
```

    ## # A tibble: 12 × 2
    ##    variable         correlation
    ##    <chr>                  <dbl>
    ##  1 acousticness        0.0852  
    ##  2 danceability        0.0647  
    ##  3 loudness            0.0577  
    ##  4 valence             0.0332  
    ##  5 mode                0.0106  
    ##  6 speechiness         0.00682 
    ##  7 key                -0.000650
    ##  8 tempo              -0.00538 
    ##  9 liveness           -0.0546  
    ## 10 energy             -0.109   
    ## 11 duration_ms        -0.144   
    ## 12 instrumentalness   -0.150

``` r
cor_pop|>
  ggplot(aes(x= variable, y = correlation))+
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "correlation with popularity", x = "Variable", y = "correlation")
```

![](correlation_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
