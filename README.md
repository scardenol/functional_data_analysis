# What is this?
A repository for the research of clustering methods for functional data analysis on high-dimensional data.

# What is the problem?
Extending clustering methods on high-dimensional functional data is an active field of research in both real-world industry and academic applications. The problem is basically characterized by the nature of the data:
- The data space is high-dimensional (a non-trivial amount of rows and columns).
- The data is functional: the data is continuum as it comes from a continuum source (usually time-dependent).
- Clustering data of this nature implies that a cluster consist not of data points but by many observations of related data points usually known as "curves".

# What do we do to solve this problem?
We develop both and extension and a new methodology to perform data analysis on this kind of data by taking two approaches:
- A distance-based traditional crisp (or hard) clustering where we compare our own custom functional PAM method with the functional version of the traditional k-means in both an euclidean and manhattan normed spaces.
- A density-based fuzzy (or soft) clustering where we introduce a new methodology by using the HDBSCAN (Hierarchical Density-Based Spatial Clustering of Applications with Noise) method to cluster the data with no hard assumptions like the distance-based methods.

# What programming language do we use?
We develop everything in the programming language `R 4.2.2` using both the traditional IDE `RStudio 2023.06.1+524` and the popular IDE `VS Code 1.82`.

# Why `R`?
The reasoning behind using this programming language is because the most serious research done on this subject is published in `R` code, as it already has peer-reviewed and cited libraries/packages on functional data analysis such as (`fda.usc`)[https://cran.r-project.org/web/packages/fda.usc/fda.usc.pdf].

# Project structure
```bash
functional_data_analysis
├───data
└───src
    ├───images
    │   └───lib
    │       ├───crosstalk-1.2.0
    │       │   ├───css
    │       │   ├───js
    │       │   └───scss
    │       ├───htmlwidgets-1.6.2
    │       ├───jquery-3.5.1
    │       ├───plotly-binding-4.10.1
    │       ├───plotly-htmlwidgets-css-2.11.1
    │       ├───plotly-main-2.11.1
    │       └───typedarray-0.1
    └───modules
        ├───clustering
        ├───library_manager
        ├───numerical
        ├───preprocessing
        ├───read_data
        ├───unused
        ├───visualization
        └───write_data
```
