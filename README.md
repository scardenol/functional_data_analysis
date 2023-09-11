# What is this?
A repository for the research of clustering methods for functional data analysis on high-dimensional data.

# What is the problem?
Extending clustering methods on high-dimensional functional data is an active field of research in both real-world industries and academic applications. The problem is basically characterized by the nature of the data:
- The data space is high-dimensional (a non-trivial amount of rows and columns).
- The data is functional: the data is continuum as it comes from a continuum source (usually time-dependent).
- Clustering data of this nature implies that a cluster consist not of data points but by many observations of related data points usually known as "curves".

# What do we do to solve this problem?
We develop both and extension and a new methodology to perform data analysis on this kind of data by taking two approaches:
- A distance-based traditional crisp (or hard) clustering where we compare our own custom functional PAM method with the functional version of the traditional k-means in both an euclidean and manhattan normed spaces.
- A density-based fuzzy (or soft) clustering where we introduce a new methodology by using the HDBSCAN (Hierarchical Density-Based Spatial Clustering of Applications with Noise) method to cluster the data with no hard assumptions like the distance-based methods.
