# compete
Analyzing head-to-head competitive interaction data


-----
### Author: James P. Curley, 
### Contributions: Jiayi Fu, Ke Shen & Ziheng Huang
### Maintainer: James P. Curley
email:  jc3181  AT columbia DOT edu



-----
##### Installation
```
devtools::install_github('jalapic/compete')
```


##### Citation
Even though this package is in development, if you do use it in published research please cite it as:

Curley, J.P.  2016, compete: Analyzing Social Hierarchies: R package version 0.1


------

#### Functions

- get_wl_matrix - turns edgelist into Win-Loss Sociomatrix
- get_di_matrix - turns Win-Loss Sociomatrix into Binary Win-Loss Sociomatrix
- org_matrix - organize Sociomatrix

- dci - Directional Consistency of Sociomatrix
- phi - Phi Skew-Symmetry Coefficient of Sociomatrix
- dc_test - Randomization Test of Directional Consistency and Phi Skew-Symmetry
- devries - Modified Landau's h' of Sociomatrix and randomization test
- ttri - Triangle Transitivity of Sociomatrix
- ttri_test - Randomization Test of Triangle Transitivity of Sociomatrix
- isi98 - I&SI ranking of Sociomatrix (see documentation)
- isi13 - I&SI ranking of Sociomatrix using updated algorithm (see documentation)
- ds - get David's Scores of Sociomatrix




------

#### Published papers

This package was used in the following papers:

So, N. et al. 2015, A Social Network Approach Reveals Associations between Mouse Social Dominance and Brain Gene Expression,  <a href="http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0134509" target="_blank">PLOS ONE</a>

Curley JP, 2016, Temporal Pairwise-Correlation Analysis Provides Empirical Support for Attention Hierarchies in Mice, Biology Letters

Williamson C, Lee W & Curley JP, 2016, Temporal Dynamics of Social Hierarchy Formation and Maintenance in Male Mice., Animal Behaviour.


<br>
<br>
