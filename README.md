# compete
Analyzing head-to-head competitive interaction data


-----
### Author: James P. Curley, 
### Contributions: Jiayi Fu, Ke Shen & Ziheng Huang
### Maintainer: James P. Curley
email:  curley AT utexas DOT edu



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
- rshps  - get summary info of all relationships in matrix
- unknonws - number of unknowns in matrix
- sparseness - get sparseness of matrix




------

#### Published papers

_Please let me know if you use this package in your papers. Unfortunately I don't have time to compile a complete list of papers using it, but if you let me know I'll add here._

_This package was used in the following papers:_

<br>

Arseneau-RobarabAnouk JM, 2017, Intra- and interindividual differences in the costs and benefits of intergroup aggression in female vervet monkeys, Animal Behaviour 123: 129-137, https://www.sciencedirect.com/science/article/pii/S0003347216302858.

Pineda-Galindo E, Cedra-Molina AL, Mayagoitia-Novales L, Matamoros-Trejo G, de la O C, 2017, Biological Validataions of Fecal Glucocorticoid, Testosterone, and Progesterone Metabolite Measurements in Captive Stumptail Macaques (Macaca arctoides), Int J Primatol DOIL 10.1007/s10764-017-9992-7

Tong X et al, 2020, Reestablishment of Social Hierarchies in Weaned Pigs after Mixing, Animals 2020, 10(1), 36; https://doi.org/10.3390/ani10010036

van Overveld T et al, 2018,  Food predictability and social status drive individual resource specializations in a territorial vulture, Scientific Reports 8: 15155  https://www.nature.com/articles/s41598-018-33564-y

van Overveld T et al, 2020, Seasonal grouping dynamics in a territorial vulture: ecological drivers and social consequences, Behavioral Ecology and Sociobiology volume 74: 28 https://link.springer.com/article/10.1007/s00265-020-2807-4

Ebenau A et al 2019, Personality homophily affects male social bonding in wild Assamese macaques, Macaca assamensis,  Animal Behaviour 155:21-35 https://www.sciencedirect.com/science/article/pii/S0003347219302052

Mishra PS et al. 2020,  Do males bond? A study of male-male relationships in Nicobar long-tailed macaques Macaca fascicularis umbrosus, Journal of Biosciences 45:22. https://link.springer.com/article/10.1007/s12038-020-9995-y

Diniz P et al 2019, Angry caciques: intrasexual aggression in a Neotropical colonial blackbird, Ethology, Ecology & Evolution 31:205-218.

<br>
<br>

_Some preprints (as of Feb 2020):_

Vilette C. et al., Ranking the Ranking Methodologies: Determining Temporal Stability in Dominance Hierarchies, https://www.biorxiv.org/content/10.1101/692384v1.abstract

Karamihalev S. et al., Sexually divergent effects of social dominance on chronic stress outcomes in mice, https://www.biorxiv.org/content/10.1101/2020.02.04.933481v1


<br>
<br>

_Some papers from my lab also:_

[See publication list of Curley Lab here](https://labs.la.utexas.edu/curley/publications/) for some examples.

So, N. et al. 2015, A Social Network Approach Reveals Associations between Mouse Social Dominance and Brain Gene Expression,  <a href="http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0134509" target="_blank">PLOS ONE</a>

Curley JP, 2016, Temporal Pairwise-Correlation Analysis Provides Empirical Support for Attention Hierarchies in Mice, Biology Letters

Williamson C, Lee W & Curley JP, 2016, Temporal Dynamics of Social Hierarchy Formation and Maintenance in Male Mice., Animal Behaviour.

Williamson CM, Franks B & Curley JP, 2016, Mouse Social Network Dynamics and Community Structure are Associated with Brain Plasticity-Related Gene Expression, Frontiers in Behavioral Neuroscience 10:152.

Williamson CM, Romeo RD & Curley JP, 2017, Dynamic changes in social dominance and mPOA GnRH expression in male mice following social opportunity, Hormones & Behavior 87:80-88. 

Williamson CM, Lee W, Romeo RD & Curley JP, 2017, Social context-dependent relationships between mouse dominance rank and plasma hormone levels. Physiology & Behavior.

Lee W, Hiura L, Yang E, Broekman KA, Ophir AG, Curley J. 2019. Social status in mouse social hierarchies is associated with variation in oxytocin and vasopressin 1a receptor densities. Hormones and Behavior.

Williamson CM, Lee W, Decasien AR, Lanham A, Romeo RD, Curley JP. Social hierarchy position in female mice is associated with plasma corticosterone levels and hypothalamic gene expression. Sci Reports. 2019 Jan 1:529131.


<br>
<br>
