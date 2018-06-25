# GenSynthNetMet Package
Contents of the package include R code, datsets, and documentation. 

## Package Description
Utilities include synthesizing social networks based on Myers Briggs Type Indicators (MBTI)
and searching for cohorts that produce networks with an realistic set of metrics. A cohort is a
collection of MBTI profiles where the number reflects the number of nodes in a real world
network. Realistic means that the metrics are closer to the metrics of a real world network
than the averaged metrics of 40 randomly generated networks that have the same number of nodes
and network density as the real world network. Search functions apply the Monte Carlo (MC)
method and a Genetic Algorithm (GA). In both types of search functions, the difference between
the averaged metrics of the randomly generated networks and the real world metrics serve as a
basis of comparison. Differences between the averaged metrics of 40 MBTI based synthesized
and the real world network metrics are calculated and compared to the basis. The MC based
synthesizes 40 networks based upon a randomly generated cohort, calculates and averages the
metrics and compares the results to the basis. If a cohort is found that beats the basis then
that cohort is saved and the process repeats. If another viable cohort is found it is compared
to the previously saved cohort. If it performs better then it is save to the list of winning
cohorts. The GA search synthesizes populations of cohorts and rank orders the cohorts based
upon the number of wins, i.e., metrics where the cohort performed better than the basis of
comparison. The two best cohorts provide the starting point for the next generation; which
is generated via cross-over methods and mutation. Utility functions support the search
functions by generating the networks, calculating the metrics, averaging the metrics,
generating comparison tables, and generating reports.

## Vignettes
* [Monte Carlo search for optimum set of metrics](https://daoneil.github.io/NetworkMetricSearch/executeMCSearch.html)
* [Genetic Algorithm based search for an optimum set of metrics](https://daoneil.github.io/NetworkMetricSearch/executeGAMetSearch.html)
