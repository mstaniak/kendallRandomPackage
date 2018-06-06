# kendallRandomWalks

[![Travis-CI Build Status](https://travis-ci.org/mstaniak/kendallRandomPackage.svg?branch=master)](https://travis-ci.org/mstaniak/kendallRandomPackage)
[![Coverage Status](https://img.shields.io/codecov/c/github/mstaniak/kendallRandomPackage/master.svg)](https://codecov.io/github/mstaniak/kendallRandomPackage?branch=master)



Simulations and distributions related to Kendall random walks:
[visit dedicated project page on Researchgate](https://www.researchgate.net/project/First-order-Kendall-maximal-autoregressive-processes-and-their-applications)

Install the newest version:

>devtools::install_github("mstaniak/kendallRandomPackage")

Help:
>?kendallRandomPackage

Main functionalities:

  * `simulate_kendall_rw` functions simulates Kendall random walks for a given step distribution. The simulation can be then plotted using generic `plot` function.
  * `transform_kendall_rw` function allows user to play with different scalings and transformations of Kendall random walks to study its properties related to convergence.
  * `ladder_moment` and `ladder_height` functions help study distribution of first ladder moment and first ladder height empirically.
  * `ladder_moment_pmf` gives exact PMF for first ladder moment.
  * `pkend`, `dkend`, `qkend` and `rkend` are typical functions related to the stable Kendall distribution.
  * `g_function` calculates Williamson transform numerically.
  
See vignette for examples.

If you have a feature request or you found a bug, please leave an issue.
The goal of this package is to let anyone interested in Kendall convolution/generalized convolutions get familiar with them through visual means and experimentation. 


Acknowledgement

This work is a part of project "First order Kendall maximal autoregressive processes and their applications", which is carried out within the POWROTY/REINTEGRATION programme of the Foundation for Polish Science co-financed by the European Union under the European Regional Development Fund.
