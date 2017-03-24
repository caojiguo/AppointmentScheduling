1. one job density.r. 

This file includes functions to compute pdf, cdf, expectation and quantile of total costs of one job with equal or unequal cost coefficients.

2. one job optimization.R. 

This file includes codes to generate Figures 1-5 and Tables 1-3.

3. two job optimization.R. 

This file computes the optimal appointment time when two jobs are involved, 

4. two job fast optimization.R. 

The file calls median.dll for an efficient calculation for median, then repeat procedures in two-job optimization

5. median.dll. 

This is an executable c file (which has been complied, so could be called in a r code), a fast algorithm to obtain median

6. three job optimization.r. 

This file is similar to what we have done in two-job optimization. The only difference is that three jobs are involved.

7. three job fast optimization.r. 

The only difference from "three job optimization.r" is that this file employs a fast algorithm by using median.dll.

8. multiple job simulation.R. 

This file computes the optimal appointment time when more than three jobs are considered.


