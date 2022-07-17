# digit_wwtp
digital workflow for evaluation of WWTP data for design and process optimisation

The following files are included - you should start them in the following order:

1. data_preparation.R - import and preparation of WWTP routine data on daily basis
2. data_check.Rmd - documentation of different plausibility checks
3. eval_design_parameters_A198.Rmd - calculation and documentation of design parameters based on ATV-DVWK A 198
4. evaluation_parameter_A198.R - parameters for eval_design_parameters_A198.Rmd
5. calc_design_A131.R- calculation of WWTP design based on DWA A 131
6. design_parameters_A131.R - parameters for calc_design_A131.R
7. eval_design_A131.Rmd - documentation of WWTP design


This is a first draft of the workflow. Calculations may contain errors. 
The comments are in english, most of the produced text in the word documents is in german due to actual use of german design guidelines of DWA. It should be self explained. 
All RMarkdown scripts can be used as examples and expanded by own interests.

There is also some example data for test of the scripts.

Markus Ahnert / Stefan Hurzlmeier