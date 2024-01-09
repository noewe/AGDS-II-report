# About this repository
This repository contains the exercises and reports I made during the course 
Applied Geodata Science II, at the University of Bern, 
during the fall semester 2023.

# Credits 
Credits for the course material and teaching go to Prof. Dr. Benjamin Stocker 
and Dr. Koen Hufkens from the research group for Geocomputation and Earth 
bservation at the University of Bern. https://github.com/geco-bern

# Structure
This repository is structured based on a GECO R project. Find the template at 
https://github.com/geco-bern/R_proj_template

The structure of the template follows the structure of an R package without
actually being one.

- Splits the dynamic reporting from academic writing (`vignettes` vs. `manuscript`)
- Splits pre-processing of data from working / included data (`data-raw` vs. `data`)
- Splits R code from other scripts (bash / python in `src`)
- Splits R functions from R analysis scripts (`R` vs `analysis`)