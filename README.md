## Coupling paraecology and hunter GPS self-follows to quantify village bushmeat hunting dynamics across the landscape scale

<br>

This repository contains example code and data for the paper *Coupling paraecology and hunter GPS self-follows to quantify village bushmeat hunting dynamics across the landscape scale*: [https://doi.org/10.1111/aje.12956](https://doi.org/10.1111/aje.12956). Analyses are reproducible, but only 4 sessions of paraecologist data collection are included (of 14 used in the paper). **Results produced here are thus not the final results in the paper, but an example product for illustrative purposes.** Anonymised full data will be available under conditions by the end of 2022 in the WILDMEAT database: [https://www.wildmeat.org](https://www.wildmeat.org.). Shiny participatory apps in English and French, and forthcoming apps and updates are at: [https://www.nadagabon.org](https://www.nadagabon.org.) 

To run the code, download the entire repository and run scripts within the R project. The 3 scripts are as follows:

* **clean_transects.R**: this runs the diagnostic tool that corrects typos and errors in raw entered paraecologist bushmeat transect data and produces a report of its quality (see section 2.2 of the paper). If you run the script, it creates .csv products in *./outputs/cleaned/*. Similar logic not included in this repository was used cleaning the hunter self-follow data.
* **estimate_offtake.R**: this runs the algorithm used to estimate observed offtake described in section 2.3 of the paper and Appendix S3. If you run the script, it creates a .csv in *./outputs/*. 
* **summarize_offtake.R**: this runs the analyses integrating transect and self-follow data to estimate annual offtake described in section 2.3 of the paper. It further summarizes data and produces figures 4--6. (Figures 1 & 2 are excluded because their creation requires explicit spatial data; Figure 3 is a photograph). If you run the script, it creates .csv data summaries in *./outputs/* and .pdf graphics in *./outputs/figures/*.

To restore the project library locally onto your machine before running scripts (in case of issues using different package versions), begin by running the following command in the console: `renv::restore()`









