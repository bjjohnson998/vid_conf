#Replication Repository for "Voter ID Laws and Confidence in Elections" by Brady Johnson

This respository has two directories, 'data' and 'scripts.'

In 'data,' you can find the analysis dataset, 'rescdf,' as well as a folder containing all of the raw datasets. The survey datasets, labelled 'post_year,' are exactly as downloaded from Pew (URLs provided in the paper's references). The file 'vid_laws,' is a hand-compiled list of states' VID laws and the years those laws went into effect (or failed/were defeated, in the case of a few states).

The 'scripts' directory contains the three .R files that make up my analysis. The first, 'data_transformations.R,' takes the raw datassets (from this repository) and recodes/merges them into the analysis dataset. The second, 'new_map.R,' is named so because it is my new(est) script for generating the map of the United States (figure 1). The final script, 'paper_script.R,' contains the code for all of the statistical analyses within the paper (table 1, figures 2--4).
