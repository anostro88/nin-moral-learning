# nin-moral-learning

## Model Fitting

Data from two studies are present in the current repository: fMRI study contains behavioral data from 27 subjects who did the task during fMRI recording with 10 trials for each of the six block. Probe study contains behavioral data from 20 subjects who performed the task with the first 10 trials for each block as the fMRI study but with other 10 following trials in which one quantity was removed at the 11th trial. 

Both studies are structured with a main folder containing:

1) `data`: here there is the input data which will be fed first read from the script fit_subjects_Models.R and then from here, fed into the Stan models. Both studies have for each participant a row with the choices performed and the outcomes realized from that choices, seperated for both money and shock. In addition, for the probe study, there are two files named filter_* which inform the quantity removed from the 11th to the 20th trial of each block. Coding of choices, outcomes, filters can be found in the coding.txt present in data folder

2) `FitSubjects_Models.R`: is the first script to use in R which load all the necessary info from data folder and sends it to STAN.

3) STAN models: M0, M1wf, M2wfOut and M2wfDec. The versions for the probe and for the fMRI studies are NOT interchangeable as they are specific for 10 trials (fMRI) and for the use of the filters on the 20 trials (probe).

4) `Check_Models.R`: load the fitted models as large Stan fit and trasforms it in easier dataframe to be analyzed. Plot and Save parameters and posterior distributions (hyperparameters and individual parameters). Compute fit metrices like ROC AUC and LOOIC. 
