# NACHO-PostProcessing 
#This repo contains the post processing code for the "CSIA-AA", "WA Seal" and AK pinnipeds repo. This repo converts raw files from the UW GC-C-IRMS "NACHO" to drift corrected data using a 12 amino acid external standard. This code does not contain corrections for an internal standard. NLE was shown to have a lower mean precision compared to other amino acids (specifically phenyalalnine) and the internal standardization was not applied. Information regarding the NACHO protocol that pertains to this repo can be found in the "Doc" folder.

#Code requires datafiles to be "cleaned" prior to analysis. This means:
#1. peaks need to be identified. Currently, retention time gets later with each machine injection as the combustion chamber degrades. As a result a single RT for a single AA can not be applied across machine runs, particularly runs that occured weeks or months apart. Peak order could be used to code peak ID, however some amino acids occur at low concentrations, can be missing (due to low concentration), and for some tissues different amino acids are present. As a result manual peak ID allowed for the best quality control for these samples, although future users are encouraged to explore other options that suit their needs. 

#2. the data need to be "cleaned". Different columns are included in the IsoDat data output (this can be specified at the time of the machine run). The columns that should be included to run this code are: Analysis (injection number), ID (the ID specified for the sample by the user), Retention Time, d29N, d15N, AA (the identified peak IDs either manually or from some other code), and ID2 (any other notes or information specified, usually used to specify concetration or run methods).

#3. External standards should have a specified name that is consistent across all standard injections and should be specified in ID1 column. We use "12AA" as our identifier, any different identifier can be used but line 37 will have to be modified accordingly.

The "Drift Correction" code should be run for each data file for each machine run starting with the standard injected after the column oxidation and finishing with the final 2 standard injections after 6 samples. It is written to run separately for each individual file and will produce 1 processed file for each cleaned file. 

# Output files
#Output files include a mean and standard deviation for each of the 12 amino acids included in the standard, thus reducing the triplicate injection to one mean value for each individual sample. If users are not interested in the mean data and would like data for all injections do not run code after line 110 labelled "Consolidating triplicates". 

# Combining Metadata code
#this code is somewhat specific to museum mammal specimen data, however it can be used as a model for users interested in adding metadata to their files and compiling their output files into a single file.
