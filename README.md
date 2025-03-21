# Research Project

# Analysis of SARS-CoV-2 spike protein mutations in the UK


## Introduction to the project
### Aims:

- Provide a complete overview of the general dataset trends
- Investigate the clustering and distribution of SARS-CoV-2 spike protein mutations in the UK
- Identify and characterise common spike mutations and non vs syn mutations
- Analyse their potential effects on viral infectivity, linking to the receptor binding domain/receptor binding motif


### Purpose:
- Respository containing all the files used in the analysis
- This includes the R scripts, the figures generated, the data used and the respository

### Analysis:
Git was used alongside GitHub for version control and to maintain a remote respository

R was used for the majority of the analysis

git lfs was used for storage of large files

[Moorhen](https://github.com/moorhen-coot/Moorhen) is a molecular graphics software used to produce protein structures in the report


R packages used:
- ggplot2
- Rmisc
- tidyverse
- viridis
- hrbrthemes
- pgirmess
- dplyr
- stringr
- devtools
- usethis
- cowplot
- mgcv




## The files

**What does each file contain**❔

#### **data**
UK_seqs_msa_0522_spike_mutations.txt ➡️ Complete sequence data with unique identifier and mutation information

test_UK_seqs_msa_0522_spike_mutations.txt ➡️ First 100 sequences from the sequence data

#### **Plots and figures**

Practice ➡️ Any figures produced using the practice data

Real plots and figures ➡️ Figures for the report

#### **R scripts**

Practice scripts ➡️ Scripts for testing analysis before implementing on the actual data. I have kept a lot of the code, most of it is commented out, this was to allow an outside user to see how any problems were overcome

Scripts for actual data ➡️ Scripts where the analysis for the report was performed

Data structure names ➡️ Names of the important data structures alongwith a description of what the structure contains

#### **Report**

Word document used for the analysis

#### **Research Project**

R project used for general organisation and management


## How to use this Research Compendium

- Open Research Project

- First open "Number of sequences per day.R" script from the practice scripts folder, this contains all the packages used for the analysis

- Each R script outlines what that script contains, provides information on the analysis performed and how the figures were generated

Recommended order to look at Scripts for actual data

**Introduction to the dataset**

1. Number of sequences per day.R
2. Number of mutations over time.R
3. Number of unique mutations over time.R

**Clustering and the distribution of mutations**

1. pivot_wider.R
2. Clustering.R

**Mutation analysis**



