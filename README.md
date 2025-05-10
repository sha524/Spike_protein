# Research Project

# Analysis of SARS-CoV-2 spike protein mutations in the UK


## Introduction to the project ##
### Aims: ###

1.	Provide a complete overview of the general dataset trends
2.	Dimensionality reduction and cluster selection
3.	Clustering analysis
4.	Mutation analysis



### Purpose: ###
- Respository containing all the files used in the analysis
- This includes the R scripts, the figures generated, the data used and the respository

### Analysis: ###
Git was used alongside GitHub for version control and to maintain a remote respository

R was used for the majority of the analysis

git lfs was used for storage of large files

[PyMol](https://github.com/schrodinger/pymol-open-source) is a molecular graphics software used to produce protein structures in the report


**R packages used:**

Graphics packages:
- ggplot2
- cowplot
- ggfortify
- maps
- mapproj
- sf
- ggthemes
- rnaturalearthdata
- rnaturalearthhires
- viridis
- hrbrthemes

Data wrangling and manipulation:
- tidyverse
- dplyr
- stringr
- Matrix

Statistical packages:
- mgcv
- Rmisc
- pgirmess

Help installing additional packages:
- devtools
- usethis

Dimensionality reduction
- factoextra
- Rtsne
- umap
- uwot

A seed was set in RStudio for reproducibility, using set.seed(123)

## The files ##

**What does each file contain**❔

#### **data** ####
UK_seqs_msa_0522_spike_mutations.txt ➡️ Complete sequence data with unique identifier and mutation information

test_UK_seqs_msa_0522_spike_mutations.txt ➡️ First 100 sequences from the sequence data

#### **Plots and figures** ####

Practice ➡️ Any figures produced using the practice data

Real plots and figures ➡️ Figures for the report

#### **R scripts** ####

Practice scripts ➡️ Scripts for testing analysis before implementing on the actual data. I have kept a lot of the code, most of it is commented out, this was to allow an outside user to see how any problems were overcome

Scripts for actual data ➡️ Scripts where the analysis for the report was performed

#### **Report** ####

Word document used for the analysis

#### **Research Project** ####

R project used for general organisation and management

#### **Supplementary information** ####

In the Plots and Figures folder is a subfolder titled Supplementary figures - this folder contains the supplementary figures



## How to use this Research Compendium ##

- Open Research Project

- First open "Number of sequences per day.R" script from the practice scripts folder, this contains all the packages used for the analysis

- Each R script outlines what that script contains, provides information on the analysis performed and how the figures were generated

Recommended order to look at Scripts for actual data

Scripts that relate to figures have been shown

**Introduction to the dataset**

1. Number of sequences per day.R ➡️ Figure 1
2. Number of mutations over time.R ➡️ Figure 2
3. Number of unique mutations over time.R ➡️ Figure 3

**Clustering and the distribution of mutations**

1. pivot_wider.R
2. Clustering.R ➡️ Figure 4, Figure 5, Figure 6

**Mutation analysis**

1. Cluster.R
2. Number of mutations per sequence.R


Country of origin.R and Mapping.R contain the scripts that were going to form the basis of the geographical distribution analysis, however, this did not occur

## Clustering and dimensionality reduction parameters ##

- **K-means** ➡️ Parameters used: nstart of 10, centers equal to 1:10 and 3 once the number of clusters had been selected

- **PCA** ➡️ Default parameters used

- **t-SNE** ➡️ The algorithm was performed using a perplexity of 30, 500 iterations and the data was reduced to two dimensions to aid visualisation

- **UMAP** ➡️ The following default parameters were used: n_neighbors set to 15, min_dist of 0.1 and the n_components equal to 2



## Conclusion ##
This compendium should provide the relevant information on the scripts used for the analysis

## Acknowlegements ##
Creative Lab University of York, datacamp, riffomonas project and other online resources used for helping with R, git and any other applications used


