### lfproQC - Label-free proteomics data Quality Control
This shiny app is developed based on the functionalities of the lfproQC R package.
It can be accessed through the URLs:
http://omics.icar.gov.in/lfproQC or
https://dabiniasri.shinyapps.io/lfproQC

## Contents

1.  [Uploading the Proteomics Expression Dataset and Group
    Information](#uploading-the-proteomics-expression-dataset-and-group-information)
2.  [Getting the Results](#getting-the-results)
3.  [Exploratory Plots Visualization](#exploratory-plots-visualization)
4.  [Differential Expression
    Analysis](#differential-expression-analysis)

### 1. Uploading the Proteomics Expression Dataset and Group Information

To upload the data, navigate to the **‘Data Upload’** tab. The user can
upload their dataset either from local storage or use the provided
example datasets. This Shiny application supports both peptide-based and
protein-based proteomics datasets. Accepted file formats include
**‘.xlsx’**, **‘.xls’**, and **‘.csv’**.

-   **Peptide-based datasets**: The first column should contain peptide
    information, and the second column should contain protein
    information.
-   **Protein-based datasets**: The first column should contain protein
    information, while the remaining columns should contain protein
    expression values.

Below are examples of peptide-based and protein-based proteomics
datasets:

<img src="/www/peptide.png" alt="Example peptide-based proteomics expression dataset">
<p>
<strong>Figure 1:</strong> Example peptide-based proteomics expression
dataset
</p>

<img src="/www/data.png" alt="Example protein-based proteomics expression dataset">
<p>
<strong>Figure 2:</strong> Example protein-based proteomics expression
dataset
</p>

Users must also upload sample group information corresponding to the
proteomics expression dataset. An example of sample group information
for the protein-based dataset is shown below:

<img src="/www/group.png" alt="Sample group information of the example protein data">
<p>
<strong>Figure 3:</strong> Sample group information for the example
protein data
</p>

Specify the data type (Peptide or Protein) and select the appropriate
peptide aggregation method (sum, mean, or median) if peptide data is
used. This will generate the aggregated or rollup protein dataset,
visible after submitting both datasets in the **‘Rollup Protein Data’**
tab.

<img src="/www/rollup1.png" alt="Uploading the peptide dataset">
<p>
<strong>Figure 4:</strong> Uploading the peptide dataset
</p>

The user can also download example datasets and group information from
the provided buttons within the app. After uploading the datasets, click
the submit button:

<img src="/www/example1.png" alt="Options for example dataset and data group">
<p>
<strong>Figure 5:</strong> Options for example dataset and data group
</p>

### 2. Getting the Results

After uploading the datasets, click the **‘Results’** tab. The main
panel will display the best combinations for the dataset based on three
statistical evaluation measures: **PCV**, **PEV**, and **PMAD**. Results
for all three measures, along with the **NRMSE** values, will be shown.
Click the eye symbol to view these results:

<img src="/www/best_comb1.png" alt="Best combinations for the dataset">
<p>
<strong>Figure 6:</strong> Best combinations for the dataset
</p>

Select either the most frequently occurring combination or any one of
the three results. All nine combinations of normalized and imputed
datasets will be displayed in the main panel, with an option to download
the datasets in **.csv** format. The user can also download only the
normalized datasets based on specific normalization methods:

<img src="/www/download_data1.png" alt="Download option for the normalized and imputed dataset">
<p>
<strong>Figure 7:</strong> Download option for the normalized and
imputed dataset
</p>

### 3. Exploratory Plots Visualization

In the **‘Exploratory Plots’** tab, users can visualize the best
combination of normalization and imputation methods through various
exploratory plots, including box plots, density plots, QQ plots, MDS
plots, and correlation heatmaps:

<img src="/www/boxplot1.png" alt="Various exploratory plots for visualization">
<p>
<strong>Figure 8:</strong> Various exploratory plots for visualization
</p>

These plots help assess the normality of the dataset and understand the
relationships between sample groups.

### 4. Differential Expression Analysis

In the **‘Differential Expression Analysis’** tab, users can perform
differential expression analysis between any two sample groups.
Available plots include the MA plot and the volcano plot:

<img src="/www/maplot1.png" alt="MA plot pairwise DE analysis">
<p>
<strong>Figure 9:</strong> MA plot pairwise DE analysis
</p>

<img src="/www/volcano_plot1.png" alt="Volcano plot pairwise DE analysis">
<p>
<strong>Figure 10:</strong> Volcano plot pairwise DE analysis
</p>

Users can adjust log-fold change values and p-values to identify
up-regulated and down-regulated proteins by selecting the checkbox
labeled **‘Override Cut-off Limits and P-value’**. Differentially
expressed protein results can be downloaded in Excel format:

<img src="/www/volcano_deg1.png" alt="Volcano plot DE analysis with user-defined cut-off limits">
<p>
<strong>Figure 11:</strong> Volcano plot DE analysis with user-defined
cut-off limits
</p>

<img src="/www/deg_result1.png" alt="Download option for the result of DE proteins">
<p>
<strong>Figure 12:</strong> Download option for the result of DE
proteins
</p>
