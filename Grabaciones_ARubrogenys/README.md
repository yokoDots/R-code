 🎧 Acoustic Parameter Analysis by Recording Device

This repository has R scripts that analyze and compare acoustic features from recordings of Ara rubrogenys. 
The recordings were made using different recording devices (Xeno-Canto, Audiomoth, Zoom H4NPro). 
The goal was to determine if recordings using different devices can be compared and used as part of 
the same analysis.

Scripts

1. `exploratory_plots.R`
-  Generates exploratory scatterplots showing relationships between variables (e.g., `SNR`, `Max_Freq`, `dTime`)
-  Visualizes group differences by device using color-coded plots
-  Helps identify trends and possible outliers
2. `pca_analysis.R`
- Performs Principal Component Analysis (PCA) on acoustic parameters
- Visualizes variance explained by each principal component
- Plots device clustering in reduced PC space
- Identifies which variables contribute most to the separation
3. `clustering_analysis.R`
-  Performs hierarchical clustering of:
  - Acoustic **variables** (to find redundancies)
  - **Devices/recordings** (to group similar samples)
-  Uses correlation-based distance and complete linkage
-  Visualizes clusters with dendrograms and colored branches
4. `anova_comparison.R`
-  Runs ANOVA for each acoustic variable across recording devices
-  Includes post-hoc t-tests vs. reference device (`XC`)
-  Outputs:
  - Residual diagnostics
  - Statistical summaries
  - Group-wise boxplots with p-values

