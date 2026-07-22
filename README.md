# 🦟 DengueScope: Identification of Transcriptomic Biomarkers for Dengue Infection

## 🩸 What was done?

- Downloaded 73 RNA-Seq dengue samples from [ENA](https://www.ebi.ac.uk/ena/browser/view/PRJNA1279769).
- Conducted read preprocessing and trimming using **fastp**.
- Performed raw data quality control using **FastQC**.
- Aligned reads to the reference genome using **STAR** aligner.
- Quantified gene expression using **featureCounts**.
- Performed differential gene expression analysis using **DESeq2**.
- Built a web application using **Shiny R** to explore the results [(App Link)](https://varshas.shinyapps.io/Denguscope/).
