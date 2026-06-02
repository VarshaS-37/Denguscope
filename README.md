# 🦟 DengueScope: RNA-Seq Differential Expression Analysis and Machine Learning-Based Biomarker Discovery for Dengue Infection

## 🩸 What was done?

- Downloaded 73 RNA-Seq dengue samples from [ENA](https://www.ebi.ac.uk/ena/browser/view/PRJNA1279769).
- Performed raw data quality control using **FastQC**.
- Conducted read preprocessing and trimming using **fastp**.
- Aligned reads to the reference genome using **STAR** aligner.
- Quantified gene expression using **featureCounts**.
- Performed differential gene expression analysis using **DESeq2**.
- Applied **DaMiRSeq** for feature selection and biomarker discovery.
- Built a web application using **Shiny R** to explore the results [(Link)](https://varshas.shinyapps.io/Denguscope/).
