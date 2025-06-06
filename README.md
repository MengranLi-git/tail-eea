# Reproducibility Information

This repository provides the necessary materials to reproduce the results presented in the poster.

## 📌 Objectives
This project focuses on extreme event attribution using advanced statistical models, including multivariate peaks-over-thresholds and causal inference frameworks.

## 📁 Contents
- `scripts/`: Contains R or Python code used in the analysis.
- `data/`: Contains a simulated dataset structured to match the real data used in the poster.
- `references_with_doi.html`: Full list of references cited in the poster.

## 📂 Data Access
The original dataset is not publicly available due to licensing restrictions. However, we provide a simulated version in `data/simulated_data.csv` for demonstration purposes. This dataset mimics the structure of the original data and allows the main pipeline to be executed.

## 💻 Reproduction Steps
1. Clone the repository or download the files.
2. Navigate to the `scripts/` directory.
3. Run the main analysis script (e.g., `main_analysis.R` or `main_analysis.py`).
4. Outputs (plots, results) will be saved in the `output/` folder.

## 🔧 Environment
- R version 4.3 / Python 3.10
- Required packages: `ggplot2`, `evd`, `scipy`, `matplotlib`, `pandas` etc.
- Set random seed = 123 for reproducibility.

## 🔗 Citation
If you use any part of this material, please cite the poster and the relevant papers in `references_with_doi.html`.

