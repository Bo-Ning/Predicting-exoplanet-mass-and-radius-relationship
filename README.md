# Predicting exoplanet mass and radius relationship

```diff
- For python users, we have a python package avaiable now. Please check https://github.com/shbhuk/mrexo! </font>
````

Folder1 : BenchmarkingWRF16-result

Contains R code, data, outputs to produce Figure 1 and Figure 2 in the paper. 

Folder2: Kepler-code

Contains R code, data, outputs to produce the rest figures in the paper. 
 
-- Kepler.R: the main code.

-- MR_Kepler_170605_noanalytTTV_noupplim.csv: Kepler data

-- k2-planets.csv: K2 data

-- MainFunctions: the folder contains the following three functions:

i) cross-validation.R: the function to choose optimal degree of Bernstein polynomials based on cross-validation method

ii) MLE.R: the function to estimate weights in the Bernstein polynomials function

iii) MRestimate.R: the function to obtain conditional and marginal densities from the joint M,R distribution

-- Result-Kepler: to produce figures presented in the paper, run the code: Kepler-plot.R 

Folder3: MR-predict

-- MRpredict.R: input a planet Radius (in original scale) (with or without measurement errors; posterior samples), return a predicted mass (in log10 scale) and it's quantiles.

--weights.mle.csv: estimated weights in the nonparametric model based on the dataset we used in the paper
