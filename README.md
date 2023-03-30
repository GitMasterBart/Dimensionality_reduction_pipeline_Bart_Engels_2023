# Dementia reduction
<a name="contact"></a>
*Contact:*
<br>
*Author: Bart Engels*
<br>
*Website: www.bartengels.eu* **Currently out of use**
<br>
*Email: b.engels@st.hanze.nl*
<br>
*Version: V3.0*

## Table of Contents

1. [Abstract](#abstract)
2. [Setup](#setup)
   1. [Setup path to root](#setup-path)
   2. [Setup files](#setup-files)
3. [References](#bibliografie)

<a name="abstract"></a>
## Abstract 

Brain disorders, including both neurodegenerative diseases (ND) and psychiatric conditions, 
have a substantial impact on the quality of life for millions of people worldwide 
[^1][^2]. 
However, diagnosing these disorders can be challenging due to 
their heterogeneous nature and shared clinical manifestations, 
resulting in a misdiagnosis rate of ± 33% [^3]. 
To address this, we implemented unsupervised machine learning techniques 
on clinical disease trajectories from the Netherlands Neurogenetics 
Database, which is built upon the extensive clinical neuropathological 
summaries from the Netherlands Brain Bank. Through the analysis of temporal 
patterns of signs and symptoms, we aimed to differentiate clinical disease 
trajectories and discover subtypes for certain brain disorders. This approach has demonstrated that 
clinical patterns can differentiate diagnoses to some extent, 
and certain clusters can be separated based on their signs/symptoms. 
The analysis that incorporates the temporal aspect provides a more 
interpretable temporal pattern for certain clusters. 
This combination of unsupervised machine learning techniques 
has the potential to identify temporal clinical disease trajectories 
and explore potential differences between and within NDD/psychiatric conditions

<a name="setup"></a>
## Setup
<a name="setup-path"></a>
### Setup path to root

The first you need to setup the correct file paths. 
To do this you need to open both `file_names` in the 
folders `py` and `r`and replace `[PATH TO ROOT]` with the path to your root-folder.

**R-file named: file_names.r**
```r
SYMPTOMFILE <- "/Input/meta_data/flattened_data_thumbRules.csv"
TEMPORAL_FILE <- "/Input/temporal_data/temporaldata_birth_1year_al.csv"
ROOT_FOLDER <- "[PATH TO ROOT]"
SUMMED_FILE <- "/Input/flattened_data/flattened_data_thumbRules.cs
```

**Python-file names: file_names.py**
```python
PICKLE_FILE = [PATH TO ROOT] +"/Dimensionality_reduction_pipeline_Bart_Engels_2023/Input/pickle_file/ALL_clinical_trajectories_dictionary_rules_of_thumb_yearly_2023-01-31.pkl"
OUTPUTFILE_TEMPORAL_DATA = [PATH TO ROOT] + "/Dimensionality_reduction_pipeline_Bart_Engels_2023/Output/temporal_data/"
OUTPUTFILE_FLATTEND_DATA = [PATH TO ROOT] + "/Dimensionality_reduction_pipeline_Bart_Engels_2023/Output/flattened_data/"
GENERAL_INFO = [PATH TO ROOT] +"/Dimensionality_reduction_pipeline_Bart_Engels_2023/Input/meta_data/General_information_13-07-2022_FULL.csv"
ATTRIBUTE_GROUPING = ("/Dimensionality_reduction_pipeline_Bart_Engels_2023/Input/meta_data/Clinical History - attributes grouping in categories - metadata.xlsx")
```


<a name="setup-files"></a>
### Setup files 

When running this workflow there are two options using one of the included files or creat 
one with the python class `PickleToTempBirth` stored in `pickle_to_matrix.py`.

*Included files:*
* `flattened_data_thumbRules.csv`
* `temporal_death_5years_cognitive_general_signs_symptoms.csv`
* `temporaldata_birth_20year_psy.csv`

Running this class is as easy as brushing your teeth; first you need to specify the signs/symptoms domains (`-d`) that you 
want to include. Then you need to decide the bucket size (`-b`), this represent the year buckets for which the 
signs/symptoms occur. Following you need declare the filename (`-f`). And at last you need to 
specify if the point of normalization is going to be 
birth or death. Upon pressing the "Enter" key, two loading bars will be displayed to indicate the progress of parsing the pickle file.

**Example of usages:**
```pyton 
python pickle_to_tempdata.py -d Psychiatric General Cognitive Sensory Psychiatric -b 1 -f temporaldata_birth_1year_al.csv -n birth
```

Now that you have set up the correct files you can follow the Notebooks, they can be found in `scripts/r/Notebooks`.
<br>The following order would be recommended: 

```r
Dimensionality reduction overview (step1, PCA).rmd -> Dimenesionaliryt reduction overview (step2, FA).rmd -> Dimensionaliry reduction overview (step3, K-means).rmd -> Dimensionality reduction overview (step4, visualistaion).rmd

```
**Succes!** If it is not clear [Contact](#contact) me!

<details>
  <summary><b>Extra and tips:</b></summary>
  <p>If you want you can remove all the <code>.placeholders</code> they are only there to create a directory hierarchy.</p>
</details>


<a name="bibliografie"></a>
*References:*

[^1]: Ahmed, R. M., Devenney, E. M., Irish, M., Ittner, A., Naismith, S., Ittner, L. M., Rohrer, J. D., Halliday, G. M., Eisen, A., Hodges, J. R., & Kiernan, M. C. (2016). Neuronal network disintegration: Common pathways linking neurodegenerative diseases. In Journal of Neurology, Neurosurgery and Psychiatry (Vol. 87, Issue 11). https://doi.org/10.1136/jnnp-2014-308350
[^2]: Armstrong, R. A., Lantos, P. L., & Cairns, N. J. (2005). Overlap between neurodegenerative disorders. In Neuropathology (Vol. 25, Issue 2). https://doi.org/10.1111/j.1440-1789.2005.00605.x
[^3]: Selvackadunco, S., Langford, K., Shah, Z., Hurley, S., Bodi, I., King, A., Aarsland, D., Troakes, C., & Al-Sarraj, S. (2019). Comparison of clinical and neuropathological diagnoses of neurodegenerative diseases in two centres from the Brains for Dementia Research (BDR) cohort. Journal of Neural Transmission, 126(3). https://doi.org/10.1007/s00702-018-01967-w
[^4]: 