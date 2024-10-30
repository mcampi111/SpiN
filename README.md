# SpiN
Repository with data and code of the paper "Standardised Hearing Loss Risk Profiles with State-Space Models"

## **Abstract**

Hearing loss is a growing public health concern, affecting millions worldwide and contributing to impaired communication, social isolation, and reduced quality of life. As a hidden condition, hearing loss is typically diagnosed through behavioural tests like pure-tone audiometry, which often fail to capture the full extent of auditory deficits. Additional tests, such as speech-in-quiet and speech-in-noise, provide a more detailed understanding, yet they are underutilised due to limitations in equipment availability and time constraints in clinical settings. To address these diagnostic limitations, we propose an advanced method for profiling hearing loss dynamics by integrating audiogram and speech test data for risk assessment and prevalence estimation. Our approach utilises state-space models (SSMs), a mathematical framework that models hidden variables, accounting for the unobservable aspects of hearing loss and inferring a common trend across population segments. We develop a baseline state-space model relying solely on audiograms and an extended version that incorporates speech tests, thereby enhancing the ability to detect subtle differences in auditory function. A rigorous inference procedure, using the estimated likelihoods of these models, is employed to test for statistical differences in auditory profiles across population segments, including by degree of hearing loss, age and sex. This procedure thoroughly evaluates how specific population characteristics affect auditory performance. Our ultimate goal is to establish a national benchmark for clinical practice that supports personalised patient care, improving diagnosis, treatment planning, and monitoring of hearing impairment. By integrating audiometric and speech data, we aim to create standardised risk profiles that inform public health policy, guide targeted screenings, and facilitate timely interventions for individuals at risk of progressive hearing loss.

## Contributions of the paper
The paper has multiple contributions, at both methodological and applied level:
1.  **Model Formulation**. We formulate the CPBMT model as an innovative state-space structured approach designed to deliver national benchmark references for various risk profiles, including age-frequency, age-hearing-loss-degree-frequency, and age-sex-frequency categories. This model extends traditional state-space frameworks from their conventional time-domain applications to the frequency domain, providing a fresh perspective on hearing loss dynamics across different frequencies. By integrating both the relationship across threshold tests, such as pure-tone audiograms and speech tests and the risk profiling task, the CPBMT model offers a comprehensive solution. It simultaneously addresses the interplay between different assessment methods and the construction of detailed risk profiles, enhancing both the accuracy of hearing loss assessments and the understanding of underlying auditory mechanisms.
2.  **Hearing Loss Risk Profiling Definition**. Our approach estimates hearing loss incidence proportions across various age groups, sexes, and degrees of hearing loss severity using empirical quantiles derived from the entire population. By calculating the proportion of individuals within each age cohort who exceed data-driven dB empirical quantiles, we identify those with poorer hearing performance. Our model is designed to be extendable. By incorporating additional data sources, such as survey responses or other audiological tests, we can refine these profiles further. This comprehensive approach enhances our understanding of hearing loss and improves the precision of targeted interventions.   
3.  **Relationships between Pure-Tone Audiogram and Speech Tests**. The CPBMT model incorporates speech tests into the analysis, providing a comprehensive inference framework that accounts for both sensitivity to sound and clarity of speech perception. By exploring the relationship between speech-in-quiet and speech-in-noise scores and pure-tone thresholds, the model identifies the explanatory power of the speech tests, thereby enhancing the precision of tailored interventions and informing public health strategies.

## Motivations For this Study

This study introduces the Campi-Peters-Buhl-Morvan-Thai-Van (CPBMT) model, a specific type of state-space structured model. Similar to the widely used Lee-Carter model \cite{lee1992modeling} in demography for mortality rate forecasting and the Nelson-Siegel model in interest rate finance \cite{nelson1987parsimonious}, the CPBMT model leverages established methodologies to analyze hearing loss patterns and provides a population-level standardized reference benchmark of age-sex-frequency and age-hearing-loss-degree-frequency specific risk profiles. The formulation of the model involves constructing hearing loss proportions as well as speech-in-noise and speech-in-quiet proportions. Two models are considered: a baseline model based solely on the audiogram and an extended model incorporating the speech tests.


## Organization of the Repository
The repository is organized in the following folders:

```diff
+ 1) Rfiles
```
This folder contains the R files derived during the analysis.

```diff
+ 2) code 
```

1. **CPBMT_Data_Analysis_v2.R**. This file containes ....
2. **CPBMT_Model_Selection.R**. This file containes ....
3. **CPBMT_Part_Reg.R**. This file performs ....
4. **CPBMT_Plots.R**. This file containes .....
5. **CPBMT_Residuals_Mod_Assessment.R**. This file containes .....
6. **CPBMT_Stat_Tests.R**. This file containes .....
7. **utils.R**. This file containes .....


```diff
+ 3) figs 
```
All the figure implemented through R are provided in this folder. The code used to generate them is in the code folder. 

## Cite

If you use this code in your project, please cite:

@article{..,
  title={..},
  author={..},
  journal={..},
  year={..}
}


