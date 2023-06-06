# hRELSA
## A translational approach to quantify a patient’s disease severity in real-time

hRELSA (Relative Severity Assessment in Humans) is a revised and updated version of the RELSA metric, specifically translated to humans. It is designed to offer a more objective, flexible, and real-time assessment of illness severity within the intensive care unit (ICU) setting. By incorporating multiple clinical input variables, hRELSA aims to minimize information loss, facilitate the comparison of individuals and groups, and generate disease-specific outcomes. This approach effectively addresses the existing gap in ICU patient monitoring.

Repository of the RELSA package: https://github.com/mytalbot/RELSA

## Features and Benefits

- **Objective and Real-time Assessment:** hRELSA provides an objective and real-time evaluation of the severity of illness in ICU patients. By considering various clinical input variables, it captures a comprehensive picture of the patient's condition.

- **Flexibility and Adaptability:** The hRELSA metric is flexible and adaptable to different disease conditions and patient populations. It accommodates the dynamic nature of ICU patients and can be customized based on specific clinical requirements.

- **Minimizing Information Loss:** By incorporating multiple clinical input variables, hRELSA minimizes the risk of information loss. It considers a broad range of factors that contribute to the overall severity assessment, leading to a more accurate representation of the patient's condition.

- **Comparability of Individuals and Groups:** hRELSA enables the comparison of severity assessments between individuals and groups within the ICU. This feature is particularly useful for benchmarking and monitoring patient outcomes over time.

- **Disease-Specific Outcomes:** The hRELSA approach generates disease-specific outcomes, allowing for a more tailored assessment of severity based on the specific condition being treated. This helps clinicians make more informed decisions regarding patient management and treatment strategies.

## Usage

To use hRELSA in your ICU setting, follow these steps:

1. **Data Collection:** Gather the necessary clinical input variables for each patient. These variables may include vital signs, laboratory results, physiological scores, and other relevant clinical measurements.

2. **Data Input:** Input the collected data into the hRELSA algorithm. Ensure that the data is formatted correctly and all required variables are included.

3. **Severity Assessment:** Run the hRELSA algorithm to obtain the severity assessment for each patient. The output will provide an objective measure of illness severity based on the input variables.

4. **Interpretation and Monitoring:** Interpret the hRELSA scores in the context of each patient's condition. Monitor the scores over time to track the patient's progress and response to treatment.

## Installation

In this repository, hRELSA is formed into a R package. You can install it as package and use it as shown in the hRELSA application repository: https://github.com/phohland/hRELSA_application

## Included Functions

- **hrelsa_adaptive_baselines.R** For the use of adaptive reference adjusting (e.g. with the patient's age)
- **hrelsa_analysis.R** For a quick analysis on e.g. which patient has the highest severity
- **hrelsa_baselines.R** For baseline calculation using no adaptive reference method
- **hrelsa_days.R** For calculation of time points with given dates
- **hrelsa_final.R** For creating severity values for every entry
- **hrelsa_format.R** For formatting of the raw data
- **hrelsa_norm.R** For normalizing of the formatted data (not for the adaptive reference method)
- **hrelsa.R** For calculation of the final severity weights and values
