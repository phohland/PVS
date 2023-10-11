# Patient Vital Status (PVS)

PVS is a revised and updated version of the RELSA metric, specifically translated to humans. It is designed to offer a more objective, flexible, and real-time assessment of a patient vital state within the intensive care unit (ICU) setting. By incorporating multiple and customizable clinical input variables, PVS aims to minimize information loss, facilitate the comparison of individuals and groups, and generate disease-specific outcomes.

Repository of the RELSA package: https://github.com/mytalbot/RELSA

The application of the PVS in the first application study: https://github.com/phohland/PVS_application

## Included Functions

- **pvs_adaptive_baselines.R** For the use of adaptive reference adjusting (e.g. with the patient's age)
- **pvs_analysis.R** For a quick analysis on e.g. which patient has the highest severity
- **pvs_baselines.R** For baseline calculation using no adaptive reference method
- **pvs_days.R** For calculation of time points with given dates
- **pvs_final.R** For creating severity values for every entry
- **pvs_format.R** For formatting of the raw data
- **pvs_norm.R** For normalizing of the formatted data (not for the adaptive reference method)
- **pvs.R** For calculation of the final severity weights and values
