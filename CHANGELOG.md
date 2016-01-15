## 0.1.1

- Backporting updates to be compatible with Spout 0.11.0.rc

## 0.1.0 (October 10, 2014)

### Changes
- Initial import from `family3dd.xls`
- All non-calculated variable are associated with forms
- Redundant identifier variables have been removed from the data dictionary
- Domains have been created for all variables originally marked as `type: choices`
- Fixed several outliers, negative and implausible values
- Variables have now been associated with forms, where appropriate
- Demographics variables and key subscales have been marked as 'commonly used'
- Missing values have been stripped from the dataset
- Family medical history variables have been removed from this release, pending a more in depth cleaning
- PHI and identifiable variables have either been obfuscated or removed from the dataset
- Variables sourced from the `baseline_lab_questionnaire` form have been updated to match exact questionnaire wording
- The CSV datasets generated from a SAS export is located here:
  - `\\rfa01\bwh-sleepepi-home\projects\cohorts\Family\nsrr-prep\_releases\0.1.0\`
    - `cfs-rectype5-dataset-0.1.0.csv`
- **Gem Changes**
  - Updated to spout 0.9.0
  - Use of Ruby 2.1.3 is now recommended
