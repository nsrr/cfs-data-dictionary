## 0.5.0

- Remove EEG spectral summary variables
- The CSV datasets generated from a SAS export is located here:
  - `\\rfawin\bwh-sleepepi-cfs\nsrr-prep\_releases\0.5.0\`

## 0.4.1 (July 19, 2018)

- Remove form references to Child Health Questionnaire (copyrighted)

### Changes
- **Gem Changes**
  - Update to ruby 2.5.1
  - Update to spout 0.14.0

## 0.4.0 (October 24, 2017)

### Changes
- Change primary identifier variable name, `obf_pptid` -> `nsrrid`
- Update display names and calculations for ICSD AHI variables
- Remove strange symbols from variable display names
- The CSV datasets generated from a SAS export is located here:
  - `\\rfawin\bwh-sleepepi-cfs\nsrr-prep\_releases\0.4.0\`
- **Gem Changes**
  - Updated to spout 0.12.1

## 0.3.0 (April 22, 2016)

### Changes
- Update self-reported sleep duration variables (e.g. `dayslp_dur_mn`)
- Added ICSD3 AHI variables (e.g. `ahi_a0h3`)
- Fixed misspellings and typos in variable labels
- Remove `whtkg` variable (duplicate of `wtkg`)
- The CSV datasets generated from a SAS export is located here:
  - `\\rfa01\bwh-sleepepi-home\projects\cohorts\Family\nsrr-prep\_releases\0.3.0\`
    - `cfs-visit5-dataset-0.3.0.csv`
    - `cfs-visit5-eeg-band-summary-dataset-0.3.0.csv`
    - `cfs-visit5-eeg-spectral-summary-dataset-0.3.0.csv`

## 0.2.1 (January 19, 2016)

### Changes
- Fixed domain name for noyesdk_2
- **Gem Changes**
  - Updated to ruby 2.3.0
  - Updated to spout 0.11.0

## 0.2.0 (October 13, 2015)

### Changes
- Added two new EEG spectral analysis datasets
- Fixed values that were being nulled for `ethnicity` variable
- Corrected filenames for many of the linked forms
- Renamed core dataset to `visit5` (from `rectype5`)
- Remove `houshold` variable that was improperly referenced in SAS script
- The CSV datasets generated from a SAS export is located here:
  - `\\rfa01\bwh-sleepepi-home\projects\cohorts\Family\nsrr-prep\_releases\0.2.0\`
    - `cfs-visit5-dataset-0.2.0.csv`
    - `cfs-visit5-eeg-spectral-summary-0.2.0.csv`
    - `cfs-visit5-eeg-band-summary-0.2.0.csv`

## 0.1.1

### Changes
- The SF-36 questionnaire has been updated to ensure that variable properly map to it on sleepdata.org
- **Gem Changes**
  - Updated to spout 0.10.1
  - Use of Ruby 2.1.5 is now recommended

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
