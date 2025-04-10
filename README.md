# Global Sociodemographic, Clinical, and Epidemiological Profiling of Patients with Mycetoma: A systematic review

**General disclaimer** This repository was created for use by CDC programs to collaborate on public health related projects in support of the [CDC mission](https://www.cdc.gov/about/cdc/#cdc_about_cio_mission-our-mission).  GitHub is not hosted by the CDC, but is a third party website used by CDC and its partners to share information and collaborate on software. CDC use of GitHub does not imply an endorsement of any one particular service, product, or enterprise. 

## Related documents

* [Open Practices](open_practices.md)
* [Rules of Behavior](rules_of_behavior.md)
* [Thanks and Acknowledgements](thanks.md)
* [Disclaimer](DISCLAIMER.md)
* [Contribution Notice](CONTRIBUTING.md)
* [Code of Conduct](code-of-conduct.md)

## Overview

This repository houses code for analyses for a paper exploring the global sociodemographic and clinical characteristics of patients affected by mycetoma and characteristing the geographic patterns of causative organisms of mycetoma. This manuscript is submitted to PloS Neglected Tropical Diseases as Salah et al, and is the product of a collaboration between the Mycetoma Research Center in Sudan and the United States Centers for Disease Control and Prevention. 

All analyses were done in R 4.0.0. Code written by Michelle Fearon Scales, [@mlfearon](https://github.com/mlfearon), utg5@cdc.gov, (CDC/NCEZID/DFWED/MDB).

### Source Data
 - All data are stored in the data folders:
   - [data_input](https://github.com/CDCgov/mycetoma-systematic-review-2024/tree/main/data_input): These files are the raw data collection for the systematic review from the selected manuscripts included in the study. Characteristics assessed include age, sex, educational level, residence, occupation, comorbidities, disease severity, involved body regions, disease outcomes, and treatment received. The number of patients with mycetoma for whom the causative organism was identified to species, genus, or aetiology were recorded for each country from the manuscripts.
   - [data_clean](https://github.com/CDCgov/mycetoma-systematic-review-2024/tree/main/data_clean): These are the cleaned data files after being run through the [data_cleaning.R](https://github.com/CDCgov/mycetoma-systematic-review-2024/blob/main/R/data_cleaning.R) script. This script does some light processing of the data to generate additional versions of the data that are used in the downstream calculations and figures.

### R code
 - All R code for this project is included in the R folder and should be used in the following order:
   - [data_cleaning.R](https://github.com/CDCgov/mycetoma-systematic-review-2024/blob/main/R/data_cleaning.R): Reads in the raw data from [data_input](https://github.com/CDCgov/mycetoma-systematic-review-2024/tree/main/data_input) and processes it to produce the cleaned data files in [data_clean](https://github.com/CDCgov/mycetoma-systematic-review-2024/tree/main/data_clean).
     - [functions.R](https://github.com/CDCgov/mycetoma-systematic-review-2024/blob/main/R/functions.R): This script is sourced at the beginning of the data_cleaning.R script to use functions developed to read in and simultaneously clean all imported data files.
   - [analysis_figures.R](https://github.com/CDCgov/mycetoma-systematic-review-2024/blob/main/R/analysis_figures.R): Reads in the cleaned data files from [data_clean](https://github.com/CDCgov/mycetoma-systematic-review-2024/tree/main/data_clean) and calculates all the summary statistics that are save in data_summary such as the mean percent and range of mycetoma patients reported with each characteristic. This script also produces all the figures showing boxplots and raw data points for the patients reported with each characteristic.
   - [quality_score_traffic_light_figure.R](https://github.com/CDCgov/mycetoma-systematic-review-2024/blob/main/R/quality_score_traffic_light_fig.R): This script sets up the `Quality_scores_clean.csv` data file to produce Figure 2 in the manuscript that shows the proportion of studies that have each quality score.
   - [causative_org_figures.R](https://github.com/CDCgov/mycetoma-systematic-review-2024/blob/main/R/causative_org_figures.R): This script produces two global maps of the causative organisms for mycetoma using the `Causative_Organisms_Taxonomy_clean.csv` data file. The first map catagorizes causative organisms by aetiology (i.e., fungal vs bacterial origin), and the second categorizes causative organisms by genus (Figures 5 and 6, respectively in the manuscript).

### Outputs
 - The output from the scripts are saved in two additional data folders: 
   - [data_summary](https://github.com/CDCgov/mycetoma-systematic-review-2024/tree/main/data_summary): These are the results from the summary statistics for the percent of patients reported with each sociodemographic and clinical characteristic, including the mean, standard deviation, range and the total number of studies that reported that characteristic. These results are shown in a table in the manuscript.
  
## Public Domain Standard Notice
This repository constitutes a work of the United States Government and is not
subject to domestic copyright protection under 17 USC § 105. This repository is in
the public domain within the United States, and copyright and related rights in
the work worldwide are waived through the [CC0 1.0 Universal public domain dedication](https://creativecommons.org/publicdomain/zero/1.0/).
All contributions to this repository will be released under the CC0 dedication. By
submitting a pull request you are agreeing to comply with this waiver of
copyright interest.

## License Standard Notice
The repository utilizes code licensed under the terms of the Apache Software
License and therefore is licensed under ASL v2 or later.

This source code in this repository is free: you can redistribute it and/or modify it under
the terms of the Apache Software License version 2, or (at your option) any
later version.

This source code in this repository is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the Apache Software License for more details.

You should have received a copy of the Apache Software License along with this
program. If not, see http://www.apache.org/licenses/LICENSE-2.0.html

The source code forked from other open source projects will inherit its license.

## Privacy Standard Notice
This repository contains only non-sensitive, publicly available data and
information. All material and community participation is covered by the
[Disclaimer](DISCLAIMER.md)
and [Code of Conduct](code-of-conduct.md).
For more information about CDC's privacy policy, please visit [http://www.cdc.gov/other/privacy.html](https://www.cdc.gov/other/privacy.html).

## Contributing Standard Notice
Anyone is encouraged to contribute to the repository by [forking](https://help.github.com/articles/fork-a-repo)
and submitting a pull request. (If you are new to GitHub, you might start with a
[basic tutorial](https://help.github.com/articles/set-up-git).) By contributing
to this project, you grant a world-wide, royalty-free, perpetual, irrevocable,
non-exclusive, transferable license to all users under the terms of the
[Apache Software License v2](http://www.apache.org/licenses/LICENSE-2.0.html) or
later.

All comments, messages, pull requests, and other submissions received through
CDC including this GitHub page may be subject to applicable federal law, including but not limited to the Federal Records Act, and may be archived. Learn more at [http://www.cdc.gov/other/privacy.html](http://www.cdc.gov/other/privacy.html).

## Records Management Standard Notice
This repository is not a source of government records, but is a copy to increase
collaboration and collaborative potential. All government records will be
published through the [CDC web site](http://www.cdc.gov).

## Additional Standard Notices
Please refer to [CDC's Template Repository](https://github.com/CDCgov/template) for more information about [contributing to this repository](https://github.com/CDCgov/template/blob/main/CONTRIBUTING.md), [public domain notices and disclaimers](https://github.com/CDCgov/template/blob/main/DISCLAIMER.md), and [code of conduct](https://github.com/CDCgov/template/blob/main/code-of-conduct.md).
