# Center for Academic Innovation - Tandem

## Overview
The repo showcases my work at Center for Academic Innovation. The scripts mainly analyzed the differences between the beginning of term (BOT) survey and the end of term (EOT) survey of two UM courses. Data were collected from students using Tandem as their assistive learning tool in class. More information about Tandem, please refer to the article [*"The 3 Things to Know about Tandem"*](https://ai.umich.edu/blog/the-3-things-to-know-about-tandem/).

## Navigation

Businesses and Leaders: The Positive Differences (BA200) Course:
- [F19_EDA.Rmd](https://github.com/mclu/CAI_Tandem/blob/master/BA200/F19_EDA.Rmd), [F19_EDA.html](https://github.com/mclu/CAI_Tandem/blob/master/BA200/F19_EDA.html): In this exploratory data analysis, I looked into students responses of two surveys in different sections and demographics, then determined if there is any significant changes over the course of the term.

- [F19_clustering.Rmd](https://github.com/mclu/CAI_Tandem/blob/master/BA200/F19_clustering.Rmd), [F19_clustering.html](https://github.com/mclu/CAI_Tandem/blob/master/BA200/F19_clustering.html): We explored student behavior in Fall 2019 class by analyzing the BOT survey. The K-means clustering technique was implemented to understand underlying student types and how those types relate to outcomes. The result showed that students can divide into four groups - self-starter, procrastinator, introvert, and extrovert. The analysis helped behavioral scientists better understand student behavior and facilitate their development of tailored messages for Tandem.

- [threshold_shiny.R](https://github.com/mclu/CAI_Tandem/blob/master/BA200/threshold_shiny.R): An interactive graph was created to help behavioral scientists understand the number of students meeting complex tailoring thresholds and what tailored messages were received by students the most.

Engineering 100 (ENGR100) Course:
- [W19_surveyEDA.Rmd](https://github.com/mclu/CAI_Tandem/blob/master/ENGR100/W19_surveyEDA.Rmd), [W19_surveyEDA.html](https://github.com/mclu/CAI_Tandem/blob/master/ENGR100/W19_surveyEDA.html): This is similar to the exploratory analysis for BA200 class. I used to diverging stacked bar chart and modified Wilcoxon signed-rank test to compare the BOT and EOT surveys.
