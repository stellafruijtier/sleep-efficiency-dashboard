# sleep-efficiency-dashboard

## Sleep efficiency & Lifestyle Dashboard

This project investigates how lifestyle factors such as **alcohol consumption, exercise frequency, and smoking** influence **sleep efficiency, duration, and awakenings**. Using **R** and **Shiny**, we developed an interactive dashboard that visualizes these patterns across age and gender categories.

This work was completed as the final project for the *Advanced Data Visualization* course at **Rollins College**.


## Background

We explored three core hypotheses:

1. **Smoking and sleep fragmentation:** Smokers, especially older adults, experience lower sleep efficiency and more frequent awakenings.

2. **Exercise and sleep quality:** Regular physical activity correlates positively with both sleep duration and sleep efficiency

3. **Alcohol and sleep decline:** Increased alcohol consumption leads to decreased sleep efficiency, though teh effect varies by demographic.

The dashboard enables users to dynamically filter by lifestyle and demographic variables to explore these hypotheses visually and statistically


## What this dashboard does

- Imports lifestyle and sleep dataset
- Enables filtering by: **age, gender, smoking, alcohol, caffeine, and exercise frequency**
- Visualizes relationships across three analysis tabs:

  - **Alcohol & Sleep**
  - **Exercise & Sleep**
  - **Sleep & Awakenings**

- Presents findings through:

  - Interactive scatter plots
  - Boxplots
  - Summary statistics

- Supports **tailored exploration** of sleep trends across demographics


## Requirements

- R version 4.0 or later
- Packages:

<pre>
install.packages(c("shiny", "plotly", "dplyr", "lubridate", "DT", "readr", "tidyr", "shinyWidgets", "bslib"))
</pre>


## How to run

1. Clone the repository:

<pre>
git clone https://github.com/stellafruijtier/sleep-efficiency-dashboard.git
cd sleep-efficiency-dashboard
</pre>

2. Launch the dashboard in R:

<pre>
shiny::runApp("sleep_dashboard.R")
</pre>


## Key Findings

🔷 Alcohol

- Alcohol negatively impacts sleep efficiency - but not uniformly
- Younger men showed surprising improvements in sleep efficiceny at high consumption levels
- Older women displayed the steepest declines as alcohol intake increased
- Both genders saw a drop in efficiency from light to heavy drinking

🔷 Exercise

- Regular exercise improves sleep quality for both genders
- Women benefit more consistently, especially at moderate frequencies (2-4x/week)
- Men showed more variability and less predictable gains
- Very regular exercise (4+ times/week) led to more **stable sleep durations**

🔷 Smoking & Awakenings

- Older smokers tended to show **lower sleep efficiency** and **more awakenings**
- Surprisingly, non-smokers sometimes showed more awakenings, suggesting other external factors (e.g., anxiety, medication) may also play a role
- Strong age x smoking interaction visible in bubble and boxplot visualizations.


## Team Contributions

- **Stella Fruijtier** - *Exercise & Sleep Tab*: Creaed visualizations and analysis linking exercise frequency with sleep outcomes. Developed filtering by age, gender, and smoking status.
- **Nico de Giacomo** - *Alcohol & Sleep Tab*: Built visual analysis to compare alcohol's effects across demographics slices. Led core hypothesis design.
- **Ignacio Bayly** - *Sleep & Awakenings Tab*: Designed visuals examinig awakenings vs. smoking across age groups.

All members collaborated on UI design, visual style, layout, and interpretation.


## Intended Impact

This dashboard offers practical insights for:

- Wellness coaches designing behavior change plans
- Health professionals analyzing sleep metrics
- Individuals wanting to optimize personal sleep hygiene

The results highlight that **sleep behavior is not one-size-fits-all** demographic aware, data-driven recommendations are key to improving wellness.


## Acknowledgments

This project was developed for the **Advanced Data Visualization** course at Rollins College given by Jasser Jasser.
