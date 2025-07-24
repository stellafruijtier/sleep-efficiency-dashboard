# sleep-efficiency-dashboard

## 🛌 Sleep efficiency & Lifestyle Dashboard

This project investigates how lifestyle factors such as **alcohol consumption, exercise frequency, and smoking** influence **sleep efficiency, duration, and awakenings**. Using **R** and **Shiny**, we developed an interactive dashboard that visualizes these patterns across age and gender categories.

This work was completed as the final project for the *Advanced Data Visualization* course at **Rollins College**.


## 📚 Background

We explored three core hypotheses:

1. **Smoking and sleep fragmentation:** Smokers, especially older adults, experience lower sleep efficiency and more frequent awakenings.

2. **Exercise and sleep quality:** Regular physical activity correlates positively with both sleep duration and sleep efficiency

3. **Alcohol and sleep decline:** Increased alcohol consumption leads to decreased sleep efficiency, though teh effect varies by demographic.

The dashboard enables users to dynamically filter by lifestyle and demographic variables to explore these hypotheses visually and statistically


## 🧠 What this dashboard does

- Imports lifestyle and sleep dataset
- Enables filtering by: **age, gender, smoking, alcohol, caffeine, and exercise frequency**
- Visualizes relationships across three analysis tabs:

  - 🍷 **Alcohol & Sleep**
  - 🏋🏻 **Exercise & Sleep**
  - 😴 **Sleep & Awakenings**

- Presents findings through:

  - Interactive scatter plots
  - Boxplots
  - Summary statistics

- Supports **tailored exploration** of sleep trends across demographics


## ⚙️ Requirements

- R version 4.0 or later
- Packages:

<pre>
install.packages(c("shiny", "plotly", "dplyr", "lubridate", "DT", "readr", "tidyr", "shinyWidgets", "bslib"))
</pre>
