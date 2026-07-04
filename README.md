# Employee Wellness EDA

Exploratory data analysis on an employee mental health survey to identify workplace factors linked with treatment-seeking behavior, with KPI engineering and visual insights for HR intervention planning.

## Business Problem

XYZ Technical Solutions recently lost an important employee to a mental-health-related incident. In response, the company wants to proactively understand which employees may need mental health support — before it becomes a crisis. This project analyzes existing employee survey data to uncover the workplace factors (support programs, stigma, work interference, leave policy, etc.) most associated with a need for treatment, so HR can design targeted wellness interventions.

**Problem Statement:** Which workplace support gaps, stigma-related factors, and work interference patterns are most associated with employees reporting a need for mental health treatment?

## Dataset

1,048 responses from a workplace mental health survey (Aug 2014), covering:

| Category | Columns |
|---|---|
| Demographics | Age, Gender, Country, state |
| Company profile | self_employed, no_employees, remote_work, tech_company |
| Employer support | benefits, care_options, wellness_program, seek_help, anonymity, leave |
| Stigma & disclosure | mental_health_consequence, phys_health_consequence, coworkers, supervisor, mental_health_interview, phys_health_interview, mental_vs_physical, obs_consequence |
| Target | treatment, work_interfere, family_history |

Source file: [`[data/employee_wellness_dataset.csv](https://github.com/DishaBisht75/employee-wellness-eda/blob/main/employee_wellness_dataset.csv)`](https://github.com/DishaBisht75/employee-wellness-eda/blob/main/employee_wellness_dataset.csv) 

## Approach

1. **Data Cleaning** — dropped irrelevant/sparse columns, fixed invalid Age outliers via median imputation, standardized 45 free-text Gender values into 3 categories, imputed nulls, removed duplicates.
2. **KPI Engineering** — built 6 custom indices from raw survey responses:
   - Employer Support Index
   - Workplace Interference Score
   - Stigma Index
   - Disclosure Comfort Score
   - Leave Accessibility Score
   - Family Risk Flag
3. **EDA** — univariate (bar/histogram), bivariate (box plots vs. treatment), and multivariate analysis (correlation heatmap, pair plot, violin plots, stacked bar charts).

## Key Visuals

**Correlation between KPIs**
![Correlation Heatmap](images/correlation_heatmap.png)

**Work interference is the strongest split by treatment need**
![Interference vs Treatment](images/interference_vs_treatment.png)

**KPI scores compared across treatment groups**
![KPI Boxplot](images/kpi_boxplot_vs_treatment.png)

## Key Insights

- **Work interference is the strongest signal** associated with needing treatment — employees reporting frequent interference are far more likely to report treatment=yes.
- **Family history is a strong risk marker** — employees with a family history of mental illness show a consistently stronger association with treatment need.
- **Stigma and disclosure comfort are inversely linked** — higher stigma correlates with lower comfort discussing mental health with coworkers/supervisors and with harder access to leave.
- **Awareness gaps exist around leave policy** — a large share of employees respond "Don't Know" when asked how easy it is to take mental health leave, suggesting a communication gap rather than a strict policy barrier.

## Recommendations

- Set up early-warning check-ins for employees showing signs of work interference.
- Improve communication of existing leave policies — many employees simply don't know their options.
- Address stigma directly (e.g., manager training, visible anonymity guarantees) to increase disclosure comfort.
- Consider family history (where voluntarily disclosed) as a factor in proactively offering wellness resources.

## Limitations

- Survey collected over a short 3-day window (Aug 27–29, 2014); may not reflect long-term patterns.
- Self-reported data is subject to response bias.
- Missing variables such as workload, job satisfaction, and absenteeism that could add further context.
- Findings are correlational, not causal.

## Tech Stack

- Python
- pandas, numpy
- matplotlib, seaborn
- Jupyter Notebook

## Repository Structure

```
employee-wellness-eda/
├── data/
│   └── employee_wellness_dataset.csv
├── notebooks/
│   └── employee_wellness_analysis.ipynb
├── images/
│   ├── correlation_heatmap.png
│   ├── interference_vs_treatment.png
│   └── kpi_boxplot_vs_treatment.png
├── requirements.txt
├── .gitignore
└── README.md
```

## How to Run

```bash
git clone https://github.com/<your-username>/employee-wellness-eda.git
cd employee-wellness-eda
pip install -r requirements.txt
jupyter notebook notebooks/employee_wellness_analysis.ipynb
```

## Author

Disha Bisht
www.linkedin.com/in/disha-bisht-00255935a
