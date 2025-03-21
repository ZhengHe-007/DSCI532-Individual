# ICU Bed Capacity Dashboard

## **Motivation**

**Target audience:** Healthcare administrators, hospital resource planners

Managing ICU bed capacity is critical for hospitals, especially during high-demand periods such as pandemics or seasonal flu outbreaks. Ensuring that resources are allocated effectively helps prevent overwhelming healthcare facilities and improves patient outcomes. This **ICU Bed Capacity Dashboard** allows healthcare administrators to **monitor projected ICU demand and availability** over time for different **Hospital Referral Regions (HRRs)**. The interactive visualizations help decision-makers assess whether ICU resources meet projected demand, plan for potential shortages, and take action to optimize capacity utilization.

## **App Description**

This Shiny app provides interactive data visualizations to explore ICU bed availability and demand projections across different HRR regions.

Click the link below to watch the demo video:

▶️ [Watch the Demo Video](https://github.com/ZhengHe-007/DSCI532-Individual/raw/main/img/demo.mp4)


## **Installation & Setup**

**1. Clone the Repository**
```
git clone https://github.com/your-repo/icu-bed-dashboard.git
cd icu-bed-dashboard
```
**2. Create and Activate the Conda Environment**
```
conda env create -f environment.yml
conda activate icu-bed-dashboard
```
**3. Run the App**
```
Rscript -e "shiny::runApp('src/app.R', launch.browser = TRUE)"
```
## **Acknowledgments**

This dashboard was designed for hospital administrators and healthcare planners to support decision-making in ICU capacity management.

Data Source: https://www.kaggle.com/datasets/mrmorj/hospital-bed-capacity-and-covid19/data
