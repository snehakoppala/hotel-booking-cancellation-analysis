# Hotel Booking Cancellation Analysis and Predictive Modeling

This repository contains an end-to-end analysis of hotel booking data from a global hotel chain. The project explores key drivers of booking cancellations and builds a logistic regression model to predict cancellation risk.

---

## 1. Business Problem

Hotel cancellations create revenue leakage and operational uncertainty. The goals of this project are:

- Understand how pricing, booking behavior, and customer characteristics affect cancellation risk.
- Quantify differences in Average Daily Rate (ADR) between hotel types.
- Build a predictive model that estimates the probability a booking will be canceled.

---

## 2. Data

- **Source:** Public hotel bookings dataset (after cleaning).
- **Clean sample size:** ~113,000 bookings.
- **Key variables:**
  - `iscanceled` - booking canceled (Yes/No)
  - `adr` - Average Daily Rate (revenue per night)
  - `leadtime` - days between booking date and arrival date
  - `deposittype` - No Deposit, Non Refund, Refundable
  - `hotel` - City vs Resort
  - `totalofspecialrequests` - number of special requests made
  - Additional fields: customer type, distribution channel, previous cancellations, previous non-canceled bookings, repeated guest flag, etc.

Missing values in `children` were imputed as 0. Records with `adr` equal to 0 or extreme outliers were removed to ensure stable modeling.

---

## 3. Repository Structure

```
hotel-booking-cancellation-analysis/
├── data/
│   └── hotel_bookings.csv
├── scripts/
│   └── Project-HotelBooking.R
├── reports/
│   └── Report
└── figures/
    ├── Specificity vs Sensitivity.jpg
    ├── Residuals vs Fitted and Q-Q plots.jpg
    ├── adr-by-hotel-type.jpg
    ├── adr-by-cancellation.jpg
    ├── leadtime-by-cancellation.jpg
    ├── totalstay-by-cancellation.jpg
    ├── cancellation-rate-by-hotel-type.jpg
    ├── cancellation-rate-by-deposit-type.jpg
    └── cancellation-rate-by-customer-type.jpg
```

---

## 4. Methods

### Exploratory Data Analysis

- Lead time distribution by cancellation status
- ADR distribution across hotel types and deposit types
- Total stay length by cancellation status
- Cancellation rates by customer type, deposit type, and hotel type

### Hypothesis Testing

**1. ADR vs Hotel Type**
- H0: Mean ADR is the same for City and Resort hotels.
- Tools: Descriptive statistics, one-way ANOVA, Levene's test, Welch t-test.
- Result: City hotels have significantly higher ADR (~$105) than Resort hotels (~$86); H0 is rejected.

**2. Cancellation vs Deposit Type**
- H0: Deposit type and cancellation status are independent.
- Tools: Contingency table, chi-square test of independence.
- Result: Strong association (chi-square ~27,335, p < 2.2e-16); deposit rules strongly influence cancellation behavior.

### Predictive Modeling

- **Model:** Logistic regression with `iscanceled` (Yes/No) as the response.
- **Main predictors:** Lead time, ADR, hotel type, deposit type, customer type, total special requests, previous cancellations, previous non-canceled bookings, repeated guest flag.
- **Evaluation:** 70/30 train-test split, ROC/AUC via `pROC`, confusion matrix via `caret`.

---

## 5. Key Results

### Exploratory Insights

| Factor | Finding |
|---|---|
| Lead time | Canceled: median ~115 days vs Non-canceled: median ~47 days |
| Total stay | Almost identical distributions - not a useful predictor |
| Customer type | Transient guests highest cancellation (~41%), Group lowest (~7%) |
| Deposit type | Non Refund bookings ~99% cancellation rate |
| Hotel type | City hotels: ~42% cancellation rate vs Resort: ~27% |

### Hypothesis Test Results

- **ADR by hotel type:** City hotels charge ~$19 more per night on average; statistically significant with unequal variances confirmed.
- **Deposit vs cancellation:** Strongly associated; deposit policies are a major driver of cancellation patterns.

### Logistic Regression Model

**Model Fit:**
- Null deviance: ~105,194
- Residual deviance: ~71,954
- AIC: ~72,012

**Key Odds Ratios:**

| Predictor | Odds Ratio | Interpretation |
|---|---|---|
| Deposit: Non Refund | ~2.85 | Strong positive association with cancellations |
| Total special requests | ~0.49 | Each additional request lowers cancellation odds |
| Hotel: Resort | ~0.82 | Lower risk than City hotels |
| Lead time | ~1.004/day | Longer lead times increase cancellation risk |
| Customer type: Transient | ~2.16 | Higher risk vs other customer types |

**Performance at 0.5 cutoff:**

| Metric | Value |
|---|---|
| Accuracy | ~80% |
| Precision | ~87% |
| Sensitivity (Recall) | ~56% |
| Specificity | ~95% |
| Balanced Accuracy | ~0.75-0.76 |
| Train AUC | ~0.831 |
| Test AUC | ~0.834 |

The model generalizes well (train and test AUC nearly identical) and behaves as a conservative classifier - minimizing false cancellation alerts while maintaining strong overall discrimination.

---

## 6. Business Insights

- **Deposit policies and lead time** are the strongest operational levers for managing cancellation risk.
- **City hotels, transient guests, and OTA bookings** form high-risk segments that may benefit from stricter deposit rules or proactive communication.
- **Special requests and repeat bookings** act as protective engagement signals that lower cancellation risk.
- **Non Refund booking records** warrant a data audit due to their near-100% recorded cancellation rate.

---

## 7. How to Run the Analysis

1. Clone this repository or download the files.
2. Open `scripts/Project-HotelBooking.R` in RStudio or Posit Cloud.
3. Set the working directory to the repository root.
4. Install required packages if not already installed:

```r
install.packages(c("dplyr", "ggplot2", "car", "broom", "caret", "pROC"))
```

5. Run the script section by section to reproduce data cleaning, EDA, hypothesis tests, model fitting, and all plots.

---

## 8. Tools and Technologies

- **Language:** R
- **Key packages:** `dplyr`, `ggplot2`, `car`, `broom`, `caret`, `pROC`
- **Environment:** RStudio / Posit Cloud
- **Version control:** GitHub

---

## 9. Authors

Sneha K S
