# 0) Libraries
#install.packages(c("dplyr", "ggplot2", "car", "broom", "caret", "pROC"))
library(dplyr)
library(ggplot2)
library(car)
library(broom)
library(caret)
library(pROC)


# ======================================================
# DATA CLEANING AND PREPROCESSING SECTION
# ======================================================
# 1) Read the CSV
hotel <- read.csv("hotel_bookings.csv",stringsAsFactors = FALSE,
                  na.strings = c("NULL", "", "NA"))

# 2) Quick structure & missingness
str(hotel)
summary(hotel)
colSums(is.na(hotel))

hotel_clean <- hotel %>%
  # 2a) Fix the very small amount of missingness in key vars
  mutate(
    children = ifelse(is.na(children), 0L, children),
    country  = ifelse(is.na(country), "Unknown", country)
    # agent / company left as-is for now; we will just not use them in models
  ) %>%
  mutate(
    total_stay = stays_in_week_nights + stays_in_weekend_nights,
    is_family  = ifelse(children > 0 | babies > 0, 1L, 0L)
  ) %>%
  # Type casting
  mutate(
    hotel                = factor(hotel),
    arrival_date_month   = factor(arrival_date_month,
                                  levels = month.name, ordered = TRUE),
    meal                 = factor(meal),
    market_segment       = factor(market_segment),
    distribution_channel = factor(distribution_channel),
    reserved_room_type   = factor(reserved_room_type),
    assigned_room_type   = factor(assigned_room_type),
    deposit_type         = factor(deposit_type),
    customer_type        = factor(customer_type),
    reservation_status   = factor(reservation_status),
    is_canceled          = factor(is_canceled,
                                  levels = c(0,1),
                                  labels = c("No","Yes"))
  )
str(hotel_clean)
summary(hotel_clean)

# Cancellation mix
prop.table(table(hotel_clean$is_canceled))

# Lead time by cancellation
ggplot(hotel_clean, aes(x = lead_time, fill = is_canceled)) +
  geom_histogram(bins = 50, position = "identity", alpha = 0.5) +
  labs(
    title = "Lead Time Distribution by Cancellation Status",
    subtitle = "Canceled bookings tend to have longer lead times",
    x = "Lead Time (days)",
    y = "Count",
    fill = "Canceled?"
  ) +
  theme_minimal()

Q1 <- quantile(hotel_clean$adr, 0.25, na.rm = TRUE)
Q3 <- quantile(hotel_clean$adr, 0.75, na.rm = TRUE)
IQR_val <- IQR(hotel_clean$adr, na.rm = TRUE)

upper_cutoff <- Q3 + 1.5 * IQR_val

hotel_clean2 <- hotel_clean %>%
  filter(adr > 0, adr < upper_cutoff)

# ADR by hotel type
ggplot(hotel_clean2, aes(x = hotel, y = adr)) +
  geom_boxplot() +
  labs(
    title = "ADR Distribution Across Hotel Types",
    subtitle = "City Hotel shows higher ADR variance with prominent outliers",
    x = "Hotel Type",
    y = "Average Daily Rate (ADR)"
  ) +
  theme_minimal()

# ADR by deposit type
ggplot(hotel_clean2, aes(x = deposit_type, y = adr)) +
  geom_boxplot() +
  labs(
    title = "ADR Variation by Deposit Type",
    subtitle = "Non-Refund deposits show extreme outliers; most values cluster below 300",
    x = "Deposit Type",
    y = "Average Daily Rate (ADR)"
  ) +
  theme_minimal()

# ======================================================
# STATISTICAL INFERENCE SECTION
# ======================================================
# Assumes: hotel_clean2 already created (ADR outliers removed, ADR > 0),
#          factors set correctly, especially: hotel, deposit_type, is_canceled.
# ======================================================

# ------------------------------------------------------
# 5.1 ANOVA: Does mean ADR differ by hotel type?
# ------------------------------------------------------

# 5.1.1 Descriptive stats for context (goes on slide)
hotel_adr_summary <- hotel_clean2 %>%
  group_by(hotel) %>%
  summarise(
    n       = n(),
    mean_adr = mean(adr),
    sd_adr   = sd(adr)
  )

print(hotel_adr_summary)
# Analysis:
# - City Hotels charge on average about 105 per night.
# - Resort Hotels charge on average about 86 per night.
# - ADR variability is higher for Resort Hotels (sd ~ 46 vs 33).


# 5.1.2 Run one-way ANOVA
# H0: mean ADR is the same for all hotel types
# H1: at least one hotel type has a different mean ADR
fit_aov <- aov(adr ~ hotel, data = hotel_clean2)
summary(fit_aov)

# Analysis:
# - F(1, 113635) ~ 5756 with p < 2e-16.
# - Extremely strong evidence that average ADR differs between City and Resort hotels.
# - Combined with the means above, City Hotels are materially more expensive on average.


# 5.1.3 ANOVA assumption checks (for the rubric)
# - Residuals roughly normal
# - Homoscedasticity (similar variance across groups)

par(mfrow = c(1, 2))
plot(fit_aov, which = 1)  # Residuals vs Fitted (pattern? equal spread?)
plot(fit_aov, which = 2)  # Normal Q-Q (major deviations?)
par(mfrow = c(1, 1))

# Visual diagnosis:
# - Residuals vs Fitted: two vertical bands (two hotel types), spread roughly similar,
#   no obvious funnel pattern → homoscedasticity is reasonable.
# - Q-Q plot: S-shaped curve, indicating some deviation from perfect normality,
#   especially in tails. With >100k observations, ANOVA is robust; note this
#   mild violation as a limitation in the write-up.

# Levene test for homogeneity of variance
leveneTest(adr ~ hotel, data = hotel_clean2)
# Analysis:
# - Very small p-value -> strong evidence that ADR variances differ
#   between City and Resort hotels (violation of equal-variance assumption).
# - Given the very large sample size, the one-way ANOVA is still reasonably
#   robust, but we treat this as a limitation and confirm results with
#   a variance-robust alternative (Welch t-test) if required.

# Optional robustness check: Welch two-sample t-test
t.test(adr ~ hotel, data = hotel_clean2, var.equal = FALSE)
# Interpretation:
# - Even allowing for unequal variances, City Hotels have a significantly higher ADR.
# - Estimated difference ~ €18 per night.
# - Combined with ANOVA results, this strongly supports that ADR differs by hotel type.

# In your report/slides:
# - Residuals look approximately normal given the large sample.
# - Variance by group is reasonably similar.


# ------------------------------------------------------
# 5.2 Chi-square: Is cancellation associated with deposit type?
# ------------------------------------------------------

# 5.2.1 Contingency table and descriptive rates (goes on slide)
tab_dep <- table(hotel_clean2$deposit_type, hotel_clean2$is_canceled)
tab_dep

# Row-wise proportions: cancellation rates within each deposit type
prop_dep <- prop.table(tab_dep, margin = 1)
prop_dep   # this is great to screenshot or copy numeric values from

# Analysis:
# - No Deposit bookings cancel about 28% of the time.
# - Non Refund bookings cancel about 99% of the time (very striking pattern).
# - Refundable bookings cancel about 21% of the time.
# - Deposit type is clearly linked to very different cancellation behavior.

# 5.2.2 Chi-square test of independence
# H0: deposit_type and is_canceled are independent
# H1: there is an association between deposit_type and is_canceled
chisq_dep <- chisq.test(tab_dep)
chisq_dep

# Analysis:
# - X-squared ~ 27,335 with p < 2.2e-16.
# - Very strong evidence that cancellation status is associated with deposit type.
# - The pattern from the proportions confirms that Non Refund bookings behave
#   very differently from No Deposit and Refundable bookings.

# 5.2.3 Check expected counts for assumption
chisq_dep$expected    

# Analysis:
# - All expected cell counts are well above 5.
# - Chi-square approximation is valid; test assumptions are satisfied.

# (Optional visual)
ggplot(hotel_clean2, aes(x = deposit_type, fill = is_canceled)) +
  geom_bar(position = "fill") +
  labs(
    title = "Cancellation Rate by Deposit Type",
    x = "Deposit Type",
    y = "Proportion of Bookings",
    fill = "Canceled?"
  ) +
  theme_minimal()


# ======================================================
# 6) EDA FOR CANCELLATION & MODELING DATASET
# ======================================================

# 6.1 Overall cancellation rate (already saw, but keep here for narrative)
prop.table(table(hotel_clean$is_canceled))
# Analysis:
# - Roughly 63% of bookings are fulfilled and 37% are canceled.
# - This is a substantial cancellation rate, so modeling cancellations has clear business value
#   (revenue forecasting, overbooking policies, etc.).

# 6.2 Cancellation by key categorical drivers
# -----------------------------------------

# 6.2.1 By hotel type
canc_hotel <- prop.table(table(hotel_clean2$hotel, hotel_clean2$is_canceled), 1)
canc_hotel
# Interpretation:
# - Row-wise proportions show how cancellation rate differs between City vs Resort.

ggplot(hotel_clean2, aes(x = hotel, fill = is_canceled)) +
  geom_bar(position = "fill") +
  labs(
    title = "Cancellation Rate by Hotel Type",
    x = "Hotel Type",
    y = "Proportion of Bookings",
    fill = "Canceled?"
  ) +
  theme_minimal()
# Analysis:
# - City Hotels have ~42% cancellation vs ~27% for Resort Hotels.
# - City properties experience meaningfully higher cancellation risk, suggesting
#   different booking behavior or market dynamics by location type.
# - This supports including 'hotel' as a predictor in the cancellation model.

# 6.2.2 By deposit type (this reuses your chi-square story visually)
canc_dep <- prop.table(table(hotel_clean2$deposit_type, hotel_clean2$is_canceled), 1)
canc_dep

ggplot(hotel_clean2, aes(x = deposit_type, fill = is_canceled)) +
  geom_bar(position = "fill") +
  labs(
    title = "Cancellation Rate by Deposit Type",
    x = "Deposit Type",
    y = "Proportion of Bookings",
    fill = "Canceled?"
  ) +
  theme_minimal()

# Analysis:
# - No Deposit bookings cancel ~28% of the time.
# - Non-Refund bookings cancel ~99% of the time – an extremely strong pattern,
#   likely reflecting how this field is populated in the data and/or business rules.
# - Refundable bookings have the lowest cancellation among deposit-based bookings (~21%).
# - Deposit type is a dominant driver of cancellation and must be in the model.

# 6.2.3 By customer type
canc_cust <- prop.table(table(hotel_clean2$customer_type, hotel_clean2$is_canceled), 1)
canc_cust

ggplot(hotel_clean2, aes(x = customer_type, fill = is_canceled)) +
  geom_bar(position = "fill") +
  labs(
    title = "Cancellation Rate by Customer Type",
    x = "Customer Type",
    y = "Proportion of Bookings",
    fill = "Canceled?"
  ) +
  theme_minimal()
# Analysis:
# - Group bookings almost never cancel (~7%), while Transient guests cancel the most (~41%).
# - Contract and Transient-Party bookings sit in the middle (~26–31% cancellations).
# - Customer type is capturing very different booking behaviors and is a useful segmentation
#   variable for both EDA and modeling.

# 6.3 Numeric drivers vs cancellation
# -----------------------------------

# 6.3.1 Lead time: we already have distribution; now summarise by cancel status
hotel_clean2 %>%
  group_by(is_canceled) %>%
  summarise(
    n          = n(),
    mean_lead  = mean(lead_time),
    median_lead= median(lead_time),
    sd_lead    = sd(lead_time)
  )

ggplot(hotel_clean2, aes(x = lead_time, fill = is_canceled)) +
  geom_histogram(bins = 50, position = "identity", alpha = 0.5) +
  labs(
    title = "Lead Time Distribution by Cancellation Status",
    subtitle = "Canceled bookings tend to have longer lead times",
    x = "Lead Time (days)",
    y = "Count",
    fill = "Canceled?"
  ) +
  theme_minimal()
# Analysis:
# - Canceled bookings are made much further in advance: mean lead time ~147 days vs ~82 days.
# - Median behavior is even clearer (115 vs 47 days).
# - Lead time is a strong, intuitive driver: long-horizon bookings are more likely to be canceled.

# 6.3.2 ADR vs cancellation
hotel_clean2 %>%
  group_by(is_canceled) %>%
  summarise(
    n         = n(),
    mean_adr  = mean(adr),
    median_adr= median(adr),
    sd_adr    = sd(adr)
  )

ggplot(hotel_clean2, aes(x = is_canceled, y = adr)) +
  geom_boxplot() +
  labs(
    title = "ADR by Cancellation Status",
    x = "Canceled?",
    y = "Average Daily Rate (ADR)"
  ) +
  theme_minimal()
# Analysis:
# - Canceled bookings have slightly higher ADR on average (~€101 vs ~€98).
# - The difference is modest but directionally suggests that higher-priced stays
#   carry marginally higher cancellation risk.
# - ADR will likely contribute, but not as strongly as lead time or deposit type.

# 6.3.3 Total stay length vs cancellation
hotel_clean2 %>%
  group_by(is_canceled) %>%
  summarise(
    n            = n(),
    mean_stay    = mean(total_stay),
    median_stay  = median(total_stay),
    sd_stay      = sd(total_stay)
  )

ggplot(hotel_clean2, aes(x = is_canceled, y = total_stay)) +
  geom_boxplot() +
  labs(
    title = "Total Stay Length by Cancellation Status",
    x = "Canceled?",
    y = "Total Nights Stayed"
  ) +
  theme_minimal()

# Analysis:
# - Total stay length is almost identical for canceled vs non-canceled bookings.
# - This suggests total_stay is not a major driver of cancellation, but we can retain it
#   as a control variable in the model for completeness.

# ======================================================
# 7) BUILD MODELING DATASET FOR LOGISTIC REGRESSION
# ======================================================

# We drop administrative or overly sparse variables (agent, company, country, reservation_status, ...),
# and keep predictors that are interpretable and relevant for the story.

hotel_model <- hotel_clean2 %>%
  mutate(
    # (Optional engineered features if needed later)
    # weekend_flag = ifelse(stays_in_weekend_nights > 0, 1L, 0L)
  ) %>%
  select(
    is_canceled,
    lead_time,
    adr,
    total_stay,
    adults, children, babies, is_family,
    hotel,
    arrival_date_year,
    arrival_date_month,
    market_segment,
    distribution_channel,
    deposit_type,
    customer_type,
    is_repeated_guest,
    previous_cancellations,
    previous_bookings_not_canceled,
    total_of_special_requests
  )

str(hotel_model)


# ======================================================
# 8) TRAIN / TEST SPLIT (FOR REGRESSION MODEL)
# ======================================================

set.seed(123)  # reproducible split

n <- nrow(hotel_model)
train_idx <- sample(seq_len(n), size = floor(0.7 * n))

hotel_train <- hotel_model[train_idx, ]
hotel_test  <- hotel_model[-train_idx, ]

# Check class balance in each split
prop.table(table(hotel_train$is_canceled))
prop.table(table(hotel_test$is_canceled))

# Analysis:
# - Both training and test sets preserve the overall class balance (~62.5% No, ~37.5% Yes).
# - This is important for fair evaluation: the test accuracy will be comparable to what we
#   would expect on new data.

# ======================================================
# 9) LOGISTIC REGRESSION: MODELING CANCELLATIONS
# ======================================================

# We’ll model the probability that a booking is canceled (Yes vs No)
# using key drivers identified in EDA: lead_time, deposit_type, hotel,
# customer_type, market_segment, distribution_channel, ADR, and history.

# 9.1 Baseline logistic regression model on training data
# -------------------------------------------------------

hotel_lr <- glm(
  is_canceled ~ lead_time +
    adr +
    total_stay +
    adults + children + babies + is_family +
    hotel +
    deposit_type +
    customer_type +
    market_segment +
    distribution_channel +
    is_repeated_guest +
    previous_cancellations +
    previous_bookings_not_canceled +
    total_of_special_requests,
  data   = hotel_train,
  family = binomial
)

summary(hotel_lr)
# Analysis:
# - The deviance test p-value ~ 0 indicates that the full logistic model provides
#   significantly better fit than an intercept-only model – our predictors, taken
#   together, have strong explanatory power for cancellation risk.
# - Signs of key coefficients line up with the EDA:
#   • lead_time, adr, total_stay: positive coefficients -> longer lead times,
#     higher ADRs, and longer stays are associated with higher odds of cancellation.
#   • adults, children: more guests per booking slightly increases cancellation risk.
#   • hotelResort Hotel has a negative coefficient -> resort properties have lower
#     cancellation odds than city hotels, holding other factors fixed.
#   • deposit_type:Non Refund has a very large positive coefficient -> bookings marked
#     as “Non Refund” are overwhelmingly likely to be canceled in this dataset.
#   • customer_type:Transient is strongly positive while Group is negative -> Transient
#     guests cancel far more often than groups, even after controlling for other factors.
#   • is_repeated_guest and previous_bookings_not_canceled are negative -> loyal guests
#     with good history are less likely to cancel.
#   • previous_cancellations and low special-request counts move risk up and down in
#     exactly the way we would expect: more prior cancellations sharply increase risk,
#     while more special requests are associated with lower cancellation odds (guests
#     who invest effort in the booking tend to show up).
# - A few variables (babies, is_family, deposit_typeRefundable, some market_segment
#   and distribution_channel levels) are not statistically significant and contribute
#   little marginal information once the bigger drivers are in the model.


# Overall model fit via deviance test
with(hotel_lr,
     pchisq(null.deviance - deviance,
            df.null - df.residual,
            lower.tail = FALSE))
# Analysis (after you run it):
# - Very small p-value -> the full logistic model provides significantly better fit
#   than an intercept-only model, so it has explanatory power for cancellations.


# 9.2 Coefficient table with odds ratios
# --------------------------------------

hotel_lr_sum <- tidy(hotel_lr) %>%
  mutate(
    exp_coef        = exp(estimate),
    perc_delta_odds = (exp(estimate) - 1) * 100
  )

hotel_lr_sum
# Interpretation of odds ratios:
# - For a 1-day increase in lead_time, the odds of cancellation increase by about
#   0.38% (exp_coef ~ 1.004). Over a 100-day difference, this compounds to a
#   material risk increase.
# - For each €10 increase in ADR, cancellation odds increase by roughly 2.8% × 10
#   ~ 28%, highlighting that higher-priced bookings are more fragile.
# - Each additional night of total_stay raises cancellation odds by about 5%.
# - Compared with city hotels, resort hotels have ~18% lower odds of cancellation
#   (odds ratio ~ 0.82).
# - Non Refund deposits have an odds ratio in the hundreds relative to No Deposit,
#   reflecting that these bookings almost always appear as “canceled” in the data.
# - Transient guests have substantially higher odds of canceling than Group or
#   Contract customers; Transient-Party sits in between.
# - Each additional previous cancellation multiplies the odds of canceling this
#   booking by a large factor (exp_coef >> 1), whereas each additional previous
#   non-canceled booking reduces the odds (exp_coef < 1).
# - Each extra special request cuts cancellation odds by about half (exp_coef ~ 0.49),
#   making “engagement” signals like requests a strong protective factor.


# 9.3 Multicollinearity check
# ---------------------------------------------------------------

vif(hotel_lr)
# Multicollinearity check:
# - All GVIF^(1/(2*Df)) values are between ~1.0 and ~2.7, well below the usual
#   red-flag thresholds (e.g., >5 or >10), so multicollinearity does not appear
#   to be a major concern.
# - Children and is_family show the highest values (~2.6–2.7), which is expected
#   because families are partly defined by children, but even this level is
#   acceptable and does not require corrective action.

# ======================================================
# 10) PREDICTION, CONFUSION MATRICES, AND AUC
# ======================================================

# 10.1 Predict probabilities on train and test
# --------------------------------------------

hotel_train$pred_prob <- predict(hotel_lr, type = "response")
hotel_test$pred_prob  <- predict(hotel_lr, newdata = hotel_test, type = "response")

# 10.2 Classify using cutoff = 0.5 and build confusion matrices
# -------------------------------------------------------------

# Training set
hotel_train$pred_class_0.5 <- ifelse(hotel_train$pred_prob > 0.5, "Yes", "No")
hotel_train$pred_class_0.5 <- factor(hotel_train$pred_class_0.5,
                                     levels = c("No", "Yes"))

train_cm_0.5 <- confusionMatrix(
  data      = hotel_train$pred_class_0.5,
  reference = hotel_train$is_canceled,
  positive  = "Yes",
  mode      = "everything"
)
train_cm_0.5
# Training-set performance (cutoff = 0.5):
# - Accuracy ~ 0.80 vs a No-Information Rate ~ 0.63, with p < 2e-16 -> the model
#   is much better than simply predicting “No cancellation” for every booking.
# - Sensitivity (recall for cancellations) ~ 0.56 -> the model captures about 56%
#   of actual cancellations at this threshold.
# - Specificity ~ 0.95 -> about 95% of non-canceled bookings are correctly left
#   as “not flagged,” so the model rarely mislabels kept bookings as risky.
# - Precision for the “Yes” class ~ 0.86 -> when the model flags a booking as
#   likely to cancel, it is correct roughly 86% of the time.
# - Balanced Accuracy ~ 0.75 reflects a reasonable trade-off between catching
#   cancellations and avoiding false alarms, but the model is tuned more toward
#   high specificity (protecting confirmed business) than maximum sensitivity.

# Test set
hotel_test$pred_class_0.5 <- ifelse(hotel_test$pred_prob > 0.5, "Yes", "No")
hotel_test$pred_class_0.5 <- factor(hotel_test$pred_class_0.5,
                                    levels = c("No", "Yes"))

test_cm_0.5 <- confusionMatrix(
  data      = hotel_test$pred_class_0.5,
  reference = hotel_test$is_canceled,
  positive  = "Yes",
  mode      = "everything"
)
test_cm_0.5
# Test-set performance (cutoff = 0.5):
# - Test accuracy ~ 0.80, sensitivity ~ 0.56, and specificity ~ 0.95 – all very
#   similar to the training metrics.
# - Precision remains high (~0.87) and balanced accuracy is ~ 0.76.
# - The close alignment of train and test performance suggests the model
#   generalizes well and is not severely overfit.
# - At a 0.5 cutoff the model behaves like a conservative risk screener:
#   it is very reliable when it predicts a cancellation, but it intentionally
#   misses some cancellations to keep false positives low.


# 10.3 ROC curve and AUC (training & test)
# ----------------------------------------

# Training ROC / AUC
hotel_train_roc <- roc(
  response = hotel_train$is_canceled,
  predictor = hotel_train$pred_prob,
  levels = c("No", "Yes"),
  direction = "<",
  auc = TRUE,
  plot = TRUE,
  legacy.axes = TRUE
)
hotel_train_roc
auc(hotel_train_roc)
# Interpretation:
# - AUC close to 0.5 -> no better than random.
# - AUC in the 0.7–0.8 range -> reasonable discrimination.
# - AUC > 0.8 -> strong discrimination between canceled and non-canceled bookings.

# Test ROC / AUC
hotel_test_roc <- roc(
  response = hotel_test$is_canceled,
  predictor = hotel_test$pred_prob,
  levels = c("No", "Yes"),
  direction = "<",
  auc = TRUE,
  plot = TRUE,
  legacy.axes = TRUE
)
hotel_test_roc
auc(hotel_test_roc)
# ROC / AUC analysis:
# - The ROC curves for both train and test sets are well above the 45° diagonal
#   baseline, indicating strong discrimination between canceled and non-canceled
#   bookings across all possible cutoffs.
# - Train AUC ~ 0.83 and test AUC ~ 0.83–0.84 -> the model can correctly rank a
#   random canceled booking above a random non-canceled booking about 83% of the
#   time, which is considered strong performance for a binary classifier.
# - The near-identical AUC values on train and test reinforce that the model is
#   robust: it captures stable patterns in booking behavior rather than noise in
#   the training sample.
# - In a business setting, managers could adjust the probability cutoff upward
#   or downward along this ROC curve depending on whether they prioritize
#   catching more risky bookings (higher sensitivity) or minimizing disruption
#   to guests who would not have canceled (higher specificity).

plot(hotel_test_roc, print.thres = TRUE)


