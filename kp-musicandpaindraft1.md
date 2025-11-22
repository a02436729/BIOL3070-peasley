---
title: "Peasley-newrmarkdownfinaltemplatefordata"
author: "Kylie Peasley"
date: "2025-11-22"
output:
  html_document:
    keep_md: true
---



# ABSTRACT
Fibromyalgia is a chronic medical condition and presents with symptoms characterized by  pain, fatigue, and various challenges in daily life. Music therapy has shown positive results in reducing pain perception. This study reanalyzed the dataset from Pando-Naudé et al. (2019) to evaluate whether listening to music reduces pain more effectively than a control (pink noise) in individuals with fibromyalgia. The dataset included forty individuals total; 20 fibromyalgia patients and 20 age-matched controls. The data was visualized with boxplots and statistical t-tests were conducted to determine whether there were significant differences between pre and post condition pain levels. Results showed a decrease in pain ratings after listening to music and a slight increase in the control, suggesting that pink noise may not provide analgesic effects. These findings support the hypothesis that music may have pain relieving effects in individuals with fibromyalgia. Although  limited by subjective pain reporting and a lower sample size, the results provide more support for music therapy for chronic pain management and can help pave way for future research.

# BACKGROUND
Fibromyalgia is a medical condition that presents through symptoms “including fatigue, sleep disturbances, cognitive dysfunction, and depressive episodes… chronic fatigue syndrome, irritable bowel syndrome (IBS), irritable bladder syndrome or interstitial cystitis, and temporomandibular disorder (TMD).” Such a disease negatively impacts an individual's ability to function in daily life. With no easy option for long-term relief, it becomes of the utmost importance to study a wide variety of therapies and treatments to improve how Fibromyalgia is addressed and treated.
In past studies, music has been associated with benefits in treatment and outcome. In their study, Kemper & Danhauer reported that individuals who received music treatment experienced lower levels of anxiety and enhanced moods. Improving the individual’s experience of pain and anxiety often indirectly improves physical condition, thereby raising quality of life.
  Our study addresses the effect of music on an individual's experience of chronic pain due to fibromyalgia. Participants in this study, all with Fibromyalgia, are split into two groups: control and treatment. These two groups are questioned about their pain level from 1-10 both before and after receiving the music treatment, either the control or music.
  This study is important because it addresses a significant need in our society. Many individuals who struggle with this chronic pain have had unsatisfactory results with traditional medicine and treatment. Believed to be present in 2-8% of the world population, effective treatment and relief from Fibromyalgia becomes an important breakthrough to strive for. This would thereby improve society and further our understanding of this disease.
The question of our study is: does music treatment help relieve pain or lower pain perception associated with fibromyalgia? We predict that participants with fibromyalgia experiencing chronic pain will report less pain while listening to music compared to pink noise (control). Additionally, we predict that participants will report lower pain after listening to music than before music treatment. Our hypothesis is that the statistical tests (T-tests) run with the data will show reliable significance.


``` r
library(ggplot2)
library(tidyr)
library(dplyr)

data <- read.csv("dataset.csv")

data_long <- data %>%
  pivot_longer(
    cols = c(pic1, pic2, pim1, pim2),
    names_to = "treatment",
    values_to = "value"
  )

ggplot(
  data_long %>%
    mutate(
      treatment = recode(
        treatment,
        pic1 = "Before Control",
        pic2 = "After Control",
        pim1 = "Before Music",
        pim2 = "After Music"
      ),
      treatment = factor(
        treatment,
        levels = c("Before Control", "After Control", "Before Music", "After Music")
      )
    ),
  aes(x = treatment, y = value, fill = treatment)
) +
  geom_boxplot(alpha = 0.6) +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  coord_cartesian(ylim = c(0, 10)) +
  scale_fill_brewer(palette = "Spectral") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10))
  ) +
  xlab("Pain Intensity") +
  ylab("Pain Level")
```

```
## Warning: Removed 92 rows containing non-finite outside the scale range
## (`stat_boxplot()`).
```

![](kp-musicandpaindraft1_files/figure-html/code chunk 1-1.png)<!-- -->

# STUDY QUESTION and HYPOTHESIS

## Questions  
 Does music treatment help relieve pain or lower pain perception associated with fibromyalgia?

## Hypothesis 
  Music therapy decreases the sensation of pain compared to or pink noise.
  
## Prediction
  Participants with fibromyalgia who were given music as a treatment will have an overall reduction in pain levels.

# METHODS
  We used the dataset provided by Pando-Naude et al.(2019), Functional connectivity of music-induced analgesia in fibromyalgia. This dataset included twenty individuals diagnosed with fibromyalgia (FM) and twenty healthy individuals matching in age, as a control (HC). Before the start of data collection, the FM group were instructed not to take any medications to assist in pain that day and HC group individuals went through a screening to ensure no presence of pain. 
    
  In the original study, participants were asked to rate their pain level on a scale of 1-10 before and after listening to 5 minutes of either music chosen by the individual or the control (pink noise). The music chosen by the individual had to meet certain criteria, having a tempo less than 120 bpm (defined as a slow tempo), as well as being familiar and deemed as pleasant by the individual. 

  Participants provided pain ratings (on a scale of one to ten) before the music or control and then after listening to either music or control. Four trials were done for each individual, each lasting approximately five minutes. For this study, no new data was collected and the original dataset was not tampered with nor was any data added to it or removed from it. 

  The data was first analyzed by creating several boxplots to visualize it before running a statistical T-Test. The first figure is a 4 panel side-by-side boxplot, comparing pain intensity across pre-control, post-control, pre-music, and post-music conditions. Figure 2 is a side-by-side boxplot comparing the pre-control and post-control pain ratings. Figure 3 is another side-by-side boxplot comparing the pre-music and post-music pain ratings. These boxplots allowed for initial visualization of the potential differences before and after the control and music, showing possible music-induced analgesic effects.

  Before running statistical analyses, the music condition variable (music vs. no-music) was recoded into binary form (1/0) in a spreadsheet and then exported back into CSV format. This formatting step ensured compatibility with the statistical functions used in R. The statistical test was done in R and the p-value was used to determine how significant the results are.

   We conducted two paired T-tests, one for the control group and another for the music group to determine whether pain ratings differed significantly between the pre- and post-stimulus conditions. All statistical analyses were performed in R and the resulting p-value and t-value were used to evaluate the statistical significance of any observed differences. 


``` r
library(ggplot2)
library(tidyr)
library(dplyr)

data <- read.csv("dataset.csv")

# Pivot pic1 and pic2 into long format
data_long <- data %>%
  pivot_longer(cols = c(pic1, pic2), names_to = "treatment", values_to = "value")

# Make boxplot
ggplot(data_long, aes(x = treatment, y = value, fill = treatment)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  theme_classic() +
  scale_fill_brewer(palette = "Set1") +
  ylab("Pain Level") +
  xlab("Pain Intensity-Control 1 (before) and Pain Intensity-Control 2 (after)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )
```

```
## Warning: Removed 46 rows containing non-finite outside the scale range
## (`stat_boxplot()`).
```

![](kp-musicandpaindraft1_files/figure-html/code chunk 2-1.png)<!-- -->


``` r
library(ggplot2)
library(tidyr)
library(dplyr)

# Read your dataset
data <- read.csv("dataset.csv")

# Pivot pic2 and pim2 into long format
data_long <- data %>%
  pivot_longer(cols = c(pim1, pim2), names_to = "treatment", values_to = "value")

# Make boxplot
ggplot(data_long, aes(x = treatment, y = value, fill = treatment)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  theme_classic() +
  scale_fill_brewer(palette = "Set1") +
  ylab("Pain Level") +
  xlab("Pain Intensity-Music 1 (before) and Pain Intensity-Music 2 (after)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )
```


``` r
library(ggplot2)
library(tidyr)
library(dplyr)

data <- read.csv("dataset.csv")

# Pivot pim1 and pim2 into long format
data_long_pim <- data %>%
  pivot_longer(cols = c(pic2, pim2), names_to = "treatment", values_to = "value")

# Make boxplot
ggplot(data_long_pim, aes(x = treatment, y = value, fill = treatment)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  theme_classic() +
  scale_fill_brewer(palette = "Set1") +
  ylab("Pain Level") +
  xlab("Pain Intensity-Control 2 (after) and Pain Intensity-Music 2 (after)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )
```

```
## Warning: Removed 46 rows containing non-finite outside the scale range
## (`stat_boxplot()`).
```

![](kp-musicandpaindraft1_files/figure-html/code chunk pim-1.png)<!-- -->



``` r
t.test(data$pic2, data$pic1, paired = TRUE)
```

```
## 
## 	Paired t-test
## 
## data:  data$pic2 and data$pic1
## t = 1.9262, df = 16, p-value = 0.07204
## alternative hypothesis: true mean difference is not equal to 0
## 95 percent confidence interval:
##  -0.04733106  0.98850753
## sample estimates:
## mean difference 
##       0.4705882
```


``` r
t.test(data$pim2, data$pim1, paired = TRUE)
```

```
## 
## 	Paired t-test
## 
## data:  data$pim2 and data$pim1
## t = -3.0822, df = 16, p-value = 0.007141
## alternative hypothesis: true mean difference is not equal to 0
## 95 percent confidence interval:
##  -1.8863514 -0.3489427
## sample estimates:
## mean difference 
##       -1.117647
```

# RESULTS
 Our results show statistical evidence suggesting that music leads to a greater decrease in pain after subjects listened to music. The control exhibited moderate correlation for the pink noise to affect pain levels in the positive direction, meaning that there may be evidence suggesting that pink noise increased pain. The treatment group showed a strong statistical significance, rejecting the null hypothesis that music does not affect pain sensation. Furthermore, there was a greater mean difference that was in the negative direction for music, reflecting a visible decrease in the average reported pain levels in patients.

# DISCUSSION/CONCLUSION
  Although our evidence supports our hypothesis, we faced limitations in our research. The major limitation was each person's interpretation of pain because of each individual's personal experiences and pain tolerances. A way to reduce the effects of this is to use a visual pain scale, such as the “Wong-Baker FACES Scale,” that can provide a visual reference point to help patients determine pain levels on a scale that could be more universal. Additionally, the experiment utilized data from only 20 patients. Future research may benefit from a larger sample size in order to achieve more concise data.
  
  The results of this experiment promote further research into finding ways to relieve pain in patients presenting with a Fibromyalgia diagnosis, specifically utilizing music as a therapy method. Further research should compare both different forms of “noise” (white, brown, etc.) and multiple music genres to other methods of pain reduction, such as medication or physical/talk therapy. Regardless, because of the idiopathic cause of this disease, the evidence showing decreased pain from music is a step towards a more comfortable future in patients who have this diagnosis, as they may find affordable and effective methods for finding relief.


# REFERENCES     
1. Eduardo A. Garza-Villarreal, Victor Pando-Naude, Fernando A. Barrios, Peter Vuust, Lene Vase, Elvira Brattico, Erick H. Pasaye, and Sarael Alcauter (2019). Functional Connectivity of Music-Induced Analgesia in Fibromyalgia. OpenNeuro. [Dataset] doi: 10.18112/openneuro.ds001928.v1.1.0

2. ChatGPT. OpenAI, version Jan 2025. Used as a reference for functions such as plot() and to correct syntax errors. Accessed 2025-11-22.

3. Clauw, Daniel J. “Fibromyalgia: A Clinical Review” | Rheumatology | JAMA | Jama Network, jamanetwork.com/journals/jama/fullarticle/1860480/. Accessed 20 Nov. 2025. 

4. Clauw, Daniel J. “Fibromyalgia: An Overview.” The American Journal of Medicine, vol. 122, no. 12, Dec. 2009, doi:10.1016/j.amjmed.2009.09.006. 

5. Danhauer, Suzanne. “Music as Therapy.” Southern Medical Journal, 2005. https://doi.org/10.1097/01.SMJ.0000154773.11986.39
