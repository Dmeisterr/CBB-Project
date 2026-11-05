# Analyzing College Basketball Matchups through Team Archetypes

**Authors:** Seth Jernigan, Ian Stewart, Daniel Lott, Michael Masterton, Michael Borg

## Description
College basketball is a strategic game where a team's success depends on more than just athletic talent: team strategy and composition are important as well. One of the most interesting parts of college basketball is the difference in playing styles between teams. Some teams have offenses with fast pace and consistent 3-point shooting as their focus, while other teams might have a more center dominant offense, and there are also differences in defensive styles. We're aiming to understand how these styles (which we refer to as archetypes) perform against each other, which offers insights into matchup dynamics and competitive advantages in certain matchups.

We use K-Means clustering to group the Power 5 teams (ACC, Big Ten, Big 12, Pac-12, and SEC; 70 total teams) into archetypes based on advanced performance metrics such as offensive and defensive efficiency, effective field goal percentage, pace, etc. After identifying these archetypes, we analyze their performance in matchups to explore whether certain styles consistently outperform others. The seasons covered are 2019-2024.

The full write-up is in [src/FinalPaper.pdf](src/FinalPaper.pdf) (source: [src/FinalPaper.Rmd](src/FinalPaper.Rmd)).

## Data Sources
The data combines multiple datasets focusing on Power 5 conference teams across 6 seasons (2019-2024).

- **KenPom.com** advanced metrics, which go beyond traditional box score numbers:
  - Team Advanced Metrics: the "Four Factors" (effective field goal percentage, turnover rate, offensive rebounding percentage, and free throw rate) for both offense and defense.
  - Efficiency and Tempo: each team's adjusted offensive and defensive efficiency ratings, along with pace metrics that describe how quickly teams play.
  - Player and Team Attributes: height data, experience levels, and bench utilization statistics.
  - Point Distribution Data: breakdowns of how teams score their points (free throws, 2-pointers, 3-pointers).
- **[ncaahoopR](https://github.com/lbenz730/ncaahoopR)**, an R package that provides access to NCAA basketball game results data. Regular season game results (scores, location, and dates) allow us to analyze performance between teams of different archetypes.

The final dataset includes 400+ team-seasons, 45 performance metrics per team, and over 11,000 games with Power 5 teams. Raw files live in [src/data/](src/data/).

## Project Structure
```
CBB-Project/
├── CBB-Project.Rproj
├── README.md
└── src/
    ├── FinalPaper.Rmd / FinalPaper.pdf   # Final paper (clustering, matchup analysis, results)
    ├── data_analysis.Rmd                 # Joining/cleaning of the KenPom files
    ├── merge_datasets.Rmd                # Dataset merging
    ├── ncaahoopR_sample.Rmd              # Scraping Power 5 game results from ncaahoopR
    ├── analysis.Rmd                      # Exploratory analysis
    ├── variableDescriptions.docx         # Description of the variables
    ├── data/                             # Raw and merged data (merged_data.csv, gameResults.csv, ...)
    ├── *.csv, *.rds                      # Clustering outputs (scaled data, perfOverExpect, margin, ...)
    └── CBB_Archetype_Dashboard/          # Interactive Shiny dashboard
        ├── app.R
        ├── functions.R
        └── *.csv, *.rds                  # Data the dashboard loads
```

## Analysis Methods
**Data processing.** Each KenPom file was joined so there is one row for every team-season pair, with all corresponding advanced metrics. We merged 6 seasons of data for each metric category, filtered to teams in the Power 5 conferences, renamed columns to distinguish offensive and defensive metrics, and joined the separate metric categories using team names and seasons as matching keys. Game results were scraped from the ncaahoopR repository (see `ncaahoopR_sample.Rmd`).

**Separate offensive and defensive clustering.** A basketball game between Team 1 and Team 2 involves two distinct matchups: Team 1's offense versus Team 2's defense, and Team 1's defense versus Team 2's offense. Because each pairing of full archetype sets occurs relatively infrequently, there isn't enough data to analyze each one individually. We therefore individually cluster teams into offensive and defensive clusters based on their offensive and defensive related metrics. This reduces the number of features within each clustering iteration, reducing the effect of the curse of dimensionality, and improving the performance of K-Means. The trade-off is that instead of a single metric to evaluate a game matchup, we obtain two separate metrics that quantify the two offense-versus-defense matchups within a game.

**Choosing K.** K-Means cannot infer the number of clusters from unlabeled data, so we used the elbow method: plotting within cluster sum of squared errors for a range of K values and choosing the 'elbow', the point where the decrease in error significantly slows. This gives a K that is close to minimizing error without overfitting.

**Performance over expectation (POE).** Using game results and KenPom net ratings, we build an offensive-cluster by defensive-cluster matrix (`perfOverExpect`) showing how each offensive archetype performs relative to expectation against each defensive archetype. A win percentage matrix is included for supplementary purposes.
- *Offensive favorability:* read the row for the team's offensive cluster. Positive values mean the cluster outperforms expectations against that defensive cluster.
- *Defensive favorability:* read the column for the team's defensive cluster. Negative values mean the defense holds that offensive archetype below expectation, i.e. performs better than expected.

**Defining characteristics of a cluster.** Since each statistic is normalized, it has a variance of 1 across all teams. For each cluster we compute *Variance Reduction = 1 - variance of that statistic for teams within the cluster*. Statistics with a large reduction are 'defining characteristics' (shared traits of the cluster, not necessarily strengths). Polygon (spider) plots then compare the average standardized values of statistics with a variance reduction of 0.25 or greater to the rest of the dataset, which lets us name archetypes, e.g. offensive cluster 1, "Sloppy Slashers", which relies on free throws but turns the ball over and is inefficient overall.

## Interactive Dashboard
We created an interactive Shiny dashboard that allows a more clear and holistic visualization for matchup analysis and identification of 'defining characteristics' for the various offensive and defensive clusters.

Located in `src/CBB_Archetype_Dashboard/`, it can be run by opening `app.R` in RStudio and executing the script (working directory must be the dashboard folder), or from R:

```r
shiny::runApp("src/CBB_Archetype_Dashboard")
```

Required packages: `shiny`, `shinydashboard`, `tidyverse`, `ggplot2`, `DT`. The paper itself additionally uses `gridExtra`, `fmsb`, `tibble`, `purrr`, and `reshape2`.

## Results
The process of clustering college basketball teams into offensive and defensive clusters provided several deliverables:
- An archetype matchup 'matrix' that acts as a basis for understanding how any defined offensive archetype performs (over expectation) against each defensive archetype.
- Emphasis on identifying the defining characteristics of each cluster, allowing a deeper understanding of how teams in college basketball are differentiated.
- A visualization dashboard that acts as an interface for fans, analysts, and team faculty alike to understand the breadth of the analysis.

Purely numerical grouping of teams using advanced metrics yields groupings that make sense in the context of basketball. Factors including overall efficiency, pace of play, scoring distribution, experience, and shot usage define archetypes that are similar to the perceived groupings of team styles in college basketball (for example, "Elite Three Level Offense", "Paint Protectors", or "Elite Turnover Forcers").

Fans and analysts can make statistically supported claims about favorable or unfavorable matchups for a specific team, and teams can use the same insights for team building and recruiting, development focuses, play style changes, and scouting and game planning for opponents.

## Future Work
- **Modeling:** a more thorough model selection process (beyond K-Means) and variable selection to more solidly justify the number of clusters.
- **Cluster similarity quantification:** use the distance of each team from every cluster center to understand teams that are a blend of two or more archetypes.
- **Cluster matchup win simulation:** bootstrapping and Monte Carlo simulation to determine true expected results of matchups between two archetypes.
- **Team archetype persistence:** quantify how much teams change in archetype from year to year, possibly leading to a better understanding of recruiting strategies and program philosophies.

## Contributors
- Seth Jernigan ([SethJ50](https://github.com/SethJ50))
- Ian Stewart
- Daniel Lott
- Michael Masterton
- Michael Borg
