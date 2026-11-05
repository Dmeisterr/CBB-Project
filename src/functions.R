makeVarDf <- function(clusterN, scaled_df) {
  # Make data frame of variance reductions for clusterN
  
  # Step 1: Grab a single clusters data
  clusterNScaled <- scaled_df %>% 
    filter(FullCluster == clusterN) %>% 
    select(-FullCluster, -TeamName, -Season)
  
  # Step 2: Calculate Centered Variance for each feature
  cNVar <- 1 - sapply(clusterNScaled, var)
  
  # Step 3: Sort the variances
  cNVar_sorted <- sort(cNVar, decreasing = FALSE)
  
  # Step 4: Convert to data frame for plotting
  var_df <- data.frame(
    feature = factor(names(cNVar_sorted), levels = names(cNVar_sorted)),
    variance = as.numeric(cNVar_sorted)
  )
  
  return(var_df)
}

varReductionPlot = function(clusterN, scaled_df) {
  # Var. Reduction Analysis for identification of Important Cluster Features
  # If there is a large variance reduction of a feature from the variance throughout
  # all observations to the variance of the feature within a cluster, then we define
  # that statistic to be a 'substantial variance reducing feature'
  
  # Make data frame of variance reductions for clusterN
  var_df <- makeVarDf(clusterN, scaled_df)
  
  # Plot in Decreasing variance reduction
  defVarRed <- ggplot(var_df, aes(x = feature, y = variance)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    theme_minimal() +
    ylab("Variance") +
    xlab("Feature") +
    ggtitle(paste("Feature Variance Decreases (Cluster", clusterN, ")", sep = " "))+
    scale_y_continuous(labels = scales::comma) +
    coord_flip() +
    theme(
      axis.text.y = element_text(
        size = 10,
        angle = 0,
        hjust = 0,
        vjust = 0.5,
        margin = margin(b = 15)
      ),
      axis.text.x = element_text(angle = 0, hjust = 1) # Ensure x-axis labels are readable
    )
  
  return(defVarRed)
}

makeCompTable <- function(clusterN, scaled_df){
  # Make table that compares stats within cluster to out of cluster
  ## NOTE: Change scaled_df to {offensive,defensive}Data for true values
  
  # Make data frame of variance reductions for clusterN
  var_df <- makeVarDf(clusterN, scaled_df)
  
  # Step 4: Convert to data frame for plotting
  var_df_sorted <- var_df %>% 
    arrange(-variance) %>% 
    filter(variance > 0.25)
  
  # Stats with significantly reduced variance
  var_reduced_stats <- var_df_sorted$feature
  
  # Every team's reduced variance stats, with their cluster
  var_reduced_values <- scaled_df %>% 
    select(all_of(var_reduced_stats), FullCluster)
  
  # Current cluster's reduced variance stats (team by team)
  clusterNValues <- var_reduced_values %>% 
    filter(FullCluster == clusterN)
  
  # Non-current cluster's reduced variance stats (team by team)
  nonClusterNValues <- var_reduced_values %>% 
    filter(FullCluster != clusterN)
  
  # Average of stats within cluster
  clusterNAvgs <- clusterNValues %>% 
    group_by(FullCluster) %>% 
    summarise(
      across(everything(), mean, na.rm = TRUE)
    )
  
  # Average of stats outside of cluster
  nonClusterNAvgs <- nonClusterNValues %>% 
    summarise(
      across(-FullCluster, mean, na.rm = TRUE)
    )
  
  variance_reductions <- as.data.frame(t(setNames(var_df_sorted$variance, var_df_sorted$feature)))
  
  clusterNTitle <- paste("Cluster", clusterN, sep = " ")
  nonClusterNTitle <- paste("Non Cluster", clusterN, sep = " ")
  
  # Comparison table: in cluster vs. out of cluster values, variance reductions
  comparisonTable <- bind_rows(
    clusterNAvgs %>% select(-FullCluster) %>% mutate(Type = clusterNTitle),
    nonClusterNAvgs %>% mutate(Type = nonClusterNTitle),
    variance_reductions %>% mutate(Type = "Variance Reduction")
  ) %>% 
    select(Type, everything())
  
  # Row that is difference of in cluster vs. out of cluster values
  difference_row <- comparisonTable %>%
    filter(Type == clusterNTitle) %>%
    select(-Type) %>%
    as.data.frame() %>%
    `-`(comparisonTable %>% filter(Type == nonClusterNTitle) %>% select(-Type) %>% as.data.frame())
  
  # Bind all rows together
  comparisonTable <- bind_rows(
    comparisonTable,
    difference_row %>% mutate(Type = "Difference")
  ) %>% 
    mutate(across(where(is.numeric), ~ round(.x, 2)))
  
  return(comparisonTable)
}

diffMeansHt <- function(clusterN, originalData, scaled_df){
  # Set Significance Level to 0.1
  # We define a feature as a 'defining feature' of a cluster if the feature is a
  # 'substantial variance reducing feature' with a statistically significant difference
  # in means between the data for that feature within that cluster, and the data for
  # that feature for all other clusters...
  # NOTE: This logic may be flawed when the average cluster is very close to all avg.
  #   but still has big variance reduction...
  
  var_df <- makeVarDf(clusterN, scaled_df)
  
  var_df_sorted <- var_df %>% 
    arrange(-variance) %>% 
    filter(variance > 0)
  
  var_reduced_stats <- var_df_sorted$feature
  
  var_reduced_values <- originalData %>% 
    select(all_of(var_reduced_stats), FullCluster)
  
  clusterNData <- var_reduced_values %>% 
    filter(FullCluster == clusterN)
  
  notClusterNData <- var_reduced_values %>% 
    filter(FullCluster != clusterN)
  
  var_reduced_values <- var_reduced_values %>% 
    select(-FullCluster)
  
  hypTestsResults <- data.frame(
    feature = character(),
    p_value = numeric(),
    t_stat = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (feature in var_reduced_stats) {
    # Get the data for the feature
    clusterN_values <- clusterNData[[feature]]
    notClusterN_values <- notClusterNData[[feature]]
    
    # Perform a t-test
    t_test_result <- t.test(clusterN_values, notClusterN_values)
    
    # Store the result (Feature, p-value, t-statistic)
    hypTestsResults <- hypTestsResults %>%
      add_row(
        feature = feature,
        p_value = round(t_test_result$p.value, 3),
        t_stat = round(t_test_result$statistic, 2)
      )
  }
  
  return(hypTestsResults)
}

makePolyPlot <- function(clusterN, scaled_df) {
  radarData <- makeCompTable(clusterN, scaled_df) %>% 
    filter(Type %in% c(paste("Cluster", clusterN), paste("Non Cluster", clusterN))) %>% 
    rename(group = Type)
  
  spiderPlot <- ggspider(radarData, grid.min = -2, grid.max = 2)
  
  return(spiderPlot)
}

makePolyPlot <- function(clusterN, scaled_df) {
  # Make Polygon Plot for visual in cluster vs. out of cluster comparison
  
  comparisonTable <- makeCompTable(clusterN, scaled_df)
  
  clusterNTitle <- paste("Cluster", clusterN, sep = " ")
  nonClusterNTitle <- paste("Non Cluster", clusterN, sep = " ")
  
  var_df <- makeVarDf(clusterN, scaled_df)
  
  var_df_sorted <- var_df %>% 
    arrange(-variance) %>% 
    filter(variance > 0)
  
  var_reduced_stats <- var_df_sorted$feature
  
  # Should use def_scaled_df above to make comparisonTable for this step!
  comparison_data_long <- comparisonTable %>%
    filter(Type != "Variance Reduction", Type != "Difference") %>%
    pivot_longer(cols = -Type, names_to = "Feature", values_to = "Value") %>%
    mutate(
      Group = case_when(
        Type == clusterNTitle ~ clusterNTitle,
        Type == nonClusterNTitle ~ nonClusterNTitle
      ),
      Feature = factor(Feature, levels = var_reduced_stats)  # Ensure proper ordering of features
    ) %>% 
    select(-Type)
  
  def_poly_plot <- ggplot(comparison_data_long, aes(x = Feature, y = Value, group = Group, color = Group)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    coord_polar(start = 0) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = -1,
        vjust = -1,
        margin = margin(t = 10)
      ),
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "top"
    ) +
    labs(
      title = paste("Comparison of Cluster", clusterN, "vs Non Cluster", clusterN, "Features", sep = " "),
      subtitle = "Polygon Plot (Radar/Spider Chart)",
      x = "Features",
      y = "Values"
    )
  
  return(def_poly_plot)
}

listTeamsInCluster <- function(clusterN, originalData){
  # List the teams in clusterN
  
  teamsClusterN <- originalData %>% 
    filter(FullCluster == clusterN) %>% 
    select(TeamName, Season)
  
  teamsClusterN
}

getTeamRating <- function(teamName, season, netratings) {
  ratingRow <- netratings %>% 
    filter(TeamName == teamName, Season == season)
  
  return(ratingRow$AdjEM)
}

getMarginOverExpected <- function(teamName, opponentName, gameSeason, netratings, gamesData) {
  underscoreTeam <- gsub(" ", "_", teamName)
  underscoreTeam <- recode(underscoreTeam, !!!teamConversions)
  
  # Grab game(s) with these two teams
  gameRow <- gamesData %>% 
    filter(team == underscoreTeam, opponent == opponentName, season == gameSeason)
  
  if(nrow(gameRow) == 0) {
    errStr <- paste("ERROR: Couldn't find game between", teamName, " and ", opponentName, " season ", gameSeason)
    print(errStr)
    return(NA)
  }
  
  # Get KenPom Adj Efficiency Margin Ratings
  teamRtg <- getTeamRating(teamName, gameSeason, netratings)
  oppRtg <- getTeamRating(opponentName, gameSeason, netratings)
  
  margins <- c()
  for(i in 1:nrow(gameRow)){ # For each game
    
    # Get Team Score, Opponent Score, and Location
    teamScore <- gameRow$team_score[i]
    oppScore <- gameRow$opp_score[i]
    location <- gameRow$location[i]
    
    # Get Home Court Advantage Value
    homeCourtAdvantage <- 0
    if(location == "H") {
      homeCourtAdvantage <- 3
    } else if (location == "A") {
      homeCourtAdvantage <- -3
    }
    
    # Create Team Margin Over Expected
    actualMargin <- teamScore - oppScore
    
    print(paste("           Team Rtg: ", teamRtg, " Opp Rtg: ", oppRtg))
    
    expectedMargin <- teamRtg - oppRtg + homeCourtAdvantage
    
    print(paste("           Actual Margin: ", actualMargin, " Expected Margin: ", expectedMargin))
    
    marginOverExpected <- actualMargin - expectedMargin
    
    margins <- c(margins, marginOverExpected)
  }
  
  return(margins)
}


