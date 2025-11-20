

# 0. DATA PREPARATION =========

## (1) Load required packages --------

if(T) {
  
  rm(list=ls())
  
  library(openxlsx)
  library(tidyverse)
  # library(dplyr)
  library(compareGroups)
  library(lavaan)
  library(semTools) 
  library(blandr)
  # library(tidyr)
  library(ggplot2)
  library(gridExtra)
  library(purrr)
  library(ltm)
  library(mice)
  library(psych)
  library(mirt)
  library(scales)
}


if(T) {
  
  
  ### ---------- a. Variable Name ----------
  
  if(T) {
    
    gold_name <- read.xlsx("./Table_1_scale_characteristics.xlsx") 
    base_name <- read.xlsx('./Table_2_demo_characteristics.xlsx') 
    game_name <- read.xlsx('./Table_3_task_characteristics.xlsx')[,4]
    
  }
  
  
  ### ---------- b. Baseline + Gold Standard Data----------
  
  if(T) {
    
    dt <- 
      read.csv("./Table_4_stimulated_game and scale data.csv") %>%
      # read.xlsx(
      #   './Table_4_stimulated_game_and_scale_data.xlsx',
      #   na.strings = "NA",
      # ) %>%
      mutate(Age = as.integer(Age)) 
    
    # Extract baseline demographic variables
    baseline <- 
      dt %>%
      dplyr::select(base_name$Variable)
    
    # Extract gold standard assessment variables
    gold <- 
      dt %>% 
      dplyr::select("user_id","Age",gold_name$Variable[-(1:9)])
    
    # Extract ADHD-RS variables       
    ADHD <-
      gold %>%
      dplyr::select("user_id","Age",contains("ADHD"))
  }
  
  ### ---------- c. Game Performance Data ----------
  
  if(T) {
    
    game_data <- 
      read.csv('./Table_5_stimulated_game_data.csv') %>%
      mutate(
        games = case_when(
          practice_id == 120 ~ "Feed",
          practice_id == 115 ~ "Fish",
          practice_id == 122 ~ "Pets",
          practice_id == 123 ~ "Card",
          practice_id == 105 ~ "Biscuit",
          practice_id == 114 ~ "Magical",
          TRUE ~ "Other"
        )
      )
    
    
    # Extract test data
    test_data <-
      game_data %>%
      arrange(user_id, practice_id, games) %>%
      group_by(user_id, games) %>%
      slice(1) %>% 
      ungroup()
    
    test_data %>%
      group_by(games) %>%
      summarise(
        distinct_users = n_distinct(user_id),
        completion_rates = round(distinct_users/259, 3)*100, 
        .groups = 'drop'
      ) %>%
      print()
    
    
    # Extract retest data
    retest_data <-
      game_data %>%
      filter(user_id %in% dt$user_id[dt$retest==1]) %>%
      arrange(user_id, practice_id, games) %>%
      group_by(user_id, games) %>%
      slice(2) %>% 
      ungroup()
    
    retest_data %>%
      group_by(games) %>%
      summarise(
        distinct_users = n_distinct(user_id),
        completion_rates = round(distinct_users/162, 3)*100, 
        .groups = 'drop'
      ) %>%
      print()
    
  }
  
} 


## (2) TASK SCORES CONSTRUCTION ========

if(T) {
  
  
  ### ------- a. IRT Model ---------
  if(T) {
    
    
    # irt model function 
    irtfun <- function(data, task_name){
      
      # params: 
      #   data: game data
      #   task_name: task name
      # returns:
      #   model
      
      dt <- 
        data %>%
        dplyr::filter(games == task_name) %>%
        dplyr::distinct(user_id, .keep_all = TRUE) %>%
        dplyr::select('user_id','is_correct') 
      
      if(task_name %in% c("Pets","Card")){
        
        dt <- 
          dt %>%
          mutate(is_correct = sub('(\\([^)]*\\)).*', '\\1', is_correct)) %>%
          filter(is_correct!='()')
        
      }  
      
      dt_irt <- 
        dt %>%
        mutate(cleaned = gsub('[()]', '', is_correct)) %>%
        mutate(chars = strsplit(cleaned, ',')) %>%
        unnest_wider(chars, names_sep = "") %>%
        rename_with(.cols = starts_with("chars"), ~ paste0("try", seq_along(.x))) %>%
        mutate(across(starts_with("try"), ~ as.numeric(.x))) %>%
        dplyr::select(-cleaned, -is_correct)      
      
      
      if(task_name == 'Magical'){
        dt_irt <- 
          dt_irt %>%
          mutate(across(everything(), ~ replace_na(., 0))) #%>%
          #dplyr::select(1:9)
      }      
      
      dt_irt_1 <- dt_irt[,-1]     
      
      # Fit IRT model (2PL)     
      irt_model <- ltm(dt_irt_1 ~ z1)
      
      # Return model
      return(irt_model)
    }
    
    
    # irt score function 
    irt_score <- function(irt_model, task_name, data) {
      
      dt <- 
        data %>%
        dplyr::filter(games == task_name) %>%
        dplyr::distinct(user_id, .keep_all = TRUE) %>%
        dplyr::select('user_id','is_correct') 
      
      if(task_name %in% c("Pets","Card")){
        
        dt <- 
          dt %>%
          mutate(is_correct = sub('(\\([^)]*\\)).*', '\\1', is_correct)) %>%
          filter(is_correct!='()')
        
      }  
      
      dt_irt <- 
        dt %>%
        mutate(cleaned = gsub('[()]', '', is_correct)) %>%
        mutate(chars = strsplit(cleaned, ',')) %>%
        unnest_wider(chars, names_sep = "") %>%
        rename_with(.cols = starts_with("chars"), ~ paste0("try", seq_along(.x))) %>%
        mutate(across(starts_with("try"), ~ as.numeric(.x))) %>%
        dplyr::select(-cleaned, -is_correct)      
      
      dt_irt_1 <- dt_irt[,-1] 
      
      irt_scores <- ltm::factor.scores(irt_model, dt_irt_1, method = "EAP")
      
      
      dt_irt$SCORE <- irt_scores$score.dat$z1
      colnames(dt_irt)[colnames(dt_irt) == "SCORE"] <- task_name
      irt_score <- dt_irt[c("user_id", task_name)]
      
      return(irt_score)
    } 
    
    
    # fit IRT models for all tasks
    irt_models <- 
      lapply(
        game_name,
        function(task_name) {
          irtfun(
            data = test_data, 
            task_name = task_name 
          )
        }
      )
    
    names(irt_models) <- game_name
    
    
    # Display model parameters and fit statistics
    for (i in seq_along(irt_models)) {
      irt_model <- irt_models[[i]]
      cat("Game:", names(irt_models)[i], "\n")
      cat("Model coefficients:\n")
      print(irt_model$coef)
      cat("\n")
    }
    
    
  }
  
  
  ### ------- b. Calculate Test Information ---------
 
    
  if(T) {
    
    # Define item information function for 2PL model
    calculate_item_info <- function(theta, a, b) {
      
      # 2PL
      prob_correct <- 1 / (1 + exp(a * (theta - b)))
      info <- a^2 * (1 - prob_correct) * prob_correct
      
      # 3PL
      #prob_correct <- c + (1 - c) /  (1 + exp(-a * (theta - b))) 
      #info <- (a^2 * (1 - prob_correct)^2) / (prob_correct)*(1 - c)^2   
      return(info)
    }
    
    
    # Define test information function
    calculate_tif <- function(coef, ability_range) {
      
      # Calculate information for each item
      item_info <- apply(coefs, 1, function(item) {
        a <- item["Dscrmn"]  # Discrimination parameter
        b <- item["Dffclt"]  # Difficulty parameter
        #c <- item["Gussng"] # Guessing parameter
        information <- sapply(ability_range, calculate_item_info,a=a,b=b) 
        return(information)
      })
      
      # Sum item information to get test information
      tif_values <- rowSums(item_info)
      names(tif_values) <-c(paste("info_at_ability"))
      
      return(tif_values)
    }
    
    
    # Define ability range for information functions 
    theta_values <- seq(-4, 4, length.out = 100) 
    
    # Calculate information functions for all tasks
    plot_data <- data.frame()
    
    for (i in 1:length(irt_models)) {
      
      irt_model <- irt_models[[i]]
      coefs <- round(coef(irt_model),3)
      
      tif_values <- calculate_tif(coef = coefs, ability_range = theta_values) 
      task_curve <- data.frame(
        ability = theta_values, 
        information = tif_values, 
        tasks = names(irt_models)[i]
      )
      
      plot_data <- rbind(plot_data, task_curve)
    }
    
    
    plot_data <-
      plot_data %>%
      mutate(tasks = factor(tasks, levels = unique(tasks)))
    
    p <- ggplot(
      data = plot_data, 
      mapping = aes(x = ability, y = information, color = tasks)
    ) +
      geom_line(linewidth = 0.8) +
      labs(
        # title = 'Test Information Function (TIF) for tasks', 
        x = 'IRT Based Scores', 
        y = 'Information',
        color = "Tasks" 
      ) +
      ggthemes::theme_few() +
      scale_color_manual(
        values = c("#e64b35","#F0E442","#4daf4a","#56B4E9","#0072B2","#CC79A7")
      ) +
      theme(
        plot.title = element_text(
          size = 12, 
          face = "bold",
          color = "black"
        ),
        text = element_text(
          size = 12, 
          face = "plain", 
          color = "black"
        ),
        axis.title = element_text(
          size = 12, 
          face = "bold", 
          color = "black"
        ),
        axis.text = element_text(
          size = 12, 
          face = "plain", 
          color="black"
        ),
        legend.title = element_text(
          size = 10, 
          face = "bold", 
          color = "black"
        ),
        legend.text = element_text(
          size = 10, 
          face = "plain", 
          color = "black"
        ),
        legend.background = element_rect(
          color = "black",  
          fill = "white",   
          linewidth = 0.4      
        )
      )
    
    print(p)
  }
    
  feed <- irt_score(irt_models$Feed, "Feed", test_data)
  biscuit <- irt_score(irt_models$Biscuit, "Biscuit", test_data)
  fish <- irt_score(irt_models$Fish, "Fish", test_data)
  magical <- irt_score(irt_models$Magical, "Magical", test_data)
  pets <- irt_score(irt_models$Pets, "Pets", test_data)
  card <- irt_score(irt_models$Card, "Card", test_data)
  
  datasets <- list(feed, fish, pets, card, biscuit, magical)
  irt_SCORE <- Reduce(function(x, y) merge(x, y, by = "user_id", all.x = TRUE), datasets)
 
  sum <- summary(irt_SCORE[,-1])
  print(sum)
  
  result <- list(irt_SCORE = irt_SCORE, SUMMARY = sum)
  
  wb <- createWorkbook()
  for (name in names(result)) {
    addWorksheet(wb, name)
    writeData(wb, sheet = name, result[[name]], rowNames = TRUE)
  }
  
  saveWorkbook(wb, 'IRT_SCORE.xlsx', overwrite = TRUE)
  
  
  
  feed_re <- irt_score(irt_models$Feed, "Feed", retest_data)
  biscuit_re <- irt_score(irt_models$Biscuit, "Biscuit", retest_data)
  fish_re <- irt_score(irt_models$Fish, "Fish", retest_data)
  magical_re <- irt_score(irt_models$Magical, "Magical", retest_data)
  pets_re <- irt_score(irt_models$Pets, "Pets", retest_data)
  card_re <- irt_score(irt_models$Card, "Card", retest_data)
  
  datasets_re <- list(feed_re, fish_re, pets_re, card_re, biscuit_re, magical_re)
  RE_irt_SCORE <- Reduce(function(x, y) merge(x, y, by = "user_id", all.x = TRUE), datasets_re)
  
  
  result <- list(RE_irt_SCORE = RE_irt_SCORE, SUMMARY = summary(RE_irt_SCORE[,-1]))
  
  wb <- createWorkbook()
  for (name in names(result)) {
    addWorksheet(wb, name)
    writeData(wb, sheet = name, result[[name]], rowNames = TRUE)
  }
  
  saveWorkbook(wb, 'RE_IRT_SCORE.xlsx', overwrite = TRUE)
  
}


# 1. DATA DESCRIPTIVE ========= 
  
## (1) Table1 --------
if(T) {
  
  data <- 
    dt %>%
    inner_join(., irt_SCORE, by = "user_id")
  
  res01 <- descrTable(retest ~.,
                      show.p.overall = T,
                      show.all = F,
                      method = c(paste(paste0(game_name,'=NA',collapse = ','),paste0(gold_name[,1],'=NA',collapse = ','),sep = ',' )),
                      data = data ); res01
  
  res02 <- descrTable(Age_new ~.,
                      show.p.overall = T,
                      show.all = T,
                      method = c(paste(paste0(game_name,'=NA',collapse = ','),paste0(gold_name[,1],'=NA',collapse = ','),sep = ',' )),
                      data = data) ;res02
  
  export2xls(res01,'TABLE1.xlsx')
  export2xls(res02,'TABLE2.xlsx')
  
  
  a <- descrTable( Age ~.,show.all = T, data = data)
  export2xls(a,'summary.xlsx')
  export2html(a,'summary.html')
  
}


## (2) Distribution  --------
if(T) {
  
  plot_data1 <- 
    irt_SCORE %>%
    pivot_longer(
      data = .,
      cols = Feed:Magical,
      names_to = "Variable",
      values_to = "Value"
    ) %>%
    mutate(Variable = factor(Variable, levels=unique(Variable)))
  
  
  p1 <- ggplot(
    data = plot_data1, 
    mapping = aes(x = Variable, y = Value, fill = Variable)
  ) +
    geom_boxplot(
      color = "#22292F", 
      alpha = .8
    ) +
    labs(
      x = 'EF Tasks', 
      y = 'IRT Based Scores',
      fill = "Tasks" 
    ) +
    ggthemes::theme_few()+
    scale_fill_manual(
      values = c("#e64b35","#F0E442","#4daf4a","#56B4E9","#0072B2","#CC79A7")
    ) +
    theme(
      plot.title = element_text(
        size = 12, 
        face = "bold",
        color = "black"
      ),
      text = element_text(
        size = 12, 
        face = "plain", 
        color = "black"
      ),
      axis.title = element_text(
        size = 12, 
        face = "bold", 
        color = "black"
      ),
      axis.text = element_text(
        size = 12, 
        face = "plain", 
        color="black"
      ),
      legend.title = element_text(
        size = 10, 
        face = "bold", 
        color = "black"
      ),
      legend.text = element_text(
        size = 10, 
        face = "plain", 
        color = "black"
      ),
      legend.background = element_rect(
        color = "black",  
        fill = "white",   
        linewidth = 0.4      
      )
    )
  
  print(p1)
  # ggsave("Figure_2_IRT_scores_for_tasks.png", plot = p1, width = 10, height = 8)       
  
}
  

# 2. VALIDITY ANALYSIS ========= 

## (1) Construct validity --------

  if(T) {
    
    #-----------------#
    # Fill missing values (mice) + CFA
    #-----------------#
    
    imp <- mice(irt_SCORE, m = 5, method = 'pmm', maxit = 50, seed = 500)
    
    # Obtain 5 parameters from the 5 datasets imputed by MICE
    CFL_list <- c()
    TLI_list <- c()
    CFL_var_list <- c()
    TLI_var_list <- c()
    
    imputed_list <- list() #
    
    for (i in 1:5) {
      
      complete_data <- imputed_list[[i]] <-  complete(imp, action = i)
    
      form = 'IC=~Feed+Fish
              WM=~Card+Magical
              FS=~Pets+Biscuit'
      
      fit <- cfa(form,data = complete_data, std.lv=TRUE, estimator = 'WLS')
      CFL_list <- c(CFL_list, fitMeasures(fit, "cfi"))
      TLI_list <- c(TLI_list, fitMeasures(fit, "tli"))
      CFL_var_list <- c(CFL_var_list, lavInspect(fit, "vcov")[1,1])  # CFL 
      TLI_var_list <- c(TLI_var_list, lavInspect(fit, "vcov")[2,2])  # TLI
    }
    
    
    # Combine 5 parameters using Rubin's rule
    Q_bar_CFL <- mean(CFL_list)
    U_bar_CFL <- mean(CFL_var_list)
    B_CFL <- var(CFL_list) * (length(CFL_list) - 1) / length(CFL_list)
    T_CFL <- U_bar_CFL + (1 + 1/length(CFL_list)) * B_CFL
    SE_CFL <- sqrt(T_CFL)
    
    # Combine TLI using Rubin's rule
    Q_bar_TLI <- mean(TLI_list)
    U_bar_TLI <- mean(TLI_var_list)
    B_TLI <- var(TLI_list) * (length(TLI_list) - 1) / length(TLI_list)
    T_TLI <- U_bar_TLI + (1 + 1/length(TLI_list)) * B_TLI
    SE_TLI <- sqrt(T_TLI)
    
    # Output combined CFL and TLI with their standard errors
    combined_CFL <- Q_bar_CFL
    combined_CFL_SE <- SE_CFL
    
    combined_TLI <- Q_bar_TLI
    combined_TLI_SE <- SE_TLI
    
    list(combined_CFL = combined_CFL, combined_CFL_SE = combined_CFL_SE,
         combined_TLI = combined_TLI, combined_TLI_SE = combined_TLI_SE)
    
    user_id <- imputed_list[[1]][, 1]
    numeric_columns_list <- lapply(imputed_list, function(df) df[, -1])
    numeric_avg <- as.data.frame(Reduce("+", numeric_columns_list) / length(numeric_columns_list))
    
    irt_SCORE_fill <- cbind(user_id, numeric_avg)
    colnames(irt_SCORE_fill)[1] <- colnames(irt_SCORE)[1]
    
    
    #-----------------#
    #----don't need fit
    #-----------------#
    CFA_dt <- irt_SCORE_fill
    cor_matrix <- cor(CFA_dt[,-1], use = "pairwise.complete.obs")
    FS <- psych::alpha(CFA_dt [,c(4,6)])
    IC <- psych::alpha(CFA_dt [,c(2,3)])
    WM <- psych::alpha(CFA_dt [,c(5,7)])
    
    
    #--------------#
    #------model fit
    #--------------#
    
    form_1f = paste('F1', paste0(names(CFA_dt)[2:7],collapse = '+' ),sep = '=~')
    fit_1f <- cfa(form_1f, data = CFA_dt, std.lv=TRUE, estimator = 'WLS')
    
    form_3f = 'IC=~Feed+Fish
               WM=~Magical+Card
               FS=~Pets+Biscuit'
    fit_3f <- cfa(form_3f,data = CFA_dt, std.lv=TRUE, estimator = 'WLS')
    
    
    #------------------------#
    #------result and evaluate
    #------------------------#
    
    # model evaluate
    summary(fit_1f, fit.measures=TRUE, standardized=TRUE)[['pe']]
    summary(fit_3f, fit.measures=TRUE, standardized=TRUE)[['pe']]
    
    eva_1f <- fitMeasures(fit_1f,c("chisq","df","rmsea","rmsea.ci.lower","rmsea.ci.upper","cfi","tli","srmr"))
    eva_3f <- fitMeasures(fit_3f,c("chisq","df","rmsea","rmsea.ci.lower","rmsea.ci.upper","cfi","tli","srmr"))
    
    # Output results > table3
    out <-round( as.data.frame( rbind(eva_1f, eva_3f)),3 )
    out <- within(out,chisqdf <- paste0(chisq,'(',df,')'))
    out <- within(out,rmseaci <- paste0(rmsea,'(',rmsea.ci.lower,'-',rmsea.ci.upper,')'))
    cfa_result <- out %>% dplyr::select(9,10,6,7,8);out
    
    
    #--------------------------#
    #convergent and discriminate
    #--------------------------#   
    
    # Results / Factor loadings, item variances, covariance between latent variables
    lavaan::parameterestimates(fit_3f)  
    
    # Results / Correlation matrix between latent variables
    OUTER <- lavaan::lavInspect(fit_3f, "cov.lv")
    
    # Results / Latent level indicators: AVE, CR, Cronbach's alpha
    INNER <- semTools::reliability(fit_3f) %>% as.data.frame()
    
    # Results / Manually calculate AVE and CR for latent variables
    res <- lavaan::inspect(fit_3f, "std")
    loadings <- res$lambda  
    theta <-  diag(res$theta)
    
    
    calculate_ave <- function(loadings, theta) {
      ave_values <- numeric(ncol(loadings))
      for (i in 1:ncol(loadings)) {
        lambda_i <- loadings[, i]
        theta_i <- theta[which(loadings[, i] != 0)]
        # Calculate sum of squared factor loadings
        lambda_squared_sum <- sum(lambda_i^2)
        # Calculate sum of error variances
        theta_sum <- sum(theta_i)
        # Calculate AVE
        ave_values[i] <- lambda_squared_sum / (lambda_squared_sum + theta_sum)
      }
      return(ave_values)
    }
    ave_values <- calculate_ave(loadings, theta)
    names(ave_values) <- colnames(loadings)
    print(ave_values)
    
    CR <-  psych::alpha(CFA_dt[,-1])
    
    ### Organize results and export
    result <- list(raw_cor = cor_matrix, raw_var = res$theta,
                   cfa_model = cfa_result, 
                   latent_cor = OUTER, latent_CR_ave = INNER,
                   latent_lambda = loadings,
                   allCR = CR$total, CRdrop = CR$alpha.drop,item_all_cor = CR$item.stats)
    
    
    ### Export results _write.xlsx
    write.xlsx(result,'CFA.xlsx', rownames = TRUE, sheetName = names(result))
    
    ### Export results _writeDATA, can export sheet names
    wb <- createWorkbook()
    for (name in names(result)) {
      addWorksheet(wb, name)
      writeData(wb, sheet = name, result[[name]], rowNames = TRUE)
    }
    saveWorkbook(wb, 'CFA.xlsx', overwrite = TRUE)              
    
    
    #--------------------------#
    # explore inner consistence
    #--------------------------#  
    ### form_3f free combinations
    generate_all_combinations <- function() {
      
      # Define three dimensions
      dimensions <- c("IC", "WM", "FS")
      
      # Define six games
      games <- c("Feed", "Card", "Magical", "Pets", "Biscuit", "Fish")
      
      # All valid combinations
      valid_combinations <- list(c(1, 1, 4), c(1, 2, 3), c(1, 3, 2), c(1, 4, 1),
                                 c(2, 1, 3), c(2, 2, 2), c(2, 3, 1), c(3, 1, 2),
                                 c(3, 2, 1), c(4, 1, 1))
      
      results <- list()
      
      for (comb in valid_combinations) {
        # Randomly shuffle game order
        set.seed(Sys.time())
        shuffled_games <- sample(games, length(games))
        
        # Create model formula
        form_list <- list()
        game_index <- 1
        dim_game_map <- list()
        
        for (i in 1:3) {
          dim <- dimensions[i]
          num_games <- comb[i]
          selected_games <- paste(shuffled_games[game_index:(game_index + num_games - 1)], collapse = "+")
          form_list[[i]] <- paste(dim, selected_games, sep = "=~")
          dim_game_map[[dim]] <- selected_games
          game_index <- game_index + num_games
        }
        
        # Combine all formulas
        form_3f <- paste(form_list, collapse = "\n")
        
        # Fit model and calculate reliability
        fit_random <- cfa(form_3f, data = CFA_dt, std.lv = TRUE)
        INNER <- semTools::reliability(fit_random) %>% as.data.frame()
        
        # Modify column names of INNER
        new_colnames <- sapply(colnames(INNER), function(col) dim_game_map[[col]])
        colnames(INNER) <- new_colnames
        
        results[[paste(comb, collapse = "-")]] <- INNER
      }
      
      # Horizontally combine results
      final_result <- do.call(cbind, results)
      
      return(final_result)
    }
    
    # Generate and output results of all combinations
    all_combinations_result <- generate_all_combinations()
    print(all_combinations_result)
    #write.xlsx(all_combinations_result,'CFA_random_fit.xlsx',rownames = TRUE)
    
    
    
    #--------------------------#
    #  calculate total score
    #--------------------------#  
    #----（PCA）----#
    
    factor_scores <- lavPredict(fit_3f)
    
    # Add factor scores to original dataframe
    CFA_dt$IC_score <- factor_scores[, "IC"]
    CFA_dt$WM_score <- factor_scores[, "WM"]
    CFA_dt$FS_score <- factor_scores[, "FS"]
    
    # Standardize factor scores
    CFA_dt$z_IC <- scale(CFA_dt$IC_score)
    CFA_dt$z_WM <- scale(CFA_dt$WM_score)
    CFA_dt$z_FS <- scale(CFA_dt$FS_score)
    
    # Create dataframe for factor scores
    scores <- data.frame(IC_score = CFA_dt$IC_score, FS_score = CFA_dt$FS_score)
    
    # Create dataframe for standardized factor scores
    standardized_scores <- data.frame(z_IC = CFA_dt$z_IC, z_FS = CFA_dt$z_FS,z_WM = CFA_dt$z_WM)
    
    # Perform PCA
    pca_result <- prcomp(standardized_scores, scale = TRUE)
    # View PCA results
    summary(pca_result)
    # Get PCA loadings
    loadings <- pca_result$rotation[,1]  # 使用第一主成分的载荷
    # View loadings
    print(loadings)
    # Calculate total score
    CFA_dt$ts_check <- loadings["z_IC"] * CFA_dt$z_IC + loadings["z_FS"] * CFA_dt$z_FS + loadings["z_WM"] * CFA_dt$z_WM
    
    
    #----(IRT total score)----#
    irt_SCORE_fill$total_score_IRT <- rowMeans(irt_SCORE_fill[,game_name])
    
    #----(one-factor CFA, 3 dimensions)----#
    RE_dimen_score_CFA <- 
      CFA_dt %>%
      dplyr::select(user_id, WM_score, FS_score, IC_score)%>%
      rename(WM_card_magical=WM_score, FS_feed_flanker=FS_score, IC_biscuit_pets=IC_score)
    
    form = 'total=~FS_feed_flanker+WM_card_magical+IC_biscuit_pets'
    fit <- cfa(form, data = RE_dimen_score_CFA, std.lv=TRUE)
    lavPredict(fit) 
    # RE_dimen_score_CFA$total_score <- lavPredict(fit) 
    irt_SCORE_fill$total_score_CFA <- lavPredict(fit) 
    
    
    #----(one-factor CFA, 6 games)----#
    form = 'total=~Feed+Fish+Card+Magical+Biscuit+Pets'
    fit <- cfa(form, data = CFA_dt, std.lv=TRUE)
    lavPredict(fit) 
    irt_SCORE_fill$total_six_fit_onefac <- lavPredict(fit)
    
    
    #----(one-factor CFA, remove magical)----#
    form = 'total=~Feed+Fish+Pets+Card+Biscuit'
    fit <- cfa(form, data = CFA_dt, std.lv=TRUE)
    irt_SCORE_fill$ts_delete_magical <- lavPredict(fit)
    
    
    #----(one-factor CFA, remove card)----#
    form = 'total=~Feed+Fish+Pets+Biscuit+Magical'
    fit <- cfa(form, data = CFA_dt, std.lv=TRUE)
    irt_SCORE_fill$ts_delete_card <- lavPredict(fit)
    
    
    #----(one-factor CFA, remove card and magical)----#
    form = 'total=~Feed+Fish+Pets+Biscuit'
    fit <- cfa(form, data = CFA_dt, std.lv=TRUE)
    irt_SCORE_fill$ts_delete_cardmagical <- lavPredict(fit)
    
    
  }

## (2) Measurement invariance ---------

if(T) {
  
  group_diff <- function(data,group){
    
    
    #configural invariance（costrain factor structure）
    fit.Configural <- cfa(form_3f, data = data, group = group) #Configural Invariance
    
    #metric invariance (weak invariance)
    fit.Metric <- cfa(form_3f, data = data, group = group
                      , group.equal = c("loadings"))   #约束组间因子载荷相同，Metric Invariance
    
    #scalar invariance (strong invariance)
    fit.Scalar <- cfa(form_3f, data = data, group = group
                      , group.equal = c("loadings","intercepts"))    #载荷和截距相等，Scalar Invariance
    
    #chi-square difference test
    diff <- lavaan::lavTestLRT(fit.Configural, fit.Metric, fit.Scalar)  #anova
    return(diff)
    
  }
  
  measureinvar <- list(gender=NULL, age=NULL)
  measureinvar$gender <- group_diff(data.frame(irt_SCORE,user_gender = data$user_gender), "user_gender")
  measureinvar$age <- group_diff(data.frame(irt_SCORE,Age = data$Age), "Age") 
  
  
  write.xlsx(measureinvar,'TABLE5.xlsx')
  
  
}


## (3) Criteria validity --------
if(T) {
  
  
  # ============ Method 1: Correlation Matrix Approach 
  
  crite <- function(data) {
    
    alpha <- 0.05
    
    # Empty matrix for storing results
    significant_corr_matrix <- matrix("", nrow = ncol(data), ncol = ncol(data))
    
    # Loop through variables to calculate correlations
    for(i in 1:ncol(data)) {
      for(j in 1:ncol(data)) {
        if(i != j) {
          # Check for zero standard deviation
          sd_i <- sd(data[, i], na.rm = TRUE)
          sd_j <- sd(data[, j], na.rm = TRUE)
          if(sd_i != 0 && sd_j != 0) {
            # Calculate correlation and p-value
            cor_result <- cor.test(data[,i], data[,j], use = "pairwise.complete.obs")
            cor_val <- cor_result$estimate
            p_val <- cor_result$p.value
            
            # Format correlation with significance star
            if(!is.na(p_val) && p_val < alpha) {
              significant_corr_matrix[i,j] <- sprintf("%.2f*", cor_val)
            } else {
              significant_corr_matrix[i,j] <- sprintf("%.2f", cor_val)
            }
          } else {
            significant_corr_matrix[i,j] <- "NA"  # Handle zero variance cases
          }
        } else {
          significant_corr_matrix[i,j] <- 1  # Diagonal elements (self-correlation)
        }
      }
    }
    
    return(significant_corr_matrix)
  }
  
  data <- 
    data %>% 
    inner_join(., irt_SCORE_fill[,-c(2:7)], by="user_id")
  
  delete_magical <- cor.test(data$MA_all,data$ts_delete_magical,use = "pairwise.complete.obs")[["estimate"]] 
  delete_card <- cor.test(data$MA_all,data$ts_delete_card,use = "pairwise.complete.obs")[["estimate"]] 
  delete_cardmagical <- cor.test(data$MA_all,data$ts_delete_cardmagical,use = "pairwise.complete.obs")[["estimate"]] 
  # onefactotal <- cor.test(data$MA_all, irt_SCORE_fill$total_onefac ,use = "pairwise.complete.obs")[["estimate"]] 
  # onefactotal_delete_magical <- cor.test(data$MA_all, irt_SCORE_fill$total_onefac ,use = "pairwise.complete.obs")[["estimate"]] 
  six_onefac <- cor.test(data$MA_all,data$total_six_fit_onefac ,use = "pairwise.complete.obs")[["estimate"]] 
  
  
  cor <- cor.test(data$MA_all, data$total_six_fit_onefac ,use = "pairwise.complete.obs")
  correlation <- cor$estimate
  cilower <- cor$conf.int[1]
  ciupper <- cor$conf.int[2]
  pvalue <- cor$p.value
  caption <- paste("Correlation:", round(correlation, 2), "\n",
                   "CI:", round(cilower, 2), "-", round(ciupper, 2),"\n",
                   "")
  
  
  p <- ggplot(data, aes(x = MA_all, y = total_six_fit_onefac)) +
    geom_point(color = "#6CB2EB") +  
    theme_minimal() +  
    theme(
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),  
      axis.line = element_line(color = "black"),  
      axis.text = element_text(color = "black"),  
      axis.title = element_text(color = "black"),  
      plot.title = element_text(color = "black", size = 14, face = "bold")  
    ) +
    labs(
      title = "Scatter Plot",  
      x = "Mental age",  
      y = "TOTAL_S"   
    ) + 
    annotate( geom="text", x=70,y=-4.5,
              label = caption ,  color = "black", size = 4 ,fontface="bold" );p
  
  
  
  # ============ Method 2: Latent Variable Correlations 
  form = 'IC =~ Feed+Fish
          WM =~ Card+Magical
          FS =~ Pets+Biscuit 
          GOLD =~ MA_all'
  
  # Fit confirmatory factor analysis model
  fit_valid <- cfa(form, data.frame(irt_SCORE_fill, MA_all=data$MA_all), std.lv=TRUE)
  cor2 <- lavaan::lavInspect(fit_valid, "cov.lv")
  
  
  # Organize correlation results
  cor <- list(cor=NULL, cor_3=NULL, cor_4=NULL, cor_5=NULL, cor_latent=NULL)
  crite_data <- data[, which(colnames(data) %in% c("MA_all","gm_ma","fm_ma","ab_ma","sl_ma","sb_ma", game_name))]
  cor$cor <- crite(crite_data)
  cor$cor_latent <- cor2
  
  rownames(cor$cor) <- names(crite_data)
  colnames(cor$cor) <- names(crite_data)
  
  
  # Export results to Excel
  wb <- createWorkbook()
  for (name in names(cor)) {
    addWorksheet(wb, name)
    writeData(wb, sheet = name, cor[[name]], rowNames = TRUE)
  }
  saveWorkbook(wb, 'criteria2.xlsx', overwrite = TRUE)  
  
  
  
# 3. RELIABILITY ANALYSIS =======
  

  imp <- mice(RE_irt_SCORE, m = 5, method = 'pmm', maxit = 50, seed = 500)
  
  RE_imputed_list <- list()
  
  for (i in 1:5) {
    RE_imputed_list[[i]] <- complete(imp, action = i)
  }
  
  RE_user_id <- RE_imputed_list[[1]][, 1]
  RE_numeric_columns_list <- lapply(RE_imputed_list, function(df) df[, -1])
  
  RE_numeric_avg <- as.data.frame(Reduce("+", RE_numeric_columns_list) / length(RE_numeric_columns_list))
  
  RE_irt_SCORE_fill <- cbind(RE_user_id, RE_numeric_avg)
  colnames(RE_irt_SCORE_fill)[1] <- colnames(RE_irt_SCORE)[1]
  
  RE_irt_SCORE_fill$total_score_IRT <- rowMeans(RE_irt_SCORE_fill[,c(2:7)])
    
  colbind <- left_join(irt_SCORE_fill, RE_irt_SCORE_fill, by = "user_id")
  cols <- c(game_name, "total_score_IRT")    
  del <- c(-0.044, 0.005, 0.002, -0.119, -0.149, -0.359, -1)
  
  #----Total score correlation-----#
  cor_result <- cor.test(colbind$total_score_IRT.x, colbind$total_score_IRT.y)
  

  
  # Method 1: Pearson correlation
  relia <- function(data){
    
    result <- data.frame(matrix(ncol = 22, nrow = length(cols))) 
    colnames(result) <- c("npre", "mean(sd)-pre", "median[IQR]-pre",
                          "npost", "mean(sd)-post", "median[IQR]-post",
                          "pre_floor_count", "pre_ceiling_count", "post_floor_count", "post_ceiling_count",
                          "norm_p_alue",   
                          "cohen's D","mean-diff","t_pvalue","median-diff","w_pvalue","delta","p","del_w","del_p",
                          "ICC","ICC_p") 
    
    for (i in 1:length(cols)) {
      
      #--------------------------------------------#
      # --- Extract game variables
      #--------------------------------------------#
      pre <-  data[[paste0(cols[i], ".x")]]
      post <- data[[paste0(cols[i], ".y")]]
      
      preom <- na.omit(pre)
      postom <-  na.omit(post)
      
      #--------------------------------------------#
      # --- Descriptive statistics
      #--------------------------------------------#
      # Calculate number of non-missing values for pre and post data
      npre <- length(preom)
      npost <- length(postom)
      
      # Calculate mean, standard deviation
      meanpre <- round(mean(preom), 2) 
      sepre <- round(sd(preom), 2)
      mdpre <- paste(meanpre, "(", sepre, ")", sep = "")
      
      meanpost <- round(mean(postom), 2)
      sepost <- round(sd(postom), 2)
      mdpost <- paste(meanpost, "(", sepost, ")", sep = "")
      
      # Calculate median, interquartile range
      #--------------------------------------------#
      medpre <- round(median(preom), 2)
      quapre <- round(quantile(preom, probs = c(0.25, 0.75)), 2)
      IQRpre <- paste0(medpre, " [", diff(quapre), "]")
      
      medpost <- round(median(postom), 2)
      quapost <- round(quantile(postom, probs = c(0.25, 0.75)), 2)
      IQRpost <- paste0(medpost, " [", diff(quapost), "]")
      
      # Calculate floor and ceiling effects
      #--------------------------------------------#
      min_val <- min(data[[paste0(cols[i], ".x")]], na.rm = TRUE)
      max_val <- max(data[[paste0(cols[i], ".x")]], na.rm = TRUE)
      pre_floor_count <- sum(data[[paste0(cols[i], ".x")]] == min_val, na.rm = TRUE)-1
      pre_ceiling_count <- sum(data[[paste0(cols[i], ".x")]] == max_val, na.rm = TRUE)-1
      pre_floor_ratio <- pre_floor_count / npre *100
      pre_ceiling_ratio <- pre_ceiling_count / npre *100
      
      min_val <- min(data[[paste0(cols[i], ".y")]], na.rm = TRUE)
      max_val <- max(data[[paste0(cols[i], ".y")]], na.rm = TRUE)
      post_floor_count <- sum(data[[paste0(cols[i], ".y")]] == min_val, na.rm = TRUE)-1
      post_ceiling_count <- sum(data[[paste0(cols[i], ".y")]] == max_val, na.rm = TRUE)-1
      post_floor_ratio <- post_floor_count / npost *100
      post_ceiling_ratio <- post_ceiling_count / npost *100
      
      
      #--------------------------------------------#
      # --- Statistical tests
      #--------------------------------------------#
      
      # Test if difference between pre-post data follows normal distribution
      #--------------------------------------------#
      normp <- shapiro.test( with(data, pre - post) )[["p.value"]]
      
      
      # Test for differences between pre and post data
      #--------------------------------------------#
      # Paired t-test
      t <- t.test(pre, post, data = data,  paired = TRUE)[["estimate"]]
      tp <- t.test(pre, post, data = data,  paired = TRUE)[["p.value"]]
      cohen <- mean(pre-post)/ sd(pre-post)
      
      # Wilcoxon paired test
      w <- wilcox.test(pre, post, data = data, paired = TRUE)[["statistic"]]
      wp <- wilcox.test(pre, post, data = data, paired = TRUE)[["p.value"]]
      
      # delta T-test
      delta <- del[i]
      p <- t.test(pre-post, mu = delta, alternative = "less")[["p.value"]]
      
      
      # Test for differences between missing and non-missing data
      #--------------------------------------------#
      # Index missing rows
      data_no_na_prex <- data %>% filter(!is.na(pre))
      INDEX <- attr(na.omit( data_no_na_prex[[paste0(cols[i], ".y")]] ) ,"na.action")
      
      preNA <- preom[INDEX]
      prenotNA <- preom[!(seq_along(preom) %in% INDEX)]
      
      
      # Perform Wilcoxon test
      del_w <- wilcox_test <- wilcox.test(preNA, prenotNA)[["statistic"]]
      del_p <- wilcox_test <- wilcox.test(preNA, prenotNA)[["p.value"]]
      
      
      #--------------------------------------------#
      ICC <- irr::icc( cbind(pre,post) )[["value"]]
      ICC_p <-irr::icc( cbind(pre,post) )[["p.value"]]
      #--------------------------------------------#
      # Add results to result dataframe
      #--------------------------------------------#
      row <- c(npre, mdpre, IQRpre, npost, mdpost, IQRpost, 
               pre_floor_count, pre_ceiling_count, post_floor_count, post_ceiling_count, 
               cohen, normp, t,tp, w, wp,delta,p,del_w,del_p,
               ICC,ICC_p)
      result[i,] <- row
    }
    
    # Set column names for result dataframe
    return(result)
  }
  
  result <- list(ALL=NULL, AGE3=NULL, AGE4=NULL, AGE5=NULL)
  result$ALL <- relia(colbind)
  
  # Group correlation
  colbind_group <- left_join(colbind, data[,1:5], by = "user_id")
  relia_group <- function(data){
    
    result <- data.frame(matrix(ncol = 8, nrow = length(cols))) 
    colnames(result) <- c("GDP_cor","GDP_p",
                          "Age_cor","Age_p",
                          "Sex_cor","Sex_p",
                          "cor","p") 
    
    
    for (i in 1:length(cols)) {
      
      #--------------------------------------------#
      # --- Extract game variables
      #--------------------------------------------#
      pre <-  data[[paste0(cols[i], ".x")]]
      post <- data[[paste0(cols[i], ".y")]]
      
      preom <- na.omit(pre)
      postom <-  na.omit(post)
      
      
      # Calculate correlation for total population
      
      cor_estimate <-  irr::icc( cbind(pre,post) )[["value"]]
      p_value <-  irr::icc( cbind(pre,post) )[["p.value"]]
      
      # Calculate correlation for subgroup populations
      var1 <- data %>% filter(GDP == "HighGDP"|GDP == "MidGDP")
      ICCmodel1 <- irr::icc(cbind( var1[[paste0(cols[i], ".x")]], var1[[paste0(cols[i],".y")]]))
      GDPe <- ICCmodel1[["value"]]
      GDPp <- ICCmodel1[["p.value"]]
      
      var2 <- data %>% filter(Age == 3 |Age == 4)
      ICCmodel2 <- irr::icc(cbind( var2[[paste0(cols[i], ".x")]], var2[[paste0(cols[i],".y")]]))
      AGEe <- ICCmodel2[["value"]]
      AGEp <- ICCmodel2[["p.value"]]
      
      var3 <- data %>% filter(user_gender == "Female")
      ICCmodel3 <- irr::icc(cbind( var3[[paste0(cols[i], ".x")]], var3[[paste0(cols[i],".y")]]))
      SEXe <-ICCmodel3[["value"]]
      SEXp <- ICCmodel3[["p.value"]]
      
      
      #--------------------------------------------#
      # Add results to result dataframe
      #--------------------------------------------#
      row <- c(GDPe,GDPp,AGEe,AGEp,SEXe,SEXp,cor_estimate,p_value)
      result[i,] <- row
    }
    
    # Set column names for result dataframe
    return(result)
    
  }
  
  result <- list(ALL=NULL, AGE3=NULL, AGE4=NULL, AGE5=NULL)
  result$ALL <- relia_group(colbind_group)
  colbind3 <- colbind[colbind_group$Age == 3,]
  colbind4 <- colbind[colbind_group$Age == 4,]
  colbind5 <- colbind[colbind_group$Age == 5,]
  result$AGE3 <- relia(colbind3)
  result$AGE4 <- relia(colbind4)
  result$AGE5 <- relia(colbind5)
  write.xlsx(result,'TABLE7.xlsx')
  
  #---------Visualization of test-retest correlation results - scatter plot--------#                  
  # Function to create scatter plot for a given game
  create_scatter_plot <- function(data, game) {
    cor <- cor.test(data[[paste0(game, ".x")]], data[[paste0(game, ".y")]], use = "pairwise.complete.obs")
    correlation <- cor$estimate
    cilower <- cor$conf.int[1]
    ciupper <- cor$conf.int[2]
    pvalue <- cor$p.value
    caption <- paste("Correlation:", round(correlation, 3), "\n",
                     "CI:", round(cilower, 2), "-", round(ciupper, 2), "\n",
                     "")
    
    p <- ggplot(data, aes_string(x = paste0(game, ".x"), y = paste0(game, ".y"))) +
      geom_point(color = "#6CB2EB") +
      ggthemes::theme_few() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_text(color = "black", size = 14, face = "bold")
      ) +
      labs(
        title = paste(game, "Scatter Plot"),
        x = paste0(game, "_initial"),
        y = paste0(game, "_retest")
      ) + 
      annotate(geom = "text", x = 0, y = -2,
               label = caption, color = "black", size = 4 )
    
    return(p)
  }
  
  # Create scatter plots for all games
  # games <- c("Feed", "Fish", "Pets", "Card", "Biscuit", "Magical")
  plots <- lapply(game_name, function(game) create_scatter_plot(colbind, game))
  
  # Arrange the plots in a 3x2 grid
  grid.arrange(grobs = plots, ncol = 3, nrow = 2)
  
  
  
  
  # Method 2: Measurement invariance
  irt_SCORE$time <- 1
  RE_irt_SCORE$time <- 2
  
  rowbind <-  bind_rows(irt_SCORE, RE_irt_SCORE)
  rowbind$time <- as.factor(rowbind$time)
  
  #CFA model 
  form_3f = '   IC =~ Feed+Fish
                WM =~ Card+Magical  
                FS =~ Pets+Biscuit   '    
  
  #configural invariance（costrain factor structure）
  fit.Configural <- cfa(form_3f, data = rowbind, group = "time", meanstructure = TRUE) #Configural Invariance
  
  #metric invariance (weak invariance)
  fit.Metric <- cfa(form_3f, data = rowbind, group = "time"
                    , group.equal = c("loadings"), meanstructure = TRUE)   # Constrain factor loadings equal across groups, Metric Invariance
  
  #scalar invariance (strong invariance)
  fit.Scalar <- cfa(form_3f, data = rowbind, group = "time"
                     , group.equal = c("loadings","intercepts"), meanstructure = TRUE)    # Loadings and intercepts equal, Scalar Invariance
  
  #chi-square difference test
  diff <- lavaan::lavTestLRT(fit.Configural, fit.Metric, fit.Scalar)  #anova
  write.xlsx(diff,'TABLE8.xlsx')  
  
  
  
}

