set.seed(123)

generate_overlap_df <- function(n = 400,
                                overall_min = 0.75,
                                group_min = 0.70,
                                max_iter = 200000) {
  
  for (iter in 1:max_iter) {
    
    # Common X distribution (keeps one unified cloud)
    advertising_spend_k <- runif(n, 20, 100)
    
    # Very mild dependence of channel on X (creates enough structure for overall corr)
    z <- as.numeric(scale(advertising_spend_k))
    p_search <- plogis(0.05 + 0.30 * z)   # mild tilt; keeps groups mixed
    channel <- ifelse(runif(n) < p_search, "Search Ads", "Retail Promo")
    channel <- factor(channel, levels = c("Retail Promo", "Search Ads"))
    
    # Ensure both groups have a decent count
    if (min(table(channel)) < 130) next
    
    # --- Key: more different slopes ---
    beta <- ifelse(channel == "Search Ads", 2.8, 1.2)
    
    # Keep intercepts close so groups don't form two separate horizontal bands
    alpha <- ifelse(channel == "Search Ads", 95, 105)
    
    # Noise tuned to maintain overlap while still giving decent within-group correlations
    sigma <- ifelse(channel == "Search Ads", 30, 22)
    
    weekly_sales_k <- alpha + beta * advertising_spend_k + rnorm(n, 0, sigma)
    
    df <- data.frame(
      channel,
      advertising_spend_k,
      weekly_sales_k
    )
    
    # Correlations
    overall_cor <- cor(df$advertising_spend_k, df$weekly_sales_k)
    
    cor_search <- with(df[df$channel == "Search Ads", ],
                       cor(advertising_spend_k, weekly_sales_k))
    cor_retail <- with(df[df$channel == "Retail Promo", ],
                       cor(advertising_spend_k, weekly_sales_k))
    
    if (overall_cor >= overall_min &&
        cor_search  >= group_min &&
        cor_retail  >= group_min) {
      
      attr(df, "correlations") <- c(
        overall = overall_cor,
        retail_promo = cor_retail,
        search_ads = cor_search,
        iterations = iter
      )
      return(df)
    }
  }
  
  stop("Could not meet constraints within max_iter. Try slightly increasing noise or relaxing group_min.")
}

df <- generate_overlap_df(n = 400, overall_min = 0.75, group_min = 0.70)
attr(df, "correlations")

df |> point_plot(weekly_sales_k ~ advertising_spend_k + channel, annot = "model")

write_csv(df, "lst-extras-datasets/advertising_sales_channel.csv")

advertising_sales_channel <- read.csv("lst-extras-datasets/advertising_sales_channel.csv")
advertising_sales_channel |> point_plot(weekly_sales_k ~ advertising_spend_k, annot = "model")
