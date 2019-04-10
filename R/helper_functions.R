#' Operator NOT IN
#'
#' This operator is the opposite of the `in` operator
#'
#' @examples 1 %ni% 1  # FALSE
#' @examples 1 %ni% 2  # TRUE
#' @export
`%ni%` <- Negate(`%in%`)

#' Standard Transformation
#'
#' This function transforms data to normal distribution
#' @param x A vector containing values
#' @return A vector of transformed values
#' @examples x <- c(1000, seq(1,100, 1)) 
#' @examples std_transform(x)  
#' @export
std_transform <- function(x)
{
    x[is.na(x)] <- 0
    x <- abs(x)
    
    # try boxcox
    lambdas <- seq(-6, 6, 0.005)
    Box = MASS::boxcox(x+1~1, lambda = lambdas)
    Cox <- data.frame(Box$x, Box$y)
    Cox <- Cox[with(Cox, order(-Cox$Box.y)), ]
    lambda = Cox[1, "Box.x"]
    
    # try various methods
    transformed <- data.frame(bc.t =  (x ^ lambda - 1) / lambda, 
                              sqrt.t = sqrt(x),
                              cubert.t = x^(1/3),
                              log.t = log(x+1),
                              anscombe.t = 2 * sqrt(x + (3/8)),
                              default.t = x)
    # find skew                        
    skew <- data.frame (bc.t = abs(e1071::skewness(transformed$bc.t)),
                        cubert.t = abs(e1071::skewness(transformed$cubert.t)),
                        sqrt.t = abs(e1071::skewness(transformed$sqrt.t)),
                        log.t = abs(e1071::skewness(transformed$log.t)),
                        anscombe.t = abs(e1071::skewness(transformed$anscombe.t)),
                        default.t = abs(e1071::skewness(transformed$default.t))
    )
    
    use.metric <- transformed[[names(which.min(skew))]]
    
    
    return(use.metric)
}


#' Calculate margin of error 
#'
#' This function calculates margin of error using bootstrap resampling
#' @param x A vector of values
#' @param B Number of resampling iterations
#' @param n Resample size
#' @return A list containing various metrics
#' @examples moe.summary <- calc_moe()
#' @examples moe.summary$interval
#' @examples moe.summary$mean
#' @examples moe.summary$moe
#' @export
calc_moe = function(x, B, n) {
    sd.N = sd(x)
    boot.samples = matrix(sample(x,size=n*B,replace=TRUE), B, n)
    boot.statistics = apply(boot.samples,1,mean)
    se = sd(boot.statistics)
    mose = se*1.96
    mose.90 = se*1.645
    interval = mean(x) + c(-1,1) * mose
    interval.90 = mean(x) + c(-1,1) * mose.90
    
    return( list(boot.statistics = boot.statistics,
                 interval = interval,
                 interval.90 = interval.90,
                 se = se,
                 moe = mose,
                 moe.90 = mose.90,
                 n = n,
                 mean = mean(x),
                 mean.s = mean(boot.statistics)))
}


#' Calculate proportions
#'
#' This function calculates variable proportions for each column in a dataframe
#' @param df A dataframe containing variables to calculate proportions from
#' @param exclude.cols A vector of columns to be excluded 
#' @return A dataframe with variabes, levels, and proportions
#' @examples calc_props(df, 'user_id')
#' @export
calc_props <- function(df, exclude.cols) {
    # Calculates demographic proportions from exposed users to be used in demo balancing of the control group
    #
    # Args:
    #   df: A dataframe that contains recoded demographics
    #
    # Returns:
    #   A dataframe with demographic proportions to be used as targets in demo balancing of the control group
    
    vars <- names(df[ , -which(names(df) %ni% exclude.cols)])
    
    out <- head(data.frame(level=NA, prop=NA, variable=NA), n=0)
    
    for(var in vars) {
        var.df <- data.frame(prop.table(table(df[[var]])))
        var.df$variable <- var
        names(var.df) <- c('level','prop','variable')
        var.df$prop <- var.df$prop
        
        out <- rbind(out, var.df)
    }
    
    return(out)
}


#' Calculate outliers
#'
#' This function calculates outliers for any given metric
#' @param x A vector containing values
#' @return A scalar value of the outlier
#' @examples x <- c(1000, seq(1,100, 1)) 
#' @examples calc_outlier(x) # returns 1000
#' @export
calc_outlier <- function(x, both=FALSE)
{
    use.metric <- std_transform(x)  
    q75 <- summary(use.metric)[5][[1]]
    q25 <- summary(use.metric)[2][[1]]
    iqr1.5 <- IQR(use.metric) * 1.5
    upper.outlier <- q75 + iqr1.5
    lower.outlier <- q25 - iqr1.5
    
    z <- data.frame(metric = x,
                    transformed = use.metric)
    
    z$outlier <- ifelse(z$transformed >= upper.outlier, 1, 0)
    
    if(both) {
        z$outlier <- ifelse(z$transformed >= upper.outlier | z$transformed <= lower.outlier, 1, 0)
    }
    
    return(z$outlier)
}


#' Generate bins for continous variables
#'
#' This function calculates outliers for any given metric
#' @param df A dataframe containing metric values
#' @param metric The column name of the metric of interest
#' @param bin.threshold The min number of bins that needs to be met in order to calculate bins up to max.bins
#' @param min.bins The min number of bins
#' @param max.bins The max number of bins
#' @param bin.cutoff Combines bins together where the percent of values contained is less than the cutoff
#' @return A list object containing the input df with outliers removed, and the outlier values
#' @examples calc_bins(df, 'spend')
#' @export
calc_bins <- function(df, metric, bin.threshold=4, min.bins=3, max.bins=8, bin.cutoff=0.055){
    bin.name <- paste0(metric,'_bin')
    
    df[is.na(df)] <- 0
    
    outlier <- calc_outlier(df[[metric]])

    df.e<-df
    
    cat.tf.bins <- hist(df.e[[metric]], max.bins, plot = FALSE)$breaks
    
    bin.props.e <- prop.table(table(findInterval(df.e[[metric]], cat.tf.bins, left.open = TRUE)))
    bin.cutoff <- ifelse(length(bin.props.e) > bin.threshold, max.bins , min.bins)
    bin.props.min <- bin.props.e[bin.cutoff:length(bin.props.e)][which.max(bin.props.e[bin.cutoff:length(bin.props.e)] < bin.cutoff)]
    
    final.bins <- cat.tf.bins[1:as.numeric(labels(bin.props.min))]
    
    bins <- findInterval(df[[metric]], final.bins, left.open = TRUE)
    
    return(bins)
}


#' Generate weights for a given set of users
#'
#' This function calculates weights for a set of users, based on predefined targets
#' @param df A dataframe containing metric values
#' @param y A df that contains quotas (i.e. targets) that will be used to determine weights
#' @param min Lower cap weight floor. Defaults to -Inf
#' @param max Upper cap weight ceiling. Defaults to Inf
#' @return A list object containing the input df with weights attached, and proportions for each variable used
#' @examples targets <- calc_props(df.test, 'user_id')
#' @examples df.list <- calc_weights(df.control, targets)
#' @examples df.weighted <- df.list$df
#' @export
calc_weights <- function(df, y, min.cap=-Inf, max.cap=Inf) {
    # Calculates weights that align to quotas (i.e. targets)
    #
    # Args:
    #   df: A df that contains recoded variables
    #   y: A df that contains quotas (i.e. targets) that will be used to determine weights
    #   min.cap: Lower limit weight cap. Defaults to -Inf
    #   max.cap: Upper limit weight cap. Defaults to Inf
    #
    # Returns:
    #   A list object, containing a weighted df, and population statistics df
    y <- y[order(y$variable, y$level),]
    
    demo.list <- unique(y$variable)
    
    x <- df
    
    df <- df[demo.list]
    
    df[df=='unknown'] <- NA
    df[df==''] <- NA 
    
    for(demo in demo.list){
        demo.levels <- as.character(y[y$variable == demo & y$quota > 0, ]$level)
        df <- df[df[[demo]] %in% demo.levels, ]
    }
    
    # create survey obj
    df <- na.omit(df)
    unweighted <- survey::svydesign(ids=~1, data=df, probs = 1)
    
    weights.list <- list()
    counter <- 1
    
    for(demo in demo.list){
        
        demo.df <- y[y$variable == demo & y$prop > 0, ]
        assign(paste(demo, '.dist', sep=''), data.frame(placeholder = unique(demo.df$level),
                                                        Freq = nrow(df) * demo.df$prop,
                                                        Pop = data.frame(table(df[[demo]]))$Freq))
        
        assign(paste(demo, '.dist', sep=''),'names<-'((get(paste(demo, '.dist', sep=''))), c(demo,'Freq','Pop')))
        
        assign(paste(demo,'.list',sep=''), length(table(df[[demo]])))
        
        weights.list.iter <- list( length(unique(demo.df$level)))
        
        weights.list.iter[[paste(demo)]] <- get(paste(demo,'.list',sep=''))
        weights.list.iter[[3]] <- as.formula(paste('~',demo,collapse=''))
        weights.list.iter[[4]] <- get(paste(demo, '.dist', sep=''))
        
        assign(paste(demo,'.weights_list',sep=''), weights.list.iter)
        
        weights.list[[counter]] <- weights.list.iter
        counter <- counter + 1
    }
    
    sample.margins <- lapply(weights.list, function(i){
        if(i[[1]] == i[[2]]){
            weights <- i[[3]]
        }else{
            weights<-NULL
        }
    })
    
    population.margins <- lapply(weights.list, function(i){
        if(i[[1]] == i[[2]]){
            weights <- i[[4]][1:2]
        }else{
            weights<-NULL
        }
    })
    
    
    sample.rake <- survey::rake(unweighted,
                        sample.margins = sample.margins[!sapply(sample.margins,is.null)], # data headers
                        population.margins = population.margins[!sapply(population.margins,is.null)], # target lists
                        control = list(verbose=FALSE, 
                                       maxit=500,
                                       epsilon=0.000001))
    
    sample.trim <- survey::trimWeights(sample.rake, lower=min.cap, upper=max.cap, strict=FALSE) 
    
    df$weight <- survey::weights(sample.trim)
    
    df <- df[order(-df$weight),]
    df$prob <- df$weight / max(df$weight)
    
    df <- merge(x, unique(df), by=demo.list, all.x=TRUE)
    df$weight <- ifelse(is.na(df$weight), 0, df$weight)
    df$prob <- ifelse(is.na(df$prob), 0, df$prob)
    
    # return object
    out <- list()
    out$df <- df
    
    for(demo in demo.list){
        out[[demo]]  <- get(paste(demo,'.dist',sep=''))
    }
    
    return(out)
}


#' Calculate weighting efficiency for a given set of weights
#'
#' This function calculates weighting efficiency, which is an indication of the amount of skewing that had to be done to get the weights to converge
#' @param x A vector of weights
#' @param base Base weights (default = 1)
#' @return The weighting efficiency score
#' @examples calc_efficiency(weights)
#' @export
calc_efficiency <- function(x, base = 1) {
    # x = weights
    # base = 1
    Pj <- rep(base, length(x))
    Rj <- x
    PjRj <- Pj*Rj
    PjRj.sq <- PjRj^2
    
    Pj.Sigm <- sum(Pj)
    Rj.Sigm <- sum(Rj)
    PjRj.Sigm <- sum(PjRj)
    PjRj.sq.Sigm <- sum(PjRj.sq)
    
    weight.index <- (PjRj.Sigm * Pj.Sigm) / Rj.Sigm
    weight.index.sq <- PjRj.sq.Sigm * ((Pj.Sigm / Rj.Sigm)^2)
    
    out <- 100 * (weight.index^2) / (weight.index.sq * Pj.Sigm)
    
    return(out)
}


#' Recode raw demographics (used for weighting) 
#'
#' This function recodes demographics to those labels used in demogrpahic weighting.
#' Requires: gender_id, age, income_id, ethnicity_id, has_children_id, hh_size_id
#' @param df A dataframe that contains values to be recoded
#' @return The input dataframe with appended recoded vars
#' @examples df <- recode_demos(df)
#' @export
recode_demos <- function(df) {
    # Recodes demographic variables
    #
    # Args:
    #   df: A dataframe that contains values to be recoded
    #
    # Returns:
    #   The same input dataframe with appended recoded vars
    
    df[df=='' | df=='unknown'] <- NA 
    
    df$gender <- ifelse(df$gender_id == 2, "Female",
                        ifelse(df$gender_id == 1, "Male", NA))
    
    
    df$age  <- ifelse(df$age >= 1 & df$age < 18, "0-18",
                      ifelse(df$age >= 18 & df$age < 25, "18-24",
                             ifelse(df$age >= 25 & df$age < 35, "25-34",
                                    ifelse(df$age >= 35 & df$age < 45, "35-44",
                                           ifelse(df$age >= 45 & df$age < 55, "45-54",
                                                  ifelse(df$age >= 55 & df$age < 65, "55-64",
                                                         ifelse(df$age >= 65, "65+", NA)))))))
    
    df$income <- ifelse(df$income_id == 0, NA,
                        ifelse(df$income_id == 1, "- $20k",
                               ifelse(df$income_id == 2, "$20k-40k",
                                      ifelse(df$income_id == 3, "$40k-60k",
                                             ifelse(df$income_id == 4, "$60k-80k",
                                                    ifelse(df$income_id == 5, "$80k-125k",
                                                           ifelse(df$income_id ==  6, "$80k-125k",
                                                                  ifelse(df$income_id == 7, "$125k +", NA))))))))
    
    df$income <- factor(df$income, levels=c('- $20k','$20k-40k','$40k-60k', '$60k-80k','$80k-125k', '$125k +'), 
                        ordered = TRUE)
    
    df$ethnicity <- ifelse(df$ethnicity_id == 1, "Asian", 
                           ifelse(df$ethnicity_id == 2, "Black or African American",
                                  ifelse(df$ethnicity_id == 3, "Hispanic or Latino",
                                         ifelse(df$ethnicity_id == 4, "White or Caucasian",
                                                ifelse(df$ethnicity_id == 5, "Other", NA)))))
    
    df$haschild <- ifelse(df$has_children_id == 1, 'Yes',
                          ifelse(df$has_children_id == 2, 'No', NA))
    
    df$householdsize <- ifelse(df$hh_size_id == 0, NA,
                               ifelse(df$hh_size_id >=5, 5, df$hh_size_id))
    df <- na.omit(df)
    df <- df[ , -which(names(df) %in% c("gender_id","income_id","ethnicity_id","has_children_id","hh_size_id"))]
    
    return(df)
}

