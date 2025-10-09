##### Some helper functions for adding and summarising lists of lists #########
# add lists of numeric vectors elementwise across the list
add <- function(...){
  args <- list(...)
  if(nargs() == 0){
    return(NULL)
  }else if(nargs() == 1){
    return(args[[1]])
  }else if(nargs() == 2){
    pmap(args, `+`)
  }else{
    reduce(args,add)
  }
}

#add lists of lists of numeric vectors element-wise across the list
add2 <- function(...){
  args <- list(...)
  if(length(args) == 0){
    return(NULL)
  }else if(length(args) == 1){
    return(args[[1]])
  }else  pmap(args,add)
}

addn <- function(...,.n){
  if(!is.numeric(.n) || .n%%1 || .n < 0){
    stop('.n must be a non-negative integer')
  }
  args <- list(...)
  if(length(args) == 0){
    return(NULL)
  }else if(length(args) == 1){
    return(args[[1]])
  }else{
    if(.n == 0){
      reduce(args,`+`)
    }else if(.n == 1){
      add(...)
    }else{
      pmap(args,addn,.n = .n - 1)
    }
  }
}

laddn <- function(l,.n){do.call(addn,c(unname(l),.n = .n))}
ladd <- function(l){laddn(l,1)}

#make a n-level nested list into a rectangular tibble with the lowest level list stored in a column list
rectangle <- function(x, names_to, values_to){
  x %>%
    rapply(enquote,how = "unlist") %>% #make a dataframe (very wide)
    lapply(eval) %>%
    as.data.frame(check.names = F) %>%
    summarise(across(.cols = everything(),.fns = list)) %>% #make the draws list columns so they take up less space with we lengthen the dataframe
    pivot_longer(everything(), #lengthen dataframe
                 names_sep = "\\.",
                 names_to = names_to,
                 values_to = values_to)
}


#Traverse n-level nested lists, calculate quantiles, then reformat nested list to a dataframe
quantilesNestedList <- function(x,depth,names_to,probs = c(0.5,0.05,0.95),quant_names = c('median', '5%','95%')){
  if(length(names_to)!=depth) stop('Each nesting level of the list needs a name')
  x %>%
    map_depth(depth, ~unname(quantile(.x,probs = probs))) %>% #get median and intervals
    rapply(enquote,how = "unlist") %>% #make a dataframe (very wide)
    lapply(eval) %>%
    as.data.frame(check.names = F) %>%
    mutate(Quantile = quant_names) %>% #label quantiles
    pivot_longer(-Quantile,
                 names_sep = if(depth > 1){"\\."}else{NULL},
                 names_to = names_to) %>%
    pivot_wider(names_from = "Quantile", values_from = 'value')

}

appendGroupTotals <- function(.d, val_col, group_cols){
  iwalk(group_cols, #for every column to do sums over
       ~{.d <<- .d %>%
         group_by(across(!all_of(c(val_col, .y)))) %>% #group over everything but the selectd value and group column so that sums are across every subgroup
         bind_rows(.,summarise(.,across(all_of(val_col), ~list(reduce(.x, `+`)))) %>% # assumes that val_col is a list column and returns it again as such
                     mutate('{.y}' := .x)) #gives the name .x to the new total category for variable .y
       })
  .d
}

quantileListColumn <- function(.d, .col, probs = c(0.5,0.05,0.95)){
  .d$out <- map(.d[,.col,drop = T],~quantile(.x, probs = probs))
  .d %>%
    unnest_wider(out) %>%
    select(!all_of(.col))
}

tidyNumber <- function(n,unit = 1000, round = TRUE, digits.round = 0, sf = 3, max.word = 0){
  ifelse(round & digits.round <= 0 & abs(n/unit) < max.word,
         paste0(ifelse(sign(n/unit) == -1,'negative ',''),
                english::english(abs(round(n/unit, digits = digits.round)))),
         format(ifelse(n/unit<10^sf & round, round(n/unit,digits = digits.round),signif(n/unit,sf)), #format to three significant figures (potentially rounding to closest integer)
                big.mark = ",",
                scientific = FALSE,
                nsmall = 0,
                trim = T,
                drop0trailing = TRUE)
         )
}

medianCIformat <- function(df,unit = 1000,newline = TRUE,round = FALSE,digits.round = 0,dropnullinterval = TRUE){
  unit = enquo(unit)
  digits.round = enquo(digits.round)
  round <- enquo(round)
  df %>%
    mutate(across(c(X5.,X95.,median),~tidyNumber(.x, !! unit, !! round, !! digits.round))) %>%
    mutate(across(c(X5.,X95.),           #remove intervals when they are the same as the point estimate
                  ~if_else(X5. == median & X95. == median & dropnullinterval,
                           '',
                           .x))) %>%
    mutate(Cost = if_else(X95. == '',  #merge cost and and interval into a single line
                          median,
                          paste0(median, ifelse(newline,'\r',' '), '(',X5.,' - ',X95.,')')))
}



group_map_named <- function(.data, .f, ..., .keep = FALSE){
  group_vars <- .data %>% group_keys %>% colnames
  ngv <- length(group_vars)
  if(ngv == 1){
    group_map(.data, .f, ..., .keep = .keep) %>% `names<-`(.data %>% group_keys %>% unlist)
  }else if(ngv > 1){
    .data <- .data %>% ungroup %>% group_by(across(all_of(group_vars[1])))
    .data %>% group_map(~{.x %>%
        group_by(across(all_of(group_vars[2:ngv]))) %>%
        group_map_named(.f, ..., .keep = .keep)}) %>%
      `names<-`(.data %>% group_keys %>% unlist)
  }
}
