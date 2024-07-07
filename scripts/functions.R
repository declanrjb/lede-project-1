library(tidyverse)
library(rvest)
library(plyr)
library(gender)
library(tidytext)

table_from_wraps <- function(cat_wraps) {
  entries <- cat_wraps %>% html_children() %>% html_text()
  entries <- str_squish(entries)
  
  attributes <- entries[seq(1,length(entries),2)]
  values <- entries[seq(2,length(entries),2)]
  
  temp_df <- as.data.frame(matrix(ncol=length(values),nrow=1))
  colnames(temp_df) <- attributes
  temp_df[1,] <- values
  return(temp_df)
}

get_cast <- function(session) {
  names <- session %>% 
    html_nodes('div[slot="content"]') %>% 
    .[1] %>% 
    html_nodes('a[data-qa="person-item"]') %>% 
    html_nodes('p[class="name"]') %>%
    html_text()
  roles <- session %>% 
    html_nodes('div[slot="content"]') %>% 
    .[1] %>% 
    html_nodes('a[data-qa="person-item"]') %>% 
    html_nodes('p[class="role"]') %>%
    html_text()
  
  temp_df <- as.data.frame(matrix(ncol=2,nrow=length(names)))
  colnames(temp_df) <- c('Name','Role')
  temp_df$Name <- names
  temp_df$Role <- roles
  return(temp_df)
}

get_metadata <- function(sess) {
  scorecard <- sess %>% html_elements(".media-scorecard")
  
  info_card <- sess %>% html_elements(".media-info")
  meta_table <- info_card %>% html_elements('.category-wrap') %>% table_from_wraps()
  
  synopsis <- info_card %>% html_elements('.synopsis-wrap') %>% html_text()
  synopsis <- gsub('Synopsis','',synopsis) %>% str_trim()
  meta_table['Synopsis'] <- synopsis
  
  meta_table['Critics'] <- scorecard %>% html_element('rt-button[slot="criticsScore"]') %>% html_text() %>% parse_number()
  meta_table['Audiences'] <- scorecard %>% html_element('rt-button[slot="audienceScore"]') %>% html_text() %>% parse_number()
  meta_table['Title'] <- sess %>% html_elements('h1[slot="titleIntro"]') %>% html_text() %>% str_squish()
  
  return(meta_table)
}

get_outbound_links <- function(session) {
  return(paste('https://www.rottentomatoes.com',session %>% html_nodes('rt-link[slot="title"]') %>% html_attr('href'),sep=""))
}

name_from_url <- function(url) {
  return(paste(gsub('https://www.rottentomatoes.com/','',url),'.csv',sep=''))
}

defer_write <- function(data,path,overwrite=FALSE) {
  if (overwrite) {
    write.csv(data,path,row.names=FALSE)
    return(TRUE)
  } else if (file.exists(path)) {
    return(FALSE)
  } else {
    write.csv(data,path,row.names=FALSE)
    return(TRUE)
  }
} 

scrape_movie_from_url <- function(url) {
  
  write_out_name <- name_from_url(url)
  
  if (!file.exists(paste('meta_data/',write_out_name,sep=''))) {
    message('NOT ON DRIVE')
    sess <- read_html_live(url)
    
    if (length(sess %>% html_nodes('h1[slot="titleIntro"]')) == 0) {
      return(c())
    }
    
    meta_df <- get_metadata(sess)
    
    cast_df <- get_cast(sess)
    cast_df <- cbind(Title=meta_df['Title'],cast_df)
    
    next_links <- get_outbound_links(sess)
    
    sess$session$close()
    
    edges_df <- as.data.frame(matrix(ncol=2,nrow=length(next_links)))
    colnames(edges_df) <- c('Source','Target')
    edges_df$Source <- meta_df[1,]$Title
    edges_df$Target <- next_links
    
    write.csv(meta_df,paste('meta_data/',write_out_name,sep=''),row.names=FALSE)
    write.csv(cast_df,paste('casts/',write_out_name,sep=''),row.names=FALSE)
    write.csv(edges_df,paste('network/',write_out_name,sep=''),row.names=FALSE)
    
    message('SUCCESFULLY WROTE')
    
    return(next_links)
  } else {
    message('IS ON DRIVE')
    seeds_df <- read_csv(paste('network/',write_out_name,sep=''),show_col_types=FALSE)
    return(seeds_df$Target)
  }
}

directory_bind <- function(dir) {
  files <- paste(dir,'/',list.files(dir),sep='')
  df <- read_csv(files[1])
  for (i in 2:length(files)) {
    temp_df <- read_csv(files[i],show_col_types=FALSE)
    df <- rbind.fill(df,temp_df)
  }
  return(df)
}

first_name <- function(names) {
  result <- str_split_i(names,' ',1)
  return(result)
}

chunk_split <- function(vec,n) {
  result <- list()
  if (length(vec) > 0) {
    i <- 1
    while (length(vec) > n) {
      temp <- vec[1:n]
      vec <- vec[-c(1:n)]
      result[[i]] <- temp
      i <- i + 1
    }
    result[[i]] <- vec
  }
  return(result)
}

gender_lookup_names <- function(names,chunk_size=10) {
  if (file.exists('gender_database/local_reference.csv')) {
    ref_df <- read_csv('gender_database/local_reference.csv',show_col_types = FALSE)
  } else {
    ref_df <- as.data.frame(matrix(ncol=4,nrow=0))
    colnames(ref_df) <- c('name','gender','proportion_female','proportion_male')
  }
  search_names <- names[which(!(names %in% ref_df$name))]
  all_chunks <- chunk_split(search_names,chunk_size)
  if (length(all_chunks) > 0) {
    for (chunk in all_chunks) {
      curr_response <- gender(
        chunk,
        method = c("genderize")
      )
      ref_df <- rbind(ref_df,curr_response)
      ref_df <- unique(ref_df)
      write.csv(ref_df,'gender_database/local_reference.csv',row.names=FALSE)
      Sys.sleep(1)
    }
  }
  return(ref_df %>% filter(name %in% names))
}

apply_gender <- function(name_vec,use_method='ssa') {
  result_df <- as.data.frame(matrix(ncol=1,nrow=length(name_vec)))
  colnames(result_df) <- c('name')
  result_df$name <- name_vec
  
  names <- name_vec %>% first_name() %>% unique()
  gender_ref <- names %>% gender(method=use_method) %>% select(name,gender) %>% unique()
  
  result_df <- left_join(result_df,gender_ref)
  
  return(result_df %>% pull(gender))
}

stat_summarize <- function(df,filter_col,stat_col,stat_func,drop_na=FALSE) {
  filter_vals <- unique(df[[{{filter_col}}]])
  if (drop_na) {
    filter_vals <- filter_vals[!is.na(filter_vals)]
  }
  result <- as.data.frame(matrix(ncol=2,nrow=length(filter_vals)))
  colnames(result) <- c(filter_col,stat_col)
  for (i in 1:length(filter_vals)) {
    filter_val <- filter_vals[i]
    target_rows <- which(df[[{{filter_col}}]] == filter_val)
    temp_df <- df[target_rows,]
    result[[{{filter_col}}]][i] <- filter_val
    result[[{{stat_col}}]][i] <- stat_func(temp_df[[{{stat_col}}]])
  }
  return(result)
}

meta_stat_summarize <- function(df,filter_cols,stat_col,stat_func,drop_na=FALSE) {
  if (length(filter_cols) == 1) {
    result <- df %>% stat_summarize(filter_cols[1],stat_col,stat_func,drop_na)
  } else {
    curr_filter_col <- filter_cols[1]
    filter_cols <- filter_cols[-c(1)]
    
    filter_vals <- unique(df[[{{curr_filter_col}}]])
    
    if (drop_na) {
      filter_vals <- filter_vals[!is.na(filter_vals)]
    }
    
    filter_val <- filter_vals[1]
    target_rows <- which(df[[{{curr_filter_col}}]] == filter_val)
    temp_df <- df[target_rows,]
    temp_result <- temp_df %>% meta_stat_summarize(filter_cols,stat_col,stat_func,drop_na)
    
    temp_result[curr_filter_col] <- filter_val
    result <- temp_result

    for (i in 2:length(filter_vals)) {
      filter_val <- filter_vals[i]
      if (is.na(filter_val)) {
        target_rows <- which(is.na(df[[{{curr_filter_col}}]]))
      } else {
        target_rows <- which(df[[{{curr_filter_col}}]] == filter_val)
      }
      temp_df <- df[target_rows,]
      
      temp_result <- temp_df %>% meta_stat_summarize(filter_cols,stat_col,stat_func,drop_na)
      
      temp_result[curr_filter_col] <- filter_val
      
      result <- rbind(result,temp_result)
    }
  }
  return(result)
}

add_na_rm <- function(func) {
  output_func <- function(x) {
    return(func(x,na.rm=TRUE))
  }
  return(output_func)
}

get_word_vector <- function(blurb) {
  return(blurb %>% tibble(txt = .) %>% unnest_tokens(word,txt) %>% pull(word))
}

first_pronoun <- function(blurb) {
  pronouns <- c('he','him','his','she','her','hers')
  word_vec <- get_word_vector(blurb)
  found_indices <- which(word_vec %in% pronouns)
  if (length(found_indices) > 0) {
    return(word_vec[min(found_indices)])
  } else {
    return(NA)
  }
}

pronoun_to_gender <- function(pronoun) {
  if (pronoun %in% c('he','him','his')) {
    return('male')
  } else if (pronoun %in% c('she','her','hers')) {
    return('female')
  } else {
    return(NA)
  }
}

millionize_entry <- function(string) {
  num_form <- parse_number(string)
  if (grepl('K',string,fixed=TRUE)) {
    num_form <- num_form * 1000
  } else if (grepl('M',string,fixed=TRUE)) {
    num_form <- num_form * 1000000
  }
  return(num_form)
}

millionize <- function(vec) {
  return(unlist(lapply(vec,millionize_entry)))
}

occurrences_in <- function(val,vec) {
  return(length(which(vec == val)))
}

value_counts <- function(vec) {
  unique_vals <- unique(vec)
  
  result <- as.data.frame(matrix(ncol=2,nrow=length(unique_vals)))
  colnames(result) <- c('Val','N')
  
  result$Val <- unique_vals
  result$N <- unlist(lapply(result$Val,occurrences_in,vec))
  
  return(result)
}