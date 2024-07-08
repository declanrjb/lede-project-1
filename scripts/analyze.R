source('scripts/functions.R')

#meta_df <- directory_bind('meta_data/m')
#write.csv(meta_df,'meta_data_all.csv',row.names=FALSE)

df <- read_csv('meta_data_all_backup.csv')

# begin data cleaning

# put title column at the front
df <- df %>% select(Title,c(colnames(df)[colnames(df) != 'Title']))

# create col for difference between critics and audience ratings
df['Diff'] <- df$Critics - df$Audiences

# inspect the highest negative differences
df %>% arrange(Diff) %>% select(Title,Diff,Critics,Audiences,Synopsis)

# calculate and write director gender using ssa method
df['Director_Gender'] <- df %>% pull(Director) %>% first_name() %>% apply_gender(use_method = 'ssa')

# check a sample by hand to verify that method of calculating genders
gender_sample <- df[sample.int(dim(df)[1],size=100),] %>% select(Director,Director_Gender)

# loss of 223 rows, 5% of data
df <- df[which(lapply(str_split(df$Director,','),length) == 1),]

# update the genders
df['Director_Gender'] <- df %>% pull(Director) %>% first_name() %>% apply_gender(use_method = 'ssa')

# clean ratings for consistency
df['Rating_Cleaned'] <- str_squish(str_split_i(df$Rating," ",1))
df$Rating_Cleaned <- str_squish(gsub(',','',df$Rating_Cleaned))

# calculate and clean the first genders from the synopses
df['First_Pronoun'] <- unlist(lapply(df$Synopsis,first_pronoun))
df['First_Gender'] <- unlist(lapply(df$First_Pronoun,pronoun_to_gender))

# drop unnecessary columns
df <- df %>% 
  select(!`Sound Mix`) %>% 
  select(!`Aspect Ratio`) %>% 
  select(!`Rerelease Date (Theaters)`) %>%
  select(!`Release Date (DVD)`) %>%
  select(!Creator) %>%
  select(!Rating) %>%
  select(!`Release Date (Streaming)`)

# get the first genre from each genre list
df['Primary_Genre'] <- str_split_i(df$Genre,',',1)

# release date is assumed to be theaters. streaming and dvd dropped
# box office is us gross
df <- df %>% 
  dplyr::rename("Language" = "Original Language") %>%
  dplyr::rename("Prod_Co" = "Production Co") %>%
  dplyr::rename("US_Gross" = "Box Office (Gross USA)") %>%
  dplyr::rename("Release_Date" = "Release Date (Theaters)")

#clean up
df$Language <- str_squish(str_split_i(df$Language,'\\(',1))

# turn string style grosses into floats
df$US_Gross <- millionize(df$US_Gross)

# parse runtime and release date
df$Runtime <- parse_date_time(df$Runtime,orders='HM')
df['Runtime_Hours'] <- hour(df$Runtime) + (minute(df$Runtime) / 60)

df$Release_Date <- parse_date_time(df$Release_Date,orders='bdY')

df['Decade'] <- year(df$Release_Date) - (year(df$Release_Date) %% 10)

# end data cleanup

# director and character
df %>% 
  meta_stat_summarize(c('Director_Gender','First_Gender'),'US_Gross',add_na_rm(mean),drop_na=TRUE) %>% 
  select(Director_Gender,First_Gender,US_Gross) %>% 
  arrange(-US_Gross)

# character by decade
char_by_dec <- df %>% 
  meta_stat_summarize(c('Decade','First_Gender'),'US_Gross',add_na_rm(mean),drop_na=TRUE) %>% 
  select(c('Decade','First_Gender','US_Gross')) %>%
  arrange(Decade)

# director by decade
dir_by_dec <- df %>% 
  meta_stat_summarize(c('Decade','Director_Gender'),'US_Gross',add_na_rm(mean),drop_na=TRUE) %>% 
  select(c('Decade','Director_Gender','US_Gross')) %>%
  arrange(Decade)

p <- ggplot(data=char_by_dec,aes(x=Decade,y=US_Gross,color=First_Gender)) +
  geom_line() +
  theme_bw()

q <- ggplot(data=dir_by_dec,aes(x=Decade,y=US_Gross,color=Director_Gender)) +
  geom_line() +
  theme_bw()

char_dir_by_dec <- df %>% 
  filter(Decade >= 1980) %>%
  meta_stat_summarize(c('Decade','Director_Gender','First_Gender'),'US_Gross',add_na_rm(mean),drop_na=TRUE) %>% 
  select(Decade,Director_Gender,First_Gender,US_Gross) %>% 
  arrange(-US_Gross) %>%
  arrange(Decade)

char_dir_by_dec['Key'] <- paste(char_dir_by_dec$Director_Gender,'_',char_dir_by_dec$First_Gender,sep='')

w <- ggplot(data=char_dir_by_dec,aes(x=Decade,y=US_Gross,color=Key)) +
  geom_line() +
  theme_bw()

# director gender since 1980
df %>% 
  filter(year(Release_Date) >= 1980) %>% 
  meta_stat_summarize(c('Director_Gender'),'US_Gross',add_na_rm(mean),drop_na=TRUE) %>% 
  select(Director_Gender,US_Gross) %>% 
  arrange(-US_Gross)

# number of films by director gender by decade
df %>% 
  meta_stat_summarize(c('Decade','Director_Gender'),'US_Gross',length,drop_na=TRUE) %>% 
  select(Decade,Director_Gender,US_Gross) %>% 
  arrange(Decade)

# director gender since 1980 by decade
dir_by_decade <- df %>% 
  filter(year(Release_Date) >= 1980) %>% 
  meta_stat_summarize(c('Decade','Director_Gender'),'US_Gross',add_na_rm(mean),drop_na=TRUE) %>% 
  select(Decade,Director_Gender,US_Gross) %>% 
  arrange(Decade)

write.csv(dir_by_decade,'output/dir_by_decade.csv',row.names=FALSE)

dec_dodge_flour <- left_join(dir_by_decade %>% 
        filter(Director_Gender == 'male') %>% 
        select(!Director_Gender) %>% 
        dplyr::rename('Male_Gross' = 'US_Gross'),
      dir_by_decade %>% 
        filter(Director_Gender == 'female') %>% 
        select(!Director_Gender) %>% 
        dplyr::rename('Female_Gross' = 'US_Gross'),
      by='Decade')

write.csv(dec_dodge_flour,'output/dec_dodge_flour.csv',row.names=FALSE)

dir_by_genre <- df %>% 
  filter(year(Release_Date) >= 1980) %>% 
  meta_stat_summarize(c('Primary_Genre','Director_Gender'),'US_Gross',add_na_rm(mean),drop_na=TRUE) %>% 
  select(Primary_Genre,Director_Gender,US_Gross) %>% 
  arrange(Primary_Genre)

genre_dodge_flour <- left_join(dir_by_genre %>% 
            filter(Director_Gender == 'male') %>% 
            select(!Director_Gender) %>% 
            dplyr::rename('Male_Gross' = 'US_Gross'),
          dir_by_genre %>% 
            filter(Director_Gender == 'female') %>% 
            select(!Director_Gender) %>% 
            dplyr::rename('Female_Gross' = 'US_Gross'),
          by='Primary_Genre')

write.csv(genre_dodge_flour,'output/genre_dodge_flour.csv',row.names=FALSE)

df %>% 
  meta_stat_summarize(c('Primary_Genre','Director_Gender'),'US_Gross',length,drop_na=TRUE) %>% 
  select(Primary_Genre,Director_Gender,US_Gross) %>% 
  arrange(Primary_Genre)

dec_dir_df <- df %>% 
  meta_stat_summarize(c('Decade','Director_Gender'),'Audiences',add_na_rm(mean),drop_na=TRUE) %>% 
  select(Decade,Director_Gender,Audiences) %>% 
  arrange(Decade)

dec_dir_flour <- left_join(dec_dir_df %>% 
            filter(Director_Gender == 'male') %>% 
            select(!Director_Gender) %>% 
            dplyr::rename('Male_Ratings' = 'Audiences'),
          dec_dir_df %>% 
            filter(Director_Gender == 'female') %>% 
            select(!Director_Gender) %>% 
            dplyr::rename('Female_Ratings' = 'Audiences'),
          by='Decade') %>%
  filter(Decade >= 1980)

write.csv(dec_dir_flour,'output/dec_dir_flour.csv',row.names=FALSE)

df %>% 
  meta_stat_summarize(c('Decade','First_Gender'),'Audiences',add_na_rm(mean),drop_na=TRUE) %>% 
  select(Decade,First_Gender,Audiences) %>% 
  arrange(Decade)

pronoun_by_decade <- df %>% 
  meta_stat_summarize(c('Decade','First_Gender'),'US_Gross',add_na_rm(mean),drop_na=TRUE) %>% 
  select(Decade,First_Gender,US_Gross) %>% 
  arrange(Decade) %>%
  filter(Decade >= 1980)

write.csv(pronoun_by_decade,'output/pronoun_by_decade.csv',row.names=FALSE)

pron_by_dec_flour <- left_join(pronoun_by_decade %>% 
                             filter(First_Gender == 'male') %>% 
                             select(!First_Gender) %>% 
                             dplyr::rename('Male_Gross' = 'US_Gross'),
                           pronoun_by_decade %>% 
                             filter(First_Gender == 'female') %>% 
                             select(!First_Gender) %>% 
                             dplyr::rename('Female_Gross' = 'US_Gross'),
                           by='Decade') %>%
  filter(Decade >= 1980)

write.csv(pron_by_dec_flour,'output/pron_by_dec_flour.csv',row.names=FALSE)

dir_and_char <- df %>% 
  meta_stat_summarize(c('Director_Gender','First_Gender'),'US_Gross',add_na_rm(mean),drop_na=TRUE) %>% 
  select(Director_Gender,First_Gender,US_Gross) %>%
  arrange(-US_Gross)

dir_and_char['Key'] <- paste(dir_and_char$Director_Gender,'_',dir_and_char$First_Gender,sep='')
dir_and_char['Nicename'] <- paste('By ',dir_and_char$Director_Gender,' about ',dir_and_char$First_Gender,sep='')
dir_and_char$Nicename <- gsub(' male',' men',dir_and_char$Nicename)
dir_and_char$Nicename <- gsub(' female',' women',dir_and_char$Nicename)

write.csv(dir_and_char,'output/dir_and_char.csv',row.names=FALSE)

df %>% 
  meta_stat_summarize(c('Director_Gender','First_Gender'),'US_Gross',length,drop_na=TRUE) %>% 
  select(Director_Gender,First_Gender,US_Gross) %>%
  arrange(-US_Gross)

beeswarm_df <- df %>% filter(year(Release_Date) >= 1980) %>%
  filter(!is.na(US_Gross)) %>%
  filter(!is.na(First_Gender)) %>%
  select(Title,Director,US_Gross,Critics,Audiences,Director_Gender,First_Gender)

write.csv(beeswarm_df,'output/beeswarm_df.csv',row.names=FALSE)
