# The Silver Ceiling
An analysis of gendered profit margins in the film industry

## Aim
This analysis used webscraping techniques to obtain more than 10,000 film records from the rankings site Rotten Tomatoes,
and analyzed their profits with respect to the genders of the director and the first character mentioned in the synopsis.

## Findings
The analysis found that film's directed by women make less than those directed by men, and those starring women make
less than those starring men. In combination, the average profits of films by men about men more than doubled the average
profits of films by women about women.

## Data Collection
RottenTomatoes.com is a movie rankings site which stores individual films under unique URLs. Each film page contains information about the film, including critical (Critics %) and popular (Audience %) reviews, a synposis, the film's U.S. gross in USD, and a list of five related films.

A spider scraper was fed an initial list of 300 seed urls taken from Rotten Tomatoes editorial list of the best movies of all time. Upon visiting each page, the scraper saved relevant information about a film to a local disk. It then collected the URLs of that page's five 'related' films and added them to the bottom of the stack for further scraping. After each iteration, the stack was refined to remove URLs that had already been visited.

This method produced a 'spiderweb' growth pattern that organically visited thousands of films reachable from the initial set before eventually converging when the remaining stack reached 0. While this method did not guarantee an even pass over all Rotten Tomatoes pages, it provided a large sample of films within the general breadth of culturally relevant films.

## Data Analysis
Data analysis was performed in R, with directors' first names converted to genders using the SSA method. The original 10,723 records were reduced to 10,213 rows with usable data. The first English-language pronoun occurring in each film synopsis was extracted and
converted to a gender, approximating the gender of the film's primary character. This was used as a second vector of analysis.

## New Skills
This project made use of read_html_live, an extension of the rvest library that enables the scraping of dynamically loaded JavaScript
pages. The extension was developed by Hadley Wickham and is currently in the experimental stage of its lifecycle. This was my first 
time using it, and I had to implement some duck type solutions for a memory leak and other bugs common to this type of release.

## Further Development
This scraping method also collected relational data connecting films to one another. Building a network graph of films
using an interactive tool like SigmaJS would be an interesting extension.