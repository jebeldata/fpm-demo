# This code was authored by Kevin Corti in May 2026. 
# It reflects the R analysis workflow used for the MethodsX article: 
# "Building factorial prompt matrices using query targets and configurable factor classes: 
# A methods framework for conversational AI response behavior research". 

# This code was executed in R Studio. 

# Tool disclosures: Portions of this code were produced with the assistance of 
# Anthropic's Claude Sonnet 4.6, particularly for de-bugging, consistency, 
# formatting, and readability purposes.

# Contact: research@jebeldata.com
################################################################################

# Start by clearing environment
rm(list = ls()) 

# Load required libraries up-front: 
library(lme4)
library(plyr)
library(emmeans)
library(jsonlite)
library(stringr)
library(binom)

# Load data for each demo: 
d1 = read.csv(file.choose()) # Demo 1 .csv file
d2 = read.csv(file.choose()) # Demo 2 .csv file
d3 = read.csv(file.choose()) # Demo 3 .csv file
d4 = read.csv(file.choose()) # Demo 4 .csv file
d5 = read.csv(file.choose()) # Demo 5 .csv file



################################################################################
####  Demo 1:

d1$WebSearchUsed1 = ifelse(d1$WebSearchUsed == 'True', 1, 0)

#### Summary stats: 
summary_d1 =
  ddply(d1, .(ModelName), summarise,
        N = length(Title),
        WebSearchCount = sum(WebSearchUsed1),
        PWebSearch = WebSearchCount / N,
        
        # Wilson confidence intervals (for handling extremes):
        CI_Lower = binom.confint(WebSearchCount, N, method = "wilson")$lower,
        CI_Upper = binom.confint(WebSearchCount, N, method = "wilson")$upper,
        
        # As %s: 
        PercentWebSearch = round(PWebSearch * 100, 1),
        Percent_CI_Lower = round(CI_Lower * 100, 1),
        Percent_CI_Upper = round(CI_Upper * 100, 1)
  )

####  Pairwise tests: 
models = summary_d1$ModelName
results = data.frame()

for (i in 1:(length(models)-1)) {
  for (j in (i+1):length(models)) {
    # Extract counts and sample sizes for the pair
    x = c(summary_d1$WebSearchCount[i], 
          summary_d1$WebSearchCount[j])
    n = c(summary_d1$N[i], 
          summary_d1$N[j])
    
    test = prop.test(x, n)
    
    results = rbind(results, data.frame(
      Comparison = paste(models[i], "vs", models[j]),
      p_value = test$p.value,
      stringsAsFactors = FALSE
    ))
  }
}

# Apply Bonferroni correction: 
results$p_adjusted = p.adjust(results$p_value, method = "bonferroni")

# Full results: 
summary_d1
results



################################################################################
####  Demo 2:


d2$WebSearchUsed1 = ifelse(d2$WebSearchUsed == 'True', 1, 0)

# Create a Condition column based on the factor combinations: 
d2$Condition = interaction(d2$SystemContentLevelLabels, 
                           d2$DeveloperContentLevelLabels, 
                           d2$SuffixPhraseLevelLabels,
                           drop = TRUE)

# Convert to numeric (1, 2, 3, 4): 
d2$Condition = as.numeric(as.factor(d2$Condition))

#### Summary stats: 
summary_d2 = 
  ddply(d2, .(Condition), summarise,
        N = length(Title),
        WebSearchCount = sum(WebSearchUsed1),
        PWebSearch = WebSearchCount / N,
        
        # Wilson confidence intervals (for handling extremes):
        CI_Lower = binom.confint(WebSearchCount, N, method = "wilson")$lower,
        CI_Upper = binom.confint(WebSearchCount, N, method = "wilson")$upper,
        
        # As %s: 
        PercentWebSearch = round(PWebSearch * 100, 1),
        Percent_CI_Lower = round(CI_Lower * 100, 1),
        Percent_CI_Upper = round(CI_Upper * 100, 1)
  )

#### Pairwise tests:
Condition2 = summary_d2$Condition
results2 = data.frame()

for (i in 1:(length(Condition2) - 1)) {
  for (j in (i + 1):length(Condition2)) {
    # Extract counts and sample sizes for the pair
    x = c(summary_d2$WebSearchCount[i], 
          summary_d2$WebSearchCount[j])
    n = c(summary_d2$N[i], 
          summary_d2$N[j])
    
    test = prop.test(x, n)
    
    results2 = rbind(results2, data.frame(
      Comparison = paste("Condition", Condition2[i], "vs", Condition2[j]),
      p_value = test$p.value,
      stringsAsFactors = FALSE
    ))
  }
}

# Bonferroni correction: 
results2$p_adjusted = p.adjust(results2$p_value, method = "bonferroni")

condition_labels2 = c(
  "Query suffix",
  "Developer instruction", 
  "System instruction",
  "Baseline"
)

results2$Comparison = NA
row_num = 1
for (i in 1:3) {
  for (j in (i+1):4) {
    results2$Comparison[row_num] = paste(condition_labels2[i], "vs", condition_labels2[j])
    row_num = row_num + 1
  }
}

# Full results: 
summary_d2
results2


################################################################################
####  Demo 3:

d3$WebSearchUsed1 = ifelse(d3$WebSearchUsed == 'True', 1, 0)

#### Summary stats: 
summary_d3 = 
  ddply(d3, .(ModelName, ReasoningLevel), summarise,
        N = length(Title),
        WebSearchCount = sum(WebSearchUsed1),
        PWebSearch = WebSearchCount / N,
        
        # Wilson confidence intervals (for handling extremes):
        CI_Lower = binom.confint(WebSearchCount, N, method = "wilson")$lower,
        CI_Upper = binom.confint(WebSearchCount, N, method = "wilson")$upper,
        
        # As %s: 
        PercentWebSearch = round(PWebSearch * 100, 1),
        Percent_CI_Lower = round(CI_Lower * 100, 1),
        Percent_CI_Upper = round(CI_Upper * 100, 1)
  )

#### Pairwise tests: 
models3 = summary_d3$ModelName
reasoning3 = summary_d3$ReasoningLevel
results3 = data.frame()

for (i in 1:(nrow(summary_d3) - 1)) {
  for (j in (i + 1):nrow(summary_d3)) {
    # Extract counts and sample sizes for the pair
    x = c(summary_d3$WebSearchCount[i], 
          summary_d3$WebSearchCount[j])
    n = c(summary_d3$N[i], 
          summary_d3$N[j])
    
    test = prop.test(x, n)
    
    results3 = rbind(results3, data.frame(
      Comparison = paste(models3[i], "-", reasoning3[i], "vs", 
                         models3[j], "-", reasoning3[j]),
      p_value = test$p.value,
      stringsAsFactors = FALSE
    ))
  }
} # Warnings are a function of 0% conditions. 

# Bonferroni correction:
results3$p_adjusted = p.adjust(results3$p_value, method = "bonferroni")
results3$p_value[is.nan(results3$p_value)] = 1.0
results3$p_adjusted[is.nan(results3$p_adjusted)] = 1.0

# Full results: 
summary_d3
results3



################################################################################
####  Demo 4:

d4$WebSearchUsed1 = ifelse(d4$WebSearchUsed == 'True', 1, 0)

#### Summary stats: 
summary_d4 = 
  ddply(d4, .(ModelName, ReasoningLevel, DeveloperContentLevelLabels,
              QueryPhraseContentLevelLabels), summarise,
        N = length(Title),
        WebSearchCount = sum(WebSearchUsed1),
        PWebSearch = WebSearchCount / N,
        
        # Wilson confidence intervals (for handling extremes):
        CI_Lower = binom.confint(WebSearchCount, N, method = "wilson")$lower,
        CI_Upper = binom.confint(WebSearchCount, N, method = "wilson")$upper,
        
        # As %s: 
        PercentWebSearch = round(PWebSearch * 100, 1),
        Percent_CI_Lower = round(CI_Lower * 100, 1),
        Percent_CI_Upper = round(CI_Upper * 100, 1)
  )

#### Pairwise proportion tests for Demo 4: 
summary_d4$Condition = 1:nrow(summary_d4)
conditions4 = summary_d4$Condition
results4 = data.frame()

for (i in 1:(length(conditions4) - 1)) {
  for (j in (i + 1):length(conditions4)) {
    x = c(summary_d4$WebSearchCount[i], summary_d4$WebSearchCount[j])
    n = c(summary_d4$N[i], summary_d4$N[j])
    
    test = prop.test(x, n)
    
    results4 = rbind(results4, data.frame(
      Comparison = paste(i, "vs", j),
      p_value = test$p.value,
      stringsAsFactors = FALSE
    ))
  }
} # Warnings are a function of 0% conditions. 

results4$p_adjusted = p.adjust(results4$p_value, method = "bonferroni")
results4$p_value[is.nan(results4$p_value)] = 1.0
results4$p_adjusted[is.nan(results4$p_adjusted)] = 1.0

# Create condition labels
condition_labels = c(
  "Low, None supplied, General",
  "Low, None supplied, Risk factors",
  "Low, Biomedical researcher, General",
  "Low, Biomedical researcher, Risk factors",
  "None, None supplied, General",
  "None, None supplied, Risk factors",
  "None, Biomedical researcher, General",
  "None, Biomedical researcher, Risk factors"
)

results4$Comparison = NA
row_num = 1
for (i in 1:7) {
  for (j in (i+1):8) {
    results4$Comparison[row_num] = paste(condition_labels[i], "vs", condition_labels[j])
    row_num = row_num + 1
  }
}

# Full results: 
summary_d4
results4



################################################################################
####  Demo 5:


d5$WebSearchUsed1 = ifelse(d5$WebSearchUsed == 'True', 1, 0)

# Check that web search is consistent across repetitions
summary_d5_rep = 
  ddply(d5, .(QueryPhraseLanguage, RepetitionIndex), summarise,
        N = length(Title),
        WebSearchCount = sum(WebSearchUsed1),
        PWebSearch = WebSearchCount/N
  )

summary_d5_rep


#### Fit the mixed-effects logistic regression: 
m5 = 
  glmer(WebSearchUsed1 ~ as.factor(QueryPhraseLanguage) + (1 | Title), family = binomial, 
        data = d5) 
# Note: dropped RepetitionIndex as random effect as it was generating warning and
# adds no variance as it is quite consistent, as shown above. 
summary(m5)

# Pairwise tests: 
emm5 = emmeans(m5, ~ QueryPhraseLanguage, type = "response")
emm5

pairwise5 = pairs(emm5, adjust = "bonferroni")
pairwise5



#### Localization analysis: 

# Parse Urls and extract URLs: 
d5$ParsedUrls = lapply(d5$DedupedUrls, function(x) {
  tryCatch(fromJSON(x), error = function(e) character(0))
})

# Extract domain from each URL: 
extract_domain = function(url) {
  str_extract(url, "(?<=://)[^/]+")
}

# Classify domain localization
classify_domain = function(domain) {
  if (is.null(domain) || is.na(domain) || domain == "") return("Other/International")
  
  # Spanish-language / Spanish-speaking country domains
  spanish_patterns = c(
    "\\.es$",        # Spain
    "\\.mx$",        # Mexico
    "\\.ar$",        # Argentina
    "\\.co$",        # Colombia
    "\\.cl$",        # Chile
    "\\.pe$",        # Peru
    "\\.ve$",        # Venezuela
    "\\.ec$",        # Ecuador
    "\\.bo$",        # Bolivia
    "\\.py$",        # Paraguay
    "\\.uy$",        # Uruguay
    "\\.cr$",        # Costa Rica
    "\\.gt$",        # Guatemala
    "\\.hn$",        # Honduras
    "\\.ni$",        # Nicaragua
    "\\.sv$",        # El Salvador
    "\\.do$",        # Dominican Republic
    "\\.cu$",        # Cuba
    "\\.pa$",        # Panama
    "\\.pr$",        # Puerto Rico
    "\\.(es|mx|ar|co|cl|pe)\\..*$"  # subdomains
  )
  
  # Kazakh / Central Asian / Russian domains
  kazakh_patterns = c(
    "\\.kz$",        # Kazakhstan
    "\\.ru$",        # Russia (common for Kazakh content)
    "\\.uz$",        # Uzbekistan
    "\\.kg$",        # Kyrgyzstan
    "\\.tj$",        # Tajikistan
    "\\.tm$",        # Turkmenistan
    "e-history\\.kz",
    "kazmed\\.kz"
  )
  
  if (any(sapply(spanish_patterns, function(p) str_detect(domain, p)))) {
    return("Spanish-localized")
  } else if (any(sapply(kazakh_patterns, function(p) str_detect(domain, p)))) {
    return("Kazakh-localized")
  } else {
    return("Other/International")
  }
}

# Apply classification to each row
d5$LocalizedDomainCount = sapply(d5$ParsedUrls, function(urls) {
  if (length(urls) == 0) return(0)
  domains = extract_domain(urls)
  classifications = sapply(domains, classify_domain)
  sum(classifications %in% c("Spanish-localized", "Kazakh-localized"))
})

d5$SpanishDomainCount = sapply(d5$ParsedUrls, function(urls) {
  if (length(urls) == 0) return(0)
  domains = extract_domain(urls)
  classifications = sapply(domains, classify_domain)
  sum(classifications == "Spanish-localized")
})

d5$KazakhDomainCount = sapply(d5$ParsedUrls, function(urls) {
  if (length(urls) == 0) return(0)
  domains = extract_domain(urls)
  classifications = sapply(domains, classify_domain)
  sum(classifications == "Kazakh-localized")
})

# Binary localized flag (at least one localized URL)
d5$HasLocalizedUrl = as.integer(d5$LocalizedDomainCount > 0)
d5$HasSpanishUrl   = as.integer(d5$SpanishDomainCount > 0)
d5$HasKazakhUrl    = as.integer(d5$KazakhDomainCount > 0)


# Total URLs across entire dataset: 
d5$UrlCount = sapply(d5$ParsedUrls, length)

# Extract all URLs with their language condition
all_urls_df = do.call(rbind, lapply(1:nrow(d5), function(i) {
  urls = d5$ParsedUrls[[i]]
  if (length(urls) == 0) return(NULL)
  data.frame(
    QueryPhraseLanguage = d5$QueryPhraseLanguage[i],
    URL = urls,
    stringsAsFactors = FALSE
  )
}))

# Global deduplication (to see total URLs returned): 
unique_urls = unique(all_urls_df$URL)

# Classify each unique URL: 
unique_url_classifications = sapply(unique_urls, function(url) {
  domain = extract_domain(url)
  classify_domain(domain)
})

# Summary:
summary_d5_localization = 
  ddply(d5, .(QueryPhraseLanguage), summarise,
        N = length(Title),
        WebSearchCount = sum(WebSearchUsed1),
        PWebSearch = WebSearchCount / N,
        
        # Wilson confidence intervals (better behaved at 0% and 100%):
        CI_Lower = binom.confint(WebSearchCount, N, method = "wilson")$lower,
        CI_Upper = binom.confint(WebSearchCount, N, method = "wilson")$upper,
        
        # As %s: 
        PercentWebSearch = round(PWebSearch * 100, 1),
        PercentWebSearch_CI_Lower = round(CI_Lower * 100, 1),
        PercentWebSearch_CI_Upper = round(CI_Upper * 100, 1)
  )
summary_d5_localization

urls_by_language = do.call(rbind, lapply(split(all_urls_df, all_urls_df$QueryPhraseLanguage), function(df) {
  unique_lang_urls = unique(df$URL)
  domains = extract_domain(unique_lang_urls)
  classifications = sapply(domains, classify_domain)
  data.frame(
    QueryPhraseLanguage = df$QueryPhraseLanguage[1],
    TotalUniqueURLs = length(unique_lang_urls),
    SpanishLocalizedURLs = sum(classifications == "Spanish-localized"),
    KazakhLocalizedURLs = sum(classifications == "Kazakh-localized"),
    TotalLocalizedURLs = sum(classifications %in% c("Spanish-localized", "Kazakh-localized")),
    stringsAsFactors = FALSE
  )
}))
urls_by_language

