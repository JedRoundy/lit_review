# Required libraries
library(httr)
library(jsonlite)
library(tibble)
library(stringr)
library(pdftools)
library(here)

##Changed api key to a source file to upload it to github
source('lit_api_key.R')

# Function to extract citation from a paper (assumes PDF parsing to extract APA citation)
extract_citation <- function(file_path) {
  pdf_info <- pdf_info(file_path)
  
  # Construct citation in APA format using available metadata
  if (!is.null(pdf_info$author) && !is.null(pdf_info$created) && !is.null(pdf_info$title)) {
    citation <- paste(pdf_info$author, "(", format(pdf_info$created, "%Y"), ").", pdf_info$title, ".")
  } else {
    # If metadata is not available, fallback to using file name as a citation
    citation <- paste("Unknown Author. (n.d.).", basename(file_path))
  }
  
  return(citation)
}

# Function to send file text to OpenAI and extract responses
send_paper_to_openai <- function(file_text) {
  # API URL and headers
  ##Edited url to point to generic endpoint, model specified in payload parameters
  url <- 'https://api.openai.com/v1/chat/completions'
  #url <- "https://api.openai.com/v1/engines/gpt-4/completions"
  ##removed headers here and added them below
  
  
  ##Split prompt into system prompt and user prompt. 
  ##Also includes prompting for JSON format
  system_prompt <- paste(
    "You will be provided with a paper that has been published in an academic finance journal.",##Added
    "You are responsible to analyze the paper, and answer questions asked about it.",##minor edits
    "The following is the JSON format that should be used for the respective questions.\n",##Added this line
    "Yes/True will be represented by 1, and No/False will be represented by 0\n",##Added this line
    "{Q1: 1/0, Q2: 1/0, Q3: 1/0, Q4: 1/0, Q5: 1/0, Q6: 1/0, Q7: 1/0}", ##added this line
    "Please analyze the following paper and provide answers to the following questions:\n",
    "Q1: Is one of the IV specifications overidentified?)\n",
    "Q2: Do the authors report the 2SLS coefficient with just identified models?\n",
    "Q3: Do the authors include an assortment of control variables in the system of equations?\n",
    "Q4: Do the authors present results with increasing numbers of control variables?\n",
    "Q5: Do the authors present results without any control variables?\n",
    "Q6: Is the exclusion condition discussed?\n",
    "Q7: Do the authors consider the possibility that the control variables themselves may be endogenous?\n"
  )


  
  ##Edited Payload structure to allow for difference in System prompts and User prompts
  ## as well as include json structuring
  ## kept max_completion benchmark and temperature of .5
  payload <- list(
    model = 'gpt-4o',
    temperature = .5,
    max_completion_tokens = 1500,
    n = 1,
    response_format = list(
      type = 'json_object'
    ),
    messages = list(
      list(role = 'system', content = system_prompt),
      list(role = 'user', content = file_text)
    )
  )
  
  ##edited this, primarily using add_headers within function instead of assigning headers and then referencing object
  ## for some reason, it doesn't like to reference the object (have experienced this everywhere else)
  # Send POST request
  response <- POST(url, 
                   body = toJSON(payload, auto_unbox = TRUE), 
                   add_headers(
    `Content-Type` = "application/json",
    `Authorization` = paste("Bearer", api_key)
    ), 
    content_type_json())
  
  ## Use as = 'parsed' function instead of as= 'text'
  ## doing this removes fromJSON() outside func
  response <- content(response, as = 'parsed')
  
  ##parses the remaining path to the response, and parses the API JSON reponse
  content_list <- fromJSON(response$choices[[1]]$message$content)
  
  ##return response for testing
  return(content_list)
}


# Function to analyze a collection of papers in a folder
analyze_papers <- function(folder_path) {
  # Create an empty dataframe to store results
  ## edited this to reflect simplified JSON response from API
  results <- tibble(
    Paper = character(),
    Is_IV_overidentified = integer(),
    Report_2SLS_Just_Identified = integer(),
    Assortment_of_Controls = integer(),
    Increasing_Controls = integer(),
    Without_Controls = integer(),
    Exclusion_Condition_Discussed = integer(),
    Endogeneity_of_Controls = integer(),
  )
  
  # Loop over each file in the folder
  files <- list.files(folder_path, pattern = "*.pdf", full.names = TRUE)
  
  for (file in files) {
    # Extract citation in APA format
    citation <- extract_citation(file)
    
    # Extract the text from the PDF file
    file_text <- pdf_text(file)
    
    ## REMOVED - START HERE -- file_text is a list -- why is 5000 characters a cutoff?
    ## Pattern to remove "Downloaded from.... Creative Commons License" from each page. 
    pattern_to_remove <- ".{315}Creative Commons License"
    
    ##Replace all occurrences in given list of strings
    file_text_stripped <- lapply(file_text, function(x) str_replace_all(x, pattern = pattern_to_remove, ""))
    
    ##REMOVED -  If the paper is too long, take only the first part (for API token limits)
    
    
    ##Append all the text together
    appended_file_text <- paste(file_text_stripped, collapse = '')
    
    
    # Send the extracted text to OpenAI for analysis
    ##edited slightly to reflect correct object reference
    analysis <- send_paper_to_openai(appended_file_text)
    
    ## REMOVED - Parse the OpenAI response based on question labels (Q1, Q2, etc.)
    ## After converting to JSON outputs, object returned is now a list
    
    ## REMOVED -  Ensure split_analysis has the correct number of answers
    ## No longer necessary due to JSON outputs
    
    # Append the answers to the results dataframe
    ##Edited to reflect fewer columns and removed split_ from analysis, as results can be directly imported from response. 
    results <- add_row(
      results,
      Paper = citation,
      Is_IV_overidentified = analysis[[1]],
      Report_2SLS_Just_Identified = analysis[[2]],
      Assortment_of_Controls = analysis[[3]],
      Increasing_Controls = analysis[[4]],
      Without_Controls = analysis[[5]],
      Exclusion_Condition_Discussed = analysis[[6]],
      Endogeneity_of_Controls = analysis[[7]],
    )
  }
  
  return(results)
}

# Analyze papers in a specified folder
folder_path <- here("Journal_of_Finance_PDFs") #"/path/to/your/folder"  # Replace with your folder path
#folder_path <- here("1. JF Papers") #"/path/to/your/folder"  # Replace with your folder path
analysis_results <- analyze_papers(folder_path)

# Print the results
print(analysis_results)

# Save the results as a CSV file
write.csv(analysis_results, "paper_analysis_results.csv", row.names = FALSE)
