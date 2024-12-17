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
    "{Q1: Yes/No, Q2: Yes/No, Q3: Yes/No, Q4: Yes/No, Q5: Yes/No, Q6: Yes/No, Q7: Yes/No}", ##and this line
    "Please analyze the following paper and provide answers to the following questions:\n",
    "Q1: Is one of the IV specifications overidentified?)\n",
    "Q2: Do the authors report the 2SLS coefficient with just identified models?\n",
    "Q3: Do the authors include an assortment of control variables in the system of equations?\n",
    "Q4: Do the authors present results with increasing numbers of control variables?\n",
    "Q5: Do the authors present results without any control variables?\n",
    "Q6: Is the exclusion condition discussed?\n",
    "Q7: Do the authors consider the possibility that the control variables themselves may be endogenous?\n"
  )
  # 
  # # Prompt with specific questions
  # prompt <- paste(
  #   "Please analyze the following paper and provide the answers in the following JSON format:\n",
  #   "Q1: Is one of the IV specifications overidentified? (Yes/No)\n",
  #   "Q2: Do the authors report the 2SLS coefficient with just identified models? (Yes/No)\n",
  #   "Q3: Do the authors include an assortment of control variables in the system of equations?\n",
  #   "Q4: Do the authors present results with increasing numbers of control variables?\n",
  #   "Q5: Do the authors present results without any control variables?\n",
  #   "Q6: Is the exclusion condition discussed?\n",
  #   "Q7: Do the authors consider the possibility that the control variables themselves may be endogenous?\n",
  #   "The text of the paper follows:\n", 
  #   file_text
  # )

  
  ###Edited Payload structure to allow for difference in System prompts and User prompts
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
  
  
  # Send POST request
  response <- POST(url, 
                   body = toJSON(payload, auto_unbox = TRUE), 
                   add_headers(
    `Content-Type` = "application/json",
    `Authorization` = paste("Bearer", api_key)
    ), 
    content_type_json())
  
  # Parse the response
  result <- fromJSON(content(response, as = "text"))
  
  # Return the answer text
  #return(result$choices[[1]]$text)
  
  ##return response for testing
  return(response)
}

# Function to analyze a collection of papers in a folder
analyze_papers <- function(folder_path) {
  # Create an empty dataframe to store results
  results <- tibble(
    Paper = character(),
    Is_IV_overidentified = character(),
    Binary_IV_overidentified = integer(),
    Report_2SLS_Just_Identified = character(),
    Binary_2SLS_Just_Identified = integer(),
    Assortment_of_Controls = character(),
    Binary_Assortment_of_Controls = integer(),
    Increasing_Controls = character(),
    Binary_Increasing_Controls = integer(),
    Without_Controls = character(),
    Binary_Without_Controls = integer(),
    Exclusion_Condition_Discussed = character(),
    Binary_Exclusion_Condition = integer(),
    Endogeneity_of_Controls = character(),
    Binary_Endogeneity_of_Controls = integer()
  )
  
  # Loop over each file in the folder
  files <- list.files(folder_path, pattern = "*.pdf", full.names = TRUE)
  
  for (file in files) {
    # Extract citation in APA format
    citation <- extract_citation(file)
    
    # Extract the text from the PDF file
    file_text <- pdf_text(file)
    
    ## START HERE -- file_text is a list -- why is 5000 characters a cutoff?
    
    # If the paper is too long, take only the first part (for API token limits)
    if (nchar(file_text) > 5000) {
      file_text <- substr(file_text, 1, 5000)
    }
    
    # Send the extracted text to OpenAI for analysis
    analysis <- send_paper_to_openai(file_text)
    
    # Parse the OpenAI response based on question labels (Q1, Q2, etc.)
    split_analysis <- str_split(analysis, "Q[0-9]:")[[1]]
    
    # Ensure split_analysis has the correct number of answers
    if (length(split_analysis) < 8) {
      split_analysis <- c(split_analysis, rep("No Answer", 8 - length(split_analysis)))
    }
    
    # Append the answers to the results dataframe
    results <- add_row(
      results,
      Paper = citation,
      Is_IV_overidentified = split_analysis[2],
      Binary_IV_overidentified = ifelse(grepl("yes", split_analysis[2], ignore.case = TRUE), 1, 0),
      Report_2SLS_Just_Identified = split_analysis[3],
      Binary_2SLS_Just_Identified = ifelse(grepl("yes", split_analysis[3], ignore.case = TRUE), 1, 0),
      Assortment_of_Controls = split_analysis[4],
      Binary_Assortment_of_Controls = ifelse(grepl("yes", split_analysis[4], ignore.case = TRUE), 1, 0),
      Increasing_Controls = split_analysis[5],
      Binary_Increasing_Controls = ifelse(grepl("yes", split_analysis[5], ignore.case = TRUE), 1, 0),
      Without_Controls = split_analysis[6],
      Binary_Without_Controls = ifelse(grepl("yes", split_analysis[6], ignore.case = TRUE), 1, 0),
      Exclusion_Condition_Discussed = split_analysis[7],
      Binary_Exclusion_Condition = ifelse(grepl("yes", split_analysis[7], ignore.case = TRUE), 1, 0),
      Endogeneity_of_Controls = split_analysis[8],
      Binary_Endogeneity_of_Controls = ifelse(grepl("yes", split_analysis[8], ignore.case = TRUE), 1, 0)
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