
 #Plotting the 30-day mortality rates for heart attack

# Step 1: Read the dataset
outcome <- read.csv("C:/Users/prana/Downloads/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")

# Step 2: Convert the 11th column (Heart Attack mortality) to numeric
outcome[, 11] <- as.numeric(outcome[, 11])

# Step 3: Plot the histogram
hist(outcome[, 11], main = "30-Day Mortality Rates from Heart Attack", xlab = "Mortality Rate", col = "blue")

#2. Finding the best hospital in a state

best <- function(state, outcome_name) {
  # Read the data
  outcome <- read.csv("C:/Users/prana/Downloads/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  # Check for valid state
  if (!state %in% outcome$State) {
    stop("invalid state")
  }
  
  # Check for valid outcome
  outcomes <- list(
    "heart attack" = 11,
    "heart failure" = 17,
    "pneumonia" = 23
  )
  
  if (!outcome_name %in% names(outcomes)) {
    stop("invalid outcome")
  }
  
  # Filter the data for the given state and outcome
  state_data <- outcome[outcome$State == state, ]
  
  # Convert the outcome column to numeric, ignoring warnings
  state_data[, outcomes[[outcome_name]]] <- as.numeric(state_data[, outcomes[[outcome_name]]])
  
  # Remove rows with NA values
  state_data <- state_data[!is.na(state_data[, outcomes[[outcome_name]]]), ]
  
  # Find the hospital with the lowest 30-day mortality rate
  best_hospital <- state_data[which.min(state_data[, outcomes[[outcome_name]]]), "Hospital.Name"]
  
  return(best_hospital)
}

# Example usage:
best("TX", "heart attack")

#3. Ranking hospitals by outcome in a state

rankhospital <- function(state, outcome_name, num = "best") {
  # Read the data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check for valid state
  if (!state %in% outcome$State) {
    stop("invalid state")
  }
  
  # Check for valid outcome
  outcomes <- list(
    "heart attack" = 11,
    "heart failure" = 17,
    "pneumonia" = 23
  )
  
  if (!outcome_name %in% names(outcomes)) {
    stop("invalid outcome")
  }
  
  # Filter the data for the given state and outcome
  state_data <- outcome[outcome$State == state, ]
  
  # Convert the outcome column to numeric, ignoring warnings
  state_data[, outcomes[[outcome_name]]] <- as.numeric(state_data[, outcomes[[outcome_name]]])
  
  # Remove rows with NA values
  state_data <- state_data[!is.na(state_data[, outcomes[[outcome_name]]]), ]
  
  # Rank the hospitals by the outcome (ascending order)
  ranked_data <- state_data[order(state_data[, outcomes[[outcome_name]]], state_data$Hospital.Name), ]
  
  # Handle num parameter
  if (num == "best") {
    return(ranked_data[1, "Hospital.Name"])
  } else if (num == "worst") {
    return(ranked_data[nrow(ranked_data), "Hospital.Name"])
  } else if (is.numeric(num) && num > 0 && num <= nrow(ranked_data)) {
    return(ranked_data[num, "Hospital.Name"])
  } else {
    return(NA)
  }
}

# Example usage:
rankhospital("MD", "heart failure", 5)


#4. Ranking hospitals in all states
rankall <- function(outcome_name, num = "best") {
  # Read the data
  outcome <- read.csv("C:/Users/prana/Downloads/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  # Check for valid outcome
  outcomes <- list(
    "heart attack" = 11,
    "heart failure" = 17,
    "pneumonia" = 23
  )
  
  if (!outcome_name %in% names(outcomes)) {
    stop("invalid outcome")
  }
  
  # Convert the relevant column to numeric
  outcome[, outcomes[[outcome_name]]] <- as.numeric(outcome[, outcomes[[outcome_name]]])
  
  # Split the data by state
  state_list <- split(outcome, outcome$State)
  
  # Initialize an empty result data frame
  result <- data.frame(hospital = character(), state = character())
  
  # Loop over each state and rank the hospitals
  for (state in names(state_list)) {
    state_data <- state_list[[state]]
    
    # Remove NA values
    state_data <- state_data[!is.na(state_data[, outcomes[[outcome_name]]]), ]
    
    # Rank the hospitals
    ranked_data <- state_data[order(state_data[, outcomes[[outcome_name]]], state_data$Hospital.Name), ]
    
    # Get the hospital based on num
    if (num == "best") {
      hospital <- ranked_data[1, "Hospital.Name"]
    } else if (num == "worst") {
      hospital <- ranked_data[nrow(ranked_data), "Hospital.Name"]
    } else if (is.numeric(num) && num > 0 && num <= nrow(ranked_data)) {
      hospital <- ranked_data[num, "Hospital.Name"]
    } else {
      hospital <- NA
    }
    
    # Append to result
    result <- rbind(result, data.frame(hospital = hospital, state = state))
  }
  
  return(result)
}


# Example usage:
rankall("heart attack", 20)


