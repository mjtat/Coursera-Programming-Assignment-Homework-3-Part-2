## Read in the data

outcome_dat<-read.csv("outcome-of-care-measures.csv", header = TRUE)

## Select only the key variables from the original data.
outcome_dat_organized <- data.frame(as.character(outcome_dat$Hospital.Name),as.character(outcome_dat$State), as.double(outcome_dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), as.double(outcome_dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), as.double(outcome_dat$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))

# Rename the variables
names(outcome_dat_organized)<-c("Hospital Name", "State", "Heart Attack", "Heart Failure", "Pneumonia")

# Remove all the NA values.
outcome_dat_organized$Heart_Attack[outcome_dat_organized$Heart_Attack == "Not Available"] <- NA
outcome_dat_organized$Heart_Failure[outcome_dat_organized$Heart_Failure == "Not Available"] <- NA
outcome_dat_organized$Pneumonia[outcome_dat_organized$Pneumonia == "Not Available"] <- NA

outcome_dat_organized <- outcome_dat_organized[complete.cases(outcome_dat_organized),]
outcome_dat_organized <- droplevels(outcome_dat_organized[complete.cases(outcome_dat_organized),])

outcome_matrix <- as.matrix(outcome_dat_organized)

## Create a function to search the lowest mortality rate based on state, and the type of death.

## Note, states must be entered as a two letter CAPITALIZED abbreviation surrounded by quotes
## e.g., "CA", "WA", "MI"

## Options for the type of death are "heart attack", "heart failure", and "pneumonia". These must
## be entered VERBATIM in quotes.

best <- function(state, outcome) {
        
        state_subset <- subset(outcome_dat_organized, State == state)
        
        names(state_subset)<- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        
        state_subset$hospital <- as.character(state_subset$hospital)
        
        outcome <- tolower(outcome)
        
        min <- with(state_subset, state_subset[order(state_subset[[outcome]], state_subset$hospital), ])

        print(min[1,1])
}

## Test the function a few times.

best("CA", "heart failure")

best("OH", "pneumonia")

best("ID", "heart attack")
