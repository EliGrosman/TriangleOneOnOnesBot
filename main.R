#!/usr/bin/env Rscript

# Get command line arguments
args = commandArgs(trailingOnly=TRUE)
MY_TOKEN <- args[1]

library(beakr)
library(httr)
library(rjson)
library(RSQLite)
library(stringr)

# The base Slack API url
baseUrl <- "https://slack.com/api/"

# Record one-on-one dialog
recordDialog <- '{ "callback_id": "oneonone_submit", "title": "Record one-on-one", "submit_label": "Record", "elements": [{"type": "select", "label": "Partner", "name": "user", "data_source": "users", "placeholder": "Who you did your one-on-one with" }, { "type": "text", "label": "comment", "name": "comment", "placeholder": "He is from Seattle, played guitar in high school. CHEM major.", "hint": "Write down a couple things you learned about your brother." }]  }'

# Open database connection
conn <- RSQLite::dbConnect(RSQLite::SQLite(), "db/database.db")


# ----- Utility functions ------------------------------------------------------


# Convert a dataframe to JSON
dfToJSON <- function(df) {
  return(rjson::toJSON(unname(split(df, 1:nrow(df)))))
}


# ----- Database accessing functions -------------------------------------------


# Get a user's fullname from their slackID
getFullname <- function(slackID) {
  q <- dbGetQuery(conn, 'SELECT fullname FROM people WHERE slackID = ?', params = c(slackID))
  return(q[1,])
}

# Get the people <slackID> has not done a one-on-one with
getNext <- function(slackID) {
  q <- dbGetQuery(conn, "SELECT pe.slackID FROM (SELECT slackID FROM people WHERE active = 1 AND slackID != ? ORDER BY fullname ASC) pe WHERE pe.slackID NOT IN (SELECT DISTINCT(p.slackID) FROM people p, records r WHERE (r.slackID1 = ? AND r.slackID2 = pe.slackID) OR (r.slackID1 = p.slackID AND r.slackID2 = ?))", params = c(slackID, slackID, slackID))
  return(q)
}

# Get all of the one-on-ones completed by all users
getOneOnOnes <- function() {
  getOneOnOnes <- dbGetQuery(conn, 'SELECT p.slackID, p.fullname, p.username, COUNT(r1.slackID1) n FROM people p, records r1, records r2 WHERE (r1.slackID1 = r2.slackID2 AND r2.slackID1 = r1.slackID2) AND r1.slackID1 = p.slackID GROUP BY p.slackID ORDER BY n DESC')
  return(getOneOnOnes)
}

# Get the completed one-on-ones for <slackID>
getCompleted <- function(slackID) {
  
  # Get this user's completed one-on-ones
  q = dbGetQuery(conn, "SELECT DISTINCT(p.slackID) FROM people p, records r WHERE (r.slackID1 = ? AND r.slackID2 = p.slackID)", c(slackID))
  
  if(nrow(q) == 0) {
    response <- data.frame(slackID = character(), alsoRecorded = character())
  } else {
    
    # Add a new row to this dataframe, alsoRecorded. Set it to false for each entry
    response <- data.frame(q, alsoRecorded = FALSE)
    
    for(i in 1:nrow(q)) {
      partnerID <- q[i, ]
      
      # Check if this user has also recorded this one-on-one
      didReport = dbGetQuery(conn, "SELECT r.slackID1 FROM records r WHERE (r.slackID1 = ? AND r.slackID2 = ?)", c(partnerID, slackID))
      
      # If they did report, set their entry to true
      if(nrow(didReport) > 0)
        response$alsoRecorded[response$slackID == partnerID] <- TRUE
    }
  }
  return(response)
}



# ----- Database updating functions --------------------------------------------


# Record a one-on-one between user <slackID> and <partnerID>
record <- function(slackID, partnerID, comment, channel) {
  
  # Check if these users are the same
  if(slackID == partnerID) {
    sendEphemeral(channel, slackID, "You cannot record an one-on-one with yourself!")
  } else {
    
    # Check if this one-on-one has already been recorded
    exists <- dbGetQuery(conn, "SELECT * FROM records WHERE slackID1 = ? AND slackID2 = ?", c(slackID, partnerID))
    
    # If it does not, add it to the database
    if(nrow(exists) == 0) {
      
      # One-on-one schema: 
      #     slackID1 (text), slackID2 (text), completedAt (datetime), comment (text)
      args <- c(slackID, partnerID, as.character(lubridate::now(tz = "America/Los_Angeles")), comment)
      
      # Insert this one-on-one into the db
      dbExecute(conn, "INSERT INTO records (slackID1, slackID2, completedDate, comment) VALUES(?, ?, ?, ?)", params = args)
      
      # Send confirmation message
      text <- paste0("Successfully logged your one-on-one with <@", partnerID, ">!")
      sendEphemeral(channel, slackID, text)
    } else {
      
      # If this one-on-one has already been recorded, let the user know
      text <- paste0("It looks like you already logged your one-on-one with <@", partnerID, ">. If this seems like an error please contact Eli.")
      sendEphemeral(channel, slackID, text)
    }
  }
  
}


# ----- Weekly one-on-one recommendations --------------------------------------


# Generate the weekly assignmetns
genWeekly <- function() {
  # reset weekly assignments
  dbExecute(conn, "DELETE FROM weekly")
  
  # Get the users who have completed no one-on-ones
  
  completedNone <- dbGetQuery(conn, "SELECT DISTINCT(p.slackID), p.fullname, COUNT(p.slackID) - 1 count FROM people p WHERE p.active = 1 AND p.slackID NOT IN (SELECT slackID1 FROM records) AND p.slackID NOT IN (SELECT slackID2 FROM records) GROUP BY p.slackID")
  # Get the rest of the users
  completedSome <- dbGetQuery(conn, "SELECT slackID, fullname, COUNT(r.slackID1) count FROM people p, records r WHERE p.active = 1 AND r.slackID1 = p.slackID GROUP BY p.slackID ORDER BY count ASC")
  
  # Combine these with those who have completed none coming first
  # NOTE: This is to prioritize assigning one-on-ones to guys who haven't completed any
  actives <- completedNone %>% rbind(completedSome)
  
  for(i in 1:nrow(actives)) {
    
    active <- actives[i, ]
    activeID <- active$slackID
    
    # Get the number of one-on-ones already assigned
    numAssigned <- dbGetQuery(conn, "SELECT count(*) FROM weekly WHERE p1_slackID = ?", c(activeID))[1,]
    
    # num is the amount of one-on-ones to assign
    # NOTE: If a user is assigned one already, only assign them one more
    num = 2 - numAssigned
    
    # Get two users (at random) to assign 
    twoGuys <- dbGetQuery(conn, "SELECT pe.slackID FROM people pe WHERE pe.active = 1 AND pe.fullname NOT IN (SELECT DISTINCT(p.fullname) FROM people p, records r WHERE (r.slackID1 = ? AND r.slackID2 = p.slackID) OR (r.slackID1 = p.slackID AND r.slackID2 = ?)) AND pe.slackID != ? AND (SELECT COUNT(*) FROM weekly WHERE p1_slackID = pe.slackID) < 2 ORDER BY RANDOM() LIMIT 2", c(activeID, activeID, activeID))
    
    # We want to assign this user the correct amount of one-on-ones 
    # This is 2 (if they aren't assigned any) 
    # 1 (if they're assigned 1) 
    # or 0 (if they're already assigned 2)
    m <- min(nrow(twoGuys), num)
    if(m != 0) {
      for(j in 1:m) {
        partnerID <- twoGuys[j, ]
        dbExecute(conn, "INSERT INTO weekly (p1_slackID, p2_slackID) VALUES (?, ?)", params = c(activeID, partnerID))  
        dbExecute(conn, "INSERT INTO weekly (p1_slackID, p2_slackID) VALUES (?, ?)", params = c(partnerID, activeID))
      }
    }
  }
  return("Generated")
}

# Send the weekly one-on-one assignments 
# NOTE: must run genWeekly() first!
sendWeekly <- function() {
  # Get all of the active members
  actives = dbGetQuery(conn, "SELECT slackID, fullname FROM people WHERE active = 1")
  for(i in 1:nrow(actives)) {
    member <- actives[i,]
    
    # Get this member's assignments
    myTwo = dbGetQuery(conn, "SELECT p2_slackID FROM weekly WHERE p1_slackID = ?", c(member$slackID))
    
    # Warning message
    message <- ""
    
    # Set start of message
    if(nrow(myTwo) == 0)
      message <- paste0(message, "Hi <@", member$slackID, ">! You do not have any recommended one-on-ones this week!\n")
    else
      message <- paste0(message, "Hi <@", member$slackID, ">! This week your recommended one-on-ones are with: \n")
    print(myTwo)
    # Add each assigned user to this message
    for(j in 1:nrow(myTwo))
      message <- paste0(message, "- <@", myTwo[j, ], ">\n")
    
    # Add message ending
    message <- paste0(message, "Again, these are no consequences for not completing these. These are just to help you decide who to do a one-on-one with next! \n Please let <@UDCLQH6EN> know if there are any issues (for example, if you were assigned someone you already did a one-on-one with).")
    
    # Send this message
    sendDM(member$slackID, message)
    
    # Wait 2 seconds
    # NOTE: This is because slack limits the amount of messages sent. 
    #       2 seconds is to be safe.
    Sys.sleep(2)
  }
  return("Sent")
}


# ----- Work with Slack API ----------------------------------------------------


# Process slack slash commands
processCommand <- function(channel_id, user_id, user_name, command, text, trigger_id) {

  if(command == "/record") {
    openDialog(trigger_id, recordDialog)
  } else if(command == "/oneonones") {
    
    # Get one-on-ones leaderboard dataframe
    data <- getOneOnOnes()
    
    # Create message header
    responseText <- '```Rank   Name             Completed 1-on-1s\n'
    
    # Only display the top 10 users
    if(nrow(data) < 10)
      num <- nrow(data)
    else
      num <- 10
    
    for(i in 1:num) {
      # Generate text for this user's entry on the leaderboard
      rank <- paste0(i, ".")
      fullname <- data[i, ]$fullname
      n <- data[i, ]$n
      responseText <- paste0(responseText, str_pad(rank, 7, "right"), str_pad(fullname, 17, "right"), n, "\n")
    }
    
    # Send leaderboard back to user
    responseText <- paste0(responseText, "```")
    sendEphemeral(channel_id, user_id, responseText)
  } else if(command == "/whonext") {
    # Get people this user has not completed a one-on-one with
    data <- getNext(user_id)
    
    # Send this list back to the user
    responseText <- "You have not had a one-on-one with: \n"
    for(i in 1:nrow(data)) {
      responseText <- paste0(responseText, "- <@", data[i, ], "> \n")
    }
    sendEphemeral(channel_id, user_id, responseText)
  } else if(command == "/completed") {
    
    # Get the users this user has completed one-on-ones with (and if they have also recorded)
    data <- getCompleted(user_id)
    
    if(nrow(data) == 0) {
      responseText <- "You have not completed any one-on-ones."
    } else {
      # Create response text
      responseText <- 'You have had one-on-ones with the following. There is a check if they have recorded and an X if not. \n'
      for(i in 1:nrow(data)) {
        if(data[i, ]$alsoRecorded) {
          check <- "✔️"
        } else {
          check <- "❌"
        }
        responseText <- paste0(responseText, "- <@", data[i, ]$slackID, "> ", check, "\n")
      }
    }
    # Send this back to the user
    sendEphemeral(channel_id, user_id, responseText)
  }
  return("")
}

# Open a dialog given a trigger_id
openDialog <- function(trigger_id, dialog) {
  r <- POST(url = paste0(baseUrl, "dialog.open"),
            body = list(token = MY_TOKEN, trigger_id = trigger_id, dialog = dialog))
  return("")
}

# Process slack dialogs
processDialog <- function(payload) {
  json <- rjson::fromJSON(payload)
  
  # Process 'oneonone_submit' dialogs
  if(json$callback_id == "oneonone_submit") {
    
    # 'oneonone_submit' means the user submitted the one-on-one record dialog
    record(json$user$id, json$submission$user, json$submission$comment, json$channel$id)
  }
  
  return("")
}

# Send a message using the slack API
sendMessage <- function(channel, text) {
  r <- POST(url = paste0(baseUrl, "chat.postMessage"), 
            body = list(token = MY_TOKEN, channel = channel, text = text))
  
  return(status_code(r))
}

# Send a ephemeral message using the slack API
sendEphemeral <- function(channel, user, text) {
  r <- POST(url = paste0(baseUrl, "chat.postEphemeral"),
            body = list(token = MY_TOKEN, channel = channel, user = user, text = text))
  
  return(status_code(r))
}

# Send a direct message to a user using the slack API
sendDM <- function(recepient, text) {
  r <- POST(url = paste0(baseUrl, "chat.postMessage"), 
            body = list(token = MY_TOKEN, channel = recepient, text = text, user = recepient))
  
  return(status_code(r))
}


# ----- Create webapp ----------------------------------------------------------


# Create web app with necessary endpoints
beakr <- newBeakr() %>% 
  httpGET(path = "/genWeekly", function(req, res, err) { genWeekly() }) %>%
  httpGET(path = "/sendWeekly", function(req, res, err) { sendWeekly() }) %>%
  httpPOST(path = "/command", decorate(processCommand)) %>%
  httpPOST(path = "/actions", decorate(processDialog)) %>%
  httpGET(path = "/stop", function(req, res, err) {
    RSQLite::dbDisconnect(conn)
    beakr::stopAllServers()
  }) %>%
  handleErrors() %>%
  listen(host = "0.0.0.0", port = 25118) 

