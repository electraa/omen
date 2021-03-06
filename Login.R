# This script is modified by Jason Bryer (jason@bryer.org) from Huidong Tian's 
# original script. The blog post describing the method is here:
# http://withr.me/authentication-of-shiny-server-application-using-a-simple-method/ 
# The original R script is located here: https://gist.github.com/withr/9001831
#
# This script adds two new features: 1. Render a logout button, and 2. provide
# the ability for visitors to create a new account.
#
# Within your server.R file, be sure to use:
# 
# source('Login.R', local=TRUE)
#
# To use this file, you can add uiOutput('uiLogin'), uiOutput('uiNewAccount'),
# and uiOutput('uiLogout') anywhere in your shiny application. If you wish to
# have part of you application available only to authenticated users, you can
# checked to see if they are logged in with the USER$Logged field (this is a
# logical). Additional, USER$Username and USER$Group will give the username
# and password of the logged in user, respectively.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
conn = dbConnect(MySQL(), user='root', password='password', dbname='omen', host="hostname")
dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}


  conn = dbConnect(MySQL(), user='root', password='password', dbname='omen', host="hostname")
  rs = dbSendQuery(conn, paste0("select * from omen.users "))
  PASSWORD = fetch(rs, n=-1)

# This is the file that contains a data.frame PASSWORD used for authentication.
#users.file <- 'users.rda'

#if(!file.exists(users.file)) {
if(is.null(PASSWORD)){
  # Create initial, empty file, otherwise errors will occur below
  PASSWORD <- data.frame(Username = character(),
                         Password = character(),
                         Group = character(),
                         Email = character(),
                         stringsAsFactors = FALSE)
  #save(PASSWORD, file=users.file)
}

USER <- reactiveValues(Logged = TRUE, 
                       Unique = format(Sys.time(), '%Y%m%d%H%M%S'),
                       Username = "Guest",
                       Email = "omenguest@fsu.gr",
                       Group = NA)

# Password input textbox
passwdInput <- function(inputId, label, value) {
  tagList(
    tags$label(label),
    tags$input(id=inputId, type="password", value=value, class='form-control')
  )
}

# Returns a panel for logging in. 
output$uiLogin <- renderUI({
  wellPanel(
    uiOutput('pass'),
    div(textInput(paste0("username", USER$Unique), 
                  "Username: ", value='')),
    div(passwdInput(paste0("password", USER$Unique), 
                    "Password: ", value='')),
    br(), 
    actionButton("Login", "Login")
  )
})

# Provides a UI for creating an account
output$uiNewAccount <- renderUI({
  wellPanel(
    uiOutput('newuser'),
    div(textInput(paste0("newusername", USER$Unique), 
                  "Username: ", value='')),
    div(passwdInput(paste0("newpassword1", USER$Unique), 
                    "Password: ", value='')),
    div(passwdInput(paste0("newpassword2", USER$Unique), 
                    "Confirm Password: ", value='')),
    div(textInput(paste0('newemail', USER$Unique), 
                  "Email Address: ", value='')),
    br(),
    actionButton("CreateUser", "Create Account")
  )
})

# UI for a logout button
output$uiLogout <- renderUI({
  actionButton('logoutButton', 'Logout')
})

# Log the user out
observeEvent(input$logoutButton, {
  if(!is.null(input$logoutButton) & input$logoutButton == 1) {
    USER$Logged <- FALSE
    USER$Username <- USER$Group <- NA
    USER$Unique <- format(Sys.time(), '%Y%m%d%H%M%S')
    USER$Email <- NA
  }
})

# Add a new user
output$newuser <- renderText({
  conn = dbConnect(MySQL(), user='root', password='password', dbname='omen', host="hostname")
  rs = dbSendQuery(conn, paste0("select * from omen.users "))
  PASSWORD = fetch(rs, n=-1)

  text <- ''
    if(!is.null(input$CreateUser)) {
      if(input$CreateUser > 0) {
        newusername <- input[[paste0('newusername', USER$Unique)]]
        newpassword1 <- input[[paste0('newpassword1', USER$Unique)]]
        newpassword2 <- input[[paste0('newpassword2', USER$Unique)]]
        newemail <- input[[paste0('newemail', USER$Unique)]]
                
        # Validate input fields
        if(is.null(newusername) |
           is.null(newpassword1) |
           is.null(newpassword2) |
           is.null(newemail)) {
          text <- 'Please enter all fields'
        } else if(nchar(newusername) < 5 | 
                  nchar(newpassword1) < 5) {
          text <- 'Please enter username and password with at least 5 characters'
        } else if(newpassword1 != newpassword2) {
          text <- 'Passwords do not match'
        } else if(is.null(newemail) |
                  nchar(newemail) < 5 |
                  grep(".+@.+", newemail) < 1) {
          text <- 'Invalid email address'
        } else if(tolower(newusername) %in% PASSWORD$Username) {
          text <- 'Username already exists'
        } else { # Add the user
          text<-''
          newuser <- data.frame(
            row_names = nrow(PASSWORD) + 1,
            Username = newusername,
            Password = newpassword1,
            Group = 'user',
            Email = newemail
          )
          for(i in names(PASSWORD)[(!names(PASSWORD) %in% names(newuser))]) {
            newuser[,i] <- NA # Make sure the data.frames line up
          }
          PASSWORD <- rbind(PASSWORD, newuser[,names(PASSWORD)])
          #save(PASSWORD, file=users.file)
          USER$Logged <- TRUE
          USER$Username <- newusername
          USER$Group <- "user"
          USER$Email <- newemail
          rs = dbSendQuery(conn, paste0("insert into omen.users values ('",newuser[,c("row_names")],"','",newuser[,c("Username")],"','", newuser[,c("Password")],"','",newuser[,c("Group")],"','",newuser[,c("Email")],"')" ))
          dbDisconnectAll
        }
      }
  }

  text
})

# Log the user in
output$pass <- renderText({
  conn = dbConnect(MySQL(), user='root', password='fsu123fsu', dbname='omen', host="147.102.23.20")
  rs = dbSendQuery(conn, paste0("select * from omen.users "))
  PASSWORD = fetch(rs, n=-1)
  default.group <- 'user' # The value for Group when creating new accounts
  
    if(!is.null(input$Login)) {
      if(input$Login > 0) {
        Username <- isolate(input[[paste0('username', USER$Unique)]])
        Password <- isolate(input[[paste0('password', USER$Unique)]])
        Id.username <- which(PASSWORD$Username == tolower(Username))
        if(!is.null(Id.username) & length(Id.username) == 1 &  Password == PASSWORD[Id.username,]$Password) 
        {
          USER$Logged <- TRUE
          USER$Username <- Username
          USER$Group <- PASSWORD[Id.username,]$Group
          USER$Email <- PASSWORD[Id.username,]$Email
        } else  {
          "Username or password failed!"
        }
      } 
    }
})
