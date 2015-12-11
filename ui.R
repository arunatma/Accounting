library(shiny)
library(rCharts)

shinyUI(navbarPage("Accounts", id = "acts",
    tabPanel("Transaction Entry",
        sidebarPanel(
            h4("Enter Transaction:"),
            dateInput("entryDate", "Date"),
            selectizeInput("fromAcc", "From", 
                choices = c(), options = list(create = TRUE)),
            selectizeInput("toAcc", "To",
                choices = c(), options = list(create = TRUE)),
            textInput("desc", "Description"),
            numericInput("amount", "Amount", 0, 0, 1000000),
            actionButton("butAdd", "Add"),
            textOutput("addButNum")
        ),
        mainPanel(
            dataTableOutput("hotTxnsTable")
        )
    ),
    tabPanel("Transactions Table",
        sidebarPanel(
            wellPanel(
                h4("View Transactions:"),
                dateInput("dtViewFrom", "From Date"),
                dateInput("dtViewTo", "To Date")
            ),
            wellPanel(
                h4("Delete Transaction:"),
                selectizeInput("delTimeStamp", "TimeStamp", choices = c()),
                actionButton("butDel", "Delete")
            )
        ),
        mainPanel(
            dataTableOutput("txnsTable")
        )
    ),
    tabPanel("Account Statement",
        sidebarPanel(
            h4("Get Statement:"),
            radioButtons("radFormat", "View Format", 
                choices = c("Overall", "Individual")),
            selectizeInput("stmtAcc", "Account", choices = c()),
            dateInput("dtStmtFrom", "From Date"),
            dateInput("dtStmtTo", "To Date")
        ),
        mainPanel(
            dataTableOutput("stmtTable")
        )
    )
    
))

