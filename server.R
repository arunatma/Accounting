library(shiny)
library(shinyTable)


transact <- function (accounts, x, y, amt){
    xBal = accounts[(accounts$name == x), ]$balance
    yBal = accounts[(accounts$name == y), ]$balance
    
    accounts[(accounts$name == x), ]$balance = xBal + amt
    accounts[(accounts$name == y), ]$balance = yBal - amt
    return (accounts)
}

xAcc = data.frame(name = "arun", balance = 2000)
yAcc = data.frame(name = "ram", balance = 8500)
accounts = rbind(xAcc, yAcc)

amt = 500
x = "arun"; y = "ram"
accounts = transact(accounts, x, y, amt)

accountNames = read.csv ("accounts.csv", stringsAsFactors = FALSE)
addAccount = function(accountNames, name){
    accountNames = rbind(accountNames, name)
    write.csv(accountNames, "accounts.csv", quote=FALSE, row.names=FALSE)
    return(accountNames)
}

# returns the modified accountNames, after writing the same to the file
# accountNames = addAccount(accountNames, "eee")

addTransaction = function(transactions, inDate, desc, credit, debit, amt,
    fileWrite = TRUE){
    curTxn = data.frame(Date=inDate, Stamp=as.numeric(Sys.time()),
        Description=desc, Credit=credit, Debit=debit, 
        Amount=amt, stringsAsFactors=FALSE)
    transactions = rbind(transactions, curTxn)
    if(fileWrite){
        write.csv(transactions, "transactions.csv", quote=FALSE, 
            row.names=FALSE)
    }
    
    return(transactions)
}

deleteTransaction = function(transactions, timeStamp){
    print(transactions)
    transactions <- transactions[transactions$Stamp != timeStamp, ]
    print("AfterDelete:")
    print(transactions)
    write.csv(transactions, "transactions.csv", quote=FALSE, 
            row.names=FALSE)
    return(transactions)
}

performTransactions = function() {
    transactions = read.csv ("transactions.csv", stringsAsFactors = FALSE)

    txnDate = as.Date("2015-01-01")
    transactions = addTransaction(transactions, txnDate, "description",
        "arun", "ram", 50)
    transactions = addTransaction(transactions, txnDate + 3, "description",
    "arun", "ram", 500)
    transactions = addTransaction(transactions, txnDate + 5, "description",
        "arun", "xxx", 60)    
    transactions = addTransaction(transactions, txnDate - 23, "description",
        "xxx", "yyy", 70)      
    transactions = addTransaction(transactions, txnDate + 95, "description",
        "zzz", "xxx", 90)    
    transactions = addTransaction(transactions, txnDate + 125, "description",
        "arun", "zzz", 200)     
    transactions = addTransaction(transactions, txnDate + 200, "description",
        "ram", "zzz", 400)    
    transactions = addTransaction(transactions, txnDate + 150, "description",
        "yyy", "zzz", 80)
    transactions = addTransaction(transactions, txnDate + 225, "description",
        "zzz", "arun", 80)     
    transactions = addTransaction(transactions, txnDate + 160, "description",
        "arun", "zzz", 90)     
    transactions = addTransaction(transactions, txnDate - 50, "description",
        "ram", "zzz", 70)      
}
    
getBalance = function(account, transactions, inDate){
    txnDate = as.Date(toString(transactions$Date))
    periodTxns = transactions[txnDate <= as.Date(inDate), ]
    credits = sum(periodTxns[periodTxns$Credit == account, ]$Amount)
    debits = sum(periodTxns[periodTxns$Debit == account, ]$Amount)
    netCredit = credits - debits
    return(netCredit)
}

getBalanceAgst = function(account1, account2, transactions, inDate){
    txnDate = as.Date(toString(transactions$Date))
    periodTxns = transactions[txnDate <= as.Date(inDate), ]
    credits = sum(periodTxns[(periodTxns$Debit == account1 & 
        periodTxns$Credit == account2), ]$Amount)
    debits = sum(periodTxns[(periodTxns$Credit == account1 & 
        periodTxns$Debit == account2), ]$Amount)
    netCredit = credits - debits
    return(netCredit)
}

getBalanceStamp = function(account, transactions, stamp){
    periodTxns = transactions[transactions$stamp <= stamp, ]
    credits = sum(periodTxns[periodTxns$Credit == account, ]$Amount)
    debits = sum(periodTxns[periodTxns$Debit == account, ]$Amount)
    netCredit = credits - debits
    return(netCredit)
}

# allBalances = data.frame(accountNames, 
#    balance = apply(accountNames, 1, getBalance, transactions, "2015-05-05"))

getLineStatement = function(ctrAcc, account, orderedTxns){
    txns = orderedTxns[orderedTxns$Credit==ctrAcc | orderedTxns$Debit==ctrAcc, ]
    resultStmt = data.frame(Date=txns$Date, Description=txns$Description)
    selector = (txns$Credit == account)
    resultStmt$FromTo = ifelse(selector, txns$Debit, txns$Credit)
    resultStmt$Charge = ifelse(selector, txns$Amount, 0)
    resultStmt$Payment = ifelse(selector, 0, txns$Amount)
    resultStmt$Balance = cumsum(resultStmt$Charge - resultStmt$Payment)
    return(resultStmt)
}     

getSingleStatement = function(account, orderedTxns){
    txns = orderedTxns
    resultStmt = data.frame(Date=txns$Date, Description=txns$Description)
    selector = (txns$Credit == account)
    resultStmt$FromTo = ifelse(selector, txns$Debit, txns$Credit)
    resultStmt$Charge = ifelse(selector, txns$Amount, 0)
    resultStmt$Payment = ifelse(selector, 0, txns$Amount)
    resultStmt$Balance = cumsum(resultStmt$Charge - resultStmt$Payment)
    return(resultStmt)
}     

getOverallStatement = function(account, transactions, fromDate, toDate){
    dtFrom = as.Date(fromDate)
    dtTo = as.Date(toDate) 

    allAcc = unique(c(transactions$Credit, transactions$Debit)) 
    counterAcc = allAcc[allAcc != account]
    dtPrev = (dtFrom - 1)
    openBalance = getBalance(account, transactions, dtPrev)
    
    txnDate = as.Date(toString(transactions$Date))
    periodTxns = transactions[(txnDate >= dtFrom & txnDate <= dtTo), ]
    accountTxns = periodTxns[(periodTxns$Credit == account | 
        periodTxns$Debit == account), ]
    
    accountTxns = addTransaction(accountTxns, toString(dtPrev), 
        "Carried Forward", account, "All Accounts", openBalance, fileWrite=FALSE)
    
    orderedTxns = accountTxns[with(accountTxns, order(Date, -Amount)), ]

    resultStmt = getSingleStatement(account, orderedTxns)
    
 return(resultStmt)
}     

getAccountStatement = function(account, transactions, fromDate, toDate){
    dtFrom = as.Date(fromDate)
    dtTo = as.Date(toDate) 
    
    allAcc = unique(c(transactions$Credit, transactions$Debit)) 
    counterAcc = allAcc[allAcc != account]
    dtPrev = (dtFrom - 1)
    openBals = sapply(counterAcc, getBalanceAgst, account, transactions, dtPrev)
    
    txnDate = as.Date(toString(transactions$Date))
    periodTxns = transactions[(txnDate >= dtFrom & txnDate <= dtTo), ]
    accountTxns = periodTxns[(periodTxns$Credit == account | 
        periodTxns$Debit == account), ]
    
    accountTxns = addTransaction(accountTxns, toString(dtPrev), 
        "Carried Forward", account, counterAcc, openBals, fileWrite=FALSE)
    
    orderedTxns = accountTxns[with(accountTxns, order(Date, -Amount)), ]

    resultStmt = lapply(counterAcc, getLineStatement, account, orderedTxns)
    resultStmt = do.call(rbind, resultStmt)
    resultStmt = resultStmt[with(resultStmt, order(FromTo, Date)), ]
    
 return(resultStmt)
}     

getInterestTxns = function(transactions, fromDate, toDate){
    defStart = as.Date("2010/1/1")
    defEnd = as.Date("2049/1/1")

    monthEnds = seq(defStart, defEnd, "months") - 1
    
    dtFrom = as.Date(fromDate)
    dtTo = as.Date(toDate) 
    inttDates = monthEnds[monthEnds >= dtFrom & monthEnds <= dtTo]
    
    allAcc = unique(c(transactions$Credit, transactions$Debit)) 
    
    numAcc = length(allAcc)
    if (numAcc > 1){
        combo = combn(c(1:numAcc), 2)
        numCombos = dim(combo)[2]
        for (i in c(1:numCombos)) {
            x = combo[,i][1]
            y = combo[,i][2]
            xAcc = allAcc[x]
            yAcc = allAcc[y]
            stmt = getAccountStatement(xAcc, transactions, defStart, defEnd)
            stmt = stmt[stmt$FromTo == yAcc, ]
            print(stmt)
            
            #interestTxns = addTransaction(data.frame(), inttDates, 
            #    "Interest Paid", x, y, inttVals, fileWrite=FALSE)
        }
    }
}


transactions <- read.csv ("transactions.csv", stringsAsFactors = FALSE)
sessionTimeStamp <- as.numeric(Sys.time())
hotTxns <- data.frame()

shinyServer(function(input, output, session) {

    output$stmtTable <- renderDataTable({
        input$butAdd # update the statement when new transactions are added

        if(input$radFormat == "Overall"){
            stmt = getOverallStatement(input$stmtAcc, transactions, 
                input$dtStmtFrom, input$dtStmtTo)
        } else {
            stmt = getAccountStatement(input$stmtAcc, transactions, 
                input$dtStmtFrom, input$dtStmtTo)
        }
            
        return (stmt)
    })
    
    output$txnsTable <- renderDataTable({
        input$butAdd # update viewTxns when new transactions added

        if(input$butDel > 0){
            transactions <<- isolate(
                deleteTransaction(transactions, input$delTimeStamp)
            )
            updateSelectizeInput(session, "delTimeStamp", 
                choices = c("Select", transactions$Stamp))             
        }
        viewTxns = transactions
        if (dim(transactions)[1] < 0){
            txnDate = as.Date(toString(transactions$Date))
            viewTxns = transactions[(txnDate >= input$dtViewFrom &
                txnDate <= input$dtViewTo), ] 
        }
                        
        return (viewTxns)
    })
    
    
    txnRxFn <<- reactive({
        if(input$butAdd > 0){
            transactions <<- isolate(
                addTransaction(transactions, toString(input$entryDate), 
                    input$desc, input$fromAcc, input$toAcc, input$amount)
            )
            
            updateSelectizeInput(session, "delTimeStamp", 
                choices = c("Select", transactions$Stamp))
            
            accounts <<- unique(c(transactions$Credit, transactions$Debit))
            sortedAcc <<- accounts[order(accounts)]

            updateSelectizeInput(session, "fromAcc", choices = sortedAcc)
            updateSelectizeInput(session, "toAcc", choices = sortedAcc)
            updateSelectizeInput(session, "stmtAcc", choices = sortedAcc)
            
        }
        transactions
    })
    
    output$hotTxnsTable <- renderDataTable({
        if(input$butAdd | input$butDel){
            transactions <<- txnRxFn()
            hotTxns <<- transactions[transactions$Stamp > sessionTimeStamp, ]
            print("arunram")
        }
        return(hotTxns)
    })
    
    output$addButNum <- renderText({
        print(transactions)
        return(input$butAdd)
    })
    
})