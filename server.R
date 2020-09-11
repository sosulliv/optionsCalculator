#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

####RENDERS
    #MAINFUNCTION CALL AND RETRIEVES SIDE PANE TABLE
    output$res <- renderTable({
        optionsData<-optionsDataFunc(input$search,input$expDate,input$optionType)
        DT<-optionsData %>% select(Contract,Strike, Last,Chg,Bid,Ask)
        #optionsData
        return(DT)
    })
    #RENDERS CONTRACTS LOV
     output$contractList <- renderUI({
       if(contractListFrame()) {
            optData=optionsDataFunc(input$search,input$expDate,input$optionType)
            selectInput("contractList", label = h3("Contract List"), choices = c("", optData$Contract))  
       }
    })
     #RENDERS PARAMETER TABLE
     output$parameters <- renderTable({
         if(analysisFrame()) {
              optData=optionsDataFunc(input$search,input$expDate,input$optionType)
              contractOptionsData <<- optData %>% rowwise() %>%  filter(Contract %in% input$contractList)
              
              Strike<<-contractOptionsData[["Strike"]] ##K Global Variable
              
              Stock<<-getQuote(input$search)$Last ##S Global Variable
              
              OptionPrice<<-contractOptionsData[["Last"]] ##Option Price Global Variable
            data.frame(Parameters=c("Stock Price", "Strike Price", "Option Price", "Days to Expire"),
                                    Values=c(Stock,Strike, OptionPrice,daysInYears(input$expDate)))
             #stop()  
         }
     })
     #RENDERS GREEKS TABLE 
     output$greeks <- renderTable({
         #    stop()
         if(analysisFrame()) {
             d1= calcd1(0.2, Stock, Strike, riskFree, daysInYears(input$expDate)) ###NEED TO UPDATE
             d2= calcd2(0.2, Stock, Strike, riskFree, daysInYears(input$expDate)) ###NEED TO UPDATE
             
             delta=delta(d1,input$optionType)
            data.frame( d1=d1, d2=d2, delta=delta(d1,input$optionType),gamma= gamma(d2, Stock, Strike, .02, riskFree, daysInYears(input$expDate)),theta=theta(d1,d2, Stock, Stock, .2, riskFree, daysInYears(input$expDate), input$optionType)
                        ,vega=vega(d1, Stock, daysInYears(input$expDate))
                        ,rho=rho(Stock, Strike, .2, .01, daysInYears(input$expDate), input$optionType, d2) 
                       ,IV=impliedVol(Stock, Strike, daysInYears(input$expDate), riskFree, OptionPrice, input$optionType)
                       ) ###NEED TO UPDATE
         }
     })      
     ##Renders Plot
     output$plot1 <- renderPlot({
         if(analysisFrame()) {
             require(scales)
             priceSpread= c(0, round(Stock*.5), round(Stock*.3), round(Stock), round(Stock*1.2), round(Stock*2))
             listDF=lapply(priceSpread, profitAndLoss) 
             df=bind_rows(listDF)
             ggplot(df, aes(x= x, y= payoff)) +geom_line(color="blue")+geom_point() + ggtitle("Profitability Curve") +
                 xlab("Stock Price") + ylab("Profit and Loss") +theme(
                     plot.title = element_text(color="black", size=25, face="bold.italic", hjust=.5),
                     axis.title.x = element_text(color="blue", size=14, face="bold"),
                     axis.title.y = element_text(color="#993333", size=14, face="bold")
                 ) + scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma)
         }
     })
     

###REACTIVE WIRING
     #ANALYSIS WINDOW
     analysisFrame <- eventReactive(input$contractList, {
        if(input$contractList != ""){
            return(TRUE)
        } else {
            return(FALSE)
        }
            
     })
     #FORCES REFRESH OF CONTRACT LIST WHEN EXP DATE UPDATED
     contractListFrame <- eventReactive(input$expDate, {
         if(input$expDate != "" ||nput$optionType != ""  ){
             return(TRUE)
         } else {
             return(FALSE)
         }
         
     })


###MAIN FUNCTION
optionsDataFunc<-function(symbols, expDate, contractType) {
    
    
    
    if(contractType=='Call'){
        optionsData=getOptionChain(symbols, Exp=expDate, src="yahoo")
        optionsData<-optionsData$calls
        optionsData<-optionsData %>% mutate(Contract = rownames(.))
        optionsData<-optionsData %>% rowwise() %>% mutate(contract_hash = digest(Contract, "md5", serialize = FALSE))
        return(optionsData)
    } else if (contractType=='Put'){
        optionsData=getOptionChain(symbols, Exp=expDate, src="yahoo")
        optionsData<-optionsData$puts
        optionsData<-optionsData %>% mutate(Contract = rownames(.))
        optionsData<-optionsData %>% rowwise() %>% mutate(contract_hash = digest(Contract, "md5", serialize = FALSE))
        return(optionsData)
    }

}

daysInYears <- function(x) {
            days<-as.numeric((as.Date(x,"%Y-%m-%d")-Sys.Date()),  units = 'days')/365
            return(days)
}

###CALCULATE RISK FREE
options("getSymbols.yahoo.warning"=FALSE)
getSymbols("DGS3MO", src = "FRED")
riskFree<<- max(c(as.numeric(last(na.omit(DGS3MO))/100)),.01)

###LOGIC FUCTIONS
calcd1 <- function(sigma, S, K, r, t) {
    d1=1/(sigma*(sqrt(t)+.00000000000001)) * (log(S/K)+ (r+sigma**2/2)*(t+.0000000000001))
    return(d1)
}

calcd2 <- function(sigma, S, K, r, t) {
    d1=calcd1(sigma, S, K, r, t)
    d2=d1-(sigma*(sqrt(t)+.0000000000001))
    return(d2)
}

calcCallPrice <- function(d_1, d_2, S, K, r, t) {
    C=pnorm(d_1, mean=0, sd=1) * S-pnorm(d_2, mean=0, sd=1) * K * exp(-r*t)
    return(C)
}

calcPutPrice <- function(d1, d2, S, K, r, t) {
    P=-pnorm(-d_1, mean=0, sd=1) * S + pnorm(-d_2, mean=0, sd=1) * K * exp(-r*t)
    return(P)
}

###GREEKS
delta <- function(d_1, contractType) {
    if (contractType == 'Call') {
        deltaValue= pnorm(d_1, mean=0, sd=1)
        return(deltaValue)
    }  else if (contractType == 'Put') {
        deltaValue= -pnorm(-d_1, mean=0, sd=1)
        return(deltaValue)
    }
}

gamma <- function(d_2, S, K, sigma, r, t) {
    gammaValue=( K * exp(-r * t) * (dnorm(d_2, mean=0, sd=1) / (S**2 * sigma * sqrt(t) ))) 
    return(gammaValue)
}

theta <- function(d_1,d_2, S, K, sigma, r, t, contractType) {
    if (contractType == 'Call') {
        thetaValue= -S*sigma* dnorm(d_1, mean=0, sd=1)/(2*sqrt(t))-r*K*exp(-r*t) * pnorm(d_2, mean=0, sd=1)
        return(thetaValue)
    }  else if (contractType == 'Put') {
        thetaValue= -S*sigma* dnorm(-d_1, mean=0, sd=1)/(2*sqrt(t))+r*K*exp(-r*t) * pnorm(-d_2, mean=0, sd=1)
        return(thetaValue)
    }
}

vega <- function(d_1, S, t) {
    vegaValue=S * dnorm(d_1, mean=0, sd=1) * sqrt(t) 
    return(vegaValue)
}


rho <- function(S, K, sigma, r, t, contractType, d_2)  {
        if (contractType == 'Call') {
            rhoValue=r*K*exp(-r * t)*pnorm(d_2, mean=0, sd=1)
            return(rhoValue)
        }  else if (contractType == 'Put') {
            rhoValue= r*K*exp(-r * t)*pnorm(-d_2, mean=0, sd=1)
            return(rhoValue)
        }
}

###VOLITILITY
BlackSholes <-function(S, K, T, r, sig, type){
                        d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
                        d2 <- d1 - sig*sqrt(T)
                        
                        if(type=='Call'){
                            value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
                            }
                         if(type=='Put'){
                         value <- K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
                            }
                         return(value)
    }

impliedVol <- function(S, K, T, r, market, type){
                sig <- 0.20
                sig.up <- 1
                sig.down <- 0.001
                count <- 0
                err <- BlackSholes(S, K, T, r, sig, type) - market 
        
        ## repeat until error is sufficiently small or counter hits 1000
        while(abs(err) > 0.00001 && count<1000){
            if(err < 0){
                sig.down <- sig
                sig <- (sig.up + sig)/2
            }else{
                sig.up <- sig
                sig <- (sig.down + sig)/2
            }
            err <- BlackSholes(S, K, T, r, sig, type) - market
            count <- count + 1
        }
        
        ## return NA if counter hit 1000
        if(count==1000){
            return(NA)
        }else{
            return(sig)
        }
    }




####GRAPH HELPER

profitAndLoss<-function(x) {
    if(input$optionType=='Call') {
       payoff= max(0, x-Strike)-OptionPrice
    } else if (input$optionType=='Put') {
        payoff=  max(0, Strike-x)-OptionPrice
    }
    return(data.frame(x,payoff*100))

}

})
