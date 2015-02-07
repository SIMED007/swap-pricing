## program for pricing interest rate or currency swaps
swap <- function(FRA,notional=1000000,marginInBps=0, exchange,frequency=6,type='IR',start=0,swaption=FALSE,swaptionArgs)
{
  #type must be either IR or currency
  #FRA must be table containing enough rates for entire length of swap
  #FRA first column should be time period
  #FRA second column is rate
  #frequency is number of months between payments (monthly = 1, quarterly = 3, semi = 6)
  #frequency should match the frequency present in FRA
  if(type=="IR"){
        
    forwardRates <- FRA[,2]
    obs <- nrow(FRA)
    discounts <-numeric(obs)
    floatingRates <- (forwardRates + marginInBps)/10000
    freq <- frequency/12
    discounts[1] <- 1 + freq*floatingRates[1]
    for(i in 2:obs)
    {
      discounts[i] <- (1+freq*floatingRates[i])*discounts[i-1]
    }
 
    sumDisc <- sum(1/discounts[(start+1):obs])
    pvRates <- floatingRates/discounts
    sumPVrates <- sum(pvRates[(start+1):obs])
    
    sfr <- sumPVrates/sumDisc
    
    fix <- rep(sfr*notional*freq,obs)
    float <- floatingRates*notional*freq
    schedule <- cbind(FRA[,1]+frequency,float,fix)
    if(start >0)
    {
      schedule[1:(start),2:3] = 0
    }
    
    if(swaption==TRUE)
    {
      annualSFR <- sfr/freq
      pStrike <- swaptionArgs$payerStrike/10000
      rStrike <- swaptionArgs$recStrike/10000
      vol <- swaptionArgs$volatility
      d1 <- log((annualSFR/pStrike)+freq*(vol*vol)*(start/2))/(vol*sqrt(start/2))
      d2 <- d1-(vol*sqrt(start/2))
      prem_pay <- notional*freq*sumDisc*(annualSFR*pnorm(d1)-pStrike*pnorm(d2))
      bpCost_pay <- 10000*prem_pay/notional

      d1 <- log((annualSFR/rStrike)+freq*(vol*vol)*(start/2))/(vol*sqrt(start/2))
      d2 <- d1-(vol*sqrt(start/2))
      prem_rec <- notional*freq*sumDisc*(annualSFR*pnorm(-d1)-rStrike*pnorm(-d2))
      bpCost_rec <- 10000*prem_rec/notional
      
      output<-list(SFR=annualSFR*10000,schedule=schedule,payerPremium=prem_pay,payerCost = bpCost_pay,receiverPremium=-prem_rec,receiverCost = -bpCost_rec)

    }
    else{
      output<-list(SFR=sfr*10000,schedule=schedule)
    }
  }
  else if(type=="currency")
  {
    forwardRates <- FRA[,2:3]
    obs <- nrow(FRA)
    discounts <-matrix(0,obs,2)
    floatingRates <- (forwardRates + marginInBps)/10000
    freq <- frequency/12
    
    discounts[1,] <- 1 + freq*floatingRates[1,]
    for(i in 2:obs)
    {
      discounts[i,] <- (1+freq*floatingRates[i,])*discounts[i-1,]
    }
    sumDisc1 <- sum(1/discounts[(start+1):obs,1])
    sumDisc2 <- sum(1/discounts[(start+1):obs,2])
    
    pvRates <- floatingRates/discounts
    sumPVrates1 <- sum(pvRates[(start+1):obs,1])
    sumPVrates2 <- sum(pvRates[(start+1):obs,2])
    
    sfr1 <- sumPVrates1/sumDisc1
    sfr2 <- sumPVrates2/sumDisc2
    
    fix1<- rep(sfr1*notional[1]*freq,obs+1)
    fix2 <- rep(-sfr2*notional[2]*freq,obs+1)
    fix1[1] <- -notional[1]
    fix2[1] <- notional[2]
    fix1[obs+1] <- fix1[obs+1]+notional[1]
    fix2[obs+1] <- fix2[obs+1]-notional[2]
    schedule1 <- as.matrix(cbind(c(FRA[,1],obs*frequency),fix1,fix2,exchange,exchange*fix2))
    schedule2 <- as.matrix(cbind(c(FRA[,1],obs*frequency),-1*fix1,-1*fix2,1/exchange,(1/exchange)*-1*fix1))
    
#     if(start >0)
#     {
#       schedule[1:(start),2:3] = 0
#     }
    output <- list(SFR1=sfr1*10000,SFR2=sfr2*10000, schedule1=schedule1,schedule2=schedule2 )
  }

  output
}