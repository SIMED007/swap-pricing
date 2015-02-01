## program for pricing interest rate or currency swaps
swap <- function(FRA,notional=1000000, marginInBps=0, frequency=6,type='IR')
{
  #type must be either IR or currency
  #FRA must be table containing enough rates for entire length of swap
  #FRA first column should be time period
  #FRA second column is rate
  #frequency is number of months between payments (monthly = 1, quarterly = 3, semi = 6)
  #frequency should match the frequency present in FRA
  
  forwardRates <- FRA[,2]
  obs <- nrow(FRA)
  discounts <-numeric(obs)
  #pvRates <- numeric(obs)
  floatingRates <- (forwardRates + marginInBps)/10000
  freq <- frequency/12
  discounts[1] <- 1 + freq*floatingRates[1]
  for(i in 2:obs)
  {
    discounts[i] <- (1+freq*floatingRates[i])*discounts[i-1]
  }
  
  sumDisc <- sum(1/discounts)
  pvRates <- floatingRates/discounts
  sumPVrates <- sum(pvRates)
  
  sfr <- 0
  diff <- sfr*sumDisc-sumPVrates
  while(abs(diff) >= 0.000001)
  {
    if(diff < 0)
    {
      sfr <- sfr + 0.000001
    }
    else
    {
      sfr <- sfr - 0.0000001
    }
    diff <- sfr*sumDisc-sumPVrates
  }
  
  list(SFR=sfr*10000,floatingRates=floatingRates*10000,fixedPayment=sfr*notional*freq)
}