## program for pricing interest rate or currency swaps
swap <- function(FRA,notional=1000000, marginInBps=0, frequency=6,type='IR',start=0,tol=1e-7)
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
  
  sfr <- floatingRates[start+1]
  diff <- sfr*sumDisc-sumPVrates
  while(abs(diff) >= tol)
  {
    if(diff < 0)
    {
      sfr <- sfr + tol
    }
    else
    {
      sfr <- sfr - tol/10
    }
    diff <- sfr*sumDisc-sumPVrates
  }
  
  fix <- rep(sfr*notional*freq,obs)
  float <- floatingRates*notional*freq
  schedule <- cbind(FRA[,1]+frequency,float,fix)
  if(start >0)
  {
    schedule[1:(start),2:3] = 0
  }
  list(SFR=sfr*10000,schedule=schedule)
}