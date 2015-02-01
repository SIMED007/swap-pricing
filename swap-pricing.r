## program for pricing interest rate or currency swaps
swap <- function(FRA,notional=1000000, marginInBps=100, frequency=6,type='IR')
{
  #type must be either IR or currency
  #FRA must be table containing enough rates for entire length of swap
  #FRA first column should be time period
  #FRA second column is rate
  #frequency is number of months between payments (monthly = 1, quarterly = 3, semi = 6)
  #frequency should match the frequency present in FRA
  
  discounts <- numeric(nrows(FRA[,1]))
  
}