karatsuba = function(a,b) {
  if (a %/% 1000 < 1 | a %/% 1000 < 1) {
    reshenie = a*b
    return(reshenie)
  }
  powerof10a = 0
  powerof10b = 0
  
  #schitaem razrydanost' a b
  while (a/10 > 1) {
    a = a/10
    powerof10a = powerof10a + 1 
  }
  while (b/10 > 1) {
    b = b/10
    powerof10b = powerof10b + 1
  }
#vozvrashaem ishodnie znacheniya A B
  a = a * 10**powerof10a
  b = b * 10**powerof10a
                          
# %/% v stepeni - chtobi ne bilo vozvedeniya v drobnuyu stepen'
  integerA = a %/% 10**((powerof10a)%/%(2))
  integerB = b %/% 10**((powerof10b)%/%(2))
  intA = integerA  * 10**((powerof10a)%/%(2))
  intB = integerB  * 10**((powerof10b)%/%(2))
  
  ostatokA = a %% 10**((powerof10a)%/%(2))
  ostatokB = b %% 10**((powerof10b)%/%(2))
  
# reshenie = (intA+ostatokA)*(intB+ostatokB) = intA*intB+ostatokA*intB+intA*ostatokB+ostatokA*ostatokB
  
  return(intA*intB+ostatokA*intB+intA*ostatokB+ostatokA*ostatokB)
}

karatsuba(1234567,6543217)
