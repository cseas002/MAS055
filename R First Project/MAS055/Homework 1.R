# Christoforos Seas 1028675
# (a) 
# Generating a random number from 5 to 50
num = 5 + sample.int(50, 5)
# (b) 

# Pn = possibility of n people having the same birthday (born the same date)
# NotPn= possibility of n people NOT having the same birthday (not born the same date)


NotPn = function(people){
  # Assuming that the number of different birthdays is 366 (most possible days in a year)
  # The probability of n people not sharing the same birthday equals to
  # 366/366 * 365/366 * 365/366 * ... * n/366 because the cases are independent
  # This number is the formula of C(366, n) (Combinations of choosing n from 366)
  # C(366, n) = 366! / ((366 - n)! * 366^n)
  # I can't use factorial() function because factorial(366) will lead to infinity
  # + it will need a lot of resourced
  # Instead I will do this procedure manually with for loops
  # If people == 1 then there is 0% possibility of not having the same birthday
  if (people == 1)
    return(0)
  possibility = 1
  for (i in 366:(366 - people + 1)) # from 366 to 366 - n + 1 
    possibility = possibility * (i / 366) 
  
  return(possibility)
}

Pn = function(people){
  # The possibility of n people having the same birthday is 1 - possibility of
  # n people NOT having the same birthday
  return(1 - NotPn(people))
}

for (number in num){
  cat("Posibility of ")
  cat(number)
  cat(" people having the same birthday is ")
  cat(Pn(number) * 100)
  cat("%\n")
}

# (c)
# My id number is 1028675 so m = 5
xAxis = c(1:85)
yAxis = c()
for (number in 1:85)
{
  cat("Posibility of ")
  cat(number)
  cat(" people having the same birthday is ")
  possibility = Pn(number) * 100
  cat(possibility)
  cat("%\n")
  yAxis = append(yAxis,possibility)
}
plot(xAxis, yAxis, xlab = "Number of people", ylab = "Possibility having the same birthday (%)")