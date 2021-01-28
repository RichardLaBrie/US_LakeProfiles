#This function only works if data.frame has the same architecture as the rLakeAnalyzer::approx.bathy function
Trunc.cone <- function(data.frame)
{
  Vol.tot = vector(length = length(data.frame[,2])-1)
for(j in 1:(length(data.frame[,2])-1))
{
  R = sqrt(data.frame[j,2]/pi)
  r = sqrt(data.frame[j+1,2]/pi)
  h = data.frame[j+1,1]-data.frame[j,1]
  
  Vol.tot[j] = pi/3 * h * (r*r + r*R + R*R)
}
Vol.tot = sum(Vol.tot)
return(Vol.tot)
}