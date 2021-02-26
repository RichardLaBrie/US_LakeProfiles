#Kirillin and Shatwell - Generalized scaling of seasonal thermal stratification in lakes

# Hcrit hypothesis
# 
# H: Zmean [m]
# hsec = secchi [m]
# L: fetch [m]
# Js = Kinematic radiation flux
# GR = global radiation #w/m^2
# U10 = wind speed at 10m (m/s)
# u = friction velocity such as u^2 = (1.3*10^-3) * (1.43*10^-3) * U10^2
# uL = corrected friction velocity
# Lmo = Monin-Obukhov length (uL^3/ Js)
# 
# Using their empirical coefficient (we cant calculate the logit regression w/o temporal resolution)
# C1 = 0.493
# C2 = 0.00060
# Hcrit = C1*hsec + sqrt(C1^2*hsec^2 + C2*L*Lmo)

#Note, We used Zmax instead of Zmean because we are interested in hypolimnion dynamics
mixing <- function(Zmax, Secchi, L, GR, U10, lambda = 0.002){
  #Constant
  g = 9.81 #m/s^2
  beta = 207*10^-6 #K^-1
  rho_w = 1000 #kg/m^3
  Cp = 4218 #J / (kg*K)
  
  #Wind
  u = sqrt(3.419*10^-6 * U10^2) #Coefficient from Tom Shatwell (pers. comm.)
  uL = u * (1 - exp(-lambda*L))
  
  #Kinematic flux
  Js = GR * g * beta / (rho_w * Cp) #[m^2 s^-3]
  
  #Monin-Obukhoz length
  Lmo = uL^3 / Js
  
  Hcrit = 0.493*Secchi +  sqrt(0.493^2 * Secchi^2 + 0.0006 * L * Lmo)
  output = ifelse(Zmax < Hcrit, "Polymictic", "Dimictic")
  return(output)
}

