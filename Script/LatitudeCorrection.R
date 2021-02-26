#This regression is based on data presented in Lewis 1983
#This regression is clearly an overfit, but all latitudes are included in [0,90], so it should not be problematic

# Lat = seq(0,90,10)
# Lat.adj = c( 0.27, 0.31, 0.34, 0.39, 0.46, 0.54, 0.68, 0.89, 1.3, 2.4)
# Lat.mat = as.data.frame(cbind(Lat,Lat.adj))
# Latreg = nlsLM(Lat.adj ~ a*Lat^6 + b*Lat^5 + c*Lat^4 + d*Lat^3 + e*Lat^2 + f*Lat + g,
#                data = Lat.mat,
#                start = list(a = 7E-11, b = -2E-8, c = 1E-6, d = -4E-5, e = 7E-4, f = -4E-4, g = 0.271))

# summary(Latreg)
#Lat.coef = summary(Latreg)$coefficients

CorLat.f <- function(Lat, ele){
  if(length(Lat)!= length(ele)) break(print("The two vectors have different length"))
  Cor.lat = Lat + ele*(7.083332e-11*Lat^6 - 1.509936e-08*Lat^5 + 1.217788e-06*Lat^4 - 4.392001e-05*Lat^3 + 7.163590e-04*Lat^2 - 4.380093e-04*Lat + 2.709720e-01)/100
  return(Cor.lat)
  }

# plot(Lat.adj~Lat, las=1)
# lat.pred = seq(0,90,0.1)
# lines(lat.pred,predict(Latreg,list(Lat=lat.pred)))

      