#Simulación de trayectorias del movimiento Browniano

#m:= número de trayectorias, t:= longitud  
#dt:= incremento, des:= desviación estándar
TrayectoriasmB<-function(m,t,dt,des=1){
  Particion<-seq(0,t,dt)
  TotalParticion<-length(Particion)
  matriz<-matrix(Particion,nrow = TotalParticion, ncol = m)
  
  datos<-matrix(rnorm(TotalParticion*m,0,sqrt(matriz)*des), 
                nrow = TotalParticion, ncol = m)
  
  trayectorias<-as.data.frame(apply(datos,2,cumsum))
  particion<-as.data.frame(Particion)
  x11()
  matplot(particion,trayectorias, type = "l", 
          xlab = "t", ylab = "Mb(t)")
}

TrayectoriasmB(10,365,0.01)


#Trayectorias del movimiento geométrico Browniano


#m:= número de trayectorias, t:= longitud  
#dt:= incremento, s_0:= precio inicial conocido del activo
#mu:= tendencia, sigma:= volatilidad>=0
DatosmBg<-function(m,t,dt,s_0,mu,sigma){
  Particion<-seq(0,t,dt)
  TotalParticion<-length(Particion)
  matriz<-matrix(Particion,nrow = TotalParticion, ncol = m)
  
  datos<-matrix(rnorm(TotalParticion*m,0,sqrt(matriz)), 
                nrow = TotalParticion, ncol = m)
  trayectoria<-apply(datos,2,cumsum)
  
  mBg<- s_0*exp((mu - 0.5*sigma^2)*Particion + sigma*trayectoria)
  mBg<-as.data.frame(mBg)
  mBg$Particion<-Particion
  return(mBg)
}

m<-10
datos<-DatosmBg(m, t=1, dt=0.1, s_0=5, mu=0.1, sigma = 0.15)
x11()
matplot(datos[,m+1], datos[,1:m], type = "l", 
        xlab = "t", ylab = "Mb(t)")



