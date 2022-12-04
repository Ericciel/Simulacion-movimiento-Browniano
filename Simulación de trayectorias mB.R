#Simulación de trayectorias del movimiento Browniano

TrayectoriasMb<-function(m,t,dt,des=1){
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
#m:= número de trayectorias, t:= longitud  
#dt:= incremento, des:= desviación estándar
TrayectoriasMb(10,365,0.01)
