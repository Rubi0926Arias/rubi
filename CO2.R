library(readxl)
dir<-"C:/Users/hp/Desktop/Rubi"
setwd(dir)
list.files()
#Leer los archivos
dat<-read_excel("Rubi.xlsx",sheet=1,na="")
#sacar logaritmos
Años<-dat[,"Años"]
Ingreso<-dat[,"Ingreso"]
log_PIB<-log(dat[,"PIB pc"])
log_CO2<-log(dat[,"CO2 pc"])

datos<-data.frame(Años,log_PIB,log_CO2,Ingreso)

View(alto_dat)
#division segun Ingresos
alto_dat<-datos[datos[,"Ingreso"]=="Alto",]
med_alto_dat<-datos[datos[,"Ingreso"]=="Med Alto",]
med_bajo_dat<-datos[datos[,"Ingreso"]=="Med Bajo",]
bajo_dat<-datos[datos[,"Ingreso"]=="Bajo",]
#%%%%%%%%%%%%%%%%%%%%%%%b C O 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%% INGRESOS ALTOS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#**************************Grefica Años vs PIB*****************************

ggplot(alto_dat,aes(x=Años,y=log_CO2))+geom_point()

#**************************Regresiones Log-Lineales**************************
library("ggplot2")

reg_Alto<-lm(log_CO2~Años,data=alto_dat)
summary(reg_Alto)

residuos_Alto<-summary(reg_Alto)["residuals"]

ggplot(alto_dat,aes(x=Años,y=log_CO2))+geom_point()+stat_smooth(method=lm)


#Regresion Log-Poligonal

#particionamos los datos en t0=1990
#***************************Creacion de columna de factores*************************
Etapa<-factor(as.character(alto_dat[,"Años"]<=1990),levels=c("TRUE","FALSE"),labels=c("A","B"))
datos_etapa<-data.frame(alto_dat,Etapa)
View(datos_etapa)
#************************************************************************************      

#REGRESION LOG-POLIGONAL 
alto_polig1<-alto_dat[alto_dat[,"Años"]<=1990,]
alto_polig2<-alto_dat[alto_dat[,"Años"]>1990,]

reg_Alto_pol1<-lm(log_CO2~Años,data=alto_polig1)
summary(reg_Alto_pol1)
ggplot(alto_polig1,aes(x=Años,y=log_CO2))+geom_point()+stat_smooth(method=lm)

reg_Alto_pol2<-lm(log_PIB~Años,data=alto_polig2)
summary(reg_Alto_pol2)
ggplot(alto_polig2,aes(x=Años,y=log_CO2))+geom_point()+stat_smooth(method=lm)

#Graficos de Regresio LOG-POLIGONAL
ggplot(datos_etapa,aes(x=Años,y=log_CO2,colour=Etapa))+geom_point()+stat_smooth(method = lm)

#***************Suma de residuos COMPARACION*****************************
#Suma de Residuos Log-Lineal
sum(abs(summary(reg_Alto)[["residuals"]]))
#Residuos Log-poligonal
sum(abs(summary(reg_Alto_pol1)[["residuals"]]),abs(summary(reg_Alto_pol2)[["residuals"]]))
#**************************************************************************
#%%%%%%%%%%%%%%%%%%%%%%%% INGRESOS MEDIO ALTOS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#**************************************************************************








#**************************************************************************
#%%%%%%%%%%%%%%%%%%%%%%%% INGRESOS MEDIO BAJO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#**************************************************************************


