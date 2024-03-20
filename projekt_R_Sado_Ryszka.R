library(readxl)
Dane<-read_excel("Wypadki.xlsx")
colnames(Dane) <- c("wojewodztwo","wypadki","drogi","wykroczenia", "nowi_kierowcy", "fotoradary", "deszcz", "turysci")
Dane<-Dane[,c(-1)]
plot(Dane)
summary(Dane)

library(moments) 
skewness(Dane)
kurtosis(Dane)

# hist(residuals(lm), main = "Histogram reszt", col = "lightgray", xlab = "Reszty")
pairs(Dane)
cor(Dane)
library(corrplot)
corrplot(cor(Dane))
#korelacja miedzy dwiema zmiennymi
cor.test(Dane$wypadki, Dane$turysci,
         method = c("pearson", "kendall", "spearman"))
#statystyka t  i p-value sugeruja, ze mozemy odrzucic H0
#Istnieje silna korelacja miedzy liczba wypadkow a liczba ludnosci, co potwierdzaja trzy metody korelacji
cor.test(~Dane$wypadki+Dane$turysci,data=Dane,alternative="two.sided",conf.level=0.95,method="pearson")

#regresja liniowa
lm<-lm(formula=wypadki ~ ., data=Dane)
lmx<-lm(formula=Dane$wypadki~Dane$turysci)
plot(Dane$turysci, Dane$wypadki)    
abline(lmx$coefficients)

#regresja wieloraka
summary(lm) # nowi kierowcy wyszli nieistotni, najprawdopodobniej bedziemy mogli je usunac 

confint(lm, level=0.95) # przedzialy ufnosci
confint(lm, level=0.90)

library(ellipse)
plot(ellipse(cor(Dane$wypadki,Dane$drogi), scale=c(sd(Dane$wypadki), sd(Dane$drogi))))
plot(ellipse(cor(Dane$drogi,Dane$fotoradary), scale=c(sd(Dane$drogi), sd(Dane$fotoradary))))
Model <- lm
#przedzial ufnosci
confint(Model, level=0.95) # nas to interesuje
confint(Model, level=0.90)
confint.lm(Model)
#mozna zmienic na linie np. plot(ellipse(Model, which = c(a,b), type= "l")
plot(ellipse(Model, which = c(3,7)))
plot(ellipse(Model, which = c(3,6)))
plot(ellipse(Model, which = c(5,4)))
plot(ellipse(Model, which = c(3,5)))

cor(Dane) #korelacja miedzy zmiennymi egzogenicznymi
summary(Model,corr=TRUE)$corr #korelacja pomiedzy zmiennymi endogenicznymi

#diagnostyka reszt
#reszty dla kazdej zmiennej (nie wiem czy potrzebne) 
par(mfrow=c(2,2), mar=c(5, 4, 4, 2) + 0.1)
plot(Dane$drogi,residuals(lm), main = "Reszty dla  dlugosci drog publicznych")
plot(Dane$wykroczenia,residuals(lm),  main = "Reszty dla  liczby zatrzymanych kierowcow")
plot(Dane$nowi_kierowcy,residuals(lm),  main = "Reszty dla  liczby kierowcow")
plot(Dane$fotoradary,residuals(lm),  main = "Reszty dla  ilosci fotoradarow")
par(mfrow=c(2,2))
plot(Dane$deszcz,residuals(lm),  main = "Reszty dla  sredniej ilosci dni opadow deszczu w ciagu roku")
plot(Dane$turysci,residuals(lm),  main = "Reszty dla  liczby turystow na 1000 ludnosci")
par(mfrow = c(1, 1))
plot(Model$residuals)
abline(h=0,col="red")
qqnorm(Model$residuals, pch=1, frame=FALSE)
qqline(Model$residuals, col = "darkblue", lwd=2)
qqnorm(rstudent(Model),ylab="Standaryzowane reszty modelu regresji")

#testowanie normalnosci reszt
shapiro.test(Model$residuals)

#ocena niezaleznosci 
library(randtests)

#testowanie, czy dane sa rozmieszczone losowo, czy wystepuje w nich jakas regularnosc (tutaj odrzucamy H0 o losowym rozmieszczeniu reszt)
runs.test(Model$residuals) 
qqnorm(Model$residuals)

jack <- rstudent(Model)
par(mfrow=c(1,1))
plot(jack,ylab="Jacknife Residuals",main="Jacknife Residuals")
jack[abs(jack)==max(abs(jack))] #czyli oobserwacja o indeksie 5 ma najwiekszy wplyw na dopasowanie modelu (wykroczenia)

#wartosc krytyczna t dla poziomu istotnosci 0.05 w tescie dwustronnym
qt(0.05/(dim(Dane)[1]*2),(dim(Dane)[1]-10-1))
#porownanie wartosci bezwzglednej "Jackknife residuals" dla obserwacji[5] z wartoscia krytyczna t (wartosc bezwzgledna tej obserwacji nie jest mniejsza niz t)
(-abs(jack[abs(jack)==max(abs(jack))])<qt(0.05/(dim(Dane)[1]*2),(dim(Dane)[1]-10-1)))
# nie ma statystycznie istotnego wplywu tej obserwacji na dopasowanie modelu.
cook <- cooks.distance(Model)
plot(cook,ylab="Cooks distances")
cook 
# Najmniejsze wartosci dla obserwacji 13,14,1 (maja niewielki wplyw na dopasowanie modelu). 
# Najwieksze dla 5,7,12 (moga miec znaczny wplyw na dopasowanie modelu)
#sorted_cook_indices <- order(cook)

#identify(1:dim(Dane)[1],cook,row.names(Dane)) # wlaczac to w R

# analiza regresji
# budowa modelu regresji ilosci wypadkow zbudowany na danych, gdzie obserwacje maja odleglosc cooka < niz najwieksza odleglosc cooka w calym zestawie danych
Model_zredukowany <- lm(wypadki~.,data=Dane,subset=(cook<max(cook)))
summary(Model_zredukowany)
summary(Model)
# indeks obserwacji o maksymalnej odleglosci cooka
number_cook<-which(cook==max(cook))
#tworzenie alternatywnego modelu regresji, eliminujac obserwacje o najwiekszej odl. cooka
Model_subset_alternative <- lm(wypadki~.,data=Dane,subset=(row.names(Dane)!=number_cook))
summary(Model_subset_alternative)

#wartość kryterium akaike.

l0<-lm(wypadki~ drogi+wykroczenia+nowi_kierowcy+fotoradary+deszcz+turysci, data=Dane)
ldrogi<-lm(wypadki~ wykroczenia+nowi_kierowcy+fotoradary+deszcz+turysci, data=Dane)
lwykroczenia<-lm(wypadki~ drogi+nowi_kierowcy+fotoradary+deszcz+turysci, data=Dane)
lnowi_kierowcy<-lm(wypadki~ drogi+wykroczenia+fotoradary+deszcz+turysci, data=Dane)
lfotoradary<-lm(wypadki~ drogi+wykroczenia+nowi_kierowcy+deszcz+turysci, data=Dane)
ldeszcz<-lm(wypadki~ drogi+wykroczenia+nowi_kierowcy+fotoradary+turysci, data=Dane)
lturysci<-lm(wypadki~ drogi+wykroczenia+nowi_kierowcy+fotoradary+deszcz, data=Dane)

AIC(l0,ldrogi,lwykroczenia,lnowi_kierowcy,lfotoradary,ldeszcz,lturysci)

#Najlepszy model, gdzie usuniemy wykroczenia

library(faraway)
library(leaps)
x <- model.matrix(Model)[,-1]
y <- Dane$wypadki
g <- leaps(x,y,nbest=1)
Cpplot(g)
# najlepszy model x1x3x4x5x6 (tak jak w AIC)

adjR2 <- leaps(x,y,nbest=1,method="adjr2") # informacje o najlepszych modelach razem z R^2
adjR2
maxadjr(adjR2, 6)

# Najlepszy model to x1,x3,x4,x5,x6

################################################################################
#USUWAMY Z MODELU ZMIENNA  WYKROCZENIA
################################################################################
Dane1<-Dane[,c(-3)]

par(mfrow=c(2,2), mar=c(5, 4, 4, 2) + 0.1)
plot(Dane1$drogi,residuals(lm), main = "Reszty dla  dlugosci drog publicznych")
plot(Dane1$nowi_kierowcy,residuals(lm),  main = "Reszty dla  liczby kierowcow")
plot(Dane1$fotoradary,residuals(lm),  main = "Reszty dla  ilosci fotoradarow")
plot(Dane1$deszcz,residuals(lm),  main = "Reszty dla  sredniej ilosci dni opadow deszczu w ciagu roku")
par(mfrow = c(1, 1))
plot(Dane1$turysci,residuals(lm),  main = "Reszty dla  liczby turystow na 1000 ludnosci")
par(mfrow = c(1, 1))

Model1<-lm(formula=wypadki ~ ., data=Dane1)
summary(Model)

qqnorm(Model1$residuals, pch=1, frame=FALSE)
qqline(Model1$residuals, col = "darkblue", lwd=2)
qqnorm(rstudent(Model1),ylab="Standaryzowane reszty modelu regresji")
qqline(rstudent(Model1), col = "darkblue", lwd=2)

#testowanie normalnosci reszt
shapiro.test(Model1$residuals) #odrzucamy H0 o normalnosci 
#kryt. AIC
################################################################################
ll0<-lm(wypadki~ drogi+nowi_kierowcy+fotoradary+deszcz+turysci, data=Dane1)
lldrogi<-lm(wypadki~ nowi_kierowcy+fotoradary+deszcz+turysci, data=Dane1)
llnowi_kierowcy<-lm(wypadki~ drogi+fotoradary+deszcz+turysci, data=Dane1)
llfotoradary<-lm(wypadki~ drogi+nowi_kierowcy+deszcz+turysci, data=Dane1)
lldeszcz<-lm(wypadki~ drogi+nowi_kierowcy+fotoradary+turysci, data=Dane1)
llturysci<-lm(wypadki~ drogi+nowi_kierowcy+fotoradary+deszcz, data=Dane1)
AIC(ll0,lldrogi,llnowi_kierowcy,llfotoradary,lldeszcz,llturysci)
#turysci do usuniecia

x <- model.matrix(Model1)[,-1]
y <- Dane1$wypadki
g <- leaps(x,y,nbest=1)
Cpplot(g)
names(Dane1)
# stad rowniez turysci do usuniecia

adjR2 <- leaps(x,y,nbest=1,method="adjr2") # informacje o najlepszych modelach razem z R^2
adjR2
maxadjr(adjR2, 5)
# Najlepszy model to 1,2,3,4 => do usuniecia turysci

################################################################################
#USUWAMY Z MODELU ZMIENNA  TURYSCI
################################################################################
Dane2<-Dane1[,c(-6)]

par(mfrow=c(2,2), mar=c(5, 4, 4, 2) + 0.1)
plot(Dane2$drogi,residuals(lm), main = "Reszty dla  dlugosci drog publicznych")
plot(Dane2$nowi_kierowcy,residuals(lm),  main = "Reszty dla  liczby kierowcow")
plot(Dane2$fotoradary,residuals(lm),  main = "Reszty dla  ilosci fotoradarow")
plot(Dane2$deszcz,residuals(lm),  main = "Reszty dla  sredniej ilosci dni opadow deszczu w ciagu roku")
par(mfrow = c(1, 1))

Model2<-lm(formula=wypadki ~ ., data=Dane2)
summary(Model2)

qqnorm(Model2$residuals, pch=1, frame=FALSE)
qqline(Model2$residuals, col = "darkblue", lwd=2)
qqnorm(rstudent(Model2),ylab="Standaryzowane reszty modelu regresji")
qqline(rstudent(Model2), col = "darkblue", lwd=2)

shapiro.test(Model2$residuals) #nie mamy podstaw do odrzucenia H0 o normalnosci

