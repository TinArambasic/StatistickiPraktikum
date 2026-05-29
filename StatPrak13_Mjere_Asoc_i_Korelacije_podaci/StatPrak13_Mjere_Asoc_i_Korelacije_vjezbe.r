#****** Vjezbe 12 **********

######################################################
#          Mjere asocijacije i korelacije            #
######################################################

#Pearsonov koeficijent korelacije, te Spearmanov i Kendallov koeficijent korelacije ranga možemo dobiti funkcijom cor()
x <- rnorm(100, 0, 1)
y <- rnorm(100, 5, 1)

cor(x, y) #ili cor(x, y, method = "pearson") 
cor(x, y, method = "spearman")
cor(x, y, method = "kendall")
# Sve tri vrijednosti su blizu 0 što je za oèekivati s obzirom da su uzorci nezavisno generirani

# Dijagram raspršenosti:
plot(x, y)

###Primjer:
x <- rnorm(100)
y <- 2*x + 1
plot(x, y)

cor(x, y)
cor(x, y, method = "spearman")
cor(x, y, method = "kendall")
# Deterministièka monotona veza i to linearna.


###Primjer: linearna veza s aditivnom greškom
x <- 1:100
y <- -5*x + rnorm(100, 0, 50)
plot(x, y)

cor(x, y)
cor(x, y, method = "spearman")
cor(x, y, method = "kendall")


###Primjer:
x <- rnorm(100)
y <- x^2
plot(x, y)

cor(x, y)
cor(x, y, method = "spearman")
cor(x, y, method = "kendall")
#Iako su x i y deterministièki vezani, niti jedna od mjera to nije u moguænosti u potpunosti prepoznati.


###Primjer:
x <- rnorm(100)
y <- x^3
plot(x, y)

cor(x, y)
cor(x, y, method = "spearman")
cor(x, y, method = "kendall")
#Uoèimo razliku izmeðu korelacije i postojanja monotone veze


#####################################################################


#### Zadatak 1.
# X - udaljenost do vatrogasnog centra
# Y - iznos štete od požara
pozar <- read.csv2("pozar.csv")
str(pozar)
attach(pozar)
x <- udaljenost
y <- steta

plot(x, y)

cor(x, y)                   #Ukazuje na postojanje pozitivne korelacije
cor(x, y, method = "spearman")
cor(x, y, method = "kendall")  #Ukazuju na postojanje rastuæe monotone veze

## a)
#Hipoteze:
# H0: \rho = 0
# H1: \rho !=0
cor.test(x, y)
#p < 2.2e-16 < 0.05 - odbacujemo H0, postoji korelacija

## b)
#Hipoteze:
# H0: \rho = 0
# H1: \rho > 0
cor.test(x, y, alternative = "greater")
# p < 2.2e-16 < 0.05 - odbacujemo H0, postoji pozitivna korelacija

## c) 
#Možemo testirati jednakost vrijednosti Spearmanovom i Kendallovog koeficijenta nuli
#Hipoteze:
# H0: \rho_S = 0
# H1: \rho_S !=0
cor.test(x, y, method = "spearman")
#p < 2.2e-16 < 0.05 - odbacujemo H0, veza je monotona
# Zbog postojanja jednakih vrijednosti koristi se asimptotska verzija testa.

#Hipoteze:
# H0: \tau = 0
# H1: \tau !=0
cor.test(x, y, method = "kendall")
#p < 2.2e-16 < 0.05 - odbacujemo H0, veza je monotona
# Zbog veliène uzorka (>50) koristi se asimptotska verzija testa.

## d) 
#Da, svakako zbog (a), (b) i (c).



#### Zadatak 2.
library("MASS")
str(Animals)
attach(Animals)

plot(body, brain, xlim = c(0, 100000))
text(body, brain, labels = row.names(Animals), pos = 4)

#Testiramo postoji li pozitivna korelacija:
# H0: \rho = 0
# H1: \rho > 0
cor.test(body, brain, alternative = "greater")
# p = 0.5108 > 0.05, nema dokaza da postoji pozitivna korelacija.

#Testiramo postoji li rastuæa veza
#Spearmanov koeficijent
# H0: \rho_S = 0
# H1: \rho_S > 0
cor.test(body, brain, method = "spearman", alternative = "greater")
#Kendallov tau
# H0: \tau = 0
# H1: \tau > 0
cor.test(body, brain, method = "kendall", alternative = "greater")
#p < 0.05 pa možemo tvrditi da postoji rastuæa veza.
#Warning upozorava na to da u promatranim uzorcima ima jednakih podataka, pa se p-vrijednost odreðuje
# korištenjem aproksimacije distribucije test-statistike (aproksimacija je T distribucija
# za Spearmanov odnosno N(0, 1) za Kendallov koeficijent).


#Za logaritmirane podatke:
plot(log(body), log(brain))
text(log(body), log(brain), labels = row.names(Animals), pos = 4)
#Uoèavamo linearnu vezu, izuzmemo li tri dinosaura koji znaèajno odstupaju.

#Testiramo postoji li pozitivna korelacija:
# H0: \rho = 0
# H1: \rho > 0
cor.test(log(body), log(brain), alternative = "greater")
#p < 0.05, pa možemo tvrditi da postoji pozitivna korelacija.

#Testiramo postoji li rastuæa veza
#Spearmanov koeficijent
# H0: \rho_S = 0
# H1: \rho_S > 0
cor.test(log(body), log(brain), method = "spearman", alternative = "greater")
#Kendallov tau
# H0: \tau = 0
# H1: \tau > 0
cor.test(body, brain, method = "kendall", alternative = "greater")
#p < 0.05 pa možemo tvrditi da postoji rastuæa veza.




#### Zadatak 3.
str(mtcars)
attach(mtcars)
plot(mpg, hp)
#Testiramo postoji li korelacija:
# H0: \rho = 0
# H1: \rho != 0
cor.test(mpg, hp)
#p < 0.05, pa možemo tvrditi da postoji korelacija.
#Testiramo postoji li monotona veza
#Spearmanov koeficijent
# H0: \rho_S = 0
# H1: \rho_S != 0
cor.test(mpg, hp, method = "spearman")
#Kendallov tau
# H0: \tau = 0
# H1: \tau != 0
cor.test(mpg, hp, method = "kendall")
#p < 0.05 pa možemo tvrditi da postoji monotona veza. 
# Po vrijednostima koeficijenata je padajuæa (radi se o potrošnji goriva u miljama po galonu).

plot(mpg, wt)
#Testiramo postoji li korelacija:
# H0: \rho = 0
# H1: \rho != 0
cor.test(mpg, wt)
#p < 0.05 pa možemo tvrditi da postoji korelacija.
#Testiramo postoji li monotona veza
#Spearmanov koeficijent
# H0: \rho_S = 0
# H1: \rho_S != 0
cor.test(mpg, wt, method = "spearman")
#Kendallov tau
# H0: \tau = 0
# H1: \tau != 0
cor.test(mpg, wt, method = "kendall")
#p < 0.05 pa možemo tvrditi dapostoji monotona veza. 
# Po vrijednostima koeficijenata je padajuæa.


