#****** Praktièni dio IV.5. - IV.7. **********



###########################################################
#     Testovi o oèekivanju za vezane sluèajne uzorke      #
###########################################################


#### Zadatak 1.
#mu1 - oèekivana plaæa prije restrukturiranja poduzeæa
#mu2 - oèekivana plaæa nakon restrukturiranja poduzeæa
#Radi se o vezanim uzorcima jer promatramo plaæe istog djelatnika.
#Parovi podataka dolaze od nezavisnih i jednako distribuiranih sluèajnih vektora.
#Komponente tih sluèajnih vektora ne moraju biti nezavisne (i nisu jer modeliraju plaæe istog zaposlenika).
#Hipoteze:
# H0: mu1 = mu2    (oèekivane plaæe su jednake)
# H1: mu1 < mu2    (Oèekivana plaæa je veæa nakon restrukturiranja)
djelatnici <- read.csv2("djelatnici.csv")
str(djelatnici)
attach(djelatnici)
#Uvijek je dobro grafièki promotriti razlike:
boxplot(placa_prije, placa_poslije)
boxplot(placa_prije - placa_poslije)
#Uzorci su dovoljno veliki za primjenu t-testa.
t.test(placa_prije, placa_poslije, alternative = "less", paired = TRUE)
#p < 0.05 - na razini znaèajnosti 0.05 odbacujemo H0 i prihvaæamo H1,
# možemo tvrditi da je oèekivana godišnja plaæa porasla nakon restrukturiranja.
#Radi se o testu na uzorku razlika:
razlike <- placa_prije - placa_poslije
#uz hipoteze:
# H0: muD = mu1-mu2 = 0
# H1: muD = mu1-mu2 < 0
t.test(razlike, mu=0, alternative = "less")
#Važno je uoèiti da se radi o vezanim uzorcima jer zakljuèak može biti potpuno drukèiji:
t.test(placa_prije, placa_poslije, alternative = "less")



#### Zadatak 2.
#Radi se o vezanim uzorcima jer je test na istoj osobi.
# mu1 - oèekivani rezultat na testu prije izlaganja stresnoj situaciji
# mu2 - oèekivani rezultat na testu nakon izlaganja stresnoj situaciji
#Hipoteze:
# H0: mu1 = mu2
# H1: mu1 > mu2
library("BSDA")
Stress
attach(Stress)
mean(prestress)
mean(poststress)
boxplot(prestress, poststress)
#S obzirom na pretpostavku normalne distribuiranosti razlika, možemo koristiti t-test.
t.test(prestress, poststress, alternative = "greater", paired = TRUE)
#p=0.01231 < 0.05 - na razini znaèajnosti 0.05 odbacujemo H0 i prihvaæamo H1,
# možemo tvrditi da je je oèekivani rezultat na testu niži nakon izlaganja stresu.




#### Zadatak 3.
#Radi se o vezanim uzorcima jer se radi o ocjenama na istoj osobi.
# mu1 - oèekivani rezultat prvog psihijatra
# mu2 - oèekivani rezultat drugog psihijatra
#Hipoteze:
# H0: mu1 = mu2
# H1: mu1 != mu2
library("BSDA")
Rehab
attach(Rehab)
mean(psych1)
mean(psych2)
boxplot(psych1, psych2)
t.test(psych1, psych2, alternative = "two.sided", paired = TRUE)
#p=0.4022 > 0.05 - na razini znaèajnosti 0.05 ne odbacujemo H0,
# nema razloga tvrditi da se ocjene znaèajno razlikuju. 




#####################################################
#   Test o proporciji za vezane sluèajne uzorke     #
#####################################################


#### Zadatak 4.
#Radi se o usporedbi proporcija u vezanim uzorcima.
# theta1 - proporcija prehlaðenih u dobi od 12 godina
# theta2 - proporcija prehlaðenih u dobi od 14 godina
# Hipoteze: H0: theta1 = theta2
#           H1: theta1 < theta2
#što se svodi na
# Hipoteze: H0: \bar{theta} = 0.5
#           H1: \bar{theta} < 0.5
#Retci 12 godina i stupci 14 godina:
tab <- matrix(c(37, 15, 10, 8), 2, 2, byrow = T)
rownames(tab) <- c(0, 1)
colnames(tab) <- c(0, 1)
tab
#Broj pozitivnih razlika (parova (1,0)): 10
#Broj ne-nul razlika (parova (1,0) i (0,1)): 10+15=25
binom.test(10, 25, p = 0.5, alternative = "less")
#p=0.2122 > 0.05 ne odbacujemo H0, proporcija prehlada u dobi od 12 godina nije znaèajno manja.





#### Zadatak 5.
#S obzirom da se promatra odgovor istog ispitanika u dva vremenska trenutka, radi se o vezanim uzorcima.
#Kao uspjeh možemo promatrati glas za demokrate. 
# theta1 - proporcija demokratskih glasaèa u 2004. 
# theta2 - proporcija demokratskih glasaèa u 2008.
# Hipoteze: H0: theta1 = theta2
#           H1: theta1 < theta2
#što se svodi na
# Hipoteze: H0: \bar{theta} = 0.5
#           H1: \bar{theta} < 0.5
#Retci 2004. godina i stupci 2008. godina:
tab <- matrix(c(188, 44, 26, 175), 2, 2, byrow = T)
rownames(tab) <- c(0, 1)
colnames(tab) <- c(0, 1)
tab
#Broj pozitivnih razlika (parova (1,0)): 26
#Broj ne-nul razlika (parova (1,0) i (0,1)): 26+44=70
binom.test(26, 70, p = 0.5, alternative = "less")
#p=0.0207 < 0.05 odbacujemo H0, proporcija demokratskih glasaèa znaèajno se poveæala.

#Pretpostavimo da smo iste rezultate dobili na nezavisnim uzorcima (drugi ispitanici).
margin.table(tab, 1)
margin.table(tab, 2)
#To znaèi 2004. 201 je od 433 ispitanika je glasao za demokrata,
# a 2008. 219 od 433 ispitanika je glasao za demokrata.
#U tom sluèaju ne bi odbacili nul-hipotezu:
prop.test(c(201, 219), c(433, 433), alternative = "less")
#Treba uoèiti da je preciznost veæa na vezanim uzorcima.





#### Zadatak 6.
#Radi se o vezanim uzorcima jer promatramo plaæe istih djelatnika.
# theta1 - proporcija plaæa manjih od 25000 prije restrukturiranja 
# theta2 - proporcija plaæa manjih od 25000 poslije restrukturiranja 
# Hipoteze: H0: theta1 = theta2
#           H1: theta1 != theta2
#što se svodi na
# Hipoteze: H0: \bar{theta} = 0.5
#           H1: \bar{theta} != 0.5
djelatnici <- read.csv2("djelatnici.csv")
str(djelatnici)
attach(djelatnici)
table(placa_prije < 25000, placa_poslije < 25000)
#Broj pozitivnih razlika (parova (1,0)): 8
#Broj ne-nul razlika (parova (1,0) i (0,1)): 8+1=9
binom.test(8, 9, p = 0.5, alternative = "two.sided")
#p=0.03906 < 0.05 odbacujemo H0, proporcije se razlikuju.

#########################################################
#              Chi-kvadrat test o nezavisnosti          #
#########################################################

#### Zadatak 7.
#Za prvo pitanje radi se o dva obilježja:
# X - èita li osoba redovito (0 - ne, 1 - da)
# Y - struèna sprema osobe (1 - NSS, 2 - SSS, 3 - VSS)
#Uzorak: (X_1, Y_1), ... (X_N, Y_N) - nezavisni sluèajni vektori jednako distribuirani kao (X, Y)
#Pitanje: jesu li X i Y nezavisne sluèajne varijable?
#Uoèimo da niti jedna margina nije fiksirana.
knjiga <- read.csv2("knjiga.csv")
str(knjiga)
attach(knjiga)
#Analizirajmo za poèetak dostupne podatke
(tf <- table(citanje, obrazovanje))  # zajednièka tablica frekvencija obilježja citanje i obrazovanje
prop.table(tf)                       # procjena zajednièke tablice distribucije obilježja citanje i obrazovanje
#procjene marginalnih distribucija dobijemo doadavanjem margina na zajednièku tablicu relativnih frekvencija
addmargins(prop.table(tf))
#margine možemo dobiti i s
margin.table(tf, 1)                  # tablica frekvencija obilježja citanje
#što je isto kao 
table(citanje)
prop.table(margin.table(tf, 1))    # procjena marginalne distribucije za obilježje citanje
#isto kao
prop.table(table(citanje))

prop.table(tf, 1)        # procjene uvjetnih distribucija obilježja obrazovanje
prop.table(tf, 2)        # procjene uvjetnih distribucija obilježja citanje
#Ako su nezavisne, uvjetne distribucije bi trebale biti jednake marginalnima, odnosno
prop.table(tf, 1)
#sve jednake
prop.table(margin.table(tf, 2)) 
#i
prop.table(tf, 2)
#sve jednake
prop.table(margin.table(tf, 1)) 

#Hipoteze:
# H0: navika èitanja i struèna sprema su nezavisne (X i Y su nezavisne)
# H1: navika èitanja i struèna sprema nisu nezavisne
#Možemo testirati chi^2 testom za nezavisnost
chisq.test(table(citanje, obrazovanje))
#p = 0.001817 < 0.05 - na razini znaèajnosti 0.05 odbacujemo H0 i prihvaæamo H1,
# možemo tvrditi da su navika èitanja i struèna sprema zavisna obilježja.
#Posebno, to znaèi i da uvjetne distribucije navike èitanja nisu jednake s obzirom na razlièite kategorije obrazovanja,
# odnosno uvjetne distribucije obrazovanja s obzirom na èitanje nisu jednake (u tom smislu nema homogenosti uvjetnih distribucija).


#Kako se provodi test:
# empirijske frekvencije:
tf
# oèekivane frekvencije u uvjetima H0:
(N <- sum(tf))
of <- c(prop.table(margin.table(tf, 1))[1] * prop.table(margin.table(tf, 2)) * N, prop.table(margin.table(tf, 1))[2] * prop.table(margin.table(tf, 2)) * N)
(of <- matrix(of, 2, 3, byrow = TRUE))

(teststat <- sum((tf-of)^2/of))
#p-vrijednost:
1-pchisq(teststat, 1*2)



#Drugo pitanje - obilježja su:
# X - èita li osoba redovito (0 - ne, 1 - da)
# Y - spol 
#chi^2 testom za nezavisnost testiramo
#Hipoteze:
# H0: navika èitanja i spol su nezavisni
# H1: navika èitanja i spol nisu nezavisni
(tf <- table(citanje, spol))
chisq.test(tf)
#p = 0.00608 < 0.05 - na razini znaèajnosti 0.05 odbacujemo H0 i prihvaæamo H1,
# možemo tvrditi da su navika èitanja i spol zavisna obilježja
#Napomena: u sluèaju 2x2 tablica chisq.test koristi Yatesovu korekciju zbog neprekidnosti
# zbog koje je definicija test statistika nešto drugaèija nego u prethodnom primjeru

#Uoèimo: H0 kao hipoteza homogenosti bi bila "navika èitanja je jednako distribuirana meðu muškarcima i ženama" ili
# "spol je jednako distribuiran meðu osobama koje redovito èitaju i onima koji ne èitaju redovito"
#Ako bi promatrali hipotezu homogensoti, u ovom konkretnom primjeru mogli bi koristiti i 
# test o usporedbi proporcija jer se radi o 2x2 tablici.




#### Zadatak 8.
#Hipoteze:
# H0: odabir smjera ne ovisi o spolu
# H1: postoji zavisnost
#Možemo trestirati chi^2 testom o nezavisnosti.
(tf <- matrix(c(100, 80, 70, 50, 50, 50, 50, 50), 2, 4, byrow = TRUE))
chisq.test(tf)
#p=0.06545 > 0.05 - na razini znaèajnosti 0.05 ne odbacujemo H0,
# tj. ne možemo tvrditi da odabir smjera ovisi o spolu.




#### Zadatak 9.
#Hipoteze:
# H0: obilježja su nezavisna
# H1: postoji zavisnost
(tf <- matrix(c(48, 26, 19, 21, 36, 30), 2, 3, byrow = TRUE))
chisq.test(tf)
#p = 0.0007232 < 0.05 - na razini znaèajnosti 0.05 odbacujemo H0 i prihvaæamo H1,
# tj. možemo tvrditi da postoji povezanost izmeðu pušenja i povišenog krvnog tlaka
#Kako možemo uoèiti vezu na osnovu tablice frekvencija?
#Možemo promatrati procjene za uvjetne distribucije:
# za tlak:
prop.table(tf, 2)
prop.table(margin.table(tf, 1))
#kod teških pušaèa je veæi udio povišenog tlaka nego kod nepušaèa, ili kod cijele populacije.
# za pušenje:
prop.table(tf, 1)
prop.table(margin.table(tf, 2))
#kod normalnog tlaka je veæi udio nepušaèa nego kod osoba s povišenim tlakom.
#Možemo promatrati i opažene i oèekivane frekvencije:
tf
chisq.test(tf)$expected



#### Zadatak 10.
#Hipoteze:
# H0: odabir politièke stranke i spol su nezavisna obilježja
# H1: odabir politièke stranke i spol nisu nezavisna obilježja
library("BSDA")
str(Politic)
#Tablica frekvencija
(tf <- table(Politic))
chisq.test(tf)
#Warning se ispisuje jer nisu sve oèekivane frekvencije veæe ili jednake 5
chisq.test(tf)$expected
#Iz tog razloga u ovom konkretnom sluèaju treba biti oprezan sa zakljuèkom chi^2 testa.
#Dobivena p-vrijednost je veæa od 0.05 pa ne bi odbacili H0, nema dokaza da odabir stranke ovisi o spolu.











