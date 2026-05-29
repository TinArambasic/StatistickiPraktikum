#******Vje?be 7 **********

#############################################################
# Test o proporciji (vjerojatnosti doga?aja) - binomni test #
#############################################################

#### Zadatak 1.
#theta - vjerojatnost pisma
#Hipoteze:
#  H0: theta = 1/2
#  H1: theta != 1/2
binom.test(7, 10, p = 0.5, alternative = "two.sided")
#p =  0.3438 > 0.1 - Na razini zna?ajnosti 0.1 ne odbacujemo H0,
#tj. na razini zna?ajnosti 0.1 ne mo?emo tvrditi da nov?i? nije pravilan.



#### Zadatak 2.
#theta - vjerojatnost glave
#Hipoteze:
#  H0: theta = 1/2   (kockar je nevin)
#  H1: theta > 1/2   (kockar poku?ava varati)
binom.test(555, 1000, p = 0.5, alternative = "greater")
# p = 0.00028 < 0.05 - Na razini zna?ajnosti 0.05 odbacujemo H0 i prihva?amo H1,
#tj. na razini zna?ajnosti 0.05 mo?emo tvrditi da je nov?i? pristran
#i da kockar ?eli dobiti igru na prijevaru.



#### Zadatak 3.
#theta - vjerojatnost da Milan pogodi boju
#Hipoteze:
#  H0: \theta = 1/4   (Milan nije vidovit, slu?ajno poga?a)
#  H1: \theta > 1/4   (Milan je vidovit)
binom.test(12, 25, p = 0.25, alternative = "greater")
#p = 0.01073 < 0.05 - Na razini zna?ajnosti 0.05 odbacujemo H0 i prihva?amo H1,
#tj. na razini zna?ajnosti 0.05 mo?emo tvrditi da je Milan vidovit.



#### Zadatak 4.
# theta - nepoznata proporcija ljudi s negativnim rezultatom
#Hipoteze:
#  H0: theta = 0.2 
#  H1: theta < 0.2
hormon <- read.csv2("hormon.csv")
str(hormon)
clo <- hormon$CLOtest
table(clo)
length(clo)
binom.test(12, 82, p = 0.2, alternative = "less")
#p=0.1394 > 0.05 => Na razini zna?ajnosti 0.05 ne odbacujemo H0, tj. ne mo?emo tvrditi da je proporcija
#ljudi u promatranoj populaciji s negativnim rezultatom testa statisti?ki zna?ajno manja od 20%.





#############################################
#              Test o kvantilima            #
#############################################


#### Zadatak 5.
str(ChickWeight)
x <- ChickWeight$weight
median(x)
#Hipoteze:
#  H0: medijan = 100 
#  H1: medijan > 100
#<=>
#  H0: Q(0.5) = 100 
#  H1: Q(0.5) > 100
#Q - funkcija kvantila populacijske distribucije mase pili?a
#<=>
#  H0: F(100) = 0.5
#  H1: F(100) < 0.5
#<=>
#  H0: P(X <= 100) = 0.5
#  H1: P(X <= 100) < 0.5
#X - slu?ajna varijabla koja modelira masu pili?a u populaciji
#Ozna?imo theta = P(X <= 100)
#<=>
#  H0: theta = 0.5
#  H1: theta < 0.5
#Sveli smo na problem procjene vjerojatnosti doga?aja.
#Uzorak iz Bernoullijeve distribucije:
x <= 100
#Broj uspjeha:
table(x <= 100)  #ili sum(x <= 100)
#Broj ponavljanja:
length(x)
binom.test(278, 578, p = 0.5, alternative = "less")
#p=0.1912 > 0.05 => Na razini zna?ajnosti 0.05 ne odbacujemo H0, tj. ne mo?emo tvrditi da je medijan mase
#pili?a statisti?ki zna?ajno ve?i od 100.




#### Zadatak 6.
hormon <- read.csv2("hormon.csv")
str(hormon)
x <- na.omit(hormon$gastrS)
median(x)
#Prethodno istra?ivanje: medijan koncentracije je 51
#Hipoteze:
#  H0: medijan = 51     Q(0.5) = 51
#  H1: medijan != 51    Q(0.5) != 51
#<=>
#  H0: F(51) = 0.5
#  H1: F(51) != 0.5
#<=>
#  H0: \theta = 0.5
#  H1: \theta != 0.5
#gdje je \theta = P(X <= 51) = F(51).
table(x<=51)
length(x)
binom.test(50, 78, p = 0.5, alternative = "two.sided")
#p = 0.01685 < 0.05 - Na razini zna?ajnosti 0.05 odbacujemo H0 i prihva?amo H1,
#tj. na razini zna?ajnosti 0.05 mo?emo tvrditi da smo opovrgnuli rezultate prethodnog medicinskog istra?ivanja.
#Medijan je razli?it od 51.



#### Zadatak 7.
krediti <- read.csv2("krediti.csv")
str(krediti)
vis <- krediti$visina_kredita
#Q(0.9) je nepoznati 0.9-kvantil. Procjena tog kvantila je
quantile(vis, 0.9)
#Hipoteze:
#  H0: Q(0.9) = 110000
#  H1: Q(0.9) < 110000
#<=>
#  H0: F(110000) = 0.9
#  H1: F(110000) > 0.9
#<=>
#  H0: \theta = 0.9
#  H1: \theta > 0.9
#gdje je \theta = P(X <= 110000) = F(110000).
table(vis <= 110000)
length(vis)
binom.test(137, 146, p = 0.9, alternative = "greater")
#p = 0.07312 > 0.05 - Na razini zna?ajnosti 0.05 ne odbacujemo H0,
#tj. ne mo?emo tvrditi da je  0.9-kvantil distribucije visine kredita manji od 110000 kn



