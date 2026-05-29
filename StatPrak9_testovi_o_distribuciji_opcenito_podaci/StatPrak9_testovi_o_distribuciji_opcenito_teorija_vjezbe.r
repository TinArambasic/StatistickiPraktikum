#******Vježbe 8. **********

###################################################################################
#   Testiranje hipoteze o distribuciji za diskretne distribucije  (\chi^2 test)   #
###################################################################################


#### Zadatak 1.
#Hipoteze:
# H0: X ~ (  0   1  )     (novèiæ je pravilan)
#           1/2 1/2
# H1: ne H0
#Oèigledno je da dani uzorak favorizira pismo (1). 
#Pitanje je može li se to pripisati sluèajnosti ili tu ima nešto više od same sluèajnosti.
that <- 6.6667            # vrijednost test-statistike
c(qchisq(0.95, 1), Inf)   # kritièno podruèje
1 - pchisq(that, 1)       # p-vrijednost

#Sintaksa za \chi^2 test: chisq.test(x, p = vjer)
#x je vektor opaženih frekvencija, a p je vektor vjerojatnosti pretpostavljene (teorijske) distribucije
chisq.test(c(20, 40), p = c(1/2, 1/2))
#p = 0.0098 < 0.05 => Na razini znaèajnosti 0.05 odbacujemo H0 i prihvaæamo H1,
# tj. na razini znaèajnosti 0.05 možemo tvrditi da je novèiæ nepravilan.
#Opažene frekvencije:
chisq.test(c(20, 40), p = c(1/2, 1/2))$observed
#Oèekivane frekvencije:
chisq.test(c(20, 40), p = c(1/2, 1/2))$expected

#Napomena: kada teorijska distribucija ima samo dva moguæa ishoda, hipoteze o distribuciji mogu
# se svesti na hipoteze o parametru Bernoullijeve distribucije. U tom sluèaju bolje je koristiti
# binomni test koji je egzaktan za razliku od \chi^2 testa.
binom.test(40, 60, p = 1/2)



#### Zadatak 2.
#Hipoteze:
#                1   2   3   4   5   6
# H0:     X ~ (                         )     (kockica je pravilna)
#               1/6 1/6 1/6 1/6 1/6 1/6
#
# H1: ne H0
(x <- c(4, 6, 17, 16, 8, 9))
(vjer <- rep(1/6, 6))    # ili vjer <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
chisq.test(x, p = vjer)
#p = 0.01439 < 0.05 => Na razini znaèajnosti 0.05 odbacujemo H0 i prihvaæamo H1,
#tj. na razini znaèajnosti 0.05 možemo tvrditi da je kockica nepravilna.



#### Zadatak 3.
#Oznaèimo okuse redom s 1, 2, 3, 4, 5.
#Treba testirati ima li obilježje uniformnu distribuciju na peteroèlanom skupu ili ne, tj.
#               1   2   3   4   5
# H0:     X ~ (                     )      (ne postoji preferencija prema nekom okusu)
#              1/5 1/5 1/5 1/5 1/5
#
# H1: ne H0
x <- c(32, 28, 16, 14, 10)
vjer <- rep(1/5, 5)
chisq.test(x, p = vjer)
#p = 0.001234 < 0.05 => Na razini znaèajnosti 0.05 odbacujemo H0 i prihvaæamo H1, tj. na razini znaèajnosti 0.05
#možemo tvrditi da postoji sklonost potrošaèa prema nekim od promatranih okusa sokova.



#### Zadatak 4.
#Oznaèimo sve moguæe ishode redom s 1, 2, 3, 4.
#Treba testirati ima li obilježje sljedeæu distribuciju
#                 1    2    3    4
# H0:     X ~ (                     )
#               0.38 0.32 0.23 0.07
#
# H1: ne H0
x <- c(122, 85, 76, 17)
vjer <- c(0.38, 0.32, 0.23, 0.07)
chisq.test(x, p = vjer)
#p = 0.3485 > 0.05 => Na razini znaèajnosti 0.05 ne odbacujemo H0, tj.
#nemamo razloga tvrditi da prvo istraživanje nije u skladu s novim istraživanjem.



#### Zadatak 5.
#Svaki od 250 puta, bacamo tri novèiæa. Sluèajna varijabla koja broji broj pisama u ishodima jednog bacanja ima
#binomnu distribuciju. Ako su novèiæi pravilni tada æe to biti binomna B(3, 1/2) distribucija èije vjerojatnosti su:
dbinom(0:3, 3, 1/2)
#Treba testirati ima li obilježje sljedeæu distribuciju
#                0    1    2    3
# H0:     X ~ (                     )
#               1/8  3/8  3/8  1/8
#
# H1: ne H0
x <- c(24, 108, 95, 23)
vjer <- c(1/8, 3/8, 3/8, 1/8)
chisq.test(x, p = vjer)
#p = 0.1096 > 0.05 => Na razini znaèajnosti 0.05 ne odbacujemo H0,
#tj. nemamo razloga sumnjati da su novèiæi nepravilni.



#### Zadatak 6.
#Vjerojatnosti Benfordove distribucije dane su s
(vjer <- log10(1:9 + 1) - log10(1:9))
plot(vjer, type="h")
#Prema Benfordovom zakonu 1 je najèešæa prva znaèajna znamenka, i  uèestalost pada do 9
#Hipoteze:
# H0: X ima Benfordovu distribuciju
# H1: X nema Benfordovu distribuciju
x <- c(17, 11, 3, 5, 5, 4, 3, 1, 3)
bp <- barplot(x/sum(x), names.arg = 1:9) # na ovaj naèin predajemo odgovarajuæe koordinate stupaca
bp
lines(bp, vjer, type = "o")

chisq.test(x, p = vjer)
#p = 0.8718 > 0.05 => Na razini znaèajnosti 0.05 ne odbacujemo H0,
#tj. ne možemo tvrditi da se distribucija razlikuje od Benfordove distribucije.
# Warning koji dobijemo je zato jer su neke oèekivane frekvencije manje od 5
chisq.test(x, p = vjer)$expected

#Na Fibonaccijevim brojevima
niz <- c(0, 1)
for (i in 3:1000) {
  niz <- c(niz, niz[i-1] + niz[i-2])
}
x <- niz[-1]
x

formatC(x, format='e')
substring(formatC(x, format='e'), 1, 1)
(x <- as.numeric(substring(formatC(x, format='e'), 1, 1))) #prve znaèajne znamenke Fibonaccijevih brojeva
frek <- table(x)
chisq.test(frek, p = vjer)
#p = 1 > 0.05 => Sigurno ne odbacujemo H0, nema razloga tvrditi da distribucija nije Benfordova.



#### Zadatak 7.
#Ako su pogotci sluèajni, onda svaka bomba ima jednaku vjerojatnost pasti na bilo koje od 576 podruèja.
#Za svako pojedino podruèje vjerojatnost pogaðanja je 1/576, a broj gaðanja je 538.
#Stoga broj pogodaka jednog podruèja ima binomnu distribuciju B(538, 1/576).
#Podatke imamo za n=576 podruèja.
#Dakle, radi se o uzorku velièine 576 iz B(538, 1/576) distribucije.
#X_i - broj pogadaka u i-tom podruèju
#(X_1, ... , X_576) - sluèajan uzorak
#Uoèimo da je pretpostavka nezavisnosti narušena.
#Ipak, testirat æemo hipoteze:
# H0: X ~ B(538, 1/576)
# H1: ne H0

#Empirijska distribucija:
podaci <- c(229, 211, 93, 35, 8)
bp <- barplot(podaci/576, names.arg = 0:4)
bp
#Sve vrijednosti veæe od 4 æemo grupirati kao jedan ishod (kategorizacija).
#Teorijska distribucija
vjer <- dbinom(0:3, 538, 1/576)
(vjer <- c(vjer, 1 - sum(vjer)))
bp
lines(bp, vjer, type = "o")
#Oèekivane frekvencije u uvjetima H0:
vjer*576
podaci

chisq.test(podaci, p = vjer)
#p = 0.9041 > 0.05 => Ne odbacujemo H0 na razini znaèajnosti 0.05.
#Nema dokaza da su Nijemci mogli ciljati (to je u suprotnosti s vjerovanjem ljudi
# da su neka podruèja ciljano više gaðana)

#Binomna distribucija može se dobro aproksimirati Poissonovom.
#Odgovarajuæi parametar Poissonove je lambda = np = 538/576
vjerP <- dpois(0:3, 538/576)
(vjerP <- c(vjerP, 1 - sum(vjerP)))
#Usporedite s:
vjer

#testiramo li u odnosu na Poissonovu distribuciju s parametrom 538/576:
chisq.test(podaci, p = vjerP)
#p = 0.9069 > 0.05 => Ne odbacujemo H0 na razini znaèajnosti 0.05.

#I ovdje smo kategorizirali sve ishode veæe ili jednake 4.
#Princip kategorizacije može se iskoristiti i za neprekidne distribucije, ali
#rezultat može jako ovisiti o izabranoj kategorizaciji.






######################################################################
#   Testiranje hipoteze o distribuciji za neprekidne distribucije    #
#      (Kolmogorov-Smirnovljev test i testovi normalnosti)           #
######################################################################


#### Zadatak 8.
#F0 - funkcija distribucije U(0,1)
#F  - funkcija distribucije iz koje dolazi j.s.u. (X_1, X_2, X_3, X_4, X_5)
#Hipoteze:
# H0: F = F0   (F(x)=F0(x), za svaki realan broj x)
# H1: F != F0  (postoji realan broj x takav da je F(x)!=F0(x))
#R funkcija: ks.test()
#prvi argument su podaci, drugi argument je string koji daje naziv odgovarajuæe
#funkcije distribucije u R-u, zatim se navode parametri te distribucije.
podaci <- c(0.07, 0.3, 0.51, 0.54, 0.95)
ks.test(podaci, "punif", 0, 1)
#p = 0.8123 > 0.05 => Na razini znaèajnosti 0.05 ne odbacujemo H0, tj. nema razloga
#sumnjati da podaci dolaze iz U(0,1) distribucije.
#Test je egzaktan pa ga možemo primijeniti i na ovako malo uzorku.


#### Zadatak 9.
farmakologija <- read.csv2("farmakologija.csv")
str(farmakologija)
lijek <- farmakologija$koncentracija_lijeka
#Grafièki prikazi koji sugeriraju teorijsku distribuciju koncentracije lijeka
hist(lijek, probability = T)
curve(dexp(x, 0.5), col = "blue", add = T)
plot(density(lijek), col = "red")
curve(dexp(x, 0.5), col = "blue", add = T)
#Procjena parametra eksponencijalne distribucije
(lambda <- 1 / mean(lijek))
#F0 - funkcija distribucije eksponencijalne distribucije s parametrom 0.5
#F - funkcija distribucije koncentracije lijeka
#Hipoteze:
# H0: F = F0   (F(x)=F0(x), za svaki realan broj x)
# H1: F != F0  (postoji realan broj x takav da je F(x)!=F0(x))
ks.test(lijek, "pexp", 0.5)
#p = 6.562e-08 < 0.05 => Na razini znaèajnosti 0.05 odbacujemo H0 i prihvaæamo H1,
#tj. ne možemo tvrditi da je koncentracija lijeka eksponencijalno distribuirana s parametrom 0.5.




#### Zadatak 10.
zarulje <- read.csv2("zarulje.csv")
str(zarulje)
x <- zarulje$trajanje
#Grafièki prikazi koji sugeriraju teorijsku distribuciju vremena trajanja žarulje
plot(density(x), col = "red")
curve(dexp(x, 0.005), col = "blue", add = T)
hist(x, probability = T)
curve(dexp(x, 0.005), col = "blue", add = T)
#procjena parametra eksponencijalne distribucije
(lambda <- 1 / mean(x))
#F0 - funkcija distribucije eksponencijalne distribucije s parametrom 0.005
#F  - funkcija distribucije vremena trajanja žarulje
#Hipoteze:
# H0: F = F0   (F(x)=F0(x), za svaki realan broj x)
# H1: F != F0  (postoji realan broj x takav da je F(x)!=F0(x))
ks.test(x, "pexp", 0.005)
#p = 0.2802 > 0.05 => Na razini znaèajnosti 0.05 ne odbacujemo H0,
#tj. nema razloga sumnjati da vrijeme trajanja žarulje ima E(0.005) distribuciju.



#### Zadatak 11.
auti <- read.csv2("automobili.csv")
str(auti)
potrosnja <- auti$potrosnja
hist(potrosnja, freq = F)
plot(density(potrosnja))
#Parametri normalne distribucije su nepoznati - koristimo Shapiro-Wilkov test
#Hipoteze:
# H0: uzorak dolazi iz (neke) normalne distribucije
# H1: uzorak ne dolazi iz normalne distribucije
shapiro.test(potrosnja)
#p = 0.7163 > 0.05 => Na razini znaèajnosti 0.05 ne odbacujemo H0,
#tj. nema razloga sumnjati u normalnu distribuiranost potrošnje.




#### Zadatak 12.
cvrstoca <- read.csv2("cvrstoca.csv")
str(cvrstoca)
x <- cvrstoca$cvrstoca
#Test normalnosti:
#Hipoteze:
# H0: èvrstoæe èeliène žice ima normalnu distribuciju
# H1: èvrstoæe èeliène žice nema normalnu distribuciju
shapiro.test(x)
#p = 0.8032 > 0.05 => na razini znaèajnosti 0.05 ne odbacujemo H0,
#tj. nema razloga sumnjati u normalnu distribuiranost èvrstoæe žice.
#Velièina uzorka je mala, ali unatoè tome možemo primijeniti Shapiro-Wilk test.
#(svakako treba oprezno zakljuèivati)

#F0 - funkcija distribucije N(300,289) distribucije
#F - funkcija distribucije èvrstoæe èeliène žice
#Hipoteze:
# H0: F(x)=F0(x), za svaki realan broj x (èvrstoæa èeliène žice ima N(300,289) distribuciju)
# H1: postoji realan broj x t.d. je F(x)!=F0(x)  (èvrstoæe èeliène žice nema N(300,289) distribuciju)
ks.test(x, "pnorm", 300, sqrt(289))
#p = 0.4358 > 0.05 => na razini znaèajnosti 0.05 ne odbacujemo H0,
#tj. nema razloga sumnjati u N(300,289) distribuciju èvrstoæe žice.



#### Zadatak 13.
#Hipoteze:
# H0: masa piliæa starih 18 dana ima normalnu distribuciju
# H1: masa piliæa starih 18 dana nema normalnu distribuciju
#Shapiro-Wilkov test 
str(ChickWeight)
x <- ChickWeight$weight[ChickWeight$Time==18]
shapiro.test(x)
#p=0.9749 > 0.05 => Na razini znaèajnosti 0.05 ne odbacujemo H0, 
#tj. nema razloga sumnjati u normalnu distribuiranost mase piliæa.


