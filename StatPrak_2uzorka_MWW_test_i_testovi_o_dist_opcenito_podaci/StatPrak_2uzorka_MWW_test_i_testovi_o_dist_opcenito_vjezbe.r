#******Vjezbe 10 **********


#######################################################################
#   Test o lokacijskom pomaku  (Mann-Whitney-Wilcoxonov test)         #
#######################################################################


#### Zadatak 1.
#Jutarnju koncentraciju glukoze u krvi dijabeti?ara koji primaju novu i standardnu terapiju modeliramo
#neprekidnim distribucijama, a uzorci su nezavisni.
# F_X - funkcija distribucije koncentracije glukoze u krvi dijabeti?ara koji primaju novu terapiju
# F_Y - funkcija distribucije koncentraciju glukoze u krvi dijabeti?ara koji primaju standardnu terapiju
#Uo?imo da su uzorci jako mali i nikako ne bi mogli primijeniti t-test da potvrdimo uspje?nost nove terapije.
#Zato hipoteze postavljamo za MWW test.
#Uspje?nije je manja razina glukoze, zato ?elimo potvrditi hipotezu da je X manje od Y, odnosno da je pomak X-a
# u odnosu na Y negativan.
#Hipoteze:
# H0: F_X(x)=F_Y(x), za sve x \in R
# H1: za delta<0, F_X(x)=F_Y(x-delta), za sve x \in R
#odnosno:
# H0: X =d Y                             (nema razlike u terapijama)
# H1: za neki delta<0 X =d Y + delta     (nova terapija je uspje?nija)
#Koristimo MWW test
x <- c(6.8, 7.2, 7.4)
y <- c(8.6, 8.9, 7.3)
wilcox.test(x, y, alternative="less")
#p = 0.1 - na razini zna?ajnosti 0.05 ne odbacujemo H0,
# tj. ne mo?emo tvrditi da je nova terapija za dijabetes uspje?nija od standardne inzulinske terapije.
#Ako opcija exact u wilcox.test nije specificirana, izra?unava se egzaktna p-vrijednost ako su veli?ine 
#uzoraka manje od 50, ina?e se koristi normalna aproksimacija test-statistike i izra?unava se p-vrijednost
#kori?tenjem te aproksimacije. 
#wilcox.test koristi definiciju test-statistike kao u Mann-Whitney U testu, odnosno od sume rangova
#oduzima se najmanja mogu?a suma rangova (ovdje 7 - 6 = 1). Kako je test-statistika MWU testa linearna 
#transformacija test-statistike MWW testa, p-vrijednosti su jednake.





#### Zadatak 2.
#Fitness skor ljudi koji vje?baju kod ku?e i ljudi koji vje?baju u teretani modeliramo
# neprekidnim distribucijama, a uzorci su nezavisni.
# F_X - funkcija distribucije fitness skora ljudi koji vje?baju kod ku?e
# F_Y - funkcija distribucije fitness skora ljudi koji vje?baju u teretani
#?elimo testirati hipotezu da je X manje od Y, odnosno da je pomak X-a
# u odnosu na Y negativan.
#Hipoteze:
# H0: F_X(x)=F_Y(x), za sve x \in R
# H1: za delta<0, F_X(x)=F_Y(x-delta), za sve x \in R
#odnosno:
# H0: X =d Y                             (distribucija skora je jednaka)
# H1: za neki delta<0 X =d Y + delta     (ljudi koji vje?baju kod ku?e imaju ni?i skor od ljudi koji vje?baju u teretani)
#Koristimo MWW test
fitness <- read.csv2("fitness.csv")
str(fitness)
attach(fitness)
x <- skor[mjesto==0]
y <- skor[mjesto==1]
wilcox.test(x, y, alternative = "less")
#Zbog postojanja jednakih podataka u ovim uzorcima (ties - jednaki u spojenim uzorcima) nije mogu?e izra?unati egzaktnu p-vrijednost
#p=0.7438 - na razini zna?ajnosti 0.05 ne odbacujemo H0,
# tj. ne mo?emo tvrditi da ljudi koji vje?baju kod ku?e posti?u slabije rezultate od ljudi koji vje?baju u teretani.
#Uo?imo da je jedan uzorak mali i izra?eno nejednake veli?ine, stoga je MWW bolji izbor od t-testa.




#### Zadatak 3.
#Cijene takvih turisti?kih aran?mana na obali i na otocima modeliramo neprekidnim distribucijama, a uzorci su nezavisni.
# F_X - funkcija distribucije cijene takvog turisti?kog aran?mana na otocima
# F_Y - funkcija distribucije cijene takvog turisti?kog aran?mana na obali
##?elimo testirati hipotezu da je X ve?e od Y, odnosno da je pomak X-a
# u odnosu na Y pozitivan.
#Hipoteze:
# H0: F_X(x)=F_Y(x), za sve x \in R
# H1: za delta > 0, F_X(x)=F_Y(x-delta), za sve x \in R
#odnosno:
# H0: X =d Y                             (distribucija cijena je jednaka)
# H1: za neki delta > 0 X =d Y + delta  (cijene na otocima su ve?e nego na obali)
#Koristimo MWW test
turizam <- read.csv2("turizam.csv")
str(turizam)
attach(turizam)
wilcox.test(otoci, obala, alternative="greater")
# p < 0.05 - na razini zna?ajnosti 0.05 odbacujemo H0 i prihva?amo H1,
# tj. mo?emo tvrditi da su cijene takvih turisti?kih aran?mana na otocima vi?e od onih na obali.
#Zbog postojanja jednakih podataka nije mogu?e izra?unati egzaktnu p-vrijednost
#Ne bi bilo pogre?no testirati i hipotezu o o?ekivanju u ovoj situaciji koriste?i t-test:
t.test(otoci, obala, alternative="greater")
#Zaklju?ak je da su cijene na otocima ve?e.





#### Zadatak 4.
#Vremena do?ivljenja dolaze iz neprekidnih distribucija, a uzorci su nezavisni (razli?iti pacijenti).
# X - vrijeme do?ivljenja za pacijeta na terapiji A
# Y - vrijeme do?ivljenja za pacijeta na terapiji B
#Hipoteze:
# H0: X =d Y                             (distribucija vremena do?ivljenja su jednaka)
# H1: za neki delta > 0 X =d Y + delta  (metoda A je uspje?nije - ve?e vrijeme do?ivljenja)
library("BSDA")
str(Censored)
attach(Censored)
x <- survival[treatment == "A"]
y <- survival[treatment == "B"]
boxplot(x, y)
plot(density(x), col = "blue")
lines(density(y), col = "red")
#Uo?imo da su obje distribucije zna?ajno asimetri?ne. U takvim situacijama prikladnije je
# pomak distribucije testirati MWW testom nego testom o o?ekivanju.
wilcox.test(x, y, alternative="greater")
#p=0.0005437 - na razini zna?ajnosti 0.05 odbacujemo H0 i prihva?amo H1,
# mo?emo tvrditi da je metoda A uspje?nija.
#Kako su uzorci ve?i od 50, koristi se normalna aproksimacija MWW test statistike.
#Op?enito, egzaktna verzija mo?e se dobiti s:
wilcox.test(x, y, alternative="greater", exact=T)
#Zbog postojanja jednakih ovdje se ne mo?e primijeniti ?to rezultira warningom.






##########################################################
#              Chi-kvadrat test o homogenosti            #
##########################################################


#### Zadatak 5.
#Promatramo obilje?je s 5 mogu?ih vrijednosti u dvije populacije.
#Hipoteze:
# H0: distribucije ocjena kod metode A i metode B su jednake
# H1: distribucije ocjena kod metode A i metode B nisu jednake
metode <- matrix(c(14, 26, 34, 16, 10, 18, 36, 58, 56, 32), 2, 5, byrow = TRUE)
colnames(metode) <- c("1", "2", "3", "4", "5")
rownames(metode) <- c("1", "2")
metode
chisq.test(metode)
#p = 0.04241 < 0.05 - na razini zna?ajnosti 0.05 odbacujemo H0 i prihva?amo H1,
# mo?emo tvrditi da ove dvije metode rezultiraju razli?itim distribucijama ocjena.

chisq.test(metode)$expected      # tablica o?ekivanih (teorijskih) frekvencija u uvjetima istinitosti H0
chisq.test(metode)$observed      # tablica opa?enih frekvencija (tablica koju smo unijeli)
#Za usporedbu zaokru?imo te brojeve
round(chisq.test(metode)$expected)
chisq.test(metode)$observed
#Mo?emo promatrati i razlike
chisq.test(metode)$observed-round(chisq.test(metode)$expected)
#Vi?e je boljih ocjena kod druge metode - daje naslutiti da je ona bolja.
#Sli?no mo?emo uo?iti i ako pogledamo uvjetne relativne frekvencije:
prop.table(metode, 1)




#### Zadatak 6.
#Dva ishoda: 1 - pomogao, 0 - nije pomogao.
#Dvije populacije:
# 1. u ti?ini - distribucija  (   0        1    )
#                              1-theta1  theta1
# 2. buka - distribucija      (   0        1    )
#                              1-theta2  theta2
#?elimo testirati jesu li te dvije distribucije jednake.
#U ovom slu?aju jednakost distribucija se svodi na jednakost proporcija pa bi problem mogli
# postaviti i kao hipoteze o proporciji za nezavisne uzorke.
# H0: theta1 = theta2
# H1: theta1 != theta2
prop.test(c(60, 45), c(100, 100))
#p = 0.04744 < 0.05 odbacujemo H0, proporcije se razlikuju.
#Ako bi hipoteze postavili u terminima distribucija:
# H0: distribucije su jednake u ti?ini i u buci
# H1: nisu jednake
#Odgovaraju?a tablica frekvencija:
(buka <- matrix(c(40, 60, 55, 45), 2, 2, byrow = TRUE))
chisq.test(buka)
#p = 0.04744 < 0.05 odbacujemo H0, distribucije se razlikuju.
#Radi se o ekvivalentnim testovima, tako da je svejedno ho?emo li hipoteze postaviti kao
# hipoteze o distribuciji ili o proporciji. Za razliku od chi^2 testa, test o proporciji
# mo?emo iskoristiti samo kada obilje?je ima dva ishoda i promatra se u dvije populacije.
#Test koristi tzv. Yatesovu korekciju zbog neprekidnosti za 2x2 tablice.
#O?ekivane frekvencije:
chisq.test(buka)$expected
#Ukupno je pomoglo 105 ljudi od njih 200, 52.5%. Ako su distribucije jednake, onda
# na 100 ljudi  o?ekujemo da ?e pomo?i 52.5 njih.




#### Zadatak 7.
#Radi se o obilje?ju sa dva mogu?a ishoda promatranom u ?etiri populacije
#Hipoteze:
# H0: alkoholizam je jednako zastupljen u svim promatranim zanimanjima
# H1: alkoholizam nije jednako zastupljen u svim promatranim zanimanjima
(alkohol <- matrix(c(32, 268, 51, 199, 67, 233, 83, 267), 4, 2, byrow = TRUE))
chisq.test(alkohol)
#p = 0.0001277 < 0.05 - na razini zna?ajnosti 0.05 odbacujemo H0 i prihva?amo H1,
# mo?emo tvrditi da alkoholizam nije jednako zastupljen u svim promatranim zanimanjima.






#####################################################################
# Kolmogorov-Smirnovljev (KS) test za dva nezavisna slu?ajna uzorka #
#####################################################################


#### Zadatak 8.
#Iznose pla?a u tim poduze?ima modeliramo neprekidnim distribucijama
# F_X - funkcija distribucije iznosa godi?nje pla?e u prvom poduze?u
# F_Y - funkcija distribucije iznosa godi?nje pla?e u konkurentskom poduze?u
#Uzorci su nezavisni.
#Hipoteze:
# H0: F_X(x)=F_Y(x), za sve x \in R
# H1: F_X(x)!=F_Y(x), za neki x \in R
#Grafovi empirijskih funkcija distribucije
djelatnici <- read.csv2("djelatnici.csv")
str(djelatnici)
attach(djelatnici)
#Koristimo Kolmogorov-Smirnovljev test za dva nezavisna slu?ajna uzorka
ks.test(placa_poslije, placa_konkurencija)   #default vrijednost za alternative je "two.sided"
#p=1.264e-12 < 0.05 - na razini zna?ajnosti 0.05 odbacujemo H0 i prihva?amo H1,
# mo?emo tvrditi da su visine godi?njih pla?a u ta dva poduze?a razli?ito distribuirane.
#Empirijske funkcije distribucije mo?emo usporediti grafi?ki:
plot(ecdf(placa_poslije), col = "blue")
plot(ecdf(placa_konkurencija), add = TRUE, col = "red")
#Uo?imo da distribucije imaju razli?ite oblike, MWW test ovdje stoga ne bi bio prikladan
# jer zahtijeva da se distribucije razlikuju samo za pomak.



#### Zadatak 9.
#(a)
#Cijene takvih turisti?kih aran?mana na obali i na otocima modeliramo neprekidnim distribucijama.
# F_X - funkcija distribucije cijena na obali
# F_Y - funkcija distribucije cijena na otocima
#Uzorci su nezavisni.
#Hipoteze:
# H0: F_X(x)=F_Y(x), za sve x \in R
# H1: F_X(x)!=F_Y(x), za neki x \in R
turizam <- read.csv2("turizam.csv")
str(turizam)
attach(turizam)
ks.test(obala, otoci)
#p = 3.934e-13 < 0.05 - na razini zna?ajnosti 0.05 odbacujemo H0 i prihva?amo H1,
# mo?emo tvrditi da se cijene razlikuju.
#Warning upozorava na to da u promatranim uzorcima ima jednakih cijena, pa se p-vrijednost KS testa odre?uje
# kori?tenjem asimptotske distribucije KS test-statistike.

#(b)
#Dodatna pretpostavka u MWW testu jest da se distribucije razlikuju samo za lokacijski pomak (istog su "oblika").
plot(ecdf(obala), col = "blue", xlim = c(min(c(obala, otoci)), max(c(obala, otoci))))
plot(ecdf(otoci), add = T, col = "red")
#Na osnovu grafova, nije pogre?no to i pretpostaviti.
wilcox.test(obala, otoci)
#p = 2.54e-12 < 0.05 - na razini zna?ajnosti 0.05 odbacujemo H0 i prihva?amo H1,
# mo?emo tvrditi da se cijene razlikuju, odnosno da postoji lokacijski pomak razli?it od 0.

#(c)
#Obala:
plot(density(obala))
shapiro.test(obala)
#p = 0.1841 > 0.05 - ne odbacujemo nul-hipotezu, nema razloga sumnjati u normalnu distribuiranost
#Otoci:
plot(density(otoci))
shapiro.test(otoci)
#p = 0.1015 > 0.05 - ne odbacujemo nul-hipotezu, nema razloga sumnjati u normalnu distribuiranost

#Hipoteze o o?ekivanju:
# H0: mu1 = mu2
# H1: mu1 != mu2
t.test(obala, otoci)
#p < 0.05 - na razini zna?ajnosti 0.05 odbacujemo H0 i prihva?amo H1,
# mo?emo tvrditi da se o?ekivane cijene razlikuju (pa onda i distribucije).

#Uo?imo da smo s tri razli?ita testa testirali vrlo sli?ne hipoteze.





#### Zadatak 10.
#Vrijeme koje p?ela provede u blizini stabala jabuka ovih dviju sorti modeliramo neprekidnom distribucijama,
#a uzorci su nezavisni.
# F_X - funkcija distribucije vremena koje p?ela provede u blizini stabla sorte Whitney Crab
# F_Y - funkcija distribucije vremena koje p?ela provede u blizini stabla sorte Redwell
#Hipoteze:
# H0: F_X(x)=F_Y(x), za sve x \in R
# H1: F_X(x)!=F_Y(x), za neki x \in R
jabuka <- read.csv2("jabuka.csv")
str(jabuka)
attach(jabuka)
#Koristimo Kolmogorov-Smirnovljev test za dva nezavisna slu?ajna uzorka
ks.test(wc, rw)
#Warning upozorava na to da u promatranim uzorcima ima jednakih vrijednosti, pa se p-vrijednost KS testa odre?uje
# kori?tenjem asimptotske distribucije KS test-statistike
#p=0.05156 - na razini zna?ajnosti 0.05 ne odbacujemo H0,
# ne mo?emo tvrditi da p?ele preferiraju jednu od dviju promatranih sorti jabuka.
#Grafovi empirijskih funkcija distribucije
plot(ecdf(wc), col = "blue")
plot(ecdf(rw), add = TRUE, col = "red")

