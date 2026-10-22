#Zadanie 1: Przeprowadzono badanie wydajności nowej aplikacji mobilnej, 
#mierząc czas jej pełnego załadowania (w milisekundach) na różnych urządzeniach.

# Dane (czas załadowania w ms)
czas.ladowania <- c(
  1150, 1210, 1180, 1250, 1195, 1170, 1230, 1140, 1205, 1165,
  1225, 1155, 1190, 1200, 1240, 1145, 1185, 1215, 1220, 1175,
  1135, 1212, 1235, 1160, 1198, 1245, 1188, 1208, 1152, 1228,
  1232, 1148, 1192, 1218, 1202)

# Polecenia:

# 1. Przeprowadź dwa wybrane testy, aby sprawdzić normalność danych (np. Shapiro-Wilk i Lilliefors).
# 2. Zbuduj 99% przedział ufności dla średniego czasu ładowania.
# 3. Zbuduj 99% przedział ufności dla wariancji i odchylenia standardowego czasu ładowania.
# 4. Napisz wnioski statystyczne. 
# 5. Narysuj histogram z naniesioną linią średniej oraz przedziałem ufności i go zinterpretuj.

# Wczytanie bibliotek
library(ggplot2)
library(gridExtra)
library(nortest)
#install.packages("nortest")

#### AD 1.

#Hipoteza zerowa $H0$ : Dane pochodzą z rozkładu normalnego
#Hipoteza alternatywna $H1$ : Dane nie pochodzą z rozkładu normalnego




# a) test Shapiro-Wilka
test_shapiro <- shapiro.test(czas.ladowania)
test_shapiro$p.value 
#p.value=0,2722305. Skoro tak, to p.value>0.05, nie ma podstaw do odrzucenia $H0$, stąd dane z próby pochodzą z rozkładu normalnego.

# b) test Liliefors
lillie_normalny <- lillie.test(czas.ladowania)
lillie_normalny$p.value
#p.value=0,7621489. Skoro tak, to p.value>0.05, nie ma podstaw do odrzucenia $H0$, stąd dane z próby są zbliżone do rozkładu normalnego.

#### AD 2. przedział ufności dla średniego czasu ładowania (nieznane odchylenie)

alfa <- 0.01
m <- mean(czas.ladowania) #srednia
n <- length(czas.ladowania) #ilosc obserwacji
S <- sd(czas.ladowania) #odchylenie
c(m,S) #(wektor sredniej i odchylenia)

L1 <- m - qt(1-alfa/2,n-1)*S/sqrt(n)
U1 <- m + qt(1-alfa/2,n-1)*S/sqrt(n)

cat("Przedział ufności na poziomie ",1-alfa,"dla średniej (sigma - nieznane):\n")
cat("[",L1,";",U1,"]  milisekund \n")

#99% przedział ufności dla średniego czasu ładowania to (1179,118; 1209,339)

#### AD 3. estymacja przedziałowa wariancji i odchylenia standardowego 

alfa <- 0.01
n <- length(czas.ladowania)
S <- sd(czas.ladowania)

L <- (n-1)*S/qchisq(1-alfa/2,n-1)
U <- (n-1)*S/qchisq(alfa/2,n-1)

cat("Przedział ufności na poziomie ",1-alfa,"dla wariancji :\n")

cat("[",L,";",U,"]  milisekund \n") 

#99% przedział ufności dla wariancji czasu ładowania to (18,892; 67,508)


Ls <- sqrt(L) #pierwiastek z kazdej ze stron przedzialu dla wariancji
Us <- sqrt(U) # -//-

cat("Przedział ufności na poziomie ",1-alfa,"odchylenia standardowego :\n")

cat("[",Ls,";",Us,"]  milisekund \n") 
#99% przedział ufności dla odchylenia standardowego czasu ładowania to (4,346; 8,216)


#### AD 4. WNIOSKI STATYSTYCZNE

# Dwa przeprowadzone testy zgodności z rozkładem normalnym, Shapiro-Wilk oraz Lilliefors, jednogłośnie pokazały, że badana próba faktycznie pochodzi z rozkładu normalnego.
# 99% przedział ufności dla średniego czasu ładowania to (1179,118; 1209,339) milisekundy. To oznacza, że z 99% prawdopodobieństwem, 
# średni czas ładowania aplikacji mobilnej trwa od 1179,118 do 1209,339 milisekundy. 
# 99% przedział ufności dla wariancji czasu ładowania to (18,892; 67,508). Inaczej mówiąc, mamy 99% pewność, że wariancja czasu ładowania znajdzie się w przedziale od
# 18,892 do 67,508 milisekund.
# 99% przedział ufności dla odchylenia standardowego czasu ładowania to (4,346; 8,216) milisekundy. Oznacza to, że z 99% prawdopodobieństwem czas z jakim ładuje się aplikacja,
# na losowo wybranym urządzeniu, odbiega od średniej o od 4,346 do 8,216 milisekundy. 





#### AD 5. Histogram z naniesioną linią średniej oraz przedziałem ufności, interpretacja

ggplot(data.frame(czas.ladowania), aes(x = czas.ladowania)) +
  geom_histogram(aes(y = ..density..), fill = "lightgray", color = "white", bins = 15) +
  geom_vline(xintercept = c(L1, U1), linetype = "dashed", color = "blue", size = 1) +
  geom_vline(xintercept = m, color = "black", linetype = "solid", size = 1.2) +
  labs(title = "Histogram czasu ładowania aplikacji mobilnej z nałożonym przedziałem ufności 99%",
       x = "Czas ładowania aplikacji mobilnej [ms]", y = "Gęstość") + 
  theme_minimal()



# INTERPRETACJA

# Histogram przedstawia czas ładowania aplikacji mobilnej. Najmniej obserwacji odnotowano ok. 1135, 1160 oraz 1250 milisekund.
# Najczęsciej natomiast aplikacja ładowała się około 1152 oraz 1214 milisekund. 
# Czarna linia oznaczająca średnią ładowania aplikacji znajduje się (prawie) dokładnie na środku. Histogram jest symetryczny.
# Niebieskie przerywane linie zaznaczają 99% przedział ufności. Oznacza to, że mamy 99% pewności co do tego, czy 
# średnia (czarna linia) znajdzie się właśnie w przedziale pomiędzy niebieskimi liniami. 
# Na naszym histogramie spełnia się to przypuszczenie.
  







#Zadanie 2: Producent laptopów twierdzi, że nowa bateria wytrzymuje średnio co najmniej 10 godzin (600 minut) pracy. 
#Testerzy podejrzewają jednak, że czas ten jest w rzeczywistości krótszy.

# Dane (czas pracy w minutach):
bateria <- c(585, 592, 605, 578, 588, 595, 582, 570, 598, 605, 602, 586)

# Polecenia:

# 1. Sformułuj hipotezę zerową (H0) oraz alternatywną (H1).
# 2. Zbadaj prawdziwość hipotezy przy użyciu wbudowanej funkcji t.test() z odpowiednim parametrem 'alternative'.
# 3. Napisz funkcję t_test_manual, która obliczy:
#    - średnią z próby, odchylenie standardowe,
#    - statystykę testową t,
#    - zbiór krytyczny dla testu (zależny od wariantu hipotezy alternatywnej: dwustronny, prawo lub lewostronny),
#    W wyniku funkcja zwraca rezultat testu.
# Następnie zweryfikuj uzyskany w poleceniu 2. wynik przy użyciu tej funkcji.
# 4. Przeprowadź wizualizację rozkładu t-Studenta z zaznaczonym obszarem krytycznym 
# oraz wartością statystyki t. Napisz wnioski.


#### AD 1. 


# Hipoteza zeroa $H0$ : Bateria laptopów wytrzymuje średnio co najmniej 10 godzin (600 minut) pracy.
# Hipoteza alternatywna $H1$ : Bateria laptopów wyczerpuje się wcześniej niż po 10 godzinach pracy.



#### AD 2. Badanie prawdziwości hipotezy za pomocą t.test()


#Szukamy alternatywy lewostronnej, ponieważ testerzy podejrzewają krótszy czas pracy laptopa, od tego który zapewnia producent. 

m0 <- 600 #średnia teoretyczna, ustalona przez producenta
m <- mean(bateria) #średnia empiryczna
n <- length(bateria) 
alfa <- 0.05 #poziom istotności
s <- sd(bateria)


test_lewostronny <- t.test(bateria,mu=m0,alternative="less",conf.level=1-alfa) 
test_lewostronny$p.value
#p.value=0,0062. Skoro tak, to p.value<0.05, są podstawy do odrzucenia $H0$ na korzyść $H1$,
# stąd bateria laptopów wyczerpuje się istotnie wcześniej niż zachwala producent.




#### AD 3. funkcja t_test_manual


t_test_manual <- function(sample, mu0, alpha, alternative) {
  
  # Wielkość próby
  n <- length(sample)
  
  # Średnia z próby
  x_bar <- mean(sample)
  
  # Odchylenie standardowe
  sigma <- sd(sample)
  
  # Obliczenie statystyki testowej t
  t_stat <- (x_bar - mu0) / (sigma / sqrt(n))
  
  
  # Wyznaczenie przedziału krytycznego
  if (alternative == "two.sided") {
    t_crit <- qt(1 - alpha / 2, df = n-1) # Wartość krytyczna dla alfa/2
    critical_region <- c(-Inf, -t_crit, t_crit, Inf) # Dwustronny przedział krytyczny
    reject <- (t_stat < -t_crit) | (t_stat > t_crit)
  } else if (alternative == "greater") {
    t_crit <- qt(1 - alpha, df = n-1) # Jednostronny test prawostronny
    critical_region <- c(t_crit, Inf)
    reject <- (t_stat > t_crit)
  } else if (alternative == "less") {
    t_crit <- qt(alpha, df  = n-1) # Jednostronny test lewostronny
    critical_region <- c(-Inf, t_crit)
    reject <- (t_stat < t_crit)
  } else {
    stop("Nieprawidłowa wartość argumentu 'alternative'. Użyj 'two.sided', 'greater' lub 'less'.")
  }
  
  # Pokazanie wyników
  cat("Średnia z próby:", x_bar, "\n")
  cat("Odchylenie standardowe:", sigma, "\n")
  cat("Statystyka testowa t:", round(t_stat, 4), "\n")
  cat("Przedział krytyczny:", critical_region, "\n")

  
  # Wniosek
  if (reject) {
    cat("Odrzucamy H0 na poziomie istotności", alpha, "\n")
  } else {
    cat("Brak podstaw do odrzucenia H0 na poziomie istotności", alpha, "\n")
  }
  
}

# WERYFIKACJA TESTU Z 2. PUNKTU
t_test_manual(bateria, m0, alpha = 0.05, alternative = "less")

# t_test_manual zweryfikował wynik z 2. punktu zadania. Oba testy odrzucają $H0$ na korzyść alternatywnej.
# Wniosek jest jednogłośny - bateria laptopa działa krócej, niż twierdzi producent.



#### AD 4.  



# wartosc statystyki t
t_stat <- (m - m0) / ( s / sqrt(n))

#obszar krytyczny. Dla lewostronnego przedzial (-Inf, t_crit)
t_crit <- qt(alfa, df=n-1)


ggplot(data.frame(x=c(-4,4)), aes(x)) +
  stat_function(fun = dt, args = list(df = n-1), color = "red", size = 1.2) +
  geom_vline(xintercept = t_crit, linetype = "dashed", color = "blue", size = 1) +
  geom_vline(xintercept = t_stat, color = "black", linetype = "solid", size = 1.2) +
  labs(title = "Wizualizacja t_Studenta z zaznaczonym obszarem krytycznym i wartością statystyki t",
       x = " Skala t", y = "Gęstość") +
  theme_minimal()

# Wizualizacja przedstawia rozkład t-Studenta (czerwony wykres) wraz z zaznaczonym obszarem krytycznym
# (na lewo od niebieskiej przerywanej linii) i wartością statystyki t (czarna linia).
# W badaniu rozważaliśmy alternatywę lewostronną, stąd obszar krytyczny (obszar zaznaczony po lewej stronie od niebieskiej przerywanej linii),
# to (-Inf; -1,79). Wartość statystyki t wyniósł -2,98. Statystyka t mieści się w obszarze krytycznym.
# Oznacza to, że odrzucamy $H0$ na korzyść alternatywnej - bateria w laptopach działa krócej niż zapewnia producent. 
# Wniosek jest taki sam, jaki wypowiedziała funkcja t_test_manual.


