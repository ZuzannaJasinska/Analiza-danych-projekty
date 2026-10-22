#Zadanie 1: Zebrano dane związane z zadeklarowanym poziomem aktywności fizycznej (Niski, Średni, Wysoki) 
#i subiektywną jakością snu (Dobra, Słaba).
#Utwórz tabelę kontyngencji, a następnie przy pomocy testu chi-kwadrat zbadaj zależność
#pomiędzy badanymi cechami. Napisz czym jest testowana hipoteza zerowa i alternatywna w tym teście.

#Następnie dokonaj interpretacji otrzymanego w teście wyniku i napisz wnioski statystyczne.
#Wykonaj wizualizację do zadania polegającą na stworzeniu wykresów słupkowych dla
#jakości snu w zależności od poziomu aktywności (funkcja ggplot). Zinterpretuj otrzymany wykres.
#Wykonaj wykres mozaikowy (funkcja mosaicplot) aby wizualnie przedstawić zależności między zmiennymi i zinterpretuj otrzymany wykres.

set.seed(123)
library(ggplot2)
library(vcd)

aktywnosc <- sample(c("Niski", "Średni", "Wysoki"), 250, replace = TRUE)
jakosc_snu <- sample(c("Dobra", "Słaba"), 250, replace = TRUE, prob = c(0.6, 0.4))



#### ROZWIAZANIE

### Tabela 

dane <- data.frame(
  aktywnosc <- sample(c("Niski", "Średni", "Wysoki"), size=250, replace = TRUE),
  jakosc_snu <- sample(c("Dobra", "Słaba"), size=250, replace = TRUE, prob = c(0.6, 0.4))
)

tabela_kontyngencji <- table(dane$aktywnosc, dane$jakosc_snu)
tabela_kontyngencji


### Test CHI
#Hipoteza_0: poziom aktywności nie ma wpływu na jakość snu (nie ma korelacji),
#Hipoteza alternatywna: poziom aktywności ma pływ na jakość snu (jest korelacja).

test_chi_kwadrat <- chisq.test(tabela_kontyngencji)
print(test_chi_kwadrat)

print(test_chi_kwadrat$p.value)




### Wniosek statystyczny 

# Hipoteza_0 zakłada, że nie ma związku między aktywnością fizyczną a jakością snu. Hipoteza alternatywna zakłada,
# że związek między tymi cechami istnieje. p-value naszego badania jest równe ok. alfa = 0,929. Alfa>0,05, nie mamy podstaw do odrzucenia
# Hipotezy_0, na korzyść Hipotezy alternatywnej. Wniosek jest jeden: nie istnieje zależność między aktywnością fizyczną a jakością snu.

### Wizualizacja zadania



wykres_zadania1_1 <- ggplot(dane, aes(x=jakosc_snu, fill=aktywnosc)) + 
  geom_bar(position = "dodge") +
  labs(title = "Jakość snu w zależności od poziomu aktywności", x = "Jakość snu", y = "Poziom aktywności fizycznej")
print(wykres_zadania1_1)

# Najwięcej obserwacji zaobserwowaliśmy u osób z wysoką aktywnością fizyczną i dobrą jakością snu,
# a najmniej u osób ze średnią aktywnością fizyczną i słabą jakością snu. Można zauważyć podobną ilość obserwacji
# w każdej z grup o słabej jakości snu. Ilości osób z dobrą jakością snu w każdej z grup jest bardziej zróżnicowana.
# Nawet dwukrotnie więcej osób uznaje swoją jakość snu za dobrą, w porównaniu ze słabą jakością snu.




wykres_zadania1_2 <- mosaicplot(~aktywnosc+jakosc_snu, data=dane, col=c("green4", "red4"), 
                                main ="Zależność między jakością snu a aktywnością",
                                xlab="Aktywność fizyczna",
                                ylab="Jakość snu")
print(wykres_zadania1_2)

# Osób o dobrej jakości snu jest nawet dwukrotnie więcej, niż osób o słabej jakości snu. 
# Największa różnica w ilości obserwacji między słabą a dobrą jakością snu jest wśród osób o wysokiej aktywności fizycznej. 







#Zadanie 2: Ankietowano 500 osób na temat ich wymarzonego kierunku wakacyjnego (Morze, Góry, Mazury, Zagranica). 
#Sprawdź, czy otrzymany rozkład jest zgodny z założeniem, że kierunki te nie są równocenne i ich teoretyczny rozkład 
#wynosi odpowiednio: Morze (30%), Góry (30%), Mazury (20%), Zagranica (20%).
#Podaj hipotezy w tym teście oraz napisz odpowiednie wnioski statystyczne.
#Narysuj wykres słupkowy i poddaj interpretacji.

kierunki <- c("Morze", "Góry", "Mazury", "Zagranica")
wybory <- sample(kierunki, 500, replace = TRUE, prob = c(0.35, 0.25, 0.15, 0.25))


#### Rozwiązanie 

dane_2 <- data.frame(kierunki <- c("Morze", "Góry", "Mazury", "Zagranica"),
                     wybory <- sample(kierunki, 500, replace = TRUE, prob = c(0.35, 0.25, 0.15, 0.25))
)


### TEST CHI ZGODNOSCI

#Hipoteza_0: rozkłady empiryczne (zaobserwujemy) i teoretyczne (30%,30%,20%,20%) będą równe,
#Hipoteza alternatywna: rozkład empiryczny i teoretyczny będą istotnie różniły się od siebie.

prawd_teor <- c(0.3,0.3,0.2,0.2)
zliczenie <- table(wybory)

 
test_chi_kwadrat2 <- chisq.test(zliczenie, p = prawd_teor) 
print(test_chi_kwadrat2)


print(test_chi_kwadrat2$p.value)



### Wniosek statystyczny 

# Hipoteza_0 zakłada, że otrzymany rozkład jest zgodny z założeniem że kierunki nie są równocenne,
# a liczebność wynosi kolejno 30%, 30%, 20%, 20% (teoretyczny). Hipoteza alternatywna zakłada, 
# że liczebność (prawd. empiryczne) nie pokryje się z rozkładem teoretycznym.
# p-value naszego badania jest równe ok. alfa = 1.453 * 10^(-19). Alfa<0,05, mamy podstawy do odrzucenia
# Hipotezy_0, na korzyść Hipotezy alternatywnej. Wniosek: Rozkład zaobserwowany (empiryczny) nie pokrywa się z 
# rozkładem przypuszczonym (teoretycznym).


### Wizualizacja zadania



wykres_zadania2_1 <- ggplot(dane_2, aes(x=wybory, fill=wybory)) + 
  geom_bar(position = "dodge") +
  labs(title = "Kierunki i liczebność wyborów", x = "Kierunki podróży", y = "Liczebność wyborów")
print(wykres_zadania2_1)

# Zdecydowana większość osób wybiera kierunek podróży morze, najrzadziej natomiast wybierane są Mazury,
# dodatkowo morze zostało wybrane nawet 2 razy częsciej od Mazur. Góry i zagranica zostały wybrane niemalże tak samo często.







