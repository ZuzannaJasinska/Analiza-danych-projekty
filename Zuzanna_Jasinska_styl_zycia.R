#Zadanie: Wpływ stylu życia na poziom cholesterolu
#Zebrano dane od 150 dorosłych osób dotyczące różnych aspektów stylu życia oraz ich poziomu cholesterolu (mg/dL). Celem badania jest określenie, 
#które czynniki stylu życia mają największy wpływ na poziom cholesterolu całkowitego.

#Zmienne:
#wiek – wiek respondenta (lata, ilościowa)
#aktywnosc_fizyczna – liczba godzin aktywności fizycznej tygodniowo (ilościowa)
#BMI – wskaźnik masy ciała (ilościowa)
#pali – czy osoba pali papierosy (0 = nie, 1 = tak, binarna)
#cholesterol – poziom cholesterolu całkowitego w mg/dL (zmienna zależna, ilościowa)

regresja2 <- readRDS("C:/Users/zuzaz/Downloads/dane_regresja2.rds")


#Wykonaj następujące polcenia:
#1. Zamień typ zmiennej pali na factor o odpowiednich poziomach. Zbuduj model regresji liniowej, 
#w którym zmienną zależną jest cholesterol, a predyktorami są: wiek, aktywnosc_fizyczna, BMI, pali.

#2. Sprawdź współczynniki regresji: Jaki wpływ na poziom cholesterolu mają poszczególne zmienne? Które zmienne są istotne statystycznie?
#Oceń jakość dopasowania modelu (R²).

#3. Zweryfikuj założenia modelu regresji: Sprawdź, czy występują problemy z multicollinearity (oblicz VIF) oraz jakie są reszty modelu.

#4. Narysuj i zinterpretuj wykresy diagnostyczne.

#5. Wykonaj predykcję z 95% przedziałami ufności dla:
#a) Osoba A: 45 lat, aktywność fizyczna = 3, palenie = 1 (pali), BMI = 27
#b) Osoba B: 45 lat, aktywność fizyczna = 3, palenie = 0 (nie pali), BMI = 27
#Porównaj wyniki i wyciągnij wnioski dotyczące wpływu palenia na cholesterol.

#6. Zrób wykres regresji z linią regresji badający zależności między zmiennymi aktywnosc_fizyczna i cholesterol .

#7. Przeprowadź analizę korelacji między zmiennymi (zwróć uwagę na problemy związane ze zmiennymi typu factor). 
#Narysuj wybrany wykres i zinterpretuj.



### ROZWIĄZANIE


#AD 1) zamień typ zmiennej pali na factor


### Dane typu factor
pali_2=factor(regresja2$pali,levels=c("1","0"),labels=c("1","0"))
summary(pali_2)


#Pomocnicza zamiana nazw zmiennych
palenie<-pali_2
cholesterol <- regresja2$cholesterol
wiek <- regresja2$wiek
aktywnosc <- regresja2$aktywnosc_fizyczna
BMI <- regresja2$BMI

dane <- data.frame(cholesterol, wiek, aktywnosc, BMI, palenie)
head(dane)

### Model regresji liniowej
model <- lm(cholesterol ~ wiek + aktywnosc + BMI + palenie, data = dane)
summary(model)


#AD 2) spr. współczynniki regresji (Jaki wplyw maja poszczegolne zmienne, ktore zmienne sa istotne statystycznie, ocen jakosc dopasowania)

summary(model)

### Każdy rok podwyższa poziom cholesterolu o 0,55 punkta, przy założeniu, że reszta czynników się nie zmienia.
### Każda godzina aktywność fizycznej obniża poziom cholesterolu o 2,48 punkta, jeśli reszta czynników się nie zmienia.
### Każdy punkt na skali BMI podwyższa poziom cholesterolu o 2,83 punkta, przy założeniu, że reszta czynników się nie zmienia. 
### Każda osoba niepaląca ma (średnio) niższy poziom cholesterolu o 15,32 punkta od osoby palącej, przy zachowaniu reszty współczynników bez zmian.

#H0 mówi, że współczynnik jest nieistotny statystycznie. H1 głosi, że współczynnik jest istotny statystycznie i warto zgłębiać dane.

### W naszym modelu p.value każdej zmiennej jest zdecydowanie mniejszy niż standardowy poziom istotności wynoszący 5%.
### Skoro dla każdej zmiennej p.value < 0,05, to należy odrzucić hipotezę zerową na korzyść alternatywnej
### - wiek, aktywność, BMI i niepalenie są współczynnikami istotnymi statystycznie.


### Współczynnik R^2=0,54, stąd model tłumaczy 54% zmienności w wynikach wydajności. Model dopasowuje się do danych niewiele lepiej niż "średnio".


#Ad 3)  sprawdź, czy występują problemy z multicollinearity (oblicz VIF) oraz jakie są reszty modelu

library(car)
vif(model)
# Wartości wskaźnika VIF wynoszą kolejno dla wieku 1,0452, dla aktywności fizycznej 1,0336, dla BMI 1,0511 oraz dla wskaźnika palenia 1,0219.
# Skoro VIF jest bardzo bliski wartości 1, oznacza, że te zmienne są liniowo niezależne.


summary(residuals(model))
#Średnia wynosi dokładnie zero - metoda najmniejszych kwadratów wykonana poprawnie.
#Mediana bardzo bliska zeru (Me = -0,4551). Model jest prawie całkowicie symetryczny - nie ma tendencji do zawyżania, albo zaniżania prognoz cholesterolu.
#Największy błąd popełniony przez model w dół wynosi 27.6035, a w górę wynosi 32,4996.
#Pierwszy i trzeci kwantyl mówi o rozrzucie błedu w szacowaniu cholesterolu. Dla połowy pacjentów ta różnica wynosi 5,86 do 6,72 w każdą ze stron,
#Jest to stosunkowo mały zakres błędu. Model jest stabilny.


#AD 4)

par(mfrow = c(2, 2))
plot(model)


# 1. Residuals vs Fitted
# Punkty są dosyć równomiernie rozrzucone - linia trendu jest niemalże pozioma. 
# Nie pojawia się lejek, czyli nie ma heteroscedastyczności.
# Model jest liniowy, a wariancja reszt jest stała.

# 2. Normal Q-Q
# Wykres sprawdza normalność rozkładu reszt. Punkty leżą bardzo blisko prostej
# — reszty są prawie normalne, spełnione jest jedno z założeń regresji liniowej.
shapiro.test(residuals(model)) 
#test potwierdza normalność (p.value>0,05 = nie ma podstaw do odrzucenia H0 mówiącej o pochodzeniu danych z rozkładu normalnego)

# 3. Scale-Location (Spread-Location)
# Wykres sprawdza jednorodność wariancji reszt. Punkty są równomiernie rozrzucone, 
# a czerwona linia jest w miarę płaska — oznacza to stałą wariancję reszt (homoscedastyczność).

# 4. Residuals vs Leverage
# Wykres identyfikuje punkty wpływowe. Punkty w centrum wykresu leżą w granicach tolreancji. 
# Dystant Cook'a nie mieści się na wykresie - nie ma obserwacji, które zniekształcają model.


#AD 5)
#5. Wykonaj predykcję z 95% przedziałami ufności dla:
#a) Osoba A: 45 lat, aktywność fizyczna = 3, palenie = 1 (pali), BMI = 27
#b) Osoba B: 45 lat, aktywność fizyczna = 3, palenie = 0 (nie pali), BMI = 27
#Porównaj wyniki i wyciągnij wnioski dotyczące wpływu palenia na cholesterol.

#Osoba paląca = A, osoba niepaląca = B

#a) Osoba A: 45 lat, aktywność fizyczna = 3, palenie = 1 (pali), BMI = 27
osoba_A <- data.frame(
  wiek = 45,
  aktywnosc=3,
  palenie="1",
  BMI=27)


#b) Osoba B: 45 lat, aktywność fizyczna = 3, palenie = 0 (nie pali), BMI = 27
osoba_B <- data.frame(
  wiek = 45,
  aktywnosc=3,
  palenie="0",
  BMI=27)


predykcja_A <- predict(model, newdata = osoba_A, interval = "confidence") # osoba A z przedziałami ufności
predykcja_A

#Osoba A (palacą) ma przewidziany cholesterol w wysokości 276,7212 (mg/dL). 95% przedział ufności wyniósł [273,129; 280,3135], czyli
# na 95% mamy pewność, że cholesterol osoby palącej mającej te same wartości parametrów (45 lat, aktywność 3, BMI równe 27),
# będzie mieścił się w zakresie [273,129; 280,3135] (mg/dL).




predykcja_B <- predict(model, newdata = osoba_B, interval = "confidence") # osoba B z przedziałami ufności
predykcja_B

#Osoba B (niepaląca) ma przewidziany cholesterol w wysokości 261,4007 (mg/dL). 95% przedział ufności wyniósł [259,2044; 263,597], czyli
# na 95% mamy pewność, że cholesterol osoby niepalącej o dokładnie tych samych parametrach (45 lat, aktywność 3, BMI równe 27),
# będzie mieścił się w zakresie [259,2044; 263,597] (mg/dL).



roznica_fit <- predykcja_A[1]-predykcja_B[1]
roznica_fit


# Różnica między przewidywanymi poziomami cholesterolu wśród osób palących i niepalących, o pozostałych cechach stałych
# (45 lat, aktywność 3, BMI równe 27), wynosi dokładne 15,32054. Osoby palące mają średnio podwyższony poziom cholesterolu o 15,32054 mg/dL,
# co dokładnie pokrywa się z poprzednimi przewidywaniami z kodu "summary(model)" (wniosek na linijce 71).

summary(model)



#AD 6) Zrób wykres regresji z linią regresji badający zależności między zmiennymi aktywnosc_fizyczna i cholesterol .
library(ggplot2)

ggplot(dane, aes(x = aktywnosc, y = cholesterol)) + # Zmieniono zmienne na właściwe!
  geom_point(alpha = 0.6, color = "gray40") +
  geom_smooth(method = "lm", se = TRUE, color = "purple4", fill = "purple", size = 1.2) +
  labs(
    title = "Zależność między poziomu cholesterolu a aktywnością fizyczną",
    x = "Aktywność fizyczna",
    y = "Poziom cholesterolu"
  )



#AD 7)
zmienne_ilosciowe <- dane[, c("cholesterol", "wiek", "aktywnosc", "BMI")]

#macierz korelacji
cor(zmienne_ilosciowe)

library(corrplot)
corrplot(cor(zmienne_ilosciowe), method = "color", addCoef.col = "black",
         tl.col = "black", tl.srt = 45, number.cex = 0.8, diag = FALSE)


#Korelacja między cholesterolem a wiekiem wynosi 0,36. Oznacza to, że wraz z wiekiem, poziom cholesterolu się zwiększa. To dosyć umiarkowany związek.
#Korelacja między cholesterolem a aktywnością wynosi -0,18. To oznacza, że poziom cholesterolu spada przy zwiększaniu aktywności. Korelacja jest słaba - zmienne są prawie niezależne
#Korelacja między cholesterolem a BMI wynosi 0,39. Wraz ze wzrostem BMI zwiększa się poziom cholesterolu. Korelacja jest umiarkowana i większa od tej między wiekiem a cholesterolem.
#Korelacja między wiekiem a aktywnością wynosi -0,13. Sens logiczny: wraz z wiekiem, aktywność fizyczna ma tendencje spadkowe. Zależność jest niska, zmienne są niezależne
#Korelacja między wiekiem a BMI wynosi -0,16. Sens logiczny: wraz z wiekiem BMI spada, jednak ta korelacja jest niska - te zmienne są prawie niezależne
#korelacja między aktywnością a BMI wynosi 0,12. Sens logiczny: wraz ze wzrostem aktywności BMI rośnie. Ta korelacja jest jednak bardzo niska - zmienne są niezależne

# Gdy korelacja wynosi mieści się w przedziale +-[0; 0,3], korelacja jest na tyle niska, że można uznać zmienne za niezależne.
# Zmienne na siebie w rzeczywistości nie wpływają.
# Korelacje między dwiema takimi samymi zmiennymi, naturalnie są równe 1.







