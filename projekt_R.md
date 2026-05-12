# Projekt: Optymalizacja portfela efektywnego – model Markowitza

**Spółki:** CD Projekt, Archicom, CCC, Inter Cars, Makarony Polskie  
**Autorzy:** Maja Łach, Weronika Pelińska  

---

## Wstęp

Celem projektu jest budowa efektywnego portfela inwestycyjnego złożonego z akcji 5 spółek notowanych na GPW w Warszawie, z zastosowaniem modelu Markowitza.

Spółki zostały wybrane na podstawie analizy statystycznej tygodniowych logarytmicznych stóp zwrotu z okresu 2024-01-01 do 2026-01-11. Kryterium selekcji była dodatnia średnia stopa zwrotu oraz najwyższa skośność.

Analiza obejmuje:

1. Dane wejściowe – średnie stopy zwrotu i ryzyko spółek z projektu EZP
2. Macierz korelacji między spółkami
3. Losową symulację portfeli (Monte Carlo)
4. Wyznaczenie granicy efektywnej
5. Portfel minimalnego ryzyka i portfel optymalny (maksymalna wartość Sharpe'a)
6. Tabelę podsumowującą kluczowe portfele
7. Wnioski

---

## Część 1 – Dane wejściowe

Tygodniowe logarytmiczne stopy zwrotu i odchylenia standardowe na podstawie danych historycznych 2024–2026.

```r
# Nazwy spółek
spolki <- c("CD Projekt", "Archicom", "CCC", "Inter Cars", "Makarony Polskie")

# Średnie tygodniowe stopy zwrotu (z raportu EZP)
# CCC: 0.834%, CD Projekt: 0.62%, Archicom: 0.37%,
# Makarony Polskie: 0.30%, Inter Cars: 0.0716%
srednie <- c(0.0062, 0.0037, 0.00834, 0.000716, 0.0030)

# Odchylenia standardowe (ryzyko tygodniowe)
odch <- c(0.0240, 0.0210, 0.0380, 0.0180, 0.0220)

# Liczba spółek
n <- length(spolki)

# Stopa wolna od ryzyka (uproszczona = 0)
rf <- 0.0

cat("DANE WEJŚCIOWE – 5 SPÓŁEK\n")

for (i in 1:n) {
  cat(sprintf("%-22s  średnia: %.4f  |  ryzyko: %.4f\n",
              spolki[i], srednie[i], odch[i]))
}
```

---

## Część 2 – Macierz korelacji

Niskie korelacje są ważne przy dywersyfikacji portfela – oznaczają, że kursy spółek nie poruszają się razem, co pozwala ograniczyć ryzyko.

```r
# Macierz korelacji z raportu EZP (5x5)
# Kolejność: CD Projekt, Archicom, CCC, Inter Cars, Makarony Polskie
korelacje <- matrix(c(
  1.00,  0.25,  0.18,  0.12,  0.05,
  0.25,  1.00,  0.20,  0.15,  0.08,
  0.18,  0.20,  1.00,  0.22,  0.07,
  0.12,  0.15,  0.22,  1.00,  0.10,
  0.05,  0.08,  0.07,  0.10,  1.00
), nrow = n, ncol = n)

rownames(korelacje) <- spolki
colnames(korelacje) <- spolki

cat("\n\n")
cat("MACIERZ KORELACJI\n")
print(round(korelacje, 2))
cat("\nNiskie współczynniki korelacji między spółkami sprzyjają\n")
cat("dywersyfikacji i ograniczeniu ryzyka portfela.\n")
cat("\n\n")

# Macierz kowariancji (z korelacji i odchyleń standardowych)
kowariancje <- matrix(0, nrow = n, ncol = n)
for (i in 1:n) {
  for (j in 1:n) {
    kowariancje[i, j] <- korelacje[i, j] * odch[i] * odch[j]
  }
}
cat("\n\n")
```

---

## Część 3 – Symulacja Monte Carlo

Losujemy 5000 portfeli o losowych wagach i obliczamy ich ryzyko, stopę zwrotu i wskaźnik Sharpe'a.

```r
set.seed(42)
N_sim <- 5000

sim_return <- numeric(N_sim)
sim_risk   <- numeric(N_sim)
sim_sharpe <- numeric(N_sim)
sim_wagi   <- matrix(0, nrow = N_sim, ncol = n)

for (s in 1:N_sim) {

  # Losowe wagi sumujące się do 1
  w <- runif(n)
  w <- w / sum(w)

  # Stopa zwrotu portfela
  ret <- sum(w * srednie)

  # Ryzyko portfela (wzór macierzowy)
  risk <- sqrt(t(w) %*% kowariancje %*% w)

  sim_return[s] <- ret
  sim_risk[s]   <- as.numeric(risk)
  sim_sharpe[s] <- (ret - rf) / as.numeric(risk)
  sim_wagi[s, ] <- w
}

cat(sprintf("SYMULACJA MONTE CARLO – wylosowano %d portfeli\n", N_sim))
cat(sprintf("Zakres ryzyka:        %.4f – %.4f\n", min(sim_risk), max(sim_risk)))
cat(sprintf("Zakres stopy zwrotu:  %.4f – %.4f\n", min(sim_return), max(sim_return)))
cat("\n\n")
```

---

## Część 4 – Portfele kluczowe

```r
# Portfel o minimalnym ryzyku
min_idx  <- which.min(sim_risk)

# Portfel optymalny – maksymalny wskaźnik Sharpe'a
best_idx <- which.max(sim_sharpe)

cat("PORTFEL O MINIMALNYM RYZYKU\n")
cat(sprintf("Stopa zwrotu : %.4f\n", sim_return[min_idx]))
cat(sprintf("Ryzyko       : %.4f\n", sim_risk[min_idx]))
cat(sprintf("Sharpe       : %.4f\n", sim_sharpe[min_idx]))
cat("Wagi spółek:\n")
for (i in 1:n) {
  cat(sprintf("  %-22s %.1f%%\n", spolki[i], sim_wagi[min_idx, i] * 100))
}
cat("\n\n")

cat("PORTFEL OPTYMALNY (maksymalny wskaźnik Sharpe'a)\n")
cat(sprintf("Stopa zwrotu : %.4f\n", sim_return[best_idx]))
cat(sprintf("Ryzyko       : %.4f\n", sim_risk[best_idx]))
cat(sprintf("Sharpe       : %.4f\n", sim_sharpe[best_idx]))
cat("Wagi spółek:\n")
for (i in 1:n) {
  cat(sprintf("  %-22s %.1f%%\n", spolki[i], sim_wagi[best_idx, i] * 100))
}
cat("\n\n")
```

---

## Część 5 – Portfele jednoskładnikowe

```r
cat("PORTFELE JEDNOSKŁADNIKOWE (100% w jednej spółce)\n")

for (i in 1:n) {
  cat(sprintf("%-22s  zwrot: %.4f  ryzyko: %.4f  Sharpe: %.4f\n",
              spolki[i], srednie[i], odch[i], (srednie[i] - rf) / odch[i]))
}
cat("\n\n")
```

---

## Część 6 – Tabela podsumowująca

```r
cat("TABELA PODSUMOWUJĄCA KLUCZOWE PORTFELE\n")

tabela <- data.frame(
  Portfel = c("Min. ryzyko", "Optymalny (max Sharpe)"),
  Stopa_zwrotu = round(c(sim_return[min_idx], sim_return[best_idx]), 4),
  Ryzyko       = round(c(sim_risk[min_idx],   sim_risk[best_idx]),   4),
  Sharpe       = round(c(sim_sharpe[min_idx], sim_sharpe[best_idx]), 4)
)

print(tabela, row.names = FALSE)
cat("\n\n")
```

---

## Część 7 – Wnioski

```r
cat("WNIOSKI\n")

cat("1. Dywersyfikacja działa – niskie korelacje między spółkami\n")
cat("   z różnych sektorów pozwalają zredukować ryzyko portfela\n")
cat("   poniżej ryzyka każdej ze spółek osobno.\n\n")
cat("2. CCC S.A. ma najwyższą średnią stopę zwrotu (0,834%),\n")
cat("   ale też najwyższe ryzyko. Inter Cars ma najniższy zwrot\n")
cat("   (0,0716%), lecz jedno z najniższych odchyleń.\n\n")
cat("3. Portfel optymalny (max Sharpe) oferuje najlepszy stosunek\n")
cat("   zysku do ryzyka – to portfel dla racjonalnego inwestora\n")
cat("   o umiarkowanej awersji do ryzyka.\n\n")
cat("4. Portfel minimalnego ryzyka minimalizuje zmienność, ale\n")
cat("   kosztem niższej stopy zwrotu.\n\n")
cat("5. Model Markowitza potwierdza, że łączenie aktywów\n")
cat("   o słabych wzajemnych korelacjach prowadzi do\n")
cat("   korzystniejszego profilu ryzyko–zwrot.\n")
cat("\n\n")
```

---

## Część 8 – Wykres: granica efektywna

```r
# Kolory punktów według wskaźnika Sharpe'a (niebieski → czerwony)
kolor_sharpe <- colorRampPalette(c("steelblue", "gold", "red"))(100)
indeks_kolor <- cut(sim_sharpe,
                    breaks = 100,
                    labels = FALSE,
                    include.lowest = TRUE)

# Wyznaczamy górną linię – granicę efektywną
# Dla każdego przedziału ryzyka bierzemy portfel z najwyższym zwrotem
progi <- seq(min(sim_risk), max(sim_risk), length.out = 61)
gr_risk   <- numeric(60)
gr_return <- numeric(60)

for (k in 1:60) {
  idx <- which(sim_risk >= progi[k] & sim_risk < progi[k + 1])
  if (length(idx) > 0) {
    gr_risk[k]   <- mean(sim_risk[idx])
    gr_return[k] <- max(sim_return[idx])
  } else {
    gr_risk[k]   <- NA
    gr_return[k] <- NA
  }
}

# Usuwamy puste przedziały i sortujemy
niepuste  <- !is.na(gr_risk)
gr_risk   <- gr_risk[niepuste]
gr_return <- gr_return[niepuste]
porzadek  <- order(gr_risk)
gr_risk   <- gr_risk[porzadek]
gr_return <- gr_return[porzadek]

# Wykres
plot(
  sim_risk,
  sim_return,
  col  = kolor_sharpe[indeks_kolor],
  pch  = 16,
  cex  = 0.5,
  xlab = "Ryzyko (odch. stand.)",
  ylab = "Tygodniowa stopa zwrotu",
  main = "Granica efektywna dla portfela 5 spółek (Monte Carlo)"
)

# Linia granicy efektywnej
lines(
  gr_risk,
  gr_return,
  col  = "steelblue",
  lwd  = 2.5,
  type = "b",
  pch  = 16,
  cex  = 0.8
)

# Portfel minimalnego ryzyka – zielony
points(sim_risk[min_idx], sim_return[min_idx],
       col = "darkgreen", pch = 19, cex = 2.5)
text(sim_risk[min_idx], sim_return[min_idx],
     labels = "Min. ryzyko", pos = 4, cex = 0.85, col = "darkgreen")

# Portfel optymalny – czerwony
points(sim_risk[best_idx], sim_return[best_idx],
       col = "red", pch = 19, cex = 2.5)
text(sim_risk[best_idx], sim_return[best_idx],
     labels = "Optymalny", pos = 4, cex = 0.85, col = "red")

# Legenda
legend(
  "bottomright",
  legend = c("Portfele (kolor = Sharpe)", "Granica efektywna",
             "Min. ryzyko", "Optymalny (max Sharpe)"),
  col    = c("steelblue", "steelblue", "darkgreen", "red"),
  pch    = c(16, 16, 19, 19),
  lty    = c(NA, 1, NA, NA),
  lwd    = c(NA, 2.5, NA, NA),
  cex    = 0.85
)
```
