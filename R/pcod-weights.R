Sj1s1 <- 100
Sj2s1 <- 100

Sj1s2 <- 100
Sj2s2 <- 100

Wj1s1 <- 10
Wj2s1 <- 12

Wj1s2 <- 30
Wj2s2 <- 32

Cs1 <- 100
Cs2 <- 150

# Eq. 3
(Ws1 <- sum(Wj1s1 * Sj1s1, Wj2s1 * Sj2s1) / sum(Sj1s1, Sj2s1))
(Ws2 <- sum(Wj1s2 * Sj1s2, Wj2s2 * Sj2s2) / sum(Sj1s2, Sj2s2))

# Eq. 4
(W <- sum(Ws1 * Cs1, Ws2 * Cs2) / sum(Cs1, Cs2))