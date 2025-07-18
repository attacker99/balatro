# Balatro Odds Simulation

Simulate the best hand type after selecting cards to discard and randomly drawing from the remaining deck.

## Card Input Format

**Standard cards:**
`[{rank: 2–9,T,J,Q,K,A}{suit: s,h,d,c}][enhancement][edition][seal][quantity]`

**Stone cards:**
`s[enhancement][edition][seal][quantity]`

### Modifiers

- **Enhancements:**
  `b` (bonus), `m` (mult), `w` (wild), `g` (glass), `s` (steel), `y` (gold), `l` (lucky)

- **Editions:**
  `f` (foil), `h` (holographic), `p` (polychrome)

- **Seals:**
  `y` (gold), `r` (red), `b` (blue), `p` (purple)

### Example
```
ac kd2 th1 s20 acgfr17 3sghp5 s20fy
```
is
```
1× Ace of clubs, 2× King of diamonds, 1× Ten of hearts, 20× Stone cards,
17× Ace of clubs (glass enhancement, foil edition, red seal),
5× Three of spades (glass enhancement, holographic and polychrome edition),
20× Stone cards (foil edition, gold seal)
```

## Try It Live

[balatro.fly.dev](https://balatro.fly.dev/)
