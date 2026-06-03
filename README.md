# R UTILS
## Purpose
This repository aims at putting together the awesome utility functions that I use everyday.

## How to use
* Clone this repository.
* In a R session within this directory, type :
```
if (!require(devtools)) {
	install.packages("devtools")
	library(devtools)
}
install_github("Vongo/rutils")
```
There you go!

## TODO
* Adding even more awesome utils

## A few examples
Note: all calls of pseudo-random functions will be implicitly preceded by `set.seed(123)`.

* __Factorization__:
```
> saf(round(rnorm(1000)))
	-3  -2  -1   0   1   2   3
	 5  61 229 402 229  68   6
```
* __Relative factorization__:
```
> safr(round(rnorm(1000)), p=2)
	  -3      -2      -1       0       1       2       3
	"0.5%"  "6.1%" "22.9%" "40.2%" "22.9%"  "6.8%"  "0.6%"
```
* __Trim useless spaces__:
```
> trim(" lorem ipsum	")
[1] "lorem ipsum"
```
* __NOT IN__:
```
> "a" %ni% letters[1:10]
[1] FALSE
```
* __Month difference between two dates__:
```
> mondf("2018-01-01", "2019-05-31")
[1] 16
```
* __Naive obfuscation__ (a substitution cipher — obfuscation, not encryption):
```
> a <- cry("Lorem ipsum sit amet!", key=1337)
> print(a)
[1] "o7ktTPrdbDTPbrAPFTtAp"
> decry(a, key=42)
[1] "NoA02VEr.m2V.EdVT20d/"
> decry(a, key=1337)
[1] "Lorem ipsum sit amet!"
```
* `ws()` allows to print on the whole terminal width (Linux only).
* __Confusion table__ (relative, via `tabler`):
```
> tabler(sample(c(TRUE, FALSE), 100, rep=TRUE), c(rep(TRUE, 50), rep(FALSE, 50)))
       c
r       FALSE  TRUE
  FALSE 53.49 46.51
  TRUE  47.37 52.63
```
which is a convenient relative equivalent of
```
> table(sample(c(TRUE, FALSE), 100, rep=TRUE), c(rep(TRUE, 50), rep(FALSE, 50)))

        FALSE TRUE
  FALSE    23   20
  TRUE     27   30
```
* __round_clever__:
To make more human readable rounds
```
> sapply(c(1.01, 12008, 2018, 0.00999), round_clever)
[1] 1.0e+00 1.2e+04 2.0e+03 1.0e-02
```
* __bucket__:
Match values to a distribution-based bucket.
```
> saf(bucket(rnorm(100, 100, 25), 10))
 1  2  3  4  5  6  7  8  9 10
10 10 10 10 10 10 10 10 10 10
```
* __bucket2__:
Match values to a user-value-based bucket.
```
> saf(bucket2(rnorm(100, 100, 25), c(50, 75, 100, 125, 150)))
 1  2  3  4  5  6
 1 13 34 35 14  3
```

Some other graphical utils are not shown here, such as `pie2` (a pretty pie chart with ordered categories, and percentage displayed aside count), `lines2` (multiple line plots from a matrix), or a terminal progress bar that lets you know how much time is remaining or when you should come back from your coffee break.
