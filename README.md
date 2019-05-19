# CIS194 Solutions (Spring 2013)

My solutions to [CIS 194: Introduction to Haskell (Spring 2013) by Brent Yorgey][cis194].

[cis194]: https://www.cis.upenn.edu/~cis194/spring13/

Notes:

- Did this course using GHC 8.6.5.
- Due to changes in GHC 7 and 8, some changes to `Editor.hs` and `Sized.hs` in HW 07 were required to compile the code. Specifically, fixes for `Semigroup` being a superclass of `Monoid`, and `Monad` also having to be declared as `Applicative`.