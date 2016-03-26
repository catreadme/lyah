# Examples from the Book "Learn You A Haskell For Great Good"
The book can be read online at [learnyouahaskell.com](http://learnyouahaskell.com/)

## Lessons learned
* learning how to think recursively is essential
* implementing `foldl` and `foldr` by yourself is the best way to really understand them
* too much function composition with `.` is not advisable
* `Functor` typeclass is fundamental for many other typeclasses
* `Applicative` is a beefed up `Functor`
* `Monad` is a beefed up `Applicative`
* learning about `kinds` is essential for really understanding types
* when in doubt, why not use a good old list comprehension?
* list comprehensions are a good fit for code challenges
* guards make the code extremely readable
* use generic versions of functions like `length` and `take`
* if you speak a different language than english, implement types like
`Maybe` or `Either` in that language, it's a great exercise (e.g. `data Vielleicht a = Nichts | Einfach a`, then make the accordings instances)
