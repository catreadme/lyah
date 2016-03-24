# Examples from the Book "Learn You A Haskell For Great Good"
The book can be read online at [learnyouahaskell.com](http://learnyouahaskell.com/)

## Lessons learned
* learning how to think recursively is essential
* implementing the Prelude functions `foldl` and `foldr` by yourself is the best way to really understand them
* too much function composition with `.` is not advisable
* The `Functor` typeclass is the sort of a foundation for many other typeclasses
* `Monad` is just another typeclass
* Learning about kinds is the best way to really understand types
* when in doubt, why not using a good old list comprehension?
* list comprehensions are often a good fit for code challanges
* guards make the code extremely readable
* use the generic versions of functions such as `length` or `take`
* if you speak a different language than english, implement types like
`Maybe` or `Either` in that language, it's a great exercise (e.g. `data Vielleicht a = Nichts | Einfach a`, then make the accordings instances)
