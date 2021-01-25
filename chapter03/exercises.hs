{-|
  Scope
  1. yes
  2. no
  3. no, because d isn't available for r
  4. yes

  do we have examples of FP based on LC but do not have strong types?
  dynamically typed FP?

  ian you've said that you don't really like haskell, or that it's
    "unfortunately" what you have to use. why is that?
    as i read this book i'd like to keep in mind where languages
    have improved.
    - comparing haskell to elm - compiler friendliness?

      like this could be friendlier:
      ```
      *Print3> concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9][

      <interactive>:39:56: error:
          parse error (possibly incorrect indentation or mismatched brackets)
      ```
    - basic list ops being unsafe?
      - is there a "standard" safe list library?

  stray unison thought: should library maintainers avoid `unique`
    types in public interfaces?

  i'm not yet fluent in "associativity"
    right associative vs left associative
  
  Chapter exercises
  1.
    a) already correct
    b) needs parentheses around infix operator ++ since it's in prefix pos
    c) already correct
    d) needs ending double quotes around `" world`
    e) arguments in wrong order
    f) already correct
    g) int needs to be an argument, not part of the string
    h) already correct
  2.
    code d <-> result a
    code e <-> result b
    code b <-> result c
    code a <-> result d
    code c <-> result e

  Building functions
  1.
    a) (++) "Curry is awesome" "!"
    b) "Curry is awesome!" !! 4
    c) drop 9 "Curry is awesome!"
  2.
    
-} 