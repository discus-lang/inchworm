## inchworm-1.1.1:

 * Matching combinators now produce the range of source locations that matched,
   instead of just the starting location.

 * Line and column offsets are now 0-based instead of 1-based,
   for easier inteface with client editors that expect this (eg VSCode).

 * Thanks to Amos Robinson: Haskell string parser now correctly handles strings
   gaps and the string escape character `'\&'`
