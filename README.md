# Type level web server

Code for the talk ["Programs at the type level"](https://www.youtube.com/watch?v=vOTwT5UThxQ), presented at Monadic Warsaw #10.

A web server implemented at the type level in Haskell.

To run it:

```
cabal new-build
./run Httperver.hs
```

Then go to <http://localhost:3000/README.md> to confirm that it works.
