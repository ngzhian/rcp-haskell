Rock Paper Scissors
===================

A simple game of rock paper scissors written in Haskell.
The main point of writing this was an exercise, especially to practice the syntax, IO, data constructors.

More about this in a [blog post here](http://ngzhian.github.io/blog/posts/2014-05-01-rps-haskell.html)

![How the game looks like now](ngzhian.github.io/blog/images/rcp-haskell.png)

The commit history shows the steps I took to slowly 'complete' the game.
The first commit was a very simple single-game.
Slowly I added more features, like continuing the game and error handling.
Then I shifted focus to improving the quality of code, using appropriate types, and trying to write more idiomatic and cleaner Haskell.

## Running

``` bash
git clone https://github.com/ngzhian/rcp-haskell.git
cd rcp-haskell
runhaskell Rock-Paper-Scissors.hs
```
