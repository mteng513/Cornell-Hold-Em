# > CornellHold'Em Instructions

Welcome to our CornellHold'Em Game! To run the game, you need to install lablgtk
and all of it's dependencies. Typically this means gtk+. You can do so by running
"brew install gtk+." This took us multiple tries and much debugging, the install
process for lablgtk is a pain. We hope you have better luck with it. From there,
you can use opam to install lablgtk. The only other dependency we have is OUnit.

To compile our code, simply run "make" in the directory. To run the game, run
"make play." To run our OUnit test suite, run "make test."

Once you run the game, you'll need to click start, then select the difficulty 
(only medium is supported, clicking the other buttons will instantiate medium).
Other difficulties can be implemented relatively easily. You then will need to
select the number of CPU's to play against. The game will then start!

