# > CornellHold'Em Instructions

NOTE: You will need to download the 2015 3110 Course VM. Sorry, Ocaml
Graphics doesn't work at all on Macs, and it doesn't work on the 2016
VM either (not easily at least).
http://www.cs.cornell.edu/courses/cs3110/2015fa/vm.html

Welcome to our CornellHold'Em Game! To run the game, you need to install lablgtk
and all of it's dependencies. On the 2015 VM, lablgtk should already be installed,
but if not running opam install lablgtk should do it.

You will also need to install several dependencies for ocamlimages.
Run: sudo apt-get install libpng12-dev libjpeg-dev libtiff-dev libxpm-dev libfreetype6-dev libgif-dev
opam install camlimages

To compile our code, simply run "make" in the directory. To run the game, run
"make play." To run our OUnit test suite, run "make test."

Once you run the game, you'll need to click start, then select the difficulty 
(only medium is supported, clicking the other buttons will instantiate medium).
Other difficulties can be implemented relatively easily. You then will need to
select the number of CPU's to play against. The game will then start!

