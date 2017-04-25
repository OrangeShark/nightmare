Prototype game for the Lisp Game Jam 2017 (Easy Mode).

The game is currently unfinished. You can find the source code at
https://github.com/orangeshark/nightmare

This project uses the xelf game engine which can be found at http://xelf.me/
It also uses the map tileset called "Cave tiletset" by MrBeast which can be found https://opengameart.org/content/cave-tileset-0

To build and run the game, it requires quicklisp and xelf and xelf's dependencies.
With the project in your quicklisp projects directory, you only need to run

     (ql:quickload :nightmare)
     (nightmare:nightmare)

The controls uses the arrow keys and to restart the game you press the page down key.
