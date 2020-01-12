Maze related algorithms implemented in Emacs Lisp.

After requiring the ``maze.el`` file you should be able to insert nifty mazes, like this:

    M-x maze/insert-binary 
    Rows? 5
    Columns? 10
    
    ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
    ▒ ▒     ▒ ▒   ▒   ▒ ▒
    ▒ ▒▒▒▒▒ ▒ ▒▒▒ ▒▒▒ ▒ ▒
    ▒   ▒   ▒ ▒ ▒     ▒ ▒
    ▒▒▒ ▒▒▒ ▒ ▒ ▒▒▒▒▒ ▒ ▒
    ▒     ▒   ▒   ▒   ▒ ▒
    ▒▒▒▒▒ ▒▒▒ ▒▒▒ ▒▒▒ ▒ ▒
    ▒         ▒ ▒   ▒ ▒ ▒
    ▒▒▒▒▒▒▒▒▒ ▒ ▒▒▒ ▒ ▒ ▒
    ▒                   ▒
    ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒

or this:

    M-x maze/insert-wilson
    Rows? 5
    Columns? 10
    
    ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
    ▒ ▒ ▒   ▒         ▒ ▒
    ▒ ▒ ▒ ▒ ▒▒▒▒▒▒▒▒▒ ▒ ▒
    ▒ ▒ ▒ ▒ ▒   ▒       ▒
    ▒ ▒ ▒▒▒ ▒ ▒▒▒▒▒▒▒▒▒ ▒
    ▒ ▒     ▒ ▒ ▒       ▒
    ▒ ▒ ▒▒▒ ▒ ▒ ▒ ▒ ▒▒▒▒▒
    ▒   ▒ ▒ ▒ ▒   ▒     ▒
    ▒▒▒ ▒ ▒▒▒ ▒ ▒ ▒▒▒▒▒ ▒
    ▒           ▒     ▒ ▒
    ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
    
which btw look way better on a terminal than on the GitHub preview :D

Just don't forget to turn ``font-lock-mode`` on in the buffer.

## Credits

Part of the code is inspired by Jamis Buck's excellent "Mazes for Programmers": consider adding it to your bookshelf: https://pragprog.com/book/jbmaze/mazes-for-programmers

