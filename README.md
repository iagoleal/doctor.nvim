# doctor.nvim

Have you ever been alone at home, coding in the middle of night,
and felt a sudden burst of existential angst?
Did you even felt the urge to open __that other editor__ just
to feel that the machine could understand and nurture you?

Well, my friend, your problems are over!
Now, with a simple ex-command, you can not only share your thoughts
with your favorite text editor but also get a proper response from it,
finely produced by the best of 1966's artificial intelligence!

## But what is this, really?

This is a implementation of Joseph Weizenbaum's [ELIZA chatbot][eliza-wikipedia]
embedded in a neovim prompt buffer.
It works just like Emacs `M-x doctor`: you fire up a command
and a chat buffer opens up.

A ELIZA bot uses a script that is separate from the implementation
to formulate its responses.
In the case of this plugin, we are using the original script
found on Weizenbaum's paper from 1966
that simulates a Rogerian psychotherapist.
(Pull Requests for new scripts are always welcome by the way!)


## Installation

The easiest way to install this plugin is using a plugin manager.
For example, with [packer.nvim][packer-url] you can run:

```lua
use "iagoleal/doctor.nvim"
```

## Usage

Run the command below to open a `doctor` buffer.

```vim
:TalkToTheDoctor
```

This is a vim prompt buffer and works like a REPL.
You can enter anything on the buffer's last line
and when you press `Return` in Insert Mode,
the Bot will reply to you on the following line.

## References

* The original paper and script: Joseph Weizenbaum. 1966. ELIZA — A Computer Program for the Study of Natural Language Communication Between Man and Machine. Commun. ACM 9, 1 (Jan. 1966), 36–45. DOI: https://doi.org/10.1145/365153.365168
* This is a port of a [previous ELIZA implementation](https://github.com/iagoleal/eliza) I did some time ago.
* The [Emacs `M-x doctor`](https://www.emacswiki.org/emacs/EmacsDoctor). (Not for the code but certainly for the inspiration)

## Important Notice

This is just a game and in no way should the taken as a substitute for a real therapist.
If you are struggling with anything related to mental health,
the right thing to do is to go look for a capable professional.


[packer-url]: https://github.com/wbthomason/packer.nvim
[eliza-wikipedia]: https://en.wikipedia.org/wiki/ELIZA
