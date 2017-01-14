Bine
----
Run BBC Micro ROMs on a modern computer.

Work in progress.

The purpose of Bine is to run BBC Micro ROMs without emulating BBC Micro hardware. The BBC Micro operating system had a well-defined API so instead of mapping OS calls to emulated BBC hardware they are mapped to OS calls on the host operating system. This means you have a BBC Micro with access to your native file system. For example you can run authentic BBC Basic in a modern environment.

Because it doesn't emulate the BBC Micro hardware, *B*ine *I*s *N*ot an *E*mulator, though it does contain a 6502 emulator.

You need to have cc65 in your path. On my Mac I installed it with

        brew install cc65

You need to have Stack installed because it's written in Haskell. There are [instructions](https://docs.haskellstack.org/en/stable/README/) for that.

Once you have Stack installed you can enter the root directory of the Bine project and run BASIC with

        sh scripts/run_bbc

You'll need to have a BBC Basic ROM stored in roms/BASIC.

![Bine screenshot](docs/beeb.png?raw=true "Bine Screenshot")

Goals
-----
I have a number of goals. One is to resurrect the first compiler I ever worked with. It's [here](http://www.bighole.nl/pub/mirror/homepage.ntlworld.com/kryten_droid/Acorn/Atom/pp/pp_spl_compiler.htm). Another goal is to have a look at some of the alternative languages that you could get for the Beeb.
Comal already runs. I think Lisp does. Having some issues with OS errors in the various Forths available.

I don't intend to do graphics, sound or anything like that.

You can, of course, compile and run 6502 code from BBC Basic.

Also
----

I like Bine by Autechre: https://www.youtube.com/watch?v=6ZRkOhu3KLY
