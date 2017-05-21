# What is this?
This is a project to visualize my dialect of a brutally simple programming language called
[brainfuck](https://en.wikipedia.org/brainfuck/).

The idea is to allow the user to inspect the 1-D memory tape, infinite in both directions
(note, I diverge from the original author's specification of a fixed-size array),
to gain insight into how their programs are running.

As I write this, this project is quite far from finished.

# Why?
Partially to learn Elm and improve my general frontend knwoledge,
partially as a step towards learning to write translators for inherently visual programming languages,
partially due to nostalgia for the fun of a favorite old project of mine:
[yahbfs](https://github.com/nabilhassein/yahbfs), yet another haskell brainfuck system.

# TODOs
Not necessarily in this order. Some I will probably never do.

- visualize the memory tape using SVG and CSS or canvas or something rather than the ugly ASCII preview
- make this visualization interactive with animations
- allow stepping through the program one command at a time instead of all at once
- visualize the program as well as memory?
- post it online!
- add IO commands , and .
- write tests to ensure the core interpreter is correct
  - or maybe rewrite in Idris to prove it correct??? interop example: https://github.com/eeue56/advent-of-code-2016/blob/master/ElmInterop.idr
- figure out how to avoid infinite loops crashing user's tab/browser
- turn the visualizer into an interactive debugger allowing the user to modify the state of memory outside of normal program execution
- visualize other simple programming languages (Forth???)
