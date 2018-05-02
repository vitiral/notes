Introduction:
- fix things _before_ they break. Keep everything tidy. Put code where it belongs. Name things correctly.
- Refactor mercilessly
- Build machines that are more maintainable in the first place.

# Quotes:
- We should view our code as the beautiful articulation of noble efforts of
  design—design as a process, not a static endpoint. It’s in the code that the
  architectural metrics of coupling and cohesion play out
- Neither architecture nor clean code insist on perfection, only on honesty and
  doing the best we can.  To err is human; to forgive, divine.
- Mature programmers know that the idea that everything is an object is a myth.
  Some-times you really do want simple data structures with procedures
  operating on them.
- Never forget to use the _simplest thing that can possibly work_.


# Chapter 1: Clean code
Terms
- _wading_: to "wade through bade code" is to be impeded by it's slog.
- LeBlanc's law: later equals never.

Tidbits:
- Code is really the language in which we ultimately express the requirements.
- Clean code does _one thing well_
- I _need_ to get **abstract method** and **rename variable/function/etc** for
  rust + python. I've felt it for a long time and it is becomming more and more
  clear.
- Reduced duplication, high expressiveness, and early building of simple
  abstractions.  That’s what makes clean code for me

## Grand redesign:
The grand redesign in the sky:
- Mess continues to grow until it is impossible to be productive in a code base.
  Every change causes major breakage.
- Management eventually caves to a rewrite -- but they want it to happen _in parllel_.
- Oh my god, this is my life.

It is up to the _programmer_ to write good code. Almost never will you be fired for
fighting back (at least as a group, I assume).

## Summary

Clean code is simple. It is enjoyable to read. It can be read easily and obviously.
Clean code must be maintained, you must continuously improve it.
Clean code has tests. It is well designed.

Refactor quickly when something is wrong.


# Chapter 2:

# Tidbits
- duplicaiton may be the root of all evil in software.
- The art of programming is and has always been the art of language design. Think of systems
  as stories to be told rather than programs to be written.

# General notes

Use intention revaling names.
- Should answer all major questions: why it exists, what it does and how to use it.
- If a name requires a comment then it does not tell you what it does (really?)
- Avoid disinformation
- Noise words are redundant. Says not to use "table" (i.e. map) in the variable name.
- Make names pronouncable
- the length of a name should corespond to the size of it's scope.
- methods should have verb or verb phrases like postPayment

Functions
- Functions should do one thing and do _only_ that one thing.
- Funcitons should only have one level of abstraction.
    - Low level things should be grouped together. Appending `"\n"` is confusing
      in a high level function.
- Reduce # of arguments to 2 or less. 3 when necessary. 0 is the best (??? not including
  `this` I assume, curious to see their thoughts on object state and `this`...)
  but 1 is really good.
- monads (single argument functions): asking a question or transforming the argument
  or issuing an event (how is that simple?)
  - don't use flags (booleans)
- dyadic functions: try to convert them to monads when possible.
    - "Or you might make the outputStream a member variable of the current class
      so that you don’t have to pass it." -- aaaaagh, my eyes are bleeding!!!
      Seriously? The solution to "reducing arguments" is to _increase the complexity
      and state management of your class?_. Yuuuck.
- larger number of arugments:
    - reduce the number of arguments by grouping them via classes (duh).
- "For  example, assertEquals might  be  better  written  as
  `assertExpectedEqualsActual(expected, actual)`". Double yuk.
- Functions should either do something or answer something, but not both.
- Error handling is one thing. A function that handles errors should do
  nothing else.

> Thoughts 2018-04-29
>
> So far I think a lot of these thoughts are very useful but I think the authors
> are overly in love with OO design. In parituclar their treatment of "functions" as
> being class members that clearly change state is... disturbing.
>
> _any_ state that is not funcamentally clear by the object in question is
> the opposite of "clean" IMO.


# Chapter 3+4: Comments + formatting

The proper use of comments is to compensate for our failure to express ourself
in code.

Really there wasn't much in these two chapters that I didn't know. Use comments
to describe things that can't be described in code. Format consistently
and with clear meaning. Use a tool to format.

# Chapter 6: Objects and data structures

Differences:
- Objects hide their data behind abstractions and expose functions to operate on that data.
- Data structures expose their data and have no meaningful functions.

Another point:
- Procedural code (code using data structures) makes it easy to add new functions without
  changing the existint data structures.
- OO code makes it easy to add new classes without changing existing functions

Also:
- Procedural code makes it hard to add new data structures because all functions must change.
- OO code makes it hard to add new functions because all the classes must change.

# Error handling:
- start your function with `try` to state what the scope and expected errors are.
- He doesn't like checked exceptions because "it could require your entire stack to handle them"...
    - ... Shouldn't they _be handled_ at the layer where they _should_ be handled?

# Ch9: Tests
The three laws of TDD:
1. You may not write production code until you've written a failing unit test.
2. You may not write more of a unit test than is sufficient to fail, and not
  compiling is failing.
3. You may not write more production code than is sufficient to pass the currently
  failing test.

Test code is _just as important as production code_ and must be kept clean.
- Tests enable change, they allow you to make change/refactorings to your codebase without fear.
- Build up a domain-specific language for the tests.
- Feel free to be non-performant/etc in tests. It is a test environment. Do almost anything in
  the name of descriptiveness.
- The number of asserts in a test ought to be minimized.
- Test a single _concept_ per test.

Follow FIRST:
- Fast: tests should be fast
- Independent: tests should be independent
- Repeatable: tests should be repeatable
- Self Validating: should have only a boolean output
- Timely: write them just _before_ writing production code.

# Ch10: Classes
Classes should have only a _single responsibility_ (SRP) and it should be
fairly easy to determine a succinct name.

Responsibility = reason to change.

- You want your system to be composed of many small classes, not a few large ones.
    - Think large toolboxes or well organized drawers
- Cohesion: each method should manipulate almost all instance variables

Classes should be open for extension but closed for modification, or the
Open-Closed Principle (OCP). Allow for new functionality via subclassing, but
keep every other class closed.

# Ch11: Systems
- Separate concerns: construction is very different from use.
- Lazy initialization is rarely good. `if self.state is None: self.state = foo`
- Build for dependency injection.
    - Use factories and classes initialized in main (or a test!) for all internal logic.
    - Use abstract interfaces so that "mocking" them is simply using a mocked interface.
- Most of this is almost _absurdly_ java specific. Using a framework that defines your
  initialization logic using obscure xml?

DIP: dependency inversion principle. Injectable classes.

I think the main takeaway is this:
- Frameworks should try and depend on simple classes/structs that the _user_ defines and
  not put almost any burden on these implementation (maybe a method/constructor).
  This allows the team to build and test their objects but work within the framework.
- A good API should largely disapear from view most of the time.
- An optimal system architecture consists of modularized domains of concern, each of which
  are implemented in plain old java objects. The different domains are integrated together
  with minimally invasive Aspects

# Ch12: Emergence

Kent Beck's 4 rules of emergent design. A design is "simple" if it follows these rules
in order of importance:
- Runs all tests.
- Contains no duplication.
- Expresses the intent of the programmer.
- Minimizes the number of classes and methods.

This pretty much sums up the chapter (which is the author's intent). I think the most
important takeaway is that expressing your intent requires your effort.

# Ch13: Concurrency
Concurrency is _hard_ and does not always reduce runtime.

Principles:
- Single Responsibility: concurrent code is already a single responsibility. It
  should be decoupled from other buisness logic.
- Limit access to any data that may be shared.
- Use copies
- Threads should be as independent as possible.
- Avoid shared data and be careful when using it. Keep locked scopes small.

Some strategies for testing:
- run with more threads than processors
- instrument your code to introduce "jiggle" (randomly sleep/yield/noop at any
  line), run this many times in random configurations.

# Ch14 - Ch16: Refinement of code examples
These sections were mostly code examples that the author walked through which
emphasized the ideas of the previous chapters. Although they were useful,
there weren't all that many "notes" that could be taken from them.

# Conclusion

Clean code is about taking the _effort to make your code enjoyable_. The last
statement was never mentioned in the book _exactly_ but it is my takeaway.
Making programming enjoyable and beautiful requires continuous effort and
refinement -- but that effort pays off in that you:
- Enjoy putting in the effort more.
- The results of your labor is something you can feel better about.
- Your code is more adapatable to change and breaks less often.

Several important points were missed, such as _almost any_ mention of tools
besides unit tests and IDEs (and a passing mention of formatters). I feel like
linters, property based testing and test frameworks should have deserved
at least a mention.

I found the book enjoyable, although the details were often too focused in
Java (you have to focus _somewhere_ though, so I get it). Highly recommend
the book, at least as a reference as a team is making their coding standards.

