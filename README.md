# Relational Arithmetics

__Reference:__ _Pure, declarative, and constructive arithmetic relations (Declarative pearl)_ Oleg Kiselyov William E. Byrd Daniel P. Friedman Chung-chieh Shan, FLOPS, 14 April 2008

The challenge of writing pure relational arithmetic programs is to make them well behaved: terminate as much as possible no matter there is an answer or not. All the evolutions of a relational arithmetic program are driven by this end, for which the following techniques can be applied:

- swap goals in the clause body 
- swap arguments of a predicate
- use more involved case analysis

This repo hosts the artifects that I am creating while following the above reference paper. I have local installations of [Racket](https://racket-lang.org/) and [SWI-Prolog](https://www.swi-prolog.org/), so that I can try encoding and querying about arithmetic relations in both microKanren and Prolog. I found it easier to explore the relations, particularly to trace them, in Prolog, compared with reasoning about them in microKanren. This is because in Prolog I tend to apply the declarative and operational semantics of Prolog whilst in microKanren I tend to apply the call-by-value semantics of Scheme.

For the end of understanding relational arithmetics, I would suggest using Prolog, so that you would not be distracted by the extra low-level functional details introduced by microKanren, and you can focus on observing the proof search of the logic programs. Indeed, it requires a lot of observation to understand why a program non-terminates and how possibly this can be fixed.

For the end of understanding how relational programming is performed in Scheme, I would try to translate familiar Prolog programs into microKanren and query about them in DrRacket. It is surprising to see that microKanren emulates Prolog very well: it always gives as many answers as Prolog would give (or more due to interleaving search), and non-terminates whenever Prolog non-terminates. It is interesting to see that non-terminating proof search in Prolog is represented in microKanren by non-productive lazy streams: nothing could be pulled out of the stream so that the Scheme function of taking elements from such a stream does not terminate.  

## Content 

The two folders [microKanren](microKanren) and [microKanren-debug](microKanren-debug) are installed via the package manager of my DrRacket. They differ only in that the _debug_ version traces the `take-inf` function, making it manefest that a properly written microKanren program (i.e. all recursive calls in it are [inverse-eta-delay](https://github.com/YueLiPicasso/microKanren)ed) non-terminates only because `take-inf` does not terminate. 

The [unary-nat](unary-nat) folder contains a Racket source and a Prolog source, both implementing the same _decidable_ addition and multiplication relations for unary natural numbers. The Prolog source also records how undecidable relations are evolved into the decidable form using the three techniques mentioned above. I discovered an _alternative_ multiplication relation that behaves in the same way as the one given by the reference paper. Swapping the arguments of one more predicate does the trick without the need of one more clause. This is inspird by the implementation of interleaving search as detailed in the [microKanren reference paper](https://github.com/YueLiPicasso/microKanren). It is this alternative multiplication that is encoded in the Racket source.   
