# Randa

Miranda is a lazy functional programming language in the ML family and an implementation of this language
in C written by David Turner, which were popular among PL theorists during the 80s and 90s. The software
was proprietary until January of 2020 when the author generously chose to release it as open source
(Simplified BSD; see [COPYING](COPYING)). The codebase is mostly of historical interest. More sophisticated open source
alternatives have replaced it. I am not aware of anyone using Miranda today. A couple of old classic texts
by Simon Peyton Jones use Miranda. It would be interesting to see those texts get another chance at life.

Randa is a [RIIR](https://deprogrammaticaipsum.com/the-great-rewriting-in-rust/) of Miranda. The goal is functional 
equivalence, aiming for binary compatibility. As I learn more about how Miranda works, I expect to deviate from 
binary compatibility, so while I _aim_ for binary compatibility, I expect to miss. 

Randa is a name starting with the letter R. Rust also starts with the letter R. Randa has obvious similarities to the 
name Miranda, its ancestor software. Finally, Randa is the name of a high school friend of mine who 
tragically and unexpectedly passed away far too young, and it makes me happy to think of her whenever I work on 
Randa the software. 


# Authorship

Robert Jacobson is the author of Randa. Nontrivial portions of this code may have been derived directly from the 
original Miranda code. Miranda was written by David Turner. 

# License

Randa is distributed under the terms of the 'Simplified" BSD License, which is the same license that Miranda is 
distributed under. See LICENSE.md for details.
