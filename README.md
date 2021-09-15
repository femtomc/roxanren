roxanren
========

An experimental fusion of [miniKanren](http://minikanren.org/) and [Rosette](https://emina.github.io/rosette/).

---

One important goal of this project is understanding the relationship between SMT-based synthesis/sketch filling (e.g. [angelic execution](https://docs.racket-lang.org/rosette-guide/ch_essentials.html#%28part._sec~3asolve%29)) and integration of SMT solving into \*kanren languages.

---

Planned extensions:

- [ ] The aforementioned inclusion of `Rosette`-solver aided constructs.
- [ ] Improve `walk` by using a persistent hash map data structure.
- [ ] Add a naive version of `evalo`.
