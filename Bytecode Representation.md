# Bytecode Representation

Things with X in their name are related to bytecode encoding.

`w` means `sizeof(word)` in bytes. For 64-bit, `w=8`.

| internal heap object        | external file rep - char sequence         |                         |
| --------------------------- | ----------------------------------------- | ----------------------- |
| 0..127                      | `self`                                    |                         |
| 128..383                    | `CHAR_X (self-128)`                       |                         |
| 384..`ATOMLIMIT`-1          | `(self-256)`                              |                         |
| integer (-127..127)         | `SHORT_X <byte>`                          |                         |
| integer                     | `INT_X <4n bytes> (-1)`                   |                         |
| double                      | `DBL_X <8 bytes>`                         |                         |
| unicode_char                | `UNICODE_X <4 bytes>`                     |                         |
| `typevar`                   | `TVAR_X <byte>`                           |                         |
| `ap(x, y)`                  | `[x] [y] AP_X`                            |                         |
| `cons(x, y)`                | `[y] [x] CONS_X`                          |                         |
| `id` (=occurrence)          | `ID_X <string terminated by '\0'>`        |                         |
| `pname` (=occurrence)       | `PN_X <2 bytes>`                          |                         |
|                             | `PN1_X <4 bytes>`                         |                         |
| `datapair(string, 0)`       | `AKA_X <string...\0>`                     |                         |
| `fileinfo(script, line_no)`&nbsp;&nbsp; | `HERE_X <string...\0> <2 bytes>`     (**) |                         |
| `constructor(n, x)`         | `[x] CONSTRUCT_X <2 bytes>`               |                         |
| `readvals(h, t)`            | `[t] RV_X`                                |                         |
| definition                  | `[val] [type] [who] [id] DEF_X`           |                         |
|                             | `[val] [pname] DEF_X`                     |                         |
| definition-list             | `[definition*] DEF_X`                     |                         |
| filename                    | `<string terminated by '\0'>`             |                         |
| `mtime`                     | `<w bytes>`                               |                         |
| complete script             | `__WORDSIZE`                              |                         |
|                             | `XVERSION`                                |                         |
|                             | `[ [filename]`                            |                         |
|                             | &nbsp;&nbsp;`[mtime]`                     |                         |
|                             | &nbsp;&nbsp;`[shareable]`                 | (=0 or 1)               |
|                             | &nbsp;&nbsp;`[definition-list] ]+`        |                         |
|                             | `'\0'`                                    |                         |
|                             | `[definition-list]`                       | (`algshfns`)              |
|                             | `[ND] or [True]`                          | (see below)             |
|                             | `DEF_X`                                   |                         |
|                             | `[freeids]`                               |                         |
|                             | `DEF_X`                                   |                         |
|                             | `[definition-list]`                       | (internals)             |
| type-error script           | `__WORDSIZE`                              |                         |
|                             | `XVERSION`                                |                         |
|                             | `'\1'`                                    |                         |
|                             | `<w bytes>`                               | (=`errline`)              |
|                             | `…`                                       | (rest as normal script) |
| syntax-error script         | `__WORDSIZE`                              |                         |
|                             | `XVERSION`                                |                         |
|                             | `'\0'`                                    |                         |
|                             | `<w bytes>`                               | (=`errline`)              |
|                             | `[ [filename]`                            |                         |
|                             | &nbsp;&nbsp;`[mtime] ]+`                  |                         |


>  Notes
>    -----
> First filename in dump must be that of `current_script` (ie the
>    main source file).  All path names in dump are correct wrt the
>    directory of the main source.
>
>  (**) empty string is abbreviation for current filename in `hereinfo`.
>    `True` in `ND` position indicates an otherwise correct dump whose exports
>    include type orphans.
