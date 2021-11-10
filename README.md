My notes, exercises, and code following along with Christopher Allen's and Julie Moronuki's book "Haskell Programming From First Principles": www.haskellbook.com

### Workflow

The standard way I've been going through the chapters in this book is to have 2 terminal windows
open, both within the chapter's directory.

One terminal is my interactive repl for trying things out, usually at the book's suggestion.
`stack ghci notes.hs` or `stack ghci exercises.hs` depending on which file I'm working on.

The other terminal is my automatic type-checker, which uses [ghcid](https://github.com/ndmitchell/ghcid).
`ghcid --command="stack ghci notes.hs"` or `ghcid --command="stack ghci notes.hs"`.