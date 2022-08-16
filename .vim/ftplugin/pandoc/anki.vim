" Anki helper functions

" Convert cloze note to regular text
"
" * `AnkiRemoveAllClozes` all cloze notes in file
" * `AnkiRemoveSelectedClozes` all clozes in selected range
"
" E.g.
"
" ```markdown
" START
" Cloze
" In this sentence {{c1::this word}} was closed.
" END
" ```
"
" ```markdown
" In this sentence this word was closed.
" ```

au FileType pandoc com! -range AnkiRemoveAllClozes :%s/{{c\d\+:://ge | :%s/}}//ge | :%s/START\nCloze\n//ge | :%s/END\n//ge
au FileType pandoc com! -range AnkiRemoveSelectedClozes :%s/\%V{{c\d\+:://ge | :%s/\%V}}//ge | :%s/\%VSTART\nCloze\n//ge | :%s/\%VEND\n//ge
