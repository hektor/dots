" Override the pandoc-syntax highlighting colors for more subtle citations
hi! link pandocPCite Comment
hi! link pandocICite Comment
hi! link pandocCiteKey Comment
hi! link pandocCiteAnchor Comment
hi! link pandocCiteLocator Comment

" Match cloze delimiters e.g. `{{c1::` and `}}`
syn match ClozeDelimiter /{{c\d\+::/ conceal containedin=pandocUListItem,pandocListItem
syn match ClozeDelimiter /}}/ conceal containedin=pandocUListItem,pandocListItem
" Match text between cloze delimiters
syn match Cloze /\({{c\d\+::\)\@<=\(\_[A-Za-z0-9$\ \\\-\*,_]*\)\(}}\)\@=/ containedin=pandocUListItem,pandocListItem

hi! link Cloze Comment
