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
syn match Cloze /\({{c\d\+::\)\@<=\(\_[A-Za-z0-9$\ \\\-\*,_`()]*\)\(}}\)\@=/ containedin=pandocUListItem,pandocListItem

" Fix task UUIDs not being highlighted correctly in pandoc lists
syn match pandocUListItem /^>\=\s*[*+-]\s\+-\@!.*$/ nextgroup=pandocUListItem,pandocLaTeXMathBlock,pandocLaTeXInlineMath,pandocEscapedDollar,pandocDelimitedCodeBlock,pandocListItemContinuation contains=@Spell,pandocEmphasis,pandocStrong,pandocNoFormatted,pandocStrikeout,pandocSubscript,pandocSuperscript,pandocStrongEmphasis,pandocStrongEmphasis,pandocPCite,pandocICite,pandocCiteKey,pandocReferenceLabel,pandocLaTeXCommand,pandocLaTeXMathBlock,pandocLaTeXInlineMath,pandocEscapedDollar,pandocReferenceURL,pandocAutomaticLink,pandocFootnoteDef,pandocFootnoteBlock,pandocFootnoteID,pandocAmpersandEscape,TaskWikiTaskUuid skipempty display
syn match TaskWikiTaskUuid containedin=TaskWikiTask /\v#([A-Z]:)?[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$/
syn match TaskWikiTaskUuid containedin=TaskWikiTask /\v#([A-Z]:)?[0-9a-fA-F]{8}$/
highlight link TaskWikiTaskUuid Comment
