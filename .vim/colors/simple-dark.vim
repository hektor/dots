highlight clear
set background=dark

if version > 580
    if exists("syntax_on")
        syntax reset
    endif
endif

let g:colors_name = "simple-dark"

if has("gui_running") || &t_Co == 256
	  hi NonText cterm=NONE ctermfg=black ctermbg=NONE gui=NONE guifg=bg guibg=#0c0c0c
    hi Normal cterm=NONE ctermfg=247 ctermbg=NONE gui=NONE guifg=#cccccc guibg=#0c0c0c
    hi Keyword cterm=NONE ctermfg=255 ctermbg=NONE gui=NONE guifg=#eeeeee guibg=#0c0c0c
    hi Constant cterm=NONE ctermfg=252 ctermbg=NONE gui=NONE guifg=#d0d0d0 guibg=#0c0c0c
    hi String cterm=NONE ctermfg=245 ctermbg=NONE gui=NONE guifg=#8a8a8a guibg=#0c0c0c
    hi Comment cterm=NONE ctermfg=240 ctermbg=NONE gui=NONE guifg=#585858 guibg=#0c0c0c
    hi Number cterm=NONE ctermfg=255  ctermbg=NONE gui=NONE guifg=#ff0000 guibg=#0c0c0c
    hi Error cterm=NONE ctermfg=255 ctermbg=DarkGray gui=NONE guifg=#eeeeee guibg=#0c0c0c
    hi ErrorMsg cterm=NONE ctermfg=255 ctermbg=DarkGray gui=NONE guifg=#eeeeee guibg=#0c0c0c
    hi Search cterm=NONE ctermfg=245 ctermbg=Gray gui=NONE guifg=#8a8a8a guibg=#0c0c0c
    hi IncSearch cterm=reverse ctermfg=255 ctermbg=245 gui=reverse guifg=#eeeeee guibg=#0c0c0c
    hi DiffChange cterm=NONE ctermfg=240 ctermbg=255 gui=NONE guifg=#8a8a8a guibg=#0c0c0c
    hi DiffText cterm=bold ctermfg=255 ctermbg=DarkGray gui=bold guifg=#bcbcbc guibg=#0c0c0c
    hi SignColumn cterm=NONE ctermfg=240 ctermbg=NONE gui=NONE guifg=#8a8a8a guibg=#0c0c0c
    hi SpellBad cterm=undercurl ctermfg=255 ctermbg=245 gui=undercurl guifg=#eeeeee guibg=#0c0c0c
    hi SpellCap cterm=NONE ctermfg=255 ctermbg=124 gui=NONE guifg=#eeeeee guibg=#0c0c0c
    hi SpellRare cterm=NONE ctermfg=240 ctermbg=black gui=NONE guifg=#8a8a8a guibg=#0c0c0c
    hi WildMenu cterm=NONE ctermfg=240 ctermbg=255 gui=NONE guifg=#585858 guibg=#0c0c0c
    hi Pmenu ctermfg=NONE ctermbg=235 cterm=NONE guifg=NONE guibg=#64666d gui=NONE
    hi PmenuSel ctermfg=NONE ctermbg=24 cterm=NONE guifg=NONE guibg=#204a87 gui=NONE
    hi SpecialKey cterm=NONE ctermfg=black ctermbg=255 gui=NONE guifg=#eeeeee guibg=#0c0c0c
    hi MatchParen cterm=NONE ctermfg=white ctermbg=NONE gui=NONE guifg=#eeeeee guibg=#0c0c0c
    hi CursorLine cterm=NONE ctermfg=NONE ctermbg=233 gui=NONE guifg=NONE guibg=#0c0c0c
    hi StatusLine cterm=bold,reverse ctermfg=240 ctermbg=NONE gui=bold,reverse guifg=#8a8a8a guibg=#0c0c0c
    hi StatusLineNC cterm=reverse ctermfg=240 ctermbg=NONE gui=reverse guifg=#303030 guibg=#0c0c0c
    hi Visual cterm=reverse ctermfg=250 ctermbg=NONE gui=reverse guifg=#bcbcbc guibg=#0c0c0c
    hi VertSplit cterm=NONE ctermfg=Gray ctermbg=NONE gui=NONE guifg=#0c0c0c guibg=#0c0c0c
    hi TermCursor cterm=reverse ctermfg=NONE ctermbg=NONE gui=reverse guifg=NONE guibg=NONE
    hi ColorColumn  cterm=reverse ctermfg=NONE ctermbg=NONE gui=reverse guifg=NONE guibg=NONE
    hi ModeMsg cterm=NONE ctermfg=DarkGray ctermbg=NONE
else
    hi Normal cterm=NONE ctermfg=247 ctermbg=NONE
    hi Keyword cterm=NONE ctermfg=White ctermbg=NONE
    hi Constant cterm=NONE ctermfg=Gray ctermbg=NONE
    hi String cterm=NONE ctermfg=Gray ctermbg=NONE
    hi Comment cterm=NONE ctermfg=DarkGray ctermbg=NONE
    hi Number cterm=NONE ctermfg=White ctermbg=NONE
    hi Error cterm=NONE ctermfg=White ctermbg=NONE
    hi ErrorMsg cterm=NONE ctermfg=White ctermbg=NONE
    hi Search cterm=NONE ctermfg=Gray ctermbg=NONE
    hi IncSearch cterm=reverse ctermfg=White ctermbg=NONE
    hi DiffChange cterm=NONE ctermfg=White ctermbg=NONE
    hi DiffText cterm=bold ctermfg=Gray ctermbg=White
    hi SignColumn cterm=NONE ctermfg=White ctermbg=NONE
    hi SpellBad cterm=undercurl ctermfg=White ctermbg=NONE
    hi SpellCap cterm=NONE ctermfg=White ctermbg=NONE
    hi SpellRare cterm=NONE ctermfg=White ctermbg=NONE
    hi WildMenu cterm=NONE ctermfg=DarkGray ctermbg=NONE
    hi Pmenu ctermfg=NONE ctermbg=236 cterm=NONE guifg=NONE guibg=#64666d gui=NONE
    hi PmenuSel ctermfg=NONE ctermbg=24 cterm=NONE guifg=NONE guibg=#204a87 gui=NONE
    hi SpecialKey cterm=NONE ctermfg=White ctermbg=NONE
    hi MatchParen cterm=NONE ctermfg=White ctermbg=DarkGray
    hi CursorLine cterm=NONE ctermfg=NONE ctermbg=NONE
    hi StatusLine cterm=bold,reverse ctermfg=230 ctermbg=NONE
    hi StatusLineNC cterm=reverse ctermfg=230 ctermbg=NONE
    hi Visual cterm=reverse ctermfg=Gray ctermbg=NONE
    hi TermCursor cterm=reverse ctermfg=NONE ctermbg=NONE
    hi ColorColumn  cterm=reverse ctermfg=NONE ctermbg=NONE
    hi ModeMsg cterm=NONE ctermfg=DarkGray ctermbg=NONE
endif

hi! link Conceal Normal
hi! link Boolean Normal
hi! link Delimiter Normal
hi! link Identifier Normal
hi! link Title Normal
hi! link Debug Normal
hi! link Exception Normal
hi! link FoldColumn Normal
hi! link Macro Normal
hi! link ModeMsg Normal
hi! link MoreMsg Normal
hi! link Question Normal
hi! link Conditional Keyword
hi! link Statement Keyword
hi! link Operator Keyword
hi! link Structure Keyword
hi! link Function Keyword
hi! link Include Keyword
hi! link Type Keyword
hi! link Typedef Keyword
hi! link Todo Keyword
hi! link Label Keyword
hi! link Define Keyword
hi! link DiffAdd Keyword
hi! link diffAdded Keyword
hi! link diffCommon Keyword
hi! link Directory Keyword
hi! link PreCondit Keyword
hi! link PreProc Keyword
hi! link Repeat Keyword
hi! link Special Keyword
hi! link SpecialChar Keyword
hi! link StorageClass Keyword
hi! link SpecialComment String
hi! link CursorLineNr String
hi! link Character Number
hi! link Float Number
hi! link Tag Number
hi! link Folded Number
hi! link WarningMsg Number
hi! link iCursor SpecialKey
hi! link SpellLocal SpellCap
hi! link LineNr Comment
hi! link NonText NonText 
hi! link DiffDelete Comment
hi! link diffRemoved Comment
hi! link PmenuSbar Visual
hi! link PmenuSel Visual
hi! link VisualNOS Visual
hi! link VertSplit VertSplit
hi! link Underlined SpellRare
hi! link rstEmphasis SpellRare
hi! link diffChanged DiffChange
