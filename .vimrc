" General config {{{
"
" Tip: acronyms for overview, use `:h` for a quick lookup.

set nocp                               " Disable vi incompatibility
filetype plugin indent on              " Filetype recognition
set enc=utf8                           " Default to UTF-8 encoding
set hid                                " Allow hiding unsaved buffers
set tf                                 " Fast tty
set ut=300                             " 300ms for update time
set to tm=200 ttm=5                    " Timeouts
set shm+=c                             " ...
set ul=500 hi=500                      " History and undo
set nu rnu scl=number                  " Line numbers & signs
set nowrap
set bs=indent,eol,start                " Indentation
set ai ts=2 sts=2 sw=2 et              " Indentation
set is ic scs hls                      " Search
set lz                                 " Only essential redraws
set nobk nowb noswf                    " No backups
set vi='20,\"101                       " Max 100 lines in registers
set novb                               " Bell
set cole=0 cocu=""                     " Conceal
set cb=unnamedplus                     " Clipboard
set fcs+=vert:│                        " Cleaner split separator (tmux style)
set list
set lcs=trail:·,tab:→\ ,nbsp:␣         " Whitespace rendering
set ar                                 " Autoread
set spellsuggest+=5                    " Limit spell suggestions
set wildignore+=*/node_modules/*,*/tmp/*,*.so,*.swp,*.zip
set thesaurus+=~/.vim/thesaurus/mthesaur.txt

" File explorer
let g:netrw_winsize = 30
let g:netrw_liststyle=3
let g:netrw_banner = 0

" }}}

" Folds {{{

" Insert date
fu! Today()
  :put =strftime('%d %b %Y')
endfu
set foldmethod=marker

augroup filetype_vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
augroup END

augroup filetype_python
    autocmd!
    autocmd FileType python setlocal foldmethod=indent
augroup END

augroup filetype_sh
    autocmd!
    autocmd FileType sh setlocal foldmethod=marker
augroup END

" }}}

" Mappings {{{
"

" Leader keys

nn <space><nop>
let mapleader = " "
let maplocalleader = ";"

" Splits & navigation

nm s <c-w>           " Split horizontal
nm ss :sp<CR><c-w>w| " Split horizontal
nm sv :vs<CR><c-w>w| " Split vertical
nn sw <c-w>w|        " Navigate splits
nn sh <c-w>h|        "
nn sj <c-w>j|        "
nn sk <c-w>k|        "
nn sl <c-w>l|        "
nn sH <c-w>8<|       " Resize splits
nn sJ <c-w>8-|       "
nn sK <c-w>8+|       "
nn sL <c-w>8>|       "
nn s= <c-w>=|        " Equalize splits

" Open

nn sb :Lex<cr>|          " File tree
nn <leader><leader> :noh<cr> |"
nn <leader>t :term<cr>| " Open terminal
nn <leader>o :!xdg-open http://localhost:8080/%:t:r.html & <cr>
" Remaps
ino <nowait> jj <esc>|   " Normal now
nn  <left>  <nop>|       " Hard mode
nn  <down>  <nop>|       " "
nn  <up>    <nop>|       " "
nn  <right> <nop>|       " "
ino <left>  <nop>|       " "
ino <down>  <nop>|       " "
ino <up>    <nop>|       " "
ino <right> <nop>|       " "
" Search
nn <c-_> :noh<cr>| " map 'ctrl + /'
" Use `the_silver_searcher` to find results (for selection if selection)
nn <leader>f :Ag <cr>
vm <leader>f y:Ag <C-r>"<cr>
" Toggle line numbers
nn <leader>n :set nu! rnu!<cr>
" Vim configuration
nn <leader>ec :vs $MYVIMRC<cr>
nn <leader>so :so %<cr>

" }}}

" Plugins {{{

" Plug setup {{{

call plug#begin()
if !exists('g:vscode')
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'github/copilot.vim'
endif
" General
Plug 'unblevable/quick-scope'
Plug 'Shougo/context_filetype.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-obsession'
Plug 'machakann/vim-sandwich'
Plug 'editorconfig/editorconfig-vim'
Plug 'honza/vim-snippets'
Plug 'chrisbra/unicode.vim'
Plug 'ap/vim-css-color'
" Fzf
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
" JS and TypeScript
Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'leafgarland/typescript-vim', { 'for': ['typescript', 'typescript.tsx'] }
Plug 'peitalin/vim-jsx-typescript', { 'for': ['typescript.tsx'] }
Plug 'evanleck/vim-svelte', {'branch': 'main'}
" JSON with comments
Plug 'neoclide/jsonc.vim'
" LaTeX
Plug 'lervag/vimtex'
" Wiki
Plug 'lervag/wiki.vim'
Plug 'hektor/taskwiki'
" Markdown
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'ferrine/md-img-paste.vim'
" TidalCycles
Plug 'supercollider/scvim'
Plug 'tidalcycles/vim-tidal'
" GLSL
Plug 'tikhomirov/vim-glsl'
Plug 'timtro/glslView-nvim'
" Jupyter notebooks
Plug 'goerz/jupytext.vim'
" OpenSCAD
Plug 'sirtaj/vim-openscad'
call plug#end()
" }}}

" Plugin config {{{

" `.../sandwich` {{{
nmap s <Nop>
xmap s <Nop>
let g:sandwich#recipes = deepcopy(g:sandwich#default_recipes)
" }}}

" `.../copilot` {{{

let g:copilot_node_command = "/home/h/.config/nvm/versions/node/v16.18.0/bin/node"
let g:copilot_no_tab_map = v:true
imap <c-l> <Plug>(copilot-next)
imap <c-h> <Plug>(copilot-prev)
imap <silent><script><expr> <s-tab> copilot#Accept("\<CR>")
" Show Copilot node v16 as it does not work with v18 yet

" }}}

" `.../vimtex` {{{

let g:vimtex_view_method='zathura'
let g:tex_flavor='latex'
let g:tex_conceal='abdmgs'
let g:vimtex_quickfix_mode=0

" }}}

" 'ferrine/md-img-paste.vim' {{{

" Paste clipboard images
au FileType pandoc nmap <buffer><silent> <leader>v :call mdip#MarkdownClipboardImage()<CR>
au FileType markdown nmap <buffer><silent> <leader>v :call mdip#MarkdownClipboardImage()<CR>

" }}}

" 'tpope/vim-commentary' {{{

xm <leader>c <Plug>Commentary
nm <leader>c <Plug>Commentary
nm <leader>cc <Plug>CommentaryLine

" }}}

" `junegunn/fzf` {{{
" `junegunn/fzf.vim`

let g:fzf_layout = {'window': { 'width': 1, 'height': 0.62}}
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Comment'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

let g:ag_working_path_mode="r"

" }}}

" `vim-pandoc/vim-pandoc` {{{
" `vim-pandoc/vim-pandoc-syntax`

au FileType pandoc call pandoc#completion#Init()
let g:pandoc#filetypes#pandoc_markdown=0
let g:pandoc#spell#enabled=0
let g:pandoc#spell#default_langs=['en_us', 'nl_be']
let g:pandoc#formatting#mode='a'
let g:pandoc#formatting#textwidth=90
let g:pandoc#modules#disabled = ["formatting", "dashes", "yaml", "metadata"]

let g:pandoc#syntax#conceal#urls=1
let g:pandoc#syntax#conceal#blacklist=[]
let g:pandoc#syntax#style#emphases=0 " Bug workaround
let g:pandoc#syntax#conceal#cchar_overrides = { "atx": " ", "li": "·" }

" }}}

" `euclio/vim-markdown-composer` {{{

let g:markdown_composer_custom_css = ['file:///home/h/.zk/pandoc.css']
let g:markdown_composer_external_renderer='pandoc -f markdown+latex_macros-yaml_metadata_block -t html5 --mathjax --bibliograph /home/h/.zk/references.bib --citeproc --lua-filter=/home/h/.wiki/lua-filters/diagram-generator/diagram-generator.lua --lua-filter=/home/h/.wiki/filters/html-links.lua --lua-filter=/home/h/.zk/filters/tikz.lua'
let g:markdown_composer_autostart = 0

" }}}

" `lervag/wiki.vim` {{{

" Only load wiki.vim for zk directory
let g:wiki_global_load=0
let g:wiki_root='~/.zk'
let g:wiki_index_name='index'
let g:wiki_zotero_root='~/doc/Zotero'
let g:wiki_filetypes=['md']
let g:wiki_completion_case_sensitive=0

" If we are on a wiki link 

" TODO: configure the following
" let g:wiki_file_handlenmap r

" Links
" FIXME: figure out what '' vs '.md' does
let g:wiki_link_extension='.md'
" Do not automatically transform to link, use `<leader>wf` for this
let g:wiki_link_toggle_on_follow=0
let g:wiki_link_target_type='md'

let g:wiki_map_text_to_link='Slugify'

" E.g. transform `My link` into `[My link](my-link.md)`
function Slugify(text) abort
  return [substitute(tolower(a:text), '\s\+', '-', 'g'), a:text]
endfunction

vmap <leader>wf <Plug>(wiki-link-toggle-visual)

" Automatically save when navigation
let g:wiki_write_on_nav=1

" }}}

" `tools-life/taskwiki` {{{

let g:taskwiki_taskrc_location='/home/h/.config/task/taskrc'
let g:taskwiki_disable_concealcursor=1
let g:taskwiki_dont_preserve_folds=1
let g:taskwiki_dont_fold=1

" }}}

" `pangloss/vim-javascript` {{{

let g:javascript_plugin_jsdoc = 1 " jsdoc syntax highlighting
let g:javascript_plugin_flow = 1 " flow syntax highlighting
let g:javascript_conceal_function = "ƒ"
let g:javascript_conceal_return = "⇖"
let g:svelte_indent_script = 0
let g:svelte_indent_style = 0

" }}}

" `.../quickscope` {{{

let g:qs_max_chars=80
let g:qs_lazy_highlight = 1

" }}}


" Tidalcycles (sclang and vim-tidal)
let g:tidal_default_config = {"socket_name": "default", "target_pane": "tidal:1.1"}
let g:tidal_no_mappings = 1

au FileType tidal nm <buffer> <leader>ep <Plug>TidalParagraphSend
au FileType tidal nm <buffer> <leader>ee <Plug>TidalLineSend
au FileType tidal nnoremap <buffer> <leader>h :TidalHush<cr>
au FileType tidal com! -nargs=1 S :TidalSilence <args>
au FileType tidal com! -nargs=1 P :TidalPlay <args>
au FileType tidal com! -nargs=0 H :TidalHush

" SuperCollider
au BufEnter,BufWinEnter,BufNewFile,BufRead *.sc,*.scd se filetype=supercollider
au Filetype supercollider packadd scvim

" }}}

" FZF
nn <c-p> :FZF<cr>
nn <leader>p :FZF<cr>
nn <leader>b :Buffers<cr>
nn <leader>h :History<cr>
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-s': 'split',
  \ 'ctrl-v': 'vsplit'
  \}
" Insert path completion
ino <expr><c-f> fzf#vim#complete#path('rg --files --sort path')

" }}}

" Color theme & statusline {{{
"

se ls=2
se stl=\ %0*%n
se stl+=\ %m
se stl+=\ %y%0*
se stl+=\ %<%F
se stl+=\ %0*%=%5l%*
se stl+=%0*/%L%*

colo yang

" }}}

" Quick hacks {{{
"

fu! Compile()
  if expand('%:e') == "md" 
    :silent exec "!pandoc % -s -o /tmp/op.pdf &"
  endif
endfu

fu! Preview()
  :call Compile()
  :silent exec "!zathura /tmp/op.pdf &"
endfu

" VIM config hot reload

" TODO: separate to filetype specific files

" JS
" Jump between `=` and `;`
au FileType javascript set mps+==:;

" JSONC (see https://github.com/neoclide/jsonc.vim/pull/9")
au BufNewFile,BufRead */.vscode/*.json setlocal filetype=jsonc

" Hacky way to pass on active note to script for automated for HTML preview
au BufEnter /home/h/.zk/*.md silent exe '!echo % > /home/h/.zk/current-zettel.txt'
au BufEnter /home/h/.zk/*.md silent exe '!cat %:r.html > /home/h/.zk/current-zettel-content.html'

" }}}

highlight QuickScopeSecondary cterm=underline
highlight QuickScopePrimary ctermbg=253 ctermfg=232 cterm=none
highlight Pmenu ctermfg=232

function! SynGroup()
    let l:s = synID(line('.'), col('.'), 1)
    echo synIDattr(l:s, 'name') . ' -> ' . synIDattr(synIDtrans(l:s), 'name')
endfun

com! -nargs=0 Syn :call SynGroup()

" Taken from /usr/share/vim/vim90/defaults.vim
augroup vimStartup
  au!

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid, when inside an event handler
  " (happens when dropping a file on gvim) and for a commit message (it's
  " likely a different one than last time).
  autocmd BufReadPost *
    \ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
    \ |   exe "normal! g`\""
    \ | endif

augroup END

augroup Vim
  au!
  " Reload vim config when ~/.vimrc is changed
  au BufWritePost $HOME/.vimrc so $MYVIMRC | redraw | echo "Reloaded vimrc"
augroup END
