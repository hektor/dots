" General config ______________________
"
" Most acronyms are unreadable, but they keep the overview
" I have to do a quick :h lookup for most of these anyway.

se nocp                  " Disable vi incompatibility
filet plugin indent on   " Filetype recognition
se enc=utf8              "
se hid                   " Allow hiding unsaved buffers
se tf                    " Fast tty
se ut=300                " 300ms for update time
se to tm=200 ttm=5       " Timeouts
se shm+=c                " ...
se ul=500 hi=500         " History and undo
se nu rnu scl=number     " Line numbers & signs
se nowrap
se bs=indent,eol,start   " Indentation
se ai ts=2 sts=2 sw=2 et " Indentation
se is ic scs hls         " Search
se lz                    " Only essential redraws
se nobk nowb noswf       " No backups
se vi='20,\"101          " Max 100 lines in registers
se novb                  " Bell
se cole=1                " Conceal
se cb=unnamedplus        " Clipboard
se fcs+=vert:│           " Cleaner split separator (tmux style)
set lcs=tab:→\ ,trail:·  " Whitespace rendering

" Functions ____________________________

" Toggle line numbers
fu! ToggleRnu() " toggle: no numbers - relative nummbers
  if(&nu) | se nonu nornu | else | se nu rnu | endif
endfu

" Keybindings
"""""""""""""

" Leader keys
nn <space> <nop>
let mapleader = " "
let maplocalleader = ";"
" Splits & navigation
nm ss :sp<CR><c-w>w    " Split horizontal
nm sv :vs<CR><c-w>w    " Split vertical
nn sw <c-w>w           " Navigate splits
nn sh <c-w>h           " "
nn sj <c-w>j           " "
nn sk <c-w>k           " "
nn sl <c-w>l           " "
nn sH <c-w>8<          " Resize splits
nn sJ <c-w>8-          " "
nn sK <c-w>8+          " "
nn sL <c-w>8>          " "
nn s= <c-w>=           " Equalize splits
" Open
nn sb :Lex<cr>         " File tree
nn <leader>t :term<cr> " Open terminal
" Remaps
ino <nowait> jj <esc>  " Normal now
nn  <left>  <nop>      " Hard mode
nn  <down>  <nop>      " "
nn  <up>    <nop>      " "
nn  <right> <nop>      " "
ino <left>  <nop>      " "
ino <down>  <nop>      " "
ino <up>    <nop>      " "
ino <right> <nop>      " "
" Search
nn <leader>/ :noh<cr>
nn <leader>f :Ag <cr>
" Line numbers
nn <leader>n :call ToggleRnu()<cr>
" Vim configuration
nn <leader>ec :split $MYVIMRC<cr>
nn <leader>so :so %<cr>

" Plugins
"""""""""

call plug#begin()
" Coc
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" General
Plug 'tpope/vim-commentary'
Plug 'machakann/vim-sandwich'
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'vimwiki/vimwiki'
" JS and TypeScript
Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'leafgarland/typescript-vim', { 'for': ['typescript', 'typescript.tsx'] }
Plug 'peitalin/vim-jsx-typescript', { 'for': ['typescript.tsx'] }
Plug 'evanleck/vim-svelte', {'branch': 'main'}
" LaTeX
Plug 'lervag/vimtex'
" TidalCycles
Plug 'supercollider/scvim'
Plug 'tidalcycles/vim-tidal'
" Lisp & Scheme
Plug 'kovisoft/slimv', { 'for': ['clojure', 'scheme', 'racket'] }
call plug#end()

" Plugin config
"""""""""""""""

" Coc
let g:coc_global_extensions = [
  \'coc-eslint',
  \'coc-prettier',
  \'coc-tsserver',
  \'coc-python',
  \'coc-html',
  \'coc-emmet',
  \'coc-css',
  \'coc-svg',
  \'coc-svelte',
  \'coc-json',
  \'coc-markdownlint',
  \'coc-yaml',
  \'coc-snippets'
  \]
" Autocomplete
ino <silent><expr><c-j> pumvisible() ? "\<c-n>" :
  \ coc#refresh()
ino <expr><c-k> pumvisible() ? "\<C-p>" : "k"
ino <expr><cr> complete_info()["selected"] != "-1" ? "\<c-y>" : "\<c-g>u\<CR>"
" Code action on cursor position
nm <leader>do <Plug>(coc-codeaction)
" Coc statusline
se statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}
" Prettier command
com! -nargs=0 Prettier :CocCommand prettier.formatFile

" LaTex
let g:vimtex_view_method='zathura'
let g:tex_flavor = 'latex'
let g:tex_conceal='abdmg'
let g:vimtex_quickfix_mode=0

" Comments
xm <leader>c <Plug>Commentary
nm <leader>c <Plug>Commentary
nm <leader>cc <Plug>CommentaryLine

" FZF
let g:fzf_layout = {'window': { 'width': 0.62, 'height': 0.62}}
let g:ag_working_path_mode="r"
let $FZF_DEFAULT_COMMAND = 'ag --hidden --ignore .git -g ""' " respect gitignore
se wildignore+=*/node_modules/*,*/tmp/*,*.so,*.swp,*.zip "   " ignore these

" Wiki
let g:vimwiki_auto_chdir = 1
let g:vimwiki_list = [{'path': '~/.vimwiki/', 'syntax': 'markdown', 'ext': '.md'}]

" JS and TypeScript
let g:javascript_plugin_jsdoc = 1 " jsdoc syntax highlighting
let g:javascript_plugin_flow = 1 " flow syntax highlighting
let g:javascript_conceal_function = "ƒ"
let g:javascript_conceal_return = "⇖"
let g:svelte_indent_script = 0
let g:svelte_indent_style = 0

" SuperCollider
au BufEnter,BufWinEnter,BufNewFile,BufRead *.sc,*.scd se filetype=supercollider
au Filetype supercollider packadd scvim

" Tidalcycles (sclang and vim-tidal)
let g:tidal_default_config = {"socket_name": "default", "target_pane": "tidal:1.1"}

" Slimv
let g:slimv_swank_cmd = '! tmux new-window -d -n REPL-SBCL "sbcl --load ~/.vim/bundle/slimv/slime/start-swank.lisp"'

" Plugin keybindings
""""""""""""""""""""

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

" Color theme & statusline
""""""""""""""""""""""""""

se nosc
se nosmd
se ls=2
se stl=\ %0*%n
se stl+=\ %m
se stl+=\ %y%1*
se stl+=\ %<%F
se stl+=\ %1*%=%5l%*
se stl+=%2*/%L%*

colo simple-dark

" Other
"""""""""""""""""""""""

" VIM config hot reload
autocmd! bufwritepost .vimrc source %
