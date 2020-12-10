" General config ______________________
"

set nocompatible

filetype plugin indent on

set encoding=utf-8
set hidden
set ttyfast
set updatetime=300
set timeout timeoutlen=200 ttimeoutlen=5
set undolevels=500
set history=500
set nu rnu
set signcolumn=number " make sign replace number
set nowrap
set backspace=indent,eol,start " indentation
set incsearch ignorecase smartcase hlsearch" search
set autoindent tabstop=2 softtabstop=2 shiftwidth=2 expandtab " indenting
set lazyredraw " only essential redraws
set synmaxcol=181
set nobackup nowb noswapfile " no backups
set viminfo='20,\"101 "max 100 lines in registers
set novisualbell
set conceallevel=1
set clipboard=unnamedplus

" Functions ____________________________

" Function: Toggle line numbers

func! ToggleRnu() " toggle: no numbers - relative nummbers
  if(&nu) | set nonu nornu | else | set nu rnu | endif
endfunc

" Keybindings __________________________

" Keybindings: leader keys

nnoremap <space> <nop>
let mapleader = " "
let maplocalleader = ";"

" Keybindings: splits

" Split horizontal & vertical
nmap ss :sp<Return><c-w>w
nmap sv :vs<Return><c-w>w

" Navigate splits
nnoremap sw <c-w>w
nnoremap sh <c-w>h
nnoremap sj <c-w>j
nnoremap sk <c-w>k
nnoremap sl <c-w>l

" Resize splits
nnoremap sH <c-w>8<
nnoremap sJ <c-w>8-
nnoremap sK <c-w>8+
nnoremap sL <c-w>8>

" Resize to equal splits
nnoremap s= <c-w>=

" Keybindings: file tree

nnoremap sb :Lex<cr>


" Keybindings: terminal

nnoremap <leader>t :term<cr>

" Keybindings: hard mode

nnoremap <left> <nop>
nnoremap <down> <nop>
nnoremap <up> <nop>
nnoremap <right> <nop>
inoremap <left> <nop>
inoremap <down> <nop>
inoremap <up> <nop>
inoremap <right> <nop>

" Keybindings: quick quit

nnoremap <leader>w :w<cr>
nnoremap <leader>W :wq<cr>
nnoremap <leader>q :q<cr>
nnoremap <leader>Q :q!<cr>

" Keybindings: exit insert mode

inoremap jj <esc>

" Keybindings: search

nnoremap <leader><space> :noh<cr>

" Silver search
nnoremap <leader>A :Ag <cr>

" Keybindings: line numbers

nnoremap <leader>n :call ToggleRnu()<cr>

" Keybindings: config

nnoremap <leader>ec :split $MYVIMRC<cr>
nnoremap <leader>so :so %<cr>

" Plugins ______________________________ 

call plug#begin()

" Plugins: General

Plug 'preservim/nerdcommenter'
Plug 'takac/vim-hardtime'
Plug 'machakann/vim-sandwich'
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'vimwiki/vimwiki'
Plug 'axvr/zepl.vim'

" Plugins: Languages

" JS & TypeScript

Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'leafgarland/typescript-vim', { 'for': ['typescript', 'typescript.tsx'] }
Plug 'peitalin/vim-jsx-typescript', { 'for': ['typescript.tsx'] }
Plug 'evanleck/vim-svelte', {'branch': 'main'}

" Coc w/ extensions

Plug 'neoclide/coc.nvim', {'branch': 'release'}
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

" Plugin: LaTeX

Plug 'lervag/vimtex'

" Plugin: TidalCycles

Plug 'supercollider/scvim'
Plug 'tidalcycles/vim-tidal'

" Plugin: Scheme
"
Plug 'jpalardy/vim-slime'

call plug#end()

" Plugin config ________________________

" Plugin: NERDCommenter

let g:NERDSpaceDelims = 1
let g:NERDCompactSexyComs = 1
let NERDAltDelims_haskell = 1
let g:NERDCustomDelimiters = { 'tidal': { 'left': '{-','right': '-}' } }
let g:NERDCustomDelimiters = { 'tidal': { 'left': '--','right': '' } }
let g:NERDCommentEmptyLines = 1

" Plugin: fzf

" Popup

let g:fzf_layout = {'window': { 'width': 0.62, 'height': 0.62}}

" Use silversearcher-ag to respect .gitignore

let $FZF_DEFAULT_COMMAND = 'ag --hidden --ignore .git -g ""'
let g:ag_working_path_mode="r"
set wildignore+=*/node_modules/*,*/tmp/*,*.so,*.swp,*.zip " fzf ignore

" Plugin: VimWiki

let g:vimwiki_list = [{'path': '~/.vimwiki/',
                      \ 'template_path': '~/.vimwiki/templates/',
                      \ 'template_default': 'default',
                      \ 'syntax': 'markdown', 'ext': '.md',
                      \ 'path_html': '~/.vimwiki/site_html/', 'custom_wiki2html': 'vimwiki_markdown',
                      \ 'html_filename_parameterization': 1,
                      \ 'template_ext': '.tpl'}]

" Plugin: JS & TypeScript

let g:javascript_plugin_jsdoc = 1 " jsdoc syntax highlighting
let g:javascript_plugin_flow = 1 " flow syntax highlighting
let g:javascript_conceal_function = "ƒ"
let g:javascript_conceal_return = "⇖"

" Plugin: Svelte

let g:svelte_indent_script = 0
let g:svelte_indent_style = 0

" Plugin: TidalCycles

" SuperCollider

au BufEnter,BufWinEnter,BufNewFile,BufRead *.sc,*.scd set filetype=supercollider
au Filetype supercollider packadd scvim

" TidalVim

let g:tidal_default_config = {"socket_name": "default", "target_pane": "tidal:1.1"}

" Plugin: Hardtime

let g:hardtime_default_on = 1
let g:hardtime_maxcount = 4

" Plugin: vim-slime

let g:slime_default_config={"socket_name": get(split($TMUX, ","), 0), "target_pane": '{last}'}
let g:slime_dont_ask_default = 1
let g:slime_paste_file=tempname()
let g:slime_target='tmux'
let g:slime_no_mappings = 1
xmap <leader>el <Plug>SlimeRegionSend
nmap <leader>ep <Plug>SlimeParagraphSend

" Plugin keybindings ___________________

" Plugin: fzf
nnoremap <c-p> :FZF<cr>
nnoremap <leader>p :FZF<cr>
nnoremap <leader>b :Buffers<cr>
nnoremap <leader>h :History<cr>
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-s': 'split',
  \ 'ctrl-v': 'vsplit'
  \}

" Plugin: coc

" Autocompletion

inoremap <silent><expr><c-j> pumvisible() ? "\<c-n>" :
  \ coc#refresh()
inoremap <expr><c-k> pumvisible() ? "\<C-p>" : "k"
inoremap <expr><cr> complete_info()["selected"] != "-1" ? "\<c-y>" : "\<c-g>u\<CR>"

" Code action on cursor position

nmap <leader>do <Plug>(coc-codeaction)

set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" :Prettier command to prettify file

command! -nargs=0 Prettier :CocCommand prettier.formatFile

" Plugin: vimtex

let g:vimtex_view_method='zathura'
let g:tex_flavor = 'latex'
let g:tex_conceal='abdmg'

" Theme ________________________________

colorscheme simple-dark

set laststatus=2
set noshowcmd
set noshowmode
set statusline=
set statusline+=%3*
set statusline+=%=
set statusline+=%1*\ %02l/%L\
set statusline+=%2*\%02v
set statusline+=%1*\
set statusline+=%0*\ %n\
set shm+=a
set shm+=W
