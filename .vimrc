" general config _______________________ 

set nocompatible

filetype plugin indent on

set encoding=utf-8
set hidden
set ttyfast
set updatetime=300
set timeout timeoutlen=1000 ttimeoutlen=5
set undolevels=500
set history=500
set shortmess+=c
set signcolumn=number " make sign replace number
set nowrap
set backspace=indent,eol,start " indentat
set incsearch ignorecase smartcase hlsearch" search     
set autoindent tabstop=2 softtabstop=2 shiftwidth=2 expandtab " indenting
set lazyredraw " only essential redraws
set synmaxcol=180
set nobackup nowb noswapfile " turn off backups
set viminfo='20,\"100 "max 100 lines in registers
set novisualbell
set conceallevel=1
set clipboard=unnamedplus

" functions ____________________________

func! ToggleRnu() " toggle: no numbers - relative nummbers
  if(&nu) | set nonu nornu | else | set nu rnu | endif
endfunc

" keybindings __________________________

nnoremap <space> <nop>
let mapleader = " "

" split
nmap ss :sp<Return><c-w>w
nmap sv :vs<Return><c-w>w

" split navigate
nnoremap sw <c-w>w
nnoremap sh <c-w>h
nnoremap sj <c-w>j
nnoremap sk <c-w>k
nnoremap sl <c-w>l

" split resize
nnoremap srh <C-w><
nnoremap srl <C-w>>
nnoremap srk <C-w>+
nnoremap srj <C-w>-

" file tree
nnoremap sb :Lex<CR>

" hard mode
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

" quick quit
nnoremap <leader>w :w<cr>
nnoremap <leader>q :q<cr>

" quick exit insert
inoremap jj <esc>

" clear search highlight
nnoremap <leader><space> :noh<cr>

" silver search
nnoremap <leader>A :Ag <cr> 

" toggle numbers
nnoremap <leader>n :call ToggleRnu()<cr>

" coc
inoremap <silent><expr> <TAB>
  \ pumvisible() ? "\<C-n>" :
  \ <SID>check_back_space() ? "\<TAB>" :
  \ coc#refresh()

noremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" <c-space> triggers completion
inoremap <silent><expr> <c-@> coc#refresh()

" code action on cursor position
nmap <leader>do <Plug>(coc-codeaction)

" apply code action to selected region
" xmap <leader>a  <Plug>(coc-codeaction-selected)
" nmap <leader>a  <Plug>(coc-codeaction-selected)

set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" :Prettier command to prettify file
command! -nargs=0 Prettier :CocCommand prettier.formatFile

" plugins ______________________________ 

call plug#begin()

Plug 'preservim/nerdcommenter'
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'junegunn/fzf.vim'

Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'leafgarland/typescript-vim', { 'for': ['typescript', 'typescript.tsx'] }
Plug 'peitalin/vim-jsx-typescript', { 'for': ['typescript.tsx'] }

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
  \]

Plug 'supercollider/scvim'
Plug 'tidalcycles/vim-tidal'

call plug#end()

" plugin config ________________________ 

" NERDCommenter
let g:NERDSpaceDelims = 1
let g:NERDCompactSexyComs = 1
let NERDAltDelims_haskell = 1
let g:NERDCustomDelimiters = { 'tidal': { 'left': '{-','right': '-}' } }
let g:NERDCustomDelimiters = { 'tidal': { 'left': '--','right': '' } }
let g:NERDCommentEmptyLines = 1

" fzf popup
let g:fzf_layout = {'window': { 'width': 0.62, 'height': 0.62}}

" fzf - use silversearcher-ag to respect .gitignore
let $FZF_DEFAULT_COMMAND = 'ag -g ""'
let g:ag_working_path_mode="r"
set wildignore+=*/node_modules/*,*/tmp/*,*.so,*.swp,*.zip " fzf ignore

" js & ts
let g:javascript_plugin_jsdoc = 1 " jsdoc syntax highlighting
let g:javascript_plugin_flow = 1 " flow syntax highlighting
let g:javascript_conceal_function = "ƒ"
let g:javascript_conceal_return   = "⇚""

" SuperCollider
au BufEnter,BufWinEnter,BufNewFile,BufRead *.sc,*.scd set filetype=supercollider
au Filetype supercollider packadd scvim

" tidalvim
let g:tidal_flash_duration = 50 

" plugin keybindings ___________________  

nnoremap <c-p> :FZF<cr>
nnoremap <leader>b :Buffers<cr>
nnoremap <leader>h :History<cr>
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-s': 'split',
  \ 'ctrl-v': 'vsplit'
  \}

" theme ________________________________  

colorscheme darkness
