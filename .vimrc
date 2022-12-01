" General config {{{
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
se cole=0 cocu=""        " Conceal
se cb=unnamedplus        " Clipboard
se fcs+=vert:│           " Cleaner split separator (tmux style)
set list
set lcs=trail:·,tab:→\ ,nbsp:␣ " Whitespace rendering
set ar                   " Autoread

" Functions ____________________________

" Toggle line numbers
fu! ToggleLineNumbers()
  set nu! rnu! " toggle: no numbers - relative nummbers
endfu

" https://alok.github.io/2018/04/26/using-vim-s-conceal-to-make-languages-more-tolerable/
fu! ToggleConceal()
  if (&cole == 0) | se cole =2 | else | set cole =0 | endif
endfu

" Insert date
fu! Today()
  :put =strftime('%d %b %Y')
endfu

" Add command line functions names
com! -nargs=0 Today :call Today()
com! -nargs=0 ToggleLineNumbers :call ToggleLineNumbers()
com! -nargs=0 ToggleConceal :call ToggleConceal()

" Setup Man command for reading man pages
if exists(":Man") != 2
  source $VIMRUNTIME/ftplugin/man.vim
endif

" Keybindings
"""""""""""""

" Leader keys
nn <space> <nop>
let mapleader = " "
let maplocalleader = ";"
" Splits & navigation
nm ss :sp<CR><c-w>w|     " Split horizontal
nm sv :vs<CR><c-w>w|     " Split vertical
nn sw <c-w>w|            " Navigate splits
nn sh <c-w>h|            " "
nn sj <c-w>j|            " "
nn sk <c-w>k|            " "
nn sl <c-w>l|            " "
nn sH <c-w>8<|           " Resize splits
nn sJ <c-w>8-|           " "
nn sK <c-w>8+|           " "
nn sL <c-w>8>|           " "
nn s= <c-w>=|            " Equalize splits
" Open
nn sb :Lex<cr>|          " File tree
nn <leader>t :term<cr>| " Open terminal
nn <leader>o :!xdg-open http://localhost:8080/%:r.html & <cr>
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
Plug 'github/copilot.vim'
" General
Plug 'unblevable/quick-scope'
Plug 'tpope/vim-commentary'
Plug 'machakann/vim-sandwich'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
" JS and TypeScript
Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'leafgarland/typescript-vim', { 'for': ['typescript', 'typescript.tsx'] }
Plug 'peitalin/vim-jsx-typescript', { 'for': ['typescript.tsx'] }
Plug 'evanleck/vim-svelte', {'branch': 'main'}
" JSON with comments
Plug 'neoclide/jsonc.vim'
" LaTeX & markdown
Plug 'lervag/vimtex'
" Wiki
Plug 'lervag/wiki.vim'
Plug 'hektor/taskwiki'
" Markdown
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
" TidalCycles
Plug 'supercollider/scvim'
Plug 'tidalcycles/vim-tidal'
call plug#end()

" Plugin config
"""""""""""""""

" Coc

" `neoclide/coc.nvim` {{{
let g:coc_global_extensions = [
  \'coc-dictionary',
  \'coc-syntax',
  \'coc-eslint',
  \'coc-prettier',
  \'coc-tsserver',
  \'coc-html',
  \'coc-emmet',
  \'coc-css',
  \'coc-svg',
  \'coc-svelte',
  \'coc-jedi',
  \'coc-json',
  \'coc-yaml',
  \'coc-snippets',
  \'coc-clangd',
  \'coc-bibtex' 
  \]
" Autocomplete
"

" `coc-snippets`

" TODO: is this block even necessary anymore?
"
" I use `coc-snippets-exand-jump` here as it is more flexible afaics
" As stated in the docs:
" `coc-snippets-expand`      Expand snippet w/ current inserted text
" `coc-snippets-expand-jump` Expand snippet or jump to next placeholder of current snippet if possible

" Use tab for expansion in every context Don't use it for jumping as
" expansions have priority over jumping (which is not always what you want)
"
" E.g. You don't want to expand `time` to say `12:05` upon `<tab>`bing to go
" from arguments to body in a function snippet

inor <silent><expr> <TAB> 
  \ coc#pum#visible() ? coc#_select_confirm() :
  \ coc#expandable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
  \ "\<TAB>"

" Use <c-j> and <c-k> to move through suggestions
" Open suggestions if not yet open

inor <silent><expr> <c-j> coc#pum#visible() ? coc#pum#next(1) : coc#refresh()

" NOTE: As the followning interfered with digraphs, I replaced `coc#refresh`
" with `"\<C-g>u\<c-k>"` which afaik just does c-k when no completion menu is
" shown
" ```
" inor <silent><expr> <c-k> coc#pum#visible() ? coc#pum#prev(1) : coc#refresh()
" ```

inor <silent><expr> <c-k> coc#pum#visible() ? coc#pum#prev(1) : "\<C-g>u\<c-k>"

" NOTE: this is cutting text in visual mode, it is replaces `$VISUAL` on next
" expansion
"
" although I mapped it, I have yet to test this feature

vmap <tab> <Plug>(coc-snippets-select)

ino <expr><cr> complete_info()["selected"] != "-1" ? "\<c-y>" : "\<c-g>u\<CR>"

" Show doccumentation
fu! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('')
  else
    call CocAction('doHover')
  endif
endfunction
nn <leader>es :CocCommand snippets.editSnippets<cr>
" Code action on cursor position
nm <leader>do <Plug>(coc-codeaction)
" Commands
com! -nargs=0 Prettier :CocCommand prettier.formatFile
" }}}

" `.../copilot` {{{

let g:copilot_node_command = "/home/h/.config/nvm/versions/node/v16.18.0/bin/node"
let g:copilot_no_tab_map = v:true
imap <c-l> <Plug>(copilot-next)
imap <c-h> <Plug>(copilot-prev)
imap <silent><script><expr> <s-tab> copilot#Accept("\<CR>")
" Show Copilot node v16 as it does not work with v18 yet

" LaTex
let g:vimtex_view_method='zathura'
let g:tex_flavor='latex'
let g:tex_conceal='abdmg'
let g:vimtex_quickfix_mode=0

" Markdown
au FileType markdown setl tw=80 fo+=t " Wrap markdown to 80 characters

" Comments
xm <leader>c <Plug>Commentary
nm <leader>c <Plug>Commentary
nm <leader>cc <Plug>CommentaryLine

" FZF
let g:fzf_layout = {'window': { 'width': 0.62, 'height': 0.62}}
let g:ag_working_path_mode="r"
let $FZF_DEFAULT_COMMAND = 'ag --hidden --ignore .git -g ""' " respect gitignore
se wildignore+=*/node_modules/*,*/tmp/*,*.so,*.swp,*.zip "   " ignore these

let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'Comment'],
  \ 'border':  ['fg', 'Comment'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'], }

let g:ag_working_path_mode="r"

" }}}

" `vim-pandoc/vim-pandoc` {{{
" `vim-pandoc/vim-pandoc-syntax`

au FileType pandoc call pandoc#completion#Init()
let g:pandoc#filetypes#pandoc_markdown=0
let g:pandoc#spell#enabled=0
let g:pandoc#spell#default_langs=['en_us', 'nl_be']
let g:pandoc#syntax#conceal#urls=1
let g:pandoc#syntax#conceal#blacklist=[]
let g:pandoc#formatting#mode='a'
let g:pandoc#formatting#textwidth=90
let g:pandoc#modules#disabled = ["formatting", "dashes", "yaml", "metadata"]

" }}}

" `lervag/wiki.vim` {{{

" Only load wiki.vim for wiki directory
let g:wiki_global_load=0
let g:wiki_root='~/.wiki'
let g:wiki_index_name='index'
let g:wiki_zotero_root='~/doc/Zotero'
let g:wiki_filetypes=['md']
let g:wiki_completion_case_sensitive=0

" Mappings
" Remap <leader>p

augroup filetype_pandoc
  autocmd!
  au BufRead,BufNewFile /home/h/.wiki/*.md nn <buffer><leader>p :WikiFzfPages<cr>
  au BufRead,BufNewFile /home/h/.wiki/*.md nnoremap <expr><buffer> sv empty(g:wiki#link#get()) ? ':vs<CR><c-w>w' : '<Plug>(wiki-link-follow-vsplit)'
  au BufRead,BufNewFile /home/h/.wiki/*.md nnoremap <expr><buffer> ss empty(g:wiki#link#get()) ? ':sp<CR><c-w>w' : '<Plug>(wiki-link-follow-split)'
augroup END

" If we are on a wiki link 

" let g:wiki_file_handlenmap r

" Links
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

" JS and TypeScript
let g:javascript_plugin_jsdoc = 1 " jsdoc syntax highlighting
let g:javascript_plugin_flow = 1 " flow syntax highlighting
let g:javascript_conceal_function = "ƒ"
let g:javascript_conceal_return = "⇖"
let g:svelte_indent_script = 0
let g:svelte_indent_style = 0

" JSONC (see https://github.com/neoclide/jsonc.vim/pull/9")
autocmd BufNewFile,BufRead */.vscode/*.json setlocal filetype=jsonc

" `.../quickscope` {{{

let g:qs_max_chars=80
let g:qs_lazy_highlight = 1

" }}}

" SuperCollider
au BufEnter,BufWinEnter,BufNewFile,BufRead *.sc,*.scd se filetype=supercollider
au Filetype supercollider packadd scvim

" Tidalcycles (sclang and vim-tidal)
let g:tidal_default_config = {"socket_name": "default", "target_pane": "tidal:1.1"}


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

au! bufwritepost .vimrc source $HOME/.vimrc

" TODO: separate to filetype specific files

" JS
" Jump between `=` and `;`
au FileType javascript set mps+==:;

" JSONC (see https://github.com/neoclide/jsonc.vim/pull/9")
au BufNewFile,BufRead */.vscode/*.json setlocal filetype=jsonc

" Hacky way to pass on active note to script for automated for HTML preview
au BufEnter /home/h/.wiki/*.md silent exe '!echo %:t > /home/h/.local/share/nvim/plugged/bro/current-page'

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

" Highlight todo's for every filetype
" https://stackoverflow.com/questions/11709965/vim-highlight-the-word-todo-for-every-filetype
augroup HiglightTODO
    autocmd!
    autocmd WinEnter,VimEnter * :silent! call matchadd('Todo', 'TODO', -1)
    autocmd WinEnter,VimEnter * :silent! call matchadd('Todo', 'FIXME', -1)
    autocmd WinEnter,VimEnter * :silent! call matchadd('Todo', 'QUESTION', -1)
augroup END
