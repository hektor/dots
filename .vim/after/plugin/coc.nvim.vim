if !exists("g:did_coc_loaded") | echo "`coc.nvim` not loaded (not configuring)" | finish | endif

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
  \'coc-bibtex',
  \'coc-vimlsp',
  \'coc-diagnostic',
  \'coc-git'
  \]

"
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

" inor <silent><expr> <c-j> coc#pum#visible() ? coc#pum#next(1) : coc#refresh()
inor <silent><expr> <c-space> coc#refresh()
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

vmap <tab> <plug>(coc-snippets-select)

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
nm <leader>do <plug>(coc-codeaction)
" Commands
com! -nargs=0 Prettier :CocCommand prettier.formatFile
