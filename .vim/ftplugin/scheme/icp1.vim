"
" Originally from:
" https://github.com/ward/dotfiles/blob/master/vim/ftplugin/scheme/icp1.vim
"

fu! s:OpenR5RS(path)
  let prelude = [
        \ "(#%require xrepl)",
        \ "(load \"" . a:path . "\")",
        \ "\<CR>"
        \ ]->join("")
  let s:term = term_start("plt-r5rs --no-prim", {"term_finish": "close", "vertical": 1})
  call term_sendkeys("", prelude)
endfu

fu! s:GetVisualSelection()
  try
    let a_save = @a
    normal! gv"ay
    return @a
  finally
    let @a = a_save
  endtry
endfu

fu! s:SendR5RSkeys()
  call term_sendkeys(s:term, s:GetVisualSelection())
endfu

nnoremap <buffer> <leader>o :call <sid>OpenR5RS(@%)<cr>
vnoremap <buffer> <leader>r <esc>:call <sid>SendR5RSkeys()<cr>
