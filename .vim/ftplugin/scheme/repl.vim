"
" Source: https://wiki.call-cc.org/vim / Jonathan Palardy
"

" Note, this is assuming you have an R5RS Scheme REPL runnning in the second
" TMUX pane.

" Send `(load <current-file>)` to tmux pane
nmap <leader>rf  :call Scheme_send_sexp("(load \"" . expand("%:p") . "\")\n")<cr>
nmap <leader>re :call Scheme_eval_defun()<cr>

" Send s-expression to TMUX buffer
fun! Scheme_send_sexp(sexp)
    let ss = escape(a:sexp, '\"')
    " Send to second tmux pane
    call system("tmux send-keys -t 1 \"" . ss . "\n\"")
endfun

" Send s-expression under cursor to TMUX buffer
fun! Scheme_eval_defun()
    let pos = getpos('.')
    silent! exec "normal! 99[(yab"
    call Scheme_send_sexp(@")
    call setpos('.', pos)
endfun
