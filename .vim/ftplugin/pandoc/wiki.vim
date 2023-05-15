set cc=81
set cocu=""
func! GetContext()
  " https://stackoverflow.com/questions/9464844/how-to-get-group-name-of-highlighting-under-cursor-in-vim
  if !exists("*synstack")
    return
  endif
  let matches = map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
  if index(matches, 'pandocLaTeXInlineMath') >= 0
    echo 'math_inline'
  elseif index(matches, 'pandocLaTeXMathBlock') >= 0
    echo 'math_block'
  else
    echo ''
  endif
endfunc
com! -nargs=0 GetContext :call GetContext()

