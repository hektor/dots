function GetClozeNumber()
  let REGEX_CLOZE_START = '{{c\d\+::'

  " Find previous cloze match
  let prev_match = searchpos(REGEX_CLOZE_START, 'bW', line("gg"))
  " Only get number if cloze was matched (i.e. when cursor does not end up at
  " start of file)
  if max(prev_match) != 0
    " Get word under cursor (ignoring curly braces and `::`),
    " get the second character of the word and cast it to an int
    " add one for the next cloze number
    return str2nr(expand("<cword>")[1]) + 1
  else
    return 1
  endif
endfunction

" Wrap the next cloze number
function GetClozeStart()
  return "{{c" . GetClozeNumber() . "::"
endfunction

function GetClozeEnd()
  return "}}"
endfunction

let g:sandwich#recipes += [
      \   {
      \     'buns':     ['GetClozeStart()', 'GetClozeEnd()'],
      \     'expr'    : 1,
      \     'filetype': ['pandoc'],
      \     'input':    ['c'],
      \     'nesting':  1
      \   }
      \ ]
