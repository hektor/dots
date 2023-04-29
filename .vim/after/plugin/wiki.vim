" " Change local buffer to directory of current file after the plugin has loaded
autocmd VimEnter * lcd %:p:h

" " Override wiki index mapping to also cd into the wiki
nm <leader>ww <Plug>(wiki-index) \| :lcd %:p:h<cr>
