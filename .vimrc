if has('mac') && ($TERM == 'xterm-256color' || $TERM == 'screen-256color')
  map <Esc>OP <F1>
  map <Esc>OQ <F2>
  map <Esc>OR <F3>
  map <Esc>OS <F4>
  map <Esc>[16~ <F5>
  map <Esc>[17~ <F6>
  map <Esc>[18~ <F7>
  map <Esc>[19~ <F8>
  map <Esc>[20~ <F9>
  map <Esc>[21~ <F10>
  map <Esc>[23~ <F11>
  map <Esc>[24~ <F12>
endif

set hls
set is
set cb=unnamed
set tabstop=8 
set softtabstop=0 
set expandtab 
set shiftwidth=4 
set smarttab
set ts=4
set sw=4
set si
set noswapfile
set nobackup
set noundofile
se autochdir
set backspace=indent,eol,start

inoremap { {}<Left>
inoremap {<CR> {<CR>}<Esc>O
inoremap {{ {
inoremap {} {}
inoremap jk <Esc>

imap <C-BS> <C-W>

autocmd BufWritePre *.cpp :normal gg=G
autocmd BufWritePre *.c :normal gg=G
autocmd filetype cpp nnoremap <F9> :w <bar> !g++ -std=c++17 % -o %:r -Wl,--stack,268435456<CR>
autocmd filetype cpp nnoremap <F10> :!%:r<CR>
autocmd filetype cpp nnoremap <C-C> :s/^\(\s*\)/\1\/\/<CR> :s/^\(\s*\)\/\/\/\//\1<CR> $ 
autocmd FileType c,cpp,java,php autocmd BufWritePre <buffer> %s/\s\+$//e

set nu
augroup numbertoggle
    autocmd!
    autocmd BufEnter,FocusGained,InsertLeave * set rnu
    autocmd BufLeave,FocusLost,InsertEnter * set nornu
augroup END
