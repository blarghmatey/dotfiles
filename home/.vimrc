" Indentation
filetype plugin indent on

if has('vim_starting')
  set nocompatible               " Be iMproved

  " Required:
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" Required:
call neobundle#rc(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck

" My list of bundles here:
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'sheerun/vim-polyglot'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'fholgado/minibufexpl.vim'
NeoBundle 'tpope/vim-surround'
NeoBundle 'flazz/vim-colorschemes'
NeoBundle 'lepture/vim-jinja'
NeoBundle 'bling/vim-airline'
NeoBundle 'shougo/neocomplcache'
NeoBundle 'shougo/unite.vim'
NeoBundle 'majutsushi/tagbar'
NeoBundle 'terryma/vim-multiple-cursors'
NeoBundle 'tpope/vim-rails'
NeoBundle 'davidhalter/jedi-vim'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'phleet/vim-mercenary'
NeoBundle 'mileszs/ack.vim'

" Other directives
" Persistent undo
set undofile
set undodir=~/.vim/undo

syntax enable
set ruler
set number
set tabstop=4
set shiftwidth=4
set expandtab
set smartindent
set t_Co=256
set laststatus=2
set backspace=indent,start

let g:airline_powerline_fonts=1
let g:neocomplcache_enable_at_startup = 1

" Filetype conditional settings
au FileType python set colorcolumn=80
au FileType ruby set tabstop=2
"    set shiftwidth=2
"
color blacklight

" My Key Mappings
:map <leader>h <C-W>h
:map <leader>j <C-W>j
:map <leader>k <C-W>k
:map <leader>l <C-W>l
