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

" Filetype conditional settings
au FileType python set colorcolumn=80
au FileType ruby set tabstop=2
"    set shiftwidth=2
"
color blacklight
