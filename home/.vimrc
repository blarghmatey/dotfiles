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
NeoBundle 'kien/ctrlp.vim' "Fuzzy text search for files, buffers
NeoBundle 'sheerun/vim-polyglot' "Multiple language syntax highlighting
NeoBundle 'scrooloose/nerdtree' "File tree navigation
NeoBundle 'fholgado/minibufexpl.vim' "Buffer navigation and management
NeoBundle 'tpope/vim-surround' "Manage quotes, braces, etc around text
NeoBundle 'flazz/vim-colorschemes' "Repository of color schemes for vim
NeoBundle 'lepture/vim-jinja' "Jinja template syntax highlighting
NeoBundle 'bling/vim-airline' "Fancy status line for vim
"NeoBundle 'shougo/neocomplcache' "Code auto completion
"NeoBundle 'shougo/unite.vim'
NeoBundle 'majutsushi/tagbar' "Display and navigation of tags for a given buffer
NeoBundle 'terryma/vim-multiple-cursors' "Multiple cursor selection like sublime
NeoBundle 'tpope/vim-rails' "Syntax highlighting and project navigation for rails
"NeoBundle 'davidhalter/jedi-vim' "High accuracy python auto complete
NeoBundle 'scrooloose/syntastic' "Polyglot linting
NeoBundle 'phleet/vim-mercenary' "Mercurial integration
NeoBundle 'rking/ag.vim' "Fast file grepping, requires the silver searcher (ag) installed on the system
NeoBundle 'jiangmiao/auto-pairs' "Auto close quotes, parentheses, braces, etc.
NeoBundle 'tpope/vim-fugitive' "Git integration
NeoBundle 'Valloric/YouCompleteMe' "Multi-language code completion, requires executing install.sh after download
NeoBundle 'xolox/vim-session', {'depends': 'xolox/vim-misc'} "Automatic session management
NeoBundle 'https://bitbucket.org/ns9tks/vim-fuzzyfinder', {'depends': 'https://bitbucket.org/ns9tks/vim-l9'}
NeoBundle 'othree/javascript-libraries-syntax.vim'
""NeoBundle 'jmcantrell/vim-virtualenv'
"NeoBundle 'klen/python-mode'
NeoBundle 'gregsexton/MatchTag'
NeoBundle 'marijnh/tern_for_vim'
NeoBundle 'mattn/emmet-vim'
NeoBundle 'scrooloose/nerdcommenter'


" Other directives
"
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
set t_Co=256 "Tell vim that it's running with 256 colors
set laststatus=2 "Tell vim-airline to always display
set backspace=indent,start "Allow for backspacing over auto-indent
set incsearch "Jump to search matches as they are being typed
set history=100 " Keep more command history
set splitbelow
set splitright
set ignorecase
set smartcase

" Command aliases
cabbrev tp tabprev
cabbrev tn tabnext
cabbrev tf tabfirst
cabbrev tl tablast


" Package configurations
let g:airline_powerline_fonts=1
let g:neobundle#types#git#default_protocol = 'ssh'
"let g:neocomplcache_enable_at_startup = 1
let g:used_javascript_libs = 'jquery,angularjs'
let g:pymode_rope_completion = 0
let g:pymode_virtualenv = 1
let g:pymode_folding = 0
let g:syntastic_check_on_open = 1
let g:ctrlp_use_caching = 0
let g:syntastic_aggregate_errors = 1

" Platoform specific stuff goes here
if has("unix")
  let s:uname = system("uname")
  if s:uname == "Darwin\n"
    let g:ycm_path_to_python_interpreter = '/usr/bin/python'
  endif
endif

colorscheme ir_black
set background=dark

" Filetype conditional settings
"augroup vimrc
"    autocmd!
"    au BufRead,BufNewFile *.py set colorcolumn=80
"    au BufLeave *.py set colorcolumn&
"    au BufRead,BufNewFile *.rb set tabstop=2 |
"                \    set shiftwidth=2
"    au BufRead,BufNewFile *.js let b:javascript_lib_use_jquery = 1
"    au BufRead,BufNewFile *.js let b:javascript_lib_use_angularjs = 1
"augroup end

" My Key Mappings
:map <leader>h <C-W>h
:map <leader>j <C-W>j
:map <leader>k <C-W>k
:map <leader>l <C-W>l
:nmap ' <leader>

" Abbreviations
"
"
" Function definitions

