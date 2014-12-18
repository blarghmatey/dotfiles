" Indentation
filetype plugin indent on

if has('vim_starting')
  set nocompatible               " Be iMproved

  " Required:
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" Required:
call neobundle#begin(expand('~/.vim/bundle/'))

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
NeoBundle 'othree/javascript-libraries-syntax.vim' "Syntax highlighting for various javascript libraries
"NeoBundle 'jmcantrell/vim-virtualenv'
NeoBundle 'klen/python-mode' " A collection of plugins useful for python development
NeoBundle 'gregsexton/MatchTag' " Highlight matching HTML tags when the cursor is inside the opening or closing tag
NeoBundle 'marijnh/tern_for_vim' " Javascript code intelligence using a node backend. After bundle install cd ~/.vim/bundles/tern_for_vim && npm install
NeoBundle 'mattn/emmet-vim' " Plugin for better HTML and CSS code intelligence
NeoBundle 'scrooloose/nerdcommenter' " Easily comment out lines or segments of code using key commands
NeoBundle 'mtth/scratch.vim' " Scratch buffer for taking notes or storing snippets during a session
NeoBundle 'tsaleh/vim-matchit'
NeoBundle 'vim-scripts/loremipsum'
NeoBundle 'blarghmatey/split-expander'
NeoBundle 'fs111/pydoc.vim'
NeoBundle 'sjl/gundo.vim'
NeoBundle 'saltstack/salt-vim'
NeoBundle 'kien/tabman.vim'
NeoBundle 'Rykka/colorv.vim'
NeoBundle 'mattn/webapi-vim'

call neobundle#end()

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
set softtabstop=4
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
set showcmd
set list listchars=tab:»·,trail:·
set wildmode=list,full
set autoread
set clipboard+=unnamed

" Command aliases
" Tab navigation
cabbrev tp tabprev
cabbrev tn tabnext
cabbrev tf tabfirst
cabbrev tl tablast
" Go to next/previous buffer and close current buffer
cabbrev bdp bp<BAR>bd#
cabbrev bdn bn<BAR>bd#


" Package configurations
let g:airline_powerline_fonts=1
let g:neobundle#types#git#default_protocol = 'ssh'
let g:neocomplcache_enable_at_startup = 1
let g:used_javascript_libs = 'jquery,angularjs'
let g:pymode_rope_completion = 0
let g:pymode_virtualenv = 1
let g:pymode_folding = 0
let g:syntastic_check_on_open = 1
let g:ctrlp_use_caching = 0
let g:syntastic_aggregate_errors = 1
let g:use_emmet_complete_tag = 1
let g:airline#extensions#tabline#enabled=1
let g:tabman_toggle = '<leader>tt'
let g:tabman_focus = '<leader>tf'
let g:ycm_collect_identifiers_from_tags_files = 1
let g:ycm_seed_identifiers_with_syntax = 1
let g:ycm_add_preview_to_completeopt = 1
let g:session_autosave_periodic = 5
let g:ycm_semantic_triggers =  {
\   'c' : ['->', '.'],
\   'objc' : ['->', '.'],
\   'ocaml' : ['.', '#'],
\   'cpp,objcpp' : ['->', '.', '::'],
\   'perl' : ['->'],
\   'php' : ['->', '::'],
\   'cs,java,javascript,d,vim,python,perl6,scala,vb,elixir,go' : ['.'],
\   'ruby' : ['.', '::'],
\   'lua' : ['.', ':'],
\   'erlang' : [':'],
\   'css' : [':', ': ', ' '],
\ }

" Platform specific stuff goes here
if has("unix")
  let s:uname = system("uname")
  if s:uname == "Darwin\n"
    let g:ycm_path_to_python_interpreter = '/usr/bin/python'
  endif
endif

"colorscheme ir_black
"set background=dark

" Filetype conditional settings
augroup vimrc
    autocmd!
    au FileType python setlocal colorcolumn=80
    au FileType eruby setlocal tabstop=2 |
                \    setlocal shiftwidth=2 |
                \    setlocal softtabstop=2
    au FileType ruby setlocal tabstop=2 |
                \    setlocal shiftwidth=2 |
                \    setlocal softtabstop=2
    au FileType javascript let b:javascript_lib_use_jquery = 1
    au FileType javascript let b:javascript_lib_use_angularjs = 1
augroup end

" My Key Mappings
:map ' <leader>
" Window split navigation made easier
:map <leader>h <C-W>h
:map <leader>j <C-W>j
:map <leader>k <C-W>k
:map <leader>l <C-W>l
" Quickly open a scratch buffer
:map <leader>s :Scratch<CR>
:map <leader>S :ScratchSelection<CR>
:nmap <leader>sd :%s/\s\+$//g<CR> " Delete trailing whitespace from file


" Abbreviations
"
"
" Function definitions
"
function! SwapPaneBuffers(direction)
    let l:bufferOne = bufnr(0)
endfunc

function! SwapPaneOrientation()
endfunc
