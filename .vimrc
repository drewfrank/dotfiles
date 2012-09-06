set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
" List bundles to install below. Format:
"   Github: 'user/reponame'
"   Vimscripts: 'scriptname'
"   Git repo: 'git://blah'
Bundle 'AutoTag'
Bundle 'mileszs/ack.vim'
Bundle 'noahfrederick/Hemisu'
Bundle 'JuliaLang/julia-vim'
Bundle 'scrooloose/nerdcommenter'
Bundle 'michaeljsmith/vim-indent-object'
Bundle 'tpope/vim-repeat'
Bundle 'ervandew/supertab'
Bundle 'tpope/vim-surround'
Bundle 'taglist.vim'
Bundle 'chriskempson/vim-tomorrow-theme'
Bundle 'ivanov/vim-ipython'
Bundle 'git://vim-latex.git.sourceforge.net/gitroot/vim-latex/vim-latex'

filetype plugin indent on " required

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

set history=1000
set ruler        " show the cursor position all the time
set showcmd      " display incomplete commands
set incsearch    " do incremental searching

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
  colorscheme elflord
endif

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
          \ | wincmd p | diffthis
endif

" vim behavior
set hidden
set encoding=utf-8
set termencoding=utf-8
set scrolloff=3
set showmode
set showcmd
set wildmenu
set wildmode=list:longest
set wildignore=.svn,.git,.hg,*.o,*.a,*.class,*.so,*.swp,*.jpg,*.png,*.gif
set laststatus=2
set number
set ignorecase
set smartcase
let mapleader=","
set vb t_vb=
set t_Co=256
set title
set autoread
set report=0

" editing preferences
set autoindent
set linebreak
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set gdefault
set foldmethod=indent
set foldlevelstart=99
set foldignore=
autocmd FileType tex setlocal iskeyword=@,48-57,_,-,:,192-255 " For easy autocompletion of label names.
autocmd FileType r setlocal ts=2 sts=2 sw=2 " Use 2-space indents like DuBois.

" handy keybindings
inoremap jj <ESC>
nnoremap <leader><space> :noh<cr>
nnoremap <space> za
vnoremap <space> zf
nnoremap <silent> <leader>s :TlistToggle<cr>

" latex-suite options
set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'
let g:Tex_ViewRule_pdf='evince'
let g:Tex_DefaultTargetFormat='pdf'
let g:Tex_MultipleCompileFormats='pdf,dvi'

" EnhancedCommentify options
let g:EnhCommentifyPretty='Yes'
let g:EnhCommentifyUserMode='Yes'
let g:EnhCommentifyBindInInsert='No'

" NERDCommenter options
let NERDCommentWholeLinesInVMode=1
let NERDSpaceDelims=1

" SuperTab options
let g:SuperTabDefaultCompletionType = "context"
let g:SuperTabContextDefaultCompletionType = "<c-p>"

" Taglist options
"let Tlist_GainFocus_On_ToggleOpen=1
" let Tlist_Auto_Open=1
"let Tlist_Close_On_Select=1
let Tlist_Exit_OnlyWindow=1
let Tlist_WinWidth=40
autocmd FileType taglist setlocal nonumber statusline=\ 
let tlist_tex_settings = 'latex;l:labels;s:sections;t:subsections;u:subsubsections'

function Stamp(...)
    normal! `["_d`]"_xhp
endfunction
nnoremap <silent> <C-I> :set opfunc=Stamp<CR>g@
