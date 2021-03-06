set nocompatible              " be iMproved, required
filetype off                  " required
set shell=bash

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-sleuth'
Plugin 'Valloric/YouCompleteMe'
Plugin 'godlygeek/csapprox'
Plugin 'vim-scripts/a.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'bling/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/nerdcommenter'
Plugin 'joonty/vdebug.git'
Plugin 'tpope/vim-fugitive'
Plugin 'tfnico/vim-gradle'
Plugin 'dag/vim-fish'
Plugin 'eagletmt/neco-ghc.git'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'jez/vim-superman'
Plugin 'kana/vim-operator-user'
Plugin 'rhysd/vim-clang-format'
Plugin 'LaTeX-Box-Team/LaTeX-Box'
Plugin 'tpope/vim-surround'
Plugin 'junegunn/vim-easy-align'
Plugin 'rust-lang/rust.vim'
" Plugin 'racer-rust/vim-racer'
Plugin 'Raimondi/delimitMate'
Plugin 'altercation/vim-colors-solarized'
Plugin 'nanotech/jellybeans.vim'
Plugin 'Shougo/vimproc.vim.git'
Plugin 'eagletmt/ghcmod-vim.git'
Plugin 'Twinside/vim-hoogle'
Plugin 'lyuts/vim-rtags'
Plugin 'chiel92/vim-autoformat'
Plugin 'CoatiSoftware/vim-sourcetrail'

call vundle#end()            " required
filetype plugin indent on    " required

set encoding=utf-8
set t_Co=256
set number
set relativenumber
set nocompatible
set visualbell
set expandtab
set shiftwidth=4
set softtabstop=4
set backspace=2
set smartindent
set smarttab
set mouse=a
if has('mouse_sgr')
  set ttymouse=sgr
endif
set hidden
set cinoptions=:0,g0,t0,N-s
set scrolloff=15
set nostartofline
set list
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
" For tmux
set t_ut=
let g:solarized_termcolors=256
let g:jellybeans_use_term_background_color=0
set background=light
colorscheme jellybeans

let mapleader = ","
let maplocalleader = "\\"

" Man
" runtime ftplugin/man.vim
" nnoremap K :Man <cword><CR>
" eclim
let g:EclimCompletionMethod = 'omnifunc'

" No swap files in working directories
set backupdir=~/.vim/backup//
set directory=~/.vim/swap//
set undodir=~/.vim/undo//
set noswapfile

" Autoformat
nnoremap <leader>= :Autoformat<CR>

" Searching
set showmatch
set hlsearch
"nnoremap <esc> :noh<CR>
nnoremap n nzzzv
nnoremap N Nzzzv
nnoremap <silent> <leader>/ :execute 'vimgrep /'.@/.'/g %'<CR>:copen<CR>

" Resizing
autocmd VimResized * exe "normal! \<c-w>="

" Folding options for marker
function! FoldMark()
    set foldmethod=marker
    set foldcolumn=3
endfunction

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Enable heavy omni completion.
if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif
"let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
"let g:neocomplete#sources#omni#input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
"let g:neocomplete#sources#omni#input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'

" For perlomni.vim setting.
" https://github.com/c9s/perlomni.vim
let g:neocomplete#sources#omni#input_patterns.perl = '\h\w*->\h\w*\|\h\w*::'


" Latex-Box
function! LatexFolds()
    let thisline = getline(v:lnum)
    if match(thisline, '\\chapter') >= 0
        return ">1"
    elseif match(thisline, '\\section') >= 0
        return ">2"
    elseif match(thisline, '\\subsection') >= 0
        return ">3"
    elseif match(thisline, '\\subsubsection') >= 0
        return ">4"
    else
        return "="
    endif
endfunction

function! LatexOptions()
    setlocal foldmethod=expr
    setlocal foldcolumn=4
    "setlocal foldexpr=LatexFolds()
endfunction

let g:LatexBox_Folding=1
let g:LatexBox_latexmk_async = 1
let g:LatexBox_viewer = "open -a Skim"
let g:Tex_ViewRule_pdf = 'Skim'
map <silent> <localleader>ls :silent !/Applications/Skim.app/Contents/SharedSupport/displayline
    \ <C-R>=line('.')<CR> "<C-R>=LatexBox_GetOutputFile()<CR>" "%:p" <CR>
augroup LaTeX
    autocmd FileType tex setlocal spell spelllang=en_us
    autocmd FileType tex setlocal textwidth=80
    autocmd FileType tex inoremap<buffer> " ``''<left><left>
    autocmd FileType tex call LatexOptions()
augroup END

" Add header to new files
" autocmd BufNewFile *.cpp so ~/.home/header.txt
" autocmd BufNewFile *.c so ~/.home/header.txt
" autocmd BufNewFile *.h so ~/.home/header.txt
" autocmd BufNewFile *.hpp so ~/.home/header.txt
" autocmd BufNewFile *.java so ~/.home/header.txt

" Automatically close brackets
" inoremap ( ()<left>
" inoremap { {}<left>
" inoremap [ []<left>
" inoremap () ()
" inoremap {} {}
" inoremap [] []
" inoremap (<cr> (<cr>)<esc>O
" inoremap {<cr> {<cr>}<esc>O
" inoremap [<cr> [<cr>]<esc>O
" inoremap {<space> {<space> <space>}<left><left>
" inoremap (<space> (<space> <space>)<left><left>
" inoremap [<space> [<space> <space>]<left><left>

" Font
if !has('nvim')
  set guifont=Droid\ Sans\ Mono\ for\ Powerline:h15
endif

" Rust
let $RUST_SRC_PATH="/Users/mpilman/Projects/rust/src/"

" status line
set laststatus=2

" To easier work with buffers
function! DelCurrBuffer()
    let bufNr = bufnr('%')
    bprevious
    execute 'bdelete' bufNr 
endfunction
nnoremap <leader>d :call DelCurrBuffer()<CR>
nnoremap <silent> <tab> :bnext<CR>
nnoremap <silent> <s-tab> :bprevious<CR>

" Navigate easier through split windows
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Useful text mappings
nnoremap - ddkP
nnoremap _ ddp
nnoremap <leader>( %x``x

" For fast editing vimrc file
nnoremap <leader>ev :vsplit $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>

" Use jk to go out of insert mode
inoremap jk <ESC>

" Navigation
inoremap <C-h> <left>
inoremap <C-j> <down>
inoremap <C-k> <up>
inoremap <C-l> <right>
nnoremap L $
nnoremap H ^
vnoremap L $
vnoremap H ^
nnoremap <UP> <NOP>
nnoremap <DOWN> <NOP>
nnoremap <LEFT> <NOP>
nnoremap <RIGHT> <NOP>
nnoremap D d$

" Haskell
let g:haskellmode_completion_ghc = 0
let g:necoghc_enable_detailed_browse = 1
augroup HaskellGroup
    autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
    autocmd FileType haskell nnoremap <localleader>l :GhcModLint<CR>
    autocmd FileType haskell nnoremap <localleader>c :GhcModCheck<CR>
    autocmd FileType haskell nnoremap <localleader>t :GhcModType<CR>
    autocmd FileType haskell nnoremap <localleader>n :GhcModTypeClear<CR>
    autocmd FileType haskell nnoremap <localleader>s :GhcModSigCodegen<CR>
    autocmd FileType haskell set tags=tags;/,codex.tags;/
augroup END

" Python special case
autocmd FileType python setlocal expandtab shiftwidth=4 tabstop=4 softtabstop=4
filetype plugin on
filetype indent on
syntax on

set shellslash
set grepprg=grep\ -nH\ $*

nnoremap ,cd :cd %:p:h<CR>

" CTRL-P
let g:ctrlp_working_path_mode = 'rw'
nnoremap <leader>v :CtrlPMRUFiles<CR>
nnoremap <leader>b :CtrlPBuffer<CR>
" Airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
" Neco-ghc
augroup Haskell
  autocmd FileType *.hs setlocal omnifunc=necoghc#omnifunc
augroup END

" SourceTrail
let g:sourcetrail_autostart = 1
nnoremap <leader>as :SourcetrailRefresh<CR>
nnoremap <leader>aa :SourcetrailActivateToken<CR>

" Rtags


" YCM
" let g:ycm_path_to_python_interpreter = '/home/vagrant/bin/python'
" let g:ycm_autoclose_preview_window_after_completion = 1
augroup CloseAutocompletePreview
    autocmd InsertLeave *.hpp,*.py :pclose
    autocmd InsertLeave *.h,*.py :pclose
    autocmd InsertLeave *.cpp,*.py :pclose
augroup END
" let g:ycm_extra_conf_vim_data = ['getcwd()']
" let g:ycm_global_ycm_extra_conf = '~/.vim/ycm_extra_conf.py'
let g:ycm_semantic_triggers = {'haskell' : ['.']}
let g:ycm_filetype_blacklist = {'tex' : 1}
nnoremap <leader>f :YcmCompleter FixIt<CR>
nnoremap <leader>g :YcmCompleter GoTo<CR>
nnoremap <leader>t :YcmCompleter GetType<CR>

" Clamp
if has('nvim')
  let g:chromatica#libclang_path = '/Applications/Xcode.app//Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib'
  let g:chromatica#enable_at_startup=1
endif

" Configure make to work with cmake
function! s:currentDirName()
    let l:cwd = getcwd()
    return split(l:cwd, "/")[-1]
endfunction
function! s:createNinjaCmd()
    let l:dirname = s:currentDirName()
    return "setlocal makeprg=ninja\\ -C\\ ../builddirs/" . l:dirname
endfunction
augroup cppmake
    autocmd!
    autocmd FileType cpp execute s:createNinjaCmd()
augroup END
