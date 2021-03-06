set shell=/usr/bin/fish

" Attempt to determine the type of a file based on its name and possibly its
" contents. Use this to allow intelligent auto-indenting for each filetype,
" and for plugins that are filetype specific.
filetype indent plugin on
 
" Enable syntax highlighting
syntax on

"------------------------------------------------------------
" Must have options {{{1
"
" These are highly recommended options.
 
" Vim with default settings does not allow easy switching between multiple files
" in the same editor window. Users can use multiple split windows or multiple
" tab pages to edit multiple files, but it is still best to enable an option to
" allow easier switching between files.
"
" One such option is the 'hidden' option, which allows you to re-use the same
" window and switch from an unsaved buffer without saving it first. Also allows
" you to keep an undo history for multiple files when re-using the same window
" in this way. Note that using persistent undo also lets you undo in multiple
" files even in the same window, but is less efficient and is actually designed
" for keeping undo history after closing Vim entirely. Vim will complain if you
" try to quit without saving, and swap files will keep you safe if your computer
" crashes.
set hidden

" set runtime path
set runtimepath+=$HOME/.local/share/nvim

if has('persistent_undo')      "check if your vim version supports it
  set undofile                 "turn on the feature  
  set undodir=$HOME/.local/share/nvim/undo  "directory where the undo files will be stored
  endif
 
" Backup files
set backupdir=$HOME/.local/share/nvim/backup,.
set directory=$HOME/.local/share/nvim/backup,.

" Note that not everyone likes working this way (with the hidden option).
" Alternatives include using tabs or split windows instead of re-using the same
" window as mentioned above, and/or either of the following options:
" set confirm
" set autowriteall
 
" Better command-line completion
set wildmenu
 
" Show partial commands in the last line of the screen
set showcmd
 
" Highlight searches (use <C-L> to temporarily turn off highlighting; see the
" mapping of <C-L> below)
set hlsearch
 
" Modelines have historically been a source of security vulnerabilities. As
" such, it may be a good idea to disable them and use the securemodelines
" script, <http://www.vim.org/scripts/script.php?script_id=1876>.
" set nomodeline
 
 
"------------------------------------------------------------
" Usability options {{{1
"
" These are options that users frequently set in their .vimrc. Some of them
" change Vim's behaviour in ways which deviate from the true Vi way, but
" which are considered to add usability. Which, if any, of these options to
" use is very much a personal preference, but they are harmless.
 
" Use case insensitive search, except when using capital letters
set ignorecase
set smartcase

" syntax based code folding
autocmd Syntax c,cpp,vim,xml,html,xhtml,python,markdown,scala setlocal foldmethod=syntax
" unfold everything when a file is opened
autocmd Syntax c,cpp,vim,xml,html,xhtml,perl,python,markdown,scala normal zR

" Allow backspacing over autoindent, line breaks and start of insert action
set backspace=indent,eol,start
 
" When opening a new line and no filetype-specific indenting is enabled, keep
" the same indent as the line you're currently on. Useful for READMEs, etc.
set autoindent
 
" Stop certain movements from always going to the first character of a line.
" While this behaviour deviates from that of Vi, it does what most users
" coming from other editors would expect.
set nostartofline
 
" Display the cursor position on the last line of the screen or in the status
" line of a window
set ruler
 
" Always display the status line, even if only one window is displayed
set laststatus=2
 
" Instead of failing a command because of unsaved changes, instead raise a
" dialogue asking if you wish to save changed files.
set confirm
 
" Use visual bell instead of beeping when doing something wrong
set visualbell
 
" And reset the terminal code for the visual bell. If visualbell is set, and
" this line is also included, vim will neither flash nor beep. If visualbell
" is unset, this does nothing.
set t_vb=
 
" Enable use of the mouse for all modes
set mouse=a
 
" Set the command window height to 2 lines, to avoid many cases of having to
" "press <Enter> to continue"
set cmdheight=2
 
" Display relative line numbers on the left
set relativenumber
 
" Quickly time out on keycodes, but never time out on mappings
set notimeout ttimeout ttimeoutlen=200
 
" Use <F11> to toggle between 'paste' and 'nopaste'
set pastetoggle=<F11>
 
 
"------------------------------------------------------------
" Indentation options {{{1
"
" Indentation settings according to personal preference.
 
" Indentation settings for using 4 spaces instead of tabs.
" Do not change 'tabstop' from its default value of 8 with this setup.
set shiftwidth=4
set softtabstop=4
set expandtab
 
" Indentation settings for using hard tabs for indent. Display tabs as
" four characters wide.
"set shiftwidth=4
set tabstop=4
 
 
"------------------------------------------------------------
" Mappings {{{1
"
" Useful mappings
 
" Map Y to act like D and C, i.e. to yank until EOL, rather than act as yy,
" which is the default
map Y y$
 
" Map <C-L> (redraw screen) to also turn off search highlighting until the
" next search
nnoremap <C-L> :nohl<CR><C-L>

" Manage plugins with vim-plug
call plug#begin('~/.local/share/nvim/site/plugged')

" Clipboard
Plug 'vim-scripts/YankRing.vim'

" :make 
Plug 'benekastah/neomake'

" Autocomplete engine
Plug 'ervandew/supertab'
Plug 'Konfekt/FastFold' 
Plug 'Shougo/neoinclude.vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'zchee/deoplete-clang'
Plug 'zchee/deoplete-jedi'

" code navigation
Plug 'vim-scripts/taglist.vim'
Plug 'easymotion/vim-easymotion'

" Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" Github integration
Plug 'tpope/vim-fugitive'

" Language support
Plug 'derekwyatt/vim-scala'
Plug 'dag/vim-fish' 
Plug 'vim-utils/vim-man'
Plug 'critiqjo/lldb.nvim'
Plug 'kana/vim-operator-user'
Plug 'plasticboy/vim-markdown'
Plug 'h1mesuke/unite-outline'
Plug 'kana/vim-operator-user'
Plug 'reedes/vim-pencil'
Plug 'ajford/vimkivy'

" Theme
Plug 'morhetz/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'ryanoasis/vim-devicons'

" Fuzzy finder
Plug 'vim-scripts/mru.vim'
Plug 'kien/ctrlp.vim'
Plug 'mhinz/vim-grepper'

" Macros
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'

" Misc Tools 
Plug 'godlygeek/tabular'
Plug 'Chiel92/vim-autoformat'
Plug 'dhruvasagar/vim-table-mode'
call plug#end()

" Run neomake on every save
autocmd! BufWritePost * Neomake
let g:neomake_c_clang_args = ["-std=c++11", "-Wextra", "-Wall", "-fsanitize=undefined","-g", "-fopenmp"]
let g:neomake_cpp_clang_args = ["-std=c++1z", "-Wextra", "-Wall", "-fsanitize=undefined","-g", "-fopenmp"]
let g:neomake_python_enabled_makers = ['flake8']

" neocomplete like
set completeopt+=noinsert
" deoplete.nvim recommend
set completeopt+=noselect

" Use deoplete.
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#clang#libclang_path = '/usr/lib/libclang.so'
let g:deoplete#sources#clang#clang_header = '/usr/lib/clang'
let g:deoplete#sources#clang#std = {'c': 'c11', 'cpp': 'c++1z', 'objc': 'c11', 'objcpp': 'c++1z'}
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']
let g:deoplete#sources#go#use_cache = 1
let g:deoplete#sources#go#json_directory = '$HOME/.local/share/nvim/go/json'
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
let g:UltiSnipsSnippetsDir="~/.local/share/nvim/custom_snippets"
let g:UltiSnipsSnippetDirectories=["custom_snippets"]

" set colorscheme
set termguicolors
set background=dark
let g:gruvbox_italic=1
colorscheme gruvbox

" air-line enable powerline fonts
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_min_count = 2

" make tables markdown compatible
let g:table_mode_corner="|"

" yank to clipboard
set clipboard=unnamedplus

" autoformat options
au BufWrite * :Autoformat
let g:autoformat_autoindent = 0
let g:autoformat_retab = 0
let g:autoformat_remove_trailing_spaces = 0
let g:formatdef_cpp_google = '"astyle -A2 -xL -xC65"'
let g:formatters_c = ['cpp_google']
let g:formatters_cpp = ['cpp_google']

" grepper options
" for browsing the input history
cnoremap <c-n> <down>
cnoremap <c-p> <up>

nmap gs <plug>(GrepperOperator)
xmap gs <plug>(GrepperOperator)

nnoremap <leader>G :Grepper -tool git<cr>
nnoremap <leader>g :Grepper -tool pt<cr>
nnoremap <leader>* :Grepper -tool pt-cword -noprompt<cr>

let g:grepper = {
    \ 'tools':     ['pt', 'git', 'grep'],
    \ 'open':      0,
    \ 'jump':      1,
    \ 'next_tool': '<leader>g',
    \ }

" load and configure the devicons plugin 
let g:webdevicons_enable = 1
let g:webdevicons_enable_nerdtree = 1
let g:webdevicons_enable_airline_tabline = 1
let g:webdevicons_enable_airline_statusline = 1
let g:webdevicons_enable_ctrlp = 1
let g:webdevicons_enable_flagship_statusline = 1
let g:WebDevIconsUnicodeDecorateFileNodes = 1
let g:WebDevIconsUnicodeGlyphDoubleWidth = 1
let g:webdevicons_conceal_nerdtree_brackets = 1
let g:WebDevIconsNerdTreeAfterGlyphPadding = ' '
let g:WebDevIconsNerdTreeGitPluginForceVAlign = 1

" Some basic pencil configuration
augroup pencil
  autocmd!
  autocmd FileType markdown,mkd call pencil#init()
  autocmd FileType text         call pencil#init({'wrap': 'hard'})
augroup END

let g:airline_section_x = '%{PencilMode()}'

" Toggle pencil mode
nnoremap <leader>pm :PencilToggle<cr>

" Spell-check Markdown files
autocmd FileType markdown setlocal spell
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_toml_frontmatter = 1

" run kivy application
nnoremap <leader>kv :!python main.py<cr>
nnoremap <leader>tp :!tup<cr>
