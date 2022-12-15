# For compiling in elpa

.PHONY : all force

all : byte-compile autoloads

ifeq ($(shell uname),Linux)
EMACS_EXE ?= emacs

else ifeq ($(shell uname),Darwin)
EMACS_EXE ?= "/Applications/Emacs.app/Contents/MacOS/Emacs"

else
# windows
# specify uniscribe to workaround weird Windows harfbuzz bug
EMACS_EXE ?= emacs -xrm Emacs.fontBackend:uniscribe

endif

BYTE_COMPILE := "(progn (setq byte-compile-error-on-warn t)(batch-byte-compile))"
byte-compile : byte-compile-clean
	$(EMACS_EXE) -Q -batch -L . --eval $(BYTE_COMPILE) *.el

byte-compile-clean :
	rm -f *.elc

autoloads : force
	$(EMACS_EXE) -Q -batch --eval "(progn (setq generated-autoload-file (expand-file-name \"autoloads.el\"))(update-directory-autoloads \".\"))"


# end of file
