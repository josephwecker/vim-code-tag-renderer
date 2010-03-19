# Vctr (Victor)
## Vim Code Tag Renderer

**Author:** Joseph Wecker <joseph.wecker@gmail.com>

**Copyright:** 2010 Joseph Wecker

**License:** [MIT License](http://www.opensource.org/licenses/mit-license.php)

Quick utility for utilizing vim's built in "TOhtml" functionality in any
arbitrary file of yours.

Given any text file or stdin input, this program will scan it for any
<code...></code> tags with 'lang="..."' attributes.  When it finds any it will
replace it with xhtml that looks exactly like whatever it looks like in your
locally installed gvim.  Whatever is in the 'lang' tag attribute is used as the
filetype.  The xhtml output for each code block is a div with the class
"code."- so have at it.  Keep in mind that if you have line numbers turned on
in vim they'll show up here as well, making copying / pasting by viewers a
little problematic.

It stores code snippets in a ./.tmp_vctr_cache directory so that if you are
running it in short iterations it won't have to regenerate every single snippet
(which can take long when launching gvim on some platforms).  It requires gvim
(although you can possibly get it to work with vim).

Reads from stdin if no file specified. Output goes to stdout. If multiple files
are specified they still go to stdout (effectively concatenating them).  Pulls
it all into memory right now because I'm lazy.

### Example

#### Original code
(I pump this documentation through vctr.  In order to have it _not_ render this
code below, I removed the quotes from around "erlang")

	<code lang=erlang>
	-module(factorial).
	-export([factorial/1]).
	
	factorial(0) -> 1.
	factorial(N) ->
		N * factorial(N-1).
	</code>

#### Command to run

<code lang="bash">
./vctr < inputfile.txt > outputfile.html
</code>

#### Rendered (on my machine)
(Note: in github etc. this ends up getting sanitized- if that's where you're
reading this, you'll have to trust that it looks awesome)

<code lang="erlang">
-module(factorial).
-export([factorial/1]).

factorial(0) -> 1.
factorial(N) ->
    N * factorial(N-1).
</code>


