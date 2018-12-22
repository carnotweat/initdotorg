My  init.el for future ref

<code>(require 'org)

(require 'ob-tangle)

(org-babel-load-file (expand-file-name "~/.emacs.d/myemacs.org"))

</code>
This structure is helpful  in quickly using packages like outshine , which tell you to modify your initdotorg.

I can list more advantages like [list2graph](https://raw.githubusercontent.com/carnotweat/initdotorg/master/pack/list2graph.org), which by

the way was my way to see whatever little knowledge I have about data structures.

All I d do was to change the # of  *s in the index of the top list.

Another example: using [xorshift](https://github.com/syohex/emacs-xorshift)  to detect differential phase between 2 signals in FPGA
and LVSD in general. As, a simple #xor gate between the two signals will create a phase difference signal proportional to the phase difference.
Next steps : Can bus sniffing on *nix systems


Upside

More [freedome](https://twitter.com/nonsameer/status/1067498112862711808)

Less [Restrictions](https://twitter.com/nonsameer/status/1067296037616812032)


Cutting it short, this seems to be enough for the gist.

To Do : Automating the computation of find and replace from user input, by scripting.
