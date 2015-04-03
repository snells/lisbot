lisbot
======
Work still in progress.

Command line tool to download packages from xdcc on irc.


<h2>Dependencies</h2>


<a href="www.sbcl.org">sbcl</a>

<a href="http://www.quicklisp.org">quicklisp</a>

boardeaux-threads

usocket

cl-ppcre

cl-fad

getopt

date-calc



<h2>Install</h2>


git clone this repo and move it to your quicklisp local-projects directory

to compile start sbcl and type

(ql:quickload :lisbot) (save-lisp-and-die "lisbot" :executable t :toplevel #'lisbot:main)


alternatively you can run it with bash script

\#!/bin/bash
sbcl --eval '(ql:quickload :lisbot)' --eval '(lisbot:main)' $@


<h2>Usage</h2>


lisbot [options] [server] [xdcc-bot-name] [packages...]

examples


lisbot irc.rizon.net my-xdcc-bot '#100' list '#4'

lisbot --directory /home/dir/ irc.rizon.net my-bot '#100' 

lisbot --create 21 3 2014 1 13 irc.rizon.net my-xdcc-bot some search terms

lisbot --file /home/user/my-generated-file

lisbot --search [optional file] search terms

lisbot --get server xdcc-bot

lisbot -a t server xdcc-bot '#100' '#101'


-h --help           prints this message

-v --verbose        prints servers messages

-c --create         Creates schedule 

-f --file           Downloads from schedule defaults to .lisbot/schedule

-d --directory      specify directory where to download, defaults to home directory

-p --password       If server requires password

-s --search         Prints out all matching lines from packlist, file defaults to ~/.lisbot/packlist

-g --get            Download packlist from xdcc bot to ~/.lisbot/packlist

-a --all            If set, lisbot will download every package simultaneously   


Creating schedule

Start with date in form day month year,

first-episode-number, how-many-times, server, bot search-terms

Will download everything where all search-terms match line in packlist.


<h2>Issues</h2>


Does not detect when you are put on queue.    
When you try to download multiple packages the bots packlist might be updated and you will get wrong package.
