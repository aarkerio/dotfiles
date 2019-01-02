# Aliases (variables)
alias potle='ssh -l www-data chipotle-software.com -p 7822'
alias APT='sudo apt update && sudo apt full-upgrade -q -y'
alias ACS='apt search'
alias SRS='apt show'
alias AWS='ssh -i "/home/manuel/Documents/backups/aws-chipotle.pem" ubuntu@ec2-34-209-15-124.us-west-2.compute.amazonaws.com'
alias APTI='sudo apt install'
alias SHU='sudo shutdown -h now'
alias rm="/bin/rm -i"
alias gh="history | grep"
alias RC="RAILS_ENV=development bin/bundle exec bin/rails db:drop db:create db:migrate db:seed"
alias spr="RAILS_ENV=test bin/bundle exec bin/spring rspec "
alias rr="RAILS_ENV=development bin/bundle exec bin/rails"
alias capistrano="cap staging2 deploy branch="
alias ll="ls -l"
alias HD="sudo hdparm -B 254 /dev/sda"
alias CH="cd /home/manuel/entwicklung/chipotle/"
alias RO="cd /home/manuel/Dokumente/personal/Schriftstellerei"
alias HO="cd /home/manuel/entwicklung/fondeadora/homie"
alias VET="psql vet4pet_development -U vet4pet"
alias s2="cd /home/manuel/entwicklung/chipotle/schnell && tmux new-session -d 'teamocil schnell' \; attach"
alias z3="cd /home/manuel/entwicklung/chipotle/lisp/ZentaurLMS && tmux new-session -d 'teamocil zentaur' \; attach"
alias EM="emacsclient -t"
alias enw="emacs -nw"
alias semc="sudo emacs -nw"
# Git stuff
alias gp='git push'
alias gl='git log'
alias gs='git status -u'
alias gfles='git diff --name-status master | more'
alias gd='git diff'
alias gdc='git diff --cached'
alias gma='git commit -am'
alias gitlog='git log --pretty=format:"%h%x09%an%x09%ad%x09%s"'
alias gitcount="git shortlog -s -n | grep aarkerio"
alias gst="git status -sb"
alias PD="pdflatex -interaction=nonstopmode"

#------------------------------------------------------------------------------
# Useful aliases to save some typing.
#------------------------------------------------------------------------------
alias cls='clear'
alias l='ls -lA'
alias lsr='ls -lSr'
alias md='mkdir'
alias rd='rmdir'
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../..'

alias my='mysql -u root -p'
alias ff='find . -type f -exec chmod 644 {} \;'
alias fd='find . -type d -exec chmod 755 {} \;'
alias checkpo='for i in *.po; do echo -n "$i: " ; msgfmt --statistics $i ; done'

