function __fish_complete_setfont
    ghostty +list-fonts | grep -v '^[[:blank:]]' | grep -v '^$' | sort | uniq
end

complete -f -c setfont -a '(__fish_complete_setfont)'

complete -f -c setfont -l list -s l -d 'List all the available fonts'
complete -f -c setfont -l help -s h -d 'Show the help message'
