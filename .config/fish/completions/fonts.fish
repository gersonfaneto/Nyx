function __fish_complete_fonts
    ghostty +list-fonts | grep -v '^[[:blank:]]' | grep -v '^$' | sort | uniq
end

complete -f -c fonts -a '(__fish_complete_fonts)'

complete -f -c fonts -l list -s l -d 'List all the available fonts'
complete -f -c fonts -l help -s h -d 'Show the help message'
