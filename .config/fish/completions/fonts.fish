function __fish_complete_fonts
    fonts --list --filter | sed 's/\x1b\[[0-9;]*m//g' | sed 's/ (current)$//'
end

complete -f -c fonts -a '(__fish_complete_fonts)'

complete -f -c fonts -l list -s l -d 'List all the available fonts'
complete -f -c fonts -l filter -s f -d 'Filter out ignored fonts (only applies with --list)'
complete -f -c fonts -l help -s h -d 'Show the help message'
