complete -c mb -f

# Main options
complete -c mb -s h -l help -d 'Show help message'
complete -c mb -s m -l move -d 'Use move instead of copy'
complete -c mb -s R -l restore -d 'Restore from backup'

# Condition-sensitive argument completion
complete -c mb -n '__fish_contains_opt -s R restore' -a '(find . -maxdepth 1 -name "*.bak" -type f 2>/dev/null | sed "s|^\./||")'
complete -c mb -n 'not __fish_contains_opt -s R restore' -a '(__fish_complete_path)'
