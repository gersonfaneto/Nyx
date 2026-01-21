local M = {}

-- Get line numbers for highlighted lines in visual mode
M.get_highlighted_line_numbers = function()
    local start_line = vim.fn.line("'<")
    local end_line = vim.fn.line("'>")

    if start_line == 0 or end_line == 0 then
        print("No visual selection found")
        return
    end

    -- Ensure start_line is always less than or equal to end_line
    if start_line > end_line then
        start_line, end_line = end_line, start_line
    end

    local line_numbers = {}
    for i = start_line, end_line do
        table.insert(line_numbers, i)
    end

    local result
    if start_line == end_line then
        -- Single line: L80
        result = string.format("L%d", start_line)
    else
        -- Multiple lines: L80-85
        result = string.format("L%d-%d", start_line, end_line)
    end

    print("Line numbers: " .. result)

    -- Copy to clipboard
    vim.fn.setreg("+", result)

    return line_numbers
end

-- Copy the current file path and line number to the clipboard, use GitHub URL if in a Git repository
M.copyFilePathAndLineNumber = function()
    local current_file = vim.fn.expand("%:p")
    local current_line = vim.fn.line(".")
    local is_git_repo = vim.fn.system("git rev-parse --is-inside-work-tree"):match("true")

    if is_git_repo then
        local current_repo = vim.fn.systemlist("git remote get-url origin")[1]
        local current_branch = vim.fn.systemlist("git rev-parse --abbrev-ref HEAD")[1]

        -- Convert Git URL to GitHub web URL format
        current_repo = current_repo:gsub("git@github.com:", "https://github.com/")
        current_repo = current_repo:gsub("%.git$", "")

        -- Remove leading system path to repository root
        local repo_root = vim.fn.systemlist("git rev-parse --show-toplevel")[1]
        if repo_root then
            current_file = current_file:sub(#repo_root + 2)
        end

        local url = string.format("%s/blob/%s/%s#L%s", current_repo, current_branch, current_file, current_line)
        vim.fn.setreg("+", url)
        print("Copied to clipboard: " .. url)
    else
        -- If not in a Git directory, copy the full file path
        vim.fn.setreg("+", current_file .. "#L" .. current_line)
        print("Copied full path to clipboard: " .. current_file .. "#L" .. current_line)
    end
end

return M
