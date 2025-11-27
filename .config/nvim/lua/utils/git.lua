local M = {}

vim.api.nvim_create_autocmd({ 'BufWrite', 'FileChangedShellPost' }, {
  group = vim.api.nvim_create_augroup('my.git.refresh_writetick', {}),
  callback = function(args)
    vim.b[args.buf].git_writetick = vim.uv.hrtime()
  end,
})

---Get the diff stats for the current buffer asynchronously
---@param args string[]? arguments passed to `git` command
---@param buf integer? buffer handler, defaults to the current buffer
---@return { added: integer?, removed: integer?, changed: integer? } diff stats
function M.diffstat(args, buf)
  buf = vim._resolve_bufnr(buf)
  if not vim.api.nvim_buf_is_valid(buf) then
    return {}
  end

  if
    (vim.b[buf].git_diffstat_writetick or 0) < (vim.b[buf].git_writetick or 1)
  then
    local bufname = vim.api.nvim_buf_get_name(buf)
    local dirname = vim.fs.dirname(bufname)
    local now = vim.uv.hrtime()
    pcall(
      vim.system,
      vim.list_extend({ 'git', '-C', dirname, unpack(args or {}) }, {
        '--no-pager',
        'diff',
        '-U0',
        '--no-color',
        '--no-ext-diff',
        '--',
        bufname,
      }),
      { stderr = false },
      vim.schedule_wrap(function(o)
        if o.code ~= 0 or not vim.api.nvim_buf_is_valid(buf) then
          return
        end

        local stat = { added = 0, removed = 0, changed = 0 }
        for _, line in ipairs(vim.split(o.stdout, '\n')) do
          if line:find('^@@ ') then
            local num_lines_old, num_lines_new =
              line:match('^@@ %-%d+,?(%d*) %+%d+,?(%d*)')
            num_lines_old = tonumber(num_lines_old) or 1
            num_lines_new = tonumber(num_lines_new) or 1
            local num_lines_changed = math.min(num_lines_old, num_lines_new)
            stat.changed = stat.changed + num_lines_changed
            if num_lines_old > num_lines_new then
              stat.removed = stat.removed + num_lines_old - num_lines_changed
            else
              stat.added = stat.added + num_lines_new - num_lines_changed
            end
          end
        end

        if (vim.b[buf].git_diffstat_writetick or 0) < now then
          vim.b[buf].git_diffstat = stat
          vim.b[buf].git_diffstat_writetick = now
        end
      end)
    )
  end

  return vim.b[buf].git_diffstat
end

---Asynchronously execute git command and get output
---NOTE: output can be out of date
---@param args string[] arguments passed to `git` command
---@param cache_key string? unique key name to cache command output in buf-local variables
---@param buf integer? buffer handler, defaults to the current buffer
---@return string?
function M.execute(args, cache_key, buf)
  cache_key = cache_key or vim.fn.sha256(table.concat(args)):sub(1, 8)

  buf = vim._resolve_bufnr(buf)
  if not vim.api.nvim_buf_is_valid(buf) then
    return
  end

  local cache_key_writetick = cache_key .. '_writetick'

  if
    (vim.b[buf][cache_key_writetick] or 0) < (vim.b[buf].git_writetick or 1)
  then
    local now = vim.uv.hrtime()
    pcall(
      vim.system,
      {
        'git',
        '-C',
        vim.fs.dirname(vim.api.nvim_buf_get_name(buf)),
        unpack(args),
      },
      { stderr = false },
      vim.schedule_wrap(function(o)
        if o.code ~= 0 or not vim.api.nvim_buf_is_valid(buf) then
          return
        end

        if (vim.b[buf][cache_key_writetick] or 0) < now then
          vim.b[buf][cache_key] = vim.trim(o.stdout)
          vim.b[buf][cache_key_writetick] = now
        end
      end)
    )
  end

  return vim.b[buf][cache_key]
end

return M
