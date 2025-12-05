local lsp = require('utils.lsp')

vim.lsp.config('*', lsp.default_config)

-- Override to perform additional checks on starting language servers
vim.lsp.start = lsp.start

for _, dir in ipairs(vim.api.nvim__get_runtime({ 'lsp' }, true, {})) do
  for config_file in vim.fs.dir(dir) do
    vim.lsp.enable(vim.fn.fnamemodify(config_file, ':r'))
  end
end

-- Show notification if no references, definition, declaration,
-- implementation or type definition is found
do
  local methods = {
    'textDocument/references',
    'textDocument/definition',
    'textDocument/declaration',
    'textDocument/implementation',
    'textDocument/typeDefinition',
  }

  for _, method in ipairs(methods) do
    local obj_name = method:match('/(%w*)$'):gsub('s$', '')
    local handler = vim.lsp.handlers[method]

    vim.lsp.handlers[method] = function(err, result, ctx, ...)
      if not result or vim.tbl_isempty(result) then
        vim.notify('[LSP] no ' .. obj_name .. ' found')
        return
      end

      -- textDocument/definition can return Location or Location[]
      -- https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_definition
      if not vim.islist(result) then
        result = { result }
      end

      if #result == 1 then
        vim.lsp.util.show_document(
          result[1],
          vim.lsp.get_client_by_id(ctx.client_id).offset_encoding,
          { focus = true }
        )
        return
      end

      handler(err, result, ctx, ...)
    end
  end
end

-- Configure hovering window style
-- Hijack LSP floating window function to use custom options
do
  local open_floating_preview = vim.lsp.util.open_floating_preview

  ---@param contents table of lines to show in window
  ---@param syntax string of syntax to set for opened buffer
  ---@param opts table with optional fields (additional keys are passed on to |nvim_open_win()|)
  ---@returns bufnr,winnr buffer and window number of the newly created floating preview window
  ---@diagnostic disable-next-line: duplicate-set-field
  function vim.lsp.util.open_floating_preview(contents, syntax, opts)
    return open_floating_preview(
      contents,
      syntax,
      vim.tbl_deep_extend('force', opts, {
        border = 'solid',
        max_width = math.max(80, math.ceil(vim.go.columns * 0.75)),
        max_height = math.max(20, math.ceil(vim.go.lines * 0.4)),
        close_events = {
          'CursorMovedI',
          'CursorMoved',
          'InsertEnter',
          'WinScrolled',
          'WinResized',
          'VimResized',
        },
      })
    )
  end

  -- Use loclist instead of qflist by default when showing document symbols
  local lsp_document_symbol = vim.lsp.buf.document_symbol

  ---@diagnostic disable-next-line: duplicate-set-field
  vim.lsp.buf.document_symbol = function()
    lsp_document_symbol({
      loclist = true,
    })
  end
end

-- Automatically stop LSP servers that no longer attach to any buffers
--
-- Once `LspDetach` is triggered, wait for 60s before checking and
-- stopping servers, in this way the callback will be invoked once
-- every 60 seconds at most and can stop multiple clients at once
-- if possible, which is more efficient than checking and stopping
-- clients on every `LspDetach` events
do
  local lsp_autostop_pending
  local lsp_autostop_timeout_ms = 60000

  vim.api.nvim_create_autocmd('LspDetach', {
    group = vim.api.nvim_create_augroup('my.lsp.auto_stop', {}),
    desc = 'Automatically stop detached language servers.',
    callback = function()
      if lsp_autostop_pending then
        return
      end
      lsp_autostop_pending = true
      vim.defer_fn(function()
        lsp_autostop_pending = nil
        for _, client in ipairs(vim.lsp.get_clients()) do
          if vim.tbl_isempty(client.attached_buffers) then
            lsp.soft_stop(client)
          end
        end
      end, lsp_autostop_timeout_ms)
    end,
  })
end

-- Keymaps
do
  ---Check if there exists an LS that supports the given method
  ---for the given buffer
  ---@param method string the method to check for
  ---@param bufnr number buffer handler
  local function buf_supports_method(method, bufnr)
    local clients = vim.lsp.get_clients({ bufnr = bufnr })
    for _, client in ipairs(clients) do
      if client:supports_method(method) then
        return true
      end
    end
    return false
  end

  ---Returns a function that takes an LSP action if given method is supported,
  ---else fallback
  ---@param method string lsp method to check
  ---@param action string action callback name
  ---@return fun(fallback: function)
  local function act_if_supports_method(method, action)
    return function(fallback)
      if buf_supports_method(method, 0) then
        vim.lsp.buf[action]()
        return
      end
      fallback()
    end
  end

  local key = require('utils.key')

  -- stylua: ignore start
  key.amend({ 'n', 'x' }, 'gd', act_if_supports_method('textDocument/definition', 'definition'), { desc = 'Go to definition' })
  key.amend({ 'n', 'x' }, 'gD', act_if_supports_method('textDocument/declaration', 'declaration'), { desc = 'Go to declaration' })
  -- stylua: ignore end

  -- stylua: ignore start
  vim.keymap.set({ 'n' }, 'gq;', function()
    local formatprg = vim.api.nvim_get_option_value('formatprg', {})
    if formatprg == "" then
      vim.lsp.buf.format()
    else
      vim.cmd('norm gqaf')
    end
  end, { desc = 'Format buffer' })
  vim.keymap.set({ 'i' }, '<M-a>', function() vim.lsp.buf.code_action() end, { desc = 'Show code actions' })
  vim.keymap.set({ 'i' }, '<C-_>', function() vim.lsp.buf.code_action() end, { desc = 'Show code actions' })
  vim.keymap.set({ 'n', 'x' }, 'g/', function() vim.lsp.buf.references() end, { desc = 'Go to references' })
  vim.keymap.set({ 'n', 'x' }, 'g.', function() vim.lsp.buf.implementation() end, { desc = 'Go to implementation' })
  vim.keymap.set({ 'n', 'x' }, 'gb', function() vim.lsp.buf.type_definition() end, { desc = 'Go to type definition' })
  vim.keymap.set({ 'n', 'x' }, '<Leader>r', function() vim.lsp.buf.rename() end, { desc = 'Rename symbol' })
  vim.keymap.set({ 'n', 'x' }, '<Leader>a', function() vim.lsp.buf.code_action() end, { desc = 'Show code actions' })
  vim.keymap.set({ 'n', 'x' }, '<Leader><', function() vim.lsp.buf.incoming_calls() end, { desc = 'Show incoming calls' })
  vim.keymap.set({ 'n', 'x' }, '<Leader>>', function() vim.lsp.buf.outgoing_calls() end, { desc = 'Show outgoing calls' })
  vim.keymap.set({ 'n', 'x' }, '<Leader>s', function() vim.lsp.buf.document_symbol() end, { desc = 'Show document symbols' })
  vim.keymap.set({ 'n', 'x' }, '<Leader>S', function() vim.lsp.buf.workspace_symbol() end, { desc = 'Show workspace symbols' })
  -- stylua: ignore end
end

-- Commands
do
  vim.api.nvim_create_user_command('LspLog', function()
    vim.cmd.vsplit(vim.lsp.log.get_filename())
  end, { desc = 'Get all the LSP logs' })

  -- vim.api.nvim_create_user_command('LspInfo', function()
  --   vim.cmd('silent checkhealth vim.lsp')
  -- end, { desc = 'Get all the information about all LSP attached' })

  -- LspRestart: Restart LSP clients for current buffer
  vim.api.nvim_create_user_command('LspRestart', function()
    local bufnr = vim.api.nvim_get_current_buf()
    local clients = vim.lsp.get_clients({ bufnr = bufnr })

    if #clients == 0 then
      vim.notify('No LSP clients attached to restart', vim.log.levels.WARN)
      return
    end

    for _, client in ipairs(clients) do
      vim.notify('Restarting ' .. client.name, vim.log.levels.INFO)
      vim.lsp.stop_client(client.id)
    end

    vim.defer_fn(function()
      vim.cmd('edit')
    end, 100)
  end, { desc = 'Restart LSP clients for current buffer' })

  -- LspStatus: Show brief LSP status
  vim.api.nvim_create_user_command('LspStatus', function()
    local bufnr = vim.api.nvim_get_current_buf()
    local clients = vim.lsp.get_clients({ bufnr = bufnr })

    if #clients == 0 then
      print('󰅚 No LSP clients attached')
      return
    end

    print('󰒋 LSP Status for buffer ' .. bufnr .. ':')
    print(
      '─────────────────────────────────'
    )

    for i, client in ipairs(clients) do
      print(
        string.format('󰌘 Client %d: %s (ID: %d)', i, client.name, client.id)
      )
      print('  Root: ' .. (client.config.root_dir or 'N/A'))
      print(
        '  Filetypes: ' .. table.concat(client.config.filetypes or {}, ', ')
      )

      local caps = client.server_capabilities
      local features = {}
      if caps.completionProvider then
        table.insert(features, 'completion')
      end
      if caps.hoverProvider then
        table.insert(features, 'hover')
      end
      if caps.definitionProvider then
        table.insert(features, 'definition')
      end
      if caps.referencesProvider then
        table.insert(features, 'references')
      end
      if caps.renameProvider then
        table.insert(features, 'rename')
      end
      if caps.codeActionProvider then
        table.insert(features, 'code_action')
      end
      if caps.documentFormattingProvider then
        table.insert(features, 'formatting')
      end

      print('  Features: ' .. table.concat(features, ', '))
      print('')
    end
  end, { desc = 'Show brief LSP status' })

  -- LspCapabilities: Show all capabilities for attached LSP clients
  vim.api.nvim_create_user_command('LspCapabilities', function()
    local bufnr = vim.api.nvim_get_current_buf()
    local clients = vim.lsp.get_clients({ bufnr = bufnr })

    if #clients == 0 then
      print('No LSP clients attached')
      return
    end

    for _, client in ipairs(clients) do
      print('Capabilities for ' .. client.name .. ':')
      local caps = client.server_capabilities

      local capability_list = {
        { 'Completion', caps.completionProvider },
        { 'Hover', caps.hoverProvider },
        { 'Signature Help', caps.signatureHelpProvider },
        { 'Go to Definition', caps.definitionProvider },
        { 'Go to Declaration', caps.declarationProvider },
        { 'Go to Implementation', caps.implementationProvider },
        { 'Go to Type Definition', caps.typeDefinitionProvider },
        { 'Find References', caps.referencesProvider },
        { 'Document Highlight', caps.documentHighlightProvider },
        { 'Document Symbol', caps.documentSymbolProvider },
        { 'Workspace Symbol', caps.workspaceSymbolProvider },
        { 'Code Action', caps.codeActionProvider },
        { 'Code Lens', caps.codeLensProvider },
        { 'Document Formatting', caps.documentFormattingProvider },
        { 'Document Range Formatting', caps.documentRangeFormattingProvider },
        { 'Rename', caps.renameProvider },
        { 'Folding Range', caps.foldingRangeProvider },
        { 'Selection Range', caps.selectionRangeProvider },
        { 'Inlay Hint', caps.inlayHintProvider },
      }

      for _, cap in ipairs(capability_list) do
        local status = cap[2] and '✓' or '✗'
        print(string.format('  %s %s', status, cap[1]))
      end
      print('')
    end
  end, { desc = 'Show all LSP capabilities' })

  -- LspDiagnostics: Show diagnostic counts
  vim.api.nvim_create_user_command('LspDiagnostics', function()
    local bufnr = vim.api.nvim_get_current_buf()
    local diagnostics = vim.diagnostic.get(bufnr)

    local counts = { ERROR = 0, WARN = 0, INFO = 0, HINT = 0 }

    for _, diagnostic in ipairs(diagnostics) do
      local severity = vim.diagnostic.severity[diagnostic.severity]
      counts[severity] = counts[severity] + 1
    end

    print('󰒡 Diagnostics for current buffer:')
    print('  Errors: ' .. counts.ERROR)
    print('  Warnings: ' .. counts.WARN)
    print('  Info: ' .. counts.INFO)
    print('  Hints: ' .. counts.HINT)
    print('  Total: ' .. #diagnostics)
  end, { desc = 'Show diagnostic counts for current buffer' })

  -- LspInfo: Comprehensive LSP information
  vim.api.nvim_create_user_command('LspInfo', function()
    local bufnr = vim.api.nvim_get_current_buf()
    local clients = vim.lsp.get_clients({ bufnr = bufnr })

    print(
      '═══════════════════════════════════'
    )
    print('           LSP INFORMATION          ')
    print(
      '═══════════════════════════════════'
    )
    print('')

    print('󰈙 Language client log: ' .. vim.lsp.get_log_path())
    print('󰈔 Detected filetype: ' .. vim.bo.filetype)
    print('󰈮 Buffer: ' .. bufnr)
    print('󰈔 Root directory: ' .. (vim.fn.getcwd() or 'N/A'))
    print('')

    if #clients == 0 then
      print('󰅚 No LSP clients attached to buffer ' .. bufnr)
      print('')
      print('Possible reasons:')
      print('  • No language server installed for ' .. vim.bo.filetype)
      print('  • Language server not configured')
      print('  • Not in a project root directory')
      print('  • File type not recognized')
      return
    end

    print('󰒋 LSP clients attached to buffer ' .. bufnr .. ':')
    print(
      '─────────────────────────────────'
    )

    for i, client in ipairs(clients) do
      print(string.format('󰌘 Client %d: %s', i, client.name))
      print('  ID: ' .. client.id)
      print('  Root dir: ' .. (client.config.root_dir or 'Not set'))
      print('  Command: ' .. table.concat(client.config.cmd or {}, ' '))
      print(
        '  Filetypes: ' .. table.concat(client.config.filetypes or {}, ', ')
      )

      if client.is_stopped() then
        print('  Status: 󰅚 Stopped')
      else
        print('  Status: 󰄬 Running')
      end

      if client.workspace_folders and #client.workspace_folders > 0 then
        print('  Workspace folders:')
        for _, folder in ipairs(client.workspace_folders) do
          print('    • ' .. folder.name)
        end
      end

      local attached_buffers = {}
      for buf, _ in pairs(client.attached_buffers or {}) do
        table.insert(attached_buffers, buf)
      end
      print('  Attached buffers: ' .. #attached_buffers)

      local caps = client.server_capabilities
      local key_features = {}
      if caps.completionProvider then
        table.insert(key_features, 'completion')
      end
      if caps.hoverProvider then
        table.insert(key_features, 'hover')
      end
      if caps.definitionProvider then
        table.insert(key_features, 'definition')
      end
      if caps.documentFormattingProvider then
        table.insert(key_features, 'formatting')
      end
      if caps.codeActionProvider then
        table.insert(key_features, 'code_action')
      end

      if #key_features > 0 then
        print('  Key features: ' .. table.concat(key_features, ', '))
      end

      print('')
    end

    local diagnostics = vim.diagnostic.get(bufnr)
    if #diagnostics > 0 then
      print('󰒡 Diagnostics Summary:')
      local counts = { ERROR = 0, WARN = 0, INFO = 0, HINT = 0 }

      for _, diagnostic in ipairs(diagnostics) do
        local severity = vim.diagnostic.severity[diagnostic.severity]
        counts[severity] = counts[severity] + 1
      end

      print('  󰅚 Errors: ' .. counts.ERROR)
      print('  󰀪 Warnings: ' .. counts.WARN)
      print('  󰋽 Info: ' .. counts.INFO)
      print('  󰌶 Hints: ' .. counts.HINT)
      print('  Total: ' .. #diagnostics)
    else
      print('󰄬 No diagnostics')
    end

    print('')
    print('Use :LspLog to view detailed logs')
    print('Use :LspCapabilities for full capability list')
  end, { desc = 'Show comprehensive LSP information' })
end
