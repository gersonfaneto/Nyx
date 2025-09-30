---@type vim.lsp.Config
return {
  cmd = { 'ruby-lsp' },
  filetypes = { 'ruby', 'eruby' },
  root_markers = { '.git', 'Gemfile', '.ruby-version' },
  settings = {
    rubyLsp = {
      enabledFeatures = {
        'codeActions',
        'diagnostics',
        'documentHighlights',
        'documentLink',
        'documentSymbols',
        'foldingRanges',
        'formatting',
        'hover',
        'inlayHint',
        'onTypeFormatting',
        'selectionRanges',
        'semanticHighlighting',
        'completion',
        'codeLens',
        'definition',
        'workspaceSymbol',
        'signatureHelp',
        'typeHierarchy',
      },
    },
  },
}
