-- Name:         solarized
-- Description:  Solarized colorscheme for Neovim
-- Author:       Ethan Schoonover (adapted for Neovim)
-- License:      MIT

-- Clear hlgroups and set colors_name {{{
vim.cmd('hi clear')
vim.g.colors_name = 'solarized'
-- }}}

-- Palette {{{
local c_base03
local c_base02
local c_base01
local c_base00
local c_base0
local c_base1
local c_base2
local c_base3
local c_yellow
local c_orange
local c_red
local c_magenta
local c_violet
local c_blue
local c_cyan
local c_green

if vim.go.bg == 'dark' then
    c_base03  = { '#002b36', 234 }
    c_base02  = { '#073642', 235 }
    c_base01  = { '#586e75', 240 }
    c_base00  = { '#657b83', 241 }
    c_base0   = { '#839496', 244 }
    c_base1   = { '#93a1a1', 245 }
    c_base2   = { '#eee8d5', 254 }
    c_base3   = { '#fdf6e3', 230 }
    c_yellow  = { '#b58900', 136 }
    c_orange  = { '#cb4b16', 166 }
    c_red     = { '#dc322f', 160 }
    c_magenta = { '#d33682', 125 }
    c_violet  = { '#6c71c4', 61 }
    c_blue    = { '#268bd2', 33 }
    c_cyan    = { '#2aa198', 37 }
    c_green   = { '#859900', 64 }
else
    c_base03  = { '#fdf6e3', 230 }
    c_base02  = { '#eee8d5', 254 }
    c_base01  = { '#93a1a1', 245 }
    c_base00  = { '#839496', 244 }
    c_base0   = { '#657b83', 241 }
    c_base1   = { '#586e75', 240 }
    c_base2   = { '#073642', 235 }
    c_base3   = { '#002b36', 234 }
    c_yellow  = { '#b58900', 136 }
    c_orange  = { '#cb4b16', 166 }
    c_red     = { '#dc322f', 160 }
    c_magenta = { '#d33682', 125 }
    c_violet  = { '#6c71c4', 61 }
    c_blue    = { '#268bd2', 33 }
    c_cyan    = { '#2aa198', 37 }
    c_green   = { '#859900', 64 }
end
-- }}}

-- Terminal colors {{{
if vim.go.bg == 'dark' then
    vim.g.terminal_color_0  = c_base02[1]
    vim.g.terminal_color_1  = c_red[1]
    vim.g.terminal_color_2  = c_green[1]
    vim.g.terminal_color_3  = c_yellow[1]
    vim.g.terminal_color_4  = c_blue[1]
    vim.g.terminal_color_5  = c_magenta[1]
    vim.g.terminal_color_6  = c_cyan[1]
    vim.g.terminal_color_7  = c_base2[1]
    vim.g.terminal_color_8  = c_base03[1]
    vim.g.terminal_color_9  = c_orange[1]
    vim.g.terminal_color_10 = c_base01[1]
    vim.g.terminal_color_11 = c_base00[1]
    vim.g.terminal_color_12 = c_base0[1]
    vim.g.terminal_color_13 = c_violet[1]
    vim.g.terminal_color_14 = c_base1[1]
    vim.g.terminal_color_15 = c_base3[1]
    vim.g.terminal_color_16 = c_orange[1]
    vim.g.terminal_color_17 = c_orange[1]
 else
    vim.g.terminal_color_0  = c_base2[1]
    vim.g.terminal_color_1  = c_red[1]
    vim.g.terminal_color_2  = c_green[1]
    vim.g.terminal_color_3  = c_yellow[1]
    vim.g.terminal_color_4  = c_blue[1]
    vim.g.terminal_color_5  = c_magenta[1]
    vim.g.terminal_color_6  = c_cyan[1]
    vim.g.terminal_color_7  = c_base02[1]
    vim.g.terminal_color_8  = c_base3[1]
    vim.g.terminal_color_9  = c_orange[1]
    vim.g.terminal_color_10 = c_base1[1]
    vim.g.terminal_color_11 = c_base0[1]
    vim.g.terminal_color_12 = c_base00[1]
    vim.g.terminal_color_13 = c_violet[1]
    vim.g.terminal_color_14 = c_base01[1]
    vim.g.terminal_color_15 = c_base03[1]
    vim.g.terminal_color_16 = c_orange[1]
    vim.g.terminal_color_17 = c_orange[1]
 end
-- }}}

-- Highlight groups {{{
local hlgroups = {
  -- UI {{{
  ColorColumn = { bg = c_base02 },
  Conceal = { bold = true, fg = c_base01 },
  CurSearch = { link = 'IncSearch' },
  Cursor = { bg = c_base0, fg = c_base03 },
  CursorColumn = { link = 'CursorLine' },
  CursorIM = { link = 'Cursor' },
  CursorLine = { bg = c_base02 },
  CursorLineNr = { fg = c_base0, bold = true },
  DebugPC = { bg = c_base02 },
  DiffAdd = { bg = c_base02 },
  DiffAdded = { fg = c_green },
  DiffChange = { bg = c_base02 },
  DiffChanged = { fg = c_yellow },
  DiffDelete = { fg = c_base02 },
  DiffDeleted = { fg = c_red },
  DiffNewFile = { fg = c_green },
  DiffOldFile = { fg = c_red },
  DiffRemoved = { fg = c_red },
  DiffText = { bg = c_base01 },
  Directory = { fg = c_blue },
  EndOfBuffer = { fg = c_base03 },
  ErrorMsg = { fg = c_red },
  FloatBorder = { bg = c_base03, fg = c_base01 },
  FloatFooter = { bg = c_base03, fg = c_base01 },
  FloatTitle = { bg = c_base03, fg = c_base01, bold = true },
  FoldColumn = { fg = c_base01 },
  Folded = { bg = c_base02, fg = c_base01 },
  Ignore = { link = 'NonText' },
  IncSearch = { bg = c_yellow, fg = c_base03 },
  LineNr = { fg = c_base01 },
  MatchParen = { bg = c_base02 },
  ModeMsg = { fg = c_red, bold = true },
  MoreMsg = { fg = c_blue },
  MsgArea = { fg = c_base0 },
  MsgSeparator = { bg = c_base03 },
  NonText = { fg = c_base01 },
  Normal = { bg = c_base03, fg = c_base0 },
  NormalFloat = { bg = c_base03, fg = c_base0 },
  NormalNC = { link = 'Normal' },
  Pmenu = { bg = c_base02, fg = c_base0 },
  PmenuExtra = { fg = c_base01 },
  PmenuSbar = { bg = c_base02 },
  PmenuSel = { bg = c_base02, fg = 'NONE' },
  PmenuThumb = { bg = c_base01 },
  Question = { link = 'MoreMsg' },
  QuickFixLine = { bg = c_base02 },
  Search = { bg = c_base02 },
  SignColumn = { fg = c_base01 },
  SpellBad = { underdashed = true },
  SpellCap = { underdashed = true },
  SpellLocal = { underdashed = true },
  SpellRare = { underdashed = true },
  StatusLine = { bg = c_base02, fg = c_base0 },
  StatusLineNC = { bg = c_base02, fg = c_base01 },
  Substitute = { bg = c_red, fg = c_base0 },
  TabLine = { link = 'StatusLineNC' },
  TabLineFill = { link = 'Normal' },
  TabLineSel = { link = 'StatusLine' },
  TermCursor = { fg = c_base03, bg = c_red },
  Title = { bold = true, fg = c_blue },
  Underlined = { fg = c_cyan, underline = true },
  VertSplit = { link = 'WinSeparator' },
  Visual = { bg = c_base02 },
  VisualNOS = { link = 'Visual' },
  WarningMsg = { fg = c_yellow },
  Whitespace = { fg = c_base02 },
  WildMenu = { link = 'Pmenu' },
  WinBar = { bg = c_base03, fg = c_base0 },
  WinBarNC = { bg = c_base03, fg = c_base01 },
  WinSeparator = { fg = c_base02 },
  lCursor = { link = 'Cursor' },
  -- }}}}

  -- Syntax {{{
  Boolean = { fg = c_orange, bold = true },
  Character = { link = 'String' },
  Comment = { fg = c_base01 },
  Constant = { fg = c_orange },
  Delimiter = { fg = c_base01 },
  Error = { fg = c_red },
  Exception = { fg = c_red },
  Float = { link = 'Number' },
  Function = { fg = c_blue },
  Identifier = { fg = c_base0 },
  Keyword = { fg = c_violet },
  Number = { fg = c_magenta },
  Operator = { fg = c_red },
  PreProc = { fg = c_red },
  Special = { fg = c_cyan },
  SpecialKey = { fg = c_base01 },
  Statement = { fg = c_violet },
  String = { fg = c_green },
  Todo = { fg = c_base03, bg = c_blue, bold = true },
  Type = { fg = c_cyan },
  -- }}}}

  -- Treesitter syntax {{{
  ['@attribute'] = { link = 'Constant' },
  ['@constructor'] = { fg = c_cyan },
  ['@constructor.lua'] = { fg = c_violet },
  ['@keyword.exception'] = { bold = true, fg = c_red },
  ['@keyword.import'] = { link = 'PreProc' },
  ['@keyword.luap'] = { link = '@string.regexp' },
  ['@keyword.operator'] = { bold = true, fg = c_red },
  ['@keyword.return'] = { fg = c_red },
  ['@module'] = { fg = c_orange },
  ['@operator'] = { link = 'Operator' },
  ['@punctuation.bracket'] = { fg = c_base01 },
  ['@punctuation.delimiter'] = { fg = c_base01 },
  ['@markup.list'] = { fg = c_cyan },
  ['@string.escape'] = { fg = c_orange },
  ['@string.regexp'] = { fg = c_orange },
  ['@string.yaml'] = { link = 'Normal' },
  ['@markup.link.label.symbol'] = { fg = c_base0 },
  ['@tag.attribute'] = { fg = c_base0 },
  ['@tag.delimiter'] = { fg = c_base01 },
  ['@comment.error'] = { bg = c_red, fg = c_base0, bold = true },
  ['@diff.delta'] = { link = 'DiffChanged' },
  ['@diff.minus'] = { link = 'DiffRemoved' },
  ['@diff.plus'] = { link = 'DiffAdded' },
  ['@markup.emphasis'] = { italic = true },
  ['@markup.environment'] = { link = 'Keyword' },
  ['@markup.environment.name'] = { link = 'String' },
  ['@markup.raw'] = { link = 'String' },
  ['@comment.note'] = { bg = c_cyan, fg = c_base03, bold = true },
  ['@markup.quote'] = { link = '@variable.parameter' },
  ['@markup.strong'] = { bold = true },
  ['@markup.heading'] = { link = 'Function' },
  ['@markup.heading.1.markdown'] = { fg = c_red },
  ['@markup.heading.2.markdown'] = { fg = c_red },
  ['@markup.heading.3.markdown'] = { fg = c_red },
  ['@markup.heading.4.markdown'] = { fg = c_red },
  ['@markup.heading.5.markdown'] = { fg = c_red },
  ['@markup.heading.6.markdown'] = { fg = c_red },
  ['@markup.heading.1.marker.markdown'] = { link = 'Delimiter' },
  ['@markup.heading.2.marker.markdown'] = { link = 'Delimiter' },
  ['@markup.heading.3.marker.markdown'] = { link = 'Delimiter' },
  ['@markup.heading.4.marker.markdown'] = { link = 'Delimiter' },
  ['@markup.heading.5.marker.markdown'] = { link = 'Delimiter' },
  ['@markup.heading.6.marker.markdown'] = { link = 'Delimiter' },
  ['@markup.heading.1.delimiter.vimdoc'] = { link = 'helpSectionDelim' },
  ['@markup.heading.2.delimiter.vimdoc'] = { link = 'helpSectionDelim' },
  ['@comment.todo.checked'] = { fg = c_base01 },
  ['@comment.todo.unchecked'] = { fg = c_red },
  ['@markup.link.label.markdown_inline'] = { link = 'htmlLink' },
  ['@markup.link.url.markdown_inline'] = { link = 'htmlString' },
  ['@comment.warning'] = { bg = c_yellow, fg = c_base03, bold = true },
  ['@variable'] = { fg = c_base0 },
  ['@variable.builtin'] = { fg = c_red },
  -- }}}

  -- LSP semantic {{{
  ['@lsp.mod.readonly'] = { link = 'Constant' },
  ['@lsp.mod.typeHint'] = { link = 'Type' },
  ['@lsp.type.builtinConstant'] = { link = '@constant.builtin' },
  ['@lsp.type.comment'] = { fg = 'NONE' },
  ['@lsp.type.macro'] = { fg = c_magenta },
  ['@lsp.type.magicFunction'] = { link = '@function.builtin' },
  ['@lsp.type.method'] = { link = '@function.method' },
  ['@lsp.type.namespace'] = { link = '@module' },
  ['@lsp.type.parameter'] = { link = '@variable.parameter' },
  ['@lsp.type.selfParameter'] = { link = '@variable.builtin' },
  ['@lsp.type.variable'] = { fg = 'NONE' },
  ['@lsp.typemod.function.builtin'] = { link = '@function.builtin' },
  ['@lsp.typemod.function.defaultLibrary'] = { link = '@function.builtin' },
  ['@lsp.typemod.function.readonly'] = { bold = true, fg = c_blue },
  ['@lsp.typemod.keyword.documentation'] = { link = 'Special' },
  ['@lsp.typemod.method.defaultLibrary'] = { link = '@function.builtin' },
  ['@lsp.typemod.operator.controlFlow'] = { link = '@keyword.exception' },
  ['@lsp.typemod.operator.injected'] = { link = 'Operator' },
  ['@lsp.typemod.string.injected'] = { link = 'String' },
  ['@lsp.typemod.variable.defaultLibrary'] = { link = '@variable.builtin' },
  ['@lsp.typemod.variable.injected'] = { link = '@variable' },
  ['@lsp.typemod.variable.static'] = { link = 'Constant' },
  -- }}}

  -- LSP {{{
  LspCodeLens = { fg = c_base01 },
  LspInfoBorder = { link = 'FloatBorder' },
  LspReferenceRead = { link = 'LspReferenceText' },
  LspReferenceText = { bg = c_base02 },
  LspReferenceWrite = { bg = c_base02, underline = true },
  LspSignatureActiveParameter = { fg = c_yellow },
  -- }}}

  -- Diagnostic {{{
  DiagnosticError = { fg = c_red },
  DiagnosticHint = { fg = c_cyan },
  DiagnosticInfo = { fg = c_blue },
  DiagnosticOk = { fg = c_green },
  DiagnosticWarn = { fg = c_yellow },
  DiagnosticSignError = { fg = c_red },
  DiagnosticSignHint = { fg = c_cyan },
  DiagnosticSignInfo = { fg = c_blue },
  DiagnosticSignWarn = { fg = c_yellow },
  DiagnosticUnderlineError = { sp = c_red, undercurl = true },
  DiagnosticUnderlineHint = { sp = c_cyan, undercurl = true },
  DiagnosticUnderlineInfo = { sp = c_blue, undercurl = true },
  DiagnosticUnderlineWarn = { sp = c_yellow, undercurl = true },
  DiagnosticVirtualTextError = { bg = c_base02, fg = c_red },
  DiagnosticVirtualTextHint = { bg = c_base02, fg = c_cyan },
  DiagnosticVirtualTextInfo = { bg = c_base02, fg = c_blue },
  DiagnosticVirtualTextWarn = { bg = c_base02, fg = c_yellow },
  DiagnosticUnnecessary = {
    fg = c_base01,
    sp = c_cyan,
    undercurl = true,
  },
  -- }}}

  -- Filetype {{{
  -- Git
  gitHash = { fg = c_base01 },

  -- Sh/Bash
  bashSpecialVariables = { link = 'Constant' },
  shAstQuote = { link = 'Constant' },
  shCaseEsac = { link = 'Operator' },
  shDeref = { link = 'Special' },
  shDerefSimple = { link = 'shDerefVar' },
  shDerefVar = { link = 'Constant' },
  shNoQuote = { link = 'shAstQuote' },
  shQuote = { link = 'String' },
  shTestOpr = { link = 'Operator' },

  -- HTML
  htmlBold = { bold = true },
  htmlBoldItalic = { bold = true, italic = true },
  htmlH1 = { fg = c_red, bold = true },
  htmlH2 = { fg = c_red, bold = true },
  htmlH3 = { fg = c_red, bold = true },
  htmlH4 = { fg = c_red, bold = true },
  htmlH5 = { fg = c_red, bold = true },
  htmlH6 = { fg = c_red, bold = true },
  htmlItalic = { italic = true },
  htmlLink = { fg = c_blue, underline = true },
  htmlSpecialChar = { link = 'SpecialChar' },
  htmlSpecialTagName = { fg = c_violet },
  htmlString = { link = 'String' },
  htmlTagName = { link = 'Tag' },
  htmlTitle = { link = 'Title' },

  -- Markdown
  markdownBold = { bold = true },
  markdownBoldItalic = { bold = true, italic = true },
  markdownCode = { fg = c_green },
  markdownCodeBlock = { fg = c_green },
  markdownError = { link = 'NONE' },
  markdownEscape = { fg = 'NONE' },
  markdownH1 = { link = 'htmlH1' },
  markdownH2 = { link = 'htmlH2' },
  markdownH3 = { link = 'htmlH3' },
  markdownH4 = { link = 'htmlH4' },
  markdownH5 = { link = 'htmlH5' },
  markdownH6 = { link = 'htmlH6' },
  markdownListMarker = { fg = c_yellow },

  -- Checkhealth
  healthError = { fg = c_red },
  healthSuccess = { fg = c_green },
  healthWarning = { fg = c_yellow },
  helpHeader = { link = 'Title' },
  helpSectionDelim = { link = 'Title' },

  -- Qf
  qfFileName = { link = 'Directory' },
  qfLineNr = { link = 'lineNr' },
  -- }}}

  -- Plugins {{{
  -- gitsigns
  GitSignsAdd = { fg = c_green },
  GitSignsChange = { fg = c_base01 },
  GitSignsDelete = { fg = c_red },
  GitSignsDeletePreview = { bg = c_base02 },

  -- fugitive
  fugitiveHash = { link = 'gitHash' },
  fugitiveHeader = { link = 'Title' },
  fugitiveHeading = { link = 'Title' },
  fugitiveStagedHeading = { fg = c_green, bold = true },
  fugitiveStagedModifier = { fg = c_green },
  fugitiveUnStagedHeading = { fg = c_yellow, bold = true },
  fugitiveUnstagedModifier = { fg = c_yellow },
  fugitiveUntrackedHeading = { fg = c_cyan, bold = true },
  fugitiveUntrackedModifier = { fg = c_cyan },

  -- telescope
  TelescopeBorder = { bg = c_base02, fg = c_base01 },
  TelescopeMatching = { fg = c_red, bold = true },
  TelescopeNormal = { bg = c_base02, fg = c_base00 },
  TelescopePromptBorder = { bg = c_base02, fg = c_base01 },
  TelescopePromptNormal = { bg = c_base02, fg = c_base00 },
  TelescopeResultsClass = { link = 'Structure' },
  TelescopeResultsField = { link = '@variable.member' },
  TelescopeResultsMethod = { link = 'Function' },
  TelescopeResultsStruct = { link = 'Structure' },
  TelescopeResultsVariable = { link = '@variable' },
  TelescopeSelection = { link = 'Visual' },
  TelescopeTitle = { bg = c_cyan, fg = c_base03 },

  -- nvim-dap-ui
  DapUIBreakpointsCurrentLine = { bold = true, fg = c_base0 },
  DapUIBreakpointsDisabledLine = { link = 'Comment' },
  DapUIBreakpointsInfo = { fg = c_blue },
  DapUIDecoration = { fg = c_base01 },
  DapUIFloatBorder = { fg = c_base01 },
  DapUILineNumber = { fg = c_cyan },
  DapUIModifiedValue = { bold = true, fg = c_cyan },
  DapUIPlayPause = { fg = c_green },
  DapUIRestart = { fg = c_green },
  DapUIScope = { link = 'Special' },
  DapUISource = { fg = c_red },
  DapUIStepBack = { fg = c_cyan },
  DapUIStepInto = { fg = c_cyan },
  DapUIStepOut = { fg = c_cyan },
  DapUIStepOver = { fg = c_cyan },
  DapUIStop = { fg = c_red },
  DapUIStoppedThread = { fg = c_cyan },
  DapUIThread = { fg = c_base0 },
  DapUIType = { link = 'Type' },
  DapUIUnavailable = { fg = c_base01 },
  DapUIWatchesEmpty = { fg = c_red },
  DapUIWatchesError = { fg = c_red },
  DapUIWatchesValue = { fg = c_base0 },

  -- lazy.nvim
  LazyProgressTodo = { fg = c_base01 },

  -- statusline
  StatusLineGitAdded = { bg = c_base02, fg = c_green },
  StatusLineGitChanged = { bg = c_base02, fg = c_yellow },
  StatusLineGitRemoved = { bg = c_base02, fg = c_red },
  StatusLineGitBranch = { bg = c_base02, fg = c_base01 },
  StatusLineHeader = { bg = c_base01, fg = c_base0 },
  StatusLineHeaderModified = { bg = c_red, fg = c_base03 },

  -- }}}
}
-- }}}

-- Highlight group overrides {{{
if vim.go.bg == 'light' then
  hlgroups.CursorLine = { bg = c_base02 }
  hlgroups.DiagnosticSignWarn = { fg = c_yellow }
  hlgroups.DiagnosticUnderlineWarn = { sp = c_yellow, undercurl = true }
  hlgroups.DiagnosticVirtualTextWarn = { bg = c_base02, fg = c_yellow }
  hlgroups.DiagnosticWarn = { fg = c_yellow }
  hlgroups.IncSearch = { bg = c_yellow, fg = c_base03, bold = true }
  hlgroups.Keyword = { fg = c_red }
  hlgroups.ModeMsg = { fg = c_red, bold = true }
  hlgroups.Pmenu = { bg = c_base03, fg = c_base0 }
  hlgroups.PmenuSbar = { bg = c_base02 }
  hlgroups.PmenuSel = { bg = c_base0, fg = c_base03 }
  hlgroups.PmenuThumb = { bg = c_base02 }
  hlgroups.Search = { bg = c_base02 }
  hlgroups.StatusLine = { bg = c_base03 }
  hlgroups.StatusLineGitAdded = { bg = c_base03, fg = c_green }
  hlgroups.StatusLineGitChanged = { bg = c_base03, fg = c_yellow }
  hlgroups.StatusLineGitRemoved = { bg = c_base03, fg = c_red }
  hlgroups.StatusLineGitBranch = { bg = c_base03, fg = c_base01 }
  hlgroups.StatusLineHeader = { bg = c_base0, fg = c_base03 }
  hlgroups.StatusLineHeaderModified = { bg = c_red, fg = c_base03 }
  hlgroups.Visual = { bg = c_base02 }
  hlgroups.WinBar = { bg = c_base03, fg = c_base0 }
  hlgroups.WinBarNC = { bg = c_base02, fg = c_base01 }
  hlgroups['@variable.parameter'] = { link = 'Identifier' }
end
-- }}}

-- Set highlight groups {{{
for name, attr in pairs(hlgroups) do
    attr.ctermbg = attr.bg and attr.bg[2]
    attr.ctermfg = attr.fg and attr.fg[2]
    attr.bg = attr.bg and attr.bg[1]
    attr.fg = attr.fg and attr.fg[1]
    attr.sp = attr.sp and attr.sp[1]
    vim.api.nvim_set_hl(0, name, attr)
end
-- }}}

-- vim:ts=2:sw=2:sts=2:fdm=marker
