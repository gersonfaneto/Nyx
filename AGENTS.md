# Agent Guidelines for Nyx

## Build/Lint/Test Commands

### Main Project
- **Build**: `make update` (NixOS rebuild + home-manager via nix run)
- **Format**: `make format` (nix fmt for .nix files)

### Neovim Configuration
- **Format**: `cd .config/nvim && make format` (stylua)
- **Lint**: `cd .config/nvim && make lint` (luacheck)
- **Format Check**: `cd .config/nvim && make format-check`
- **Test Single**: `<Leader>tt` or `:TestNearest` (vim-test plugin)
- **Test File**: `<Leader>tf` or `:TestFile`
- **Test Suite**: `<Leader>ts` or `:TestSuite`

## Code Style Guidelines

### Lua (Neovim config)
- **Indentation**: 2 spaces
- **Line Length**: 79 columns
- **Quotes**: Single quotes preferred (AutoPreferSingle)
- **Formatting**: stylua with LuaJIT syntax
- **Linting**: luacheck with LuaJIT std, vim global
- **Types**: Lua LSP with vim runtime globals

### Nix
- **Formatter**: alejandra
- **Imports**: Follow nixpkgs conventions

### General
- **Line Endings**: LF (Unix)
- **Error Handling**: Use vim.notify for user messages
- **Naming**: snake_case for functions, PascalCase for modules