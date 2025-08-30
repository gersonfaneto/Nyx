# AGENTS.md - Neovim Configuration Project

## Build/Lint/Test Commands

### Primary Commands
- `make all` - Run format check and linting
- `make format-check` - Check code formatting with stylua
- `make format` - Format code with stylua
- `make lint` - Lint code with luacheck

### Testing Commands (via vim-test plugin)
- `:TestNearest` - Run test nearest to cursor
- `:TestFile` - Run all tests in current file
- `:TestSuite` - Run entire test suite
- `:TestLast` - Re-run last test
- `:TestClass` - Run first test class in file

## Code Style Guidelines

### Formatting (stylua)
- Column width: 79 characters
- Indentation: 2 spaces
- Quote style: Auto-prefer single quotes
- Syntax: LuaJIT

### Linting (luacheck)
- Standard: LuaJIT
- Globals: `vim` allowed
- Max line length: No limit (false)

### EditorConfig
- Indent style: space
- Indent size: 2
- End of line: lf

### Lua Language Server
- Runtime: LuaJIT
- Library: Neovim runtime paths
- Diagnostics globals: `vim`

## Naming Conventions

### Functions and Variables
- Use `snake_case` for function names and variables
- Use descriptive, meaningful names
- Prefix private functions with underscore when needed

### Modules and Files
- Use `PascalCase` for module names
- File names match module names
- Use hyphens in file names for multi-word modules

### Constants
- Use `UPPER_CASE` for constants
- Group related constants together

## Import Patterns

### Module Imports
```lua
local module_name = require('path.to.module')
local utils = require('utils')
```

### Local Imports
- Import at top of file
- Group related imports together
- Use local assignments for frequently used modules

## Error Handling

### Protected Calls
```lua
local ok, result = pcall(some_function, arg1, arg2)
if not ok then
  vim.notify('Error: ' .. result, vim.log.levels.ERROR)
end
```

### Validation
```lua
if not condition then
  error('Descriptive error message')
end
```

## Code Organization

### File Structure
- `lua/core/` - Core Neovim configuration
- `lua/plugins/` - Plugin configurations
- `lua/configs/` - Tool-specific configurations
- `lua/utils/` - Utility functions
- `lua/dap-configs/` - Debug adapter configurations
- `lua/test-configs/` - Test configurations

### Function Organization
- Group related functions together
- Use clear function signatures with type annotations
- Document complex functions with comments

## Best Practices

### Performance
- Use `vim.loader.enable()` for faster loading
- Defer non-critical setup with autocmds
- Use `pcall` for potentially failing operations

### Type Safety
- Use EmmyLua annotations for function parameters
- Enable Lua language server diagnostics
- Use `---@type` annotations for complex types

### User Experience
- Provide clear error messages
- Use `vim.notify()` for user feedback
- Handle edge cases gracefully

## Key Bindings

### Leader Key
- `<Space>` - Primary leader key
- `<Space>` - Local leader key

### Common Patterns
- `<Leader>t` - Test commands
- `<Leader>g` - Git commands
- `<Leader>f` - File operations
- `<Leader>b` - Buffer operations