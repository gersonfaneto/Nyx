vim.filetype.add({
  extension = {
    mdc = 'markdown',
    log = 'log',
    env = 'dotenv',
  },
  filename = {
    ['.envrc'] = 'bash',
    ['.env'] = 'dotenv',
    ['.eslintrc.json'] = 'jsonc',
    ['.luarc.json'] = 'jsonc',
  },
  pattern = {
    ['[jt]sconfig*.json'] = 'jsonc',
    -- INFO: Match filenames like - ".env.example", ".env.local" and so on
    -- needed to make dotenv-linter with null-ls works correctly
    ['%.env%.[%w_.-]+'] = 'dotenv',
    ['.*%.gradle'] = 'groovy',
    ['.*/%.github/.*%.y*ml'] = 'yaml.github',
    -- For dockder compose-language-service
    ['compose.y.?ml'] = 'yaml.docker-compose',
    ['docker%-compose%.y.?ml'] = 'yaml.docker-compose',
  },
})
