return {
  {
    'tpope/vim-projectionist',
    event = 'BufReadPre',
    config = function()
      require('configs.vim-projectionist')
    end,
  },

  {
    'vim-test/vim-test',
    keys = {
      { '<Leader>tk', desc = 'Run the first test class in current file' },
      { '<Leader>ta', desc = 'Run all tests in current file' },
      { '<Leader>tt', desc = 'Run the test neartest to cursor' },
      { '<Leader>tr', desc = 'Run the last test' },
      { '<Leader>ts', desc = 'Run the whole test suite' },
      { '<Leader>to', desc = 'Go to last visited test file' },
    },
    cmd = {
      'TestClass',
      'TestVisit',
      'TestNearest',
      'TestSuite',
      'TestFile',
      'TestLast',
    },
    config = function()
      require('configs.vim-test')
    end,
  },
}
