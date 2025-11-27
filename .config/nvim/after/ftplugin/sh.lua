if vim.fn.executable('shfmt') == 1 then
  vim.bo.formatprg = 'shfmt --indent 2 --space-redirects --case-indent'
end
