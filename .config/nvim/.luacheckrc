---@diagnostic disable: lowercase-global

std = 'luajit'

globals = { 'vim' }

read_globals = { 'vim' }

max_line_length = false

ignore = {
  '631', -- Line too long
  '212', -- Unused argument
  '213', -- Unused loop variable
  '122', -- Setting read-only field (false positive for vim.opt and vim.g)
}

self = false -- Don't report unused self arguments of methods
