local au = require('utils.au')

au.autocmd({
  event = { 'BufRead', 'BufNewFile' },
  pattern = '*.mp3,*.flac,*.wav,*.ogg',
  command = [[set filetype=media]],
})

au.autocmd({
  event = { 'BufRead', 'BufNewFile' },
  pattern = '*.jpg,*.png,*.gif,*.jpeg',
  command = [[set filetype=media]],
})

au.autocmd({
  event = { 'BufRead', 'BufNewFile' },
  pattern = '*.avi,*.mp4,*.mkv,*.mov,*.mpg',
  command = [[set filetype=media]],
})

au.autocmd({
  event = { 'BufRead', 'BufNewFile' },
  pattern = '*.pdf',
  command = [[set filetype=media]],
})
