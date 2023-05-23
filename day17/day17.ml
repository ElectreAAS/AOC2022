let day _display _pool input_buffer =
  let line = Eio.Buf_read.line input_buffer in
  line
