let day _display contents =
  let lines = String.trim contents |> String.split_on_char '\n' in
  List.hd lines
