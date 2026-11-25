let znaki_utf8 tekst =
  let rec czytaj i acc =
    if i = String.length tekst then Array.of_list (List.rev acc)
    else
      let d = String.get_utf_8_uchar tekst i in
      if not (Uchar.utf_decode_is_valid d) then failwith "Plik nie jest w UTF-8";
      czytaj (i + Uchar.utf_decode_length d) (Uchar.utf_decode_uchar d :: acc)
  in czytaj 0 []

let () =
  let tekst = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  let znaki = znaki_utf8 tekst in
  let n = Array.length znaki in
  let poczatek = ref 0 and najlepszy = ref 0 in
  let rec rozszerz a b =
    if a >= 0 && b < n && Uchar.equal znaki.(a) znaki.(b) then
      rozszerz (a - 1) (b + 1)
    else if b - a - 1 > !najlepszy then begin
      poczatek := a + 1;
      najlepszy := b - a - 1
    end
  in
  for i = 0 to n - 1 do rozszerz i i; rozszerz i (i + 1) done;
  let buf = Buffer.create !najlepszy in
  for i = !poczatek to !poczatek + !najlepszy - 1 do
    Buffer.add_utf_8_uchar buf znaki.(i)
  done;
  Printf.printf "Długość: %d\n%s\n" !najlepszy (Buffer.contents buf)
