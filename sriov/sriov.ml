open Network_utils
open Stdext
open Unixext
open Xstringext

let print_bool a =
    match a with
    | true -> print_string "True"
    | false -> print_string "False"

let is_regex_match regexp str = 
    try Re_str.search_forward (regexp) str 0; true
    with Not_found -> false

let get_dev_num_with_same_driver driver = 
    Sys.readdir ("/sys/bus/pci/drivers/" ^ driver)
    |> Array.to_list
    |> List.filter (is_regex_match (Re_str.regexp "[0-9]+:[0-9]+:[0-9]+.[0-9]+"))
    |> List.length

let test_get_dev_num ()= 
    get_dev_num_with_same_driver "igb"


let test_parse () =
    Unixext.read_lines "/build/share/srccode/xcp-networkd/sriov/1.txt"
    |> String.concat "\n"
    |> Sriov.get_sriov_info

let test_parse_modprobe () =
    let a, b, c = Sriov.parse_modprobe_conf_internal "/build/3.test" "igb" 8 in
    print_endline (string_of_bool a);
    print_endline (string_of_bool b);
    print_endline c

let _ = 
    test_parse_modprobe ()