open Network_utils
open Xapi_stdext_unix

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


(* let test_parse () =
    Unixext.read_lines "/build/share/srccode/xcp-networkd/sriov/1.txt"
    |> String.concat "\n"
    |> Sriov.get_sriov_info *)

let test_parse_modprobe () =
    let a, b, c = Sriov.parse_modprobe_conf_internal "/build/share/test/5.test" "igb" "7,7"  in
    print_endline (string_of_bool a);
    print_endline (string_of_bool b)


(* let _ =
    test_parse_modprobe () *)
let cli () = 
    let a=read_line () in
    let test () = 
        match a with
        | "1" -> 
            let b=read_line () in
            Sriov.get_capabilities b |> List.map print_endline |> ignore
        | "2" -> 
            let b=read_line () in
            Sriov.enable_internal b |> 
            (fun x -> 
                match x with
                | Ok Network_interface.Sysfs_successful -> print_endline "sysfs successful"
                | Ok Network_interface.Modprobe_successful -> print_endline "modprobe successful"
                | Ok Network_interface.Modprobe_successful_requires_reboot -> print_endline "modprobe need reboot"
                | Error (_, msg) -> print_endline msg
            )
        | "3" -> 
            begin
                let b=read_line () in
                match Sriov.disable_internal b with
                | Ok () -> print_endline "disable OK"
                | Error (_, msg) -> print_endline msg
            end
        | "4" ->
            begin
                let open Network_interface.Sriov in
                let pcibuspath = read_line () in
                let vf_string = read_line () in 
                Scanf.sscanf vf_string "%s@,%d,%d" (fun mac vlan rate -> 
                match Sriov.make_vf_conf_internal pcibuspath (Some mac) (Some vlan) (Some rate) with
                | Ok () -> print_endline "config vf succuessful"
                | Error (_, msg) -> print_endline msg
                )
            end
        | _ -> ()
    in
    test ()

let _ = cli ()

(* let _ =
    let re = Re_posix.compile_pat "virtfn(\d+)" in
    Re.execp re "virtfn20"
    |> print_bool *)

(* let test_get_vf_index ()=
    let pcibuspath = read_line () in
    let parent_dev = Sysfs.parent_device_of_vf pcibuspath in
    Sysfs.device_index_of_vf parent_dev pcibuspath
    |> print_int *)
