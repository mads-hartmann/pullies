module Commands = {
  type t =
    | Help
    | List
    | Error Errors.t;
};

let parse args =>
  switch args {
  | []
  | ["help"]
  | ["-h"]
  | ["--help"] => Commands.Help
  | ["ls"] => Commands.List
  | [unknown, ..._] => Commands.Error (Errors.UnknownCommand unknown)
  };
