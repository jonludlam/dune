open Import

type target =
  | Lib of Lib.Local.t
  | Pkg of Package.Name.t
