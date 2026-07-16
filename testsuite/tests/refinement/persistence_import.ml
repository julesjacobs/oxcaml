module type Imported = module type of Persistence_interface
module Reexport : Imported = Persistence_interface
