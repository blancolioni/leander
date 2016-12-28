with Leander.Environments;

package Leander.Parser.Modules is

   function Load_Module
     (Name : String;
      Path : String)
      return Leander.Environments.Environment;

end Leander.Parser.Modules;
