with Ada.Exceptions;

package Leander.Logging is

   type Log_Level is
     (Fatal, Error, Warn, Info, Debug, Trace);

   type Log_Interface is limited interface;
   type Reference is access all Log_Interface'Class;

   procedure Log
     (This     : in out Log_Interface;
      Level    : Log_Level;
      Message  : String)
   is abstract;

   procedure Close_Log (This : in out Log_Interface)
   is abstract;

   function Null_Logger return Reference;

   procedure Log_Exception
     (This       : in out Log_Interface'Class;
      Level      : Log_Level;
      Occurance  : Ada.Exceptions.Exception_Occurrence);

   procedure Fatal
     (This : in out Log_Interface'Class;
      Message : String);

   procedure Error
     (This : in out Log_Interface'Class;
      Message : String);

   procedure Warn
     (This : in out Log_Interface'Class;
      Message : String);

   procedure Info
     (This : in out Log_Interface'Class;
      Message : String);

   procedure Debug
     (This : in out Log_Interface'Class;
      Message : String);

   procedure Trace
     (This : in out Log_Interface'Class;
      Message : String);

end Leander.Logging;
