package body Leander.Logging is

   type Null_Logger_Instance is
     new Log_Interface with null record;

   overriding procedure Log
     (This     : in out Null_Logger_Instance;
      Level    : Log_Level;
      Message  : String)
   is null;

   overriding procedure Close_Log (This : in out Null_Logger_Instance)
   is null;

   Local_Null_Logger : aliased Null_Logger_Instance;

   function Null_Logger return Reference
   is (Local_Null_Logger'Access);

   procedure Log_Exception
     (This       : in out Log_Interface'Class;
      Level      : Log_Level;
      Occurance  : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      This.Log
        (Level,
         Ada.Exceptions.Exception_Message (Occurance));
      This.Log
        (Level,
         Ada.Exceptions.Exception_Information (Occurance));
   end Log_Exception;

   -----------
   -- Debug --
   -----------

   procedure Debug
     (This : in out Log_Interface'Class;
      Message : String)
   is
   begin
      This.Log (Debug, Message);
   end Debug;

   -----------
   -- Error --
   -----------

   procedure Error
     (This : in out Log_Interface'Class;
      Message : String)
   is
   begin
      This.Log (Error, Message);
   end Error;

   -----------
   -- Fatal --
   -----------

   procedure Fatal
     (This : in out Log_Interface'Class;
      Message : String)
   is
   begin
      This.Log (Fatal, Message);
   end Fatal;

   ----------
   -- Info --
   ----------

   procedure Info
     (This : in out Log_Interface'Class;
      Message : String)
   is
   begin
      This.Log (Info, Message);
   end Info;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (This : in out Log_Interface'Class;
      Message : String)
   is
   begin
      This.Log (Trace, Message);
   end Trace;

   ----------
   -- Warn --
   ----------

   procedure Warn
     (This : in out Log_Interface'Class;
      Message : String)
   is
   begin
      This.Log (Warn, Message);
   end Warn;

end Leander.Logging;
