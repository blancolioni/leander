package body Leander.Core.Bindings is

   ------------
   -- Insert --
   ------------

   procedure Insert
     (List    : in out Binding_List;
      Name    : String;
      Binding : Leander.Core.Trees.Tree_Type)
   is
   begin
      List.Map.Insert (Name, Binding);
   end Insert;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (List    : Binding_List;
      Process : not null access
        procedure (Name : String;
                   Tree : Leander.Core.Trees.Tree_Type))
   is
   begin
      for Position in List.Map.Iterate loop
         Process (Binding_Maps.Key (Position),
                  Binding_Maps.Element (Position));
      end loop;
   end Scan;

end Leander.Core.Bindings;
