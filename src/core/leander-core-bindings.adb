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
      if List.Has_Signature (Name) then
         declare
            Rec : Value_Binding_Record := List.Map.Element (Name);
         begin
            Rec.Has_Value := True;
            Rec.Value := Binding;
            Rec.Value.Set_Annotation (Rec.Signature);
            List.Map.Replace_Element
              (List.Map.Find (Name), Rec);
         end;
      else
         List.Map.Insert
           (Name, (Has_Value => True, Value => Binding, others => <>));
      end if;
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (List      : in out Binding_List;
      Name      : String;
      Signature : Leander.Types.Trees.Tree_Type)
   is
   begin
      if List.Has_Binding (Name) then
         declare
            Rec : Value_Binding_Record := List.Map.Element (Name);
         begin
            Rec.Has_Signature := True;
            Rec.Signature := Signature;
            Rec.Value.Set_Annotation (Rec.Signature);
            List.Map.Replace_Element
              (List.Map.Find (Name), Rec);
         end;
      else
         List.Map.Insert
           (Name, (Has_Signature => True,
                   Signature     => Signature,
                   others        => <>));
      end if;
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
         if Binding_Maps.Element (Position).Has_Value then
            Process (Binding_Maps.Key (Position),
                     Binding_Maps.Element (Position).Value);
         else
            Process (Binding_Maps.Key (Position),
                     Trees.Empty);
         end if;
      end loop;
   end Scan;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (List    : Binding_List;
      Process : not null access
        procedure (Name : String;
                   Tree : Leander.Core.Trees.Tree_Type;
                   Signature : Leander.Types.Trees.Tree_Type))
   is
   begin
      for Position in List.Map.Iterate loop
         declare
            Element : constant Value_Binding_Record :=
                        Binding_Maps.Element (Position);
         begin
            Process (Binding_Maps.Key (Position),
                     (if Element.Has_Value then Element.Value
                      else Leander.Core.Trees.Empty),
                     (if Element.Has_Signature then Element.Signature
                      else Leander.Types.Trees.Empty));
         end;
      end loop;
   end Scan;

end Leander.Core.Bindings;
