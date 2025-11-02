with Leander.Logging;
with Leander.Names;

package body Leander.Data_Types.Builder is

   -------------
   -- Add_Con --
   -------------

   procedure Add_Con
     (This   : in out Data_Type_Builder'Class;
      Name   : Leander.Core.Conid;
      Scheme : Leander.Core.Schemes.Reference)
   is
   begin
      This.Cons.Append
        (Con_Scheme'
           (Id => Name, Scheme => Nullable_Scheme_Reference (Scheme)));
   end Add_Con;

   -----------
   -- Build --
   -----------

   procedure Build
     (This   : in out Data_Type_Builder'Class)
   is

      function Pat_Id
        (Index : Positive)
         return String
      is ("~" & Character'Val (Character'Pos ('a')
          + Index + This.Cons.Last_Index - 1));

      function Var_Id
        (Index : Positive)
         return String
      is ("~" & Character'Val (Character'Pos ('a') + Index - 1));

      Var_Ids : constant Leander.Names.Name_Array :=
                  [for I in 1 .. This.Cons.Last_Index =>
                               Leander.Names.To_Leander_Name
                                 (Var_Id (I))];

      function Con_Arg_Count
        (Scheme : Leander.Core.Schemes.Reference)
         return Natural;

      function Create_Con_Record
        (Index : Positive)
         return Con_Record;

      -------------------
      -- Con_Arg_Count --
      -------------------

      function Con_Arg_Count
        (Scheme : Leander.Core.Schemes.Reference)
         return Natural
      is
         use type Core.Types.Reference;
         T : Core.Types.Reference := Scheme.Fresh_Instance;
         Count : Natural := 0;
      begin
         while T.Is_Application
           and then T.Left.Is_Application
           and then T.Left.Left = Core.Types.T_Arrow
         loop
            Count := Count + 1;
            T := T.Right;
         end loop;
         return Count;
      end Con_Arg_Count;

      -----------------------
      -- Create_Definition --
      -----------------------

      function Create_Con_Record
        (Index : Positive)
         return Con_Record
      is
         CR        : Con_Scheme renames This.Cons (Index);
         Id        : constant Core.Conid := CR.Id;
         Scheme    : constant Leander.Core.Schemes.Reference :=
                       Leander.Core.Schemes.Reference (CR.Scheme);
         Arg_Count : constant Natural := Con_Arg_Count (Scheme);
         Pat_Ids   : constant Leander.Names.Name_Array :=
                       [for I in 1 .. Arg_Count =>
                                      Leander.Names.To_Leander_Name
                                        (Pat_Id (I))];
         E         : Calculus.Tree :=
                       Calculus.Symbol (Var_Ids (Index));
      begin
         for Id of Pat_Ids loop
            E := Calculus.Apply (E, Calculus.Symbol (Id));
         end loop;
         for Id of reverse Var_Ids loop
            E := Calculus.Lambda (Id, E);
         end loop;
         for Id of reverse Pat_Ids loop
            E := Calculus.Lambda (Id, E);
         end loop;
         Leander.Logging.Log
           ("DATA",
            "create con" & Index'Image
            & ": " & Core.To_String (Id)
            & " :: "
            & Scheme.Show
            & " = "
            & Leander.Calculus.To_String (E));
         return Con_Record'
           (Con_Name => Id,
            Con_Type => Scheme,
            Con_Defn => E);
      end Create_Con_Record;

   begin
      This.DT := new Instance'
        (Con_Count => This.Cons.Last_Index,
         Id        => This.Tycon.Constructor.Id,
         Tycon     => Core.Types.Reference (This.Tycon),
         Kind      => This.Kind,
         Cons      => [for I in 1 .. This.Cons.Last_Index =>
                           Create_Con_Record (I)]);
   end Build;

   ---------------
   -- Data_Type --
   ---------------

   function Data_Type
     (This : Data_Type_Builder'Class)
      return Reference
   is
   begin
      return Reference (This.DT);
   end Data_Type;

   -----------
   -- Start --
   -----------

   procedure Start
     (This  : in out Data_Type_Builder'Class;
      Tycon : Leander.Core.Types.Reference;
      Kind  : Leander.Core.Kinds.Kind)
   is
   begin
      This.Id := Tycon.Constructor.Id;
      This.Kind := Kind;
      This.Tycon := Nullable_Type_Reference (Tycon);
      This.Cons.Clear;
      This.DT := null;
   end Start;

end Leander.Data_Types.Builder;
