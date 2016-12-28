with Leander.Kinds.Trees;

package body Leander.Primitives is

   Local_Int_Type   : Leander.Types.Trees.Tree_Type;
   Local_List_Type  : Leander.Types.Trees.Tree_Type;
   Local_Empty_List : Leander.Types.Trees.Tree_Type;
   Local_Cons       : Leander.Types.Trees.Tree_Type;

   ----------
   -- Cons --
   ----------

   function Cons return Leander.Types.Trees.Tree_Type is
   begin
      return Local_Cons;
   end Cons;

   ----------------
   -- Empty_List --
   ----------------

   function Empty_List return Leander.Types.Trees.Tree_Type is
   begin
      return Local_Empty_List;
   end Empty_List;

   --------------
   -- Int_Type --
   --------------

   function Int_Type return Leander.Types.Trees.Tree_Type is
   begin
      return Local_Int_Type;
   end Int_Type;

   ---------------
   -- List_Type --
   ---------------

   function List_Type return Leander.Types.Trees.Tree_Type is
   begin
      return Local_List_Type;
   end List_Type;

   Type_Con_0  : constant Leander.Kinds.Trees.Tree_Type :=
                   Leander.Kinds.Trees.Leaf
                     (Leander.Kinds.Primitive);
   Type_Con_1  : constant Leander.Kinds.Trees.Tree_Type :=
                   Leander.Kinds.Trees.Apply
                     (Leander.Kinds.Trees.Apply
                        (Leander.Kinds.Map,
                         Type_Con_0),
                      Type_Con_0);
   Type_Con_2  : constant Leander.Kinds.Trees.Tree_Type :=
                   Leander.Kinds.Trees.Apply
                     (Leander.Kinds.Trees.Apply
                        (Leander.Kinds.Map,
                         Type_Con_0),
                      Type_Con_1);

begin
   Local_Int_Type :=
     Leander.Types.Trees.Leaf
       (Leander.Types.Constructor
          ("Int", Type_Con_0));

   Local_List_Type :=
     Leander.Types.Trees.Leaf
       (Leander.Types.Constructor
          ("[]", Type_Con_1));

   Local_Empty_List :=
     Leander.Types.Trees.Apply
       (Local_List_Type,
        Leander.Types.Variable ("a", Type_Con_0));

   Local_Cons :=
     Leander.Types.Trees.Apply
       (Leander.Types.Trees.Apply
          (Leander.Types.Constructor ("->", Type_Con_2),
           Leander.Types.Variable ("a", Type_Con_0)),
        Leander.Types.Trees.Apply
          (Local_List_Type,
           Leander.Types.Variable ("a", Type_Con_0)));

end Leander.Primitives;
