module BadExportCon (OnlyCon) where
-- A capitalised name alone in an export list is a type or a class. This
-- module has only a constructor of that name, which needs Holder(OnlyCon).
data Holder = OnlyCon
