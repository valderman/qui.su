module DB.Migrations (DB.Migrations.migrate) where
import Database.Selda
import Database.Selda.Migrations hiding (migrate)
import DB.Migrations.AddRawQuizText as AddRawQuizText

migrate :: SeldaM b ()
migrate = autoMigrate False
  [ AddRawQuizText.migrate
  ]