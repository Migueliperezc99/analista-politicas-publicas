# Sesión 5 - Git - GitHub

install.packages("usethis")
library(usethis)

use_git_config(
  user.name  = "Miguel Perez",              # tu nombre
  user.email = "migueliperezc99@gmail.com"  # tu correo (usa el mismo de GitHub)
)

usethis::create_github_token()
gitcreds::gitcreds_set()

usethis::use_git()

usethis::use_github()

# Completa sesión 5: configuración Git + primer push a GitHub