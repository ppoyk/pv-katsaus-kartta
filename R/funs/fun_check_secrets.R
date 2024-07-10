# Sisältää funktiot erilaisia skriptin vaatimien yhteyksien tarkistamista varten

# Tarkistaa tietokannan tiedot sisältävän tiedoston olemassaolon
# Tarvittaessa luo uuden ,kysyen sisällön käyttäjältä.
check_db_info <- function(file) {
  if (file.exists(file)){
    message("Tiedosto tietokantaan yhdistämistä varten löytyy ✅")
  } else {
    warning("##### Tiedot pohjavesitietokantaan yhdistämistä varten puuttuvat kohteesta",file)
    cat("\n#### Syötä tallennettavat tiedot ⏬\n")
    yaml::write_yaml(data.frame(
      palvelin = {cat("\n----- Syötä palvelimen nimi:"); readline()},
      tietokanta = {cat("\n----- Syötä tietokannan nimi:"); readline()}
    ),
    file = file(file, open = "w"))
  }
}


