# Funktio, jolla avataan yhteys SQL-tietokantaan
# Kysyy käytäjältä tietokannan identifioivat tiedot, jos vastaavaa tiedostoa ei ole
# Kun tiedot ovat tiedostossa, funktio lukee arvot jatkossa sieltä
# Tiedoston kansiota "/data_source" tai "/secrets" ei seurata versionhallinnassa

.connect_db <- function(secrets_dir) {
  # Annetun polun tulee viedä kansioon jossa db_info niminen yaml sijaitsee
  # (Pääskriptissä määritelty muuttujaan dir_secrets)
  # Muutetaan annettu polku varalta uudelleen poluksi
  secrets_dir <- file.path(secrets_dir)
  
  # Aseta vaatimukset paketeista
  if (!requireNamespace(c("DBI", "obcd", "yaml"), quietly = TRUE)) {
    stop(
      "Packages \"DBI\", \"odbc\" and \"yaml\" must be installed to use this function.",
      call. = FALSE
  )}


  # Tarkista tallennuskansion olemassaolo
  if (!dir.exists(secrets_dir)) {
    stop("Tietokannan asetustiedoston kansiota ei ole!\nTarkista ", secrets_dir, call. = FALSE)
  }
  
  # Määritä polku tiedostonimeä myöten
  secrets_dir <- file.path(secrets_dir, "db_info.yaml")
  
  # Tarkista tietokannan osoitetiedoston olemassaolo
  if (!file.exists(secrets_dir)) {
    # Kysy db tietoja, ja kirjoita vastaukset tiedostoon, jota jatkossa käytetään
    writeLines("\n##### Yhteyttä ei voitu muodostaa\n##### Tiedot puuttuvat")
    yaml::write_yaml(data.frame(
        palvelin = {cat("------Syötä palvelimen nimi: "); readline()},
        tietokanta = {cat("------Syötä tietokannan nimi: "); readline()}
        ),
      file = file(secrets_dir, open = "w"))
  }

  # Siivous
  on.exit(expr = {rm(secrets_dir)
  })
  
  # Muodosta yhteys lukemalla rivit tiedostosta, ja
  # vie yhteys funktiota kutsuneeseen ympäristöön
  return(
    DBI::dbConnect(odbc::odbc(),
      .connection_string = paste0(
                             "driver={SQL Server};server=",
                             yaml::yaml.load_file(secrets_dir)[["palvelin"]],
                             ";database=",
                             yaml::yaml.load_file(secrets_dir)[["tietokanta"]],
                             ";trusted_connection=true"
                           )
    )
  )
}
