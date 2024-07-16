# Tarkistaa kansion olemassaolon projektin rakennetta varten
# Tarvittaessa luo puuttuvan kansion, ja ilmoittaa käyttäjälle
# Osaa ohittaa myös tiedostoihin viittaavat polut

check_dir <- function(..., tilde = NULL, ensure = FALSE) {
  
  dir <- file.path(...) # Luodaan kaikista kohteista tiedostopolku
  
  if (!is.null(tilde)) {
    if(file.exists(tilde)) {
      # Jos juurikansio on määritelty, käytetään polun lyhyttä muotoa, eli
      # trimmataan alusta juuriosa pois (tällöin printissä myös parempi muotoilu)
      dir <- gsub(paste0(tilde,"/"), "", dir)
    } else stop(paste("File path shortening not found:",tilde))
  }
  
  if (dir.exists(dir)) { # Jos kansio on olemassa, hyvä.
    message("Kansio ",dir," löytyy levyltä ✅")
    
  } else if (!file_test("-d", dir)) { # Jos ei ole olemassaoleva kansio
    # Jos on polku tiedostoon jota ei olemassa (polun loppu: piste + 1-5 kirjainta)
    if (file_test("-f", dir) || grepl("\\..{1,5}$", dir)) { 
      message("Polku ",dir," on tiedosto! Tarkistetaan isäntäkansio...")
      # Otetaan isäntäkansio, ja aloitetaan alusta. Useimmiten turha tarkistus,
      # ellei '...' määrittele polkua tiedostolle alakansioon jota ei vielä ole
      check_dir(dirname(dir), tilde=tilde, ensure=ensure) 
    # Jos ei ole polku tiedostoon, mutta nimessä on piste
    } else if (grepl("\\.", dir)) { 
        stop("Kansion nimeen ei haluta pistettä! (",dir,")")
    } else { # Jos ei ole tiedosto eikä olemassaoleva kansio, luodaan kansio jos pyydetty
      if (ensure) {
        warning("HUOM! Kansiota ei havaittu levyllä: ",dir,"\n")
        dir.create(dir, recursive = T); message("Kansio luotu: ",dir)
      } else stop("HUOM! Kansiota ei havaittu levyllä: ",dir,"\n")
    }
  }
  return(dir) # Palautetaan polku, jotta funktiota voidaan käyttää kuten file.path()
}

