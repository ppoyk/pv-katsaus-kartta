# Pohjavesidatan piirron pääfunktio

pv_funktio <- function(m_id, a_id, period, ref_vuosi_vali, plot_dir) {
  # period = kuvaajien aikaväli (c(alku,loppu))
  # m_id & a_id = tarkasteltavan manuaali-automaatti putkiparin IDt
  # Debug # k <- i
          # latest_date <- Sys.Date()
  
  stopifnot(is.numeric(c(a_id,m_id)),
            is_date(period), length(period)==2, period[[1]]<period[[2]])
  
  # Hae paikkojen tiedot
  paikka_a <- paikka[paikka$Paikka_Id == a_id, ]
  paikka_m <- paikka[paikka$Paikka_Id == m_id, ]
  
  if (nrow(paikka_a) < 1)
    stop("Linkkitaulun paikkaa ",a_id," ei löydy haetusta paikkojen taulusta")
  if (nrow(paikka_m) < 1)
    stop("Linkkitaulun paikkaa ",m_id," ei löydy haetusta paikkojen taulusta")
  
  # Hae paikkojen korkeusdata
  pdata_a <- korkeus[korkeus$Paikka_Id == paikka_a$Paikka_Id, ]
  pdata_m <- korkeus[korkeus$Paikka_Id == paikka_m$Paikka_Id, ]
  
  
  seq_1 <- data.frame()
  # Aseta piirrettävän jakson aikaväli
  start <- as.POSIXct(period[[1]])
  # Aseta piirrettävän kuvaajan oikean reunan pvm
  end <- as.POSIXct(period[[2]])
  
  # Rajaa KAIKISTA man.mittauksista vertailujakso. Näytetään plottien taustalla
  pdata_m_ref <- subset(pdata_m,
                        data.table::year(pdata_m$Aika) >= ref_vuosi_vali[[1]] &
                          data.table::year(pdata_m$Aika) <= ref_vuosi_vali[[2]])
  
  seq <- seq(from = start, by = 60*60*24, to = end)
  day <- as.numeric(format(as.POSIXct(seq), format = "%d"))
  month <- as.numeric(format(as.POSIXct(seq), format = "%m"))
  year <- as.numeric(format(as.POSIXct(seq), format = "%Y"))
  date <- format(as.POSIXct(seq), format = "%Y-%m-%d")
  seq_1 <- cbind(seq, day, month, year, date)
  
  pdata_a$vuosi <- data.table::year(pdata_a$Aika)
  pdata_a$month <- data.table::month(pdata_a$Aika)
  pdata_a$day <- data.table::mday(pdata_a$Aika)
  pdata_a$pvm <- format(as.POSIXct(pdata_a$Aika, format = "%Y-%m-%d"), format = "%Y-%m-%d")
  
  pdata_a <- subset(pdata_a, pdata_a$pvm >= start)
  
  
  pdata_m$vuosi <- data.table::year(pdata_m$Aika)
  pdata_m$month <- data.table::month(pdata_m$Aika)
  #pdata_m$day <- format(as.POSIXct(pdata_m$Aika, format = "%Y-%m-%d"), format = "%d")
  pdata_m$day <- data.table::mday(pdata_m$Aika)
  
  # Ota manuaalimittauksista vertailujakso, joka näytetään plottien taustalla
  pdata_m_ref <- subset(pdata_m,
                        pdata_m$vuosi >= ref_vuosi_vali[[1]] &
                          pdata_m$vuosi <= ref_vuosi_vali[[2]])
  
  # Tarkista että löytyykö vertailujaksolta ollenkaan dataa
  if (nrow(pdata_m_ref) >= 1) {
    # Hae vielä vertailuvuosien jakso datasta plottauksen tekstejä varten
    min_t <- as.numeric(min(pdata_m_ref$vuosi))
    max_t <- as.numeric(max(pdata_m_ref$vuosi))
    vertailujakso_title <- paste("Vertailujakso",min_t,"-",max_t)
    
    pdata_m_ref$pvm <- format(as.POSIXct(pdata_m_ref$Aika), format = "%Y-%m-%d")
    
    # Laske jokaiselle vertailujakson kuukaudelle tilastoarvot (min, max, keski)
    kk_ref_stats <- data.frame()
    for (i in unique(pdata_m_ref$vuosi)) {
      for (j in unique(pdata_m_ref$month)) {
        yksi_kk <- subset(pdata_m_ref, pdata_m_ref$vuosi == i & pdata_m_ref$month == j)
        luku <- mean(as.numeric(yksi_kk$Korkeus), na.rm=T)
        luku2 <- min(as.numeric(yksi_kk$Korkeus), na.rm=T)
        luku3 <- max(as.numeric(yksi_kk$Korkeus), na.rm=T)
        yksi_kk_stats <- c(i, j, luku, luku2, luku3)
        kk_ref_stats <- rbind(kk_ref_stats, yksi_kk_stats)
      }
    }
    colnames(kk_ref_stats) <- c("vuosi", "kk", "arvo", "min", "max")
    kk_ref_stats <- na.omit(kk_ref_stats)
    
    # Laske kuukausikeskiarvot yksittäisten kuukausien tilastoista
    df <- data.frame()
    for (i in unique(kk_ref_stats$kk)) {
      keski <- mean(kk_ref_stats[kk_ref_stats$kk == i, "arvo"], na.rm=T)
      min <- min(kk_ref_stats[kk_ref_stats$kk == i, "arvo"], na.rm=T)
      max <- max(kk_ref_stats[kk_ref_stats$kk == i, "arvo"], na.rm=T)
      plus <- c(i, keski, min, max)
      
      df <- rbind(df, plus)
    }
    colnames(df) <- c("kk", "mean", "min", "max")
    
  } else {
    vertailujakso_title <- paste("Ei vertailudataa jaksolta",
                                 ref_vuosi_vali[[1]],"-",ref_vuosi_vali[[2]])
    df <- data.frame(matrix(NA, nrow=12, ncol=4))
    colnames(df) <- c("kk", "mean", "min", "max")
  }
  
  
  # Aineistojen yhdistys
  yht <- base::merge(seq_1, pdata_a, by.x = "date", by.y = "pvm", all = TRUE)
  yht_1 <- base::merge(yht, df, by.x = "month.x", by.y = "kk", all = TRUE)
  yht_1 <- base::merge(yht_1, pdata_m, by.x = "Aika", by.y = "Aika", all.x = TRUE)
  yht_1 <- base::merge(yht_1, paikka, by.x = "Paikka_Id.x", by.y = "Paikka_Id", all.x = TRUE)
  
  yht_1$Aika1 <- base::as.Date(as.POSIXct(yht_1$date, format = "%Y-%m-%d"))
  
  
  # Aineistojen plottaus
  # Jos kuukausittaiset referenssiarvot on voitu laskea, piirrä ne
  if (all(!is.na(df[[1]]))) {
    plot <- ggplot(data = yht_1) +
      geom_line(aes(x = yht_1$Aika1, y = yht_1$Korkeus.x)) +
      geom_step(aes(x = yht_1$Aika1, y = yht_1$mean, colour = "blue")) +
      geom_step(aes(x = yht_1$Aika1, y = yht_1$min, colour = "red")) +
      geom_step(aes(x = yht_1$Aika1, y = yht_1$max, colour = "red")) +
      geom_point(aes(x = yht_1$Aika1, y = yht_1$Korkeus.y)) +
      ggtitle(paste(paikka_a$Tunnus, paikka_m$Tunnus, vertailujakso_title)) +
      scale_x_date(date_breaks = "month") +
      theme(axis.text.x = element_text(angle = -90, vjust = 0.5))
    # plot(plot)
  # Piirrä ilman tilastollisiä käyriä jos niitä ei voinut laskea
  } else {
    plot <- ggplot(data = yht_1) +
      geom_line(aes(x = yht_1$Aika1, y = yht_1$Korkeus.x)) +
      geom_point(aes(x = yht_1$Aika1, y = yht_1$Korkeus.y)) +
      ggtitle(paste(paikka_a$Tunnus, paikka_m$Tunnus, vertailujakso_title)) +
      scale_x_date(date_breaks = "month") +
      theme(axis.text.x = element_text(angle = -90, vjust = 0.5))
  }
  
  
  # # Tarkista manuaali ja autom paikkojen kokoluokkien täsmääminen
  # pval_kokoluokat <- c(paikka_a$pvalue_kokoluokka, paikka_m$pvalue_kokoluokka)
  # if (pval_kokoluokat[1] == pval_kokoluokat[2]) {
  #   pval_kokolk <- pval_kokoluokat[1]
  # } else {
  #   stop("Linkitettyjen paikkojen pv-alueet eroavat",print(a_id),print(m_id))
  # }
  pval_kokolk <- paikka_m$pvalue_kokoluokka # LUOTETAAN TOISTAISEKSI MANUAALIIN
  
  # Aseta alakansiot ja tiedostonimet alueen koon perusteella
  if (pval_kokolk == "Pieni") {
    tall_nimi_1 <- file.path(plot_dir, "pieni", paste0(paikka_a$Tunnus, "_pv_pieni.png"))
    tall_nimi_2 <- file.path(plot_dir, paste0(paikka_a$Tunnus, "_pv_pieni.png"))
  } else if (pval_kokolk == "Keskikoko") {
    tall_nimi_1 <- file.path(plot_dir, "keskikoko", paste0(paikka_a$Tunnus, "_pv_keskikoko.png"))
    tall_nimi_2 <- file.path(plot_dir, paste0(paikka_a$Tunnus, "_pv_keskikoko.png"))
  } else if (pval_kokolk == "Suuri") {
    tall_nimi_1 <- file.path(plot_dir, "suuri", paste0(paikka_a$Tunnus, "_pv_suuri.png"))
    tall_nimi_2 <- file.path(plot_dir, paste0(paikka_a$Tunnus, "_pv_suuri.png"))
  } else stop(paste(
    "Paikan",ltaulu[k,"ManuaaliPaikka_Id"],"pv-alueen kokoluokkaa ei ole määritelty"))
  
  ggsave(tall_nimi_1, plot, create.dir=T)
  ggsave(tall_nimi_2, plot, create.dir=T)
  
  yht_1 <- yht_1[!is.na(yht_1$Korkeus.x), ]
  yht_1 <- yht_1[order(yht_1$date, decreasing = TRUE),]
  
  tulostus <- c(
    "Tunnus_a" = paikka_a$Tunnus, "Tunnus_m" = paikka_m$Tunnus,
    "Korkeus - ka" = yht_1[1, "Korkeus.x"] - yht_1[1, "mean"],
    "Korkeus - min" = yht_1[1, "Korkeus.x"] - yht_1[1, "min"],
    "Korkeus - max" = yht_1[1, "Korkeus.x"] - yht_1[1, "max"],
    "Korkeus" = yht_1[1, "Korkeus.x"],
    "Ka" = yht_1[1, "mean"], "Max" = yht_1[1, "max"],
    "Min" = yht_1[1, "min"], "Pvm" = yht_1[1, "date"],
    "KoordErLat" = paikka_a$KoordErLat, "KoordErLong" = paikka_a$KoordErLong,
    "PohjavesiAlue_Id" = yht_1[1, "PohjavesiAlue_Id"],
    "pvalue_kokoluokka" = pval_kokolk)
  
  # Pyöristä numerosarakkeet (ei koordinaattisarakkeita)
  for (s in names(koonti)[!names(koonti) %in% c("KoordErLat","KoordErLong")]) {
    if (is.numeric(koonti[[s]])) koonti[[s]] <- round(koonti[[s]], 2)
  }
  
  return(tulostus)
}
