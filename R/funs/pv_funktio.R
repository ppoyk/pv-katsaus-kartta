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
  
  
  # Aseta piirrettävän jakson aikaväli
  start <- as.POSIXct(period[[1]])
  # Aseta piirrettävän kuvaajan oikean reunan pvm
  end <- as.POSIXct(period[[2]])

  
  # Käsitellään manuaalimittaukset
  pdata_m$vuosi <- data.table::year(pdata_m$Aika)
  pdata_m$month <- data.table::month(pdata_m$Aika) |> factor(levels = c(1:12))
  pdata_m$day <- data.table::mday(pdata_m$Aika)
  
  # Rajaa KAIKISTA man.mittauksista vertailujakso. Näytetään plottien taustalla
  pdata_m_ref <- subset(pdata_m,
                        data.table::year(pdata_m$Aika) >= ref_vuosi_vali[[1]] &
                          data.table::year(pdata_m$Aika) <= ref_vuosi_vali[[2]])
  
  # Rajaa manuaalimittaukset plottausvälille
  pdata_m <- subset(pdata_m, pdata_m$Aika >= start & pdata_m$Aika <= end)
  
  
  #Rajaa automaatin data halutulle aikavälille (myös loppupäästä, yleensä ei päde)
  pdata_a <- subset(pdata_a, pdata_a$Aika >= start & pdata_a$Aika <= end)
  # TODO Tarvitaanko tätä ??
  pdata_a$vuosi <- data.table::year(pdata_a$Aika)
  pdata_a$month <- data.table::month(pdata_a$Aika)
  pdata_a$day <- data.table::mday(pdata_a$Aika)
  pdata_a$pvm <- format(as.POSIXct(pdata_a$Aika, format="%Y-%m-%d"), format="%Y-%m-%d")
  

  # Tarkista että löytyykö vertailujaksolta ollenkaan dataa
  if (nrow(pdata_m_ref) > 0) {
    # Hae vielä vertailuvuosien jakso datasta plottauksen tekstejä varten
    min_t <- min(pdata_m_ref$vuosi)
    max_t <- max(pdata_m_ref$vuosi)
    vertailujakso_title <- paste("Vertailujakso",min_t,"-",max_t)
    
    pdata_m_ref$pvm <- format(as.POSIXct(pdata_m_ref$Aika), format = "%Y-%m-%d")
    
    # Laske kuukausiarvot päivittäisistä arvoista. Koottu yhteen tauluun.
    ref_df <- data.frame(
      aggregate(Korkeus ~ month, data=pdata_m_ref, FUN="mean", drop=F),
      aggregate(Korkeus ~ month, data=pdata_m_ref, FUN="min", drop=F)[,-1],
      aggregate(Korkeus ~ month, data=pdata_m_ref, FUN="max", drop=F)[,-1]
    )
    colnames(ref_df) <- c("month", "ref_mean", "ref_min", "ref_max")
    
  } else {
    vertailujakso_title <- paste("Ei vertailudataa jaksolta",
                                 ref_vuosi_vali[[1]],"-",ref_vuosi_vali[[2]])
    ref_df <- data.frame(matrix(NA, nrow=12, ncol=4))
    ref_df[,1] <- c(1:12)
    colnames(ref_df) <- c("month", "ref_mean", "ref_min", "ref_max")
  }
  
  
  # Aineistojen yhdistys
  yht_1 <- base::merge(pdata_a, ref_df, by = "month", all = TRUE)
  
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
  tall_nimi_alakans <- switch(as.character(pval_kokolk),
    "Pieni"     = f.path(plot_dir,"pieni",    p0(paikka_a$Tunnus,".png")),
    "Keskikoko" = f.path(plot_dir,"keskikoko",p0(paikka_a$Tunnus,".png")),
    "Suuri"     = f.path(plot_dir,"suuri",    p0(paikka_a$Tunnus,".png")),
    stop(p("Virheellinen PValueen kokoluokka:",pval_kokolk,"\t-",paikka_m,m_id))
  )
  tall_nimi_all     <- switch(as.character(pval_kokolk),
    "Pieni"     = f.path(plot_dir, p0(paikka_a$Tunnus, "_pv_pieni.png")),
    "Keskikoko" = f.path(plot_dir, p0(paikka_a$Tunnus, "_pv_keskikoko.png")),
    "Suuri"     = f.path(plot_dir, p0(paikka_a$Tunnus, "_pv_suuri.png")),
    stop(p("Virheellinen PValueen kokoluokka:",pval_kokolk,"\t-",paikka_m,m_id))
  )
  
  ggsave(tall_nimi_alakans, plot, create.dir = T)
  ggsave(tall_nimi_all,     plot, create.dir = T)
  
  yht_1 <- yht_1[!is.na(yht_1$Korkeus.x), ]
  yht_1 <- yht_1[order(yht_1$Aika, decreasing = TRUE),]
  
  tulostus <- c(
    "Tunnus_a" = paikka_a$Tunnus, "Tunnus_m" = paikka_m$Tunnus,
    "asema_tunnus" = paikka_m$asema_tunnus, "asema" = paikka_m$asema_nimi,
    "ref_ka_delta"  = yht_1[1, "Korkeus"] - yht_1[1, "ref_mean"],
    "ref_min_delta" = yht_1[1, "Korkeus"] - yht_1[1, "ref_min"],
    "ref_max_delta" = yht_1[1, "Korkeus"] - yht_1[1, "ref_max"],
    "Korkeus" = yht_1[1, "Korkeus"],
    "ref_ka" = yht_1[1, "ref_mean"], "ref_max" = yht_1[1, "ref_max"],
    "ref_min" = yht_1[1, "ref_min"], "Pvm" = yht_1[1, "Aika"],
    "KoordErLat" = paikka_a$KoordErLat, "KoordErLong" = paikka_a$KoordErLong,
    "PohjavesiAlue_Id" = yht_1[1, "PohjavesiAlue_Id"],
    "pvalue_kokoluokka" = pval_kokolk)
  
  # Pyöristä numerosarakkeet (ei koordinaattisarakkeita)
  for (s in names(tulostus)[!names(tulostus) %in% c("KoordErLat","KoordErLong")]) {
    if (is.numeric(tulostus[[s]])) tulostus[[s]] <- round(tulostus[[s]], 2)
  }
  
  return(tulostus)
}
