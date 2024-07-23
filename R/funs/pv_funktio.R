# Pohjavesidatan piirron pääfunktio

pv_funktio <- function(m_id, a_id, period, ref_vuosi_vali, plot_dir) {
  # period = kuvaajien aikaväli (c(alku,loppu))
  # m_id & a_id = tarkasteltavan manuaali-automaatti putkiparin IDt
  # Debug:
  # a_id <- ltaulu[20,1]; m_id <- ltaulu[20,2]
  # period <- c(as.Date("2022-01-01"), Sys.Date())
  # ref_vuosi_vali <- c(1980, 2020)
  # plot_dir <- file.path(D$output,"test")
  
  stopifnot(is.numeric(c(a_id,m_id)),
            is_date(period), length(period)==2, period[[1]]<period[[2]])
  requireNamespace("ggplot2"); requireNamespace("data.table")
  
  # Hae paikkojen tiedot
  paikka_a <- paikka[paikka$Paikka_Id == a_id, ]
  paikka_m <- paikka[paikka$Paikka_Id == m_id, ]
  
  # Hae havaintopaikkojen pv-alueen kokoluokka (tiedostopolkuja varten)
  # # Tarkista manuaali ja autom paikkojen kokoluokkien täsmääminen
  # pval_kokoluokat <- c(paikka_a$pvalue_kokoluokka, paikka_m$pvalue_kokoluokka)
  # if (pval_kokoluokat[1] == pval_kokoluokat[2]) {
  #   pval_kokolk <- pval_kokoluokat[1]
  # } else {
  #   stop("Linkitettyjen paikkojen pv-alueet eroavat",print(a_id),print(m_id))
  # }
  pval_kokolk <- paikka_m$pvalue_kokoluokka # LUOTETAAN TOISTAISEKSI MANUAALIIN
  
  if (nrow(paikka_a) < 1)
    stop("Linkkitaulun paikkaa ",a_id," ei löydy haetusta paikkojen taulusta")
  if (nrow(paikka_m) < 1)
    stop("Linkkitaulun paikkaa ",m_id," ei löydy haetusta paikkojen taulusta")
  
  # Hae paikkojen korkeusdata
  pdata_a <- korkeus[korkeus$Paikka_Id == paikka_a$Paikka_Id, ]
  pdata_m <- korkeus[korkeus$Paikka_Id == paikka_m$Paikka_Id, ]
  
  
  # Aseta piirrettävän jakson aikaväli
  start <- as.POSIXct(period[[1]])
  end <- as.POSIXct(period[[2]])

  
  # Muutetaan man.mittausten kuukausi factoriksi (referenssidatan laskentaa varten)
  pdata_m$month <- data.table::month(pdata_m$Aika) |> factor(levels = c(1:12))

  # Rajaa KAIKISTA man.mittauksista vertailujakso, josta lasketaan tilastoluvut
  pdata_m_ref <- subset(pdata_m,
                        data.table::year(pdata_m$Aika) >= ref_vuosi_vali[[1]] &
                          data.table::year(pdata_m$Aika) <= ref_vuosi_vali[[2]])
  
  # Rajaa manuaalimittaukset plottausvälille (ref.datan oton jälkeen)
  pdata_m <- subset(pdata_m, pdata_m$Aika >= start & pdata_m$Aika <= end)
  
  #Rajaa automaatin data plottausvälille (myös loppupäästä, yleensä ei päde)
  pdata_a <- subset(pdata_a, pdata_a$Aika >= start & pdata_a$Aika <= end)
  # Laske tarvittavat lisäsarakkeet ajalle
  pdata_a$month <- data.table::month(pdata_a$Aika)
  

  # Tarkista että löytyykö vertailujaksolta ollenkaan dataa
  if (nrow(pdata_m_ref) > 0) {
    # Hae toteutunut vertailuvuosien jakso plottauksen tekstejä varten
    min_t <- min(data.table::year(pdata_m_ref$Aika))
    max_t <- max(data.table::year(pdata_m_ref$Aika))
    vertailujakso_title <- paste("Vertailujakso",min_t,"-",max_t)
    
    # Laske kuukausiarvot päivittäisistä arvoista. Koottu yhteen tauluun.
    ref_df <- data.frame(
      aggregate(Korkeus ~ month, data=pdata_m_ref, FUN="mean", drop=F),
      aggregate(Korkeus ~ month, data=pdata_m_ref, FUN="min", drop=F)[,-1],
      aggregate(Korkeus ~ month, data=pdata_m_ref, FUN="max", drop=F)[,-1]
    )
  } else {
    vertailujakso_title <- paste("Ei vertailudataa jaksolta",
                                 ref_vuosi_vali[[1]],"-",ref_vuosi_vali[[2]])
    ref_df <- data.frame(matrix(NA, nrow=12, ncol=4))
    ref_df[,1] <- c(1:12)
  }
  colnames(ref_df) <- c("month", "ref_mean", "ref_min", "ref_max")
  

  # Tarkista että AM dataa löytyy piirtoväliltä. Generoi jos puuttuu (yhd_1 varten)
  if (nrow(pdata_a) < 1) {
    N.ref_df <- nrow(ref_df)
    #Jakson pituuden säätö vaadittu jotta nykhetk & siitä taaksepäin joka kk saa arvon 
    fake_aika <- seq.Date(period[[1]], period[[2]], length.out = N.ref_df + 1)[-1]
    pdata_a[1:N.ref_df,"Aika"]  <- fake_aika
    pdata_a[1:N.ref_df,"month"] <- data.table::month(fake_aika)#Tarv. yhdistämiseen
  }
  
  # Aineistojen yhdistys (AM data & man.mittausten kk-statsit vertailujaksolta)
  yht_1 <- base::merge(pdata_a, ref_df, by = "month", all = TRUE)
  
  
  # Tee datasta plotti
  plot <- ggplot(data = yht_1) +
    # Automaatin käyrä
    geom_line(aes(x = Aika, y = Korkeus)) +
    # Lisää ehdolliset tasot plottiin (eli plotataan vain jos voidaan)
    list(
      # Referenssiaikavälin tilastoarvot
      if (!all(is.na(yht_1$ref_mean))) {
        list(
         geom_step(aes(x = Aika, y = ref_mean), colour = "dodgerblue"),
         geom_step(aes(x = Aika, y = ref_min), colour = "red"),
         geom_step(aes(x = Aika, y = ref_max), colour = "red")
        )
      },
      # Manuaalimittausten pisteet
      if (nrow(pdata_m) > 0) {
        geom_point(data=pdata_m, aes(x = Aika, y = Korkeus))
        }
    ) +
    ggtitle(paste(paikka_a$Tunnus, paikka_m$Tunnus, vertailujakso_title)) +
    scale_x_datetime(date_breaks = "month") +
    theme(axis.text.x = element_text(angle = -90, vjust = 0.5))
  
  
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
  
  # Valmistele automaatin data tulostukseen
  yht_1 <- yht_1[!is.na(yht_1$Korkeus.x), ]
  # Paikan tuorein arvo ylimmäksi
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
