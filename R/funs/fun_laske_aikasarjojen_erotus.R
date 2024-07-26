# Funktio, joka korjaa aikasarjan arvot vakioarvolla
# Vakioarvo lasketaan annetun referenssidatan ja korjattavan datan erotuksesta
# A:n on tarkoitus olla AM korkeusdataa, B:n referenssidataa (man.mittaukset)

# Funktio käyttää koko annettuja aikasarjoja korjausarvon laskemiseen!!
# Anturidrifti voi aiheuttaa sen että aikasarjan alkupää muuttuu virheellisesti
# Koska funktiota on tarkoitus käyttää vain tarkastellessa lähiaikojen(<2v) dataa,
# ei mahdollista driftiä ole huomioitu korjauksen laskemisessa.

# Debug
#A <- korkeus[korkeus$Paikka_Id == 49153,c("Aika","Korkeus")]
#B <- korkeus[korkeus$Paikka_Id == 43423,c("Aika","Korkeus")]


aikasarja_erotus <- function(A, B, drop_quant = 0.95) {
  stopifnot(c("Aika","Korkeus") %in% names(A), c("Aika","Korkeus") %in% names(B))
  
  # Hae toisiaan vastaavat A ja B aikasarjojen indeksit
  A_ind <- match(B$Aika, A$Aika) |> na.omit()
  B_ind <- match(A$Aika, B$Aika) |> na.omit()
  # Jätä jäljelle vain toisiaan vastaavat ajanhetket
  A <- A[A_ind, ]; B <- B[B_ind, ]
  
  # Laske aikasarjojen erotus pisteissä missä molemmista on dataa
  erot <- B$Korkeus - A$Korkeus
  # Laske absoluuttinen poikkeama (outlierien suodatusta varten)
  abs_erot <- abs(erot)
  
  # SIMPLE (keskiarvo erotuksista, joista pudotettu outlierit)
  erot_suod <- erot[abs_erot < DescTools::Quantile(abs_erot, probs = drop_quant)]
  res <- mean(erot_suod)

  # MONIMUTKAISET (ei toimi kunnolla)
  # # Suodata outlierit (pudottaa ne joilla abs.erotus yli .95 kvantiilissa)
  # A <- A[abs_erot < DescTools::Quantile(abs_erot, probs = drop_quant), ]
  # B <- B[abs_erot < DescTools::Quantile(abs_erot, probs = drop_quant), ]
  # x0 <- 0 # Optimisation starting value (most timeseries are aligned)
  
  # # Pienimmän neliösumman optimointi (vaihtoehtona myös lsfit())
  # res <- optim(c(x0), function(x) sum((B[["Korkeus"]] - (A[["Korkeus"]] + x))^2),
  #              method = "Brent", lower = -100, upper = 100)
  # res <- res$par # Otetaan vain saavutettu x:n arvo
  
  # Nonlinear Least Squares (much slower, can fail if residual=0)
  # res <- nls(b ~ a + x,
  #            data = data.frame("a" = A[["Korkeus"]], "b" = B[["Korkeus"]]),
  #            start=list(x=x0), )
  # res <- broom::tidy(res)$estimate

  # Palauta aikasarjojen täsmäämiseen sopivin luku 
  return(res)
}

# More debug:
# korj <- aikasarja_erotus(A, B, drop_quant = 0.95)
# ggplot() + geom_line(data=A, aes(y=Korkeus+korj, x=Aika))+
#   geom_point(data=B, aes(y=Korkeus, x=Aika)) +
#   scale_x_datetime(date_breaks = "year") +
#   theme(axis.text.x = element_text(angle = -90, vjust = 0.5)
 # ) +
 #  coord_cartesian(ylim=c(155,157.8))
