# ==========================================================
# PALETAS
# ==========================================================

pal_bw_accent <- function(domain, accent=ACCENT) {
  colorNumeric(
    palette  = grDevices::colorRampPalette(c("#111111","#FFFFFF",accent))(256),
    domain   = domain,
    na.color = "#00000000"
  )
}

make_pal_pos <- function(values, scale=c("linear","quantile"), accent=ACCENT) {
  scale <- match.arg(scale)
  v  <- suppressWarnings(as.numeric(values)); v[!is.finite(v)] <- NA_real_
  dom <- v[is.finite(v)]
  if (length(dom)<2L||length(unique(dom))<2L) {
    pal <- pal_bw_accent(c(0,1),accent=accent)
    return(list(pal=pal, values=ifelse(is.na(v),NA_real_,0)))
  }
  if (scale == "quantile") {
    br <- as.numeric(quantile(dom,probs=seq(0,1,length.out=8),na.rm=TRUE,type=7))
    br <- unique(br[is.finite(br)])
    if (length(br)>=3L) {
      n_bins <- max(3L,length(br)-1L)
      cols   <- grDevices::colorRampPalette(c("#111111","#FFFFFF",accent))(n_bins)
      pal    <- colorBin(cols,bins=br,domain=dom,na.color="#00000000",pretty=FALSE)
      return(list(pal=pal, values=v))
    }
  }
  pal <- pal_bw_accent(dom, accent=accent)
  list(pal=pal, values=v)
}

make_pal_delta <- function(values, scale=c("linear","quantile"), accent=ACCENT) {
  scale <- match.arg(scale)
  v  <- suppressWarnings(as.numeric(values)); v[!is.finite(v)] <- NA_real_
  dom <- v[is.finite(v)]
  if (length(dom)<2L||length(unique(dom))<2L) {
    pal <- pal_bw_accent(c(-1,1),accent=accent)
    return(list(pal=pal, values=ifelse(is.na(v),NA_real_,0)))
  }
  if (scale=="quantile") {
    br <- as.numeric(quantile(dom,probs=seq(0,1,length.out=8),na.rm=TRUE,type=7))
    br <- unique(br[is.finite(br)])
    if (length(br)>=3L) {
      n_bins <- max(3L,length(br)-1L)
      cols   <- grDevices::colorRampPalette(c("#111111","#FFFFFF",accent))(n_bins)
      pal    <- colorBin(cols,bins=br,domain=dom,na.color="#00000000",pretty=FALSE)
      return(list(pal=pal, values=v))
    }
  }
  M <- as.numeric(quantile(abs(dom),probs=0.98,na.rm=TRUE,type=7))
  if (!is.finite(M)||M<=0) M <- max(abs(dom),na.rm=TRUE)
  if (!is.finite(M)||M<=0) M <- 1
  pal <- pal_bw_accent(c(-M,M), accent=accent)
  list(pal=pal, values=v)
}
