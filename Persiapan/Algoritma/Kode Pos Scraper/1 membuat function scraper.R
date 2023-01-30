library(dplyr)
library(rvest)

scraper_kodepos = function(kode_cek){
  # sumber daripada sumber
  url      = "https://kodepos.id/"
  # tempel
  url_cek  = paste0(url,kode_cek)
  # ambil detail
  df = 
    url_cek %>% 
    read_html() %>% {
      tibble(
        provinsi  = html_nodes(.,"tr:nth-child(1) td:nth-child(1) a") %>% html_text() %>% .[1],
        kota_kab  = html_nodes(.,"tr:nth-child(1) td:nth-child(2) a") %>% html_text(),
        kecamatan = html_nodes(.,"tr:nth-child(1) td:nth-child(3) a") %>% html_text()
      )
    }
  if(nrow(df) < 1){df = data.frame(provinsi = NA,kota_kab = NA, kecamatan = NA)}
  df$kode_pos = kode_cek
  return(df)
}

