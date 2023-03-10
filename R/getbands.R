getbands <- function(ls){
    if(ls == 4){bands <- c("SR_B1","SR_B2","SR_B3","SR_B4", "SR_B5", "QA_PIXEL")}
    if(ls == 5){bands <- c("SR_B1","SR_B2","SR_B3","SR_B4", "SR_B5", "QA_PIXEL")}
    if(ls == 7){bands <- c("SR_B1","SR_B2","SR_B3","SR_B4", "SR_B5", "QA_PIXEL")}
    if(ls == 8){bands <- c("SR_B1","SR_B2","SR_B3","SR_B4", "SR_B5","SR_B6", "QA_PIXEL")}
    if(ls == 9){bands <- c("SR_B1","SR_B2","SR_B3","SR_B4", "SR_B5","SR_B6", "QA_PIXEL")}
  return(bands)
}
