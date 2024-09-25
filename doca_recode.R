requiredPackages = c('rlang','jsonlite')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

codebook<-function(data, var, labeler){
  result<-
    quo(factor({{var}}, levels=labeler$code, labels=labeler$label))|>
    eval_tidy(data=data)
  return(result)
}



speclist<-fromJSON("https://raw.githubusercontent.com/Neilblund/APAN/refs/heads/main/codebook.json")



# general claims
doca$gen_claim1 <-codebook(doca, claim1, speclist$general_claims)
doca$gen_claim2 <-codebook(doca, claim2, speclist$general_claims)
doca$gen_claim3 <-codebook(doca, claim3, speclist$general_claims)
doca$gen_claim4 <-codebook(doca, claim4, speclist$general_claims)

# claims
doca$claim1 <-codebook(doca, claim1, speclist$detail_claims)
doca$claim2 <-codebook(doca, claim2, speclist$detail_claims)
doca$claim3 <-codebook(doca, claim3, speclist$detail_claims)
doca$claim4 <-codebook(doca, claim4, speclist$detail_claims)



# target
doca$targ1 <-codebook(doca, targ1, speclist$targets)
doca$targ2 <-codebook(doca, targ2, speclist$targets)


# forms

doca$form1<- codebook(doca, form1, speclist$forms)
doca$form2<- codebook(doca, form2, speclist$forms)
doca$form3<- codebook(doca, form3, speclist$forms)
doca$form4<- codebook(doca, form4, speclist$forms)


# initiating groups
doca$igrp1c1 <-codebook(doca, igrp1c1, speclist$groups)
doca$igrp1c2 <-codebook(doca, igrp1c2, speclist$groups)

doca$igrp2c1 <-codebook(doca, igrp1c1, speclist$groups)
doca$igrp2c2 <-codebook(doca, igrp2c2, speclist$groups)

doca$igrp3c1 <-codebook(doca, igrp1c1, speclist$groups)
doca$igrp3c2 <-codebook(doca, igrp2c2, speclist$groups)


# valence
valence<-c("in favor", "in opposition", "unknown/NA")

doca$val1<-factor(doca$val1, labels=valence)
doca$val2<-factor(doca$val2, labels=valence)
doca$val3<-factor(doca$val3, labels=valence)
doca$val4<-factor(doca$val4, labels=valence)

doca$event_date<- with(doca, as.Date(paste0(evyy, "-", evmm, "-", evdd)))


