el_imputed_full <- el_imputed
el_imputed <- subset(el_imputed, man_only == 0)

el_full <- el
el <- subset(el, man_only == 0)