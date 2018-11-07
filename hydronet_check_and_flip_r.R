# title: "hydronet_check_and_flip_r"
# author: "Jeronimo Carranza / asterionat.com"
# date: "2018-10-2018"


library(sf)

arclist = function(arcid, tree, updw=0){
  arc <- tree[arcid,]
  node = st_line_sample(arc, sample = updw)
  arcs_id = st_intersection(tree, node)$HYNID
  arcs_id = arcs_id[arcs_id != arcid]
  return(arcs_id)  
}

checkarc = function(arcid, tree){
  uparcs = arclist(arcid, tree, 0)
  dwarcs = arclist(arcid, tree, 1)
  if (any(uparcs %in% checked)) {
    tree[arcid,]$geometry = flip(tree[arcid,])
    tocheck <<- c(tocheck, dwarcs)
    flipped <<- c(flipped, arcid)
  } else {
    tocheck <<- c(tocheck, uparcs)
  }
  return(tocheck)
}

flip = function(arc){
  crs_arc = st_crs(arc)
  prec_arc = st_precision(arc)
  coord = st_coordinates(arc)
  revcoord = apply(coord[,1:2], 2, rev)
  flipped = st_sfc(st_linestring(revcoord), crs = crs_arc, precision = prec_arc)
  return(flipped)
}

check = function(hynsf, return_op = flipped){
  checked <<- vector()
  tocheck <<- vector()
  flipped <<- vector()
  outfalls <- hynsf[hynsf$HYNOUT==1,]
  for (i in 1:nrow(outfalls)){
    checked <<- c(checked, outfalls[i,]$HYNID)
    tocheck <<- checkarc(outfalls[i,]$HYNID, hynsf)
  }
  n = 0
  while(n <= nrow(hynsf) & length(tocheck) > 0) {
    checked <<- c(checked, tocheck[1])
    tocheck <<- checkarc(tocheck[1], hynsf)
    tocheck <<- tocheck[-which(tocheck %in% checked)]
    n = n + 1
  }
  cat('Checked Arcs:', checked, '\n')
  cat('Flipped Arcs:', flipped, '\n')
  return(return_op)
}


# Replace with your data source 
# Reemplazar con su fuente de datos
rivers <- st_read("example_data/rivers02.shp")

# If HYNID (exclusive ID, integer) and HYNOUT (1 for outfalls and 0 for the others arcs) exist  
# in your data source, and they are correctly assigned, you can comment the following lines
# Si HYNID (ID exclusivo entero) y HYNOUT (1 para desembocaduras y 0 para otros arcos) existen
# en su fuente de datos y están correctamente asignados, puede comentar las líneas siguientes
rivers$HYNID = c(1:nrow(rivers))
rivers$HYNOUT = 0
rivers[c(125),]$HYNOUT = 1    # c(125) is the list of IDs of outfalls 
rivers[,]$HYNOUT

# Run this chunck to probe the script with your data 
# Ejecutar este chunck para probar el script con sus datos 
rivers_flipped = check(rivers, flipped)
rivers_flipped

# Run this chunck to persist the results. Replace with your data target.
# Ejecutar este chuck para salvar los resultados. Reemplazar con su destino.
st_write(rivers,"example_data/rivers02_updated.shp", delete_layer = TRUE)
