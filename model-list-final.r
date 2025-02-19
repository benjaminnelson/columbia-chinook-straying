final_models<- list(
  bf(
    mvbind(donor, recipient) ~ 1 + area + distsource + distance + localflow + escape + difftemp + localtemp + (1 + localflow + escape + difftemp + localtemp | population), 
    phi ~ 1,
    zi ~ 1 + area + distsource + distance + localflow + escape + difftemp + localtemp + (1 + localflow + escape + difftemp + localtemp | population),
    family = zero_inflated_beta()
  ),

  bf(
    mvbind(donor, recipient) ~ 1 + as.factor(year) + area + distsource + distance + localflow + escape + difftemp + localtemp + (1 + localflow + escape + difftemp + localtemp | population), 
    phi ~ 1,
    zi ~ 1 + area + distsource + distance + localflow + escape + difftemp + localtemp + (1 + localflow + escape + difftemp + localtemp | population),
    family = zero_inflated_beta()
  ),

  bf(
    mvbind(donor, recipient) ~ 1 + area + distsource + distance + localflow + escape + difftemp + contemp + (1 + localflow + escape + difftemp + contemp | population), 
    phi ~ 1,
    zi ~ 1 + area + distsource + distance + localflow + escape + difftemp + contemp + (1 + localflow + escape + difftemp + contemp | population),
    family = zero_inflated_beta()
  ),

  bf(
    mvbind(donor, recipient) ~ 1 + as.factor(year) + area + distsource + distance + localflow + escape + difftemp + contemp + (1 + localflow + escape + difftemp + contemp | population), 
    phi ~ 1,
    zi ~ 1 + area + distsource + distance + localflow + escape + difftemp + contemp + (1 + localflow + escape + difftemp + contemp | population),
    family = zero_inflated_beta()
  ),
  # 5
  bf(
    mvbind(donor, recipient) ~ 1 + area + distsource + distance + localflow + escape + difftemp + localtemp + (1 | population), 
    phi ~ 1,
    zi ~ 1 + area + distsource + distance + localflow + escape + difftemp + localtemp + (1 | population),
    family = zero_inflated_beta()
  ),

  bf(
    mvbind(donor, recipient) ~ 1 + as.factor(year) + area + distsource + distance + localflow + escape + difftemp + localtemp + (1 | population), 
    phi ~ 1,
    zi ~ 1 + area + distsource + distance + localflow + escape + difftemp + localtemp + (1 | population),
    family = zero_inflated_beta()
  ),

  bf(
    mvbind(donor, recipient) ~ 1 + area + distsource + distance + localflow + escape + difftemp + contemp + (1 | population), 
    phi ~ 1,
    zi ~ 1 + area + distsource + distance + localflow + escape + difftemp + contemp + (1 | population),
    family = zero_inflated_beta()
  ),

  bf(
    mvbind(donor, recipient) ~ 1 + as.factor(year) + area + distsource + distance + localflow + escape + difftemp + contemp + (1 | population), 
    phi ~ 1,
    zi ~ 1 + area + distsource + distance + localflow + escape + difftemp + contemp + (1 | population),
    family = zero_inflated_beta()
  ),

  bf(
    mvbind(donor, recipient) ~ 1 + area + distsource + distance + localflow + escape + difftemp + localtemp + (1 + localflow + escape + difftemp + localtemp | population), 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),
  # 10
  bf(
    mvbind(donor, recipient) ~ 1 + as.factor(year) + area + distsource + distance + localflow + escape + difftemp + localtemp + (1 + localflow + escape + difftemp + localtemp | population), 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),

  bf(
    mvbind(donor, recipient) ~ 1 + area + distsource + distance + localflow + escape + difftemp + contemp + (1 + localflow + escape + difftemp + contemp | population), 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),

  bf(
    mvbind(donor, recipient) ~ 1 + as.factor(year) + area + distsource + distance + localflow + escape + difftemp + contemp + (1 + localflow + escape + difftemp + contemp | population), 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),

  bf(
    mvbind(donor, recipient) ~ 1 + area + distsource + distance + localflow + escape + difftemp + localtemp + (1 | population), 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),

  bf(
    mvbind(donor, recipient) ~ 1 + as.factor(year) + area + distsource + distance + localflow + escape + difftemp + localtemp + (1 | population), 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),
  #15
  bf(
    mvbind(donor, recipient) ~ 1 + area + distsource + distance + localflow + escape + difftemp + contemp + (1 | population), 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),

  bf(
    mvbind(donor, recipient) ~ 1 + as.factor(year) + area + distsource + distance + localflow + escape + difftemp + contemp + (1 | population), 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),
  # 17
  bf(
    mvbind(donor, recipient) ~ 1 + (1 | population), 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),
  
  bf(
    mvbind(donor, recipient) ~ 1 + as.factor(year), 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),

  bf(
    mvbind(donor, recipient) ~ 1 + as.factor(year) + (1 | population), 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),
  
  bf(
    mvbind(donor, recipient) ~ 1 + distance, 
    phi ~ 1,
    zi ~ 1 + distance,
    family = zero_inflated_beta()
  ),
  
  bf(
    mvbind(donor, recipient) ~ 1 + distsource, 
    phi ~ 1,
    zi ~ 1 + distsource,
    family = zero_inflated_beta()
  ),
  
  bf(
    mvbind(donor, recipient) ~ 1 + area, 
    phi ~ 1,
    zi ~ 1 + area,
    family = zero_inflated_beta()
  ),
  
  bf(
    mvbind(donor, recipient) ~ 1 + (1 + contemp|population), 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),
  
  bf(
    mvbind(donor, recipient) ~ 1 + (1 + localtemp|population), 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),
  
  # 25
  bf(
    mvbind(donor, recipient) ~ 1 + (1 + escape|population), 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),
  
  bf(
    mvbind(donor, recipient) ~ 1 + (1 + difftemp|population), 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),
  
  bf(
    mvbind(donor, recipient) ~ 1 + (1 + localflow|population), 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),
  
  bf(
    mvbind(donor, recipient) ~ 1 + contemp, 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),
  
  bf(
    mvbind(donor, recipient) ~ 1 + localtemp, 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),
  
  bf(
    mvbind(donor, recipient) ~ 1 + escape, 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),
  
  bf(
    mvbind(donor, recipient) ~ 1 + difftemp, 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  ),
  
  bf(
    mvbind(donor, recipient) ~ 1 + localflow, 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
    ),
    
    bf(
      mvbind(donor, recipient) ~ 1 + contemp, 
      phi ~ 1,
      zi ~  1 + contemp,
      family = zero_inflated_beta()
    ),
    
  bf(
      mvbind(donor, recipient) ~ 1 + localtemp, 
      phi ~ 1,
      zi ~ 1 + localtemp,
      family = zero_inflated_beta()
    ),
    
  bf(
    mvbind(donor, recipient) ~ 1 + escape, 
    phi ~ 1,
    zi ~ 1 + escape,
    family = zero_inflated_beta()
  ),
  
  bf(
    mvbind(donor, recipient) ~ 1 + difftemp, 
    phi ~ 1,
    zi ~ 1 + difftemp,
    family = zero_inflated_beta()
  ),
  
  bf(
    mvbind(donor, recipient) ~ 1 + localflow, 
    phi ~ 1,
    zi ~ 1 + localflow,
    family = zero_inflated_beta()
  ),
  
  bf(
    mvbind(donor, recipient) ~ 1, 
    phi ~ 1,
    zi ~ 1,
    family = zero_inflated_beta()
  )
)

final_models_uncor<- list(
  bf(
  mvbind(donor, recipient) ~ 1 + area + distsource + distance + localflow + escape + difftemp + localtemp + (1 + localflow + escape + difftemp + localtemp |i| population), 
  phi ~ 1,
  zi ~ 1 + area + distsource + distance + localflow + escape + difftemp + localtemp + (1 + localflow + escape + difftemp + localtemp |i| population),
  family = zero_inflated_beta()
  ),
  
  bf(
    mvbind(donor, recipient) ~ 1 + area + distsource + distance + localflow + escape + difftemp + contemp + (1 + localflow + escape + difftemp + contemp |i| population), 
    phi ~ 1,
    zi ~ 1 + area + distsource + distance + localflow + escape + difftemp + contemp + (1 + localflow + escape + difftemp + contemp |i| population),
    family = zero_inflated_beta()
  )
)