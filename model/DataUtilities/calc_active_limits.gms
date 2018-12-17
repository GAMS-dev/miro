$title Alternative Pmin calculation

geninfo(gen,'Pmax','UWcalc') = geninfo(gen,'Pmax','given');

geninfo(gen,'Pmin','UWcalc') = 
                     (.69)*geninfo(gen,'Pmax','given')
               + (.42-.69)*geninfo(gen,'Pmax','given')$(geninfo(gen,'Pmax','given') > 200)
               + (.45-.42)*geninfo(gen,'Pmax','given')$(geninfo(gen,'Pmax','given') > 400)
               + (.48-.45)*geninfo(gen,'Pmax','given')$(geninfo(gen,'Pmax','given') > 600)
               + (.69-.48)*geninfo(gen,'Pmax','given')$(geninfo(gen,'Pmax','given') > 800)
;

