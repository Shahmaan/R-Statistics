#Triangulation!

#Här är ett exempel på god kodvana - en mainfunktion som kallar på två hjälpfunktioner.

#Mainfunktionen heter circle_intersection och tar in behandlad mätdata från 
#dataframen.
circle_help_function <- function(circ1, circ2, tol){
  #Vi börjar med att namnge våra variabler och lägga till toleransparameter
  x1 <- circ1[1]
  y1 <- circ1[2]
  r1 <- circ1[3] + tol
  x2 <- circ2[1]
  y2 <- circ2[2]
  r2 <- circ2[3] + tol
  
  #Räknar ut intersection points.
  #Analytiskt uttryck som tar lång tid att komma fram till...
  #Här kollar vi på två punkter som två cirklar skär i varandra.

  bigR <- sqrt((x2-x1)^2+(y2-y1)^2)
  tmp_vect <- c(y2-y1,x1-x2)
  sqrt_term <- (1/2)*sqrt(2*(r1^2+r2^2)/(bigR^2) - (r1^2-r2^2)^2/(bigR^4)-1)
  diff <- sqrt_term*tmp_vect
  origin_points <- (1/2)*(c(x1 + x2, y1 + y2)) + (r1^2-r2^2)/(2*bigR^2)*c(x2 - x1, y2 - y1)
  coords1 <- origin_points + diff
  coords2 <- origin_points - diff
  
  result_coords <- rbind(coords1, coords2)

  return(result_coords)
  
  
  
}

true_intersection <- function(coordinate_matrix1, coordinate_matrix2){
  #Ska ta in alla intersections och se vilka som ligger närmast varandra!
  #
  #Best value sätts till ett godtyckligt, högt tal
  best_value <- 100000000
  index <- 0
  for(i in 1:nrow(coordinate_matrix1)){
    for(j in 1:nrow(coordinate_matrix2)){
      #Räkna ut L2-norm för varje koordinatpunkt - uppdatera 
      #index ifall vi har en bra överensstämmelse!
      val <- norm(coordinate_matrix1[i, ] - coordinate_matrix2[j, ], "2")
      if(is.na(val)){
        print("Skalade du verkligen avståndet med velocity?")
      }
      if(val < best_value){
        best_value <- val
        index <- i
        
      }
    }
  }
  true_intersect <- coordinate_matrix1[index, ]
  return(toString(true_intersect))
}


#Ska returna 2x2 matris för intersection points som input...

circle_intersection <- function(circle_mat, tol = 0.00005){
  #circle_mat 3x3-matris med x_koord_mast, y_koord_mast, avstånd_tillkälla
  #för Mast 1, 2 och 3.
  #Mainfunktion. Alltid bra för överblicken att ha en mainfunktion 
  #som kallar på "hjälpfunktioner"!
  #Kommer att ge tillbaks en stringrepresentation av koordinaterna där det 
  #första värdet är x-koordinat och det andra värdet är y-koordinat
  
  #tol för numerisk stabilitet - undviker avrundningsfel som omöjliggör en 
  #analytisk lösning!
  circle1 <- circle_mat[1, ]
  circle2 <- circle_mat[2, ]
  circle3 <- circle_mat[3, ]

  
  coordinates1 <- circle_help_function(circle1, circle2, tol)
  coordinates2 <- circle_help_function(circle1, circle3, tol)
  
  true_intersect <- true_intersection(coordinates1, coordinates2)
  
  
}


