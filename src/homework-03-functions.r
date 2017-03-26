#
#                    Házi feladat 3     
#                    Programozás I.     
#                  2016/17. II. félév 
#                     Farsang Bence      
#                      2017.03.19.
#
#------------------------------------------------------
#                       V.feladat #
#------------------------------------------------------
#------------------------------------------------------
# 3.
#------------------------------------------------------
get_gender <- function(name) {
  # index of the superhero
  x  <-  which(comic_characters$name  ==  name)
  
  # if gender minority
  if (is.na(comic_characters$gsm)==TRUE) {
    
    # parameter 'y' shows the index of charachter 'C'
    y  <-  which(strsplit(comic_characters$sex[which(comic_characters$name  ==  name)],
                          "")[[1]]  ==  "C")
    # using substring to omit 'Charcters' part
    z  <-  substr(comic_characters$sex[which(comic_characters$name == name)], start = 1, stop = y-2)
    print(z)
    
    # if NOT gender minority
  } else {
    # parameter 'y' shows the index of charachter 'C'
    y  <-  which(strsplit(comic_characters$gsm[x], "")[[1]]  ==  "C")
    # using substring to omit 'Charcters' part
    z  <-  substr(comic_characters$gsm[x], start = 1, stop = y-2)
    print(z)
  }
}

#------------------------------------------------------
# 4.
#------------------------------------------------------

# Nézd meg az előzőleg írt get_gender függvénnyel, hogy milyen genderű Thor, 
# Katherine Pryde és Loki Laufeyson! Thornál és Lokinál egy vektort kell kapnod.

superheroes = c("Thor","Katherine Pryde","Loki Laufeyson")
superheroes = c("Thor (Thor Odinson)","Spider-Man (Peter Parker)")
supergender = sapply(superheroes, function(x) get_gender(x))
is.vector(supergender)

