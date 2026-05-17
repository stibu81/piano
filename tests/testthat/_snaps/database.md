# printing a chords database works

    Code
      print(chords_db, width = 100)
    Output
      # A tibble: 6 x 9
        type            notation name  left      right           rooted left_hand
        <chr>           <chr>    <chr> <chr>     <chr>           <lgl>  <lgl>    
      1 major           maj7     <NA>  3, 13, 9  "5, 7, 9"       FALSE  FALSE    
      2 minor           m7       <NA>  1, 5, b3  "11, b7, 9, 11" TRUE   FALSE    
      3 dominant        7        <NA>  b7, 3, 9  ""              FALSE  TRUE     
      4 half-diminished 7b5      <NA>  1, b3, b7 "9, #11, b7"    TRUE   FALSE    
      5 diminished      o7       <NA>  1, 1      "1, b3, b5, 6"  TRUE   FALSE    
      6 augmented       7        <NA>  1, 1      "b7, 3, #5"     TRUE   FALSE    
        top_degree alterations
        <chr>      <chr>      
      1 9          ""         
      2 11         ""         
      3 9          ""         
      4 b7         "#11"      
      5 6          "b5"       
      6 #5         "#5"       

