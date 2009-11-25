{"leagues": [
     $config.("leagues"):{
     \{"name" : "$it.("leagueName")$",
      "divisions" : [
          $it.("divisions"):{
          \{"name" : "$it.("divisionName")$",
           "seasons" : [
               $it.("seasons"):{\{"name" : "$it.("seasonName")$", "link" : "$it.("relativeLink")$"\}};separator=","$
           ]
          \}};separator=","$
      ]
     \}};separator=","$
 ]
}
