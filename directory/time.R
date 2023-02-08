library(profvis)
profvis({ runApp('/cloud/project') }  
        , prof_output = '/cloud/project/directory')

profvis(prof_input = '/cloud/project/directory/filee6357d6794.Rprof') 

p <- profvis(prof_input = '/cloud/project/directory/filee6357d6794.Rprof')
htmlwidgets::saveWidget(p, "/cloud/project/directory/diagnosis.html")
