#install.packages("rjson")
library("rjson")                                                               #Convertit l'objet R en objets JSON et vice-versa
library("rvest")                                                               #Un package pour le web scraping et l'analyse


##################################################  Fonction qui permet de retirer le lien de la page suivante par rapport a la page courante

next_page<-function(x){
  
  url <- paste('https://dblp.uni-trier.de/pers?pos=',x,'.html',sep="")          #Extraire l'url de la page courante x
  page <- read_html(url)                                                        #Lecture du html de la page correspondant a l'url
  html_nodes(page, "a[href][href^='?pos=']") %>% html_attr("href")              #Extraire le lien de la page qui suit la page courante qui se trouve dans une balise a ou utilisant
                                                                                #un patern avec les expression reguliere
  
}


##################################################  Fonction qui effectue l'extraction des noms d'auteurs d'une page courante

extraction_author<-function(x){
  url <- paste('https://dblp.uni-trier.de/pers?pos=',
               x,
               '.html',
               sep="")                                                        #Extraire l'url de la page courante x
  page <- read_html(url)                                                      #Lecture du html de la page correspondant a l'url
  t<-list()                                                                   #Initialisation d'une liste t
  t<-html_nodes(page, "a[href][href^='https://dblp.uni-trier.de/pers/hd/']") %>% html_attr("href")#Extraire le lien de la page qui suit la page courante qui se trouve dans une balise a ou utilisant
                                                                                                  #un patern avec les expression reguliere
  return(t)                                                                   #Retourner la liste contenant les noms d'auteurs de la page courante
  
}

##################################################  Fonction qui effectue l'extraction des article d'un auteur ainsi que les information relative aux article

info_author<-function(x){
  
  
  author_liste<-extraction_author(1)                                          #Extraction de la liste contenant les noms d'auteurs d'une page courante
  p<-matrix(ncol = 13)                                                        #Initialisation d'une matrice
  
  for (i in c(1:length(author_liste))){                                       #Boucle sur chaque auteur de la liste des auteurs
    
    url<-author_liste[i]                                                      #Prendre l'url de l'auteur correspondant
    page <- read_html(url)                                                    #Lecture de la page html de l'url
    title_html<-html_nodes(page, "span[class][class='title']")                #Extraction des noeud de la balise span suivant le type de class
    title<-html_text(title_html)                                              #Extraction du text de chaque noeud retirer precedement 
    t<-0                                                                      #Initialisation a 0 d'une variable 
  
    
    
    for (i in c(1:length(title))){                                            #Parcourire les titre de chaque article d'un auteur
      
      
      print(title[i])                                                         #Affichage du titre de chaque article
      url<-paste("https://dblp.org/search/publ/api?q=",title[i],"&format=json",sep="")#Affectation de l'url de chaque titre dans une variable url et utilisation de l'API
      result <- fromJSON(file = url)                                          #Avoir le resultat de l'execution de l'api sous format json
      result<-as.data.frame(t(result))                                        #Transformation du resultat du format json en data frame
    
    
      if(length(result$result$result$hits$hit[[1]]$info$type)==0){            #Verification le type de l'article si ce parametre n'est pas préciser par l'API
        type<-c("/")                                                          #Dans le cas ou le type n'est pas spécifier
        
      
      }else{
        type<-c(result$result$result$hits$hit[[1]]$info$type)                #Prendre dans le cas ou l'API fournie le resultat
        
      }
    
      if(type=="Journal Articles"){                                          #Filtre tous les journaux article de l'auteur
      
        auteur<-list()                                                       #Initilisation de la liste des auteur qui on contribuer dans l'article 
        pid<-list()                                                          #Initilisation de la liste des pids qui on contribuer de chaque auteur
        type_liste<-typeof(result$result$result$hits$hit[[1]]$info$authors$author[[1]])#Vérifie le type pour connaitre la structure du fichier json
        if(type_liste=="list"){                                              #Dans le cas ou c'est une liste
      
          for (i in c(1:length(result$result$result$hits$hit[[1]]$info$authors$author))){#on extrait chaque auteur et chaque pid de chaque auteur
            auteur<-append(auteur, (result$result$result$hits$hit[[1]]$info$authors$author[[i]]$text))#on met les auteur dans une liste
            pid<-append(pid, (result$result$result$hits$hit[[1]]$info$authors$author[[i]]$'@pid'))#on met les pid de chaque auteur dans une liste
          }
      
        aut<-""                                                              #Initialisation d'une chaine de caractere qui vas contenire les auteur
        pid2<-""                                                             #Initialisation d'une chaine de caractere qui vas contenire les pid des auteurs
        
        for (i in c(1:length(auteur))){                                      #Mettre tous les auteur en une seul chaine de caractere pour n'avoir qu'une seul case et les pid
          aut<-paste(aut,auteur[[i]],sep ="    ")                            #dans une autre case 
          pid2<-paste(pid2,pid[[i]],sep ="    ")
        }
      
        }else{
        
          aut<-result$result$result$hits$hit[[1]]$info$authors$author$text   #mettre l'auteur dans une chaine dans le cas ou dans la structure n'est pas une liste dans le json
          pid2<-result$result$result$hits$hit[[1]]$info$authors$author$'@pid'#mettre le pid dans une chaine dans le cas ou dans la structure n'est pas une liste dans le json
        
        }
        r<-c()                                                               #Initialisation d'un vecteur
      
                                              #######Lien title#######
      
      
      if(length(result$result$result$hits$hit[[1]]$info$title)==0){
        r[1]<-c("/")                                                        #Cas ou ce n'est spécifier
        
        
      }else{
        r[1]<-c(result$result$result$hits$hit[[1]]$info$title)            #Cas ou l'information est fournie par l'API              
      }
      
                                            #######Lien auteur#######
      
      
      r[2]<-c(aut)                                                         
      
                                            #######Lien pid#######
      
      
      r[3]<-c(pid2)
      
                                            #######Lien venue#######
      
      
      if(length(result$result$result$hits$hit[[1]]$info$venue)==0){
        r[4]<-c("/")                                                        #Cas ou ce n'est spécifier
        
        
      }else{
        r[4]<-c(result$result$result$hits$hit[[1]]$info$venue)               #Cas ou l'information est fournie par l'API
      }
      
                                            #######Lien volume#######
      
      
      if(length(result$result$result$hits$hit[[1]]$info$volume)==0){
        r[5]<-c("/")                                                        #Cas ou ce n'est spécifier
        
        
      }else{
        r[5]<-c(result$result$result$hits$hit[[1]]$info$volume)            #Cas ou l'information est fournie par l'API
      }
      
                                            #######Lien type#######
      
      
      if(length(result$result$result$hits$hit[[1]]$info$number)==0){
        r[6]<-c("/")                                                        #Cas ou ce n'est spécifier
        
        
      }else{
        r[6]<-c(result$result$result$hits$hit[[1]]$info$number)            #Cas ou l'information est fournie par l'API
      }
      
                                          #######Lien pages#######
      
      
      if(length(result$result$result$hits$hit[[1]]$info$pages)==0){
        r[7]<-c("/")                                                        #Cas ou ce n'est spécifier
        
        
      }else{
        r[7]<-c(result$result$result$hits$hit[[1]]$info$pages)            #Cas ou l'information est fournie par l'API
      }
      
                                          #######Lien year#######
      
      
      if(length(result$result$result$hits$hit[[1]]$info$year)==0){
        r[8]<-c("/")                                                        #Cas ou ce n'est spécifier
        
        
      }else{
        r[8]<-c(result$result$result$hits$hit[[1]]$info$year)            #Cas ou l'information est fournie par l'API
      }
      
      
                                            #######Lien key#######
      
      
      if(length(result$result$result$hits$hit[[1]]$info$key)==0){
        r[9]<-c("/")                                                        #Cas ou ce n'est spécifier
        
        
      }else{
        r[9]<-c(result$result$result$hits$hit[[1]]$info$key)            #Cas ou l'information est fournie par l'API
      }
      
                                            #######Lien type#######
      
      
      if(length(result$result$result$hits$hit[[1]]$info$type)==0){
        r[10]<-c("/")                                                        #Cas ou ce n'est spécifier
        
        
      }else{
        r[10]<-c(result$result$result$hits$hit[[1]]$info$type)            #Cas ou l'information est fournie par l'API
      }
      
                                            #######Lien doi#######
      
      
      if(length(result$result$result$hits$hit[[1]]$info$doi)==0){
        r[11]<-c("/")                                                        #Cas ou ce n'est spécifier
        
        
      }else{
        r[11]<-c(result$result$result$hits$hit[[1]]$info$doi)            #Cas ou l'information est fournie par l'API
      }
      
      
                                            #######Lien ee#######
      
      
      
      if(length(result$result$result$hits$hit[[1]]$info$ee)==0){
        r[12]<-c("/")                                                        #Cas ou ce n'est spécifier
        
        
      }else{
        r[12]<-c(result$result$result$hits$hit[[1]]$info$ee)            #Cas ou l'information est fournie par l'API
      }
      
      
                                          #######Lien url#######
      
      
      if(length(result$result$result$hits$hit[[1]]$info$url)==0){
        r[13]<-c("/")                                                        #Cas ou ce n'est spécifier
        
        
      }else{
        r[13]<-c(result$result$result$hits$hit[[1]]$info$url)            #Cas ou l'information est fournie par l'API
      }
      
     p<-rbind(p,r)                                                       #Ajout du vecteur comme ligne a la matrice
      
    }
    
    
  }
  
  }
  p<-as.data.frame(p)                                                    #Convertire la matrice en data frame
  names(p)<-c("title","auteur","pid","venue","volume","number","pages","year","key","type","doi","ee","url")#renommer les colonnes de la matrice
  View(p)                                                                #Visialisation de la dataframe
  write.table(p, "data1.csv", row.names=FALSE, sep="\t",dec=",", na=" ")  #Convertion de la dataframe en fichier csv
}


info_author(1)


