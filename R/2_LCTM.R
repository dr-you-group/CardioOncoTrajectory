#### Basic model ####

LCTM_Basic <- function(sample, seed = 100, nwg = TRUE, idiag = TRUE) {
  
  set.seed(seed)
  assign("Basic_1", lcmm::hlme(fixed = ef ~ diffInDay,
                             random = ~ diffInDay,
                             ng = 1,
                             subject = "ptno",
                             idiag = T,
                             data = data.frame(sample) ),
         
         envir = parent.frame())

  
  for (i in 2:4){
    set.seed(seed)
    m<-paste0("Basic_", as.character(nwg),"_", as.character(idiag),"_",i)
    
    assign(m, lcmm::hlme(fixed = ef ~ diffInDay,
                         mixture= ~ diffInDay,
                         random = ~ diffInDay,
                         ng = i,
                         nwg = nwg,
                         subject = "ptno",
                         idiag = idiag,
                         data = data.frame(sample) ),
           envir = parent.frame())
    
  }
  
}

#### Adjusted LCTM #### 

LCTM_Adjusted <- function(sample, seed = 100, nwg = TRUE, idiag = TRUE) {
  
  set.seed(seed)
  assign("Adjusted_1", lcmm::hlme(fixed = ef ~ diffInDay + sexDummy + age65,
                                  random = ~ diffInDay + age65,
                                  ng = 1,
                                  subject = "ptno",
                                  idiag = T,
                                  data = data.frame(sample) ),
         
         envir = parent.frame())
  
  
  for (i in 2:4){
    set.seed(seed)
    m<-paste0("Adjusted_", as.character(nwg),"_", as.character(idiag),"_",i)
    
    assign(m, lcmm::hlme(fixed = ef ~ diffInDay + sexDummy + age65,
                         mixture= ~ ~ diffInDay + age65,
                         random = ~ ~ diffInDay + age65,
                         ng = i,
                         nwg = nwg,
                         subject = "ptno",
                         idiag = idiag,
                         data = data.frame(sample) ),
           envir = parent.frame())
    
  }
  
}


#### Quadratic LCTM #### 

LCTM_Quadratic <- function(sample, seed = 100, nwg = TRUE, idiag = TRUE) {
  
  set.seed(seed)
  assign("Quadra_1", lcmm::hlme(fixed = ef ~ 1+diffInDay+I(diffInDay^2),
                                  random = ~ diffInDay,
                                  ng = 1,
                                  subject = "ptno",
                                  idiag = T,
                                  data = data.frame(sample) ),
         
         envir = parent.frame())
  
  
  for (i in 2:4){
    set.seed(seed)
    m<-paste0("Quadra_", as.character(nwg),"_", as.character(idiag),"_",i)
    
    assign(m, lcmm::hlme(fixed = ef ~ 1+diffInDay+I(diffInDay^2),
                         mixture= ~ ~ diffInDay,
                         random = ~ ~ diffInDay,
                         ng = i,
                         nwg = nwg,
                         subject = "ptno",
                         idiag = idiag,
                         data = data.frame(sample) ),
           envir = parent.frame())
    
  }
  
}
