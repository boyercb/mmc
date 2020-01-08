# Create codebook function ------------------------------------------------

make_ODK_codebook <- function(choices_sheet,
                              questions_sheet,
                              remove_non_questions = TRUE, 
                              choices_value_name = "value",
                              choices_label_name = "label.English",
                              Qs_name = "name",
                              Qs_label_name = "label.English"){
  
  choices <- choices_sheet
  Qs <- questions_sheet
  
  if(!choices_value_name %in% names(choices)) stop("Please provide the name of the values variable in the choices sheet to choices_value_name.")
  if(!choices_label_name %in% names(choices)) stop("Please provide the name of the labels variable in the choices sheet to choices_labels_name.")
  if(!Qs_name %in% names(Qs)) stop("Please provide the name of the question names variable in the questions sheet to Qs_name.")
  if(!Qs_label_name %in% names(Qs)) stop("Please provide the name of the question labels variable in the questions sheet to Qs_label_name.")
  
  choices$value <- choices[,choices_value_name]
  choices$label <- choices[,choices_label_name]
  
  Qs$name <- Qs[,Qs_name]
  Qs$label <- Qs[,Qs_label_name]
  
  codebook <- lapply(
    X = Qs$name,
    FUN = function(qname){
      type <- Qs$type[which(Qs$name == qname)][1]
      is_select <- grepl(pattern = "select",x = type,ignore.case = T)
      
      if(is_select){
        list_name <- unlist(strsplit(x = as.character(type),split = "\\s+"))[2]
        matches <- grep(pattern = paste0("\\b",list_name,"\\b"),
                        x = choices$list_name,
                        ignore.case = T)
        
        return_labels <- data.frame(
          integer = choices$value[matches],
          label = choices$label[matches]
        )
        
      } else {
        return_labels <- type
      }
      
      question <- Qs$label[which(Qs$name == qname)][1]
      
      return_labels <- list(question = question, answer = return_labels)
      
      return(return_labels)
    }
  )
  
  names(codebook) <- Qs$name
  
  if(remove_non_questions){
    is_not_question <- unlist(sapply(
      X = codebook,
      FUN = function(x){
        (!is.data.frame(x$answer)) & grepl(pattern = "note|group|geopoint|calculate|audio|phonenumber|start|end",x = x$answer,ignore.case = TRUE)[1]
      }))
    
    codebook <- codebook[-which(is_not_question,arr.ind = T)]
    
  }
  
  return(codebook)
  
}


# Function for quickly finding questions in codebook ----------------------

find_q <- function(wording,codebook){
  truefalse <- sapply(codebook,function(x)grepl(wording,x$question,T))
  names(codebook)[truefalse]
}

# Function for making factors from codebook -------------------------------

factor_q <- function(var_name,data,codebook){
  factor(data[,var_name],
         levels = codebook[var_name][[1]]$answer$integer,
         labels = codebook[var_name][[1]]$answer$label)
}

# Function for dummying out variables -------------------------------------

dummy_q <- function(var_name,integers,data){
  variable <- data[,var_name]
  NAs <- is.na(variable)
  new_var <- as.numeric(variable %in% integers)
  new_var[NAs] <- NA
  return(new_var)
}


# Function for looking at object without NAs ------------------------------

no.na <- function(x)x[!is.na(x)]



