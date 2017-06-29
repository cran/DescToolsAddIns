
# insert_in <- function() {
#   rstudioapi::insertText(" %in% ", location = )
# }


Str <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("Str(%s)", sel))
  } else {
    cat("No selection!\n")
  }
}

Example <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("example(%s)", sel))
  } else {
    cat("No selection!\n")
  }
}


Abstract <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("Abstract(%s)", sel))
  } else {
    cat("No selection!\n")
  }
}

Summary <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("summary(%s)", sel))
  } else {
    cat("No selection!\n")
  }
}


Fix <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("fix(%s)", sel))
  } else {
    cat("No selection!\n")
  }
}



Desc <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("Desc(%s)", sel))
  } else {
    cat("No selection!\n")
  }
}


Select <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    if(sel == "pch") {
      if(!exists("pch"))
        PlotPch(newwin = TRUE)
    } else if(sel=="col"){
        txt <- eval(parse(text="ColPicker(newwin=TRUE)"))
        dev.off()
        opt <- options(useFancyQuotes=FALSE); on.exit(options(opt))
        rstudioapi::insertText(gettextf("col=c(%s)", paste(dQuote(txt), collapse=", ")))

    } else {
      if(sel != ""){
        txt <- eval(parse(text=gettextf("SelectVarDlg(%s)", sel)))
        rstudioapi::insertText(txt)
      }
    }
  } else {
    cat("No selection!\n")
  }

}


BuildModel <- function(){

  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != ""){
    txt <- eval(parse(text=gettextf("ModelDlg(%s)", sel)))
    rstudioapi::insertText(txt)
  } else {
    cat("No selection!\n")
  }

}




Plot <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("plot(Desc(%s))", sel))
  } else {
    cat("No selection!\n")
  }
}


Head <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != ""){
    rstudioapi::sendToConsole(gettextf("head(%s)", sel), execute = TRUE)
  } else {
    cat("No selection!\n")
  }

}

Some <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != ""){
    rstudioapi::sendToConsole(gettextf("Some(%s)", sel), execute = TRUE)
  } else {
    cat("No selection!\n")
  }

}

Save <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text

  if(sel != "") {
    f <- tclvalue(eval(parse(text=gettextf("tkgetSaveFile(initialfile='%s.rda', title='Save a file...')", sel))))
    if(f != "")
      rstudioapi::sendToConsole(gettextf("save(x=%s, file='%s')", sel, f))
  } else {
    cat("No selection!\n")
  }

}


XLView <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("XLView(%s)", sel))
  } else {
    cat("No selection!\n")
  }
}


IntView <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("View(%s)", sel))
  } else {
    cat("No selection!\n")
  }
}



FileOpen <- function(){
  txt <- eval(parse(text="FileOpenCmd(fmt=NULL)"))
  if(txt != "") {
    rstudioapi::insertText(txt)
  }
}


FlipBackSlash <- function() {
  txt <- getActiveDocumentContext()$selection[[1]]$text
  if(txt != "") {
    txt <- gsub("\\\\", "/", txt)
    rstudioapi::modifyRange(txt)
  } else {
    cat("No selection!\n")
  }

}


SetArrow <- function(){
    xy <- eval(parse(text="locator(n = 2)"))
    eval(parse(text="Arrow(x0 = xy$x[2], y0 = xy$y[2], x1 = xy$x[1], y1 = xy$y[1], head=3)"))
    txt <- gettextf("Arrow(x0 = %s, y0 = %s, x1 = %s, y1 = %s, head = 3)\n",
                    round(xy$x[2],2), round(xy$y[2],2), round(xy$x[1],2), round(xy$y[1],2))
    rstudioapi::modifyRange(txt)
}



Enquote <- function(){

  txt <- getActiveDocumentContext()$selection[[1]]$text
  if(txt != "") {
    opt <- options(useFancyQuotes=FALSE)
    on.exit(options(opt))

    txt <- paste(dQuote(strsplit(txt, split="\n")[[1]]), collapse=",")
    rstudioapi::modifyRange(txt)

  } else {
    cat("No selection!\n")
  }


}


EvalEnquote <- function(){

  txt <- getActiveDocumentContext()$selection[[1]]$text
  if(txt != "") {

    txt <- eval(parse(text=txt))

    opt <- options(useFancyQuotes=FALSE)
    on.exit(options(opt))

    txt <- paste(dQuote(txt), collapse=",")
    rstudioapi::modifyRange(txt)

  } else {
    cat("No selection!\n")
  }


}




NewMatrix <- function(){

  m <- edit(data.frame())
  m <- as.matrix(m)

  if(!all(dimnames(m)[[2]] == paste("var", 1:length(dimnames(m)[[2]]), sep="")))
    dnames <- gettextf(", \n       dimnames=list(%s)", toString(dimnames(m)))
  else
    dnames <- ""

  txt <- gettextf("m <- matrix(c(%s), nrow=%s%s)\n", toString(m), dim(m)[1], dnames)
  rstudioapi::insertText(txt)

}


