.onAttach <- function(libname, pkgname) {

  ## Some GUI functionality 
  if(.Platform$GUI[1] == "Rgui" && require(tcltk, quiet=TRUE)) {

    ## Define the auxiliary function for parameter setting
    set.dfa.parameters <<- function() {

      ## Main Frame
      base <- tktoplevel()
      tkwm.title(base, "Parameter setzen")

      ## First Frame: Monthly or quarterly data?
      quart  <- tclVar(FALSE)
      frame1 <- tkframe(base, relief="groove", borderwidth=2)
      buton1 <- tkradiobutton(frame1, text="Monthly",   valu=FALSE, vari=quart)
      buton2 <- tkradiobutton(frame1, text="Quarterly", value=TRUE, vari=quart)
      tkpack(tklabel(frame1, text="Monthly or quarterly data?"), anchor="w")
      tkpack(buton1, anchor="w")
      tkpack(buton2, anchor="w")
    
      ## Second Frame: Integration order of the series
      dd     <- tclVar(0)
      frame2 <- tkframe(base, relief="groove", borderwidth=2)
      buton1 <- tkradiobutton(frame2, text="0", value=0, variable=dd)
      buton2 <- tkradiobutton(frame2, text="1", value=1, variable=dd)
      buton3 <- tkradiobutton(frame2, text="2", value=2, variable=dd)
      tkpack(tklabel(frame2, text="Integration order of the series"), anch="w")
      tkpack(buton1, anchor="w")
      tkpack(buton2, anchor="w")
      tkpack(buton3, anchor="w")

      ## Third Frame: Type of filter?
      tpfilter <- tclVar(FALSE)
      frame3   <- tkframe(base, relief="groove", borderwidth=2)
      txt1     <- "Turning Point"
      txt2     <- "Best Level"
      buton1   <- tkradiobutton(frame3, text=txt1, valu=FALSE, variab=tpfilter)
      buton2   <- tkradiobutton(frame3, text=txt2, valu=TRUE,  variab=tpfilter)
      tkpack(tklabel(frame3, text="Type of filter?"), anchor="w")
      tkpack(buton1, anchor="w")
      tkpack(buton2, anchor="w")

      ## Fourth Frame: Lagrange parameter for phase restriction
      lambda  <- tclVar(1)
      frame4  <- tkframe(base, relief="groove", borderwidth=2)
      tkpack(tklabel(frame4, text="Lagrange parameter for phase restriction"))
      tkpack(tkscale(frame4, from=1, to=20, showvalue=TRUE, variable=lambda,
                     resolution=1, orient="horiz"))

      ## Fifth Frame: Shape of the frequency weighting function
      expweight <- tclVar(0.8)
      frame5    <- tkframe(base, relief="groove", borderwidth=2)
      txt       <- "Shape of the frequency weighting function"
      tkpack(tklabel(frame5,text=txt))
      tkpack(tkscale(frame5, from=0.5, to=2, showvalue=TRUE,
                     variable=expweight, resolution=0.05, orient="horiz"))

      ## Sixth Frame: Regularity constraint for moduli of poles
      pbd     <- tclVar(1.01)
      frame6  <- tkframe(base, relief="groove", borderwidth=2)
      tkpack(tklabel(frame6, text="Regularity constraint for moduli of poles"))
      tkpack(tkscale(frame6, from=1.01, to=3, showvalue=TRUE, variable=pbd,
                     resolution=0.01, orient="horiz"))

      ## Seventh Frame: Number of optimization loops
      n.loops <- tclVar(10)
      frame7  <- tkframe(base, relief="groove", borderwidth=2)
      tkpack(tklabel(frame7, text="Number of optimization loops"))
      tkpack(tkscale(frame7, from=1, to=20, showvalue=TRUE, variable=n.loops,
                     resolution=1, orient="horiz"))
    
      ## Eigth Frame: Text output for run-time control?
      verbose <- tclVar(FALSE)
      frame8  <- tkframe(base, relief="groove", borderwidth=2)
      buton1  <- tkradiobutton(frame8, text="None",   valu=0,  variab=verbose)
      buton2  <- tkradiobutton(frame8, text="Minimal",valu=1,  variab=verbose)
      buton3  <- tkradiobutton(frame8, text="Full",   valu=2,  variab=verbose)
      tkpack(tklabel(frame8,text="Text output for run-time control?"),anch="w")
      tkpack(buton1, anchor="w")
      tkpack(buton2, anchor="w")
      tkpack(buton3, anchor="w")
    
      ## OK button
      OnOK   <- function() {
        out <<- c(tclvalue(quart),     tclvalue(dd),        tclvalue(tpfilter),
                  tclvalue(lambda),    tclvalue(expweight), tclvalue(pbd),
                  tclvalue(n.loops),   tclvalue(verbose))
        tkdestroy(base)
      }

      q.but <- tkbutton(base, text="OK", command=OnOK)

      ## Do it!
      tkpack(frame1, frame2, frame3, frame4, frame5, frame6, frame7, frame8,
             q.but, fill="x")
    }

    ## Define the package name
    pacnam <- "idpSignalExtraction"

    ## Add the first main menu
    hmenu1 <- paste(pacnam, "-Fit", sep="")
    winMenuAdd(hmenu1)
    
    ## Loading the data
    dl.mentry <- "Load data"
    dl.action <- "dnm <- load(file.choose())"
    winMenuAddItem(paste(hmenu1, "/Data", sep=""), dl.mentry, dl.action)

    ## Define a dataset
    ds.defstr <- "Dataset"
    ds.defans <- expression(if (exists("dnm")) dnm else "")
    ds.mentry <- "Set data"
    ds.action <- "dnm <- winDialogString(ds.defstr, eval(ds.defans))"
    winMenuAddItem(paste(hmenu1, "/Data", sep=""), ds.mentry, ds.action)

    ## Set parameters
    ps.mentry <- "Set parameters"
    ps.action <- "set.dfa.parameters()"          
    winMenuAddItem(paste(hmenu1, "/Parameters", sep=""), ps.mentry, ps.action)
    
    ## Run the main routine
    mn <- "Start main routine..."
    a1 <- expression(if(exists("dnm"))get(dnm) else stop ("Set data first!"))
    a2 <- expression(if(exists("out"))as.logical(as.numeric(out[1]))else FALSE)
    a3 <- expression(if(exists("out"))as.numeric(out[2]) else 0)
    a4 <- expression(if(exists("out"))as.logical(as.numeric(out[3]))else TRUE)
    a5 <- expression(if(exists("out"))as.numeric(out[4]) else 1)
    a6 <- expression(if(exists("out"))as.numeric(out[5]) else 0.8)
    a7 <- expression(if(exists("out"))as.numeric(out[6]) else 1.01) 
    a8 <- expression(if(exists("out"))as.numeric(out[7]) else 10)
    a9 <- expression(if(exists("out"))as.numeric(out[8]) else 1)
    mn.action <- "fit <- dfa(eval(a1),eval(a2),eval(a3),eval(a4),eval(a5),"
    mn.action <- paste(mn.action,"eval(a6),eval(a7),eval(a8),eval(a9))",sep="")
    winMenuAddItem(hmenu1, "-", "")
    winMenuAddItem(hmenu1,mn,mn.action)

    ## Add the second main menu
    hmenu2 <- paste(pacnam, "-Analysis", sep="")
    winMenuAdd(hmenu2)

    ## Set an object
    os.defstr <- "Object"
    os.defans <- expression(if (exists("fit")) "fit" else "")
    os.mentry <- "Set object"
    os.action <- "fnam <- winDialogString(os.defstr, eval(os.defans))"
    winMenuAddItem(hmenu2, os.mentry, os.action)

    ## Plot, Predict, Coef, Fitted
    winMenuAddItem(hmenu2, "-", "")
    psp.obj <- expression(if (exists("fit")) fit else get(fnam))
    psp.ac1 <- "plot(eval(psp.obj))"
    psp.ac2 <- "predict(eval(psp.obj)); pval <- predict(eval(psp.obj))"
    psp.ac3 <- "coef(eval(psp.obj)); coeffi <- coef(eval(psp.obj))"
    psp.ac4 <- "fitted(eval(psp.obj)); fval <- fitted(eval(psp.obj))"
    winMenuAddItem(hmenu2, "Plot",          psp.ac1)
    winMenuAddItem(hmenu2, "Predict",       psp.ac2)
    winMenuAddItem(hmenu2, "Coefficients",  psp.ac3)
    winMenuAddItem(hmenu2, "Fitted Values", psp.ac4)
  }
}



   
