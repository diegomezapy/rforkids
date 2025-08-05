#' Lanzar la app educativa
#'
#' Esta función lanza una interfaz gráfica simple para enseñar conceptos básicos de estadística.
#' @export
launch_rforkids <- function() {
  library(tcltk)

  ventana <- tktoplevel()
  tkwm.title(ventana, "Estadística básica para niños")

  valores_tcl <- vector("list", 5)
  entradas <- vector("list", 5)
  for (i in 1:5) {
    tkgrid(tklabel(ventana, text = paste("Número", i)))
    valores_tcl[[i]] <- tclVar("0")
    entradas[[i]] <- tkentry(ventana, textvariable = valores_tcl[[i]], width = 10)
    tkgrid(entradas[[i]])
  }

  texto_resultado <- tclVar("")
  etiqueta_resultado <- tklabel(ventana, textvariable = texto_resultado, font = "Helvetica 14")
  tkgrid(etiqueta_resultado)

  get_values <- function() {
    as.numeric(sapply(valores_tcl, function(x) tclvalue(x)))
  }

  boton_max <- tkbutton(ventana, text = "Máximo", command = function() {
    v <- get_values()
    tclvalue(texto_resultado) <- paste("Máximo:", max(v, na.rm = TRUE))
  })
  tkgrid(boton_max)

  boton_min <- tkbutton(ventana, text = "Mínimo", command = function() {
    v <- get_values()
    tclvalue(texto_resultado) <- paste("Mínimo:", min(v, na.rm = TRUE))
  })
  tkgrid(boton_min)

  boton_graf <- tkbutton(ventana, text = "Graficar", command = function() {
    v <- get_values()
    barplot(v, col = "skyblue", main = "Gráfico de barras", ylim = c(0, max(v, na.rm = TRUE) + 1))
  })
  tkgrid(boton_graf)
}
