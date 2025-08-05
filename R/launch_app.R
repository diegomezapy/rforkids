# ======================================================
# DivertiStats – Aventura Estadística (tcltk + tkrplot)
# Windows, R >= 4.x • Sin tcltk2
# ======================================================

library(tcltk)
library(tkrplot)

# ---------- Paleta y fuentes ----------
pal <- list(
  bg_main = "#f0f9ff",
  bg_tabs = c("#e6f7ff", "#fff7e6", "#f9f0ff", "#e6ffe6", "#fff0f5", "#f8f9fa"),
  fg_main = "#1e293b",
  accent1 = "#38bdf8",
  accent2 = "#fb7185",
  accent3 = "#a78bfa",
  button  = "#0ea5e9",
  success = "#10b981"
)

title_font    <- tkfont.create(family = "Comic Sans MS", size = 18, weight = "bold")
subtitle_font <- tkfont.create(family = "Comic Sans MS", size = 14, weight = "bold")
text_font     <- tkfont.create(family = "Arial",         size = 11)
button_font   <- tkfont.create(family = "Arial",         size = 10, weight = "bold")

# ---------- Utilidades ----------
create_plot_window <- function(parent, title, plot_fun, width = 500, height = 400) {
  win <- tktoplevel(parent)
  tkwm.title(win, title)
  tkwm.geometry(win, paste0(width, "x", height))
  tkwm.resizable(win, FALSE, FALSE)
  img <- tkrplot(win, fun = plot_fun, hscale = 1.5, vscale = 1.5)
  tkpack(img, padx = 10, pady = 10)
  tkfocus(win)
  invisible(win)
}

animate_button <- function(btn) {
  # Solo funciona con tkbutton (no ttkbutton)
  obg <- tryCatch(as.character(tkcget(btn, "-background")), error = function(e) "#e5e7eb")
  for (color in c("#ff6b6b", "#ffd166", "#06d6a0", "#118ab2")) {
    tkconfigure(btn, background = color); tcl("update"); Sys.sleep(0.07)
  }
  tkconfigure(btn, background = obg)
}

validate_numbers <- function(entries) {
  nums <- numeric(0)
  for (i in seq_along(entries)) {
    val <- tclvalue(tkget(entries[[i]]))
    if (val == "") {
      tkmessageBox(title = "Faltan datos",
                   message = paste("Por favor ingresa el número", i),
                   icon = "warning"); return(NULL)
    }
    num_val <- suppressWarnings(as.numeric(val))
    if (is.na(num_val)) {
      tkmessageBox(title = "Dato inválido",
                   message = paste("El número", i, "no es válido:", val),
                   icon = "error"); return(NULL)
    }
    nums <- c(nums, num_val)
  }
  nums
}

# ---------- Ventana principal ----------
root <- tktoplevel()
tkwm.title(root, "DivertiStats - Aventura Estadística para Niños")
tkwm.geometry(root, "900x700")
tkwm.resizable(root, FALSE, FALSE)
tkconfigure(root, background = pal$bg_main)

# Notebook principal (ttknotebook) y helper para añadir pestañas
nb <- ttknotebook(root); tkpack(nb, fill = "both", expand = TRUE)
add_tab <- function(nb, tab, label) tkadd(nb, tab, text = label)

# =====================================================
# PESTAÑA 1: BIENVENIDA
# =====================================================
tab1 <- ttkframe(nb, padding = 20)
add_tab(nb, tab1, "🌟 Bienvenida")
frame1 <- ttkframe(tab1, padding = 10); tkpack(frame1, expand = TRUE, fill = "both")

title_label <- tklabel(frame1,
                       text = "¡Bienvenidos a DivertiStats!",
                       font = title_font, foreground = pal$accent1, background = pal$bg_tabs[1]
)
tkpack(title_label, pady = c(10, 10))

image_placeholder <- tklabel(frame1, text = "✨", font = tkfont.create(size = 48),
                             background = pal$bg_tabs[1])
tkpack(image_placeholder, pady = 5)

welcome_text <- paste(
  "¡Hola exploradores de datos!\n\n",
  "En esta aventura aprenderás estadística jugando:\n\n",
  "• Cómo ordenar números y encontrar extremos\n",
  "• Simular lanzamientos de dados y monedas\n",
  "• Entender la diferencia entre media y mediana\n",
  "• Crear gráficos coloridos con tus propios datos\n\n",
  "¡Usa las pestañas superiores para comenzar tu viaje!\n",
  "Recuerda: ¡Las matemáticas pueden ser súper divertidas!"
)
text_widget <- tktext(frame1, height = 12, width = 80, wrap = "word",
                      font = text_font, background = "white",
                      padx = 15, pady = 10, relief = "sunken")
tkinsert(text_widget, "end", welcome_text)
tkconfigure(text_widget, state = "disabled")
tkpack(text_widget, pady = 10)

fun_fact_button <- tkbutton(frame1, text = "¡Dato Curioso!",
                            command = function() {
                              facts <- c(
                                "El primer gráfico estadístico publicado es de 1786 (Playfair).",
                                "¡Los dados existen desde hace más de 5000 años!",
                                "La media y la mediana pueden ser muy diferentes con outliers.",
                                "La estadística ayuda a predecir el clima y a mejorar la medicina."
                              )
                              tkmessageBox(title = "Dato Divertido", message = sample(facts, 1), icon = "info")
                              animate_button(fun_fact_button)
                            },
                            font = button_font, background = pal$accent3, foreground = "white", borderwidth = 2
)
tkpack(fun_fact_button, pady = 10)

# =====================================================
# PESTAÑA 2: MIS NÚMEROS (con subtabs)
# =====================================================
tab2 <- ttkframe(nb, padding = 12); add_tab(nb, tab2, "🔢 Mis Números")
tab2_nb <- ttknotebook(tab2); tkpack(tab2_nb, fill = "both", expand = TRUE, padx = 10, pady = 10)

tab2a <- ttkframe(tab2_nb, padding = 10); tkadd(tab2_nb, tab2a, text = "Ingresar Datos")
tab2b <- ttkframe(tab2_nb, padding = 10); tkadd(tab2_nb, tab2b, text = "Resultados")

# --- Panel de entrada ---
input_frame <- ttklabelframe(tab2a, text = "Ingresa tus 5 números", padding = 15)
tkpack(input_frame, fill = "x", padx = 20, pady = 10)

entries <- vector("list", 5)
for (i in 1:5) {
  fr <- ttkframe(input_frame)
  lab <- tklabel(fr, text = paste("Número", i, ":"), font = text_font, foreground = pal$fg_main)
  ent <- tkentry(fr, width = 8, font = text_font, background = "white", relief = "sunken")
  tkgrid(lab, ent, padx = 5, pady = 6)
  tkpack(fr, anchor = "w", padx = 10, pady = 2)
  entries[[i]] <- ent
}

# --- Variables y panel de resultados ---
sorted_var <- tclVar(""); min_var <- tclVar(""); max_var <- tclVar(""); mean_var <- tclVar("")
results_frame <- ttklabelframe(tab2b, text = "Resultados", padding = 20)
tkpack(results_frame, fill = "both", expand = TRUE, padx = 20, pady = 20)

mk_row <- function(parent, label, tvar, col = pal$accent2) {
  fr <- ttkframe(parent)
  lab <- tklabel(fr, text = label, font = text_font, foreground = pal$fg_main, width = 20, anchor = "e")
  val <- tklabel(fr, textvariable = tvar, font = text_font, foreground = col, width = 25, anchor = "w")
  tkgrid(lab, val, padx = 10, pady = 6, sticky = "ew")
  tkpack(fr, fill = "x", padx = 10, pady = 2)
}
mk_row(results_frame, "Números Ordenados:", sorted_var)
mk_row(results_frame, "Valor Mínimo:",     min_var)
mk_row(results_frame, "Valor Máximo:",     max_var)
mk_row(results_frame, "Promedio:",         mean_var, pal$success)

# --- Lógica de cálculo y gráfico ---
calculate_stats <- function() {
  nums <- validate_numbers(entries); if (is.null(nums)) return(invisible(NULL))
  tclvalue(sorted_var) <<- paste(sort(nums), collapse = ", ")
  tclvalue(min_var)    <<- min(nums)
  tclvalue(max_var)    <<- max(nums)
  tclvalue(mean_var)   <<- mean(nums)
  
  # Cambiar a la subpestaña de resultados
  tcl(tab2_nb, "select", tab2b)
  
  # Abrir gráfico
  create_plot_window(root, "Tus Números", function() {
    par(bg = pal$bg_tabs[2], mar = c(4,4,3,1))
    barplot(nums, names.arg = paste("N", 1:5),
            col = c("#ff9aa2","#ffb7b2","#ffdac1","#e2f0cb","#b5ead7"),
            border = NA, main = "Tus Números",
            xlab = "Posición", ylab = "Valor")
    abline(h = mean(nums), lty = 2)
    grid(nx = NA, ny = NULL, col = "white", lty = 2)
    legend("topleft", legend = sprintf("Media = %.2f", mean(nums)), bty = "n")
  })
}

button_frame <- ttkframe(tab2a); tkpack(button_frame, fill = "x", pady = 8)
btn_calculate <- tkbutton(button_frame, text = "Calcular Estadísticas",
                          command = calculate_stats, font = button_font,
                          background = pal$button, foreground = "white", width = 25)
tkpack(btn_calculate, pady = 6)

# =====================================================
# PESTAÑA 3: LANZAR DADOS
# =====================================================
tab3 <- ttkframe(nb, padding = 12); add_tab(nb, tab3, "🎲 Lanzar Dados")
dice_frame <- ttklabelframe(tab3, text = "Simulador de Dados", padding = 20)
tkpack(dice_frame, expand = TRUE, fill = "both", padx = 30, pady = 20)

dice_text <- tklabel(dice_frame, text = "¡Haz clic para lanzar los dados!",
                     font = subtitle_font, foreground = pal$accent3, background = pal$bg_tabs[3])
tkpack(dice_text, pady = c(10, 12))

dice_button <- tkbutton(dice_frame, text = "Lanzar Dados",
                        command = function() {
                          results <- sample(1:6, 2, replace = TRUE); total <- sum(results)
                          tkconfigure(dice_text, text = paste("Resultados:", results[1], "y", results[2], "\nTotal:", total))
                          animate_button(dice_button)
                          create_plot_window(root, "Frecuencia de Dados", function() {
                            rolls <- sample(1:6, 100, replace = TRUE)
                            par(bg = pal$bg_tabs[3])
                            hist(rolls, breaks = 0.5:6.5,
                                 col = c("#ff9aa2","#ffb7b2","#ffdac1","#e2f0cb","#b5ead7","#c7ceea"),
                                 main = "100 Lanzamientos de Dados", xlab = "Valor del Dado", ylab = "Frecuencia",
                                 border = NA)
                            grid(nx = NA, ny = NULL, col = "white", lty = 2)
                          })
                        },
                        font = button_font, background = pal$accent3, foreground = "white", width = 20
)
tkpack(dice_button, pady = 10)

# =====================================================
# PESTAÑA 4: MEDIA VS MEDIANA
# =====================================================
tab4 <- ttkframe(nb, padding = 12); add_tab(nb, tab4, "📏 Media vs Mediana")
mm_frame <- ttklabelframe(tab4, text = "Comparador", padding = 20)
tkpack(mm_frame, expand = TRUE, fill = "both", padx = 30, pady = 20)

explanation <- paste(
  "¿Cuál es la diferencia?\n\n",
  "• Media: El promedio de todos los números (suma/cantidad)\n",
  "• Mediana: El valor central tras ordenar los números\n\n",
  "¡Prueba con diferentes valores para ver cómo cambian!"
)
explain_text <- tktext(mm_frame, height = 6, width = 60, wrap = "word",
                       font = text_font, background = "white", padx = 15, pady = 10)
tkinsert(explain_text, "end", explanation); tkconfigure(explain_text, state = "disabled")
tkpack(explain_text, pady = 10)

data_frame <- ttkframe(mm_frame); tkpack(data_frame, pady = 15)
tkpack(tklabel(data_frame, text = "Tamaño del conjunto:", font = text_font),
       side = "left", padx = 5)
size_var <- tclVar("10")
size_slider <- tkscale(data_frame, from = 5, to = 30, variable = size_var,
                       orient = "horizontal", showvalue = TRUE, resolution = 1, length = 200)
tkpack(size_slider, side = "left", padx = 10)

generate_button <- tkbutton(data_frame, text = "Generar Datos",
                            command = function() {
                              size <- as.numeric(tclvalue(size_var))
                              x <- rnorm(size, mean = 50, sd = 15)
                              mu <- mean(x); md <- median(x)
                              create_plot_window(root, "Media vs Mediana", function() {
                                par(bg = pal$bg_tabs[4])
                                plot(sort(x), type = "o", pch = 19, col = pal$accent1,
                                     main = "Media vs Mediana",
                                     xlab = "Posición ordenada", ylab = "Valor")
                                abline(h = mu, col = pal$accent2, lwd = 2, lty = 2)
                                abline(h = md, col = pal$success, lwd = 2, lty = 2)
                                legend("topleft", legend = c("Datos", sprintf("Media = %.2f", mu), sprintf("Mediana = %.2f", md)),
                                       col = c(pal$accent1, pal$accent2, pal$success), pch = c(19, NA, NA), lty = c(1, 2, 2), lwd = 2, bty = "n")
                              })
                            },
                            font = button_font, background = pal$accent1, foreground = "white"
)
tkpack(generate_button, side = "left", padx = 15)

# =====================================================
# PESTAÑA 5: GRÁFICOS DIVERTIDOS
# =====================================================
tab5 <- ttkframe(nb, padding = 12); add_tab(nb, tab5, "📊 Gráficos Divertidos")
graph_frame <- ttklabelframe(tab5, text = "Explorador de Gráficos", padding = 20)
tkpack(graph_frame, expand = TRUE, fill = "both", padx = 30, pady = 20)

graph_types  <- c("Barras", "Pastel", "Puntos", "Líneas")
graph_choice <- tclVar("Barras")
selector <- ttkframe(graph_frame); tkpack(selector, pady = 10)

for (gt in graph_types) {
  rb <- tkradiobutton(selector, text = gt, variable = graph_choice, value = gt)
  tkpack(rb, side = "left", padx = 8)
}

show_graph_button <- tkbutton(graph_frame, text = "Mostrar Gráfico Muestral",
                              command = function() {
                                gt <- tclvalue(graph_choice)
                                data <- sample(1:100, 5)
                                create_plot_window(root, paste("Gráfico de", gt), function() {
                                  par(bg = pal$bg_tabs[5])
                                  cols <- c("#ff9aa2","#ffb7b2","#ffdac1","#e2f0cb","#b5ead7")
                                  if (gt == "Barras") {
                                    barplot(data, names.arg = LETTERS[1:5], col = cols, main = "Gráfico de Barras")
                                  } else if (gt == "Pastel") {
                                    pie(data, labels = LETTERS[1:5], col = cols, main = "Gráfico de Pastel")
                                  } else if (gt == "Puntos") {
                                    plot(data, pch = 19, cex = 2, col = pal$accent1, main = "Gráfico de Puntos", ylim = c(0, 100))
                                  } else if (gt == "Líneas") {
                                    plot(data, type = "o", pch = 19, col = pal$accent2, main = "Gráfico de Líneas", ylim = c(0, 100))
                                  }
                                })
                              },
                              font = button_font, background = pal$accent2, foreground = "white", width = 25
)
tkpack(show_graph_button, pady = 10)

# =====================================================
# PESTAÑA 6: CURIOSIDADES
# =====================================================
tab6 <- ttkframe(nb, padding = 12); add_tab(nb, tab6, "💡 Curiosidades")
facts_frame <- ttklabelframe(tab6, text = "Datos Fascinantes", padding = 20)
tkpack(facts_frame, expand = TRUE, fill = "both", padx = 30, pady = 20)

facts <- c(
  "El primer gráfico estadístico fue creado por William Playfair en 1786.",
  "Los dados más antiguos tienen más de 5000 años y se hallaron en Egipto.",
  "El 7 es el total más común al lanzar dos dados (6 combinaciones).",
  "La estadística ayuda a predecir desde el clima hasta epidemias.",
  "El 90% de los datos del mundo se generó en los últimos años."
)
current_fact <- tclVar(1)
fact_label <- tklabel(facts_frame, text = facts[1], font = text_font,
                      wraplength = 600, justify = "center", foreground = pal$fg_main)
tkpack(fact_label, pady = 20)

next_fact_button <- tkbutton(facts_frame, text = "Siguiente Curiosidad ➡️",
                             command = function() {
                               i <- as.integer(tclvalue(current_fact)) + 1
                               if (i > length(facts)) i <- 1
                               tclvalue(current_fact) <<- i
                               tkconfigure(fact_label, text = facts[i]); animate_button(next_fact_button)
                             },
                             font = button_font, background = pal$accent3, foreground = "white"
)
tkpack(next_fact_button, pady = 8)

# ---------- Ejecutar ----------
tkfocus(root)
tkwait.window(root)

