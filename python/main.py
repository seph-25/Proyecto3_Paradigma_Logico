#!/usr/bin/env python3
import tkinter as tk
from tkinter import ttk, messagebox
from pyswip import Prolog
import os
from datetime import datetime

# ============================================================================
# CLASE PRINCIPAL
# ============================================================================

class MenuSaludableApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Menú Saludable Inteligente")
        # MEJORA: Ventana más grande y centrada para evitar solapamientos
        self.root.geometry("1200x800")
        self.root.minsize(1000, 700)  # Tamaño mínimo para evitar solapamientos
        
        # Centrar la ventana en la pantalla
        self._center_window()

        # Initialize Prolog
        self.prolog = Prolog()

        # Load Prolog knowledge base
        prolog_file = os.path.join(os.path.dirname(__file__), "..", "prolog", "menu_saludable.pl")
        prolog_file = os.path.abspath(prolog_file)

        if not os.path.exists(prolog_file):
            messagebox.showerror("Error", f"No se encontró el archivo Prolog: {prolog_file}")
            root.destroy()
            return

        self.prolog.consult(prolog_file)

        # Historial de aceptaciones/rechazos
        self.historial = []

        # Variables de filtros
        self.var_vegetariano = tk.BooleanVar(value=False)
        self.var_con_postre = tk.BooleanVar(value=True)
        self.var_tipo_carne = tk.StringVar(value="Todas")
        self.var_cal_min = tk.StringVar(value="")
        self.var_cal_max = tk.StringVar(value="")
        
        # Variables para ingredientes
        self.ingredientes_incluir = set()
        self.ingredientes_excluir = set()
        self.ingredientes_data = {}

        # Menús generados y aprobados
        self.menus_actuales = []
        self.menus_aprobados = []

        self.cargar_ingredientes()
        self.create_widgets()

    def cargar_ingredientes(self):
        """Carga todos los ingredientes disponibles desde Prolog y los categoriza"""
        try:
            self.ingredientes_data = {
                "Entradas": [],
                "Carbohidratos": [],
                "Proteínas": [],
                "Vegetales": [],
                "Postres": []
            }
            
            # Cargar entradas
            for result in self.prolog.query("entrada(Nombre, Calorias, Vegetariano)"):
                self.ingredientes_data["Entradas"].append({
                    "nombre": str(result["Nombre"]),
                    "calorias": result["Calorias"],
                    "vegetariano": result["Vegetariano"]
                })
            
            # Cargar carbohidratos
            for result in self.prolog.query("carbohidrato(Nombre, Calorias, Vegetariano)"):
                self.ingredientes_data["Carbohidratos"].append({
                    "nombre": str(result["Nombre"]),
                    "calorias": result["Calorias"],
                    "vegetariano": result["Vegetariano"]
                })
            
            # Cargar carnes/proteínas
            for result in self.prolog.query("carne(Nombre, Calorias, Tipo, Vegetariano)"):
                self.ingredientes_data["Proteínas"].append({
                    "nombre": str(result["Nombre"]),
                    "calorias": result["Calorias"],
                    "tipo": str(result["Tipo"]),
                    "vegetariano": result["Vegetariano"]
                })
            
            # Cargar vegetales
            for result in self.prolog.query("vegetal(Nombre, Calorias, Vegetariano)"):
                self.ingredientes_data["Vegetales"].append({
                    "nombre": str(result["Nombre"]),
                    "calorias": result["Calorias"],
                    "vegetariano": result["Vegetariano"]
                })
            
            # Cargar postres
            for result in self.prolog.query("postre(Nombre, Calorias, Vegetariano)"):
                self.ingredientes_data["Postres"].append({
                    "nombre": str(result["Nombre"]),
                    "calorias": result["Calorias"],
                    "vegetariano": result["Vegetariano"]
                })
                
        except Exception as e:
            print(f"Error cargando ingredientes: {e}")
            messagebox.showerror("Error", f"No se pudieron cargar los ingredientes: {e}")

    def create_widgets(self):
        # ===== FRAME DE FILTROS =====
        frame_filtros = ttk.LabelFrame(self.root, text="Filtros", padding=10)
        frame_filtros.pack(fill="x", padx=10, pady=5)

        # Fila 1: Vegetariano y Postre
        fila1 = ttk.Frame(frame_filtros)
        fila1.pack(fill="x", pady=5)

        ttk.Checkbutton(fila1, text="Solo Vegetariano",
                       variable=self.var_vegetariano,
                       command=self.on_toggle_vegetariano).pack(side="left", padx=10)

        ttk.Checkbutton(fila1, text="Con Postre",
                       variable=self.var_con_postre,
                       command=self.generar_menus).pack(side="left", padx=10)

        # Fila 2: Tipo de Carne
        fila2 = ttk.Frame(frame_filtros)
        fila2.pack(fill="x", pady=5)

        ttk.Label(fila2, text="Tipo de Carne:").pack(side="left", padx=5)
        tipos_carne = ["Todas", "Pollo", "Res", "Pescado", "Cerdo", "Vegetariano"]
        combo_carne = ttk.Combobox(fila2, textvariable=self.var_tipo_carne,
                                   values=tipos_carne, state="readonly", width=15)
        combo_carne.pack(side="left", padx=5)
        # Guardar como atributo para poder activarlo/desactivarlo cuando se marque "Solo Vegetariano"
        self.combo_carne = combo_carne
        combo_carne.bind("<<ComboboxSelected>>", lambda e: self.generar_menus())

        # Fila 3: Calorías
        fila3 = ttk.Frame(frame_filtros)
        fila3.pack(fill="x", pady=5)

        ttk.Label(fila3, text="Calorías Mínimas:").pack(side="left", padx=5)
        entry_min = ttk.Entry(fila3, textvariable=self.var_cal_min, width=10)
        entry_min.pack(side="left", padx=5)
        entry_min.bind("<KeyRelease>", lambda e: self.generar_menus())

        ttk.Label(fila3, text="Máximas:").pack(side="left", padx=5)
        entry_max = ttk.Entry(fila3, textvariable=self.var_cal_max, width=10)
        entry_max.pack(side="left", padx=5)
        entry_max.bind("<KeyRelease>", lambda e: self.generar_menus())

        # Selector de cantidad de menús
        ttk.Label(fila3, text="Cantidad de Menús:").pack(side="left", padx=(20, 5))
        self.var_cantidad_menus = tk.StringVar(value="8")
        combo_cantidad = ttk.Combobox(fila3, textvariable=self.var_cantidad_menus, 
                                     values=[str(i) for i in range(3, 26)], 
                                     width=5, state="readonly")
        combo_cantidad.pack(side="left", padx=5)
        combo_cantidad.bind("<<ComboboxSelected>>", lambda e: self.generar_menus())

        ttk.Button(fila3, text="Regenerar Menús",
                  command=self.regenerar_menus_diversos).pack(side="left", padx=20)

        # ===== FRAME DE MENÚS CON PESTAÑAS =====
        frame_menus = ttk.LabelFrame(self.root, text="Menús", padding=10)
        frame_menus.pack(fill="both", expand=True, padx=10, pady=(5, 2))

        # Crear notebook para pestañas de menús
        self.notebook_menus = ttk.Notebook(frame_menus)
        self.notebook_menus.pack(fill="both", expand=True)

        # PESTAÑA 1: Menús Sugeridos
        frame_sugeridos = ttk.Frame(self.notebook_menus)
        self.notebook_menus.add(frame_sugeridos, text="🍽️ Sugeridos")

        # Contenedor con scroll para menús sugeridos
        self.canvas = tk.Canvas(frame_sugeridos)
        scrollbar = ttk.Scrollbar(frame_sugeridos, orient="vertical", command=self.canvas.yview)
        self.frame_menus_scrollable = ttk.Frame(self.canvas)

        self.frame_menus_scrollable.bind(
            "<Configure>",
            lambda e: self.canvas.configure(scrollregion=self.canvas.bbox("all"))
        )

        self.canvas.create_window((0, 0), window=self.frame_menus_scrollable, anchor="nw")
        self.canvas.configure(yscrollcommand=scrollbar.set)

        self.canvas.pack(side="left", fill="both", expand=True)
        scrollbar.pack(side="right", fill="y")

        # PESTAÑA 2: Menús Aprobados
        frame_aprobados = ttk.Frame(self.notebook_menus)
        self.notebook_menus.add(frame_aprobados, text="✅ Aprobados")

        # Contenedor con scroll para menús aprobados
        self.canvas_aprobados = tk.Canvas(frame_aprobados)
        scrollbar_aprobados = ttk.Scrollbar(frame_aprobados, orient="vertical", command=self.canvas_aprobados.yview)
        self.frame_aprobados_scrollable = ttk.Frame(self.canvas_aprobados)

        self.frame_aprobados_scrollable.bind(
            "<Configure>",
            lambda e: self.canvas_aprobados.configure(scrollregion=self.canvas_aprobados.bbox("all"))
        )

        self.canvas_aprobados.create_window((0, 0), window=self.frame_aprobados_scrollable, anchor="nw")
        self.canvas_aprobados.configure(yscrollcommand=scrollbar_aprobados.set)

        self.canvas_aprobados.pack(side="left", fill="both", expand=True)
        scrollbar_aprobados.pack(side="right", fill="y")

        # Vincular eventos de scroll con rueda del mouse
        self.canvas.bind("<Configure>", self.reorganizar_tarjetas)
        self.canvas.bind("<MouseWheel>", self._on_mousewheel)
        self.frame_menus_scrollable.bind("<MouseWheel>", self._on_mousewheel)
        
        self.canvas_aprobados.bind("<Configure>", self.reorganizar_tarjetas_aprobados)
        self.canvas_aprobados.bind("<MouseWheel>", self._on_mousewheel_aprobados)
        self.frame_aprobados_scrollable.bind("<MouseWheel>", self._on_mousewheel_aprobados)

        # ===== FRAME DE SELECCIÓN DE INGREDIENTES =====
        frame_ingredientes = ttk.LabelFrame(self.root, text="Seleccionar Ingredientes", padding=10)
        frame_ingredientes.pack(fill="x", expand=False, padx=10, pady=(2, 5))
        
        # Limitar altura para evitar que ocupe demasiado espacio
        frame_ingredientes.configure(height=220)

        # Crear notebook para organizar por categorías
        notebook_ingredientes = ttk.Notebook(frame_ingredientes)
        notebook_ingredientes.pack(fill="both", expand=True)

        self.checkboxes_ingredientes = {}
        self.vars_ingredientes = {}

        for categoria, ingredientes in self.ingredientes_data.items():
            # Frame para cada categoría
            frame_categoria = ttk.Frame(notebook_ingredientes)
            notebook_ingredientes.add(frame_categoria, text=categoria)

            # Frame con scroll para la lista de ingredientes - altura reducida para evitar solapamientos
            canvas_ingredientes = tk.Canvas(frame_categoria, height=120)
            scrollbar_ingredientes = ttk.Scrollbar(frame_categoria, orient="vertical", command=canvas_ingredientes.yview)
            frame_scroll_ingredientes = ttk.Frame(canvas_ingredientes)

            frame_scroll_ingredientes.bind(
                "<Configure>",
                lambda e: canvas_ingredientes.configure(scrollregion=canvas_ingredientes.bbox("all"))
            )

            canvas_ingredientes.create_window((0, 0), window=frame_scroll_ingredientes, anchor="nw")
            canvas_ingredientes.configure(yscrollcommand=scrollbar_ingredientes.set)

            canvas_ingredientes.pack(side="left", fill="both", expand=True)
            scrollbar_ingredientes.pack(side="right", fill="y")
            
            # MEJORA: Habilitar scroll con rueda del mouse para ingredientes
            canvas_ingredientes.bind("<MouseWheel>", self._on_mousewheel_ingredientes)
            frame_scroll_ingredientes.bind("<MouseWheel>", self._on_mousewheel_ingredientes)

            # Controles para la categoría
            frame_controles = ttk.Frame(frame_categoria)
            frame_controles.pack(fill="x", pady=5)

            ttk.Button(frame_controles, text="✓ Incluir Todos",
                      command=lambda cat=categoria: self.incluir_todos_categoria(cat)).pack(side="left", padx=5)
            ttk.Button(frame_controles, text="✗ Excluir Todos",
                      command=lambda cat=categoria: self.excluir_todos_categoria(cat)).pack(side="left", padx=5)
            ttk.Button(frame_controles, text="🔄 Restaurar",
                      command=lambda cat=categoria: self.limpiar_categoria(cat)).pack(side="left", padx=5)

            # Crear checkboxes para cada ingrediente
            self.vars_ingredientes[categoria] = {}
            
            for ingrediente in ingredientes:
                nombre = ingrediente["nombre"]
                calorias = ingrediente["calorias"]
                
                frame_ingrediente = ttk.Frame(frame_scroll_ingredientes)
                frame_ingrediente.pack(fill="x", pady=1)

                # Variables para incluir y excluir - TODOS INCLUIDOS POR DEFECTO
                var_incluir = tk.BooleanVar(value=True)
                var_excluir = tk.BooleanVar(value=False)
                
                # Agregar a conjunto de ingredientes incluidos por defecto
                self.ingredientes_incluir.add(nombre)
                
                self.vars_ingredientes[categoria][nombre] = {
                    "incluir": var_incluir,
                    "excluir": var_excluir
                }

                # Checkbox para incluir
                check_incluir = ttk.Checkbutton(frame_ingrediente, text="Inc",
                                              variable=var_incluir,
                                              command=lambda n=nombre, cat=categoria: self.on_incluir_ingrediente(cat, n))
                check_incluir.pack(side="left", padx=2)

                # Checkbox para excluir
                check_excluir = ttk.Checkbutton(frame_ingrediente, text="Exc",
                                              variable=var_excluir,
                                              command=lambda n=nombre, cat=categoria: self.on_excluir_ingrediente(cat, n))
                check_excluir.pack(side="left", padx=2)

                # Label con nombre y calorías
                ttk.Label(frame_ingrediente, text=f"{nombre} ({calorias} cal)",
                         width=25).pack(side="left", padx=5)

        # ===== FRAME DE ESTADÍSTICAS =====
        frame_stats = ttk.LabelFrame(self.root, text="Estadísticas", padding=10)
        frame_stats.pack(fill="x", padx=10, pady=5)

        self.label_stats = ttk.Label(frame_stats, text="Aceptados: 0 | Rechazados: 0 | Total: 0")
        self.label_stats.pack()

        # Generar menús iniciales
        # Sincronizar estado del combobox con el filtro vegetariano al inicio
        self.on_toggle_vegetariano()
        self.generar_menus()

    def on_toggle_vegetariano(self):
        """Habilita/deshabilita el combobox de tipo de carne cuando se selecciona "Solo Vegetariano".

        - Si se marca vegetariano: fija el valor de tipo_carne a 'Vegetariano' y desactiva el combobox.
        - Si se desmarca: vuelve el combobox a estado 'readonly' (reactivándolo) y mantiene el valor previo si corresponde.
        Finalmente regenera los menús.
        """
        try:
            if self.var_vegetariano.get():
                # Forzar que el filtro de tipo de carne refleje vegetariano y deshabilitar selector
                self.var_tipo_carne.set("Vegetariano")
                # Si el widget fue creado, deshabilitarlo para evitar cambios
                if hasattr(self, 'combo_carne'):
                    self.combo_carne.config(state="disabled")
            else:
                # Reactivar selector de carnes
                if hasattr(self, 'combo_carne'):
                    self.combo_carne.config(state="readonly")
                    self.combo_carne.set("Todas")

        except Exception as e:
            print(f"Error en on_toggle_vegetariano: {e}")
        finally:
            # Regenerar menús para aplicar el nuevo filtro
            self.generar_menus()

    def on_incluir_ingrediente(self, categoria, nombre):
        """Maneja la selección de un ingrediente para incluir"""
        var_incluir = self.vars_ingredientes[categoria][nombre]["incluir"]
        var_excluir = self.vars_ingredientes[categoria][nombre]["excluir"]
        
        if var_incluir.get():
            # Si se incluye, no puede estar excluido
            var_excluir.set(False)
            self.ingredientes_incluir.add(nombre)
            self.ingredientes_excluir.discard(nombre)
        else:
            self.ingredientes_incluir.discard(nombre)
        
        self.generar_menus()

    def on_excluir_ingrediente(self, categoria, nombre):
        """Maneja la selección de un ingrediente para excluir"""
        var_incluir = self.vars_ingredientes[categoria][nombre]["incluir"]
        var_excluir = self.vars_ingredientes[categoria][nombre]["excluir"]
        
        if var_excluir.get():
            # Si se excluye, no puede estar incluido
            var_incluir.set(False)
            self.ingredientes_excluir.add(nombre)
            self.ingredientes_incluir.discard(nombre)
        else:
            self.ingredientes_excluir.discard(nombre)
        
        self.generar_menus()

    def incluir_todos_categoria(self, categoria):
        """Incluye todos los ingredientes de una categoría (ya están por defecto)"""
        for nombre, vars_dict in self.vars_ingredientes[categoria].items():
            vars_dict["incluir"].set(True)
            vars_dict["excluir"].set(False)
            self.ingredientes_incluir.add(nombre)
            self.ingredientes_excluir.discard(nombre)
        self.generar_menus()

    def excluir_todos_categoria(self, categoria):
        """Excluye todos los ingredientes de una categoría"""
        for nombre, vars_dict in self.vars_ingredientes[categoria].items():
            vars_dict["incluir"].set(False)
            vars_dict["excluir"].set(True)
            self.ingredientes_excluir.add(nombre)
            self.ingredientes_incluir.discard(nombre)
        self.generar_menus()

    def limpiar_categoria(self, categoria):
        """Restaura al estado por defecto (todos incluidos)"""
        for nombre, vars_dict in self.vars_ingredientes[categoria].items():
            vars_dict["incluir"].set(True)  # Estado por defecto: incluidos
            vars_dict["excluir"].set(False)
            self.ingredientes_incluir.add(nombre)  # Volver a incluir
            self.ingredientes_excluir.discard(nombre)
        self.generar_menus()

    def regenerar_menus_diversos(self):
        """Regenera menús forzando mayor diversidad"""
        # Limpiar menús actuales para forzar nueva selección
        self.menus_actuales = []
        
        # Incrementar temporalmente el número de menús a considerar
        self._aumentar_diversidad = True
        try:
            self.generar_menus()
        finally:
            self._aumentar_diversidad = False

    def generar_menus(self):
        """Genera menús basados en los filtros actuales usando Prolog"""
        # Limpiar frame de menús
        for widget in self.frame_menus_scrollable.winfo_children():
            widget.destroy()

        self.tarjetas_widgets = []

        # Preparar parámetros para Prolog
        vegetariano_filter = "true" if self.var_vegetariano.get() else "false"
        con_postre = "true" if self.var_con_postre.get() else "false"
        tipo_carne = self.var_tipo_carne.get().lower()

        # Configurar calorías (usar 'none' si no está especificado)
        try:
            cal_min = int(self.var_cal_min.get()) if self.var_cal_min.get() else None
        except ValueError:
            cal_min = None

        try:
            cal_max = int(self.var_cal_max.get()) if self.var_cal_max.get() else None
        except ValueError:
            cal_max = None

        min_cal_str = str(cal_min) if cal_min is not None else "none"
        max_cal_str = str(cal_max) if cal_max is not None else "none"

        # Preparar listas de ingredientes para Prolog
        incluir_lista = list(self.ingredientes_incluir) if self.ingredientes_incluir else []
        excluir_lista = list(self.ingredientes_excluir) if self.ingredientes_excluir else []
        
        try:
            # Usar cantidad de menús seleccionada por el usuario
            cantidad_menus = int(self.var_cantidad_menus.get())
            
            # CAMBIO: Usar nuevo predicado con cantidad personalizable
            query = f"""get_menu_details_con_cantidad({vegetariano_filter}, {tipo_carne}, {con_postre}, {min_cal_str}, {max_cal_str}, {incluir_lista}, {excluir_lista}, {cantidad_menus},
                        EntradaNom, EntradaCal, EntradaVeg,
                        CarbNom, CarbCal, CarbVeg,
                        CarneNom, CarneCal, CarneTipo, CarneVeg,
                        VegNom, VegCal, VegVeg,
                        PostreNom, PostreCal, PostreVeg,
                        TotalCal)"""

            # Obtener menús ya ordenados por aprendizaje y con diversidad mejorada
            results = []
            for i, result in enumerate(self.prolog.query(query)):
                if i >= cantidad_menus:  # Usar cantidad seleccionada por el usuario
                    break
                results.append(result)

            if not results or len(results) == 0:
                ttk.Label(self.frame_menus_scrollable,
                         text="⚠️ No hay menús que cumplan con los filtros seleccionados",
                         foreground="red").pack(pady=20)
                return

            # Convertir resultados a formato Python (ya vienen priorizados y diversos desde Prolog)
            self.menus_actuales = []
            seen_menus = set()  # Para evitar duplicados

            for result in results:
                # Crear representación de menú para detectar duplicados
                menu_key = (result['EntradaNom'], result['CarbNom'],
                           result['CarneNom'], result['VegNom'], result['PostreNom'])

                # Evitar duplicados
                if menu_key in seen_menus:
                    continue
                seen_menus.add(menu_key)

                menu_dict = self.convertir_resultado_prolog(result)
                self.menus_actuales.append(menu_dict)

            if not self.menus_actuales:
                ttk.Label(self.frame_menus_scrollable,
                         text="⚠️ No hay menús que cumplan con los criterios",
                         foreground="orange").pack(pady=20)
                return

            # Mostrar menús (ya vienen ordenados por aprendizaje y con diversidad desde Prolog)
            self.tarjetas_widgets = []
            for idx, menu in enumerate(self.menus_actuales, 1):
                tarjeta = self.crear_tarjeta_menu(menu, idx)
                self.tarjetas_widgets.append(tarjeta)

            # Organizar tarjetas en grid
            self.reorganizar_tarjetas()

        except Exception as e:
            ttk.Label(self.frame_menus_scrollable,
                     text=f"⚠️ Error al consultar Prolog: {str(e)}",
                     foreground="red").pack(pady=20)
            print(f"Error en query Prolog: {e}")

    def convertir_resultado_prolog(self, result):
        """Convierte un resultado de Prolog con campos individuales a formato Python dict"""
        menu_dict = {
            "entrada": {
                "nombre": str(result['EntradaNom']),
                "calorias": int(result['EntradaCal']),
                "vegetariano": str(result['EntradaVeg']) == "true"
            },
            "carbohidrato": {
                "nombre": str(result['CarbNom']),
                "calorias": int(result['CarbCal']),
                "vegetariano": str(result['CarbVeg']) == "true"
            },
            "carne": {
                "nombre": str(result['CarneNom']),
                "calorias": int(result['CarneCal']),
                "tipo": str(result['CarneTipo']),
                "vegetariano": str(result['CarneVeg']) == "true"
            },
            "vegetal": {
                "nombre": str(result['VegNom']),
                "calorias": int(result['VegCal']),
                "vegetariano": str(result['VegVeg']) == "true"
            },
            "postre": None if str(result['PostreNom']) == "none" else {
                "nombre": str(result['PostreNom']),
                "calorias": int(result['PostreCal']),
                "vegetariano": str(result['PostreVeg']) == "true"
            },
            "calorias": int(result['TotalCal'])
        }

        return menu_dict


    def reorganizar_tarjetas(self, event=None):
        """Reorganiza las tarjetas en grid según el ancho disponible"""
        if not hasattr(self, 'tarjetas_widgets') or not self.tarjetas_widgets:
            return

        # Obtener ancho disponible
        ancho_disponible = self.canvas.winfo_width()
        if ancho_disponible <= 1:  # Canvas aún no inicializado
            self.root.after(100, self.reorganizar_tarjetas)  # Reintentar
            return

        # Calcular número de columnas (ancho mejorado por tarjeta: 380px para más espacio)
        ancho_tarjeta = 380
        padding_total = 20  # Padding extra entre columnas
        num_columnas = max(1, (ancho_disponible - padding_total) // ancho_tarjeta)

        # Reorganizar en grid con mejor espaciado
        for idx, tarjeta in enumerate(self.tarjetas_widgets):
            fila = idx // num_columnas
            columna = idx % num_columnas
            tarjeta.grid(row=fila, column=columna, padx=8, pady=8, sticky="ew")

        # Configurar peso de columnas para distribución uniforme
        for col in range(num_columnas):
            self.frame_menus_scrollable.grid_columnconfigure(col, weight=1, minsize=ancho_tarjeta)

    def crear_tarjeta_menu(self, menu, numero):
        """Crea una tarjeta visual para mostrar un menú"""
        frame_tarjeta = ttk.Frame(self.frame_menus_scrollable, relief="solid", borderwidth=1)
        # No usar pack aquí, se usará grid en reorganizar_tarjetas

        # Encabezado
        frame_header = ttk.Frame(frame_tarjeta)
        frame_header.pack(fill="x", padx=10, pady=5)

        ttk.Label(frame_header, text=f"Menú #{numero}",
                 font=("Arial", 12, "bold")).pack(side="left")

        ttk.Label(frame_header, text=f"{menu['calorias']} calorías",
                 font=("Arial", 10), foreground="green").pack(side="right")

        # Contenido del menú
        frame_contenido = ttk.Frame(frame_tarjeta)
        frame_contenido.pack(fill="x", padx=20, pady=5)

        items = [
            ("Entrada:", menu["entrada"]["nombre"]),
            ("Carbohidrato:", menu["carbohidrato"]["nombre"]),
            ("Proteína:", menu["carne"]["nombre"]),
            ("Vegetal:", menu["vegetal"]["nombre"]),
        ]

        if menu["postre"]:
            items.append(("Postre:", menu["postre"]["nombre"]))

        for etiqueta, nombre in items:
            frame_item = ttk.Frame(frame_contenido)
            frame_item.pack(fill="x", pady=2)
            ttk.Label(frame_item, text=etiqueta, width=15).pack(side="left")
            ttk.Label(frame_item, text=nombre).pack(side="left")

        # Botones de acción
        frame_botones = ttk.Frame(frame_tarjeta)
        frame_botones.pack(fill="x", padx=10, pady=10)

        ttk.Button(frame_botones, text="✓ Aceptar",
                  command=lambda: self.aceptar_menu(menu)).pack(side="left", padx=5)

        ttk.Button(frame_botones, text="✗ Rechazar",
                  command=lambda: self.rechazar_menu(menu)).pack(side="left", padx=5)

        return frame_tarjeta

    def crear_tarjeta_menu_con_puntuacion(self, menu, numero, puntuacion):
        """Crea una tarjeta visual para mostrar un menú con su puntuación de aprendizaje"""
        frame_tarjeta = ttk.Frame(self.frame_menus_scrollable, relief="solid", borderwidth=1)
        # No usar pack aquí, se usará grid en reorganizar_tarjetas

        # Encabezado con puntuación
        frame_header = ttk.Frame(frame_tarjeta)
        frame_header.pack(fill="x", padx=10, pady=5)

        ttk.Label(frame_header, text=f"Menú #{numero}",
                 font=("Arial", 12, "bold")).pack(side="left")

        # Mostrar puntuación si es significativa
        if puntuacion > 0:
            color_puntuacion = "green"
            simbolo = "👍"
        elif puntuacion < 0:
            color_puntuacion = "red"
            simbolo = "👎"
        else:
            color_puntuacion = "gray"
            simbolo = "⚪"
        
        frame_puntuacion = ttk.Frame(frame_header)
        frame_puntuacion.pack(side="right")
        
        if abs(puntuacion) > 0.1:  # Mostrar solo si la puntuación es significativa
            ttk.Label(frame_puntuacion, text=f"{simbolo} {puntuacion:.1f}",
                     font=("Arial", 9), foreground=color_puntuacion).pack(side="right", padx=5)
        
        ttk.Label(frame_puntuacion, text=f"{menu['calorias']} cal",
                 font=("Arial", 10), foreground="green").pack(side="right", padx=5)

        # Contenido del menú
        frame_contenido = ttk.Frame(frame_tarjeta)
        frame_contenido.pack(fill="x", padx=20, pady=5)

        items = [
            ("Entrada:", menu["entrada"]["nombre"]),
            ("Carbohidrato:", menu["carbohidrato"]["nombre"]),
            ("Proteína:", menu["carne"]["nombre"]),
            ("Vegetal:", menu["vegetal"]["nombre"]),
        ]

        if menu["postre"]:
            items.append(("Postre:", menu["postre"]["nombre"]))

        for etiqueta, nombre in items:
            frame_item = ttk.Frame(frame_contenido)
            frame_item.pack(fill="x", pady=2)
            ttk.Label(frame_item, text=etiqueta, width=15).pack(side="left")
            ttk.Label(frame_item, text=nombre).pack(side="left")

        # Botones de acción
        frame_botones = ttk.Frame(frame_tarjeta)
        frame_botones.pack(fill="x", padx=10, pady=10)

        ttk.Button(frame_botones, text="✓ Aceptar",
                  command=lambda: self.aceptar_menu(menu)).pack(side="left", padx=5)

        ttk.Button(frame_botones, text="✗ Rechazar",
                  command=lambda: self.rechazar_menu(menu)).pack(side="left", padx=5)

        return frame_tarjeta

    def aceptar_menu(self, menu):
        """Registra la aceptación de un menú"""
        registro = {
            "menu": menu,
            "accion": "aceptado",
            "timestamp": datetime.now(),
            "filtros": self.obtener_estado_filtros()
        }
        self.historial.append(registro)
        
        # NUEVO: Agregar menú a la lista de aprobados
        if menu not in self.menus_aprobados:
            self.menus_aprobados.append(menu)
            self.actualizar_pestaña_aprobados()
        
        # CAMBIO 1: Registrar cada componente directamente en Prolog
        try:
            # Registrar entrada
            entrada_nom = menu['entrada']['nombre']
            self.prolog.assertz(f"regla_preferencia(entrada, '{entrada_nom}', aceptado_usuario)")
            
            # Registrar carbohidrato
            carb_nom = menu['carbohidrato']['nombre']
            self.prolog.assertz(f"regla_preferencia(carbohidrato, '{carb_nom}', aceptado_usuario)")
            
            # Registrar carne
            carne_nom = menu['carne']['nombre']
            self.prolog.assertz(f"regla_preferencia(carne, '{carne_nom}', aceptado_usuario)")
            
            # Registrar vegetal
            vegetal_nom = menu['vegetal']['nombre']
            self.prolog.assertz(f"regla_preferencia(vegetal, '{vegetal_nom}', aceptado_usuario)")
            
            # Registrar postre si existe
            if menu['postre']:
                postre_nom = menu['postre']['nombre']
                self.prolog.assertz(f"regla_preferencia(postre, '{postre_nom}', aceptado_usuario)")
                
        except Exception as e:
            print(f"Error registrando preferencias en Prolog: {e}")
        
        self.actualizar_estadisticas()
        messagebox.showinfo("Éxito", "¡Menú aceptado! 👍\nSe ha agregado a tus favoritos y aprendido tu preferencia.")

    def rechazar_menu(self, menu):
        """Registra el rechazo de un menú"""
        registro = {
            "menu": menu,
            "accion": "rechazado",
            "timestamp": datetime.now(),
            "filtros": self.obtener_estado_filtros()
        }
        self.historial.append(registro)
        
        # CAMBIO 2: Registrar cada componente como rechazo directamente en Prolog
        try:
            # Registrar entrada como rechazada
            entrada_nom = menu['entrada']['nombre']
            self.prolog.assertz(f"regla_aversion(entrada, '{entrada_nom}', rechazado_usuario)")
            
            # Registrar carbohidrato como rechazado
            carb_nom = menu['carbohidrato']['nombre']
            self.prolog.assertz(f"regla_aversion(carbohidrato, '{carb_nom}', rechazado_usuario)")
            
            # Registrar carne como rechazada
            carne_nom = menu['carne']['nombre']
            self.prolog.assertz(f"regla_aversion(carne, '{carne_nom}', rechazado_usuario)")
            
            # Registrar vegetal como rechazado
            vegetal_nom = menu['vegetal']['nombre']
            self.prolog.assertz(f"regla_aversion(vegetal, '{vegetal_nom}', rechazado_usuario)")
            
            # Registrar postre como rechazado si existe
            if menu['postre']:
                postre_nom = menu['postre']['nombre']
                self.prolog.assertz(f"regla_aversion(postre, '{postre_nom}', rechazado_usuario)")
                
        except Exception as e:
            print(f"Error registrando aversiones en Prolog: {e}")
        
        self.actualizar_estadisticas()
        messagebox.showinfo("Registrado", "Menú rechazado.\nSe ha registrado tu preferencia. 👎")

    def registrar_aprendizaje_prolog(self, menu, accion):
        """Registra el aprendizaje de un menú en Prolog"""
        try:
            # Registrar cada componente del menú
            if accion == "aceptado":
                # Registrar preferencias
                self.prolog.assertz(f"regla_preferencia(entrada, '{menu['entrada']['nombre']}', aceptado)")
                self.prolog.assertz(f"regla_preferencia(carbohidrato, '{menu['carbohidrato']['nombre']}', aceptado)")
                self.prolog.assertz(f"regla_preferencia(carne, '{menu['carne']['nombre']}', aceptado)")
                self.prolog.assertz(f"regla_preferencia(vegetal, '{menu['vegetal']['nombre']}', aceptado)")
                if menu["postre"]:
                    self.prolog.assertz(f"regla_preferencia(postre, '{menu['postre']['nombre']}', aceptado)")
            
            elif accion == "rechazado":
                # Registrar aversiones
                self.prolog.assertz(f"regla_aversion(entrada, '{menu['entrada']['nombre']}', rechazado)")
                self.prolog.assertz(f"regla_aversion(carbohidrato, '{menu['carbohidrato']['nombre']}', rechazado)")
                self.prolog.assertz(f"regla_aversion(carne, '{menu['carne']['nombre']}', rechazado)")
                self.prolog.assertz(f"regla_aversion(vegetal, '{menu['vegetal']['nombre']}', rechazado)")
                if menu["postre"]:
                    self.prolog.assertz(f"regla_aversion(postre, '{menu['postre']['nombre']}', rechazado)")
                    
        except Exception as e:
            print(f"Error al registrar en Prolog: {e}")
            
    def extraer_patrones_historial(self):
        """Analiza el historial local para identificar patrones de preferencias"""
        patrones = {
            "mas_aceptados": {},
            "mas_rechazados": {},
            "estadisticas": {}
        }
        
        # Contadores por componente
        for categoria in ["entrada", "carbohidrato", "carne", "vegetal", "postre"]:
            patrones["mas_aceptados"][categoria] = {}
            patrones["mas_rechazados"][categoria] = {}
            patrones["estadisticas"][categoria] = {}
        
        # Analizar historial
        for registro in self.historial:
            menu = registro["menu"]
            accion = registro["accion"]
            
            # Procesar cada componente
            componentes = [
                ("entrada", menu["entrada"]["nombre"]),
                ("carbohidrato", menu["carbohidrato"]["nombre"]),
                ("carne", menu["carne"]["nombre"]),
                ("vegetal", menu["vegetal"]["nombre"])
            ]
            
            if menu["postre"]:
                componentes.append(("postre", menu["postre"]["nombre"]))
            
            for categoria, nombre in componentes:
                if nombre not in patrones["estadisticas"][categoria]:
                    patrones["estadisticas"][categoria][nombre] = {"aceptado": 0, "rechazado": 0}
                
                patrones["estadisticas"][categoria][nombre][accion] += 1
        
        # Identificar más aceptados y rechazados
        for categoria in patrones["estadisticas"]:
            for nombre, stats in patrones["estadisticas"][categoria].items():
                total = stats["aceptado"] + stats["rechazado"]
                if total > 0:
                    confianza_aceptacion = stats["aceptado"] / total
                    confianza_rechazo = stats["rechazado"] / total
                    
                    if confianza_aceptacion >= 0.7:
                        patrones["mas_aceptados"][categoria][nombre] = confianza_aceptacion
                    if confianza_rechazo >= 0.7:
                        patrones["mas_rechazados"][categoria][nombre] = confianza_rechazo
        
        return patrones
    
    def sincronizar_aprendizaje_con_prolog(self):
        """Sincroniza el historial local con las reglas de Prolog"""
        try:
            # Limpiar reglas existentes (opcional, para empezar limpio)
            # self.prolog.retractall("regla_preferencia(_, _, _)")
            # self.prolog.retractall("regla_aversion(_, _, _)")
            
            # Re-procesar todo el historial
            for registro in self.historial:
                self.registrar_aprendizaje_prolog(registro["menu"], registro["accion"])
            
            print("Aprendizaje sincronizado con Prolog")
            
        except Exception as e:
            print(f"Error al sincronizar aprendizaje: {e}")
    

    

    

    


    def obtener_estado_filtros(self):
        """Obtiene el estado actual de todos los filtros"""
        return {
            "vegetariano": self.var_vegetariano.get(),
            "con_postre": self.var_con_postre.get(),
            "tipo_carne": self.var_tipo_carne.get(),
            "cal_min": self.var_cal_min.get(),
            "cal_max": self.var_cal_max.get()
        }

    def actualizar_estadisticas(self):
        """Actualiza las estadísticas mostradas"""
        aceptados = sum(1 for h in self.historial if h["accion"] == "aceptado")
        rechazados = sum(1 for h in self.historial if h["accion"] == "rechazado")
        total = len(self.historial)

        self.label_stats.config(
            text=f"Aceptados: {aceptados} | Rechazados: {rechazados} | Total: {total}"
        )

    def _center_window(self):
        """Centra la ventana en la pantalla"""
        self.root.update_idletasks()
        width = self.root.winfo_width()
        height = self.root.winfo_height()
        pos_x = (self.root.winfo_screenwidth() // 2) - (width // 2)
        pos_y = (self.root.winfo_screenheight() // 2) - (height // 2)
        self.root.geometry(f"{width}x{height}+{pos_x}+{pos_y}")

    def _on_mousewheel(self, event):
        """Maneja el scroll con rueda del mouse para menús"""
        self.canvas.yview_scroll(int(-1 * (event.delta / 120)), "units")

    def _on_mousewheel_aprobados(self, event):
        """Maneja el scroll con rueda del mouse para menús aprobados"""
        self.canvas_aprobados.yview_scroll(int(-1 * (event.delta / 120)), "units")

    def _on_mousewheel_ingredientes(self, event):
        """Maneja el scroll con rueda del mouse para ingredientes"""
        # Encontrar el canvas de ingredientes activo
        widget = event.widget
        while widget and not isinstance(widget, tk.Canvas):
            widget = widget.master
        if widget and isinstance(widget, tk.Canvas):
            widget.yview_scroll(int(-1 * (event.delta / 120)), "units")

    def actualizar_pestaña_aprobados(self):
        """Actualiza la pestaña de menús aprobados"""
        # Limpiar frame de menús aprobados
        for widget in self.frame_aprobados_scrollable.winfo_children():
            widget.destroy()

        if not self.menus_aprobados:
            ttk.Label(self.frame_aprobados_scrollable,
                     text="No hay menús aprobados aún.\n¡Acepta algunos menús para verlos aquí!",
                     font=("Arial", 12),
                     foreground="gray").pack(pady=50)
            return

        # Crear tarjetas para menús aprobados
        self.tarjetas_aprobados = []
        for idx, menu in enumerate(self.menus_aprobados, 1):
            tarjeta = self.crear_tarjeta_menu_aprobado(menu, idx)
            self.tarjetas_aprobados.append(tarjeta)

        # Organizar tarjetas en grid
        self.reorganizar_tarjetas_aprobados()

    def reorganizar_tarjetas_aprobados(self, event=None):
        """Reorganiza las tarjetas de menús aprobados en grid"""
        if not hasattr(self, 'tarjetas_aprobados') or not self.tarjetas_aprobados:
            return

        # Obtener ancho disponible
        ancho_disponible = self.canvas_aprobados.winfo_width()
        if ancho_disponible <= 1:  # Canvas aún no inicializado
            self.root.after(100, self.reorganizar_tarjetas_aprobados)
            return

        # Calcular número de columnas
        ancho_tarjeta = 380
        padding_total = 20
        num_columnas = max(1, (ancho_disponible - padding_total) // ancho_tarjeta)

        # Reorganizar en grid con mejor espaciado
        for idx, tarjeta in enumerate(self.tarjetas_aprobados):
            fila = idx // num_columnas
            columna = idx % num_columnas
            tarjeta.grid(row=fila, column=columna, padx=8, pady=8, sticky="ew")

        # Configurar peso de columnas
        for col in range(num_columnas):
            self.frame_aprobados_scrollable.grid_columnconfigure(col, weight=1, minsize=ancho_tarjeta)

    def crear_tarjeta_menu_aprobado(self, menu, numero):
        """Crea una tarjeta visual para mostrar un menú aprobado"""
        frame_tarjeta = ttk.Frame(self.frame_aprobados_scrollable, relief="solid", borderwidth=1)
        
        # Configurar estilo para menú aprobado (fondo ligeramente verde)
        style = ttk.Style()
        style.configure("Approved.TFrame", background="#e8f5e8")

        # Encabezado
        frame_header = ttk.Frame(frame_tarjeta)
        frame_header.pack(fill="x", padx=10, pady=5)

        ttk.Label(frame_header, text=f"Favorito #{numero}",
                 font=("Arial", 12, "bold"), foreground="#2d5a2d").pack(side="left")

        # Indicador de aprobado
        ttk.Label(frame_header, text="✅ APROBADO",
                 font=("Arial", 9, "bold"), foreground="green").pack(side="right", padx=5)

        ttk.Label(frame_header, text=f"{menu['calorias']} cal",
                 font=("Arial", 10), foreground="green").pack(side="right")

        # Contenido del menú
        frame_contenido = ttk.Frame(frame_tarjeta)
        frame_contenido.pack(fill="x", padx=20, pady=5)

        items = [
            ("Entrada:", menu["entrada"]["nombre"]),
            ("Carbohidrato:", menu["carbohidrato"]["nombre"]),
            ("Proteína:", menu["carne"]["nombre"]),
            ("Vegetal:", menu["vegetal"]["nombre"]),
        ]

        if menu["postre"]:
            items.append(("Postre:", menu["postre"]["nombre"]))

        for etiqueta, nombre in items:
            frame_item = ttk.Frame(frame_contenido)
            frame_item.pack(fill="x", pady=2)
            ttk.Label(frame_item, text=etiqueta, width=15, font=("Arial", 9)).pack(side="left")
            ttk.Label(frame_item, text=nombre, font=("Arial", 9, "bold")).pack(side="left")

        # Botones de acción
        frame_botones = ttk.Frame(frame_tarjeta)
        frame_botones.pack(fill="x", padx=10, pady=10)

        ttk.Button(frame_botones, text="🔄 Generar Similar",
                  command=lambda: self.generar_similar_a_aprobado(menu)).pack(side="left", padx=5)

        ttk.Button(frame_botones, text="🗑️ Remover",
                  command=lambda: self.remover_menu_aprobado(menu)).pack(side="right", padx=5)

        return frame_tarjeta

    def generar_similar_a_aprobado(self, menu_base):
        """Genera menús similares a un menú aprobado"""
        messagebox.showinfo("Funcionalidad", "Generando menús similares a tu favorito...\n(Esta funcionalidad se puede expandir)")
        # Aquí se podría implementar lógica para generar menús similares
        # Por ahora, simplemente cambiamos a la pestaña de sugeridos
        self.notebook_menus.select(0)  # Cambiar a pestaña de sugeridos
        
    def remover_menu_aprobado(self, menu):
        """Remueve un menú de la lista de aprobados"""
        if messagebox.askyesno("Confirmar", "¿Estás seguro de que quieres remover este menú de tus favoritos?"):
            self.menus_aprobados.remove(menu)
            self.actualizar_pestaña_aprobados()
            messagebox.showinfo("Removido", "Menú removido de tus favoritos.")

# ============================================================================
# PUNTO DE ENTRADA
# ============================================================================

if __name__ == "__main__":
    root = tk.Tk()
    app = MenuSaludableApp(root)
    root.mainloop()